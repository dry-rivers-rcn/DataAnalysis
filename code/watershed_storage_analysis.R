#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Watershed Storage 
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 10/8/2019
#Purpose: Estimate aggregate watershed storage for USGS gagesII dataset
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup Workspace------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory (delete this once incoporated into larger workflow)
rm(list=ls())

#download relevant packages
library(rgdal)
library(raster)
library(sf)
library(fasterize)
library(rslurm)
library(tidyverse)

#Define data directories
data_dir<-"/nfs/njones-data/Research Projects/DryRiversRCN/spatial_data/"
#This directory contains:
# (1) Watershed Shapefiles Obtained from John Hammond (during the DryRiversRCN)
# (2) Depth to bedrock: https://doi.org/10.1371/journal.pone.0169748
      #web address: https://data.isric.org/geonetwork/srv/eng/catalog.search#/metadata/f36117ea-9be5-4afd-bb7d-7a3e77bf392a
# (3) porosity:  https://doi.org/10.1002/2014GL059856
      #web address: https://dataverse.scholarsportal.info/dataset.xhtml?persistentId=doi:10.5683/SP2/DLGXYO


#Bring spatial data into R environmnet
sheds<-sf::st_read(paste0(data_dir, "all_conus.shp"))
bedrock_depth<-raster::raster(paste0(data_dir,'BDTICM_M_250m_ll.tif'))
porosity<-st_read(paste0(data_dir, "GLHYMPS/GLHYMPS.gdb"), layer="Final_GLHYMPS_Polygon") #This will take a hot second...

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Extracterize Function------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create function to extract values from large raster function
extracterize<-function(r, p, uid, fun=function(x){mean(x, na.rm=T)}){
  
  #reproject polygon 
  p<-sf::st_transform(p, crs=r@crs)
  
  #convert ws_poly to to a grid of points
  p_grd<-fasterize::fasterize(p, crop(r, p))
  p_pnt<-raster::rasterToPoints(p_grd) %>% 
    dplyr::as_tibble() %>% 
    sf::st_as_sf(., coords = c("x", "y"), crs = r@crs) %>% 
    sf::as_Spatial(.)
  
  #Extract values at points
  p_values <- raster::extract(r, p_pnt)
  
  #Apply function
  result<- fun(p_values)
  
  #Create output
  output<-dplyr::tibble(
    uid,
    value = result
  )
  
  #Export Output
  output
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Depth to Bedrock-----------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create wrapper function for depth to bedrock estimate
fun_bedrock<-function(n){
  tryCatch(extracterize(
              r = bedrock_depth,
              p = sheds[n,],
              uid = sheds$GAGE_ID[n]), 
           error = tibble(uid=sheds$GAGE_ID[n], 
                          value = -9999))}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Porosity-------------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.1 Organize Data (Fasterize!!!)-----------------------------------------------
#Convert porosity to centroid
por_pnt<-porosity %>% st_centroid(., by_element = T)

#project sheds shapefile
sheds_projected<-sheds %>% st_transform(., crs=st_crs(porosity))

#Create boundy box of projected shapefile
box<-st_bbox(sheds_projected) %>% st_as_sfc(.)

#Crop points in BBox
por_pnt<-por_pnt %>% filter(st_intersects(por_pnt, box, sparse=F))

#Filter porosity dataset to points within boundy box
porosity<-porosity %>% filter(IDENTITY_ %in% por_pnt$IDENTITY_)

#fasterize
r<-raster(extent(porosity), res = 250)
porosity<-fasterize(porosity, r, field = 'Porosity')

#4.2 Create crop function-------------------------------------------------------
#Create wrapper for extraction function
fun_porosity<-function(n){
  extracterize(r = porosity, 
               p = sheds_projected[n,],
               uid = sheds_projected$GAGE_ID[n])
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5.0 Run functions--------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#5.1 Global Options-------------------------------------------------------------
#Define global simulation options
cluster_name<-"sesync"
time_limit<-"12:00:00"
n.nodes<-8
n.cpus<-8
sopts <- list(partition = cluster_name, time = time_limit)
params<-data.frame(n=seq(1,nrow(sheds)))

#5.2 Send jobs to cluster-------------------------------------------------------
#Record Start Time
t0<-Sys.time()

#bedrock
bdk<- slurm_apply(fun_bedrock, 
                  params,
                  add_objects = c(
                    #Functions
                    "fun_bedrock", "extracterize",
                    #Spatial data
                    "sheds","bedrock_depth"),
                  nodes = n.nodes, cpus_per_node=n.cpus,
                  pkgs=c('sp','sf','raster','fasterize','dplyr'),
                  slurm_options = sopts)


#porosity
por<- slurm_apply(fun_porosity, 
                  params,
                  add_objects = c(
                    #Functions
                    "fun_porosity", "extracterize",
                    #Spatial data
                    "sheds_projected","porosity"),
                  nodes = n.nodes, cpus_per_node=n.cpus,
                  pkgs=c('sp','sf','raster','fasterize','dplyr'),
                  slurm_options = sopts)


#check job status
print_job_status(bdk)
print_job_status(por)

#5.3 Gather slurm results-------------------------------------------------------
#gather results
bdk_results <- get_slurm_out(bdk, outtype = "table")
por_results <- get_slurm_out(por, outtype = 'table')

#record stop time
tf<-Sys.time()
tf-t0

#Cleanup jobs
cleanup_files(bdk)
cleanup_files(por)

#Join results, cleanup, and estimate storage
results<-bdk_results %>% as_tibble() %>% 
  #Rename depth to bedrock and convert to m
  rename(depth_bedrock_m = value) %>% 
  mutate(depth_bedrock_m = depth_bedrock_m/100) %>% 
  #Add porosity data
  left_join(por_results) %>% rename(porosity = value) %>% 
  #Calculate storage metric
  mutate(storage_m = depth_bedrock_m*porosity) %>% 
  #Use group_by to deal with duplicates
  group_by(uid) %>% 
  summarise(depth_bedrock_m=mean(depth_bedrock_m, na.rm=T),
            porosity = mean(porosity, na.rm=T), 
            storage_m = mean(storage_m, na.rm=T))
