#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: LULC Timeseries
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 10/15/2019
#Purpose: Estimate LULC Timeseries from 1938 to present for USGS gagesII dataset
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup Workspace------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory (delete this once incorporated into larger workflow)
rm(list=ls())

#download relevant packages
library(sp)
library(sf)
library(raster)
library(fasterize)
library(dplyr)
library(tidyr)
library(stringr)
library(rslurm)

#Define data directories
data_dir<-"/nfs/njones-data/Research Projects/DryRiversRCN/spatial_data/"
results_dir<-"/nfs/njones-data/Research Projects/DryRiversRCN/results/"

#This directory contains:
# (1) Watershed Shapefiles Obtained from John Hammond (during the DryRiversRCN)
# (2) Hindcasted LULC 1938-1992: doi: 10.1080/1747423X.2016.1147619
#        web address: https://www.sciencebase.gov/catalog/item/59d3c73de4b05fe04cc3d1d1
# (3) Historic LULC:1992 to 2006:  https://doi.org/10.1890/13-1245.1
#        web address: https://www.sciencebase.gov/catalog/item/5b96c2f9e4b0702d0e826f6d
# (4) NLCD LULC: 2008 - 2016: 
#        web address: https://www.mrlc.gov/data
# (5) State boundaries (Tiger)
#        web address: https://catalog.data.gov/dataset/tiger-line-shapefile-2017-nation-u-s-current-state-and-equivalent-national

#Download watershed data
sheds<-sf::st_read(paste0(data_dir, "all_conus.shp"))
states<-sf::st_read(paste0(data_dir, "tl_2017_us_state.shp"))

#Crop sheds to continental US
#prep states shape
states<-states %>% 
  #project object
  sf::st_transform(., crs=st_crs(sheds)) %>% 
  #Remove state outside CONUS
  dplyr::filter(!(STUSPS %in% c("AK", "HI", "GU","PR", "AS", "VI", "MP"))) 
#crop sheds shapefile
sheds<-sheds[states,]

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Pre-1992 Data--------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.1 Create raster stack of LULC------------------------------------------------
#Identify relevant files in subdirectory
r<-list.files(paste0(data_dir,"USGS_LULC/"))[str_detect(list.files(paste0(data_dir,"USGS_LULC/")),"Backcasting")]
r<-paste0(paste0(data_dir,"USGS_LULC/"), r)

#Create raster stack
r <- do.call(stack, lapply(r, raster))

#2.2 Create function to extract LULC data---------------------------------------
fun<-function(n){
  
  #Define polygon 
  p<-sheds[n,]
  
  #Define UID
  uid<-sheds$GAGE_ID[n]
  
  #reproject polygon 
  p<-sf::st_transform(p, crs=r@crs)
  
  #convert ws_poly to to a grid of points
  p_grd<-fasterize::fasterize(p, crop(r[[1]], p))
  p_pnt<-raster::rasterToPoints(p_grd) %>% 
    dplyr::as_tibble() %>% 
    sf::st_as_sf(., coords = c("x", "y"), crs = r@crs) %>% 
    sf::as_Spatial(.)
  
  #Extract values at points
  p_values <- raster::extract(r, p_pnt)
  
  #Apply function
  results<- p_values %>% 
    #Convert to tibble
    dplyr::as_tibble() %>% 
    #conver to long format
    tidyr::pivot_longer(everything()) %>%
    #tally by LULC value and year
    dplyr::group_by(name, value) %>% dplyr::tally(.) %>% 
    #Convert to area
    dplyr::mutate(n = n*res(p_grd)[1]*res(p_grd)[2]) %>% 
    #Convert back to wide format
    tidyr::pivot_wider(., names_from = value, values_from = n) %>% 
    #Convert name to year
    dplyr::ungroup(.) %>% dplyr::rename(year = name) %>% 
    dplyr::mutate(year = stringr::str_extract(year, "\\d+")) %>% 
    #Add uid Info
    dplyr::mutate(uid = uid)
  
  #Export results
  results
}

#2.3 Send function to cluster---------------------------------------------------
#Define global simulation options
cluster_name<-"sesync"
time_limit<-"12:00:00"
n.nodes<-20
n.cpus<-8
sopts <- list(partition = cluster_name, time = time_limit)
params<-data.frame(n=seq(1,nrow(sheds)))

#send job to cluster
t0<-Sys.time()
job<- slurm_apply(fun, 
                   params,
                   add_objects = c("sheds","r"),
                  nodes = n.nodes, cpus_per_node=n.cpus,
                  pkgs=c('sp','sf','raster','fasterize','dplyr', 'tidyr', 'stringr'),
                  slurm_options = sopts)

#check job status
print_job_status(job)

#Gather job results
results <- get_slurm_out(job, outtype = "raw")
results <- data.table::rbindlist(results, fill=T) %>% as_tibble()

#Document time
tf<-Sys.time()
tf-t0

#Save backup
save.image("hindcast_results.RDATA")
write.csv(results, paste0(results_dir,"LULC_hindcast.csv"))

#Cleanup working space
cleanup_files(job)
remove(list = ls()[ls()!='sheds' &  ls()!='data_dir' & ls()!='results_dir'])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 1992-2006 Data-------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.1 Create raster stack of LULC------------------------------------------------
#Identify relevant files in subdirectory
r<-list.files(paste0(data_dir,"USGS_LULC/"))[str_detect(list.files(paste0(data_dir,"USGS_LULC/")),"Historical_")]
r<-paste0(paste0(data_dir,"USGS_LULC/"), r)

#Create raster stack
r <- do.call(stack, lapply(r, raster))

#3.2 Create function to extract LULC data---------------------------------------
fun<-function(n){
  
  #Define polygon 
  p<-sheds[n,]
  
  #Define UID
  uid<-sheds$GAGE_ID[n]
  
  #reproject polygon 
  p<-sf::st_transform(p, crs=r@crs)
  
  #convert ws_poly to to a grid of points
  p_grd<-fasterize::fasterize(p, crop(r[[1]], p))
  p_pnt<-raster::rasterToPoints(p_grd) %>% 
    dplyr::as_tibble() %>% 
    sf::st_as_sf(., coords = c("x", "y"), crs = r@crs) %>% 
    sf::as_Spatial(.)
  
  #Extract values at points
  p_values <- raster::extract(r, p_pnt)
  
  #Apply function
  results<- p_values %>% 
    #Convert to tibble
    dplyr::as_tibble() %>% 
    #conver to long format
    tidyr::pivot_longer(everything()) %>%
    #tally by LULC value and year
    dplyr::group_by(name, value) %>% dplyr::tally(.) %>% 
    #Convert to area
    dplyr::mutate(n = n*res(p_grd)[1]*res(p_grd)[2]) %>% 
    #Convert back to wide format
    tidyr::pivot_wider(., names_from = value, values_from = n) %>% 
    #Convert name to year
    dplyr::ungroup(.) %>% dplyr::rename(year = name) %>% 
    dplyr::mutate(year = stringr::str_extract(year, "\\d+")) %>% 
    #Add uid Info
    dplyr::mutate(uid = uid)
  
  #Export results
  results
}

#3.3 Send function to cluster---------------------------------------------------
#Define global simulation options
cluster_name<-"sesync"
time_limit<-"12:00:00"
n.nodes<-20
n.cpus<-8
sopts <- list(partition = cluster_name, time = time_limit)
params<-data.frame(n=seq(1,nrow(sheds)))

#send job to cluster
t0<-Sys.time()
job <- slurm_apply(fun, 
                   params,
                   add_objects = c("sheds","r"),
                   nodes = n.nodes, cpus_per_node=n.cpus,
                   pkgs=c('sp','sf','raster','fasterize','dplyr', 'tidyr', 'stringr'),
                   slurm_options = sopts)

#check job status
print_job_status(job)

#Gather job results
results <- get_slurm_out(job, outtype = "raw")
results <- data.table::rbindlist(results, fill=T) %>% as_tibble()

#Document time
tf<-Sys.time()
tf-t0

#Save backup
save.image("historical_results.RDATA")
write.csv(results, paste0(results_dir,"LULC_historic.csv"))

#Cleanup working space
cleanup_files(job)
remove(list = ls()[ls()!='sheds' &  ls()!='data_dir' & ls()!='results_dir'])

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 2006-2016 Data-------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.1 Create raster stack of LULC------------------------------------------------
#Identify relevant files in subdirectory
r<-list.files(data_dir)[str_detect(list.files(data_dir),"NLCD")]
r<-r[str_detect(r, '.img')]
r<-r[!str_detect(r,'Change')]
r<-r[!str_detect(r,'2001')]
r<-r[!str_detect(r,'2004')]
r<-r[!str_detect(r,'2006')]
r<-paste0(data_dir, r)

#identify raters of interest
r <- do.call(stack, lapply(r, raster))

#4.2 Create function to extract LULC data---------------------------------------
fun<-function(n){
  
  #Define polygon 
  p<-sheds[n,]
  
  #Define UID
  uid<-sheds$GAGE_ID[n]
  
  #reproject polygon 
  p<-sf::st_transform(p, crs=r@crs)
  
  #convert ws_poly to to a grid of points
  p_grd<-fasterize::fasterize(p, crop(r[[1]], p))
  p_pnt<-raster::rasterToPoints(p_grd) %>% 
    dplyr::as_tibble() %>% 
    sf::st_as_sf(., coords = c("x", "y"), crs = r@crs) %>% 
    sf::as_Spatial(.)
  
  #Crop raster for speed
  r_crop<-crop(r, p)
  
  #Extract values at points
  p_values <- raster::extract(r_crop, p_pnt)
  
  #Apply function
  results<- p_values %>% 
    #Convert to tibble
    dplyr::as_tibble() %>% 
    #conver to long format
    tidyr::pivot_longer(everything()) %>%
    #tally by LULC value and year
    dplyr::group_by(name, value) %>% dplyr::tally(.) %>% 
    #Convert to area
    dplyr::mutate(n = n*res(p_grd)[1]*res(p_grd)[2]) %>% 
    #Convert back to wide format
    tidyr::pivot_wider(., names_from = value, values_from = n) %>% 
    #Convert name to year
    dplyr::ungroup(.) %>% dplyr::rename(year = name) %>% 
    dplyr::mutate(year = stringr::str_extract(year, "\\d+")) %>% 
    #Add uid Info
    dplyr::mutate(uid = uid)
  
  #Export results
  results
}


#4.3 Send function to cluster---------------------------------------------------
#Define global simulation options
cluster_name<-"sesync"
time_limit<-"12:00:00"
n.nodes<-20
n.cpus<-8
sopts <- list(partition = cluster_name, time = time_limit)
params<-data.frame(n=seq(1,nrow(sheds)))

#send job to cluster
job <- slurm_apply(fun, 
                   params,
                   add_objects = c("sheds","r"),
                   nodes = n.nodes, cpus_per_node=n.cpus,
                   pkgs=c('sp','sf','raster','fasterize','dplyr', 'tidyr', 'stringr'),
                   slurm_options = sopts)

#check job status
t0<-Sys.time()
print_job_status(job)

#Gather job results
results <- get_slurm_out(job, outtype = "raw")
results <- data.table::rbindlist(results, fill=T) %>% as_tibble()

#Document time
tf<-Sys.time()
tf-t0

#Save backup
save.image("nlcd_results.RDATA")
write.csv(results, paste0(results_dir,"LULC_historic.csv"))

#Cleanup working space
cleanup_files(job)
remove(list = ls()[ls()!='sheds' &  ls()!='data_dir' & ls()!='results_dir'])


