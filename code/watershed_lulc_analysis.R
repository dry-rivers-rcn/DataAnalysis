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
# (2) Hindcasted LULC 1938-1992: https://doi.org/10.5066/F7KK99RR
#        web address: https://www.sciencebase.gov/catalog/item/59d3c73de4b05fe04cc3d1d1
# (3) Historic LULC:1992 to 2006: https://doi.org/10.5066/P95AK9HP
#        web address: https://www.sciencebase.gov/catalog/item/5b96c2f9e4b0702d0e826f6d
# (4) State boundaries (Tiger)
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
fun_backcast<-function(n){
  
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
  
  #Look for missing colnames
  missing<-c('year', paste0(seq(1,16)), 'uid')
  missing<-missing[!(missing %in% colnames(results))]
  
  #Add missing cols to tibble
  for(i in 1:length(missing)){
    results[,missing[i]]<-0
  }
  
  #Export results
  results
}

#2.3 Send function to cluster---------------------------------------------------
#Define global simulation options
cluster_name<-"sesync"
time_limit<-"12:00:00"
n.nodes<-12
n.cpus<-8
sopts <- list(partition = cluster_name, time = time_limit)
params<-data.frame(n=seq(1,nrow(sheds)))

#send job to cluster
t0<-Sys.time()
job1<- slurm_apply(fun_backcast, 
                   params,
                   add_objects = c(
                    #Functions
                    "fun_backcast", 
                    #Spatial data
                    "sheds","r"),
                  nodes = n.nodes, cpus_per_node=n.cpus,
                  pkgs=c('sp','sf','raster','fasterize','dplyr', 'tidyr', 'stringr'),
                  slurm_options = sopts)

#check job status
print_job_status(job1)

#Gather job results
results_1 <- get_slurm_out(job1, outtype = "raw")
results_1 <- bind_rows(results_1)

#Document time
tf<-Sys.time()
tf-t0

#Cleanup jobs
cleanup_files(job1)

#Save backup
save.image("hindcast_results.RDATA")

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 1992-2006 Data-------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.1 Create raster stack of LULC------------------------------------------------
#Identify relevant files in subdirectory
r<-list.files(paste0(data_dir,"USGS_LULC/"))[str_detect(list.files(paste0(data_dir,"USGS_LULC/")),"Historical_")]
r<-paste0(paste0(data_dir,"USGS_LULC/"), r)

#Create raster stack
r <- do.call(stack, lapply(r, raster))

#2.3 Create function to extract LULC data---------------------------------------
fun_historical<-function(n){
  
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
  
  #Look for missing colnames
  missing<-c('year', paste0(seq(1,16)), 'uid')
  missing<-missing[!(missing %in% colnames(results))]
  
  #Add missing cols to tibble
  for(i in 1:length(missing)){
    results[,missing[i]]<-0
  }
  
  #Export results
  results
}

#2.3 Send function to cluster---------------------------------------------------
#Define global simulation options
cluster_name<-"sesync"
time_limit<-"12:00:00"
n.nodes<-24
n.cpus<-8
sopts <- list(partition = cluster_name, time = time_limit)
params<-data.frame(n=seq(1,nrow(sheds)))

#send job to cluster
t0<-Sys.time()
job2<- slurm_apply(fun_backcast, 
                   params,
                   add_objects = c(
                     #Functions
                     "fun_backcast", 
                     #Spatial data
                     "sheds","r"),
                   nodes = n.nodes, cpus_per_node=n.cpus,
                   pkgs=c('sp','sf','raster','fasterize','dplyr', 'tidyr', 'stringr'),
                   slurm_options = sopts)

#check job status
print_job_status(job2)

#Gather job results
output <- get_slurm_out(job1, outtype = "raw")
results_2 <- bind_rows(output)

#Document time
tf<-Sys.time()
tf-t0

#Cleanup jobs
cleanup_files(job2)

#Save backup
save.image("historic_results.RDATA")
