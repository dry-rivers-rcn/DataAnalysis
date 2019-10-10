#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Watershed Analyis
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 10/8/2019
#Purpose: Estimate aggregate watershed attributes for Gage Analysis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup Workspace------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory (delete this once incoporated into larger workflow)
rm(list=ls())

#download relevant packages
library(raster)
library(sf)
library(fasterize)
library(parallel)
library(tidyverse)

#Define data directories
data_dir<-"/nfs/njones-data/Research Projects/DryRiversRCN/spatial_data/"
  #This directory contains:
  # (1)  Watershed Shapefiles Obtained from John Hammond (during the DryRiversRCN)
  # (2) Depth to bedrock: https://doi.org/10.1371/journal.pone.0169748

#Bring spatial data into R environmnet
sheds<-sf::st_read(paste0(data_dir, "all_conus.shp"))
bedrock_depth<-raster::raster(paste0(data_dir,'BDTICM_M_250m_ll.tif'))

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Extract data---------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create function to extract values from large raster function
extracterize<-function(r, p, uid, fun=function(x){mean(x, na.rm=T)}){

  #reproject polygon 
  p<-st_transform(p, crs=r@crs)
  
  #convert ws_poly to to a grid of points
  p_grd<-fasterize::fasterize(p, crop(r, p))
  p_pnt<-rasterToPoints(p_grd) %>% 
    as_tibble() %>% 
    st_as_sf(., coords = c("x", "y"), crs = r@crs) %>% 
    as_Spatial(.)
  
  #Extract values at points
  p_values <- raster::extract(r, p_pnt)
  
  #Apply function
  result<- fun(p_values)
  
  #Create output
  output<-tibble(
    uid,
    value = result
  )
  
  #Export Output
  output
}

#Create wrapper for extraction function
fun<-function(n){
  extracterize(r = bedrock_depth, 
               p = sheds[n,],
               uid = sheds$gage_num[n])
}

#Apply function -- smoke em [i.e. multiple cores] if you got em!
output<-mclapply(seq(1,nrow(sheds)), fun, mc.cores = detectCores()) %>% bind_rows(.)
