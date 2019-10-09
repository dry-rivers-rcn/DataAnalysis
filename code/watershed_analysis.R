#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Watershed Analyis
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 10/8/2019
#Purpose: Estimate aggregate watershed attributes for Gage Analysis
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#Next steps, download depth to bedrock layer
#Estimate accross CONUS


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup Workspace------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory (delete this once incoporated into larger workflow)
rm(list=ls())

#download relevant packages
library(raster)
library(sf)
library(velox)
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
#Create function to extract data from 10 watersheds at a time
#fun<-function(n){
  
  #for testing 
  n<-1

  #Subshed sheds
  sheds_subset<-sheds[n,]
  
  #Convert to bedrock depth projection
  sheds_subset<-sf::st_transform(sheds_subset, crs=bedrock_depth@crs)

  #Crop bedrock raster
  b<-raster::crop(bedrock_depth, sheds)

  #Turn bedrock depth into velox object
  bedrock_depth_v<-velox::velox(bedrock_depth)

  #Convert sheds to sp (for velox extraction)
  sheds_sp<-sf::as_Spatial(sf::st_geometry(sheds),
                           IDs = as.character(sheds[["GAGE_ID"]]))


  #Velox extraction
  output<-bedrock_depth_v$extract(sheds_sp, fun = function(x){mean(x, na.rm=T)},  df = TRUE)
  
  #Export results
  output
}

#Apply function
sims<-seq(1, ceiling(nrow(sheds)/10))
