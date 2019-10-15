#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: LULC Timeseries
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 10/15/2019
#Purpose: Estimate LULC Timeseries from 1938 to present for USGS gagesII dataset
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
data_dir<-"/nfs/njones-data/Research Projects/DryRiversRCN/spatial_data/USGS_LULC"
results_dir<-"/nfs/njones-data/Research Projects/DryRiversRCN/results/"

#This directory contains:
# (1) Watershed Shapefiles Obtained from John Hammond (during the DryRiversRCN)
# (2) Hindcasted LULC 1938-1992: https://doi.org/10.5066/F7KK99RR
#        web address: https://www.sciencebase.gov/catalog/item/59d3c73de4b05fe04cc3d1d1
# (3) Historic LULC:1992 to 2006: https://doi.org/10.5066/P95AK9HP
#        web address: https://www.sciencebase.gov/catalog/item/5b96c2f9e4b0702d0e826f6d
