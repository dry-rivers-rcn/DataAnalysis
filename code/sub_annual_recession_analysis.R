#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Event-based Recession Analyis
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 9/21/2019
#Purpose: Begin exploring recession analysis accross IRES flow data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

#This is in process -- not ready for prime time!! :)


#Some reading:
  #https://agupubs.onlinelibrary.wiley.com/doi/full/10.1002/wrcr.20532


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#1.0 Setup Workspace------------------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Clear memory (delete this once incoporated into larger workflow)
rm(list=ls())

#download relevant packages
library(dataRetrieval)
library(EcoHydRology)
library(foreign)
library(tidyverse)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Create function to isolate inividual storms--------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




#fun<-function(data, gage, rise_threshold)

#for testing
data<- flow
gage<- '01110000'
storm_threshold<-0.5
bf_threshold<-0.05

#Identify individual storms

#Isolate data by gage number and order by date
df<-data %>%
  #isolate data (by site)
  filter(site_no == gage) %>% 
  #order by date
  arrange(date)

#Define flow thresholds based on input
storm_threshold<-quantile(df$q_cfs, storm_threshold)
bf_threshold<-quantile(df$q_cfs, bf_threshold)

#Identify local min and max
df<-df %>% 
  #Find local min and max
  mutate(local_max = if_else(lag(q_cfs) < q_cfs & lead(q_cfs) < q_cfs, 1, 0), 
         local_min = if_else(lag(q_cfs) > q_cfs & lead(q_cfs) > q_cfs, 1, 0)) %>% 
  #Delete local max below threshold
  mutate(local_max = if_else(q_cfs>storm_threshold, local_max, 0)) %>% 
  #Create unique ID's for local max
  mutate(peak_id = cumsum(local_max))

#Create inner function clip time series into individual hydrographs 
#Note, for now we're just clipping the recession. Later on, we 
#can add the rising limb if needed!
inner_fun<-function(storm_id){
  #filter df
  ts<-df %>% 
    #filter to storm of choice
    filter(peak_id==storm_id) %>% 
    #rename peak_id
    rename(storm_id = peak_id) %>% 
    #filter to data above flow threshold
    filter(q_cfs>bf_threshold) %>% 
    #select collumns of interest
    select(date, q_cfs, storm_id)
  
  #export ts
  ts
}

#Create list with individual storm data in each list element
storms<-lapply(seq(1, max(df$peak_id)), clip_fun)

#Create function to analyse each storm
#Note, this could be combined with the "inner_fun" above for speed, 
#but I broke it into two parts for now for ease.
analysis_fun<-function(n){
  #Isolate time series
  ts<-storms[[n]]
  
  #Analyze Time series
  ts<-ts %>% 
    mutate(t_day = date - min(date), 
           t_day = as.numeric(paste(t_day))+1)
  
  #Create linear model
  model<-lm(log(q_cfs)~log(t_day), data=ts)
  
}

