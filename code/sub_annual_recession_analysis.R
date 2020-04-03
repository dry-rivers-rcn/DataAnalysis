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
library(foreign)
library(tidyverse)

#Define data dir
data_dir<-"C:\\DryRiversRCN_Analysis/IRES metrics-zero-no-zero- NSF RCN/daily_data_with_climate_and_PET/"

#Define list of USGS Gages from John's data files
gage_list<-list.files(data_dir) %>% 
  substr(., 1, nchar(.)-4)

#Test gages
#gage_list<-c("01110000","02294491", '06404998', "08086212","11159200", "13152500")

#Download gage data
gage_info<-read.dbf("C:\\DryRiversRCN_Analysis/gagesII/gagesII_9322_sept30_2011.dbf")

#Organize gage info
gage_info<-gage_info %>% select(STAID, DRAIN_SQKM, HUC02, AGGECOREGI)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#2.0 Organize flow data into long format----------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create download function
download_fun<-function(n){
  
  #Identify gage 
  gage<-gage_list[n]
  
  #download data
  ts<-read_csv(paste0(data_dir, gage, ".csv")) %>% 
    mutate(site_no = gage) %>%
    rename(Q_cfs = X_00060_00003) %>% 
    select(site_no, Date, Q_cfs) 
  
  #Check for errors
  if(is.double(ts$Q_cfs)==FALSE){
    ts<-tibble(
      site_no = gage, 
      Date = as.Date("1900-1-01"), 
      Q_cfs = -9999
    )
  }
  
  #Export data
  ts
}

#Apply function and bind rows
flow<-lapply(seq(1, length(gage_list)), download_fun) 
flow<-flow %>% bind_rows()

#Filter -9999 values out
flow<-flow %>% filter(Q_cfs>-9999)

gage_list <- unique(flow$site_no)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Create function to isolate inividual storms--------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create function based on site_no
fun<-function(n){

#Identify gage
gage_id<-gage_list[n]

#reduce flow to gage of interest
df<-flow %>% filter(site_no == gage_id) %>% as_tibble()

#Clean data
df<-df %>% 
  #Remove NA
  drop_na(Q_cfs) %>% 
  #order by date
  arrange(Date)

#Isolate individual recession events
df<-df %>% 
  #Estiamte change in flow (dq/dt)
  mutate(dQ = Q_cfs-lag(Q_cfs)) %>% 
  #Remove rising limb
  filter(dQ<0) %>% 
  #Estimate change in time
  mutate(dt = as.numeric(paste(Date - lag(Date)))) %>% 
  drop_na(dt) %>% 
  #Define individual events
  mutate(event = if_else(dt ==1, 0, 1),
         event_id = cumsum(event) + 1) %>% 
  #Remove event and dt collumns
  select(-c(dt, event))
  
#Create inner function to estimate recession metrics 
inner_fun<-function(m){
  #Subset to individual event
  r<-df %>% filter(event_id==m) 
  
  #Change sign on dQ/dt (note, dQ==dQ/dt here because dt is one day!)
  r<-r %>% mutate(dQ=-1*dQ)
  
  #Remove events < 4 days
  if(nrow(r)>=5){
  
    #remove first day
    r<-r[-1,]
    
    #Create linear model in log-log space
    model<-lm(log(dQ)~log(Q_cfs+0.001), data=r)
    
    #Create output
    inner_output<-tibble(
      event_id = r$event_id[1],
      dur = nrow(r)+1,
      date_mean = mean(as.POSIXlt(r$Date, "%Y/%m/%d")$yday),
      Q_mean = mean(r$Q_cfs),
      log_a = model$coefficients[1],
      b = model$coefficients[2],
      rsq = summary(model)$r.squared)
  }else{
    inner_output<-tibble(
      event_id = r$event_id[1],
      dur = nrow(r),
      Q_mean = mean(r$Q_cfs),
      log_a = NA,
      b = NA,
      rsq = NA)
  }
  
  #Return output
  inner_output
}

#Execute inner function
output<-lapply(seq(1,max(df$event_id, na.rm=T)), inner_fun) %>% 
  bind_rows() %>% 
  drop_na() %>% 
  mutate(site_no = df$site_no[1])

#Export 
output
}

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#4.0 Run f(x) in parralel-----------------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create dummy function to catch erros 
execute<-function(a){
  tryCatch(fun(a), error=function(e) NULL)
}

#Start recording time 
t0<-Sys.time()

#Determine the number of cores on the machine
n.cores<-detectCores()

#Create Clusters
cl <- makePSOCKcluster(n.cores) 

#Export libraries to clusers
clusterEvalQ(cl, library(tidyverse))  

#Export function to cluser
clusterExport(cl, c('fun', 'gage_list', 'flow'), env=.GlobalEnv)  

#Execute function on cluster
x<-parLapply(cl, seq(1,length(gage_list)), execute) 

#Stop clusters
stopCluster(cl)  #Turn clusters off

#Record time
tf<-Sys.time()
tf-t0

#Gather data
recession<-x %>% bind_rows()


