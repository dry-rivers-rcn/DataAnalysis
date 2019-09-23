#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Title: Recession Analyis
#Coder: Nate Jones (cnjones7@ua.edu)
#Date: 9/21/2019
#Purpose: Begin exploring recession analysis accross IRES flow data
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

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

#Download flow data from NWIS
# flow<-readNWISdv(siteNumbers = gage_list, parameterCd = c("00060"))
# 
# #Organize flow data
# flow <- flow %>% 
#   #convert to tibble
#   as_tibble() %>% 
#   #rename collumns
#   rename(date = Date, 
#          Q_cfs = X_00060_00003) %>% 
#   #select collumns of interest
#   select(site_no, date, Q_cfs)

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Create function to isolate inividual storms--------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create function to conduct recession analysis
recession_fun<-function(n){
  
  #Clean data
  df<-flow[[n]] %>%
    as_tibble(.) %>% 
    #Remove NA
    drop_na(Q_cfs) %>% 
    #order by date
    arrange(Date)
  
  if(df$Q_cfs[1]!=-9999){
    
    #Isolate gage watershed information
    site_no<-unique(df$site_no)
    info<-gage_info %>% filter(STAID == site_no)
    
    #Convert to cm/day
    df$q_cm<-df$Q_cfs/info$DRAIN_SQKM*2.83168466e-6*86400
    
    #Estimate baseflow using EcoHydRology package
    df$q_bf<- BaseflowSeparation(df$q_cm)$bt
    
    #Estimate dQ/dt
    df<- df %>%
      mutate(dq = q_bf - lag(q_bf)) %>%
      filter(dq<0) %>%
      filter(q_bf>0)
      
    #Create model to esitmate a and b
    model<-lm(log10(dq*-1)~log10(q_cm), data=df)
    
    #Add model data to output
    df<-df %>% 
      mutate(log_q0 = model$coefficients[1], 
             log_k = model$coefficients[2],
             p_value = summary(model)$coefficients[8])
  }else{
    df<-df %>% 
      mutate( 
        q_cm   = -9999, 
        q_bf   = -9999, 
        dq     = -9999, 
        log_q0 = -9999, 
        log_k  = -9999, 
        p_value = -9999)
  }
  
  #Export 
  df
}
  
#Execute function (use mcapply for larger dataset)
output<-lapply(seq(1, length(flow)), recession_fun) %>% bind_rows()

#split output for plotting and output
df<-output %>% select(site_no, Date, q_bf, dq)
output<-output %>% 
  select(site_no, log_k, log_q0, p_value) %>% 
  distinct(.) %>%
  mutate(k = 10^log_k) %>% 
  rename(STAID = site_no) %>% 
  left_join(., gage_info)


#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#3.0 Plot Recession Plots for Funzies-------------------------------------------
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#Create function to plot points plus model (This really should be done in ggplot)
plot_fun<-function(gage){
  #Identify data of interest
  ts <- df %>% filter(site_no == gage)
  
  #Identify recession coef
  k<-output$k[output$STAID==gage]
  
  #Set plot parameters
  par(mar=c(3.5,3.5,2.5,0.5))
  par(mgp=c(2,0.5,0))
  par(ps=12)
  
  #Create model
  model<-lm(log10(ts$dq*-1)~log10(ts$q_bf))
  
  #Plot blank plot
  plot(ts$q_bf, ts$dq*-1, log="xy", type="n",
       xlim = c(2e-8, 5e-1),
       ylim = c(1e-10, 1),
       xlab = "Baseflow [cm/day]", 
       ylab = "dq/dt [cm/day^2]",
       main = paste("USGS Gage ",gage),
       cex.lab = 14/12, 
       cex.axis = 10/12)
  
  #Plot points
  points(ts$q_bf, ts$dq*-1, pch=19, col="grey70")
  
  #Plot model Line
  abline(model, lty=2, lwd=2, col="red")
  
  #add legend
  legend("topleft", paste("k=",round(k,digits=1)),  bty = "n")
  
  #add box
  box()
}

#Plot
par(mfrow=c(3,2))
for(i in 1:6){
  plot_fun(gage_list[i])
}
