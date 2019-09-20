#Things I am planning to revise for next version:
 # Some trends not computed where NA rather than 0 represents 0 no flow days. Need to redo code so that if there are 0 no flow days, a 0 is used

library(dplyr)
library(lubridate)
library(stringr)

# set working directory containing drainage area file 
setwd("/Users/johnhammond/Desktop/DRRCN_trends/")
# read in file with drainage areas of each USGS gage
drain <- read.csv("all_CONUS_info.csv")
# set working directory containing individual gage time series 
setwd("/Users/johnhammond/Desktop/DRRCN_trends/zero_flow_sites/daily_data_just_flow")
# make a list of all of the individual gage files
files <- dir(pattern=".csv")

# run loop over individual gage files, try wrapper supresses errors to allow code to progress if there is a problem with data at one site, will not return false values, just NA
for(i in seq_along(files)){
  # set working directory containing individual gage time series files
  setwd("/Users/johnhammond/Desktop/DRRCN_trends/zero_flow_sites/daily_data_just_flow")
  # test code on randon time series
  # i = 835 
  # i = 1
  # get the current site number
  try(site <- as.numeric(gsub(".csv","", files[i])))
  # usgs sites if less than 8 digits have leading 0's
  try(sitewith0 <- str_pad(site, 8, pad = "0"))
  #read in the current site data
  try(sf <- read.csv(files[i], stringsAsFactors = FALSE))
  # get the year, month, yday and water year of each date
  try(sf$month <- month(sf$Date))
  try(sf$year <- year(sf$Date))
  try(sf$yday <- yday(sf$Date))
  try(sf$wday <- ifelse(sf$yday > 274,  sf$yday - 273, sf$yday + 92))
  try(sf$wyear <- ifelse(sf$month >9, sf$year +1, sf$year))
  try(sf$wyear <- as.numeric(sf$wyear))
  #get log of streamflow (some proposed analyses require this)
  try(sf$lnqcfs <- sf$X_00060_00003)
  try(sf$lnqcfs[sf$lnqcfs == 0] <- 0.000000001)
  try(sf$lnqcfs <- log(sf$lnqcfs))
  # get area normalized flow using drainage area
  try(drainagearea <- subset(drain, drain$site_no == site))
  try(drainagearea <- drainagearea$drain_area_va*2.5999)
  try(sf$mmd <- 1000*(sf$X_00060_00003*0.028316847*86400)/(drainagearea*1000000))
  # see how many years of data missing less than 10% daily values, only want to use those years of data
  try(dailydatacounts <- sf %>% group_by(wyear) %>% summarise(count = sum(!is.na(X_00060_00003))))
  try(dailydatafull <- subset(dailydatacounts, dailydatacounts$count>330))
  try(wyearsdata <- as.numeric(NROW(dailydatafull)))
  try(wyears <- unique(dailydatafull$wyear))
  
  for(n in seq_along(wyears)) {
    # try example year
    # n = 5
    try(datayear <- subset(sf, sf$wyear == wyears[n]))
    try(currentwyear <- wyears[n])
    try(calyear <- wyears[n]-1)
    try(datacalyear <- subset(sf, sf$year == calyear))
    # calculate days with flow (and data) by season (jja, son, djf, mam)
    
    try(jja <- subset(datayear, datayear$month > 5 & datayear$month < 9))
    try(son <- subset(datacalyear, datacalyear$month > 8 & datacalyear$month < 12))
    try(d <- subset(datayear, datayear$month == 12 ))
    try(j <- subset(datayear, datayear$month == 1 ))
    try(f <- subset(datayear, datayear$month == 2 ))
    try(djf <- rbind(d,j,f))
    try(mam <- subset(datayear, datayear$month > 2 & datayear$month < 6))
    
    try(jjaflow <- as.numeric(length(which(jja$X_00060_00003 > 0))))
    try(sonflow <- as.numeric(length(which(son$X_00060_00003 > 0))))
    try(djfflow <- as.numeric(length(which(djf$X_00060_00003 > 0))))
    try(mamflow <- as.numeric(length(which(mam$X_00060_00003 > 0))))
    
    try(jjanoflow <- as.numeric(length(which(jja$X_00060_00003 == 0))))
    try(sonnoflow <- as.numeric(length(which(son$X_00060_00003 == 0))))
    try(djfnoflow <- as.numeric(length(which(djf$X_00060_00003 == 0))))
    try(mamnoflow <- as.numeric(length(which(mam$X_00060_00003 == 0))))
    
    try(jjacount <- as.numeric(sum(!is.na(jja$X_00060_00003))))
    try(soncount <- as.numeric(sum(!is.na(son$X_00060_00003))))
    try(djfcount <- as.numeric(sum(!is.na(djf$X_00060_00003))))
    try(mamcount <- as.numeric(sum(!is.na(mam$X_00060_00003))))
    
    try(totaldataperwyear <- jjacount+soncount+djfcount+mamcount)
    try(totalflowperwyear <- jjaflow+sonflow+djfflow+mamflow)
    try(totalnoflowperwyear <- jjanoflow+sonnoflow+djfnoflow+mamnoflow)
    
    try(annualfractionnoflow <- totalnoflowperwyear/totaldataperwyear)
    try(jjafractionnoflow <- jjanoflow/jjacount)
    try(sonfractionnoflow <- sonnoflow/soncount)
    try(djffractionnoflow <- djfnoflow/djfcount)
    try(mamfractionnoflow <- mamnoflow/mamcount)
    
    #day of water year with peak flow
    try(peakflow <- as.numeric(max(datayear$X_00060_00003, na.rm = TRUE)))
    try(peakflowdate <- as.numeric(which.max(datayear$X_00060_00003)))
    try(peakflowmmd <- as.numeric(max(datayear$mmd, na.rm = TRUE)))
    try(peakdatewy <- as.numeric(datayear$wday[peakflowdate]))
    
    #days of water year with lowest flow
    
    try(lowflow <- as.numeric(min(datayear$X_00060_00003, na.rm = TRUE)))
    try(lowflowdate <- as.numeric(which.min(datayear$X_00060_00003)))
    try(lowflowmmd <- as.numeric(min(datayear$mmd, na.rm = TRUE)))
    try(lowdatewy <- as.numeric(datayear$wday[lowflowdate]))
    
    try(zeroflowfirst <- as.numeric(which(datayear$X_00060_00003 == 0)[1]))
    try(zeroflowcentroiddate <- as.numeric(mean(which(datayear$X_00060_00003 == 0), na.rm = TRUE)))
    
    try(zeroflowfirstjja <- as.numeric(which(jja$X_00060_00003 == 0)[1]))
    try(zeroflowcentroiddatejja <- as.numeric(mean(which(jja$X_00060_00003 == 0), na.rm = TRUE)))
    
    try(zeroflowfirstson <- as.numeric(which(son$X_00060_00003 == 0)[1]))
    try(zeroflowcentroiddateson <- as.numeric(mean(which(son$X_00060_00003 == 0), na.rm = TRUE)))
    
    try(zeroflowfirstdjf <- as.numeric(which(djf$X_00060_00003 == 0)[1]))
    try(zeroflowcentroiddatedjf <- as.numeric(mean(which(djf$X_00060_00003 == 0), na.rm = TRUE)))
    
    try(zeroflowfirstmam <- as.numeric(which(mam$X_00060_00003 == 0)[1]))
    try(zeroflowcentroiddatemam <- as.numeric(mean(which(mam$X_00060_00003 == 0), na.rm = TRUE)))
    
    #area-normalized annual water yield
    
    try(area_norm_yield_mm <- sum(datayear$mmd, na.rm = TRUE))
    try(area_norm_yield_mm_10_percent <- 0.1*area_norm_yield_mm)
    try(area_norm_yield_mm_50_percent <- 0.5*area_norm_yield_mm)
    try(area_norm_yield_mm_90_percent <- 0.9*area_norm_yield_mm)
    
    try(datayear$cum_mmd <- cumsum(datayear$mmd))
    
    try(cumdist10days <- as.numeric(which(datayear$cum_mmd>area_norm_yield_mm_10_percent)[1]))
    try(cumdist50days <- as.numeric(which(datayear$cum_mmd>area_norm_yield_mm_50_percent)[1]))
    try(cumdist90days <- as.numeric(which(datayear$cum_mmd>area_norm_yield_mm_90_percent)[1]))
    
    #number of discrete periods of flow/no flow
    #length of each period of flow
    
    try(datayear$flowbinary <- ifelse(datayear$X_00060_00003>0,1,0))
    try(flow.rle  <- rle(datayear$flowbinary))
    try(flow.values <- as.data.frame(flow.rle$values))
    try(flow.values$lengths <- flow.rle$lengths)
    try(colnames(flow.values) <- c("value","length"))
    try(flowperiods <- subset(flow.values, flow.values$value == 1))
    try(noflowperiods <- subset(flow.values, flow.values$value == 0))
    
    try(totalflowperiods <- as.numeric(length(flowperiods$length)))
    try(meanlengthflow <- as.numeric(mean(flowperiods$length)))
    try(maxlengthflow <- as.numeric(max(flowperiods$length)))
    try(minlengthflow <- as.numeric(min(flowperiods$length)))
    try(medianlengthflow <- as.numeric(median(flowperiods$length)))
    
    try(totalnoflowperiods <- as.numeric(length(noflowperiods$length)))
    try(meanlengthnoflow <- as.numeric(mean(noflowperiods$length)))
    try(maxlengthnoflow <- as.numeric(max(noflowperiods$length)))
    try(minlengthnoflow <- as.numeric(min(noflowperiods$length)))
    try(medianlengthnoflow <- as.numeric(median(noflowperiods$length)))
    
    # coefficient of variations
    try(cvlengthflow <- sd(flowperiods$length, na.rm = TRUE)/meanlengthflow )
    try(cvlengthnoflow <- sd(noflowperiods$length, na.rm = TRUE)/meanlengthnoflow )
    
    
    
    
    # make blank table and then fill in....
    
    try( metrics <- data.frame(matrix(nrow = 1, ncol = 56)))
    try(colnames(metrics) <- c("sitewith0","currentwyear","drainagearea",    "wyearsdata",    "jjaflow",    "sonflow",    "djfflow",    "mamflow",    "jjanoflow",    "sonnoflow",    "djfnoflow",    "mamnoflow",    "jjacount",    "soncount",    "djfcount",    "mamcount","totaldataperwyear",    "totalflowperwyear","totalnoflowperwyear",    "annualfractionnoflow",    "jjafractionnoflow",    "sonfractionnoflow",    "djffractionnoflow",    "mamfractionnoflow",    "peakflow",    "peakflowmmd",    "peakdatewy",    "lowflow",    "lowflowmmd",    "lowdatewy",    "zeroflowfirst",    "zeroflowcentroiddate",    "zeroflowfirstjja",    "zeroflowcentroiddatejja","zeroflowfirstson",    "zeroflowcentroiddateson",    "zeroflowfirstdjf",    "zeroflowcentroiddatedjf",    "zeroflowfirstmam",    "zeroflowcentroiddatemam",    "area_norm_yield_mm",    "cumdist10days",    "cumdist50days",    "cumdist90days",  "totalflowperiods",    "meanlengthflow",    "maxlengthflow",    "minlengthflow",    "medianlengthflow",    "totalnoflowperiods",    "meanlengthnoflow",    "maxlengthnoflow",    "minlengthnoflow",    "medianlengthnoflow",    "cvlengthflow","cvlengthnoflow"))
    try(metrics$sitewith0 <- sitewith0)
    try(metrics$currentwyear <- currentwyear)
    try(metrics$drainagearea <- drainagearea)
    try(metrics$wyearsdata <- wyearsdata)
    try(metrics$jjaflow <- jjaflow)
    try(metrics$sonflow <- sonflow)
    try(metrics$djfflow <- djfflow)
    try(metrics$mamflow <- mamflow)
    try(metrics$jjanoflow <- jjanoflow)
    try(metrics$sonnoflow <- sonnoflow)
    try(metrics$djfnoflow <- djfnoflow)
    try(metrics$mamnoflow <- mamnoflow)
    try(metrics$jjacount <- jjacount)
    try(metrics$soncount <- soncount)
    try(metrics$djfcount <- djfcount)
    try(metrics$mamcount <- mamcount)
    try(metrics$totaldataperwyear <- totaldataperwyear)
    try(metrics$totalflowperwyear <- totalflowperwyear)
    try(metrics$totalnoflowperwyear <- totalnoflowperwyear)
    try(metrics$annualfractionnoflow <- annualfractionnoflow)
    try(metrics$jjafractionnoflow <- jjafractionnoflow)
    try(metrics$sonfractionnoflow <- sonfractionnoflow)
    try(metrics$djffractionnoflow <- djffractionnoflow)
    try(metrics$mamfractionnoflow <- mamfractionnoflow)
    try(metrics$peakflow <- peakflow)
    try(metrics$peakflowmmd <- peakflowmmd)
    try(metrics$peakdatewy <- peakdatewy)
    try(metrics$lowflow <- lowflow)
    try(metrics$lowflowmmd <- lowflowmmd)
    try(metrics$lowdatewy <- lowdatewy)
    try(metrics$zeroflowfirst <- zeroflowfirst)
    try(metrics$zeroflowcentroiddate <- zeroflowcentroiddate)
    try(metrics$zeroflowfirstjja <- zeroflowfirstjja)
    try(metrics$zeroflowcentroiddatejja <- zeroflowcentroiddatejja)
    try(metrics$zeroflowfirstson <- zeroflowfirstson)
    try(metrics$zeroflowcentroiddateson <- zeroflowcentroiddateson)
    try(metrics$zeroflowfirstdjf <- zeroflowfirstdjf)
    try(metrics$zeroflowcentroiddatedjf <- zeroflowcentroiddatedjf)
    try(metrics$zeroflowfirstmam <- zeroflowfirstmam)
    try(metrics$zeroflowcentroiddatemam <- zeroflowcentroiddatemam)
    try(metrics$area_norm_yield_mm <- area_norm_yield_mm)
    try(metrics$cumdist10days <- cumdist10days)
    try(metrics$cumdist50days <- cumdist50days)
    try(metrics$cumdist90days <- cumdist90days)
    try(metrics$totalflowperiods <- totalflowperiods)
    try(metrics$meanlengthflow <- meanlengthflow)
    try(metrics$maxlengthflow <- maxlengthflow)
    try(metrics$minlengthflow <- minlengthflow)
    try(metrics$medianlengthflow <- medianlengthflow)
    try(metrics$totalnoflowperiods <- totalnoflowperiods)
    try(metrics$meanlengthnoflow <- meanlengthnoflow)
    try(metrics$maxlengthnoflow <- maxlengthnoflow)
    try(metrics$minlengthnoflow <- minlengthnoflow)
    try(metrics$medianlengthnoflow <- medianlengthnoflow)
    try(metrics$cvlengthflow <- cvlengthflow)
    try(metrics$cvlengthnoflow <- cvlengthnoflow)
    
    # set working directory to write a file for each water year and each gage
    setwd("/Users/johnhammond/Desktop/DRRCN_trends/zero_flow_sites/annual_no_flow_metrics_081419")
    
    try(write.csv(metrics, paste(sitewith0,".",wyears[n],".annual.metrics.summary.csv",sep = "")))
    
  }
}


##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################

# first, summarize WY P,T,PET to join to this table..... that and gages 2 attributes.....

setwd("/Users/johnhammond/Desktop/DRRCN_trends/zero_flow_sites/daily_data_with_climate_and_PET")
# make a list of all of the individual gage files
files <- dir(pattern=".csv")

# run loop over individual gage files, try wrapper supresses errors to allow code to progress if there is a problem with data at one site, will not return false values, just NA
for(i in seq_along(files)){
  # set working directory containing individual gage time series files
  setwd("/Users/johnhammond/Desktop/DRRCN_trends/zero_flow_sites/daily_data_with_climate_and_PET")
  # test code on randon time series
  # i = 835 
  # i = 1
  # get the current site number
  try(site <- as.numeric(gsub(".csv","", files[i])))
  # usgs sites if less than 8 digits have leading 0's
  try(sitewith0 <- str_pad(site, 8, pad = "0"))
  #read in the current site data
  try(sf <- read.csv(files[i], stringsAsFactors = FALSE))
  # get the year, month, yday and water year of each date
  try(sf$month <- month(sf$Date))
  try(sf$year <- year(sf$Date))
  try(sf$yday <- yday(sf$Date))
  try(sf$wday <- ifelse(sf$yday > 274,  sf$yday - 273, sf$yday + 92))
  try(sf$wyear <- ifelse(sf$month >9, sf$year +1, sf$year))
  try(sf$wyear <- as.numeric(sf$wyear))
  try(wyears <- unique(sf$wyear))
  
  colnames(sf)[10] <- "PET_mm"
  
  try(sf <- subset(sf, sf$year > 1981 & sf$year <2017))
  
  for(n in seq_along(wyears)) {
    # try example year
    # n = 5
    try(datayear <- subset(sf, sf$wyear == wyears[n]))
    try(currentwyear <- wyears[n])
    try(calyear <- wyears[n]-1)
    try(datacalyear <- subset(sf, sf$year == calyear))
    # calculate days with flow (and data) by season (jja, son, djf, mam)
    
    try(jja <- subset(datayear, datayear$month > 5 & datayear$month < 9))
    try(son <- subset(datacalyear, datacalyear$month > 8 & datacalyear$month < 12))
    try(d <- subset(datayear, datayear$month == 12 ))
    try(j <- subset(datayear, datayear$month == 1 ))
    try(f <- subset(datayear, datayear$month == 2 ))
    try(djf <- rbind(d,j,f))
    try(mam <- subset(datayear, datayear$month > 2 & datayear$month < 6))
    
    # annual total P, jja son djf mam total p
    
    try(p_mm_wy <- sum(datayear$P_mm, na.rm = TRUE))
    try(p_mm_jja <- sum(jja$P_mm, na.rm = TRUE))
    try(p_mm_son <- sum(son$P_mm, na.rm = TRUE))
    try(p_mm_djf <- sum(djf$P_mm, na.rm = TRUE))
    try(p_mm_mam <- sum(mam$P_mm, na.rm = TRUE))
    
    # annual total P, jja son djf mam total p
    
    try(pet_mm_wy <- sum(datayear$PET_mm, na.rm = TRUE))
    try(pet_mm_jja <- sum(jja$PET_mm, na.rm = TRUE))
    try(pet_mm_son <- sum(son$PET_mm, na.rm = TRUE))
    try(pet_mm_djf <- sum(djf$PET_mm, na.rm = TRUE))
    try(pet_mm_mam <- sum(mam$PET_mm, na.rm = TRUE))
    
    try(T_max_c_wy <- mean(datayear$tmax_C, na.rm = TRUE))
    try(T_max_c_jja <- mean(jja$tmax_C, na.rm = TRUE))
    try(T_max_c_son <- mean(son$tmax_C, na.rm = TRUE))
    try(T_max_c_djf <- mean(djf$tmax_C, na.rm = TRUE))
    try(T_max_c_mam <- mean(mam$tmax_C, na.rm = TRUE))
    
    try(T_min_c_wy <- mean(datayear$tmin_C, na.rm = TRUE))
    try(T_min_c_jja <- mean(jja$tmin_C, na.rm = TRUE))
    try(T_min_c_son <- mean(son$tmin_C, na.rm = TRUE))
    try(T_min_c_djf <- mean(djf$tmin_C, na.rm = TRUE))
    try(T_min_c_mam <- mean(mam$tmin_C, na.rm = TRUE))
    
    try(datayear$cum_pmm <- cumsum(datayear$P_mm))
    
    p_mm_10_percent <- p_mm_wy*0.1
    p_mm_50_percent <- p_mm_wy*0.5
    p_mm_90_percent <- p_mm_wy*0.9
    
    try(pcumdist10days <- as.numeric(which(datayear$cum_pmm>p_mm_10_percent)[1]))
    try(pcumdist50days <- as.numeric(which(datayear$cum_pmm>p_mm_50_percent)[1]))
    try(pcumdist90days <- as.numeric(which(datayear$cum_pmm>p_mm_90_percent)[1]))
    
    # make blank table and then fill in....
    
    try( metrics <- data.frame(matrix(nrow = 1, ncol = 25)))
    try(colnames(metrics) <- c("sitewith0",
                               "currentwyear",
                               "p_mm_wy",
                               "p_mm_jja",
                               "p_mm_son",
                               "p_mm_djf",
                               "p_mm_mam",
                               "pet_mm_wy",
                               "pet_mm_jja",
                               "pet_mm_son",
                               "pet_mm_djf",
                               "pet_mm_mam",
                               "T_max_c_wy",
                               "T_max_c_jja",
                               "T_max_c_son",
                               "T_max_c_djf",
                               "T_max_c_mam",
                               "T_min_c_wy",
                               "T_min_c_jja",
                               "T_min_c_son",
                               "T_min_c_djf",
                               "T_min_c_mam",
                               "pcumdist10days",
                               "pcumdist50days",
                               "pcumdist90days"))
   
    try(metrics$sitewith0 <- sitewith0)
    try(metrics$currentwyear <- currentwyear)
    try(metrics$p_mm_wy <- p_mm_wy)
    try(metrics$p_mm_jja <- p_mm_jja)
    try(metrics$p_mm_son <- p_mm_son)
    try(metrics$p_mm_djf <- p_mm_djf)
    try(metrics$p_mm_mam <- p_mm_mam)
    try(metrics$pet_mm_wy <- pet_mm_wy)
    try(metrics$pet_mm_jja <- pet_mm_jja)
    try(metrics$pet_mm_son <- pet_mm_son)
    try(metrics$pet_mm_djf <- pet_mm_djf)
    try(metrics$pet_mm_mam <- pet_mm_mam)
    try(metrics$T_max_c_wy <- T_max_c_wy)
    try(metrics$T_max_c_jja <- T_max_c_jja)
    try(metrics$T_max_c_son <- T_max_c_son)
    try(metrics$T_max_c_djf <- T_max_c_djf)
    try(metrics$T_max_c_mam <- T_max_c_mam)
    try(metrics$T_min_c_wy <- T_min_c_wy)
    try(metrics$T_min_c_jja <- T_min_c_jja)
    try(metrics$T_min_c_son <- T_min_c_son)
    try(metrics$T_min_c_djf <- T_min_c_djf)
    try(metrics$T_min_c_mam <- T_min_c_mam)
    try(metrics$pcumdist10days <- pcumdist10days)
    try(metrics$pcumdist50days <- pcumdist50days)
    try(metrics$pcumdist90days <- pcumdist90days)
    
    
    # set working directory to write a file for each water year and each gage
    setwd("/Users/johnhammond/Desktop/DRRCN_trends/zero_flow_sites/annual_climate_aggregation")
    
    try(write.csv(metrics, paste(sitewith0,".",wyears[n],".annual.metrics.summary.csv",sep = "")))
    
  }
}

##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################


# Then pull together and merge these datasets into annual, annual with climat, mean annual with climate files

setwd("/Users/johnhammond/Desktop/DRRCN_trends/zero_flow_sites/annual_no_flow_metrics_081419")

allgage <- list.files(pattern = ".csv")
 
allgagedata <- do.call(rbind, lapply(allgage, read.csv))

setwd("/Users/johnhammond/Desktop/DRRCN_trends/zero_flow_sites/annual_climate_aggregation")

allclimate <- list.files(pattern = ".csv")

allclimatedata <- do.call(rbind, lapply(allclimate, read.csv))

setwd("/Users/johnhammond/Desktop/DRRCN_trends/zero_flow_sites/")

write.csv(allclimatedata, "annual_climate_metrics_for_no_flow_sites_081419.csv")

write.csv(allgagedata, "annual_no_flow_metrics_for_no_flow_sites_081419.csv")


##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################
##############################################################################

# Finally, run trends on those with 27 years data or more from 1980 - present..? - with climate data?

# merging no flow and climate metrics for WYs 1982-2016 a period of 35 years, good for trend analysis

setwd("/Users/johnhammond/Desktop/DRRCN_trends/zero_flow_sites/")

climate <- read.csv("annual_climate_metrics_for_no_flow_sites_081419.csv")
climate$siteyear <- paste(climate$sitewith0,climate$currentwyear, sep = "")
noflow <- read.csv("annual_no_flow_metrics_for_no_flow_sites_081419.csv")
noflow$siteyear <- paste(noflow$sitewith0,noflow$currentwyear, sep = "")

noflow <- subset(noflow, noflow$currentwyear>1981 & noflow$currentwyear<2017)
climate <- subset(climate, climate$currentwyear>1981 & climate$currentwyear<2017)

noflowandclimate <- merge(climate,noflow, all.x = TRUE, by = "siteyear")


noflowandclimatemeanannual_mean <- noflowandclimate %>% group_by(sitewith0.x) %>% summarise_if(is.numeric, mean, na.rm = TRUE)
  
noflowandclimatemeanannual_std <- noflowandclimate %>% group_by(sitewith0.x) %>% summarise_if(is.numeric, sd, na.rm = TRUE)

noflowandclimatemeanannual_coefvar <- noflowandclimatemeanannual_mean/noflowandclimatemeanannual_std

write.csv(noflowandclimatemeanannual_mean, "mean_annual_no_flow_and_climate_metric_means_for_no_flow_sites_081519.csv")
write.csv(noflowandclimatemeanannual_coefvar, "mean_annual_no_flow_and_climate_metric_coefvars_for_no_flow_sites_081519.csv")


noflowandclimatetrends <- noflowandclimate[,c("sitewith0.x","p_mm_wy" ,                "p_mm_jja"            ,   
                                              "p_mm_son"   ,             "p_mm_djf"             ,   "p_mm_mam"            ,   
                                              "pet_mm_wy"   ,            "pet_mm_jja"           ,   "pet_mm_son"          ,   
                                              "pet_mm_djf"   ,           "pet_mm_mam"           ,   "T_max_c_wy"          ,   
                                              "T_max_c_jja"   ,          "T_max_c_son"          ,   "T_max_c_djf"         ,   
                                              "T_max_c_mam"    ,         "T_min_c_wy"           ,   "T_min_c_jja"         ,   
                                              "T_min_c_son"     ,        "T_min_c_djf"          ,   "T_min_c_mam"         ,   
                                              "pcumdist10days"   ,       "pcumdist50days"       ,   "pcumdist90days"      ,   
                                              "drainagearea"      ,      "wyearsdata"           ,  
                                              "jjaflow"           ,      "sonflow"              ,   "djfflow"             ,   
                                              "mamflow"           ,      "jjanoflow"            ,   "sonnoflow"           ,   
                                              "djfnoflow"         ,      "mamnoflow"            ,   "jjacount"            ,   
                                              "soncount"          ,      "djfcount"             ,   "mamcount"            ,   
                                              "totaldataperwyear"  ,     "totalflowperwyear"    ,   "totalnoflowperwyear" ,   
                                              "annualfractionnoflow",    "jjafractionnoflow"    ,   "sonfractionnoflow"    ,  
                                              "djffractionnoflow"   ,    "mamfractionnoflow"    ,   "peakflow"            ,   
                                              "peakflowmmd"         ,    "peakdatewy"           ,   "lowflow"             ,   
                                              "lowflowmmd"          ,    "lowdatewy"            ,   "zeroflowfirst"       ,   
                                              "zeroflowcentroiddate",    "zeroflowfirstjja"     ,   "zeroflowcentroiddatejja",
                                              "zeroflowfirstson"    ,    "zeroflowcentroiddateson", "zeroflowfirstdjf"      , 
                                              "zeroflowcentroiddatedjf", "zeroflowfirstmam"     ,   "zeroflowcentroiddatemam",
                                              "area_norm_yield_mm"    ,  "cumdist10days"        ,   "cumdist50days"          ,
                                              "cumdist90days"         ,  "totalflowperiods"     ,   "meanlengthflow"        , 
                                              "maxlengthflow"         ,  "minlengthflow"        ,   "medianlengthflow"      , 
                                              "totalnoflowperiods"    ,  "meanlengthnoflow"     ,   "maxlengthnoflow"       , 
                                              "minlengthnoflow"       ,  "medianlengthnoflow"   ,   "cvlengthflow"          , 
                                              "cvlengthnoflow")]

library(Kendall)
library(trend)
library(plyr)
library(PerformanceAnalytics)
library(arrayhelpers)
library(lubridate)
library(zyp)
library(stringr)
library(data.table)
library(dplyr)
library(jsonlite)



sites <- unique(noflowandclimatetrends$sitewith0.x)


###################### prepare geoknife ecoregion monthly data

for(i in seq_along(sites)){
  # i = 10
  
  current <- subset(noflowandclimatetrends, noflowandclimatetrends$sitewith0.x == sites[i])
  
  current[current==-Inf] <- NA
  current[current==Inf] <- NA
  
  results <- data.frame(matrix(NA, nrow = 77, ncol = 3))
  rownames(results) <- colnames(noflowandclimatetrends[2:78])
  colnames(results) <- c("tau","pval","slope")
  
for(c in 2:78){
  
  # c = 30
  currentcolumnname <- colnames(current)[c]
  currentcolumn <- as.data.frame(current[,c])
  colnames(currentcolumn) <- "variable"
  
  currentcolumn$wyear <- 1982:2016
  
  nacounts <-  as.numeric(sum(is.na(currentcolumn$variable)))
  
  if(nacounts < 6){
  
    manken <- MannKendall(currentcolumn$variable)
    sen <- zyp.sen(variable~wyear, currentcolumn)
    tau <- manken$tau
    pval <- manken$sl
    
    output <- c(tau,pval,sen$coefficients[2])
   
    results[(c-1),] <- output
    

  } 
  
  
}
  
  results$sigslope <- ifelse(results$pval < 0.05, results$slope, NA)
  results <- as.data.frame(results[,"sigslope"])
  colnames(results) <- sites[i]
  rownames(results) <- colnames(noflowandclimatetrends[2:78])
  
  setwd("/Users/johnhammond/Desktop/DRRCN_trends/zero_flow_sites/trends")
  
  write.csv(results, paste(sites[i],".trend.summary.csv",sep = ""))
  
}

##################################################################
##################################################################
##################################################################
##################################################################
##################################################################
##################################################################
##################################################################
##################################################################
##################################################################

setwd("/Users/johnhammond/Desktop/DRRCN_trends/zero_flow_sites/trends")

# merge annual climate and no flow

all <- list.files(pattern = ".csv")

allclimatedata <- do.call(cbind, lapply(all, read.csv))

setwd("/Users/johnhammond/Desktop/DRRCN_trends/zero_flow_sites/")

write.csv(allclimatedata, "trends_in_no_flow_and_climate_for_no_flow_sites_30_plus_years_081619.csv.csv")
# merge no flow annual metrics, climate, info to one mean annual file






##################################################################
##################################################################
##################################################################
##################################################################
##################################################################
##################################################################
##################################################################
##################################################################
##################################################################
# 
# setwd("/Users/johnhammond/Desktop/DRRCN_trends/zero_flow_sites")
# 
# trends <- read.csv("trends_in_no_flow_and_climate_for_no_flow_sites_30_plus_years_081619.csv")
# mean_annual_mean <- read.csv("mean_annual_no_flow_and_climate_metric_means_for_no_flow_sites_081519.csv")
# mean_annual_coefvar <- read.csv("mean_annual_no_flow_and_climate_metric_coefvars_for_no_flow_sites_081519.csv")
# mean_annual_coefvar$sitewith0.x <- mean_annual_mean$sitewith0.x
# 
# write.csv(mean_annual_coefvar, "mean_annual_no_flow_and_climate_metric_coefvars_for_no_flow_sites_081519.csv")
# 
# trendcolnames <- as.character(trends$X)
# 
# trends <- trends[,c(2,seq(3,1819,2))]
# trends <- as.data.frame(t(trends))
# colnames(trends) <- trendcolnames
# trends <- trends[-1,]
# trends$site <- gsub("X","",row.names(trends)) 
# 
# write.csv(trends, "trends_in_no_flow_and_climate_for_no_flow_sites_30_plus_years_081619.csv")
# 
