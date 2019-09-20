library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(dplyr)
library(scales)
library(maptools)
library(dataRetrieval)
library(wesanderson)

##################################################
setwd("C:\\Users\\jhammond\\Desktop\\DRRCN_081919_first_run\\csvs")


climate <- read.csv("annual_climate_metrics_for_no_flow_sites_081419.csv")
noflow <- read.csv("annual_no_flow_metrics_for_no_flow_sites_081419.csv")

climate$site_year <- paste(climate$sitewith0, climate$currentwyear)
noflow$site_year <-   paste(noflow$sitewith0, noflow$currentwyear)
  
climatenoflowannual <- merge(climate,noflow, by = "site_year")

write.csv(climatenoflowannual, "annual_no_flow_and_climate_metric_means_for_no_flow_sites_081919.csv")

# read in file with drainage areas of each USGS gage
info <- read.csv("all_CONUS_info.csv")
gages2 <- read.csv("common_metrics_of_interest_gages_2.csv")

trends <- read.csv("trends_in_no_flow_and_climate_for_no_flow_sites_30_plus_years_081619.csv")
mean_annual_mean <- read.csv("mean_annual_no_flow_and_climate_metric_means_for_no_flow_sites_081519.csv")
mean_annual_coefvar <- read.csv("mean_annual_no_flow_and_climate_metric_coefvars_for_no_flow_sites_081519.csv")

mean_annual_coefvar$sitewith0.x <- mean_annual_mean$sitewith0.x

# for plotting

trendswinfo <- merge(trends, info, by.x = "site", by.y = "site_no", all.x = TRUE)
meanswinfo <- merge(mean_annual_mean, info, by.x = "sitewith0.x", by.y = "site_no", all.x = TRUE)
coefvarswinfo <- merge(mean_annual_coefvar, info, by.x = "sitewith0.x", by.y = "site_no", all.x = TRUE)

# for writing

trendswinfowrite <- merge(trendswinfo, gages2, by.x = "site", by.y = "STAID", all.x = TRUE)
meanswinfowrite <- merge(meanswinfo, gages2, by.x = "sitewith0.x", by.y = "STAID", all.x = TRUE)
coefvarswinfowrite <- merge(coefvarswinfo, gages2, by.x = "sitewith0.x", by.y = "STAID", all.x = TRUE)

write.csv(trendswinfowrite, "trends_in_no_flow_and_climate_for_no_flow_sites_30_plus_years_081919_with_info.csv")
write.csv(meanswinfowrite, "mean_annual_no_flow_and_climate_metric_means_for_no_flow_sites_081919_with_info.csv")
write.csv(coefvarswinfowrite, "mean_annual_no_flow_and_climate_metric_coefvars_for_no_flow_sites_081919_with_info.csv")

##################################################

# MAKE VERY SIMPLE PLOTS JUST SHOWING WTHER TREND PRESENT, POSITIVE OR NEGATIVE

# reference in triangle, non-ref in circle

setwd("C:\\Users\\jhammond\\Desktop\\DRRCN_081919_first_run\\trend_plots")

states <- map_data("state")

variables <- colnames(trendswinfowrite)[3:79]

for(i in seq_along(variables)){

  # i = 31
  currentvariable <- as.character(paste0(variables[i]))
  currentdata <- trendswinfowrite[,c(currentvariable,"dec_lat_va","dec_long_va","CLASS")]
  currentdata[,1] <- ifelse(currentdata[,1] >0,1, ifelse(currentdata[,1]<0,-1,NA))
  currentdata$ref_class <- currentdata$CLASS
  cols <- c("-1" = "red", "1" = "blue", NA == "gray")
  
  trends_plot <- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + 
  coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
  theme_linedraw() + 
  geom_point(data=currentdata, aes(x=dec_long_va, y=dec_lat_va,  shape = ref_class, colour = as.factor(as.numeric(as.character(currentdata[,1])))), size = 2,  alpha=1) +
  scale_color_manual(values = cols, name = "Trend")

ggsave(filename=paste(variables[i], "_plot.png",sep = ""), plot = trends_plot, width = 20, height = 10, units = "cm")
}


pal <- wes_palette(21, name = "Zissou1", type = "continuous")

setwd("C:\\Users\\jhammond\\Desktop\\DRRCN_081919_first_run\\coefvar_plots")


variables <- colnames(coefvarswinfowrite)[c(6:28,35:86)]

for(i in seq_along(variables)){
  
  # i = 31
  currentvariable <- as.character(paste0(variables[i]))
  currentdata <- coefvarswinfowrite[,c(currentvariable,"dec_lat_va","dec_long_va","CLASS")]
  currentdata$ref_class <- currentdata$CLASS
  

  trends_plot <- ggplot(data = states) + 
    geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + 
    coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
    theme_linedraw() + 
    geom_point(data=currentdata, aes(x=dec_long_va, y=dec_lat_va, shape = ref_class, colour = currentdata[,1]), size = 2, shape=20, alpha=1) +
    scale_color_gradientn(name = currentvariable, colours = pal)
  
  ggsave(filename=paste(variables[i], "_plot.png",sep = ""), plot = trends_plot, width = 20, height = 10, units = "cm")
}






setwd("C:\\Users\\jhammond\\Desktop\\DRRCN_081919_first_run\\mean_annual_plots")


variables <- colnames(meanswinfowrite)[c(6:28,35:86)]

for(i in seq_along(variables)){
  
  # i = 31
  currentvariable <- as.character(paste0(variables[i]))
  currentdata <- meanswinfowrite[,c(currentvariable,"dec_lat_va","dec_long_va","CLASS")]
  currentdata$ref_class <- currentdata$CLASS
  mid <- mean(currentdata[,1], na.rm = TRUE)

  trends_plot <- ggplot(data = states) + 
    geom_polygon(aes(x = long, y = lat, group = group), fill = "white", color = "black") + 
    coord_fixed(1.3, xlim = c(-124.25,-70), ylim = c(26,48.5)) +
    theme_linedraw() + 
    geom_point(data=currentdata, aes(x=dec_long_va, y=dec_lat_va, shape = ref_class, colour = currentdata[,1]), size = 2, shape=20, alpha=1) +
    scale_color_gradientn(name = currentvariable, colours = pal)
  
  ggsave(filename=paste(variables[i], "_plot.png",sep = ""), plot = trends_plot, width = 20, height = 10, units = "cm")
}
