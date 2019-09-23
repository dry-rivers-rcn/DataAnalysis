library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(dplyr)
library(scales)
library(maptools)
library(dataRetrieval)

setwd("/Users/johnhammond/Desktop/intermittence_plots/")

data <- read.csv("intermittence_metric_summary_16May19.csv")

states <- map_data("state")

sites <- unique(gsub(".raw_daily_cfs.csv", "", data$site_no))

siteinfo <- readNWISsite(sites)

data$site_no <- gsub(".raw_daily_cfs.csv", "", data$site_no)

data <- merge(data,siteinfo, by = "site_no")

datasubset <- subset(data, data$totalflowperiods>1 & data$wyears == 2015)

totalflowperiods_plot <- ggplot(data = states) + 
  geom_polygon(aes(x = long, y = lat, group = group), fill = "black", color = "white") + 
  coord_fixed(1.3, xlim = c(-124.25,-103), ylim = c(31.75,48.5)) +
  theme_linedraw() + 
  geom_point(data=datasubset, aes(x=dec_long_va, y=dec_lat_va,  colour = totalflowperiods), size = 2, shape=20, alpha=1) +
  scale_color_gradientn(colors = rainbow(5), space ="Lab", name = "Flow periods")

ggsave(filename="totalflowperiods_plot.pdf", plot = total_flow_plot, width = 20, height = 10, units = "cm")
