# Data munge- 
#
# Convert and merge DO and SC timeseries from HOBOware format to dataframe/xts version for inclusion into shinyapp.

library(tidyr)
library(readr)
library(dplyr)
library(xts)
library(lubridate)
library(dygraphs)

folder <- "C:/Users/astarke/Documents/DO_monitoring/DO_Monitorig_Data_PreQAQC/1stWeek_DOmonitoring"
filelist <- list.files(path = folder,full.names = TRUE, pattern = "csv$")

files <- paste(folder, filelist, sep = '/')
# Hoboware exports in UTF-8 ,,taken from original working code below, fileEncoding="UTF-8-BOM",
datalist = lapply(filelist, function(x){read.csv(file = x,  header = TRUE, stringsAsFactors = FALSE)}) #return a list of dataframes

alldata <- Reduce(function(x, y) merge(x, y, all = TRUE), datalist, accumulate = FALSE)
alldata$Date.Time <- mdy_hms(alldata$Date.Time)

cleanDOdata <- alldata %>% 
		select(-starts_with("Start")) %>% 
	       	select(-starts_with("End")) %>% 
	       	select(-starts_with("Coupler")) %>% 
	       	select(-starts_with("Host")) %>% 
		select(-starts_with("Stopped")) %>% 
		select(Date.Time, starts_with("DO")) %>% 
	filter(complete.cases(.))

names <- c("Date.Time",  "DO.1.lower", "DO.1.upper", "DO.2.lower", "DO.2.upper", "DO.3.lower", "DO.3.upper") # Need to order the names correctly

names(cleanDOdata) <- names

# Cleaning up Temp data for comparisons and analysis ----
cleanTempdata <- alldata %>% 
	select(Date.Time, starts_with("Temp")) %>% 
	select()

# Save all our work... ----
save.image("~/R_Code/DO_Dashboard/.RData")

