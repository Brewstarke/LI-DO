# DO staions Pulled from USGS site:
# 
# Site 1
# http://waterdata.usgs.gov/ny/nwis/uv/?site_no=404327073072701&PARAmeter_cd=00400,00095,00010,90860,00300,63680,62361,99137
#
# Site 2
# http://waterdata.usgs.gov/ny/nwis/uv/?site_no=404149073051301&PARAmeter_cd=00400,00095,00010,90860,00300,63680,62361,99137
# 
# Stie 3
# http://waterdata.usgs.gov/ny/nwis/uv/?site_no=404153073030701&PARAmeter_cd=00400,00095,00010,90860,00300,63680,62361,99137
# 

library(dataRetrieval)

site1 <- '404327073072701'
site2 <- '404149073051301'
site3 <- '404153073030701'

parameters <- c('00400','00095','00010','90860','00300','63680','62361','99137')
DOparameter <- "00300"

site1_Sitedata <- readNWISsite(site1)
site1_Rawdata <- readNWISuv(site1, DOparameter, "", "")


DO1 <- site1_Rawdata %>% 
	rename(DO_0.5m = X_0.5m.above.seabed_00300_00011) %>% 
	rename(DO_0.1m = X_0.1m.above.seabed_00300_00011) %>% 
	select(dateTime, starts_with("DO")) %>% 
	xts(order.by = .$dateTime) %>% 
	dygraph(., main = "Station 1- Mouth of Connetquot River", group = "DO-stations") %>% 
	dyAxis("y", valueRange = c(-1, 11)) %>% 
	dyRangeSelector() %>% 
	dyOptions(useDataTimezone = FALSE, colors = RColorBrewer::brewer.pal(12, "Paired")[1:2])  
DO1
