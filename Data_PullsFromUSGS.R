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

siteIDs <- c('405004073391001', 
             '405240073314901', 
             '405232073281801', 
             '405318073250101', 
             '405329073212601', 
             '403739073401001', 
             '403731073353801', 
             '403734073153401', 
             '404513072555701', 
             '404338073082101', 
             '404327073072701', 
             '404149073051301', 
             '404153073030701')


siteNames <- c('Hemp', 
               'OBH', 
               'CSH', 
               'Hunt', 
               'NPH', 
               'HB', 
               'MB', 
               'FI', 
               'BP', 
               'NCB', 
               'GSB1', 
               'GSB2', 
               'GSB3')

names(siteIDs) <- siteNames


DOparameter <- "00300"


RawDO <- readNWISuv(siteIDs, DOparameter, "", "")

siteInfo <- attr(RawDO, "siteInfo")

siteLocations <- siteInfo %>% 
  select(station_nm, site_no, starts_with("dec")) %>% 
  rename(Lat = dec_lat_va, Lon = dec_lon_va)

RawDO %>% left_join(siteLocations, by = 'site_no') -> DOdata

save(DOdata, siteLocations, file = "appData.RData")  # Save data objects so data does not need to be reloaded from the web,.

