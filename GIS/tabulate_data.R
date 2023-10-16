library(terra)
library(sf)


std_dmu <- st_read("./deer_data/dmu_2011.shp") # an arbitrary choice of standard dmu
wolf_year <- rast("./rasters/wolf/packden_1981.tif") * 0.
year_focus <- c(1981:2020) 

tabulated <- data.frame(matrix(NA,1,5))
names(tabulated) <- c("year","dmu", "SAKD", "wolfden", "wolfyear")
tabulated <- tabulated[-1, ]

deer_basename <- "./rasters/deer/SAKD_1981.tif"
wolf_basename <- "./rasters/wolf/packden_1981.tif"
for(yr in year_focus){
  cat(yr,"\n")
  wolf_raster <- rast(sub("1981",yr,wolf_basename))
  wolf_year <- wolf_year + (wolf_raster>0)
  tmp <- data.frame(year = yr, dmu = std_dmu$DEER_MGMT_)
  if(yr==2001){
    tmp$SAKD <- NA
  } else {
    deer_raster <- rast(sub("1981",yr,deer_basename))
    tmp$SAKD <- extract(deer_raster, std_dmu, fun = mean,na.rm=TRUE)$SAKD # 2001 issue
  }
  
  tmp$wolfden <- extract(wolf_raster, std_dmu, fun = mean,na.rm=TRUE)$den
  tmp$wolfyear <- extract(wolf_year, std_dmu, fun = max,na.rm=TRUE)$den
  tabulated <- rbind(tabulated, tmp)
  write.csv(tabulated, "./clean_data/wolf_deer_year.csv")
  
}


