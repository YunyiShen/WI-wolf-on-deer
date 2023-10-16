get_dmu_year <- function(year_focus, dmu_years, bas = "./deer_data/dmu_"){
  ress <- year_focus
  for(year_idx in which(!year_focus %in% dmu_years)){
    the_year <- year_focus[year_idx]
    smaller_years_foc <- sum(dmu_years<=the_year)
    ress[year_idx] <- dmu_years[smaller_years_foc]
  }
  ress <- paste0(bas, ress, ".shp")
  return(ress)
}

clean_count <- function(w){
  w <- as.character(w) # in case it's number
  w <- sub("(\\+|-).*","",w) |>
    as.numeric()
  return(w + (w == 0) )
}

library(terra)
library(sf)

#standard_crs <- "+proj=tmerc +lat_0=0 +lon_0=-90 +k=0.9996 +x_0=520000 +y_0=-4480000 +ellps=GRS80 +units=m +no_defs"

year_focus <- 1981:2020

##### read dmu ####
dmu_years <- list.files("./deer_data", ".shp$", full.names = F, recursive = T) |>
  sub(pattern = "dmu_", replacement = "" ) |>
  sub(pattern = ".shp", replacement = "" ) |>
  as.numeric()

dmu_all <- get_dmu_year(year_focus, dmu_years) 
dmu_base <- st_read(dmu_all[1])
basemap <- terra::rast("./rasters/dem.tiff") |>
  project(y=crs(dmu_base))
res(basemap) <- c(200,200)

##### associate density #####
# the year spreadsheet has DMU id rather than name
deer_den <- read.csv("./deer_data/DMU_deer_density_1981-2013.csv")
deer_rastername_ <- "./rasters/deer/SAKD_1981.tif"
for(yr in 1981:2013){
  yr_idx <- yr-1980
  dmu_that_year <- st_read(dmu_all[yr_idx],quiet = TRUE)
  deer_den_that_year <- deer_den[deer_den$YR==yr,]
  row.names(deer_den_that_year) <- deer_den_that_year$U
  dmu_that_year$SAKD <- deer_den_that_year[as.data.frame(dmu_that_year)[,
                                                         ifelse(yr<2002,"UNIT_ID","DEER_MGMT_")
                                                         ],"SAKD"]/2.58999 # convert to km2
  
  deer_rastername <- sub("1981",yr,deer_rastername_)
  
  deer_raster <- tryCatch( {rasterize(dmu_that_year, basemap, 
                           field = "SAKD", 
                           filename = deer_rastername, overwrite=TRUE)},
                           error = function(e){list()}
                           )
  if("SpatRaster" %in% class(deer_raster)){
    plot(deer_raster,main = yr)
  } else{
    cat(yr,"error\n")
  }
}

## after 2013 ## 
deer_den <- read.csv("./deer_data/DMU_deer_density_2002-2020.csv")
deer_den$U <- sub("( Forest| Farmlandf)", "", deer_den$DMU)
deer_den <- aggregate(cbind(SAKN,DR)~YR+U, FUN = sum, data = deer_den)
deer_den$SAKD <- deer_den$SAKN/deer_den$DR


for(yr in 2014:2020){
  yr_idx <- yr-1980
  dmu_that_year <- st_read(dmu_all[yr_idx],quiet = TRUE)
  deer_den_that_year <- deer_den[deer_den$YR==yr,]
  row.names(deer_den_that_year) <- deer_den_that_year$U
  dmu_col_name <- ifelse(yr<2018, ifelse(yr<2002,"UNIT_ID","DEER_MGMT_"), "DMU")
  
  dmu_that_year$SAKD <- deer_den_that_year[as.data.frame(dmu_that_year)[,dmu_col_name],"SAKD"]/2.58999 # convert to km2
  
  deer_rastername <- sub("1981",yr,deer_rastername_)
  
  deer_raster <- tryCatch( {rasterize(dmu_that_year, basemap, 
                                      field = "SAKD", 
                                      filename = deer_rastername, overwrite=TRUE)},
                           error = function(e){list()}
  )
  if("SpatRaster" %in% class(deer_raster)){
    plot(deer_raster,main = yr)
  } else{
    cat(yr,"error\n")
  }
}




##### deal with wolves ##### 
# sub("(\\+|-).*","","2+") regex for changing winter count to its lower bound

all_wolf_pack <- list.files("../WolfPack_loc", "shp$", recursive = T, full.names = T)


wolf_rastername_ <- "./rasters/wolf/packden_1981.tif"
for(yr in year_focus){
  shp_idx <- which( regexpr(yr, all_wolf_pack)!=-1 )[1]
  if(length(shp_idx) < 1){
    cat(yr, "error\n")
    next
  }
  shp_file <- all_wolf_pack[shp_idx]
  
  
  this_polygon <- st_read(shp_file, quiet = TRUE) #|>
  if(is.na(st_crs(this_polygon))){
    st_crs(this_polygon) <- st_crs(dmu_base)
  }
  
  this_polygon <- st_transform(this_polygon,crs = st_crs(dmu_base))
  #plot(dmu_base$geometry, main = yr)
  #plot(this_polygon$geometry,add = TRUE)
  
  ## they don't have consistent name
  count_names <- c("Count","COUNT","WINTER_COU")
  
  this_col_names <- names(this_polygon)
  this_count_name <- this_col_names[which(this_col_names %in% count_names)[1]]
  
  
  this_polygon$den <- clean_count(as.data.frame( this_polygon[,this_count_name])[,1]) / 
    (as.numeric( st_area(this_polygon))/(1000^2))
  
  wolf_rastername <- sub("1981",yr,wolf_rastername_)
  
  wolf_raster <- tryCatch( {rasterize(this_polygon, basemap, 
                                      field = "den", 
                                      background = 0)},
                           error = function(e){list()}
  )
  if("SpatRaster" %in% class(wolf_raster)){
    wolf_raster <- mask(wolf_raster, dmu_base,filename = wolf_rastername, overwrite=TRUE)
    plot(wolf_raster,main = yr)
  } else{
    cat(yr,"error\n")
  }
    
}

