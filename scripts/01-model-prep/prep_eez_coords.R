##THIS IS THE CODE FOR GENERATING THE EEZ SHAPEFILE. Important.
 gc()
 rm(list = ls())
 
 library(raster)
 library(sf)
 library(tidyverse)
 library(patchwork)
 
 #-- load stock distribution
 transformed_stockdistrib <- readRDS(here("data","transformed_stockdistrib.rds"))
 head(transformed_stockdistrib)
 dim(transformed_stockdistrib) #149547 by 1155
 #check if all cell_id's are ocean id
 transformed_stockdistrib %>% filter(ocean==0) #ok, they are all ocean.
 
 ocean_coordinates <- transformed_stockdistrib %>% select(cell_id,lon,lat,f_highly_mpa)
 ocean_coordinates %>% ggplot(aes(x=lon,y=lat,fill=1)) + geom_raster()
 
 ##ADD EEZ SHAPEFILE
 eez_shp <- readRDS(here("data","eez_shp_processed.rds"))
 eez_shp2 <- tibble::rowid_to_column(eez_shp, "My_ID")
 
 #this maps ID to country names. note that st_drop_geometry() drops the geometry
 country_name_library <- eez_shp2 %>% st_drop_geometry() %>% select(My_ID, territory1,iso_ter1,sovereign1,iso_sov1)
 names(country_name_library)[1] <- "CountryCode"
 head(country_name_library)
 
 # Path to the emLab data directory
 emlab_data_dir <- "/Volumes/GoogleDrive/Shared drives/emlab/data"
 ocean_low_res_moll <- raster::raster(file.path(emlab_data_dir, "ocean-conservation-priorities", "inputs", "ocean_low_res_moll.tif"))
 r2 <- rasterize(eez_shp2, ocean_low_res_moll, field="My_ID") 
 
 eezccoord <- rasterToPoints(r2, spatial = TRUE)
 eezccoord2 <- as.data.frame(eezccoord)
 head(eezccoord2) #these are coordinates
 dim(eezccoord2)
 
 colnames(eezccoord2)<-c("CountryCode","lon","lat")

 #check for uniqueness of the entries
 dim(unique(eezccoord2[c("lon", "lat")])) #ok, all unique
 
 #Plot US
 eezccoord2 %>% dplyr::filter(CountryCode==113) %>% ggplot(aes(x=lon,y=lat,fill=1)) + geom_raster()
 #Plot World
 eezccoord2 %>% ggplot(aes(x=lon,y=lat,fill=1)) + geom_raster()
 
 head(eezccoord2)
 
 #testme <- left_join(ocean_coordinates,eezccoord2,by=c("lon","lat")) 
 #ok, matched
 
 eez_mollweide <- left_join(eezccoord2,country_name_library,by="CountryCode")
 saveRDS(eez_mollweide, file = here("data","eez_mollweide.rds"))
 