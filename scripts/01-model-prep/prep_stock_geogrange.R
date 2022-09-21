#generate stock distributions/geographic range in mollweide and compatible with Juan's coordinate system
#process: convert the geographic range of each stock to mollweide. Use 0.5 as threshold for the presence/absence of a stock.
#saved the converted file as transformed_stockdistrib.rds. The file has cell ids, coordinates, etc.

### --------------------------------------------------------
### Section 1 - Load packages and set directories ----------
### --------------------------------------------------------
gc()
rm(list = ls())

library(here)
library(dplyr)
library(raster)
library(sf)
library(tidyverse)
library(ggspatial)

source(here("scripts","functions","convert_mollweide.R"))

# Path to the Global Ocean Conservation Priorities directory on the emLab Google Drive
gocp_project_dir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/ocean-conservation-priorities"

# Path to the emLab data directory
emlab_data_dir <- "/Volumes/GoogleDrive/Shared drives/emlab/data"

# Path to the Pristine Seas tourism directory on the emLab Google Drive
this_project_dir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/ps-tourism"

### Section 2 - Load spatial input data -----
ocean_low_res_moll <- raster::raster(file.path(emlab_data_dir, "ocean-conservation-priorities", "inputs", "ocean_low_res_moll.tif"))

##-- Load ocean_df file from juan. The code below also loads files necessary for biodiversity model
load(file = file.path(this_project_dir,  "data", "02-processed-data", "bio_model_input.RData"))
stocklayer <- ocean_df %>% select(cell_id,lon,lat,f_highly_mpa)
head(ocean_df) #ok we have lon lat here
dim(ocean_df)

##-- plot ocean_df
ocean_df %>%  ggplot(aes(x=lon,y=lat,fill=1)) + geom_raster()

##-- load the coordinates of our stock list and convert to Mollweide
Cleanmegacell<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/Cleanmegacell.rds")
CleanCoordmegacell<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/CleanCoordmegacell.rds")
dim(Cleanmegacell)
dim(CleanCoordmegacell) #this is not mollweide
head(CleanCoordmegacell)
Cleanmegacell[,1]
plot(Cleanmegacell[,1]/max(Cleanmegacell[,1]))

MegaData<-readRDS(here("data","MegaData_Ray.rds"))
head(MegaData)
which(MegaData$INCLUDE==1) #these are the stock numbers that will be included in our analysis

##--plot stock (unconverted coordinate)
#cbind(CleanCoordmegacell,probability=(Cleanmegacell[,i]/max(Cleanmegacell[,i]))) %>% ggplot(aes(x=lon,y=lat,fill=probability)) + geom_raster()

checkmerged <- ocean_df
for (i in which(MegaData$INCLUDE==1)){
#for (i in c(1:2)){

stock<-cbind(CleanCoordmegacell,probability=(Cleanmegacell[,i]/max(Cleanmegacell[,i])))
#head(stock1)
#dim(stock1)
#plot(stock1$probability)

#convert the probability values to mollweid coordinate. Then add a threshold for the presence-absence map.
mollwide_stock <- stock %>%
  raster::rasterFromXYZ(crs = "+proj=longlat +datum=WGS84") %>%
  raster::projectRaster(ocean_low_res_moll) %>%
  as.data.frame(xy = T) %>%
  set_names(c("lon", "lat", "probability")) %>%
  filter(!is.na(probability), probability>=0.50) %>%
  mutate(probability=1) #this converts probability to 1.

names(mollwide_stock) = c("lon","lat",noquote(MegaData$stockid[i]))
  
#head(mollwide_stock1)
#dim(mollwide_stock1)

#dim(ocean_df)
#head(ocean_df)

#head(mollwide_stock1)
#plot(mollwide_stock1$probability)
#max(mollwide_stock1$probability, na.rm=T)

#ggplot(stock1,aes(x=lon,y=lat,fill=probability)) + geom_raster()
#ggplot(mollwide_stock1, aes(x=lon,y=lat,fill=probability)) + geom_raster()

#min(mollwide_stock1$lon)
#min(mollwide_stock1$lat)
#min(ocean_df$lon)
#min(ocean_df$lat)

##--merge with oceandf
checkmerged <- left_join(checkmerged,mollwide_stock,by=c("lat","lon"))
}
#min(checkmerged)
#dim(checkmerged)
head(checkmerged)

##--check if the coordinates of the two datasets align
#checkmerged$probability[is.na(checkmerged$probability)] <- 0
sum(checkmerged$`Fis-29732`, na.rm=T)>0 #if FALSE, then they are not aligned
ggplot(checkmerged, aes(x=lon,y=lat,fill=`Fis-22832`)) + geom_raster()

#save the file so we do not need to re-run again
transformed_stockdistrib <- checkmerged
saveRDS(transformed_stockdistrib, file = here("data","transformed_stockdistrib.rds"))
