#generate stock distributions/geographic range in mollweide and compatible with Juan's coordinate system


### --------------------------------------------------------
### Section 1 - Load packages and set directories ----------
### --------------------------------------------------------
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

stock1<-cbind(CleanCoordmegacell,probability=(Cleanmegacell[,1]>0)*1)
head(stock1)
plot(stock1$probability)

#this converts stock 1 to mollweide projection
mollwide_stock1 <- stock1 %>% 
  raster::rasterFromXYZ(crs = "+proj=longlat +datum=WGS84") %>% 
  raster::projectRaster(crs = "+proj=moll") %>% 
  as.data.frame(xy = T) %>% 
  #filter(!is.na(probability)) %>%
  set_names(c("lon", "lat", "probability")) 

head(mollwide_stock1)
plot(mollwide_stock1$probability)

ggplot(stock1,aes(x=lon,y=lat,fill=probability)) + geom_raster()
ggplot(mollwide_stock1, aes(x=lon,y=lat,fill=probability)) + geom_raster()

min(mollwide_stock1$lon)
min(mollwide_stock1$lat)
min(ocean_df$lon)
min(ocean_df$lat)

#merge with oceandf
checkmerged<-left_join(ocean_df,mollwide_stock1,by=c("lat","lon"))
min(checkmerged)
dim(checkmerged)
head(checkmerged)

##--check if the coordinates of the two datasets align
checkmerged$probability[is.na(checkmerged$probability)] <- 0
sum(checkmerged$probability)>0 #if FALSE, then they are not aligned
