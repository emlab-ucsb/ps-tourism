### -----------------------------------------------------------
# 
# Master script to run the Pristine Seas tourism model
# This script is just a sample - Currently it operates the biodiversity model, but placeholders for the other relevant parts are included and this script could be expanded.
# 
### -----------------------------------------------------------

### --------------------------------------------------------
### Section 1 - Load packages and set directories ----------
### --------------------------------------------------------

library(raster)
library(sf)
library(tidyverse)
library(ggspatial)
library(here)

### NOTE - you may have to adjust these paths depending on where your computer has mounted the Google Drive files
# Path to the emLab data directory
emlab_data_dir <- "/Volumes/GoogleDrive/Shared drives/emlab/data"

# Path to the Pristine Seas tourism directory on the emLab Google Drive
this_project_dir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/ps-tourism"


### -----------------------------------------------------------
### Section 2 - Source functions and load input data ----------
### -----------------------------------------------------------

# Source functions
sapply(list.files(
  pattern = "[.]R$",
  path = here::here("scripts", "functions"),
  full.names = TRUE
),
source)

# Load data files necessary for biodiversity model
load(file = file.path(this_project_dir,  "data", "02-processed-data", "bio_model_input.RData"))


#checks -- remove this later. Just wanted to explore the files.
head(ocean_df)
plot(ocean_df$ocean)


# I will add other data files here including the P0 raster and the Q0 raster
head(ocean_df)
dim(ocean_df)

ocean_df2<-ocean_df %>% filter(ocean==1)
dim(ocean_df2)

ocean_df2 %>%  ggplot(aes(x=lon,y=lat,fill=1)) + geom_raster() #ok, great


#compare this with my ocean matrix
CleanCoordmegacell<-readRDS(here("data","CleanCoordmegacell_mollweide.rds"))
dim(CleanCoordmegacell)
head(CleanCoordmegacell)
CleanCoordmegacell %>%  ggplot(aes(x=lon,y=lat,fill=1)) + geom_raster() #ok, great

CleanCoordmegacell$ren_coord<-1

testme <- left_join(ocean_df, CleanCoordmegacell, by=c("lon","lat"))
dim(testme) #coordinates do not match

#merge the two --- matches.



### -----------------------------------------------
### Section 3 - Final prep for model run ----------
### -----------------------------------------------

# set Z for biodiversity
z_bio <- 0.25

# Create a filter to limit the tourism model to pixels viable for dive tourism
# tourism_cell_ids <- #KAT IS STILL WORKING ON THIS, BUT THIS WILL GO HERE

# Vector of protected cell_ids (starting) - this is just an example, this bit would likely be looped eventually such that pixels are iteratively added to the "is mpa" vector
is_mpa_vect <- ocean_df$f_highly_mpa > 0.5 # select pixels that are at least 50% highly protected

protected_cell_ids <- ocean_df$cell_id[is_mpa_vect]

protected_cells <- matrix(is_mpa_vect, 
                          nrow = 1, 
                          ncol = length(is_mpa_vect))

### --------------------------------
### Section 4 - Run model ----------
### --------------------------------

# Caluclate biodiversity benefit from today's protected cells
calculate_relative_bio_benefit(is_mpa_vect = is_mpa_vect,
                               v_out_matrix =  v_out_matrix,
                               v_in_matrix = v_in_matrix, 
                               weights  = bio_weights, 
                               z_bio = z_bio, 
                               bau_benefit = bau_benefit, 
                               total_benefit_diff = total_benefit_diff)

# Just for fun (and to check), check maximum possible biodiversity benefit if the whole ocean was protected)
is_mpa_vect_all <- is_mpa_vect
is_mpa_vect_all[is_mpa_vect_all==F] = T

calculate_relative_bio_benefit(is_mpa_vect = is_mpa_vect_all,
                               v_out_matrix =  v_out_matrix,
                               v_in_matrix = v_in_matrix, 
                               weights  = bio_weights, 
                               z_bio = z_bio, 
                               bau_benefit = bau_benefit, 
                               total_benefit_diff = total_benefit_diff)

