### -----------------------------------------------------------
# 
# This script prepares the ocean data frame used throughout the analysis. It assigns unique pixel IDs and the fraction of each pixel that is currently highly protected. 
# 
### -----------------------------------------------------------

### Section 1 - Load packages and set directories -----

library(raster)
library(sf)
library(tidyverse)
library(ggspatial)

# Path to the Global Ocean Conservation Priorities directory on the emLab Google Drive
gocp_project_dir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/ocean-conservation-priorities"

# Path to the emLab data directory
emlab_data_dir <- "/Volumes/GoogleDrive/Shared drives/emlab/data"

# Path to the Pristine Seas tourism directory on the emLab Google Drive
this_project_dir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/ps-tourism"

# Load raw ocean files
ocean_low_res_moll <- raster::raster(file.path(emlab_data_dir, "ocean-conservation-priorities", "inputs", "ocean_low_res_moll.tif"))

mpatlas <- sf::st_read(file.path(emlab_data_dir, "mpa-atlas", "MPAtlas_20201223_clean", "mpatlas_20201223_clean.shp"))

mpatlas_info <- mpatlas %>% 
  st_drop_geometry()

mpas_to_include <- c(15907, 14985, 14982, 12864, 9234, 737, 9051, 68813326, 
                     68807894, 68819490, 15624, 68819076, 68818813, 68808197)

mpas_to_exclude <- c(68808626, 68808627)

# Identify highly protected MPAs
highly_mpas <- mpatlas %>% 
  filter(is_mpa == 1,
         status == "Designated",
         implemente == 1) %>%
  filter(!mpa_id %in% mpas_to_exclude) %>% 
  filter(no_take %in% c("All") | mpa_id %in% mpas_to_include | country == "GAB")

mpas_for_review <- mpatlas %>% 
  filter(is_mpa == 1,
         status == "Designated",
         implemente == 1) %>%
  filter(!mpa_id %in% mpas_to_exclude) %>% 
  filter(no_take %in% c("All", "Part") | mpa_id %in% mpas_to_include) %>% 
  filter(!mpa_id %in% highly_mpas$mpa_id, iucn_categ %in% c("II", "Ia", "Ib")) %>% 
  arrange(desc(rep_m_area))

highly_mpas <- bind_rows(highly_mpas, mpas_for_review) %>% 
  st_set_crs(st_crs(mpatlas))

highly_mpas_info <- highly_mpas %>% st_drop_geometry()

highly_mpas_raster <- highly_mpas %>% 
  st_transform(st_crs(ocean_low_res_moll)) %>% 
  rasterize(ocean_low_res_moll, getCover = T) %>% 
  mask(ocean_low_res_moll)

# Make output ocean data frame
ocean_df <- stack(ocean_low_res_moll, highly_mpas_raster) %>% 
  raster::as.data.frame(xy = T) %>% 
  set_names(c("lon", "lat", "ocean", "f_highly_mpa")) %>% 
  filter(!is.na(ocean)) %>% 
  rowid_to_column(var = "cell_id") %>% 
  as_tibble()

save(ocean_df,
     file =  file.path(this_project_dir,  "data", "02-processed-data", "ocean.RData"))
