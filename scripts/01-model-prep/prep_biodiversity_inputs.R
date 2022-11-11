### -----------------------------------------------------------
# 
# This script prepares the inputs for the biodiversity model
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

### Section 2 - Load spatial input data -----

ocean_low_res_moll <- raster::raster(file.path(emlab_data_dir, "ocean-conservation-priorities", "inputs", "ocean_low_res_moll.tif"))

mpatlas <- sf::st_read(file.path(emlab_data_dir, "mpa-atlas", "MPAtlas_20201223_clean", "mpatlas_20201223_clean.shp"))

### Section 3 - Prep ocean layer -----

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

### Section 4 - Prep species layer -----

spp_files <- tibble(filepath = list.files(c(file.path(gocp_project_dir, "data", "02_processed", "species_distributions", "birdlife"),
                                            file.path(gocp_project_dir, "data", "02_processed", "species_distributions", "aquamaps")),
                                          full.names = T),
                    valid_sci_name = str_replace_all(str_remove(basename(filepath), "\\.tif"), "_", " ")) %>% 
  arrange(valid_sci_name)

smts_info <- tibble(filepath = list.files(file.path(gocp_project_dir, "data", "02_processed","seamounts"), full.names = T)) 

provs_info <- tibble(filepath = list.files(file.path(gocp_project_dir, "data", "02_processed", "biogeography"), full.names = T, pattern = "\\.tif"))

biodiversity_df <- stack(c(spp_files$filepath, smts_info$filepath, provs_info$filepath))

biodiversity_df <- biodiversity_df %>% 
  raster::as.data.frame(xy = T)

biodiversity_df <- biodiversity_df %>% 
  rename("lon" = "x", "lat" = "y") %>% 
  inner_join(ocean_df %>% 
               select(lon, lat, cell_id)) %>%
  select(lon, lat, cell_id, everything()) %>%
  as_tibble() 

bio_feature_names <- colnames(biodiversity_df)[-c(1:3)]

### Section 5 - Species weights -----

spp_wts <- data.table::fread(file.path(gocp_project_dir, "data", "02_processed", "species_list", "spp_weights.csv")) %>% 
  as_tibble() %>% 
  rename(w = spp_weight)

smts_wts <- tibble(filepath = list.files(file.path(gocp_project_dir, "data", "02_processed","seamounts"), full.names = T)) %>% 
  mutate(w = sum(spp_wts$w)/n())

provs_wts <- tibble(filepath = list.files(file.path(gocp_project_dir, "data", "02_processed", "biogeography"), full.names = T, pattern = "\\.tif")) %>% 
  mutate(w = sum(spp_wts$w)/n())

bio_features_info <- bind_rows(spp_wts %>% 
                                 mutate(sub_goal = "species"), 
                               smts_wts %>% 
                                 mutate(sub_goal = "seamounts"), 
                               provs_wts %>% 
                                 mutate(sub_goal = "provinces"))

n_bio_features <- nrow(bio_features_info)

features_matrix <- biodiversity_df %>% 
  select(-lon,-lat,-cell_id) %>% 
  as.matrix()

rownames(features_matrix) <- biodiversity_df$cell_id

stopifnot(
  sum(map_lgl(biodiversity_df %>%
                select(-lon,-lat,-cell_id), is.numeric)) == ncol(features_matrix)
) 

norm_features_matrix <- sweep(features_matrix, 2, colSums(features_matrix, na.rm = T), FUN = "/")

stopifnot(
  sum(colSums(norm_features_matrix, na.rm = T)) == ncol(features_matrix)
) 

norm_features_matrix <- norm_features_matrix[rowSums(is.na(norm_features_matrix)) != ncol(norm_features_matrix), ]

stopifnot(
  identical(colnames(norm_features_matrix), 
            biodiversity_df %>% 
              select(-lon,-lat,-cell_id) %>% 
              colnames())
)  # Is the order of the features mantained?

norm_features_matrix[is.na(norm_features_matrix)] <- 0

z_bio <- 0.25

bio_weights <- bio_features_info$w

### Section 6 - Impacts -----

bio_abatable_impacts_df <- raster(file.path(gocp_project_dir, 
                                            "data", "02_processed", "impacts", "chi", "abatable_impacts_5_yr_avg_log.tif")) %>% 
  raster::as.data.frame(xy = T) %>% 
  set_names(c("lon", "lat", "Ia")) %>% 
  inner_join(ocean_df) %>% 
  as_tibble() %>% 
  replace_na(list(Ia = 0))

bio_unabatable_impacts_df <- raster(file.path(gocp_project_dir, 
                                              "data", "02_processed", "impacts", "chi", "unabatable_impacts_5_yr_avg_log.tif")) %>% 
  raster::as.data.frame(xy = T) %>% 
  set_names(c("lon", "lat", "Iu")) %>% 
  inner_join(ocean_df) %>% 
  as_tibble()%>% 
  replace_na(list(Iu = 0)) 

v_out_matrix <- norm_features_matrix %>% 
  sweep(1, (1 - bio_abatable_impacts_df$Ia), FUN = "*") %>% 
  sweep(1, (1 - bio_unabatable_impacts_df$Iu), FUN = "*")

v_in_matrix <- norm_features_matrix %>% 
  sweep(1, (1 - bio_unabatable_impacts_df$Iu), FUN = "*")

v_diff_matrix <- norm_features_matrix %>% 
  sweep(1, (bio_abatable_impacts_df$Ia - bio_abatable_impacts_df$Ia*bio_unabatable_impacts_df$Iu), FUN = "*")

### Section 7 - Starting conditions vs. best possible -----

#What is the starting condition? - The impacts layer is not independent of the current protection level. Pixels already in MPAs would naturally have smaller abatable impacts and a lower biodiversity benefit. 

# What fraction of suitable habitat remains in bau?
v_bau_per_species <- v_out_matrix %>% 
  colSums(na.rm = T)

## average fraction of habitat suitability protected is nothing is done
mean(v_bau_per_species)

## average spp persistence if nothing is done
mean(v_bau_per_species^z_bio)

## total starting biodiversity benefit
bau_benefit <- sum(bio_weights*v_bau_per_species^z_bio)

bau_benefit/sum(bio_weights*1^z_bio)

# What if we protect everything? We still don't get full benefits due to unabatable threats
v_full_mpa_per_species <- v_in_matrix %>% 
  colSums(na.rm = T)

## average fraction of habitat suitability remaining if all ocean is protected
mean(v_full_mpa_per_species) 

## average spp persistence if everything is protected
mean(v_full_mpa_per_species^z_bio)

## total ending biodiversity benefit
max_benefit <- sum(bio_weights*v_full_mpa_per_species^z_bio)

max_benefit/sum(bio_weights*1^z_bio)

#max_benefit when all threats removed (both abatable and unabatable)
max_benefit_allthreats <- sum(bio_weights*1^z_bio)

# What is the max difference made by species?
v_diff_per_species <- v_diff_matrix %>% 
  colSums(na.rm = T)

# things check out?
all.equal(v_full_mpa_per_species - v_diff_per_species, v_bau_per_species)

## average  difference made in terms of suitable habitat protected
mean(v_diff_per_species) #  26% increase in suitable habitat remaining

## increase in persistence 
total_benefit_diff <- sum(bio_weights*v_full_mpa_per_species^z_bio) - sum(bio_weights*v_bau_per_species^z_bio)
total_benefit_diff/sum(bio_weights*v_bau_per_species^z_bio) # a max of 41% increase in spp permanence

### Section 8 - Output -----

save(ocean_df, highly_mpas, highly_mpas_raster,
     biodiversity_df, bio_features_info, n_bio_features, bio_feature_names, norm_features_matrix, bio_weights, 
     v_in_matrix, v_out_matrix, v_diff_matrix, v_bau_per_species, v_full_mpa_per_species,
     bau_benefit, max_benefit, max_benefit_allthreats, total_benefit_diff,
     file = file.path(this_project_dir,  "data", "02-processed-data", "bio_model_input.RData"))

save(v_out_matrix, v_in_matrix, bio_weights, bau_benefit, total_benefit_diff, 
     file =  file.path(this_project_dir,  "data", "02-processed-data", "minimal_bio_model_input.RData"))
