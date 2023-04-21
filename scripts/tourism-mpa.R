#--
#Code for the paper: "Marine protected areas for dive tourism"
#R. Cabral
#Last update: 21 April 2023
#-- This code evaluates the build-up of biomass inside MPAs, evaluate biodiversity change, and calculate dive tourism benefits
gc()
rm(list = ls())

#-- load libraries
library(doParallel)
library(raster)
library(rgdal)
library(maptools)
library(cowplot)
library(reshape)
library(scales)
library(maps)
library(sf)
library(tidyverse)
library(patchwork)
library(data.table)
library(here)
library(fst)
library(ggspatial)
library(ggrepel)
library(bigstatsr)
library(Hmisc)
library(dplyr)

#-- Path to the Pristine Seas tourism directory on the emLab Google Drive
this_project_dir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/ps-tourism"

#-- load stock distribution
transformed_stockdistrib <- readRDS(here("data","transformed_stockdistrib.rds"))
head(transformed_stockdistrib)
dim(transformed_stockdistrib) #149547 by 1155

#-- check if all cell_id's are ocean id
transformed_stockdistrib %>% filter(ocean==0) %>% dim() #ok, they are all ocean.

ocean_coordinates <- transformed_stockdistrib %>% select(cell_id,lon,lat,f_highly_mpa)
ocean_coordinates %>% ggplot(aes(x=lon,y=lat,fill=1)) + geom_raster()

#-- load MegaData --- add biomass density
MegaData<-readRDS(here("data","MegaData_Ray.rds"))
MegaData_filtered <- MegaData %>% filter(INCLUDE==1) %>% mutate(bvk_fin = 1-(ExploitationRate_BAU1_Ray/r_fin)) %>% dplyr::select(stockid,SciName,r_fin,Kfin,bvk_fin)
#-- ensure no negative numbers
MegaData_filtered$bvk_fin[MegaData_filtered$bvk_fin < 0] <- 0

MegaData_filtered$ID <- seq.int(nrow(MegaData_filtered)) #add ID number 
head(MegaData_filtered)
tail(MegaData_filtered)
dim(MegaData_filtered)

MegaData_filtered$check_stock_id<-colnames(transformed_stockdistrib)[6:1155] #ok. This is just a check that the files are matched.

#-- load the distance matrix (source, sink, distance)
merged_dist_matrix <- readRDS(here("data","distance-library","merged_dist_matrix","merged_dist_matrix.rds"))
#-- convert distance from m to km
merged_dist_matrix$distance <- merged_dist_matrix$distance/1000
head(merged_dist_matrix)

#-- load homerange and pld data predictions, then process
pld_data <- read.csv(here("data","homerange_pld_predictions","pld_rf_predictions_final.csv")) %>% dplyr::select(species,observed_PLD,predicted_PLD) %>% dplyr::rename(SciName = species)
#-- summarize in case of duplicates
pld_data_mean <- pld_data %>% group_by(SciName) %>% summarise(mean_observed_PLD = mean(observed_PLD), sd_observed_PLD = sd(observed_PLD), mean_predicted_PLD = mean(predicted_PLD))
head(pld_data_mean)
dim(pld_data_mean)

homerange_data <- read.csv(here("data","homerange_pld_predictions","homerange_rf_predictions_10112022.csv")) %>% dplyr::select(species,observed_homerange,predicted_homerange) %>% dplyr::rename(SciName = species)
homerange_data_mean <- homerange_data %>% group_by(SciName) %>% summarise(mean_observed_homerange = mean(observed_homerange), sd_observed_homerange = sd(observed_homerange), mean_predicted_homerange = mean(predicted_homerange))
head(homerange_data_mean)

species_list <- MegaData_filtered %>% dplyr::select(SciName) %>% unique()
dim(species_list)#811 species

db_with_pld <- left_join(species_list,pld_data_mean,by="SciName")
db_with_pld_hrange <- left_join(db_with_pld,homerange_data_mean)
head(db_with_pld_hrange)
dim(db_with_pld_hrange)

db_with_pld_hrange_filtered <- db_with_pld_hrange %>% mutate(PLD = ifelse(!is.na(mean_observed_PLD),mean_observed_PLD,mean_predicted_PLD), homerange = ifelse(!is.na(mean_observed_homerange),mean_observed_homerange,mean_predicted_homerange),
                                                             complete = ifelse((PLD>0 & homerange>0),1,0))
sum(db_with_pld_hrange_filtered$complete,na.rm=T) #610 species out of 811. Check what proportion of K this is

#-- merge with the full data list
MegaData_PLD_hrange <- left_join(MegaData_filtered,db_with_pld_hrange_filtered,by="SciName")

#-- max dispersal distance limit (3 sigma larvae)
3*1.33*(max(MegaData_PLD_hrange$PLD,na.rm=T)^1.3)*sqrt(pi/2) #18K

#-- add dispersal distance limit to the megadata file
MegaData_filtered_step2 <- MegaData_PLD_hrange %>% mutate(sigma_larvae = 1.33*(PLD^1.3)*sqrt(pi/2), dispersal_distance_limit = 3*sigma_larvae, homerange_radius = sqrt(homerange/pi))
head(MegaData_filtered_step2)

#-- add the geographic range of the stock
full_stock_distrib <- transformed_stockdistrib[6:1155]
geog_range_perstock<- colSums(full_stock_distrib,na.rm=T) %>% as.data.frame()
colnames(geog_range_perstock) <- c('geog_range')
geog_range_perstock$stockid <- row.names(geog_range_perstock) 
head(geog_range_perstock)

MegaData_filtered_step3 <- left_join(MegaData_filtered_step2,geog_range_perstock,by="stockid") 
MegaData_filtered_step_fin <- MegaData_filtered_step3 %>% mutate(Kperpixel = Kfin/geog_range)

head(MegaData_filtered_step_fin)
sum(MegaData_filtered_step_fin$complete, na.rm = T) #898
min(MegaData_filtered_step_fin$dispersal_distance_limit,na.rm=T)

#-- Remove stocks with no intersection with existing diving as their biomass will be unaffected by placing MPAs in existing dive sites

#-- load .rdata for input files [from Kat]
load(here("data","dive","tourism_model_input.RData")) %>% head()
## Input files content:"ocean_df_with_eezs" "dives_input" "suitability_input" "suitability_input" "price_country_region_input" "price_interpolated_input" 

# #!!!!!!!!!
# #remove dive sites with no biomass information
# #-- stocks with no intersection with diving can be removed as they will be unaffected by MPAs in dive areas
# filter1 <- transformed_stockdistrib %>% filter(cell_id %in% suitability_input$cell_id)
# filter2 <- filter1 %>% select(-c(cell_id,lon,lat,ocean,f_highly_mpa))
# #check if all dive sites intersect with the stocks
# dive_with_biom <- rowSums(filter2,na.rm=T)
# suitability_input$with_stock <- (dive_with_biom>0)*1
# dive_with_no_biom <- suitability_input %>% filter(with_stock==0)
# dive_with_no_biom #ok, there are 7 dive sites that have no biomass information!!! so we should remove them.
# 
# #revised Kat's file
# dives_input <- dives_input %>% filter(! cell_id %in% dive_with_no_biom$cell_id)
# dim(dives_input)
# head(dives_input)
# suitability_input <- suitability_input %>% filter(! cell_id %in% dive_with_no_biom$cell_id)
# price_country_region_input <- price_country_region_input %>% filter(! cell_id %in% dive_with_no_biom$cell_id)
# price_interpolated_input <- price_interpolated_input %>% filter(! cell_id %in% dive_with_no_biom$cell_id)

dim(transformed_stockdistrib)
head(transformed_stockdistrib)
filter1 <- transformed_stockdistrib %>% filter(cell_id %in% suitability_input$cell_id)
dim(filter1)#there are 1807 dive sites x 1155

filter2 <- filter1 %>% select(-c(cell_id,lon,lat,ocean,f_highly_mpa))
dim(filter2) #1807 dive sites, 1150 stocks

rowSums(filter2,na.rm=T) %>% min() #all 1807 dive pixels have at least 1 dive site

#Check if stocks intersect with a dive site
mysum <-colSums(filter2,na.rm=T) %>% as.data.frame()
head(mysum)
dim(mysum)
names(mysum)[1] <- "n_pixel_intersect"
sp_intersect_dive <- cbind(stockname = rownames(mysum), mysum)
dim(sp_intersect_dive)
head(sp_intersect_dive)

sp_intersect_dive %>% filter(n_pixel_intersect==0) %>% dim() ##remove 103 stocks with no intersection with diving!
stocklist_with_diving <- sp_intersect_dive %>% filter(n_pixel_intersect>0) %>% select(stockname)#which(mysum!=0)
dim(stocklist_with_diving)#1047 stocks that intersects with dive sites
head(stocklist_with_diving)

#-- now, how about stock list with complete parameters?
stocklist_complete_params <- MegaData_filtered_step_fin %>% filter(complete==1) #which(MegaData_filtered_step_fin$complete==1)
dim(stocklist_complete_params)#898 stocks with complete params
head(stocklist_complete_params)

#-- our final stocklist will be the intersection of the two
stocklist <- stocklist_with_diving %>% filter(stockname %in% stocklist_complete_params$stockid)
stocklist <- stocklist$stockname
length(stocklist)#813 stocks with complete information

#stocklist in numeric, since it is easy to work with numbers
head(MegaData_filtered)
stocklist_index <- MegaData_filtered %>% filter(stockid %in% stocklist) %>% select(ID)
stocklist_index <- stocklist_index$ID

#-- check if all dive sites have country names
dive_with_eez_details <- left_join(dives_input,ocean_df_with_eezs,by="cell_id")
head(dive_with_eez_details) #we need to fill out the eez names of some countries

##-- load EEZ file, and use this to fill out missing details from the above EEZ file
eez_mollweide <- readRDS(file = here("data","eez_mollweide.rds"))
head(eez_mollweide)
dim(eez_mollweide)

#-- There are 5 pixels with diving but with no EEZ info. This creates country details for 5 pixels with no EEZ info, all New Zealand pixels
head(ocean_coordinates)
new_zealand_add_v2 <- ocean_coordinates %>% filter(cell_id %in% c(113701, 114285, 114866, 114868, 120176)) %>% mutate(CountryCode = 95, territory1="New Zealand",iso_ter1="NZL", sovereign1="New Zealand", iso_sov1="NZL") %>%
   select(CountryCode,lon,lat,territory1,iso_ter1,sovereign1,iso_sov1)
eez_mollweide <- rbind(eez_mollweide,new_zealand_add_v2)
tail(eez_mollweide)
dim(eez_mollweide)

#-- create cell id with country names
cell_id_with_country <- left_join(ocean_coordinates,eez_mollweide,by=c("lon","lat"))
head(cell_id_with_country)
dim(cell_id_with_country)

#-- add Kat's EEZ file
cell_id_with_country_kat_prime <- left_join(cell_id_with_country,ocean_df_with_eezs,by=c("cell_id","lon","lat"))
#add price data with region details, given that region info is included in price info
cell_id_with_country_kat <- left_join(cell_id_with_country_kat_prime,price_country_region_input,by="cell_id")
head(cell_id_with_country_kat)
dim(cell_id_with_country_kat)

#--view regions of the world
#ggplot(data=cell_id_with_country_kat, aes(x=lon, y=lat, color=region.y)) + geom_point()

#Use filter to include only pixels with diving
cell_id_with_country_diving_only <- cell_id_with_country_kat %>% filter(cell_id %in% dives_input$cell_id)

#-- fill out region --- for pixels with missing regions
region_library <- cell_id_with_country_diving_only %>% select(territory1,region.x,region.y) %>% filter(! is.na(territory1)) %>% unique() %>% mutate(region_fill = ifelse(is.na(region.x), region.y, region.x)) %>% select(territory1,region_fill) %>% unique() %>% group_by(territory1) %>% mutate(n=n()) %>% filter(n==1 | !is.na(region_fill)) %>% select(territory1,region_fill)
#region_library <- cell_id_with_country_diving_only %>% select(territory1,region.y) %>% filter(! is.na(territory1)) %>% unique() %>% filter(! is.na(region.y))
#region_library <- rename(region_library, region_fill=region.y)

#-- manual assignment of region for territories/countries with no region assignment 
region_library <- region_library %>%
  mutate(region_fill=replace(region_fill, territory1=="Alaska", "North America")) %>%
  mutate(region_fill=replace(region_fill, territory1=="Albania", "Europe & Central Asia")) %>%
  mutate(region_fill=replace(region_fill, territory1=="Andaman and Nicobar", "South Asia")) %>%
  mutate(region_fill=replace(region_fill, territory1=="Antarctica", "Latin America & Caribbean")) %>%  #dive pixels nearest to latin america
  mutate(region_fill=replace(region_fill, territory1=="Jersey", "Europe & Central Asia")) %>%
  mutate(region_fill=replace(region_fill, territory1=="Slovenia", "Europe & Central Asia")) %>%
  mutate(region_fill=replace(region_fill, territory1=="Romania", "Europe & Central Asia")) %>%
  mutate(region_fill=replace(region_fill, territory1=="Azores", "Europe & Central Asia")) %>%
  mutate(region_fill=replace(region_fill, territory1=="Lebanon", "Middle East & North Africa")) %>%
  mutate(region_fill=replace(region_fill, territory1=="Madeira", "Europe & Central Asia")) %>%
  mutate(region_fill=replace(region_fill, territory1=="Canary Islands", "Europe & Central Asia")) %>%
  mutate(region_fill=replace(region_fill, territory1=="Bahrain", "Middle East & North Africa")) %>%
  mutate(region_fill=replace(region_fill, territory1=="Western Sahara", "Sub-Saharan Africa")) %>%
  mutate(region_fill=replace(region_fill, territory1=="Hawaii", "East Asia & Pacific")) %>%
  mutate(region_fill=replace(region_fill, territory1=="Antigua and Barbuda", "Latin America & Caribbean")) %>%
  mutate(region_fill=replace(region_fill, territory1=="Dominica", "Latin America & Caribbean")) %>%
  mutate(region_fill=replace(region_fill, territory1=="Aruba", "Latin America & Caribbean")) %>%
  mutate(region_fill=replace(region_fill, territory1=="Cameroon", "Sub-Saharan Africa")) %>%
  mutate(region_fill=replace(region_fill, territory1=="Galapagos", "Latin America & Caribbean")) %>%
  mutate(region_fill=replace(region_fill, territory1=="Chagos Archipelago", "South Asia")) %>%
  mutate(region_fill=replace(region_fill, territory1=="Mayotte", "Sub-Saharan Africa")) %>%
  mutate(region_fill=replace(region_fill, territory1=="Glorioso Islands", "Sub-Saharan Africa")) %>%
  mutate(region_fill=replace(region_fill, territory1=="Réunion", "Sub-Saharan Africa")) %>%
  mutate(region_fill=replace(region_fill, territory1=="Easter Island", "East Asia & Pacific"))

head(region_library)
cell_id_with_country_kat_withregion <- left_join(cell_id_with_country_diving_only,region_library, by="territory1")
head(cell_id_with_country_kat_withregion)
dim(cell_id_with_country_kat_withregion)

#check if all is well
ggplot(data=cell_id_with_country_kat_withregion, aes(x=lon, y=lat, color=region_fill)) + geom_point()

#-- PAPER STAT: fraction and absolute size of global ocean suitable for diving
dim(suitability_input) #1814 suitable dive sites
dim(suitability_input)[1]*100/dim(transformed_stockdistrib)[1] #1.21% - fraction of ocean with diving.
dim(suitability_input)[1]*50*50# 4.5 million km - total area of ocean surface with diving

#-- check dives_input
head(dives_input)
dim(dives_input)#1814x4, number of dives and lower and upper bound
sum(dives_input$n_dives_extrap)#33.1 million, average number of dives in the ocean
sum(dives_input$n_dives_extrap_min)#17.1 million lower, lower bound
sum(dives_input$n_dives_extrap_max)#54 million max, upper bound

#-- check price
head(price_constant_input)
cons_price_per_dive <- price_constant_input$price[1]
cons_price_per_dive #constant price per dive of US$58.75

#-- annual revenue in billion US$
sum(dives_input$n_dives_extrap)*cons_price_per_dive/1e9 #1.94B
#lower bound annual revenue
sum(dives_input$n_dives_extrap_min)*cons_price_per_dive/1e9 #1B
#upper bound unnual revenue
sum(dives_input$n_dives_extrap_max)*cons_price_per_dive/1e9 #3.17B

#-- check other prices
head(price_country_region_input)
plot(price_country_region_input$price)

head(price_interpolated_input)
plot(price_interpolated_input$price)

#-- how many species?
MegaData_filtered_step_fin %>% filter(stockid %in% stocklist) %>% select(SciName) %>% unique() %>% dim() #599 species

#-- %K considered
MegaData_filtered_step_fin %>% filter(stockid %in% stocklist) %>% dplyr::summarize(sum(Kfin))/sum(MegaData_PLD_hrange$Kfin) #we will use 74% of the K.

#-- check final list of dataset
Checkme <- MegaData_filtered_step_fin %>% filter(stockid %in% stocklist)
ggplot(Checkme, aes(x=mean_observed_PLD, y= mean_predicted_PLD)) + geom_point() + geom_abline(slope=1, intercept=0)

# #NOTE-- JUST OPEN THIS WHEN THE DISTANCE MATRICES CHANGE
# #check if we can load individual files here then merge
# #distmat_filenames <- list.files(path=here("data","distance-library","fst_file"), pattern=".fst", all.files=FALSE,full.names=TRUE)
# distmat_filenames <- list.files(path=here("data","distance-library"), pattern=".rds", all.files=FALSE,full.names=TRUE)
# distmat_filenames[1]
# length(distmat_filenames)#40 files
# 
# #let us load all the distance matrices, subset, then save as fst file.
# for (i in 1:length(distmat_filenames)){
#   myfile <- readRDS(distmat_filenames[i]) %>% filter(distance<=3667657.8)
#   write.fst(myfile , here("data","distance-library","dist_matrix_subset_fst",paste0(i,"_connect_matrix.fst")))
# }

##-- Connectivity matrix, larvae
#-- no need to run, so we will switch this function off
run_subset_connectivitymatrix <- 0 #1 for turn on, 0 to switch off

#Subset larvae connectivity matrix
if(run_subset_connectivitymatrix == 1){
  
  distmat_filenames_subset_fst <- list.files(path=here("data","distance-library","dist_matrix_subset_fst"), pattern=".fst", all.files=FALSE,full.names=TRUE)
  distmat_filenames_subset_fst[1]
  length(distmat_filenames_subset_fst)#40 files
  
  source(here("scripts", "functions","func_stitch_connect_matrix.R"))
  
  stocklist2 <- c(461,467,473,475,478,stocklist[393:821])
  nstock<-length(stocklist2)
  registerDoParallel(6)
  #stocklist2 <- 10#stocklist[10:nstock]
  #registerDoParallel(detectCores()/2)
  foreach(stock_num=stocklist2) %dopar% {
    stock_subset_i <- which(transformed_stockdistrib[,stock_num+5] > 0)
    
    ##--! this is the code that can be used when the maximum disperal is over 1K km.
    distance_mat_full_prop_larvae <- func_stitch_connect_matrix(distmat_files=distmat_filenames_subset_fst,stock_subset_i=stock_subset_i,MegaData_filtered_step2=MegaData_filtered_step2,stock_num=stock_num)
    
    ##-- merged_dist_matrix is in km
    #distance_mat_full_prop_larvae <- merged_dist_matrix %>% filter(source %in% stock_subset_i, sink %in% stock_subset_i, distance<=MegaData_filtered_step2$dispersal_distance_limit[stock_num]) %>% group_by(source) %>% mutate(biom_prop = exp(-( distance^2 / (2*(MegaData_filtered_step2$sigma_larvae[stock_num]^2))) ), biom_prop = biom_prop/sum(biom_prop)) %>% dplyr::select(-distance) %>% as.data.table()
    
    #fts is the fastest in saving and loading files.
    fst::write.fst(distance_mat_full_prop_larvae , paste0("/Users/ren/Documents/GitHub/tourism-mpa-support/connect_matrix/",stock_num,"_connect_larvae.fst"))
  }
  doParallel::stopImplicitCluster()
}

# #check how many files
# connect_matrix_nfiles <- list.files(path="/Users/ren/Documents/GitHub/tourism-mpa-support/connect_matrix/", pattern=".fst", all.files=FALSE,full.names=TRUE)
# length(connect_matrix_nfiles)#821 files
# for (i in 1:length(connect_matrix_nfiles)){
#   check<-read.fst(connect_matrix_nfiles[i])
#   print(dim(check)[1])
# }

##check the output
#seeme <- read.fst("/Users/ren/Documents/GitHub/tourism-mpa-support/connect_matrix/1_connect_larvae.fst")
#seeme %>% group_by(source) %>% summarize(sum(biom_prop))
#seeme %>% group_by(sink) %>% summarize(sum(biom_prop))

##-- Connectivity matrix, adult
#-- no need to re-run so we will turn this function off
run_subset_connectivitymatrix_adult <- 0 #1 for on, 0 to switch this off

if(run_subset_connectivitymatrix_adult == 1){
  registerDoParallel(detectCores()/2)
  foreach(stock_num=stocklist) %dopar% {
    stock_subset_i <- which(transformed_stockdistrib[,stock_num+5] > 0)
    #included here: filter distance
    distance_mat_full_prop_adult <- merged_dist_matrix %>% filter(source %in% stock_subset_i, sink %in% stock_subset_i, distance<=MegaData_filtered_step2$homerange_radius[stock_num]) %>% group_by(source) %>% mutate(biom_prop = 1/n()) %>% dplyr::select(-distance) %>% as.data.table()
    # distance_mat_full_prop <- distance_mat_full %>% filter(pos1 %in% stock_subset_i, pos2 %in% stock_subset_i) %>% group_by(pos1) %>% mutate(biom_prop = exp(-( dist^2 / (2*(sigma^2))) ), biom_prop = biom_prop/sum(biom_prop)) %>% dplyr::select(-dist) %>% as.data.table()
    
    #fts is the fastest in saving and loading files.
    fst::write.fst(distance_mat_full_prop_adult , paste0("/Users/ren/Documents/GitHub/tourism-mpa-support/connect_matrix_adult/",stock_num,"_connect_adult.fst"))
  }
  doParallel::stopImplicitCluster()
}

##--for checking the code
#stock_subset_i <- which(transformed_stockdistrib[,255+5] > 0)
#distance_mat_full_prop_adult <- merged_dist_matrix %>% filter(source %in% stock_subset_i, sink %in% stock_subset_i, distance<=MegaData_filtered_step2$homerange_radius[stock_num]) %>% group_by(source) %>% mutate(biom_prop = 1/n()) %>% dplyr::select(-distance) %>% as.data.table()
#distance_mat_full_prop_adult %>% group_by(source) %>% summarize(sum(biom_prop))
#distance_mat_full_prop_adult %>% group_by(sink) %>% summarize(sum(biom_prop))

#test<-fst::read.fst(paste0("/Users/ren/Documents/GitHub/tourism-mpa-support/connect_matrix/1_connect_larvae.fst"))
#head(test)

##-- Main code that evaluates biomass change for each stock and biodiversity score change
#-- visualize number of dives distribution using histogram with logarithmic scale
ggplot(data.frame(log(dives_input$n_dives_extrap)), aes(log(dives_input$n_dives_extrap))) + geom_histogram(bins=20)

dive_per_country <- left_join(dives_input,cell_id_with_country_kat_withregion,by="cell_id")
iso_library <- dive_per_country %>% select(sovereign1,territory1,iso_sov1,iso_ter1) %>% unique() %>% arrange(sovereign1) %>% filter(!is.na(sovereign1))

#filter CountryCode==NA and try to fill out details
no_info_pixels <- dive_per_country %>% filter(is.na(CountryCode))
no_info_pixels$cell_id #ok, "numeric(0)" means that all pixels have info now (we filled out the 5 pixels with no country info, i.e., New Zealand).

#-- Data Check: check nearest distance to fill out missing pixel info
# dive_per_country %>% select(lon,lat,CountryCode,territory1,sovereign1) %>% mutate(distance = sqrt(abs(no_info_pixels$lon[1]-lon)+abs(no_info_pixels$lat[1]-lat))) %>% arrange(distance) %>% head(20)
# #pixel 1 is New Zealand
# #New Zealand Country code is 95
# 
# dive_per_country %>% select(lon,lat,CountryCode,territory1,sovereign1) %>% mutate(distance = sqrt(abs(no_info_pixels$lon[2]-lon)+abs(no_info_pixels$lat[2]-lat))) %>% arrange(distance) %>% head(20)
# #pixel 2 is New Zealand
# 
# dive_per_country %>% select(lon,lat,CountryCode,territory1,sovereign1) %>% mutate(distance = sqrt(abs(no_info_pixels$lon[3]-lon)+abs(no_info_pixels$lat[3]-lat))) %>% arrange(distance) %>% head(20)
# #pixel 3 is New Zealand
# 
# dive_per_country %>% select(lon,lat,CountryCode,territory1,sovereign1) %>% mutate(distance = sqrt(abs(no_info_pixels$lon[4]-lon)+abs(no_info_pixels$lat[4]-lat))) %>% arrange(distance) %>% head(20)
# #pixel 4 is New Zealand
# 
# dive_per_country %>% select(lon,lat,CountryCode,territory1,sovereign1) %>% mutate(distance = sqrt(abs(no_info_pixels$lon[5]-lon)+abs(no_info_pixels$lat[5]-lat))) %>% arrange(distance) %>% head(20)
# #pixel 5 is New Zealand

# --This code is just for verifying that indeed the 5 points are all in New Zealand
# land_shp_moll<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/land_shp_moll.rds")
# no_info_pixels %>% ggplot(aes(x=lon,y=lat)) + geom_point(color="red") + #scale_fill_viridis()+#option="plasma")+#scale_fill_gradient(color=viridis)+#scale_fill_gradient(low="white", high="#00539CFF")+#guides(fill=guide_legend())+
#   theme(axis.title.x = element_blank(),axis.title.y = element_blank(), panel.background = element_blank())+
#   geom_raster()+
#   geom_sf(data = land_shp_moll,fill="darkgray", lwd = 0.1,  inherit.aes = F)

#-- load country classification (SIDS, developing, etc.)
country_classification <- read.csv(here("data","UN_territory_sovereign_classification.csv"))
#country_classification$SIDS <-as.factor(country_classification$Classification)

country_classification_with_iso <- left_join(country_classification,iso_library,by=c("sovereign1","territory1"))
country_classification_kat <- read.csv(here("data","country_status_lookup_manual_category.csv")) %>% mutate(Classification_kat = ifelse(manual_development_status=="Developed", "Developed", "Developing")) %>%
  select(iso3,Classification_kat) %>% dplyr::rename(iso_ter1 = iso3)

#-- note from Kat: use "development_status" -- developed and others.
country_classification_with_iso_and_class <- left_join(country_classification_with_iso,country_classification_kat,by="iso_ter1") %>% mutate(match = (Classification==Classification_kat))

#-- plot number of dives per country
head(dive_per_country)
plot_number_dives <- dive_per_country %>% group_by(territory1) %>% dplyr::summarize(n_dive=sum(n_dives_extrap)) %>% left_join(country_classification,by="territory1") %>%
  arrange(-n_dive) %>% slice(1:50) %>% ggplot(aes(x = reorder(as.factor(territory1), n_dive/1000000), y = n_dive/1000000, fill=Classification))+
  geom_bar(stat = "identity")+ theme_classic()+ coord_flip()+ labs(y = "Dive per year, million")+theme(axis.title.y = element_blank())
plot_number_dives

#-- plot number of dive pixels per country
plot_number_divepexels_country <- dive_per_country %>% group_by(territory1) %>% dplyr::summarize(n_divesites=n()) %>% filter(territory1!="NA") %>% left_join(country_classification,by="territory1") %>%
  arrange(-n_divesites) %>% slice(1:50) %>% ggplot(aes(x = reorder(as.factor(territory1), n_divesites), y = n_divesites, fill=Classification))+
  geom_bar(stat = "identity")+ theme_classic()+ coord_flip()+ labs(y = "Number of dive site pixel")+theme(axis.title.y = element_blank())
plot_number_divepexels_country

##-- correlate # of dives with on-reef values
ndive_per_sovereign <- dive_per_country %>% group_by(sovereign1,iso_sov1) %>% dplyr::summarize(n_dive=sum(n_dives_extrap))
head(ndive_per_sovereign)

onreef_values <- read.csv(here("data","tourism_reef_values","Tourvalues_Spalding.csv")) %>% group_by(iso_sov1) %>% summarise(onreef_value=sum(OnReef))
head(onreef_values)

correlate_dive_and_value <- merge(x=ndive_per_sovereign,y=onreef_values,by="iso_sov1")
head(correlate_dive_and_value)

plot_correlate_dive_and_value<- ggplot(correlate_dive_and_value, aes(x=onreef_value/1000000,y=n_dive/1000000))+geom_point()+geom_smooth(method = lm,colour="gray")+
  geom_text_repel(aes(onreef_value/1000000, n_dive/1000000, label = sovereign1), size = 3)+
  labs(x="On-reef tourism value, billion US$", y = "Dive per year, million")+theme_classic()
plot_correlate_dive_and_value

##--correlate # of dives with flikr data
ndive_per_sovereign <- dive_per_country %>% group_by(sovereign1,iso_sov1) %>% dplyr::summarize(n_dive=sum(n_dives_extrap))
head(ndive_per_sovereign)
flickr_data <- read.csv(here("data","flickr","flickr_webscraped_data_raw_v11.csv"))  
flickr_data_sum <- flickr_data %>% group_by(iso_code) %>% dplyr::summarize(count=n()) %>% dplyr::rename(iso_sov1=iso_code)
head(flickr_data_sum)

correlate_dive_and_flickr <- merge(x=ndive_per_sovereign,y=flickr_data_sum,by="iso_sov1") %>% filter(is.na(iso_sov1)==F)
head(correlate_dive_and_flickr)

plot_correlate_dive_and_flickr <- ggplot(correlate_dive_and_flickr, aes(x=count,y=n_dive/1000000))+geom_point()+geom_smooth(method = lm,colour="gray")+
  geom_text_repel(aes(count, n_dive/1000000, label = sovereign1), size = 3)+
  labs(x="Number of flickr photos", y = "Dive per year, million")+theme_classic()

#figure 1 main
figure1<-cowplot::plot_grid(plot_number_divepexels_country, plot_number_dives,plot_correlate_dive_and_value,plot_correlate_dive_and_flickr, ncol = 2, labels = "AUTO",rel_heights=c(1,0.5))
figure1
ggsave(here("figures","main","plot_number_dives.jpg"),figure1, width = 20, height = 20, units = "cm")

#saving a country file for me to assign country development categorization
##---[no need to run] head(dive_per_country)
##---[no need to run] dive_per_country %>% select(sovereign1) %>% unique() %>% write.csv(.,file = here("data","country_classification.csv"))
##---[no need to run] dive_per_country %>% select(territory1) %>% unique() %>% write.csv(.,file = here("data","territory_classification.csv"))
##---[no need to run] dive_per_country %>% select(sovereign1,territory1) %>% unique() %>% arrange(sovereign1) %>% filter(!is.na(sovereign1)) %>% write.csv(.,file = here("data","territory_sovereign_classification.csv"))
##---[no need to run] checkme <- read.csv(here("data","UN_territory_sovereign_classification.csv"))

#prep data to plot the world's dive sites
ocean_coordinates_dive_suitable <- left_join(ocean_coordinates,suitability_input,by="cell_id")
ocean_coordinates_dive_suitable_v2 <- left_join(ocean_coordinates_dive_suitable,dives_input,by="cell_id")

#plot suitability later or current locations of dive sites
world_dive_sites <- ocean_coordinates_dive_suitable_v2 %>% mutate(suitable = replace_na(suitable,0)) %>% ggplot() + geom_raster(aes(x=lon,y=lat,fill=suitable)) + scale_fill_gradientn(colours=c("black","orange")) #ok, great
world_dive_sites
ggsave(here("figures","supplementary","world_dive_sites.jpg"),world_dive_sites, width = 20, height = 12, units = "cm")

#-- MPA location. Assume that a pixel is an MPA is f_highly_mpa>=0.5.
MPA_vec <- transformed_stockdistrib %>% dplyr::select(cell_id,f_highly_mpa) %>% mutate(f_highly_mpa = (f_highly_mpa>=0.5))
MPA_loc <- MPA_vec %>% filter(f_highly_mpa=="TRUE") %>% select(cell_id)

#-- Question: How many of the suitable dive sites are already in MPAs?
divesite_in_MPA <- suitability_input %>% filter(cell_id %in% MPA_loc$cell_id) %>% dim()
dive_cell_id_MPA <- suitability_input %>% filter(cell_id %in% MPA_loc$cell_id) %>% select(cell_id)
divesite_in_MPA#22 pixels out of 1814
divesite_in_MPA[1]*100/dim(dives_input)[1] #1.21% of the dive sites are inside MPA (using 50 x 50km resolution).

##--biodiversity prep
#-- load the functions
# sapply(list.files(pattern = "[.]R$", path = here::here("scripts", "functions"), full.names = TRUE),source)
source(here("scripts", "functions","calculate_relative_bio_benefit.R"))
source(here("scripts", "functions","func_evaluateMPA_explicit.R"))

#-- load data files necessary for biodiversity model
load(file = file.path(this_project_dir,  "data", "02-processed", "model-inputs", "bio_model_input.RData"))
# set Z for biodiversity
z_bio <- 0.25

##---BIODIVERSITY CODE
# Calculate biodiversity benefit from today's protected cells
bio_benefit_current<-calculate_relative_bio_benefit(is_mpa_vect = MPA_vec$f_highly_mpa, v_out_matrix =  v_out_matrix,
                                                    v_in_matrix = v_in_matrix, weights  = bio_weights, 
                                                    z_bio = z_bio, bau_benefit = bau_benefit, total_benefit_diff = total_benefit_diff)

#Biodiv benefits of zero MPA
MPA_vec$f_zero_mpa<-FALSE
bio_benefit_zero<-calculate_relative_bio_benefit(is_mpa_vect = MPA_vec$f_zero_mpa, v_out_matrix =  v_out_matrix,
                                                 v_in_matrix = v_in_matrix, weights  = bio_weights, 
                                                 z_bio = z_bio, bau_benefit = bau_benefit, total_benefit_diff = total_benefit_diff)


#Biodiv benefits of 100% ocean in MPA
MPA_vec$f_all_mpa<-TRUE
bio_benefit_all<-calculate_relative_bio_benefit(is_mpa_vect = MPA_vec$f_all_mpa, v_out_matrix =  v_out_matrix,
                                                v_in_matrix = v_in_matrix, weights  = bio_weights, 
                                                z_bio = z_bio, bau_benefit = bau_benefit, total_benefit_diff = total_benefit_diff)
#% increase from zero MPA to current MPA
(bio_benefit_current-bio_benefit_zero)*100/bio_benefit_zero
#% increase from current MPA to all MPA
(bio_benefit_all-bio_benefit_current)*100/bio_benefit_current

#biodiversity score, no MPA
bio_benefit_zero/max_benefit_allthreats #0.5326662

#biodiversity score, current MPA
bio_benefit_current/max_benefit_allthreats #0.5445721

#biodiversity score, all MPA
bio_benefit_all/max_benefit_allthreats #0.7563106

#% increase from current MPA to all MPA
(bio_benefit_all-bio_benefit_current)*100/bio_benefit_current

#% increase from current MPA to all threats solved (not needed)
(max_benefit_allthreats-bio_benefit_current)*100/bio_benefit_current

##----- BIOMASS CODE
#--test:
#func_evaluateMPA_explicit(stock_num=1, transformed_stockdistrib,MegaData_filtered_step_fin,MPA_loc)$biomass

rerun_biomass_code <- 0 #1 for on, 0 to switch this off
if(rerun_biomass_code == 1){
  stock_include <- stocklist#c(1,2,4,5,6)#stocklist[8] #c(1,2,4,5,6) #comment this. this is just a placeholder for building our code
  
  ptm <- proc.time()
  registerDoParallel(detectCores()/2)
  collate_biomass_equi_merged <- foreach(stock_num=stock_include, .combine='cbind') %dopar% {
    func_evaluateMPA_explicit(stock_num, transformed_stockdistrib,MegaData_filtered_step_fin,MPA_loc)$biomass
  }
  doParallel::stopImplicitCluster()
  (proc.time() - ptm)/60 #check process time in minutes
  colnames(collate_biomass_equi_merged)<-MegaData_filtered_step_fin$stockid[stock_include]
  dim(collate_biomass_equi_merged)
  
  saveRDS(collate_biomass_equi_merged, file = here("data","collate_biomass_equi_merged.rds"))
}
collate_biomass_equi_merged<-readRDS(file = here("data","collate_biomass_equi_merged.rds"))

##--Calculate B/K per pixel
#compute K per pixel of our stock list
max(transformed_stockdistrib[6:1155],na.rm=T)
min(transformed_stockdistrib[6:1155],na.rm=T)
full_stock_distrib <- transformed_stockdistrib[6:1155]
filtered_stock_distrib <- full_stock_distrib %>% select(c(stocklist))#full_stock_distrib[,stocklist_index] #this is the stock distrib of our filtered stock. Max value is 1 and with NAs
dim(filtered_stock_distrib) #149547 x 813

#K/geogrange 
Kmultiplyer <- MegaData_filtered_step_fin %>% select(Kperpixel) %>% slice(stocklist_index) %>% data.frame()
head(Kmultiplyer)
dim(Kmultiplyer)
min(Kmultiplyer)

df_Kmultiplyer <- t(data.frame(rep(Kmultiplyer,each=149547)))
dim(df_Kmultiplyer)
min(df_Kmultiplyer)

#filtered_stock_distrib[is.na(filtered_stock_distrib)] <- 0 #Replace NAs to 0
dim(filtered_stock_distrib) #stock distribution we considered in our model
dim(df_Kmultiplyer)

Kdistrib <- filtered_stock_distrib * df_Kmultiplyer#multiply with Kmultiplyer

#check if the above is correct
filtered_stock_distrib %>% select(`Fis-29732`) %>% filter(!is.na(`Fis-29732`)) %>% head()
Kdistrib %>% select(`Fis-29732`) %>% filter(!is.na(`Fis-29732`)) %>% head() #ok

TotalKperPixel <- rowSums(Kdistrib,na.rm=T) #colSums to get the K per pixel

#k per pixel.
transformed_stockdistrib %>% ggplot(aes(x=lon,y=lat,fill=TotalKperPixel)) + scale_fill_viridis_c() + geom_raster()

#calculate B/K per pixel
BvK <- rowSums(collate_biomass_equi_merged,na.rm = TRUE)/TotalKperPixel
length(BvK)
max(BvK,na.rm=T)
BvK[order(-BvK)]
transformed_stockdistrib %>% ggplot(aes(x=lon,y=lat,fill=BvK)) + scale_fill_viridis_c(limits = c(0, 1)) + geom_raster() #ok, great

##-----NOW, close all dive pixels and calculate BvK
head(suitability_input)
#change dive sites into MPAs
MPA_vec_dive <- transformed_stockdistrib %>% dplyr::select(cell_id,f_highly_mpa) %>% mutate(f_highly_mpa = (f_highly_mpa>=0.5)) %>% mutate(f_highly_mpa = replace(f_highly_mpa,cell_id %in% suitability_input$cell_id, TRUE))
#this is current MPA + converting all dive sites into MPA.
MPA_loc_dive <- MPA_vec_dive %>% filter(f_highly_mpa=="TRUE") %>% select(cell_id)

rerun_biomass_wMPAdive_code <- 0 #1 for on, 0 to switch this off
if(rerun_biomass_wMPAdive_code == 1){
  ptm <- proc.time()
  registerDoParallel(detectCores()/2)
  collate_biomass_equi_merged_dive <- foreach(stock_num=stock_include, .combine='cbind') %dopar% {
    func_evaluateMPA_explicit(stock_num, transformed_stockdistrib,MegaData_filtered_step_fin,MPA_loc_dive)$biomass
  }
  doParallel::stopImplicitCluster()
  (proc.time() - ptm)/60 #check process time in minutes
  colnames(collate_biomass_equi_merged_dive) <- MegaData_filtered_step_fin$stockid[stock_include]
  
  saveRDS(collate_biomass_equi_merged_dive, file = here("data","collate_biomass_equi_merged_dive.rds"))
}
collate_biomass_equi_merged_dive <- readRDS(file = here("data","collate_biomass_equi_merged_dive.rds"))

#compute B/K per pixel
BvK_dive <- rowSums(collate_biomass_equi_merged_dive,na.rm = TRUE)/TotalKperPixel
#transformed_stockdistrib %>% ggplot(aes(x=lon,y=lat,fill=BvK_dive)) + scale_fill_viridis_c(limits = c(0, 1)) + geom_raster() 
transformed_stockdistrib %>% ggplot(aes(x=lon,y=lat,fill=BvK_dive)) + scale_fill_viridis_c() + geom_raster() 

Delta_biomass <- BvK_dive - BvK
Delta_biomass[is.nan(Delta_biomass)] <- 0
Delta_biomass[Delta_biomass<0] <- 0 #in case
length(Delta_biomass)

#cell_id of unprotected dive sites
dive_cell_id_unprotected <- suitability_input$cell_id[!(suitability_input$cell_id %in% dive_cell_id_MPA$cell_id)]
length(dive_cell_id_unprotected)

#let us subset to dive sites only and check the results.
plot(Delta_biomass[suitability_input$cell_id])
mean(Delta_biomass[suitability_input$cell_id]) #0.438 increase in biomass density or from below's calculation, x% of base biomass.

#unprotected dive sites only
length(Delta_biomass[dive_cell_id_unprotected])
plot(Delta_biomass[dive_cell_id_unprotected])
mean(Delta_biomass[dive_cell_id_unprotected]) #0.4437

transformed_stockdistrib %>% ggplot(aes(x=lon,y=lat,fill=Delta_biomass)) + scale_fill_viridis_c(limits = c(0, max(Delta_biomass,na.rm=T))) + geom_raster()

#--biomass ratio
#calculate % increase of biomass inside MPA
biomass_data <- ocean_coordinates %>% mutate(ratio_biomass=(BvK_dive-BvK)*100/BvK, TotalKperPixel = TotalKperPixel, unprotected_dive_site=NA)
biomass_data$ratio_biomass[is.na(biomass_data$ratio_biomass)]<-0 #convert NA to 0
biomass_data$unprotected_dive_site[dive_cell_id_unprotected] <- 1 #identify unprotected dive sites
biomass_data <- biomass_data %>% mutate(ratio_biom_divesite = ratio_biomass*unprotected_dive_site)

#K-weighted mean and stdev
#weighted.mean(biomass_data_divesites$ratio_biom_divesite, biomass_data_divesites$TotalKperPixel)
wtd.mean(biomass_data$ratio_biom_divesite, biomass_data$TotalKperPixel) #113.1
sqrt(wtd.var(biomass_data$ratio_biom_divesite, biomass_data$TotalKperPixel))#standard dev, 132.6

#regular stat
mean(biomass_data$ratio_biom_divesite, na.rm=T) #on average, total biomass inside MPAs will increase by 117% vs. BAU

#check the spread of the ratio of biomass
sample.n <- sum(!is.na(biomass_data$ratio_biom_divesite))
(sample.sd <- sd(biomass_data$ratio_biom_divesite,na.rm=T))
(sample.se <- sample.sd/sqrt(sample.n))

land_shp_moll <- readRDS(here("data","land_shp_moll.rds"))

#save data needed for plotting Fig 2
#1) "biomass_data" contains the change in biomass due to MPA. If you want the dive sites only, use "biomass_data_divesites"
#2) "land_shp_moll" for the EEZ land background
save(biomass_data, land_shp_moll, file = here("scripts","figures","Figure2_data.RData"))

cuts <-c(0,20,40,60,80,100, max(biomass_data$ratio_biom_divesite, na.rm=T))

b0 <- biomass_data %>% ggplot(aes(x=lon,y=lat,fill=ratio_biom_divesite)) + geom_raster(aes(fill = cut(ratio_biom_divesite, cuts))) +
  scale_fill_brewer(palette = "RdBu", drop = FALSE) + 
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), panel.background = element_blank())+
  geom_sf(data = land_shp_moll,fill="darkgrey", lwd = 0,  inherit.aes = F)+labs(title = "", fill = "% biomass increase")

b1 <- biomass_data %>% ggplot(aes(x=lon,y=lat,fill=ratio_biom_divesite)) + geom_raster(aes(fill = cut(ratio_biom_divesite, cuts))) +
  scale_fill_brewer(palette = "RdBu", drop = FALSE) + 
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), panel.background = element_blank(),legend.position="none")+
  geom_sf(data = land_shp_moll,fill="darkgrey", lwd = 0,  inherit.aes = F)+
  xlim(0.9e7, 1.5e7) +ylim(-2.5e6,3e6)+labs(title = "Southeast Asia", fill = "% biomass increase")

b2 <- biomass_data %>% ggplot(aes(x=lon,y=lat,fill=ratio_biom_divesite)) + geom_raster(aes(fill = cut(ratio_biom_divesite, cuts))) +
  scale_fill_brewer(palette = "RdBu", drop = FALSE) + 
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), panel.background = element_blank(),legend.position="none")+
  geom_sf(data = land_shp_moll,fill="darkgrey", lwd = 0,  inherit.aes = F)+
  xlim(-0.2e7, 0.4e7) +ylim(2.5e6,7.5e6)+labs(title = "Europe", fill = "% biomass increase")

b3 <- biomass_data %>% ggplot(aes(x=lon,y=lat,fill=ratio_biom_divesite)) + geom_raster(aes(fill = cut(ratio_biom_divesite, cuts))) +
  scale_fill_brewer(palette = "RdBu", drop = FALSE) + 
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), panel.background = element_blank(),legend.position="none")+
  geom_sf(data = land_shp_moll,fill="darkgrey", lwd = 0,  inherit.aes = F)+
  xlim(-1.1e7, -0.5e7) +ylim(-0.5e6,5.2e6)+labs(title = "Carribean", fill = "% biomass ratio")

bottom_row <- plot_grid(b1,b2,b3, nrow=1, labels = c('B', 'C','D'), label_size = 12)

#Plot and Save Figure 2, biomass change plot
Fig2 <- cowplot::plot_grid(b0,bottom_row, nrow = 2, labels =c('A',''),rel_heights=c(2,1))
Fig2

ggsave(here("figures","main","Fig2.jpg"), Fig2, width = 15, height = 10, units = "in")

##-----NOW, close all dive pixels and calculate change in diversity
biodiv_bau <- calculate_relative_bio_benefit(is_mpa_vect = MPA_vec$f_highly_mpa, v_out_matrix =  v_out_matrix,
                                             v_in_matrix = v_in_matrix, weights  = bio_weights, 
                                             z_bio = z_bio, bau_benefit = bau_benefit, total_benefit_diff = total_benefit_diff)

biodiv_dive <- calculate_relative_bio_benefit(is_mpa_vect = MPA_vec_dive$f_highly_mpa, v_out_matrix =  v_out_matrix,
                                              v_in_matrix = v_in_matrix, weights  = bio_weights, 
                                              z_bio = z_bio, bau_benefit = bau_benefit, total_benefit_diff = total_benefit_diff)

#biodiversity score, with dive sites protected
biodiv_dive/max_benefit_allthreats #0.57

#% increase in biodiv score from current MPA to MPA with dive sites
(biodiv_dive-bio_benefit_current)*100/bio_benefit_current

#How much is the contribution of each pixel to the biodiversity change?
#solution: 1.) close each non-MPA dive pixel, then calculate the biodiversity change. Sum all deltas then normalize the per pixel contribution.
#identify the non-MPA dive pixels
#dive pixels -- use dive_suitability$cell_id for the cell ids of dive pixels.

divepixels_unprotected <- suitability_input$cell_id[which(! suitability_input$cell_id %in% MPA_loc$cell_id)]
length(divepixels_unprotected)#1792 pixels
#compare this with all dive pixels
length(suitability_input$cell_id)#1814 pixels

#--run this just once because it takes time. Rerun if there are changes in the dive locations or MPA locations.
run_me <- 0
if(run_me==1){
  #close each pixel and compute for the biodiversity benefits
  MPA_vec_base <- transformed_stockdistrib %>% dplyr::select(cell_id,f_highly_mpa) %>% mutate(f_highly_mpa = (f_highly_mpa>=0.5))
  
  store_per_pixel_delta_biodiv_benefit <- data.frame(cell_id=divepixels_unprotected)
  store_per_pixel_delta_biodiv_benefit$delta_biodiv_benefit <- 0
  
  #parallel version of the code
  #registerDoParallel(detectCores()/2)
  #registerDoParallel(5)
  #foreach(i=1:length(divepixels_unprotected)) %dopar% {
  for (i in 1:length(divepixels_unprotected)){  
    MPA_vec_close1 <- MPA_vec_base %>% mutate(f_highly_mpa = replace(f_highly_mpa,cell_id %in% divepixels_unprotected[i], TRUE))
    #evaluate biodiv benefit of closing 1 pixel then subtract with the BAU result. Then save the results.
    store_per_pixel_delta_biodiv_benefit$delta_biodiv_benefit[i] <- calculate_relative_bio_benefit(is_mpa_vect = MPA_vec_close1$f_highly_mpa, v_out_matrix =  v_out_matrix,
                                                                                                   v_in_matrix = v_in_matrix, weights  = bio_weights, 
                                                                                                   z_bio = z_bio, bau_benefit = bau_benefit, total_benefit_diff = total_benefit_diff) - biodiv_bau
    print(i)
  }
  #doParallel::stopImplicitCluster()
  
  head(store_per_pixel_delta_biodiv_benefit)
  #normalize the benefit value per pixel and save it
  store_per_pixel_delta_biodiv_benefit$delta_biodiv_benefit<-store_per_pixel_delta_biodiv_benefit$delta_biodiv_benefit/sum(store_per_pixel_delta_biodiv_benefit$delta_biodiv_benefit)
  sum(store_per_pixel_delta_biodiv_benefit$delta_biodiv_benefit)
  #plot(store_per_pixel_delta_biodiv_benefit$delta_biodiv_benefit)
  saveRDS(store_per_pixel_delta_biodiv_benefit, file = here("data","per_pixel_delta_biodiv_benefit.rds"))
}

#-- load saved normalized benefit value when each pixel is protected one by one.
normalized_per_pixel_delta_biodiv_benefit <- readRDS(file = here("data","per_pixel_delta_biodiv_benefit.rds"))
normalized_per_pixel_delta_biodiv_benefit %>% filter(cell_id %in% divepixels_unprotected)

#-- convert per pixel delta biodiv benefit to be max value per territory, sovereign, and region
cell_id_geography <- dive_per_country %>% select(cell_id, lon,lat,CountryCode,territory1,sovereign1, region_fill)
biodiv_geography <- left_join(normalized_per_pixel_delta_biodiv_benefit,cell_id_geography,by="cell_id")

#plot the no country label entries. This should be null.
biodiv_geography %>% filter(is.na(CountryCode)) %>% ggplot(aes(x=lon,y=lat)) + geom_raster()

#--renormalize values based on different geography partitioning
#renormalize_country_biodiv_benefit <- biodiv_territory %>% group_by(CountryCode) %>% mutate(max_benefit_country = max(delta_biodiv_benefit)) %>%
#  mutate(renorm_biodiv_benefit = delta_biodiv_benefit/max_benefit_country)

head(biodiv_geography)
#renormalize by territory, sovereign, and region
renormalize_country_biodiv_benefit <- biodiv_geography %>% group_by(territory1) %>% mutate(max_benefit_ter = max(delta_biodiv_benefit)) %>%
  mutate(renorm_biodiv_benefit_ter = delta_biodiv_benefit/max_benefit_ter) %>% group_by(sovereign1) %>% mutate(max_benefit_sov = max(delta_biodiv_benefit)) %>%
  mutate(renorm_biodiv_benefit_sov = delta_biodiv_benefit/max_benefit_sov) %>% group_by(region_fill) %>% mutate(max_benefit_reg = max(delta_biodiv_benefit)) %>%
  mutate(renorm_biodiv_benefit_reg = delta_biodiv_benefit/max_benefit_reg)


##--DIVE TOURISM BIOECONOMIC MODEL from here

###--START--PRICE DATA DIMENSIONS CHECK---!!!
dim(suitability_input)#out:1814x2
dim(price_constant_input)#out:1814x2
length(divepixels_unprotected)#out:1792

suitability_input %>% filter(cell_id %in% price_constant_input$cell_id) %>% dim() #out:1814
price_constant_input %>% filter(cell_id %in% divepixels_unprotected) %>% dim() #out: 1792
###--END--PRICE DATA DIMENSIONS CHECK---!!!

change_biomass_density_full <- Delta_biomass[divepixels_unprotected]
change_diversity_full <- renormalize_country_biodiv_benefit$renorm_biodiv_benefit_ter
choke_price <- 220.32 #empirically derived. 99% top percentile.
#make sure that the choke price is > than the max interpolated price... by definition, no diving should exist at choke price
max(price_interpolated_input$price) #US$ 125.88   #max value of the interpolated price data

price_per_dive_constant <- price_constant_input %>% filter(cell_id %in% divepixels_unprotected) %>% select(price)#rep(mean(price_constant_input$price),times=length(divepixels_unprotected)) #(divepixels_unprotected*0)+60#placeholder while resolving my question
price_per_dive_constant <- price_per_dive_constant$price

effect_name <- 4.2/2
effect_biomass <- price_per_dive_constant*0.8386*change_biomass_density_full/max(change_biomass_density_full)
effect_biodiversity <- price_per_dive_constant*0.8186*change_diversity_full/max(change_diversity_full)

#plot WTP histogram
hist(effect_biomass,20)
hist(effect_biodiversity,20)

mean(effect_biomass)
mean(effect_biodiversity)
max(change_biomass_density_full)

#paper stats --- MPA and non-MPA combined
#revenue = price * ndives
sum(dives_input$n_dives_extrap)*price_per_dive_constant[1]
sum(dives_input$n_dives_extrap_min)*price_per_dive_constant[1]
sum(dives_input$n_dives_extrap_max)*price_per_dive_constant[1]
#consumer surplus of all dive sites in the world, regardless if they are protected or unprotected
sum((0.5*dives_input$n_dives_extrap*(choke_price-price_per_dive_constant[1])))/1e9 #2.67 billion
#lower bound
sum((0.5*dives_input$n_dives_extrap_min*(choke_price-price_per_dive_constant[1])))/1e9 #1.38 billion
#upper bound
sum((0.5*dives_input$n_dives_extrap_max*(choke_price-price_per_dive_constant[1])))/1e9 #4.36 billion

#remove the pixels that are in MPA
base_number_dive <- dives_input$n_dives_extrap[dives_input$cell_id %in% divepixels_unprotected]

parameter_b <- base_number_dive/(choke_price-price_per_dive_constant)
parameter_a <- parameter_b*choke_price

new_choke_price <- (parameter_a+((effect_name+effect_biomass+effect_biodiversity)*base_number_dive/(choke_price-price_per_dive_constant)))/parameter_b

user_fee_opt <- effect_name + effect_biomass + effect_biodiversity
plot(user_fee_opt) #cool! so we have user fee per site.
mean(user_fee_opt) #53USD

dive_tax <- 0

shifted_number_dive <- parameter_a - parameter_b*price_per_dive_constant + ((effect_name+effect_biomass+effect_biodiversity-dive_tax)*base_number_dive/(choke_price-price_per_dive_constant))
shifted_number_dive

new_dive_MPAnotax <- shifted_number_dive - base_number_dive

#calculate average % increase in the number of dives with no crowding effect.
percent_dive_increase <- (shifted_number_dive - base_number_dive)*100/base_number_dive
plot(percent_dive_increase)
mean(percent_dive_increase)#32.89% increase in the number of dives per pixel on average

#in total, 32.1% increase in the number of dives
(sum(shifted_number_dive) - sum(base_number_dive))*100/sum(base_number_dive) #or
sum(shifted_number_dive) - sum(base_number_dive) #10.49 million more

change_dive_revenue <- price_per_dive_constant*(shifted_number_dive - base_number_dive) # in USD
new_choke_price <- (parameter_a+((effect_name+effect_biomass+effect_biodiversity)*base_number_dive/(choke_price-price_per_dive_constant)) - (dive_tax*base_number_dive/(choke_price-price_per_dive_constant))   )/parameter_b

#current consumer surplus (base dive number), current MPA removed
sum((0.5*base_number_dive*(choke_price-price_per_dive_constant)))/1e9 #2.64 billion

#consumer surplus (lower bound dive number)
low_number_dive <- dives_input$n_dives_extrap_min[dives_input$cell_id %in% divepixels_unprotected]
sum((0.5*low_number_dive*(choke_price-price_per_dive_constant)))/1e9 #1.36 billion

#consumer surplus (upper bound dive number)
high_number_dive <- dives_input$n_dives_extrap_max[dives_input$cell_id %in% divepixels_unprotected]
sum((0.5*high_number_dive*(choke_price-price_per_dive_constant)))/1e9 #4.31 billion

change_consumer_suplus <- (0.5*shifted_number_dive*(new_choke_price-price_per_dive_constant))-(0.5*base_number_dive*(choke_price-price_per_dive_constant))
sum(change_dive_revenue/1e9)#billion
sum(change_consumer_suplus/1e9)#billion

(total_wtp <- effect_name + effect_biomass + effect_biodiversity)
(dive_tax <- (shifted_number_dive - base_number_dive)/parameter_b) #this is the same as total wtp
plot(total_wtp,dive_tax)#proof that total_wp is equal to the dive_tax

(tax_revenue <- dive_tax*base_number_dive)
sum(tax_revenue)/1e9 #this is how much you can get from pixel-specific tax when establishing an MPA and imposing tax that does not affect dive numbers

##what if we implement a tax of mean(user_fee_opt)?
#MANUSCRIPT STAT:  Imposing a dive fee equal to the additional WTP of divers holds dive numbers constant pre- and post- MPA network implementation, while generating x billion in additional revenue
sum(base_number_dive)
(tax_revenue_constant_tax <- sum(mean(user_fee_opt)*base_number_dive)/1e9)

#explore here --- different user fees.
explore_user_fee_wMPA <- list()
explore_user_fee_noMPA <- list()

wtp_combined <-effect_name + effect_biomass + effect_biodiversity
average_user_fee<-mean(wtp_combined)
average_user_fee
dive_group_size<-5 #this is for scenario with additional crowding -- sensitivity
reduce_wtp_crowd<-0.1 #this is for scenario with additional crowding -- sensitivity

source(here("scripts","functions","func_evaluate_divefee.R"))

##--SENSITIVITY ANALYSIS
#0.) base model
length(base_number_dive)
explore_user_fee_base <- func_evaluate_divefee(wtp_combined=wtp_combined,base_number_dive=base_number_dive,choke_price=choke_price,price_per_dive=price_per_dive_constant,congestion=0)
explore_user_fee_base %>% filter(Scenario=="With MPA") %>% summarise(max = max(tax_revenue)/1e9) ##MAXIMUM DIVE FEE REVENUE

##1.) add diver congestion
#explore_user_fee_congestion <- func_evaluate_divefee(wtp_combined=wtp_combined,base_number_dive=base_number_dive,choke_price=choke_price,price_per_dive=price_per_dive_constant,congestion=1,reduce_wtp_crowd=reduce_wtp_crowd)
#explore_user_fee_congestion %>% filter(Scenario=="With MPA") %>% summarise(max = max(tax_revenue)/1e9) ##MAXIMUM DIVE FEE REVENUE

#1.) lower estimate dive number
low_number_dive <- dives_input$n_dives_extrap_min[dives_input$cell_id %in% divepixels_unprotected]
explore_user_fee_ndivelow <- func_evaluate_divefee(wtp_combined=wtp_combined,base_number_dive=low_number_dive,choke_price=choke_price,price_per_dive=price_per_dive_constant,congestion=0)
explore_user_fee_ndivelow %>% filter(Scenario=="With MPA") %>% summarise(max = max(tax_revenue)/1e9) ##MAXIMUM DIVE FEE REVENUE

#2.) Upper estimate dive number
high_number_dive <- dives_input$n_dives_extrap_max[dives_input$cell_id %in% divepixels_unprotected]
explore_user_fee_ndivehigh <- func_evaluate_divefee(wtp_combined=wtp_combined,base_number_dive=high_number_dive,choke_price=choke_price,price_per_dive=price_per_dive_constant,congestion=0)
explore_user_fee_ndivehigh %>% filter(Scenario=="With MPA") %>% summarise(max = max(tax_revenue)/1e9) ##MAXIMUM DIVE FEE REVENUE

#4.) Price averaged per EEZ or Region if EEZ value unavailable
price_per_dive_country_region <- price_country_region_input %>% filter(cell_id %in% divepixels_unprotected) %>% select(price)
price_per_dive_country_region <- price_per_dive_country_region$price
explore_user_fee_eezprice <- func_evaluate_divefee(wtp_combined=wtp_combined,base_number_dive=base_number_dive,choke_price=choke_price,price_per_dive=price_per_dive_country_region,congestion=0)
explore_user_fee_eezprice %>% filter(Scenario=="With MPA") %>% summarise(max = max(tax_revenue)/1e9) ##MAXIMUM DIVE FEE REVENUE

#5.) Interpolated price
price_per_dive_interpolated <- price_interpolated_input %>% filter(cell_id %in% divepixels_unprotected) %>% select(price)
price_per_dive_interpolated <- price_per_dive_interpolated$price
explore_user_fee_interpolatedprice <- func_evaluate_divefee(wtp_combined=wtp_combined,base_number_dive=base_number_dive,choke_price=choke_price,price_per_dive=price_per_dive_interpolated,congestion=0)
explore_user_fee_interpolatedprice %>% filter(Scenario=="With MPA") %>% summarise(max = max(tax_revenue)/1e9) ##MAXIMUM DIVE FEE REVENUE

#6.) Normalization process of biodiversity benefit: just re-run the code by changing groupings (ter, sov, reg)

###----Add uncertainty bounds based on the number of dives data
#Number of dives in non-MPA dive pixels
ndives_range_unprotected <- dives_input %>% filter(cell_id %in% divepixels_unprotected)
head(ndives_range_unprotected)

explore_user_fee_repeat <- list()
index <- 0
for (i in 1:500){
  index <- index+1
  #option 1: add noise to individual dive site    
  #rand_number_dive <- ndives_range_unprotected %>% group_by(cell_id) %>% mutate(rand_mun=runif(1, min = 0, max = 1)) %>% mutate(n_dive_rand = round(n_dives_extrap_min+(rand_mun*(n_dives_extrap_max-n_dives_extrap_min))))
  #option 2: add same noise to all dive site    
  rand_number_dive <- ndives_range_unprotected %>% mutate(rand_mun=runif(1, min = 0, max = 1)) %>% mutate(n_dive_rand = round(n_dives_extrap_min+(rand_mun*(n_dives_extrap_max-n_dives_extrap_min))))
  explore_user_fee <- func_evaluate_divefee(wtp_combined=wtp_combined,base_number_dive=rand_number_dive$n_dive_rand,choke_price=choke_price,price_per_dive=price_per_dive_constant,congestion=0)
  explore_user_fee$run <- index
  explore_user_fee_repeat[[index]]<-explore_user_fee
}

explore_user_fee_merged <- do.call("rbind",explore_user_fee_repeat)
dim(explore_user_fee_merged)
head(explore_user_fee_merged)


##-------PLOTTING CODE-------------
#--Prepare Figure 4 plots

mean_taxrevenue_db <- explore_user_fee_merged %>% select(dive_tax, tax_revenue, Scenario) %>% group_by(Scenario,dive_tax) %>% summarise(mean_taxrev = mean(tax_revenue))
head(mean_taxrevenue_db)

#MANUSCRIPT STAT: "We find that it is possible to generate up to x billion of revenue from user fees even without establishing MPAs on existing dive sites"
mean_taxrevenue_db %>% filter(Scenario=="No MPA") %>% summarise(max(mean_taxrev)/1e9)# Ans: 1.45
#MANUSCRIPT STAT: "The maximum possible amount of revenue that can be generated from dive fees also increases to x when dive fees are combined with highly protected MPAs"
mean_taxrevenue_db %>% filter(Scenario=="With MPA") %>% summarise(max(mean_taxrev)/1e9)# Ans: 2.53

panel1 <- explore_user_fee_merged %>% ggplot(aes(x=dive_tax,y=tax_revenue/1e9,color=Scenario, group=interaction(Scenario,run)))+geom_line(size=1, alpha=0.01)+
  geom_line(data=mean_taxrevenue_db, aes(x=dive_tax, y=mean_taxrev/1e9, group=Scenario, linetype=Scenario))+theme_classic()+labs(x ="", y = "Dive fee revenue\n(billion US$)")+ theme(legend.position = "none")   
panel1

max_val <- max(explore_user_fee_merged$tax_revenue/1e9)
min_val <- min(explore_user_fee_merged$tax_revenue/1e9)
min_ref <- min_val+0.6*(max_val-min_val)

#difference plot panel 1
data1 <- explore_user_fee_merged %>% select(dive_tax, tax_revenue, Scenario, run) %>% filter(Scenario =="With MPA")
data2 <- explore_user_fee_merged %>% select(dive_tax, tax_revenue, Scenario, run) %>% filter(Scenario =="No MPA")
diff_data <- left_join(data1,data2, by=c("dive_tax","run")) %>% mutate(difference = tax_revenue.x-tax_revenue.y)

mean_tax_revenue_diff <- diff_data %>% group_by(dive_tax) %>% summarise(mean_tax_revenue_diff = mean(difference))
panel1_diff <- diff_data %>% ggplot(aes(x=dive_tax,y=difference/1e9)) + geom_line(aes(group=run),size=1, alpha=0.01)+
  geom_line(data=mean_tax_revenue_diff, aes(x=dive_tax, y=mean_tax_revenue_diff/1e9))+theme_classic()+labs(x ="User fee per dive\n(US$)", y = "Difference\n(billion US$)")+ theme(legend.position = "none")   +
  theme(panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),legend.background = element_rect(fill='transparent'),legend.box.background = element_rect(fill='transparent'))+
  ylim(0,max(diff_data$difference/1e9))
panel1_diff

panel1_inset <- panel1+annotation_custom(grob = ggplotGrob(panel1_diff), xmin=75, xmax=200, ymin=min_ref, ymax=max_val) 

#+geom_ribbon(data=mean_taxrevenue_db, aes(ymin=(mean_taxrev/1e9)-0.5, ymax=(mean_taxrev/1e9)+0.5, group=Scenario))
#see sample plotting here: https://thenode.biologists.com/visualizing-data-with-r-ggplot2/education/

mean_delta_dive_revenue <- explore_user_fee_merged %>% select(dive_tax, delta_dive_revenue, Scenario) %>% group_by(Scenario,dive_tax) %>% summarise(mean_delta_dive_revenue = mean(delta_dive_revenue))
panel2 <- explore_user_fee_merged %>% ggplot(aes(x=dive_tax,y=delta_dive_revenue/1e9,color=Scenario, group=interaction(Scenario,run)))+geom_line(size=1, alpha=0.01)+
  geom_line(data=mean_delta_dive_revenue, aes(x=dive_tax, y=mean_delta_dive_revenue/1e9, group=Scenario, linetype=Scenario))+theme_classic()+labs(x ="", y = "\u0394 dive revenue\n(billion US$)")+ theme(legend.position = "none")   
panel2

max_val <- max(explore_user_fee_merged$delta_dive_revenue/1e9)
min_val <- min(explore_user_fee_merged$delta_dive_revenue/1e9)
min_ref <- min_val+0.6*(max_val-min_val)

#difference plot panel 2
data1 <- explore_user_fee_merged %>% select(dive_tax, delta_dive_revenue, Scenario, run) %>% filter(Scenario =="With MPA")
data2 <- explore_user_fee_merged %>% select(dive_tax, delta_dive_revenue, Scenario, run) %>% filter(Scenario =="No MPA")
diff_data <- left_join(data1,data2, by=c("dive_tax","run")) %>% mutate(difference = delta_dive_revenue.x-delta_dive_revenue.y)

mean_delta_dive_revenue_diff <- diff_data %>% group_by(dive_tax) %>% summarise(mean_delta_dive_revenue_diff = mean(difference))
panel2_diff <- diff_data %>% ggplot(aes(x=dive_tax,y=difference/1e9)) + geom_line(aes(group=run),size=1, alpha=0.01)+
  geom_line(data=mean_delta_dive_revenue_diff, aes(x=dive_tax, y=mean_delta_dive_revenue_diff/1e9))+theme_classic()+labs(x ="User fee per dive\n(US$)", y = "Difference\n(billion US$)")+ theme(legend.position = "none")+
  theme(panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),legend.background = element_rect(fill='transparent'),legend.box.background = element_rect(fill='transparent')) +
  ylim(0,max(diff_data$difference/1e9))
panel2_diff

panel2_inset <- panel2+annotation_custom(grob = ggplotGrob(panel2_diff), xmin=75, xmax=200, ymin=min_ref, ymax=max_val) 

#change consumer surplus plot
mean_delta_consumer_surplus <- explore_user_fee_merged %>% select(dive_tax, delta_consumer_surplus, Scenario) %>% group_by(Scenario,dive_tax) %>% summarise(mean_delta_consumer_surplus = mean(delta_consumer_surplus))
panel3 <- explore_user_fee_merged %>% ggplot(aes(x=dive_tax,y=delta_consumer_surplus/1e9,color=Scenario, group=interaction(Scenario,run)))+geom_line(size=1, alpha=0.01)+
  geom_line(data=mean_delta_consumer_surplus, aes(x=dive_tax, y=mean_delta_consumer_surplus/1e9, group=Scenario, linetype=Scenario))+theme_classic()+labs(x ="User fee per dive\n(US$)", y = "\u0394 consumer surplus\n(billion US$)")+ theme(legend.position = "none")   
panel3

max_val <- max(explore_user_fee_merged$delta_consumer_surplus/1e9)
min_val <- min(explore_user_fee_merged$delta_consumer_surplus/1e9)
min_ref <- min_val+0.6*(max_val-min_val)

#difference plot panel 3
data1<-explore_user_fee_merged %>% select(dive_tax, delta_consumer_surplus, Scenario, run) %>% filter(Scenario =="With MPA")
data2<-explore_user_fee_merged %>% select(dive_tax, delta_consumer_surplus, Scenario, run) %>% filter(Scenario =="No MPA")
diff_data <- left_join(data1,data2, by=c("dive_tax","run")) %>% mutate(difference = delta_consumer_surplus.x-delta_consumer_surplus.y)

mean_delta_consumer_surplus_diff <- diff_data %>% group_by(dive_tax) %>% summarise(mean_delta_consumer_surplus_diff = mean(difference))
panel3_diff <- diff_data %>% ggplot(aes(x=dive_tax,y=difference/1e9)) + geom_line(aes(group=run),size=1, alpha=0.01)+
  geom_line(data=mean_delta_consumer_surplus_diff, aes(x=dive_tax, y=mean_delta_consumer_surplus_diff/1e9))+theme_classic()+labs(x ="User fee per dive\n(US$)", y = "Difference\n(billion US$)")+ theme(legend.position = "none")+
  theme(panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),legend.background = element_rect(fill='transparent'),legend.box.background = element_rect(fill='transparent')) +
  ylim(0,max(diff_data$difference/1e9))
panel3_diff

panel3_inset <- panel3+annotation_custom(grob = ggplotGrob(panel3_diff), xmin=75, xmax=200, ymin=min_ref, ymax=max_val) 

#change number of dive plot
mean_delta_number_dives <- explore_user_fee_merged %>% select(dive_tax, delta_number_dives, Scenario) %>% group_by(Scenario,dive_tax) %>% summarise(mean_delta_number_dives = mean(delta_number_dives))
panel4 <- explore_user_fee_merged %>% ggplot(aes(x=dive_tax,y=delta_number_dives/1e6,color=Scenario, group=interaction(Scenario,run)))+geom_line(size=1, alpha=0.01)+
  geom_line(data=mean_delta_number_dives, aes(x=dive_tax, y=mean_delta_number_dives/1e6, group=Scenario, linetype=Scenario))+theme_classic()+labs(x ="User fee per dive\n(US$)", y = "\u0394 # dives\n(million)")+ theme(legend.position = "none")   
panel4

max_val <- max(explore_user_fee_merged$delta_number_dives/1e6)
min_val <- min(explore_user_fee_merged$delta_number_dives/1e6)
min_ref <- min_val+0.6*(max_val-min_val)

#difference plot panel 4
data1<-explore_user_fee_merged %>% select(dive_tax, delta_number_dives, Scenario, run) %>% filter(Scenario =="With MPA")
data2<-explore_user_fee_merged %>% select(dive_tax, delta_number_dives, Scenario, run) %>% filter(Scenario =="No MPA")
diff_data <- left_join(data1,data2, by=c("dive_tax","run")) %>% mutate(difference = delta_number_dives.x-delta_number_dives.y)

mean_delta_number_dives_diff <- diff_data %>% group_by(dive_tax) %>% summarise(mean_delta_number_dives_diff = mean(difference))
panel4_diff <- diff_data %>% ggplot(aes(x=dive_tax,y=difference/1e6)) + geom_line(aes(group=run),size=1, alpha=0.01)+
  geom_line(data=mean_delta_number_dives_diff, aes(x=dive_tax, y=mean_delta_number_dives_diff/1e6))+theme_classic()+labs(x ="User fee per dive\n(US$)", y = "Difference\n(million)")+ theme(legend.position = "none")+
  theme(panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),legend.background = element_rect(fill='transparent'),legend.box.background = element_rect(fill='transparent'))+
  ylim(0,max(diff_data$difference/1e6))
panel4_diff

panel4_inset <- panel4+annotation_custom(grob = ggplotGrob(panel4_diff), xmin=75, xmax=200, ymin=min_ref, ymax=max_val) 

# extract legend
legend_b <- get_legend(panel1 + guides(color = guide_legend(nrow = 1)) + theme(legend.position = "bottom"))

##-- Plot FIGURE 4 with inset and save
plot_explore_user_fee_v1<- cowplot::plot_grid(panel1_inset,panel2_inset,panel3_inset,panel4_inset, ncol = 2, labels = "AUTO",rel_heights=c(1,1))
Fig4 <- cowplot::plot_grid(plot_explore_user_fee_v1, legend_b, ncol = 1, rel_heights=c(1,.05))
Fig4
ggsave(here("figures","main","Fig4.jpg"),Fig4, width = 8, height = 8, units = "in")

# #--FIGURE 5
# data1 <- mean_taxrevenue_db %>% filter(Scenario == "With MPA")
# data2 <- mean_delta_dive_revenue %>% filter(Scenario == "With MPA")
# data3 <- left_join(data1,data2,by=c("Scenario","dive_tax")) %>% mutate(total_revenue=mean_taxrev+mean_delta_dive_revenue)
# 
# data4 <- mean_taxrevenue_db %>% filter(Scenario == "No MPA")
# data5 <- mean_delta_dive_revenue %>% filter(Scenario == "No MPA")
# data6 <- left_join(data4,data5,by=c("Scenario","dive_tax")) %>% mutate(total_revenue=mean_taxrev+mean_delta_dive_revenue)
# 
# ylim_min <- min(data3$mean_taxrev,data6$mean_taxrev,data3$mean_delta_dive_revenue,data6$mean_delta_dive_revenue)/1e9
# ylim_max <- max(data3$mean_taxrev,data6$mean_taxrev,data3$mean_delta_dive_revenue,data6$mean_delta_dive_revenue)/1e9
# 
# library(geomtextpath)  
# Fig5a <- data3 %>% ggplot() +   geom_textline(aes(x=dive_tax,y=mean_taxrev/1e9),label = "Dive fee revenue", size = 3, vjust = -0.5,
#                                              linewidth = 1, linecolor = "blue", linetype = "dotted", color = "black") +
#   geom_textline(aes(x=dive_tax,y=mean_delta_dive_revenue/1e9),label = "\u0394 dive industry revenue", size = 3, vjust = -0.5,
#                 linewidth = 1, linecolor = "red", linetype = "dashed", color = "black") +
#   geom_textline(aes(x=dive_tax,y=total_revenue/1e9),label = "Total revenue", size = 3, vjust = -0.5,
#                 linewidth = 1, linecolor = "black", color = "black")+ theme_minimal()+labs(x ="User fee per dive\n(US$)", y = "Revenue\n(US$ billion)")+ggtitle("With MPA")+ylim(ylim_min,ylim_max)
# 
# Fig5b <- data6 %>% ggplot() +   geom_textline(aes(x=dive_tax,y=mean_taxrev/1e9),label = "Dive fee revenue", size = 3, vjust = -0.5,
#                                                    linewidth = 1, linecolor = "blue", linetype = "dotted", color = "black") +
#   geom_textline(aes(x=dive_tax,y=mean_delta_dive_revenue/1e9),label = "\u0394 dive industry revenue", size = 3, vjust = -0.5,
#                 linewidth = 1, linecolor = "red", linetype = "dashed", color = "black") +
#   geom_textline(aes(x=dive_tax,y=total_revenue/1e9),label = "Total revenue", size = 3, vjust = -0.5, linewidth = 1, linecolor = "black", 
#                 color = "black")+ theme_minimal()+labs(x ="User fee per dive\n(US$)", y = "Revenue\n(US$ billion)")+ggtitle("No MPA")+ylim(ylim_min,ylim_max)
# Fig5 <- cowplot::plot_grid(Fig5a,Fig5b, ncol = 2, labels = "AUTO")
# Fig5
# 
# ggsave(here("figures","main","Fig5.jpg"),Fig5, width = 8, height = 4, units = "in")


#plot_explore_user_fee_v1 <- cowplot::plot_grid(panel1,panel1_diff,panel2,panel2_diff,panel3,panel3_diff,panel4,panel1_diff, ncol = 2, labels = "AUTO",rel_heights=c(1,1))
#plot_explore_user_fee_v2 <- cowplot::plot_grid(plot_explore_user_fee_v1, legend_b, ncol = 1, rel_heights=c(1,.05))
#plot_explore_user_fee_v2
#ggsave(here("figures","main","plot_explore_user_fee.jpg"),plot_explore_user_fee_v2, width = 6, height = 10, units = "in")


##----FIGURE 3, consumer surplus and tax beneficiaries plot
#with MPA + average user fee (53 USD per dive)

component_effect_name <- round(effect_name/average_user_fee,4)
component_biomass_effect <- round(mean(effect_biomass)/average_user_fee,4)
component_biodiversity_effect <- round(mean(effect_biodiversity)/average_user_fee,4)

p1_contribution <- data.frame(value = c(component_effect_name, component_biomass_effect, component_biodiversity_effect), Component = c("MPA name","Biomass","Biodiversity")) %>%
  ggplot(aes(x = "", y = value, fill = Component)) + geom_col(color = "black") +
  geom_text(aes(label = percent(value)), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +  ggtitle("Drivers of MPA benefits")+
  theme_void() + theme(plot.title = element_text(hjust = 0.5))
p1_contribution #contribution of different components (name, biodiv, biomass) to WTP

#Proportion of foreign and local divers per region
#Load this csv file: local_vs_foreign_tourist_origins_by_region
diver_origin <- read.csv(here("data","dive","local_vs_foreign_tourist_origins_by_region.csv")) %>% dplyr::rename(Origin=origin)

#--consumer surplus beneficiaries by region and diver origin
#remove the pixels that are in MPA
ndives_data_v1 <- dives_input[dives_input$cell_id %in% divepixels_unprotected,]
ndives_data_v2 <- left_join(ndives_data_v1,cell_id_with_country_kat_withregion,by="cell_id") %>% select(cell_id,n_dives_extrap,n_dives_extrap_min,n_dives_extrap_max,region_fill)

#consumer_surplus_per_region <- ndives_data_v2 %>% mutate(consumer_surplus = 0.5*n_dives_extrap*(choke_price-price_per_dive_constant[1])) %>% group_by(region_fill) %>% summarise(consumer_surplus_per_region = sum(consumer_surplus)/1e6) %>% drop_na() %>% dplyr::rename(region=region_fill)
#this just tracks the beneficiaries of the change in consumer surplus due to MPA
consumer_surplus_per_region <- ndives_data_v2 %>% mutate(consumer_surplus = change_consumer_suplus) %>% group_by(region_fill) %>% summarise(consumer_surplus_per_region = sum(consumer_surplus)/1e6) %>% drop_na() %>% dplyr::rename(region=region_fill)

consumer_surplus_beneficiary <- left_join(diver_origin,consumer_surplus_per_region, by="region") %>% mutate(consumer_surplus = consumer_surplus_per_region*percent_avg/100)

consumer_surplus_beneficiary$percent_avg<-round(consumer_surplus_beneficiary$percent_avg)

df_sorted <- arrange(consumer_surplus_beneficiary, region, Origin) 
head(df_sorted)

df_cumsum <- df_sorted %>% group_by(region) %>% mutate(label_ypos=cumsum(consumer_surplus))

sum(df_cumsum$consumer_surplus)

p2_dissagregated <- df_cumsum %>% ggplot(aes(x=region, y=consumer_surplus, fill=Origin, label=scales::percent(percent_avg/100,accuracy = 1L))) +
  geom_bar(stat="identity") + geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Paired")+
  labs(x="",y="Consumer surplus\n(US$ million)")+  
  theme_minimal()+
  ggtitle("Consumer surplus beneficiaries\nby region and diver origin")+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
p2_dissagregated 

#-- consumer surplus beneficiaries by foreign and local
p2_development <- consumer_surplus_beneficiary  %>% group_by(Origin) %>% summarise(total_consumer_surplus=sum(consumer_surplus)) %>%
  mutate(countT= sum(total_consumer_surplus), Percent_Contribution = total_consumer_surplus/countT) %>%
  ggplot(aes(x = "", y = Percent_Contribution, fill = Origin)) + geom_col(color = "black") +
  geom_text(aes(label = percent(Percent_Contribution)), position = position_stack(vjust = 0.5)) +
  ggtitle("Consumer surplus beneficiaries") + coord_polar(theta = "y") +
  scale_fill_brewer(palette="Blues") + theme_void() + theme(plot.title = element_text(hjust = 0.5))
p2_development

# # #consumer surplus for developing countries only
# head(ndives_data_v2)
# dim(ndives_data_v2)
# head(country_classification)
# left_join(country_classification,by="territory1") 
# step1<-ndives_data_v2 %>% mutate(consumer_surplus = 0.5*n_dives_extrap*(choke_price-price_per_dive_constant[1]))
# left_join(step1,country_classification,by="territory1") %>% dim() 
# #%>% group_by(region_fill) %>% summarise(consumer_surplus_per_region = sum(consumer_surplus)/1e6) %>% drop_na() %>% dplyr::rename(region=region_fill)
# # dive_per_country[dive_per_country$cell_id %in% divepixels_unprotected,] %>% left_join(country_classification,by="territory1") %>% select(cell_id,Classification,region_fill,n_dives_extrap) %>% head()

#-- Apply tax and track beneficiaries.
dive_tax <- mean(user_fee_opt)

#shifted number of dives with tax
shifted_number_dive <- round(parameter_a - parameter_b*price_per_dive_constant + ((effect_name+effect_biomass+effect_biodiversity-dive_tax)*base_number_dive/(choke_price-price_per_dive_constant)))
min(shifted_number_dive)
plot((shifted_number_dive - base_number_dive))
tax_revenue <- dive_tax*shifted_number_dive
sum(tax_revenue)/1e9 #this is how much you can get from pixel-specific tax when establishing an MPA and imposing tax that does not affect dive numbers

#-- the code below is not needed
# dive_tax <- mean(user_fee_opt)
# price_per_dive <- cons_price_per_dive

# #shifted number of dives with crowding and with tax
# delta_q_crowding <- (wtp_combined - dive_tax)*base_number_dive/(choke_price-price_per_dive)
# shifted_number_dive_withcrowding <- base_number_dive + delta_q_crowding*(1-(reduce_wtp_crowd*dive_group_size*delta_q_crowding/base_number_dive))
# shifted_number_dive_withcrowding[shifted_number_dive_withcrowding<0]<-0
# sum(shifted_number_dive_withcrowding)
# 
# #with crowding effect and with MPA
# change_dive_revenue_withcrowding <- price_per_dive*(shifted_number_dive_withcrowding - base_number_dive) # in USD
# new_choke_price_withcrowding <- (parameter_a+((wtp_combined-dive_tax)*base_number_dive/(choke_price-price_per_dive)))/parameter_b
# change_consumer_suplus_withcrowding <- (0.5*shifted_number_dive_withcrowding*(new_choke_price_withcrowding-price_per_dive))-(0.5*base_number_dive*(choke_price-price_per_dive))
# tax_revenue_withcrowding <- dive_tax*shifted_number_dive_withcrowding
# explore_user_fee_withcrowding<-c(dive_tax,sum(shifted_number_dive_withcrowding)-sum(base_number_dive),sum(change_dive_revenue_withcrowding),sum(change_consumer_suplus_withcrowding),sum(tax_revenue_withcrowding))%>% 
#   setNames(., c("dive tax", "N dives", "Change dive revenue","Change consumer surplus","Tax revenue"))
# length(tax_revenue_withcrowding)# length is 1792
#---

head(dive_per_country)
cell_developmentstatus <- dive_per_country[dive_per_country$cell_id %in% divepixels_unprotected,] %>% left_join(country_classification,by="territory1") %>% select(cell_id,Classification,region_fill,n_dives_extrap)
dim(cell_developmentstatus)

#--add consumer surplus, no intervention
#cell_developmentstatus <-cell_developmentstatus %>% mutate(consumer_surplus = 0.5*n_dives_extrap*(choke_price-price_per_dive_constant[1]))

#add tax revenue (with MPA and tax=WTP), change consumer surplus from MPA, and  new dives from MPA (no tax)
cell_developmentstatus <-cell_developmentstatus %>% mutate(tax_revenue = tax_revenue, consumer_surplus = change_consumer_suplus, new_dive = new_dive_MPAnotax)
head(cell_developmentstatus)
dim(cell_developmentstatus)

data_1 <- cell_developmentstatus %>% filter(Classification=="Developing") %>%dplyr::rename(region=region_fill) 
head(data_1)
dim(data_1)
checkme<-left_join(data_1,diver_origin,by="region") %>% mutate(consumer_surplus_development=consumer_surplus*percent_avg/100)
head(checkme)

###MANUSCRIPT STAT: % of consumer surplus in developing countries that is captured by foreign divers
checkme %>% group_by(Origin) %>% summarise(total_consumer_surplus=sum(consumer_surplus_development)) %>% drop_na() %>%
  mutate(Total = sum(total_consumer_surplus), Fraction = total_consumer_surplus/Total) #67%

#PLOT THIS
cons_surp_beneficiaries <- checkme %>% group_by(Origin) %>% summarise(total_consumer_surplus=sum(consumer_surplus_development)) %>% drop_na() %>%
  mutate(Total = sum(total_consumer_surplus), Fraction = total_consumer_surplus/Total) %>% ggplot(aes(x = "", y = Fraction, fill = Origin)) +
  geom_col(color = "black") +
  geom_text(aes(label = percent(Fraction)),position = position_stack(vjust = 0.5)) +
  ggtitle("Consumer surplus beneficiaries\n of MPAs in developing countries")+
  coord_polar(theta = "y") +
  scale_fill_brewer(palette="Greens") +
  theme_void()+   theme(plot.title = element_text(hjust = 0.5))
cons_surp_beneficiaries

##MANUSCRIPT STAT: Importantly, x% of new marine diving will be demanded developing countries
cell_developmentstatus %>% group_by(Classification) %>% summarise(ndive=sum(new_dive)) %>% filter(Classification %in% c("Developed","Developing")) %>%
  mutate(Total = sum(ndive), Fraction = ndive/Total) #61%

#PLOT THIS
dive_demand_beneficiaries <- cell_developmentstatus %>% group_by(Classification) %>% summarise(ndive=sum(new_dive)) %>% filter(Classification %in% c("Developed","Developing")) %>%
  mutate(Total = sum(ndive), Fraction = ndive/Total) %>% ggplot(aes(x = "", y = Fraction, fill = Classification)) +
  geom_col(color = "black") +
  geom_text(aes(label = percent(Fraction)),position = position_stack(vjust = 0.5)) +
  ggtitle("Locations of new diving\n demand from MPAs")+
  coord_polar(theta = "y") +
  scale_fill_brewer(palette="Greens") +
  theme_void()+   theme(plot.title = element_text(hjust = 0.5))
dive_demand_beneficiaries 

Fig_supp_newdemand <- cowplot::plot_grid(dive_demand_beneficiaries,cons_surp_beneficiaries, ncol = 1, labels = "AUTO")
ggsave(here("figures","supplementary","Fig_supp_newdemand.jpg"),Fig_supp_newdemand, width = 5, height = 8, units = "in")

#tax revenue distribution
p3_development <- cell_developmentstatus %>% group_by(Classification) %>% summarise(Contribution=round(sum(tax_revenue)/10^6))%>%
  filter(Classification %in% c("Developed", "Developing")) %>% mutate(Percent_Contribution = Contribution/sum(Contribution)) %>%
  ggplot(aes(x = "", y = Percent_Contribution, fill = Classification)) + geom_col(color = "black") +
  geom_text(aes(label = percent(Percent_Contribution)), position = position_stack(vjust = 0.5)) +
  #ylab("Contribution (US$ million)")
  ggtitle("User fee revenue beneficiaries")+
  coord_polar(theta = "y") +
  scale_fill_brewer(palette="Reds") +
  theme_void()+   theme(plot.title = element_text(hjust = 0.5))
p3_development

##USER FEE REVENUE BY REGION BY DEV CATEGORY
generate_data<-cell_developmentstatus %>% group_by(region_fill, Classification) %>% summarise(total_tax_revenue=sum(tax_revenue)/10^6) %>% drop_na() %>% filter(Classification!="In transition")

# Create the barplot
p3_dissagregated <- ggplot(data=generate_data, aes(x=region_fill, y=total_tax_revenue, fill=Classification)) +
  geom_bar(stat="identity")+
  scale_fill_brewer(palette="Reds")+
  ggtitle("User fee revenue beneficiaries\nby region and country classification")+
  labs(x="",y="Potential user fee revenue\n(US$ million)")+  
  theme_minimal()+theme(axis.text.x = element_text(angle=90, vjust=0.5,hjust=1))
p3_dissagregated 

Fig3a <- cowplot::plot_grid(p1_contribution,p2_development,p3_development, ncol = 1, labels = "AUTO")
Fig3b <- cowplot::plot_grid(p2_dissagregated,p3_dissagregated , ncol = 1, labels = c('D', 'E'))
Fig3 <- cowplot::plot_grid(Fig3a,Fig3b,ncol=2)
ggsave(here("figures","main","Fig3.jpg"),Fig3, width = 8, height = 8, units = "in")

# #This is the summary table of results! with/without MPA and varying tax level.
# head(explore_user_fee_merged)
# 
# #Summary table: with MPA, no tax. Just printing the change in biological parameters.
# #this table will output the following:
# #1. Pixel number of unprotected dive sites
# #2. Change in biomass per MPA pixels
# #3. Change in biodiversity metric per MPA pixel
# summary_table1 <- data.frame(pixel_number = divepixels_unprotected)
# summary_table1$change_biomass <- change_biomass_density_full
# summary_table1$change_diversity <- change_diversity_full
# #summary_table1$change_ndive <- 
# head(summary_table1)

#---SAVE DATA FOR PLOTTING
save(biomass_data, land_shp_moll, explore_user_fee_merged, effect_name, effect_biomass, effect_biodiversity, average_user_fee, 
     dives_input, divepixels_unprotected, cell_id_with_country_kat_withregion, change_consumer_suplus, cell_developmentstatus, file = here("scripts","figures","Figure_data.RData"))