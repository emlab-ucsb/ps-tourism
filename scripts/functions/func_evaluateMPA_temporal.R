#evaluate biomass inside MPA
#this is a full model where all dive sites will be placed inside MPA then run the simulation for time t=100?
#parameters needed: stock_num, transformed_stockdistrib, MegaData_filtered_step3, MPA_loc

#Clean version (18 Nov 2021)
#This code evaluates the build-up of biomass inside MPAs, evaluate biodiversity change, and compute change in tourism benefits from user fees


gc()
rm(list = ls())

library(doParallel)
library(raster)
library(rgdal)
library(maptools)
library(dplyr)
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

#-- Path to the Pristine Seas tourism directory on the emLab Google Drive
this_project_dir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/ps-tourism"

#-- load stock distribution
transformed_stockdistrib <- readRDS(here("data","transformed_stockdistrib.rds"))
head(transformed_stockdistrib)
dim(transformed_stockdistrib) #149547 by 1155
#check if all cell_id's are ocean id
transformed_stockdistrib %>% filter(ocean==0) #ok, they are all ocean.

ocean_coordinates <- transformed_stockdistrib %>% select(cell_id,lon,lat,f_highly_mpa)
ocean_coordinates %>% ggplot(aes(x=lon,y=lat,fill=1)) + geom_raster()

##load EEZ file
eez_mollweide <- readRDS(file = here("data","eez_mollweide.rds"))
head(eez_mollweide)

#cell id with country names
cell_id_with_country <- left_join(ocean_coordinates,eez_mollweide,by=c("lon","lat"))

#--- load MegaData --- add biomass density
MegaData<-readRDS(here("data","MegaData_Ray.rds"))
MegaData_filtered <- MegaData %>% filter(INCLUDE==1) %>% mutate(bvk_fin = 1-(ExploitationRate_BAU1_Ray/r_fin)) %>% dplyr::select(stockid,SciName,r_fin,Kfin,bvk_fin)
MegaData_filtered$ID <- seq.int(nrow(MegaData_filtered)) #add ID number 
head(MegaData_filtered)
tail(MegaData_filtered)
dim(MegaData_filtered)

MegaData_filtered$check_stock_id<-colnames(transformed_stockdistrib)[6:1155] #ok. This is just a check that the files are matched.

#--- load the distance matrix (source, sink, distance)
merged_dist_matrix<-readRDS(here("data","distance-library","merged_dist_matrix.rds"))
#convert distance from m to km
merged_dist_matrix$distance <- merged_dist_matrix$distance/1000
head(merged_dist_matrix)

##comment this for now
# #add MPA info: i.e., MPA_sink and MPA_source... whether sink or sources are MPAs.
# MPA_source <- transformed_stockdistrib %>% dplyr::select(cell_id,f_highly_mpa) %>% mutate(f_highly_mpa = 1*(f_highly_mpa>=0.5)) %>% dplyr::rename(source=cell_id, MPA_source=f_highly_mpa)
# MPA_sink <- MPA_source %>% dplyr::rename(sink=source, MPA_sink=MPA_source)
# merged_dist_matrix_source <- left_join(merged_dist_matrix,MPA_source,by="source")
# merged_dist_matrix_source_sink <- left_join(merged_dist_matrix_source,MPA_sink,by="sink")
# 
# dim(merged_dist_matrix_source_sink)

# #merging data option 1
# MPA<-c(1:100000)
# ptm <- proc.time()
# merged_dist_matrix_source_sink %>% mutate(MPA_sink=replace(MPA_sink,sink %in% MPA,1)) %>% head()
# (proc.time() - ptm)/60 #check process time in minutes
# 
# #merging data option 2
# dataA <- merged_dist_matrix %>% as.data.table()
# dataB <- MPA_sink %>% as.data.table()
# ptm <- proc.time()
# dataA[dataB,on="sink"]
# (proc.time() - ptm)/60 #check process time in minutes

##--Run only once. Subsetting the connectivity matrix. Save inside a folder then call inside the function
#--subset the stock and get the row numbers where entry == 1, then subset the connectivit matrix

#--load homerange and pld data predictions then process
pld_data <- read.csv(here("data","homerange_pld_predictions","pld_rf_prediction_updated.csv")) %>% dplyr::select(species,observed_PLD,predicted_PLD) %>% dplyr::rename(SciName = species)
#there are duplicates so we need to summarize
pld_data_mean <- pld_data %>% group_by(SciName) %>% summarise(mean_observed_PLD = mean(observed_PLD), sd_observed_PLD = sd(observed_PLD), mean_predicted_PLD = mean(predicted_PLD))
head(pld_data_mean)
dim(pld_data_mean)

homerange_data <- read.csv(here("data","homerange_pld_predictions","homerange_rf_predictions.csv")) %>% dplyr::select(species,observed_homerange,predicted_homerange) %>% dplyr::rename(SciName = species)
homerange_data_mean <- homerange_data %>% group_by(SciName) %>% summarise(mean_observed_homerange = mean(observed_homerange), sd_observed_homerange = sd(observed_homerange), mean_predicted_homerange = mean(predicted_homerange))
head(homerange_data_mean)

species_list <- MegaData_filtered %>% select(SciName) %>% unique()
dim(species_list)

db_with_pld <- left_join(species_list,pld_data_mean,by="SciName")
db_with_pld_hrange <- left_join(db_with_pld,homerange_data_mean)
head(db_with_pld_hrange)
dim(db_with_pld_hrange)

db_with_pld_hrange_filtered <- db_with_pld_hrange %>% mutate(PLD = ifelse(!is.na(mean_observed_PLD),mean_observed_PLD,mean_predicted_PLD), homerange = ifelse(!is.na(mean_observed_homerange),mean_observed_homerange,mean_predicted_homerange),
                                                             complete = ifelse((PLD>0 & homerange>0),1,0))
sum(db_with_pld_hrange_filtered$complete,na.rm=T) #606 species out of 811. Check what proportion of K this is
#merge with the full data list
MegaData_PLD_hrange <- left_join(MegaData_filtered,db_with_pld_hrange_filtered,by="SciName")
#ok, good, now check their relationships...

#% of the original biomass considered. 
MegaData_PLD_hrange %>% filter(complete==1) %>% summarize(sum(Kfin))/sum(MegaData_PLD_hrange$Kfin) #we have data for 80% of the biomass.

#ok, we have home range and PLD data. We have entries with no data. We can remove these data as we rely on relative biomass metric rather than absolute.

###--- Add family info 

# #load the PLD data. Load home range.
# pld_data <- read.csv(here("data","mobility_pld_imputed.csv")) %>% dplyr::select(SciName,pld)
# 
# #--!!! This is the checking of the home range data.
# homerange_data <- read.csv(here("data","homerange","homerange_rf_predictions.csv")) %>% distinct(species, .keep_all = TRUE) %>% dplyr::select(species,pred) %>% dplyr::rename(SciName = species, homerange = pred)
# # max(homerange_data$pred)
# # dim(homerange_data)
# # head(homerange_data)
# #check homerange data# ok, there are 651 predicted values only.
# test<-left_join(pld_data,homerange_data,by="SciName")
# sum(!is.na(test$homerange)*1)
# #--!!!

#max dispersal distance limit
3*1.33*(max(MegaData_PLD_hrange$PLD,na.rm=T)^1.3)*sqrt(pi/2) #18K... ok, we used 1K for now. Revisit.

#merge
#MegaData_filtered_step2 <- left_join(MegaData_filtered,pld_data, by="SciName") %>% mutate(sigma_larvae = 1.33*(pld^1.3)*sqrt(pi/2), dispersal_distance_limit = 3*sigma_larvae)
MegaData_filtered_step2 <- MegaData_PLD_hrange %>% mutate(sigma_larvae = 1.33*(PLD^1.3)*sqrt(pi/2), dispersal_distance_limit = 3*sigma_larvae)
head(MegaData_filtered_step2)

#!!!!this is the merging of the home range data. for now, comment this until my questions are answered
#MegaData_filtered_step3 <- left_join(MegaData_filtered_step2,homerange_data, by="SciName")
#placeholder
MegaData_filtered_step3 <- MegaData_filtered_step2 %>% mutate(homerange=sigma_larvae)

run_subset_connectivitymatrix <- 0 #1 for on, 0 to switch this off
if(run_subset_connectivitymatrix == 1){
  registerDoParallel(detectCores()/2)
  #CHECK IF THIS IS CORRECT: which(MegaData$INCLUDE==1)
  foreach(stock_num=1:1150) %dopar% {
    #for (stock_num in which(MegaData$INCLUDE==1)){
    stock_subset_i<-which(transformed_stockdistrib[,stock_num+5] > 0)
    #included here: filter distance
    distance_mat_full_prop <- merged_dist_matrix %>% filter(source %in% stock_subset_i, sink %in% stock_subset_i, distance<=MegaData_filtered_step2$dispersal_distance_limit[1]) %>% group_by(source) %>% mutate(biom_prop = exp(-( distance^2 / (2*(sigma_larvae^2))) ), biom_prop = biom_prop/sum(biom_prop)) %>% dplyr::select(-distance) %>% as.data.table()
    # distance_mat_full_prop <- distance_mat_full %>% filter(pos1 %in% stock_subset_i, pos2 %in% stock_subset_i) %>% group_by(pos1) %>% mutate(biom_prop = exp(-( dist^2 / (2*(sigma^2))) ), biom_prop = biom_prop/sum(biom_prop)) %>% dplyr::select(-dist) %>% as.data.table()
    
    #fts is the fastest in saving and loading files.
    fst::write.fst(distance_mat_full_prop , paste0("/Users/ren/Documents/GitHub/tourism-mpa-support/connect_matrix/",stock_num,"_connect_larvae.fst"))
  }
  doParallel::stopImplicitCluster()
}

#test<-fst::read.fst(paste0("/Users/ren/Documents/GitHub/tourism-mpa-support/connect_matrix/1_connect_larvae.fst"))
#head(test)

#-- adult movement matrix

#---- this is the main code that evaluates the biomass change for each stock and the change in biodiversity

# Load the dive suitability layer
dive_suitability <- read.csv(here("data","dive","dive_suitability_by_cell.csv"))
head(dive_suitability)
dim(dive_suitability)#1997 dive sites!!!
#Fraction and absolute size of global ocean suitable for diving
dim(dive_suitability)[1]*100/dim(transformed_stockdistrib)[1] #this is the fraction of ocean with diving. 1.34%
dim(dive_suitability)[1]*50*50# 5 million km2. This is the total area of ocean surface with diving.

#check other files
#number of dives
number_of_dives <- read.csv(here("data","dive","number_of_dives_extrapolated_by_cell.csv"))
head(number_of_dives)
dim(number_of_dives)#1997
sum(number_of_dives$n_dives_extrap)#50700017 dives.

dive_per_country <- left_join(number_of_dives,cell_id_with_country,by="cell_id")
iso_library<-dive_per_country %>% select(sovereign1,territory1,iso_sov1,iso_ter1) %>% unique() %>% arrange(sovereign1) %>% filter(!is.na(sovereign1))

#load country classification (SIDS, developing, etc.)
country_classification <- read.csv(here("data","UN_territory_sovereign_classification.csv"))
#country_classification$SIDS <-as.factor(country_classification$Classification)

country_classification_with_iso <- left_join(country_classification,iso_library,by=c("sovereign1","territory1"))
country_classification_kat <- read.csv(here("data","country_status_lookup_manual_category.csv")) %>% mutate(Classification_kat = ifelse(manual_development_status=="Developed", "Developed", "Developing")) %>%
  select(iso3,Classification_kat) %>% dplyr::rename(iso_ter1 = iso3)

#note from Kat: use "development_status" -- developed and others.
country_classification_with_iso_and_class <- left_join(country_classification_with_iso,country_classification_kat,by="iso_ter1") %>% mutate(match = (Classification==Classification_kat))

##--price per dive
prices_constant_by_cell <- read.csv(here("data","dive","prices_constant_by_cell.csv"))
dim(prices_constant_by_cell)
plot(prices_constant_by_cell$price) #60 USD per dive

#price per country
prices_country_region_by_cell <- read.csv(here("data","dive","prices_country_region_by_cell.csv"))
plot(prices_country_region_by_cell$price)
#request to add country name or country code in the file. Or a file of cell_id and country names.

#ok, use constant price. 
ocean_coordinates_dive_suitable <- left_join(ocean_coordinates,dive_suitability,by="cell_id")
ocean_coordinates_dive_suitable_v2 <- left_join(ocean_coordinates_dive_suitable,number_of_dives,by="cell_id")

#plot suitability later
ocean_coordinates_dive_suitable_v2 %>% mutate(suitable = replace_na(suitable,0)) %>% ggplot() + geom_raster(aes(x=lon,y=lat,fill=suitable)) + scale_fill_gradientn(colours=c("black","orange")) #ok, great

#Question: how many stocks have no intersection with diving? We can remove those.
head(transformed_stockdistrib)
filter1<-transformed_stockdistrib %>% filter(cell_id %in% dive_suitability$cell_id)
dim(filter1)#there are 2197 dive sites 
check<-filter1[,6:1155]
dim(check)
mysum<-colSums(check,na.rm=T) %>% as.data.frame()
head(mysum)
names(mysum)[1] <- "n_pixel_intersect"
mysum %>% filter(n_pixel_intersect==0) %>% dim() ##remove 94 stocks with no intersection with diving!
stocklist<-which(mysum!=0)
##ACTION NEEDED--- REMOVE THOSE STOCKS###

# MPA location -- assume that a pixel is an MPA is f_highly_mpa>=0.5. Consult this assumption
MPA_vec <- transformed_stockdistrib %>% dplyr::select(cell_id,f_highly_mpa) %>% mutate(f_highly_mpa = (f_highly_mpa>=0.5))
MPA_loc <- MPA_vec %>% filter(f_highly_mpa=="TRUE") %>% select(cell_id)

#Question: How many of the suitable dive sites are already in MPAs?
divesite_in_MPA <- dive_suitability %>% filter(cell_id %in% MPA_loc$cell_id) %>% dim()
divesite_in_MPA#23 pixels. 
divesite_in_MPA[1]*100/dim(number_of_dives)[1] #1.15% of the dive sites are inside MPA... hmmm... this sounds correct? maybe we should analyze it using high res data.

##--biomass prep file
nstock<-dim(MegaData_filtered)[1]

stock_include<-stocklist#1:nstock#c(1,3,5)#which(MegaData$INCLUDE==1)
length(stock_include) #1056 stocks

#for each stock, do the fisheries model


