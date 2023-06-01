#Pristine seas tourism-MPA model
#Last checked: 2 Nov 2022
#Ren Cabral
#This code evaluates the build-up of biomass inside MPAs, evaluate biodiversity change, and compute the corresponding dive tourism benefits
gc()
rm(list = ls())

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
library(viridis)

#-- Path to the Pristine Seas tourism directory on the emLab Google Drive
this_project_dir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/ps-tourism"

#-- load stock distribution
transformed_stockdistrib <- readRDS(here("data","transformed_stockdistrib.rds"))
head(transformed_stockdistrib)
dim(transformed_stockdistrib) #149547 by 1155
#check if all cell_id's are ocean id
transformed_stockdistrib %>% filter(ocean==0) #ok, they are all ocean.
#plot(transformed_stockdistrib$f_highly_mpa) # use 0.5 as threshold for highly protected MPA.

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
#ensure no negative numbers
MegaData_filtered$bvk_fin[MegaData_filtered$bvk_fin < 0] <- 0# Set very small negative values to 0

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

#--load homerange and pld data predictions then process
pld_data <- read.csv(here("data","homerange_pld_predictions","pld_rf_predictions_final.csv")) %>% dplyr::select(species,observed_PLD,predicted_PLD) %>% dplyr::rename(SciName = species)
#summarize in case of duplicates
pld_data_mean <- pld_data %>% group_by(SciName) %>% summarise(mean_observed_PLD = mean(observed_PLD), sd_observed_PLD = sd(observed_PLD), mean_predicted_PLD = mean(predicted_PLD))
head(pld_data_mean)
dim(pld_data_mean)

homerange_data <- read.csv(here("data","homerange_pld_predictions","homerange_rf_predictions_10112022.csv")) %>% dplyr::select(species,observed_homerange,predicted_homerange) %>% dplyr::rename(SciName = species)
homerange_data_mean <- homerange_data %>% group_by(SciName) %>% summarise(mean_observed_homerange = mean(observed_homerange), sd_observed_homerange = sd(observed_homerange), mean_predicted_homerange = mean(predicted_homerange))
head(homerange_data_mean)

species_list <- MegaData_filtered %>% dplyr::select(SciName) %>% unique()
dim(species_list)

db_with_pld <- left_join(species_list,pld_data_mean,by="SciName")
db_with_pld_hrange <- left_join(db_with_pld,homerange_data_mean)
head(db_with_pld_hrange)
dim(db_with_pld_hrange)

db_with_pld_hrange_filtered <- db_with_pld_hrange %>% mutate(PLD = ifelse(!is.na(mean_observed_PLD),mean_observed_PLD,mean_predicted_PLD), homerange = ifelse(!is.na(mean_observed_homerange),mean_observed_homerange,mean_predicted_homerange),
                                                             complete = ifelse((PLD>0 & homerange>0),1,0))
sum(db_with_pld_hrange_filtered$complete,na.rm=T) #610 species out of 811. Check what proportion of K this is
#merge with the full data list
MegaData_PLD_hrange <- left_join(MegaData_filtered,db_with_pld_hrange_filtered,by="SciName")

#% of the original biomass considered. 
MegaData_PLD_hrange %>% filter(complete==1) %>% dplyr::summarize(sum(Kfin))/sum(MegaData_PLD_hrange$Kfin) #we have data for 81% of the K.

#max dispersal distance limit (3 sigma larvae)
3*1.33*(max(MegaData_PLD_hrange$PLD,na.rm=T)^1.3)*sqrt(pi/2) #18K

#merge
MegaData_filtered_step2 <- MegaData_PLD_hrange %>% mutate(sigma_larvae = 1.33*(PLD^1.3)*sqrt(pi/2), dispersal_distance_limit = 3*sigma_larvae, homerange_radius = sqrt(homerange/pi))
head(MegaData_filtered_step2)

#add the geographic range of the stock
full_stock_distrib <- transformed_stockdistrib[6:1155]
geog_range_perstock<- colSums(full_stock_distrib,na.rm=T) %>% as.data.frame()
colnames(geog_range_perstock) <- c('geog_range')
geog_range_perstock$stockid <- row.names(geog_range_perstock) 
head(geog_range_perstock)

MegaData_filtered_step3 <- left_join(MegaData_filtered_step2,geog_range_perstock,by="stockid") 
MegaData_filtered_step_fin <- MegaData_filtered_step3 %>% mutate(Kperpixel = Kfin/geog_range)

head(MegaData_filtered_step_fin)
sum(MegaData_filtered_step_fin$complete, na.rm = T)
min(MegaData_filtered_step_fin$dispersal_distance_limit,na.rm=T)

##--Determine the stocks that will be included in our analysis. The stocks will be those that intersects with any of the dive sites and those with complete parameters.
#We can remove stocks with no intersection with diving

# Load the dive suitability layer
dive_suitability <- read.csv(here("data","dive","dive_suitability_by_cell.csv"))
head(dive_suitability)
dim(dive_suitability)#2043 dive sites!!!
#Fraction and absolute size of global ocean suitable for diving
dim(dive_suitability)[1]*100/dim(transformed_stockdistrib)[1] #this is the fraction of ocean with diving. 1.37%
dim(dive_suitability)[1]*50*50# 5.1 million km2. This is the total area of ocean surface with diving.

#Identify the stock that intersects with diving
head(transformed_stockdistrib)
filter1<-transformed_stockdistrib %>% filter(cell_id %in% dive_suitability$cell_id)
dim(filter1)#there are 2197 dive sites 
check<-filter1[,6:1155]
dim(check)
mysum<-colSums(check,na.rm=T) %>% as.data.frame()
head(mysum)
names(mysum)[1] <- "n_pixel_intersect"
mysum %>% filter(n_pixel_intersect==0) %>% dim() ##remove 94 stocks with no intersection with diving!
stocklist_with_diving <- which(mysum!=0)
length(stocklist_with_diving)
#now, how about stock list with complete parameters?
stocklist_complete_params <- which(MegaData_filtered_step_fin$complete==1)
length(stocklist_complete_params)
#our final stocklist will be the intersection of the two
stocklist <- stocklist_complete_params[stocklist_complete_params %in% stocklist_with_diving]
length(stocklist)#so there are 821 stocks with complete information

#how many species?
length(unique(MegaData_filtered_step_fin[stocklist,]$SciName))#600 species

##Check final list of dataset
Checkme <- MegaData_filtered_step_fin[stocklist,]

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

##--Connectivity matrix, larvae
##--No need to run so we will switch this function off
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

##--Connectivity matrix, adult
##-- no need to re-run so we will turn this function off
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

#---- this is the main code that evaluates the biomass change for each stock and the change in biodiversity
#number of dives
number_of_dives <- read.csv(here("data","dive","number_of_marine_dives_by_cell_extrapolated.csv"))
head(number_of_dives)
dim(number_of_dives)#2043
sum(number_of_dives$n_dives_extrap)#42277478 dives.

#revenue in billion
sum(number_of_dives$n_dives_extrap)*60/1000000000

hist(number_of_dives$n_dives_extrap)
ggplot(data.frame(log(number_of_dives$n_dives_extrap)), aes(log(number_of_dives$n_dives_extrap))) +     # Histogram with logarithmic axis
  geom_histogram(bins=10)

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

#plot number of dives per country
head(dive_per_country)
plot_number_dives <- dive_per_country %>% group_by(territory1) %>% dplyr::summarize(n_dive=sum(n_dives_extrap)) %>% left_join(country_classification,by="territory1") %>%
  arrange(-n_dive) %>% slice(1:50) %>%
  #ggplot(aes(x=as.factor(territory1),y=n_dive)) + geom_col()
  ggplot(aes(x = reorder(as.factor(territory1), n_dive/1000000), y = n_dive/1000000, fill=Classification))+
  geom_bar(stat = "identity")+ theme_classic()+#theme_minimal()+#theme_classic()+
  coord_flip()+ labs(y = "Dive per year, million")+theme(axis.title.y = element_blank())
plot_number_dives

#plot number of dive pixels per country
plot_number_divepexels_country <- dive_per_country %>% group_by(territory1) %>% dplyr::summarize(n_divesites=n()) %>% filter(territory1!="NA") %>% left_join(country_classification,by="territory1") %>%
  arrange(-n_divesites) %>% slice(1:50) %>%
  #ggplot(aes(x=as.factor(territory1),y=n_dive)) + geom_col()
  ggplot(aes(x = reorder(as.factor(territory1), n_divesites), y = n_divesites, fill=Classification))+
  geom_bar(stat = "identity")+ theme_classic()+#theme_minimal()+#theme_classic()+
  coord_flip()+ labs(y = "Number of dive site pixel")+theme(axis.title.y = element_blank())
plot_number_divepexels_country

#-- dive density might be interesting! # of dives per pixel per territory
ndivepixel_territory <- cell_id_with_country  %>% group_by(territory1) %>% dplyr::summarize(total_territory_pixel=n())
#plot % of the eez that are number of dive sites
plot_divedensity <- dive_per_country %>% group_by(territory1) %>% dplyr::summarize(n_dive=sum(n_dives_extrap)) %>% left_join(ndivepixel_territory,by="territory1") %>% mutate(percent_divearea = n_dive/total_territory_pixel) %>% filter(territory1!="NA") %>%
  arrange(-percent_divearea) %>% slice(1:50) %>%
  #ggplot(aes(x=as.factor(territory1),y=n_dive)) + geom_col()
  ggplot(aes(x = reorder(as.factor(territory1), percent_divearea), y = percent_divearea))+
  geom_bar(stat = "identity")+ theme_classic()+#theme_minimal()+#theme_classic()+
  coord_flip()+ labs(y = "# yearly dives/pixel")+theme(axis.title.y = element_blank())
plot_divedensity
ggsave(here("figures","supplementary","plot_divedensity.jpg"),plot_divedensity, width = 10, height = 14, units = "cm")

##--correlate # of dives with on-reef values
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
#flickr_data <- read.csv(here("data","flickr","flickr_aggregate_by_country.csv")) 
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

#save a country file for me to assign country development categorization
head(dive_per_country)
##---[no need to run] dive_per_country %>% select(sovereign1) %>% unique() %>% write.csv(.,file = here("data","country_classification.csv"))
##---[no need to run] dive_per_country %>% select(territory1) %>% unique() %>% write.csv(.,file = here("data","territory_classification.csv"))
##---[no need to run] dive_per_country %>% select(sovereign1,territory1) %>% unique() %>% arrange(sovereign1) %>% filter(!is.na(sovereign1)) %>% write.csv(.,file = here("data","territory_sovereign_classification.csv"))
#checkme <- read.csv(here("data","UN_territory_sovereign_classification.csv"))

##--price per dive
prices_constant_by_cell <- read.csv(here("data","dive","prices_interpolated_matched_to_cell_id.csv"))
prices_constant_by_cell_ocean <- prices_constant_by_cell %>% filter(ocean==1)
mean(prices_constant_by_cell_ocean$price)
mean(prices_constant_by_cell$price)
#plot(prices_constant_by_cell$price) #60 USD per dive

#ok, use constant price. 
ocean_coordinates_dive_suitable <- left_join(ocean_coordinates,dive_suitability,by="cell_id")
ocean_coordinates_dive_suitable_v2 <- left_join(ocean_coordinates_dive_suitable,number_of_dives,by="cell_id")

#plot suitability later
world_dive_sites <- ocean_coordinates_dive_suitable_v2 %>% mutate(suitable = replace_na(suitable,0)) %>% ggplot() + geom_raster(aes(x=lon,y=lat,fill=suitable)) + scale_fill_gradientn(colours=c("black","orange")) #ok, great
world_dive_sites
ggsave(here("figures","supplementary","world_dive_sites.jpg"),world_dive_sites, width = 20, height = 12, units = "cm")

#save plot of the dive sites of the world

# MPA location -- assume that a pixel is an MPA is f_highly_mpa>=0.5. Consult this assumption
MPA_vec <- transformed_stockdistrib %>% dplyr::select(cell_id,f_highly_mpa) %>% mutate(f_highly_mpa = (f_highly_mpa>=0.5))
MPA_loc <- MPA_vec %>% filter(f_highly_mpa=="TRUE") %>% select(cell_id)

#Question: How many of the suitable dive sites are already in MPAs?
divesite_in_MPA <- dive_suitability %>% filter(cell_id %in% MPA_loc$cell_id) %>% dim()
divesite_in_MPA#23 pixels. 
divesite_in_MPA[1]*100/dim(number_of_dives)[1] #1.13% of the dive sites are inside MPA (using 50 x 50km resolution). Add high res data analysis.

##--biodiversity prep
# Source functions
#sapply(list.files(pattern = "[.]R$", path = here::here("scripts", "functions"), full.names = TRUE),source)
#Load the functions
source(here("scripts", "functions","calculate_relative_bio_benefit.R"))
source(here("scripts", "functions","func_evaluateMPA_explicit.R"))

# Load data files necessary for biodiversity model
#load(file = file.path(this_project_dir,  "data", "02-processed-data", "bio_model_input.RData"))
load(file = file.path("/Users/kat/Library/CloudStorage/GoogleDrive-kmillage@ucsb.edu/Shared\ drives/emlab/projects/current-projects/ps-tourism/data/02-processed-data/bio_model_input.RData"))
# set Z for biodiversity
z_bio <- 0.25

##--biomass prep file
nstock<-dim(MegaData_filtered)[1]

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
(bio_benefit_current-bio_benefit_zero)*100/bio_benefit_zero #2.23517
#% increase from current MPA to all MPA
(bio_benefit_all-bio_benefit_current)*100/bio_benefit_current #38.88162

#biodiversity score, no MPA
bio_benefit_zero/max_benefit_allthreats #0.5326662

#biodiversity score, current MPA
bio_benefit_current/max_benefit_allthreats #0.5445721

#biodiversity score, all MPA
bio_benefit_all/max_benefit_allthreats #0.7563106

#% increase from current MPA to all MPA
(bio_benefit_all-bio_benefit_current)*100/bio_benefit_current

#% increase from current MPA to all threats solved (not needed)
(max_benefit_allthreats-bio_benefit_current)*100/bio_benefit_current # 83.6304

##----- BIOMASS CODE
#--test:
#func_evaluateMPA_explicit(stock_num=1, transformed_stockdistrib,MegaData_filtered_step_fin,MPA_loc)$biomass


rerun_biomass_code <- 0 #1 for on, 0 to switch this off

if(rerun_biomass_code == 1){
  stock_include <- stocklist#c(1,2,4,5,6)#stocklist[8] #c(1,2,4,5,6) #comment this. this is just a placeholder for building our code
  
  ptm <- proc.time()
  registerDoParallel(detectCores()/2)
  #stock_include<-stocklist ##stocklist#1:nstock#c(1,3,5)#which(MegaData$INCLUDE==1)
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
filtered_stock_distrib <- full_stock_distrib[,stocklist] #this is the stock distrib of our filtered stock. Max value is 1 and with NAs
dim(filtered_stock_distrib)

#K/geogrange 
Kmultiplyer <- MegaData_filtered_step_fin %>% select(Kperpixel) %>% slice(stocklist) %>% data.frame()
head(Kmultiplyer)
dim(Kmultiplyer)

df_Kmultiplyer <- t(data.frame(rep(Kmultiplyer,each=149547)))
dim(df_Kmultiplyer)

#filtered_stock_distrib[is.na(filtered_stock_distrib)] <- 0 #Replace NAs to 0
Kdistrib <- filtered_stock_distrib * df_Kmultiplyer#multiply with Kmultiplyer
#check if the above is correct
filtered_stock_distrib %>% select(`Fis-29732`) %>% filter(!is.na(`Fis-29732`))
Kdistrib %>% select(`Fis-29732`) %>% filter(!is.na(`Fis-29732`)) #ok finally

TotalKperPixel <- rowSums(Kdistrib,na.rm=T) #colSums to get the K per pixel

#k per pixel. Looks right
transformed_stockdistrib %>% ggplot(aes(x=lon,y=lat,fill=TotalKperPixel)) + scale_fill_viridis_c() + geom_raster()

#calculate B/K per pixel
BvK <- rowSums(collate_biomass_equi_merged,na.rm = TRUE)/TotalKperPixel
length(BvK)
max(BvK,na.rm=T)
BvK[order(-BvK)]
transformed_stockdistrib %>% ggplot(aes(x=lon,y=lat,fill=BvK)) + scale_fill_viridis_c(limits = c(0, 1)) + geom_raster() #ok, great

##-----NOW, close all dive pixels and calculate BvK
head(dive_suitability)
#change dive sites into MPAs
MPA_vec_dive <- transformed_stockdistrib %>% dplyr::select(cell_id,f_highly_mpa) %>% mutate(f_highly_mpa = (f_highly_mpa>=0.5)) %>% mutate(f_highly_mpa = replace(f_highly_mpa,cell_id %in% dive_suitability$cell_id, TRUE))
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

#let us subset to dive sites only and check the results.
plot(Delta_biomass[dive_suitability$cell_id]) #hmmm... there are negative biomass. This is an artefact of the estimation we used.
mean(Delta_biomass[dive_suitability$cell_id]) #0.445 increase in biomass density or from below's calculation, x% of base biomass.

transformed_stockdistrib %>% ggplot(aes(x=lon,y=lat,fill=Delta_biomass)) + scale_fill_viridis_c(limits = c(0, max(Delta_biomass,na.rm=T))) + geom_raster()

#biomass ratio and filter dive pixels only

#calculate % increase of biomass inside MPA
#biomass_data <- data.frame(ratio_biomass=(rowSums(collate_biomass_equi_merged_dive,na.rm = TRUE)-rowSums(collate_biomass_equi_merged,na.rm = TRUE))*100/rowSums(collate_biomass_equi_merged,na.rm = TRUE))
biomass_data <- data.frame(ratio_biomass=(BvK_dive-BvK)*100/BvK)
biomass_data$ratio_biom_divesite <- NA
biomass_data$ratio_biom_divesite[dive_suitability$cell_id] <- biomass_data$ratio_biomass[dive_suitability$cell_id]
biomass_data$TotalKperPixel<-TotalKperPixel

max(biomass_data$ratio_biom_divesite, na.rm=T)

biomass_data_divesites <- biomass_data %>% filter(!is.na(ratio_biom_divesite))

#K-weighted mean and stdev
#weighted.mean(biomass_data_divesites$ratio_biom_divesite, biomass_data_divesites$TotalKperPixel)#weighted mean = 117.6%
wtd.mean(biomass_data_divesites$ratio_biom_divesite, biomass_data_divesites$TotalKperPixel)
sqrt(wtd.var(biomass_data_divesites$ratio_biom_divesite, biomass_data_divesites$TotalKperPixel))#standard dev

#regular stat
mean(biomass_data$ratio_biom_divesite, na.rm=T) #on average, total biomass inside MPAs will increase by 127% vs. BAU
min(biomass_data$ratio_biom_divesite, na.rm=T)
hist(biomass_data$ratio_biom_divesite, na.rm=T)

#check the spread of the ratio of biomass
sample.n <- sum(!is.na(biomass_data$ratio_biom_divesite))
(sample.sd <- sd(biomass_data$ratio_biom_divesite,na.rm=T))
(sample.se <- sample.sd/sqrt(sample.n))

### Figure 2 --------------------------

# Mollewide projection
prj <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84"

# Map theme
map_theme <- theme_linedraw()+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        legend.position = "bottom",
        legend.key.width = unit(2, "cm"),
        plot.title = element_text(size= 13, hjust=0.5, color = "#4e4d47"), 
        panel.border = element_blank(),
        axis.ticks = element_blank())

# Land shapefile (Mollewide)
land_shp_moll <- readRDS(here("data","land_shp_moll.rds")) %>%
  sf::st_transform(crs = st_crs(prj)) # redefining CRS to make error go away

# Biomass data
biomass_df <- transformed_stockdistrib %>% 
  dplyr::select(cell_id, lon, lat, ocean, f_highly_mpa) %>%
  bind_cols(biomass_data)

# cuts <-c(0,20,40,60,80,100)

# Global map
fig_2_a <- biomass_df %>% 
  dplyr::filter(!is.na(ratio_biom_divesite)) %>%
  ggplot()+
  geom_raster(aes(x = lon, y = lat, fill=ratio_biom_divesite))+
  geom_sf(data = land_shp_moll, color = "black", fill = "black", size = 0.1)+
  scale_fill_viridis(name = "% biomass increase",
                     option = "C",
                     limits = c(0,500), 
                     oob = squish,
                     guide = guide_colorbar(title.position = "top", title.hjust = 0.5))+
  map_theme

### Caribbean plot
# Bounding box coordinates for area we want to plot - easier to pick lat/lon this way
b_coords = data.frame(lon = c(-100, -100, -60, -60, -100),
                      lat = c(-10, 30, 30, -10, -10)) %>%
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326) %>%
  st_transform(crs = st_crs(prj)) %>%
  st_bbox() # xmin, ymin, xmax, ymax

# Plot
fig_2_b <- fig_2_a +
  coord_sf(xlim = c(b_coords[1], b_coords[3]),
           ylim = c(b_coords[2], b_coords[4]))+
  # xlim(-1.1e7, -0.5e7) +
  # ylim(-0.5e6,5.2e6) +
  theme(panel.border = element_rect(color = "black", fill = NA))

### Europe plot
# Bounding box coordinates for area we want to plot - easier to pick lat/lon this way
c_coords = data.frame(lon = c(-10, -10, 40, 40, -10),
                      lat = c(20, 70, 70, 20, 20)) %>%
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326) %>%
  st_transform(crs = st_crs(prj)) %>%
  st_bbox() # xmin, ymin, xmax, ymax

# Plot 
fig_2_c <- fig_2_a +
  coord_sf(xlim = c(c_coords[1], c_coords[3]),
           ylim = c(c_coords[2], c_coords[4]))+
  # xlim(-0.2e7, 0.4e7) +
  # ylim(2.5e6,7.5e6) +
  theme(panel.border = element_rect(color = "black", fill = NA))

### Southeast Asia plot
# Bounding box coordinates for area we want to plot - easier to pick lat/lon this way
d_coords = data.frame(lon = c(95, 95, 145, 145, 95),
                      lat = c(-20, 25, 25, -20, -20)) %>%
  st_as_sf(coords = c("lon", "lat"),
           crs = 4326) %>%
  st_transform(crs = st_crs(prj)) %>%
  st_bbox() # xmin, ymin, xmax, ymax

fig_2_d <- fig_2_a +
  coord_sf(xlim = c(d_coords[1], d_coords[3]),
           ylim = c(d_coords[2], d_coords[4]))+
  # xlim(0.9e7, 1.5e7) +
  # ylim(-2.5e6,3e6) +
  theme(panel.border = element_rect(color = "black", fill = NA))

# Combine to make final plot
fig_2_bottom_row <- plot_grid(fig_2_b + theme(legend.position = "none"),
                              fig_2_c + theme(legend.position = "none"),
                              fig_2_d + theme(legend.position = "none"),
                              nrow = 1,
                              rel_widths = c(1,1,1),
                              labels = c("B)", "C)", "D)"),
                              label_y = 1.1)

fig_2 <- plot_grid(fig_2_a + theme(legend.position = "none"),
                   fig_2_bottom_row,
                   cowplot::get_legend(fig_2_a+
                                         theme(legend.margin = margin(t = 0.25, unit='cm'))),
                   nrow = 3,
                   labels = c("A)", "", ""),
                   rel_heights = c(5,3.5,1))

fig_2 

ggsave(here("figures","main","plot_biomass_increase.jpg"), fig_2, width = 6.5, height = 6.75, units = "in")

##-----NOW, close all dive pixels and calculate change in diversity
biodiv_bau <- calculate_relative_bio_benefit(is_mpa_vect = MPA_vec$f_highly_mpa, v_out_matrix =  v_out_matrix,
                                             v_in_matrix = v_in_matrix, weights  = bio_weights, 
                                             z_bio = z_bio, bau_benefit = bau_benefit, total_benefit_diff = total_benefit_diff)

biodiv_dive <- calculate_relative_bio_benefit(is_mpa_vect = MPA_vec_dive$f_highly_mpa, v_out_matrix =  v_out_matrix,
                                              v_in_matrix = v_in_matrix, weights  = bio_weights, 
                                              z_bio = z_bio, bau_benefit = bau_benefit, total_benefit_diff = total_benefit_diff)

#biodiversity score, with dive sites protected
biodiv_dive/max_benefit_allthreats #0.5735252

#% increase in biodiv score from current MPA to MPA with dive sites
(biodiv_dive-bio_benefit_current)*100/bio_benefit_current

#How much is the contribution of each pixel to the biodiversity change?
#solution: 1.) close each non-MPA dive pixel, then calculate the biodiversity change. Sum all deltas then normalize the per pixel contribution.
#identify the non-MPA dive pixels
#dive pixels -- use dive_suitability$cell_id for the cell ids of dive pixels.

divepixels_unprotected <- dive_suitability$cell_id[which(! dive_suitability$cell_id %in% MPA_loc$cell_id)]
length(divepixels_unprotected)#2020 pixels
#compare this with all dive pixels
length(dive_suitability$cell_id)#2043 pixels

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

#this is the normalized benefit value when each pixel is protected one by one.
normalized_per_pixel_delta_biodiv_benefit<-readRDS(file = here("data","per_pixel_delta_biodiv_benefit.rds"))

normalized_per_pixel_delta_biodiv_benefit %>% filter(cell_id %in% divepixels_unprotected)

#let us convert the per pixel delta biodiv benefit to be max value per territory
cell_id_territory <- dive_per_country %>% select(cell_id, lon,lat,CountryCode,territory1,sovereign1)
biodiv_territory <- left_join(normalized_per_pixel_delta_biodiv_benefit,cell_id_territory,by="cell_id")

#plot the no country label entries 
biodiv_territory %>% filter(is.na(CountryCode)) %>% ggplot(aes(x=lon,y=lat)) + geom_raster()
#!!! Those with no names perhaps are areas with no clear jurisdiction. Let us assume they are as one country.

#assume that entries with no country name will have a country code of 0.
biodiv_territory_complete <- biodiv_territory %>% mutate(CountryCode = ifelse(is.na(CountryCode), 0, CountryCode))

#renormalize values
renormalize_country_biodiv_benefit <- biodiv_territory_complete %>% group_by(CountryCode) %>% mutate(max_benefit_country = max(delta_biodiv_benefit)) %>%
  mutate(renorm_biodiv_benefit = delta_biodiv_benefit/max_benefit_country)

#now, build the dive tourism bioeconomic model

#WTP based on the changes in biomass and diversity 
price_per_dive <- 60 #price per dive
choke_price <- 197.25

change_biomass_density_full <- Delta_biomass[divepixels_unprotected]
change_diversity_full <- renormalize_country_biodiv_benefit$renorm_biodiv_benefit

##explore---
##calculate biomass density per pixel in g/m2
#changebiomass<-TotalKperPixel*Delta_biomass
#hist(changebiomass[divepixels_unprotected]/(50*50),200)

effect_name <- 4.2/2 #placeholder 2 USD
effect_biomass <- price_per_dive*0.8386*change_biomass_density_full/max(change_biomass_density_full)
effect_biodiversity <- price_per_dive*0.8186*change_diversity_full/max(change_diversity_full)

#plot WTP histogram
hist(effect_biomass,20)
hist(effect_biodiversity,20)

mean(effect_biomass)
mean(effect_biodiversity)
max(change_biomass_density_full)

#remove the pixels that are in MPA
base_number_dive <- number_of_dives$n_dives_extrap[number_of_dives$cell_id %in% divepixels_unprotected]

parameter_b <- base_number_dive/(choke_price-price_per_dive)
parameter_a <- parameter_b*choke_price

new_choke_price <- (parameter_a+((effect_name+effect_biomass+effect_biodiversity)*base_number_dive/(choke_price-price_per_dive)))/parameter_b

user_fee_opt <- effect_name + effect_biomass + effect_biodiversity
plot(user_fee_opt) #cool! so we have user fee per site.
mean(user_fee_opt)#10USD...

dive_tax <- 0

shifted_number_dive <- parameter_a - parameter_b*price_per_dive + ((effect_name+effect_biomass+effect_biodiversity-dive_tax)*base_number_dive/(choke_price-price_per_dive))
shifted_number_dive

#calculate average % increase in the number of dives with no crowding effect.
percent_dive_increase <- (shifted_number_dive - base_number_dive)*100/base_number_dive
plot(percent_dive_increase)
mean(percent_dive_increase)#13% increase in the number of dives on average

change_dive_revenue <- price_per_dive*(shifted_number_dive - base_number_dive) # in USD
new_choke_price <- (parameter_a+((effect_name+effect_biomass+effect_biodiversity)*base_number_dive/(choke_price-price_per_dive)) - (dive_tax*base_number_dive/(choke_price-price_per_dive))   )/parameter_b
change_consumer_suplus <- (0.5*shifted_number_dive*(new_choke_price-price_per_dive))-(0.5*base_number_dive*(choke_price-price_per_dive))
sum(change_dive_revenue)
sum(change_consumer_suplus)

(total_wtp <- effect_name + effect_biomass + effect_biodiversity)
(dive_tax <- (shifted_number_dive - base_number_dive)/parameter_b) #this is the same as total wtp
plot(total_wtp,dive_tax)#proof that total_wp is equal to the dive_tax

(tax_revenue <- dive_tax*base_number_dive)

#explore here --- different user fees.
explore_user_fee_wMPA <- list()
explore_user_fee_noMPA <- list()

index <- 0
wtp_combined <-effect_name + effect_biomass + effect_biodiversity
average_user_fee<-mean(wtp_combined)
average_user_fee
dive_group_size<-5
reduce_wtp_crowd<-0.1

for(frac_user_fee_opt in seq(0,5, by=0.01)){
  index <- index+1
  dive_tax <- frac_user_fee_opt*wtp_combined
  
  #shifted number of dives with crowding and with tax
  delta_q_wMPA <- (wtp_combined - dive_tax)*base_number_dive/(choke_price-price_per_dive)
  shifted_number_dive_wMPA <- base_number_dive + delta_q_wMPA#*(1-(reduce_wtp_crowd*dive_group_size*delta_q_crowding/base_number_dive))
  #if negative, it will be zero because there is no negative diving
  shifted_number_dive_wMPA[shifted_number_dive_wMPA<0]<-0
  
  #with MPA
  change_dive_revenue_wMPA <- price_per_dive*(shifted_number_dive_wMPA - base_number_dive) # in USD
  #new_choke_price_withcrowding <- (parameter_a+((wtp_combined-dive_tax)*base_number_dive/(choke_price-price_per_dive)))/parameter_b
  #change_consumer_suplus_withcrowding <- (0.5*shifted_number_dive_withcrowding*(new_choke_price_withcrowding-price_per_dive))-(0.5*base_number_dive*(choke_price-price_per_dive))
  change_consumer_suplus_wMPA <- (0.5*((shifted_number_dive_wMPA^2)/base_number_dive)*(choke_price-price_per_dive))-(0.5*base_number_dive*(choke_price-price_per_dive))
  tax_revenue_wMPA <- dive_tax*shifted_number_dive_wMPA
  explore_user_fee_wMPA[[index]]<-c(frac_user_fee_opt*average_user_fee,sum(shifted_number_dive_wMPA)-sum(base_number_dive),sum(change_dive_revenue_wMPA),sum(change_consumer_suplus_wMPA),sum(tax_revenue_wMPA))
  
  #no MPA
  delta_q_noMPA <- (-dive_tax)*base_number_dive/(choke_price-price_per_dive)
  shifted_number_dive_noMPA <- base_number_dive + delta_q_noMPA#*(1-(reduce_wtp_crowd*dive_group_size*delta_q_crowding_noMPA/base_number_dive))
  #if negative, it will be zero because there is no negative diving
  shifted_number_dive_noMPA[shifted_number_dive_noMPA<0]<-0
  
  #shifted_number_dive_noMPA <- parameter_a - parameter_b*price_per_dive + ((- dive_tax)*base_number_dive/(choke_price-price_per_dive))
  change_dive_revenue_noMPA <- price_per_dive*(shifted_number_dive_noMPA - base_number_dive) # in USD
  #new_choke_price_noMPA <- (parameter_a+((-dive_tax)*base_number_dive/(choke_price-price_per_dive)))/parameter_b
  #change_consumer_suplus_noMPA <- (0.5*shifted_number_dive_withcrowding_noMPA*(new_choke_price_noMPA-price_per_dive))-(0.5*base_number_dive*(choke_price-price_per_dive))
  change_consumer_suplus_noMPA <- (0.5*((shifted_number_dive_noMPA^2)/base_number_dive)*(choke_price-price_per_dive))-(0.5*base_number_dive*(choke_price-price_per_dive))
  tax_revenue_noMPA <- dive_tax*shifted_number_dive_noMPA
  explore_user_fee_noMPA[[index]]<-c(frac_user_fee_opt*average_user_fee,sum(shifted_number_dive_noMPA)-sum(base_number_dive),sum(change_dive_revenue_noMPA),sum(change_consumer_suplus_noMPA),sum(tax_revenue_noMPA))
  
}

#this is the only line that needs to be changed depending on how crowding is accounted for
explore_user_fee_wMPA_merged <- do.call("rbind",explore_user_fee_wMPA) %>% as.data.frame() %>% setNames(., c("fraction user fee opt", "N dives", "Change dive revenue","Change consumer surplus","Tax revenue"))
explore_user_fee_wMPA_merged$Scenario<-"With MPA"

explore_user_fee_noMPA_merged <- do.call("rbind",explore_user_fee_noMPA) %>% as.data.frame() %>% setNames(., c("fraction user fee opt", "N dives", "Change dive revenue","Change consumer surplus","Tax revenue"))
explore_user_fee_noMPA_merged$Scenario<-"No MPA"

explore_user_fee_merged <- rbind(explore_user_fee_wMPA_merged,explore_user_fee_noMPA_merged)

### Figure 4 --------------------------
plot_theme <- theme_classic()+
  theme(axis.title = element_text(size = 12),
        axis.text = element_text(size = 10))

fig_4_a <- ggplot(explore_user_fee_merged, aes(x=`fraction user fee opt`,y=`Tax revenue`/1e9,group=Scenario))+
  geom_line(aes(linetype=Scenario), lwd = 1)+
  labs(x ="", y = "Dive fee revenue \n(billion US$)")+
  scale_linetype(guide = guide_legend(title.hjust = 0.5, title.position = "top"))+
  plot_theme +
  theme(legend.position = "bottom")

fig_4_b <- ggplot(explore_user_fee_merged, aes(x=`fraction user fee opt`,y=`Change dive revenue`/1e9, group=Scenario))+
  geom_line(aes(linetype=Scenario), lwd = 1)+
  labs(x ="", y = "\u0394 dive revenue \n(billion US$)")+
  plot_theme

fig_4_c <- ggplot(explore_user_fee_merged, aes(x=`fraction user fee opt`, y=`Change consumer surplus`/1e9, group=Scenario))+
  geom_line(aes(linetype=Scenario), lwd = 1)+
  labs(x ="Average user fee per dive \n(US$)", y = "\u0394 consumer surplus \n(billion US$)")+
  plot_theme

fig_4_d <- ggplot(explore_user_fee_merged,aes(x=`fraction user fee opt`,y=`N dives`/1e6,group=Scenario))+
  geom_line(aes(linetype=Scenario), lwd = 1)+
  labs(x ="Average user fee per dive \n(US$)", y = "\u0394 # dives \n(million)")+
  plot_theme

# Combine into final plot
fig_4_no_leg <- cowplot::plot_grid(fig_4_a + theme(legend.position = "none"),
                                   fig_4_b + theme(legend.position = "none"),
                                   fig_4_c + theme(legend.position = "none"),
                                   fig_4_d + theme(legend.position = "none"), 
                                   ncol = 2, 
                                   labels = c("A)", "B)", "C)", "D)"),
                                   rel_heights=c(1,1))

fig_4 <- plot_grid(fig_4_no_leg,
                   get_legend(fig_4_a +
                                theme(legend.margin = margin(t = 0.25, unit='cm'))),
                   ncol = 1,
                   rel_heights = c(6, 1))

fig_4

ggsave(here("figures","main","plot_explore_user_fee.jpg"),fig_4, width = 6.5, height = 6, units = "in")

#This is the results for the values. Increase in consumer surplus etc.
explore_user_fee_merged[1,]

### Figure 3 --------------------------

#with MPA, then user fee of 13 USD per dive (to be exact, check the average user fee)
component_effect_name <- round(effect_name/average_user_fee,4)
component_biomass_effect <- round(mean(effect_biomass)/average_user_fee,4)
component_biodiversity_effect <- round(mean(effect_biodiversity)/average_user_fee,4)

pie_theme <- theme_void()+
  theme(legend.position = "none", # Removes the legend
                   plot.title = element_text(hjust = 0.5))

pie_dat <- data.frame(value = c(component_effect_name, component_biomass_effect, component_biodiversity_effect), 
                      Component = c("MPA name effect","Biomass effect","Biodiversity effect")) %>%
  mutate(csum = rev(cumsum(rev(value))), 
         pos = value/2 + lead(csum, 1),
         pos = if_else(is.na(pos), value/2, pos))
  
fig_3_a <- ggplot(pie_dat , aes(x = "", y = value, fill = fct_inorder(Component))) +
  geom_col(color = "black") +
  coord_polar(theta = "y") +
  ggtitle("Drivers of global MPA benefits")+
  geom_label_repel(data = pie_dat,
                   aes(y = pos, label = paste0(Component, "\n", percent(value))),
                   size = 4.5, nudge_x = c(0.8, 1, 1.3), show.legend = FALSE) +
  pie_theme

fig_3_a  #ok, this is the contribution of different components

#apply tax here and see where the benefits will go? #calculate tax revenue with crowding: tax_revenue_withcrowding
dive_tax <- round(mean(user_fee_opt))

#shifted number of dives with crowding and with tax
delta_q_crowding <- (wtp_combined - dive_tax)*base_number_dive/(choke_price-price_per_dive)
shifted_number_dive_withcrowding <- base_number_dive + delta_q_crowding*(1-(reduce_wtp_crowd*dive_group_size*delta_q_crowding/base_number_dive))
shifted_number_dive_withcrowding[shifted_number_dive_withcrowding<0]<-0
sum(shifted_number_dive_withcrowding)

#with crowding effect and with MPA
change_dive_revenue_withcrowding <- price_per_dive*(shifted_number_dive_withcrowding - base_number_dive) # in USD
new_choke_price_withcrowding <- (parameter_a+((wtp_combined-dive_tax)*base_number_dive/(choke_price-price_per_dive)))/parameter_b
change_consumer_suplus_withcrowding <- (0.5*shifted_number_dive_withcrowding*(new_choke_price_withcrowding-price_per_dive))-(0.5*base_number_dive*(choke_price-price_per_dive))
tax_revenue_withcrowding <- dive_tax*shifted_number_dive_withcrowding
explore_user_fee_withcrowding<-c(frac_user_fee_opt*average_user_fee,sum(shifted_number_dive_withcrowding)-sum(base_number_dive),sum(change_dive_revenue_withcrowding),sum(change_consumer_suplus_withcrowding),sum(tax_revenue_withcrowding))%>% 
  setNames(., c("dive tax", "N dives", "Change dive revenue","Change consumer surplus","Tax revenue"))
length(tax_revenue_withcrowding)# length is 1974

head(dive_per_country)
cell_developmentstatus <- dive_per_country[dive_per_country$cell_id %in% divepixels_unprotected,] %>% left_join(country_classification,by="territory1") %>% select(cell_id,Classification)
cell_developmentstatus$tax_revenue<-tax_revenue_withcrowding

#edit this once we have the true data
cs_dat <- cell_developmentstatus %>% 
  group_by(Classification) %>% 
  summarise(Contribution=round(sum(tax_revenue)/10^6))%>%
  filter(Classification %in% c("Developed", "Developing")) %>% 
  mutate(Percent_Contribution = Contribution/sum(Contribution)) %>%
  mutate(Percent_Contribution = replace(Percent_Contribution, Classification == "Developed", 0.80)) %>%
  mutate(Percent_Contribution = replace(Percent_Contribution, Classification == "Developing", 0.20)) %>%  
  mutate(csum = rev(cumsum(rev(Percent_Contribution))), 
         pos = Percent_Contribution/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Percent_Contribution/2, pos))
  
fig_3_b <- ggplot(cs_dat, aes(x = "", y = Percent_Contribution, fill = fct_inorder(Classification))) +
  geom_col(color = "black") +
  ggtitle("Consumer surplus beneficiaries")+
  coord_polar(theta = "y") +
  geom_label_repel(data = cs_dat,
                   aes(y = pos, label = paste0(Classification, "\n", percent(Percent_Contribution))),
                   size = 4.5, nudge_x = c(0.9), show.legend = FALSE) +
  scale_fill_brewer()+
    #labels=c('Foreign','Local')) +
  pie_theme

fig_3_b

tax_rev_dat <- cell_developmentstatus %>% 
  group_by(Classification) %>% 
  summarise(Contribution=round(sum(tax_revenue)/10^6))%>%
  filter(Classification %in% c("Developed", "Developing")) %>% 
  mutate(Percent_Contribution = Contribution/sum(Contribution)) %>%
  mutate(csum = rev(cumsum(rev(Percent_Contribution))), 
         pos = Percent_Contribution/2 + lead(csum, 1),
         pos = if_else(is.na(pos), Percent_Contribution/2, pos))
  
fig_3_c <- ggplot(tax_rev_dat, aes(x = "", y = Percent_Contribution, fill = fct_inorder(Classification))) +
  geom_col(color = "black") +
  ggtitle("Tax revenue beneficiaries")+
  coord_polar(theta = "y") +
  geom_label_repel(data = tax_rev_dat,
                   aes(y = pos, label = paste0(Classification, "\n", percent(Percent_Contribution))),
                   size = 4.5, nudge_x = c(0.9), show.legend = FALSE) +
  scale_fill_brewer() +
  pie_theme

fig_3_c

# combine into final plot 
fig_3_bottom_row <- cowplot::plot_grid(fig_3_b,
                                       fig_3_c,
                                       nrow = 1,
                                       labels = c("B)", "C)"))

fig_3 <- plot_grid(fig_3_a,
                   fig_3_bottom_row,
                   ncol = 1,
                   rel_heights = c(2,1.5),
                   labels = c("A)", ""))

fig_3

ggsave(here("figures","main","fig_3.jpg"),fig_3, width = 6.5, height = 6.75, units = "in")

#This is the summary table of results! with/without MPA and varying tax level.
head(explore_user_fee_merged)

#Summary table: with MPA, no tax. Just printing the change in biological parameters.
#this table will output the following:
#1. Pixel number of unprotected dive sites
#2. Change in biomass per MPA pixels
#3. Change in biodiversity metric per MPA pixel
# summary_table1 <- data.frame(pixel_number = divepixels_unprotected)
# summary_table1$change_biomass <- change_biomass_density_full
# summary_table1$change_diversity <- change_diversity_full
# summary_table1$change_ndive <- 
# head(summary_table1)




