#Pristine seas tourism-MPA model
#Author: Reniel Cabral
#Clean version (18 Nov 2021)
#This code evaluates the build-up of biomass inside the current MPAs.
#After fully parameterizing this code, it is easy to add a chunk of code that evaluates tourism benefits of new MPAs.

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
#plot(transformed_stockdistrib$f_highly_mpa) # ok, we need a threshold for highly protected MPA.
#plot(transformed_stockdistrib$`Fis-29732`)
#Question: what is the threshold for highly protected MPA? 0.5? or any pixel with MPA will be considered MPA?

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
dim(dive_suitability)[1]*100/dim(transformed_stockdistrib)[1] #this is the fraction of ocean with diving.
dim(dive_suitability)[1]*50*50# this is in km2. This is the total area of ocean surface with diving.

#check other files
#number of dives
number_of_dives <- read.csv(here("data","dive","number_of_dives_extrapolated_by_cell.csv"))
head(number_of_dives)
dim(number_of_dives)#1997
sum(number_of_dives$n_dives_extrap)#50700017 dives.
hist(number_of_dives$n_dives_extrap)
ggplot(data.frame(log(number_of_dives$n_dives_extrap)), aes(log(number_of_dives$n_dives_extrap))) +     # Histogram with logarithmic axis
  geom_histogram(bins=10)

#load country classification (SIDS, developing, etc.)
country_classification <- read.csv(here("data","UN_territory_classification.csv"))
country_classification$SIDS <-as.factor(country_classification$SIDS)

#plot number of dives per country
dive_per_country <- left_join(number_of_dives,cell_id_with_country,by="cell_id")

head(dive_per_country)
plot_number_dives <- dive_per_country %>% group_by(territory1) %>% summarize(n_dive=sum(n_dives_extrap)) %>% left_join(country_classification,by="territory1") %>%
  arrange(-n_dive) %>% slice(1:50) %>%
  #ggplot(aes(x=as.factor(territory1),y=n_dive)) + geom_col()
  ggplot(aes(x = reorder(as.factor(territory1), n_dive/1000000), y = n_dive/1000000, fill=SIDS))+
  geom_bar(stat = "identity")+ theme_classic()+#theme_minimal()+#theme_classic()+
  coord_flip()+ labs(y = "Dive per year, million")+theme(axis.title.y = element_blank())
plot_number_dives


#plot number of dive pixels per country
plot_number_divepexels_country <- dive_per_country %>% group_by(territory1) %>% summarize(n_divesites=n()) %>% filter(territory1!="NA") %>%
  arrange(-n_divesites) %>% slice(1:50) %>%
  #ggplot(aes(x=as.factor(territory1),y=n_dive)) + geom_col()
  ggplot(aes(x = reorder(as.factor(territory1), n_divesites), y = n_divesites))+
  geom_bar(stat = "identity")+ theme_classic()+#theme_minimal()+#theme_classic()+
  coord_flip()+ labs(y = "Number of dive site pixel")+theme(axis.title.y = element_blank())
plot_number_divepexels_country


#number of pixels per territory
ndivepixel_territory <- cell_id_with_country  %>% group_by(territory1) %>% summarize(total_territory_pixel=n())
#plot % of the eez that are number of dive sites
plot_number_divesites <- dive_per_country %>% group_by(territory1) %>% summarize(n_divesites=n()) %>% left_join(ndivepixel_territory,by="territory1") %>% mutate(percent_divearea = n_divesites*100/total_territory_pixel) %>% filter(territory1!="NA") %>%
  arrange(-percent_divearea) %>% slice(1:50) %>%
  #ggplot(aes(x=as.factor(territory1),y=n_dive)) + geom_col()
  ggplot(aes(x = reorder(as.factor(territory1), percent_divearea), y = percent_divearea))+
  geom_bar(stat = "identity")+ theme_classic()+#theme_minimal()+#theme_classic()+
  coord_flip()+ labs(y = "% of EEZ that is dive sites")+theme(axis.title.y = element_blank())
plot_number_divesites

##--correlate # of dives with on-reef values
#number of dive per country
ndive_per_country <- dive_per_country %>% group_by(territory1) %>% summarize(n_dive=sum(n_dives_extrap))
head(ndive_per_country)
dim(ndive_per_country)
#on-reef tourism value per territory from Spalding et al.
onreef_values <- read.csv(here("data","Tourvalues_Spalding.csv")) 
head(onreef_values)
dim(onreef_values)
#now perform inner join
correlate_dive_and_value <- merge(x=ndive_per_country,y=onreef_values,by="territory1")
dim(correlate_dive_and_value)
head(correlate_dive_and_value)

plot_correlate_dive_and_value<- ggplot(correlate_dive_and_value, aes(x=OnReef/1000000,y=n_dive/1000000))+geom_point()+geom_smooth(method = lm,colour="gray")+
  geom_text_repel(aes(OnReef/1000000, n_dive/1000000, label = territory1), size = 3)+ 
  labs(x="On-reef tourism value, billion US$", y = "Dive per year, million")+theme_classic()
plot_correlate_dive_and_value

plotme<-cowplot::plot_grid(plot_number_divepexels_country, plot_number_dives,plot_correlate_dive_and_value,NULL, ncol = 2, labels = "AUTO",rel_heights=c(1,0.5))
plotme
ggsave(here("figures","main","plot_number_dives.jpg"),plotme, width = 20, height = 20, units = "cm")

#save a country file for me to assign country development categorization
head(dive_per_country)
##---[no need to run] dive_per_country %>% select(sovereign1) %>% unique() %>% write.csv(.,file = here("data","country_classification.csv"))
##---[no need to run] dive_per_country %>% select(territory1) %>% unique() %>% write.csv(.,file = here("data","territory_classification.csv"))


##--price per dive
prices_constant_by_cell <- read.csv(here("data","dive","prices_constant_by_cell.csv"))
dim(prices_constant_by_cell)
plot(prices_constant_by_cell$price) #60 USD per dive

#price per country
prices_country_region_by_cell <- read.csv(here("data","dive","prices_country_region_by_cell.csv"))
plot(prices_country_region_by_cell$price)
#request to add country name or country code in the file. Or a file of cell_id and country names.


#ok, use constant price for now. 

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
23*100/2197 #1.05% of the dive sites are inside MPA... hmmm... this sounds correct? maybe we should analyze it using high res data.

##--biodiversity prep
# Source functions
sapply(list.files(pattern = "[.]R$", path = here::here("scripts", "functions"), full.names = TRUE),source)
# Load data files necessary for biodiversity model
load(file = file.path(this_project_dir,  "data", "02-processed-data", "bio_model_input.RData"))
# set Z for biodiversity
z_bio <- 0.25

##--biomass prep file
nstock<-dim(MegaData_filtered)[1]

##---BIODIVERSITY CODE
# Calculate biodiversity benefit from today's protected cells
calculate_relative_bio_benefit(is_mpa_vect = MPA_vec$f_highly_mpa, v_out_matrix =  v_out_matrix,
                               v_in_matrix = v_in_matrix, weights  = bio_weights, 
                               z_bio = z_bio, bau_benefit = bau_benefit, total_benefit_diff = total_benefit_diff)

##----- BIOMASS CODE
#--test:
#func_evaluateMPA(stock_num=1, transformed_stockdistrib,MegaData_filtered_step3,MPA_loc)$bvk_equi

ptm <- proc.time()
registerDoParallel(detectCores()/2)
stock_include<-stocklist#1:nstock#c(1,3,5)#which(MegaData$INCLUDE==1)
collate_bvk_equi_merged <- foreach(stock_num=stock_include, .combine='cbind') %dopar% {
  func_evaluateMPA(stock_num, transformed_stockdistrib,MegaData_filtered_step3,MPA_loc)$bvk_equi
}
doParallel::stopImplicitCluster()
(proc.time() - ptm)/60 #check process time in minutes
colnames(collate_bvk_equi_merged)<-MegaData_filtered_step3$stockid[stock_include]
#head(collate_bvk_equi_merged)
dim(collate_bvk_equi_merged)

#plot
#collate_bvk_equi_merged[is.na(collate_bvk_equi_merged)] <- 0 #never do this.
collate_bvk_equi_merged[collate_bvk_equi_merged>1]<-1 #cap to 1
plotme<-rowMeans(collate_bvk_equi_merged,na.rm = TRUE) #ok, this is just the mean of the densities. A better plot would be bvk_i*k_i/k_total

#compute B/K per pixel
max(transformed_stockdistrib[6:1155],na.rm=T)
full_stock_distrib <- transformed_stockdistrib[6:1155]
filtered_stock_distrib <- full_stock_distrib[,stocklist] #this is the stock distrib of our filtered stock. Max value is 1 and with NAs

#compute for the geog range
dim(filtered_stock_distrib)
geog_range_perstock<-colSums(filtered_stock_distrib,na.rm=T)
max(geog_range_perstock,na.rm=T) #ok cool

#K/geogrange 
head(MegaData_filtered)
Kmultiplyer <- MegaData_filtered %>% select(Kfin) %>% slice(stocklist) %>% mutate(KvGeogRange=Kfin/geog_range_perstock) %>% select(KvGeogRange) %>% data.frame()
head(Kmultiplyer)
dim(Kmultiplyer)

df_Kmultiplyer <- t(data.frame(rep(Kmultiplyer,each=149547)))
dim(df_Kmultiplyer)

dim(filtered_stock_distrib)

#filtered_stock_distrib[is.na(filtered_stock_distrib)] <- 0 #Replace NAs to 0
Kdistrib <- filtered_stock_distrib * df_Kmultiplyer#multiply with Kmultiplyer
#check if the above is correct
filtered_stock_distrib %>% select(`Fis-29732`) %>% filter(!is.na(`Fis-29732`))
Kdistrib %>% select(`Fis-29732`) %>% filter(!is.na(`Fis-29732`)) #ok finally

TotalKperPixel <- rowSums(Kdistrib,na.rm=T) #colSums to get the K per pixel

#k per pixel. Looks right
transformed_stockdistrib %>% ggplot(aes(x=lon,y=lat,fill=TotalKperPixel)) + scale_fill_viridis_c() + geom_raster()

BperPixel<-collate_bvk_equi_merged * df_Kmultiplyer
#transformed_stockdistrib %>% ggplot(aes(x=lon,y=lat,fill=rowSums(BperPixel,na.rm = TRUE))) + scale_fill_viridis_c() + geom_raster() #hmmm
BvK <- rowSums(BperPixel,na.rm = TRUE)/TotalKperPixel
length(BvK)
max(BvK,na.rm=T)

transformed_stockdistrib %>% ggplot(aes(x=lon,y=lat,fill=BvK)) + scale_fill_viridis_c(limits = c(0, 1)) + geom_raster() #ok, great

##-----NOW, close all dive pixels and calculate BvK
head(dive_suitability)
#change dive sites into MPAs
MPA_vec_dive <- transformed_stockdistrib %>% dplyr::select(cell_id,f_highly_mpa) %>% mutate(f_highly_mpa = (f_highly_mpa>=0.5)) %>% mutate(f_highly_mpa = replace(f_highly_mpa,cell_id %in% dive_suitability$cell_id, TRUE))
MPA_loc_dive <- MPA_vec_dive %>% filter(f_highly_mpa=="TRUE") %>% select(cell_id)
##----- BIOMASS CODE
ptm <- proc.time()
registerDoParallel(detectCores()/2)
collate_bvk_equi_merged_dive <- foreach(stock_num=stock_include, .combine='cbind') %dopar% {
  func_evaluateMPA(stock_num, transformed_stockdistrib,MegaData_filtered_step3,MPA_loc_dive)$bvk_equi
}
doParallel::stopImplicitCluster()
(proc.time() - ptm)/60 #check process time in minutes
colnames(collate_bvk_equi_merged_dive)<-MegaData_filtered_step3$stockid[stock_include]

collate_bvk_equi_merged_dive[collate_bvk_equi_merged_dive>1]<-1 #cap to 1

#compute B/K per pixel
BperPixel_dive<-collate_bvk_equi_merged_dive * df_Kmultiplyer
BvK_dive <- rowSums(BperPixel_dive,na.rm = TRUE)/TotalKperPixel
transformed_stockdistrib %>% ggplot(aes(x=lon,y=lat,fill=BvK_dive)) + scale_fill_viridis_c(limits = c(0, 1)) + geom_raster() 

Delta_biomass <- BvK_dive - BvK
transformed_stockdistrib %>% ggplot(aes(x=lon,y=lat,fill=Delta_biomass)) + scale_fill_viridis_c(limits = c(0, max(Delta_biomass,na.rm=T))) + geom_raster()

Ratio_biomass <- BvK_dive/BvK
max(Ratio_biomass, na.rm=T)
mean(Ratio_biomass, na.rm=T)
Ratio_biomass[dive_suitability$cell_id]
#Ratio_biomass[is.nan(Ratio_biomass)] <- 0

mean(Ratio_biomass[dive_suitability$cell_id],na.rm=T) #on average, total biomass inside MPAs will increase by 23% vs. BAU

#plot only the areas with dive tourism
b0<-transformed_stockdistrib %>% ggplot(aes(x=lon,y=lat,fill=Ratio_biomass)) + scale_fill_viridis_c(limits = c(0, max(Ratio_biomass,na.rm=T))) + geom_raster()
b1<-transformed_stockdistrib %>% ggplot(aes(x=lon,y=lat,fill=Ratio_biomass)) + scale_fill_viridis_c(limits = c(0, max(Ratio_biomass,na.rm=T))) + geom_raster()+
  xlim(0.9e7, 1.5e7) +ylim(-2.5e6,3e6)

#ok, the above is the biomass component. We know how much bvk will increase inside MPA.

plot_biomass_increase<-cowplot::plot_grid(b0, b1,b1,b1, ncol = 2, labels = "AUTO",rel_heights=c(1,1))
plot_biomass_increase
ggsave(here("figures","main","plot_biomass_increase.jpg"),plot_biomass_increase, width = 20, height = 10, units = "cm")




##-----NOW, close all dive pixels and calculate change in diversity
biodiv_bau <- calculate_relative_bio_benefit(is_mpa_vect = MPA_vec$f_highly_mpa, v_out_matrix =  v_out_matrix,
                                             v_in_matrix = v_in_matrix, weights  = bio_weights, 
                                             z_bio = z_bio, bau_benefit = bau_benefit, total_benefit_diff = total_benefit_diff)

biodiv_dive <- calculate_relative_bio_benefit(is_mpa_vect = MPA_vec_dive$f_highly_mpa, v_out_matrix =  v_out_matrix,
                                              v_in_matrix = v_in_matrix, weights  = bio_weights, 
                                              z_bio = z_bio, bau_benefit = bau_benefit, total_benefit_diff = total_benefit_diff)

#How much is the contribution of each pixel to the biodiversity change?
#solution: 1.) close each non-MPA dive pixel, then calculate the bodiversity change. Sum all deltas then normalize the per pixel contribution.
#identify the non-MPA dive pixels
#dive pixels -- use dive_suitability$cell_id for the cell ids of dive pixels.

divepixels_unprotected <- dive_suitability$cell_id[which(! dive_suitability$cell_id %in% MPA_loc$cell_id)]
length(divepixels_unprotected)#1974
#compare this with all dive pixels
length(dive_suitability$cell_id)

#--run this just once because it takes time. This can be optimized by parallelizing the code (easy to implement)
run_me <- 0
if(run_me==1){
  
  #close each pixel and compute for the biodiversity benefits
  MPA_vec_base <- transformed_stockdistrib %>% dplyr::select(cell_id,f_highly_mpa) %>% mutate(f_highly_mpa = (f_highly_mpa>=0.5))
  
  store_per_pixel_delta_biodiv_benefit <- data.frame(cell_id=divepixels_unprotected)
  store_per_pixel_delta_biodiv_benefit$delta_biodiv_benefit <- 0
  
  #parallel version of the code
  registerDoParallel(detectCores()/2)
  foreach(i=1:length(divepixels_unprotected)) %dopar% {
    MPA_vec_close1 <- MPA_vec_base %>% mutate(f_highly_mpa = replace(f_highly_mpa,cell_id %in% divepixels_unprotected[i], TRUE))
    #evaluate biodiv benefit of closing 1 pixel then subtract with the BAU result. Then save the results.
    store_per_pixel_delta_biodiv_benefit$delta_biodiv_benefit[i] <- calculate_relative_bio_benefit(is_mpa_vect = MPA_vec_close1$f_highly_mpa, v_out_matrix =  v_out_matrix,
                                                                                                   v_in_matrix = v_in_matrix, weights  = bio_weights, 
                                                                                                   z_bio = z_bio, bau_benefit = bau_benefit, total_benefit_diff = total_benefit_diff) - biodiv_bau
  }
  doParallel::stopImplicitCluster()
  
  # #unparallel version
  # for (i in 1:length(divepixels_unprotected)){
  #   #close a pixel
  #   MPA_vec_close1 <- MPA_vec_base %>% mutate(f_highly_mpa = replace(f_highly_mpa,cell_id %in% divepixels_unprotected[i], TRUE))
  #   #evaluate biodiv benefit of closing 1 pixel then subtract with the BAU result. Then save the results.
  #   store_per_pixel_delta_biodiv_benefit$delta_biodiv_benefit[i] <- calculate_relative_bio_benefit(is_mpa_vect = MPA_vec_close1$f_highly_mpa, v_out_matrix =  v_out_matrix,
  #                                                                                                  v_in_matrix = v_in_matrix, weights  = bio_weights, 
  #                                                                                                  z_bio = z_bio, bau_benefit = bau_benefit, total_benefit_diff = total_benefit_diff) - biodiv_bau
  # }
  
  head(store_per_pixel_delta_biodiv_benefit)
  #normalize the benefit value per pixel and save it
  store_per_pixel_delta_biodiv_benefit$delta_biodiv_benefit<-store_per_pixel_delta_biodiv_benefit$delta_biodiv_benefit/sum(store_per_pixel_delta_biodiv_benefit$delta_biodiv_benefit)
  sum(store_per_pixel_delta_biodiv_benefit$delta_biodiv_benefit)
  plot(store_per_pixel_delta_biodiv_benefit$delta_biodiv_benefit)
  saveRDS(store_per_pixel_delta_biodiv_benefit, file = here("data","per_pixel_delta_biodiv_benefit.rds"))
}

#this is the normalized benefit value when each pixel is protected one by one.
normalized_per_pixel_delta_biodiv_benefit<-readRDS(file = here("data","per_pixel_delta_biodiv_benefit.rds"))

#now, build the bioeconomic model





#FINxxxxxx
#xxxxxxxxxxxxxx

#--load EEZ mollweide file
Final_EEZ_file_with_names <- readRDS(here("data","Final_EEZ_file_with_names.rds"))
head(Final_EEZ_file_with_names)

#--Plot the EEZ
Final_EEZ_file_with_names %>% ggplot(aes(x=lon,y=lat,fill=1)) + geom_raster()

#--load other files. Load the MPA coordinates and our working coordinate.
#land_shp_moll <- readRDS(here("data","land_shp_moll.rds"))
MPA_coord <- readRDS(here("data","MPA_coord_mollweide.rds"))
CleanCoordmegacell<-readRDS(here("data","CleanCoordmegacell_mollweide.rds"))
dim(CleanCoordmegacell)

#--extract K per cell (not stored in Github because of the file size)
Cleanmegacell<-readRDS(file = "/Users/ren/Documents/GitHub/tourism-mpa-support/Cleanmegacell_mollweide.rds")
#FoodProvison_SupportFiles/Code Food Provision MPA/Cleanmegacell_mollweide.rds")
#readRDS(here("data","Cleanmegacell_mollweide.rds")) #

#we will assume that K per cell is constant per stock. ok, we can deal with this later. For now, all good.
colSums(Cleanmegacell)
plot(Cleanmegacell[,1])

head(Cleanmegacell)
dim(Cleanmegacell)
colSums(Cleanmegacell)
MegaData<-readRDS(here("data","MegaData_Ray.rds"))#readRDS(file = "/Users/ren/Documents/GitHub/FoodProvison_SupportFiles/Code Food Provision MPA/MegaData_Ray.rds")
head(MegaData)

#check the order of the stock list
stocklist <- data.frame(MegaData$stockid)
stocklist$stocklist2 <- colnames(Cleanmegacell)

#K per stock
KperStock <- MegaData$Kfin
head(KperStock)
length(KperStock)

KperStock_expand<-matrix(rep(KperStock,each=120297),nrow=120297)
dim(KperStock_expand)

KperStockCell<-KperStock_expand*Cleanmegacell#KperStock
colSums(KperStockCell)
KperCell<-rowSums(KperStockCell)
length(KperCell)

#get MPA positions
MPA_coord$MPA<-1 
head(MPA_coord)
dim(MPA_coord)
CleanCoordmegacell_MPA<-left_join(CleanCoordmegacell,MPA_coord,by=c("lon","lat"))
head(CleanCoordmegacell_MPA)
dim(CleanCoordmegacell_MPA)
#positions of 1s
MPAposition<-which(CleanCoordmegacell_MPA$MPA==1)
head(MPAposition)
length(MPAposition)

#--join eez and clean coord file with MPA
CleanCoordmegacell_EEZ_wMPA<-left_join(CleanCoordmegacell_MPA,Final_EEZ_file_with_names,by=c("lon","lat"))
#convert NA to 0, then plot MPA file to check
CleanCoordmegacell_EEZ_wMPA$MPA[is.na(CleanCoordmegacell_EEZ_wMPA$MPA)] <- 0
head(CleanCoordmegacell_EEZ_wMPA)
dim(CleanCoordmegacell_EEZ_wMPA)

#--plot current MPAs
CleanCoordmegacell_EEZ_wMPA %>% filter(MPA==1)  %>%  ggplot(aes(x=lon,y=lat,fill=1)) + geom_raster() #ok, great

#Plot world
CleanCoordmegacell_EEZ_wMPA %>%  ggplot(aes(x=lon,y=lat,fill=1)) + geom_raster() #ok, great
head(CleanCoordmegacell_EEZ_wMPA)
#--note:CleanCoordmegacell_EEZ_wMPA contains the lat, lon, MPA or not, territory, and soverignity

#add biol parameters placeholder and i.d.
biol_data <- CleanCoordmegacell_EEZ_wMPA %>% dplyr::select(lon, lat, MPA) %>% mutate(r=0.5, K=1000, E=0.25, B=1000) %>% mutate(pos1 = row_number()) #B=K*runif(n(), min=0, max=1)
max(biol_data$pos1)
head(biol_data)
dim(biol_data)[1]

#--create distance matrix
#this is a test
sqrt((biol_data[1,1]-biol_data[2,1])^2+(biol_data[1,2]-biol_data[2,2])^2) #50100 unit in meters -- so this is 50km or 0.5 degree
(n_pixel <- dim(biol_data)[1]) #there are 120294 pixels

# #IMPORTANT -- JUST RUN ONCE. COMMENTED FOR NOW. JUST LOAD THE OUTPUT.
# #Option 1 for computing the distance matrix is good
# #permutation with no repetition, and only store the elements within the x km dispersal distance
# dispersal_distance_lim<-1000 #let us say a limit of 1000km as the dispersal distance range...realistic?
# distance_mat<-list()
# cores<-detectCores()
# registerDoParallel(cores-1)
# for (i in 1:n_pixel){ #calculate the distance of pixel i to all (except itself and no repetition, i.e., the half of the distance matrix)
#   distance <- sqrt((biol_data[i,1]-biol_data[i+1:n_pixel,1])^2+(biol_data[i,2]-biol_data[i+1:n_pixel,2])^2)/1000
#   position <- which(distance<=dispersal_distance_lim)
# 
#   if (length(position)==0){next} #in case zero data
# 
#   #save i,j,distance
#   prep_data<-as.data.frame(distance[position])
#   colnames(prep_data) <- "dist"
#   prep_data$pos1<-i
#   prep_data$pos2<-position+i
# 
#   distance_mat[[i]] <- prep_data
# }
# distance_mat_merged <- do.call("rbind",distance_mat)
# stopImplicitCluster()
# head(distance_mat_merged)
# dim(distance_mat_merged)
# max(distance_mat_merged$pos1)
# max(distance_mat_merged$pos2)
# #save the data then load so we do not need to run the code above
# saveRDS(distance_mat_merged, file = "/Users/ren/Documents/GitHub/tourism-mpa/data/distance_mat_merged.rds")

distance_mat_merged <- readRDS(here("data","distance_mat_merged.rds"))
dim(distance_mat_merged)
head(distance_mat_merged)

#--Complete the distance matrix by adding in the self-loop and the other part of the mirror matrix
#--the second mirror half of the matrix
distance_mat_part2 <- distance_mat_merged %>% dplyr::select(dist,pos2,pos1) 
colnames(distance_mat_part2) <- c("dist","pos1","pos2")
head(distance_mat_part2)

#--the link to itself
distance_mat_part3 <- data.frame(dist = rep(0,n_pixel)) %>% mutate(pos1 = 1:n_pixel, pos2 = 1:n_pixel)
head(distance_mat_part3)

#now, combine the three datasets to complete the matrix
distance_mat_full<-rbind(distance_mat_merged,distance_mat_part2,distance_mat_part3)
dim(distance_mat_full)

##--ADD a column indicating the proportion of biomass that will move at a specific site. Use a gaussian dispersal.
sigma <- 100 #this is a single value for now. Eventually, we will have species-specific parameter

#use group_by, remove the distance column
distance_mat_full_prop <- distance_mat_full %>% group_by(pos1) %>% mutate(biom_prop = exp(-( dist^2 / (2*(sigma^2))) ), biom_prop = biom_prop/sum(biom_prop)) %>% dplyr::select(-dist) %>% as.data.table()
head(distance_mat_full_prop) #fast!
#check if correct
distance_mat_full_prop %>% filter(pos1==1) %>% summarise(sum(biom_prop)) #ok good

#ok, now that we have the distance matrix, implement biomass diffusion and larval dispersal 
head(biol_data)

biom <- biol_data %>% dplyr::select(pos1,B)
head(biom)

head(biol_data)
E <- biol_data$E #we can make this dynamic. i.e., as MPA size increases, E changes.
MPAcell <- biol_data$MPA
rK<-biol_data %>% dplyr::select(pos1,r,K) %>% as.data.table()
setkey(rK,pos1)
setkey(distance_mat_full_prop,pos1)
distance_mat_full_prK<-distance_mat_full_prop[rK]
#distance_mat_full_prKE<-merge(distance_mat_full_prop,rKE, all.x=TRUE)
head(distance_mat_full_prK)

##***CHUNK*** Derive B/K per pixel and E per pixel.
#K per stock
dim(KperStockCell)

rperStock_expand <- matrix(rep(MegaData$r_fin,each=120297),nrow=120297)
dim(rperStock_expand)
rperStockCell <- rperStock_expand*KperStockCell #r*K per cell
rperCell <- rowSums(rperStockCell)/rowSums(KperStockCell)
length(rperCell)
head(rperCell)

#plot r per cell
CleanCoordmegacell_EEZ_wMPA %>% select(lon, lat, MPA) %>% mutate(rperCell=rperCell) %>%
  ggplot(aes(x=lon,y=lat,fill=rperCell)) + scale_fill_viridis_c(limits = c(0, max(rperCell))) + geom_raster()

#plot E per cell
plot(MegaData$Efin_BAU1_Ray,MegaData$ExploitationRate_BAU1_Ray) #ok the formula is ER=1-E

ERperStock_expand <- matrix(rep(MegaData$ExploitationRate_BAU1_Ray,each=120297),nrow=120297)
ERperStockCell <- ERperStock_expand*KperStockCell #r*K per cell
ERperCell <- rowSums(ERperStockCell)/rowSums(KperStockCell)
CleanCoordmegacell_EEZ_wMPA %>% dplyr::select(lon, lat, MPA) %>% mutate(ERperCell=ERperCell) %>%
  ggplot(aes(x=lon,y=lat,fill=ERperCell)) + scale_fill_viridis_c(limits = c(0, max(ERperCell))) + geom_raster()

###CHUNK
#implement the analytic solution. THIS SPEEDS UP THE COMPUTATION
#steps
#NOTE: This is not necessary!!!
#1. K per pixel per stock per cell. K density per pixel per stock should be constant.
Cleanmegacell<-readRDS(file = "/Users/ren/Documents/GitHub/FoodProvison_SupportFiles/Code Food Provision MPA/Cleanmegacell_mollweide.rds")
ncell<-dim(Cleanmegacell)[1]
kpercell_filter<-(Cleanmegacell>0)*1 #stock extent

#kperstock
MegaData <- readRDS(file = "/Users/ren/Documents/GitHub/FoodProvison_SupportFiles/Code Food Provision MPA/MegaData_Ray.rds")
kperstock <- MegaData$Kfin
head(kperstock)

#kpercellperstock. This distributes K spatially.
kpercell_denominator<-matrix(rep(colSums(kpercell_filter)/kperstock,each=ncell),nrow=ncell)
kpercell_expand<-kpercell_filter/kpercell_denominator
colSums(kpercell_expand) #ok, looks good. the same as kperstock

#plot k per cell
head(CleanCoordmegacell_EEZ_wMPA)
CleanCoordmegacell_EEZ_wMPA %>% dplyr::select(lon, lat) %>% mutate(kpercell=rowSums(kpercell_expand)) %>%
  ggplot(aes(x=lon,y=lat,fill=kpercell)) + scale_fill_viridis_c()+ geom_raster()

#2. r per stock
head(MegaData)
rperstock<-MegaData$r_fin

#compute bvk
#bvk_calculated <- MegaData %>% filter(INCLUDE==1) %>% summarise(bvk_calc = 1-(ExploitationRate_BAU1_Ray/r_fin))
bvk_calculated <- MegaData %>% summarise(bvk_calc = 1-(ExploitationRate_BAU1_Ray/r_fin))
head(bvk_calculated)
dim(bvk_calculated)

#3. B0vK #revisit this. Critical parameter
#BvKperStock_expand <- matrix(rep(MegaData$BK2012,each=120297),nrow=120297)
BvKperStock_expand <- matrix(rep(bvk_calculated$bvk_calc,each=120297),nrow=120297)
biomperStockCell <- BvKperStock_expand*kpercell_expand #r*K per cell
bvk_params <- BvKperStock_expand*(kpercell_expand>0) ##use this parameter
#this contains bvk per species per cell
#we do not actually need the bvk per cell. We just need a single value as it is constant per stock.
plot(bvk_params[,1])

dim(biomperStockCell)
head(biomperStockCell) 
#print the first entry: 
sum(biomperStockCell[,1])
plot(biomperStockCell[,1]) #ok, this is flat. good.

#Only include the stocks that are part of our analysis.
BvKperCell <- rowSums(biomperStockCell[,which(MegaData$INCLUDE==1)])/rowSums(kpercell_expand[,which(MegaData$INCLUDE==1)])

#plot average. 
CleanCoordmegacell_EEZ_wMPA %>% dplyr::select(lon, lat, MPA) %>% #mutate(BvKperCell=BvKperCell) %>%
  ggplot(aes(x=lon,y=lat,fill=BvKperCell)) + scale_fill_viridis_c(limits = c(0, 1)) + geom_raster()

#4. Assume all pixels allow fishing. Evaluate change in biomass at pixel i. Our assumption is conservative given that build-up of biomass only happens inside the MPA. 
#Non-mpa positions
MPAcell <- biol_data$MPA
nonMPAposition<-which(MPAcell==0)

length(nonMPAposition)
length(MPAcell)

#--explore the connectivity matrix here
#--This is the base code for generating distance_mat_full_prop
head(distance_mat_full) #the columns are: dist, pos1, pos2 (pos1 is the source)
distance_mat_full_prop <- distance_mat_full %>% group_by(pos1) %>% mutate(biom_prop = exp(-( dist^2 / (2*(sigma^2))) ), biom_prop = biom_prop/sum(biom_prop)) %>% dplyr::select(-dist) %>% as.data.table()
head(distance_mat_full_prop) #fast!

#--check if correct
distance_mat_full_prop %>% group_by(pos1) %>% summarise(checksum=sum(biom_prop)) #ok, the answer is correct.

#--add evaluate MPA here
source(here("scripts","func_evaluateMPA.R")) 

##--Run only once. Subsetting the connectivity matrix. Save inside a folder then call inside the function
#--subset the stock and get the row numbers where entry == 1, then subset the connectivit matrix
run_subset_connectivitymatrix <- 0 #1 for on, 0 to switch this off
#the code below can be optimized by running this in parallel.
if(run_subset_connectivitymatrix == 1){
  registerDoParallel(detectCores()/2)
  #CHECK IF THIS IS CORRECT: which(MegaData$INCLUDE==1)
  foreach(stock_num=which(MegaData$INCLUDE==1)) %dopar% {
    #for (stock_num in which(MegaData$INCLUDE==1)){
    stock_subset_i<-which(Cleanmegacell[,stock_num] > 0) #that's K density map so the values are < 1
    distance_mat_full_prop <- distance_mat_full %>% filter(pos1 %in% stock_subset_i, pos2 %in% stock_subset_i) %>% group_by(pos1) %>% mutate(biom_prop = exp(-( dist^2 / (2*(sigma^2))) ), biom_prop = biom_prop/sum(biom_prop)) %>% dplyr::select(-dist) %>% as.data.table()
    
    #fts is the fastest in saving and loading files.
    #fst::write.fst(distance_mat_full_prop , here("data","connect_matrix",paste0(stock_num,"_connect.fst")))
    fst::write.fst(distance_mat_full_prop , paste0("/Users/ren/Documents/GitHub/tourism-mpa-support/connect_matrix/",stock_num,"_connect.fst"))
  }
  doParallel::stopImplicitCluster()
}
#test<-fst::read.fst(here("data","connect_matrix",paste0(1,"_connect.fst")))
#head(test)

head(MegaData)
MegaData$bvk_fin<-bvk_calculated$bvk_calc

# #--test the code
# stock_num<-1
# bvk_equi <- func_evaluateMPA(stock_num, Cleanmegacell,biol_data,distance_mat_full,MegaData)
# head(bvk_equi)
# dim(bvk_equi)
# #--store the results and collate later
# collate_bvk_equi<-list()
# nstock<-2#dim(MegaData)[1]
# for (stock_num in 1:nstock){ 
#   collate_bvk_equi[[stock_num]] <- func_evaluateMPA(stock_num, Cleanmegacell,biol_data,distance_mat_full,MegaData)$bvk_equi
# }
# collate_bvk_equi_merged <- data.frame(do.call("cbind",collate_bvk_equi))
# colnames(collate_bvk_equi_merged)<-MegaData$stockid[1:nstock]
# head(collate_bvk_equi_merged)
# dim(collate_bvk_equi_merged)
# #ok, done testing. Now, do parallel compute

#--parallel compute
#This code evaluates the biomass change for each stock
nstock<-dim(MegaData)[1]
ptm <- proc.time()
registerDoParallel(detectCores()/2)
stock_include<-c(1,3,5)#which(MegaData$INCLUDE==1)#c(1,3,5)#which(MegaData$INCLUDE==1)#c(1,3,5)#edit this. include only stocks for the analysis
#collate_bvk_equi_merged <- foreach(stock_num=stock_include[1:length(stock_include)], .combine='cbind') %dopar% {
collate_bvk_equi_merged <- foreach(stock_num=stock_include, .combine='cbind') %dopar% {
  func_evaluateMPA(stock_num, Cleanmegacell,biol_data,distance_mat_full,MegaData)$bvk_equi
}
doParallel::stopImplicitCluster()
(proc.time() - ptm)/60 #check process time in minutes
colnames(collate_bvk_equi_merged)<-MegaData$stockid[stock_include]
head(collate_bvk_equi_merged)
dim(collate_bvk_equi_merged)

#plot
#collate_bvk_equi_merged[is.na(collate_bvk_equi_merged)] <- 0 #never do this.
collate_bvk_equi_merged[collate_bvk_equi_merged>1]<-1 #cap to 1
plotme<-rowMeans(collate_bvk_equi_merged,na.rm = TRUE)
plot(plotme)

plot(plotme,BvKperCell)

CleanCoordmegacell_EEZ_wMPA %>% ggplot(aes(x=lon,y=lat,fill=plotme)) + scale_fill_viridis_c(limits = c(0, max(plotme))) + geom_raster() #ok, great

#--to do
#calculation of tourism values can be incorporated in the function

# biomass_withMPA <- foreach(i=1:2, .combine='rbind') %dopar% {
#   EvaluateMPA <- MPAcell #this is inside since we need to close each pixel and put it back
#   EvaluateMPA[nonMPAposition[i]]<-1
#   
#   # for (t in 1:20){
#   #   biom_diff <- distance_mat_full_prK[biom_diff] %>% mutate(Bdist=B*biom_prop, Growth=biom_prop*r*B) %>% group_by(pos2) %>% dplyr::select(pos2,Bdist,Growth) %>% summarize(B_add=sum(Bdist),G_add=sum(Growth)) %>%
#   #     mutate(B=((1-(E*(1-EvaluateMPA)))*B_add)+pmax(G_add*(1-(biom_diff$B/KperCell)),0)) %>% dplyr::rename(pos1 = pos2) %>% dplyr::select(pos1,B) %>% as.data.table()
#   # }
#   sum(biom_diff$B)
# }
# doParallel::stopImplicitCluster()
# (proc.time() - ptm)/60 #check process time in minutes
# 
# plot(biomass_withMPA) #ok, looks good