#This code aims to estimate the PLD parameters for species we do not have data on
#I think the best strategy is to just use family-level info to impute PLD for other species given no clear patterns of PLD with other variables.
#to do:
#1. CHECK THE PLD DISTRIBUTIONS IF WE CAN CONSTRAIN IT (BIMODAL)

gc()
rm(list = ls())
library(here)
library(dplyr)
library(tidyverse)
library(rfishbase)
library(stringr)

##--load our species list
MegaData <- readRDS(file = "/Users/ren/Documents/GitHub/FoodProvison_SupportFiles/Code Food Provision MPA/MegaData_Ray.rds")
MagaData_filter <-MegaData %>% filter(INCLUDE==1)
FinalSpeciesList<- unique(MagaData_filter$SciName) %>% data.frame() %>% dplyr::rename("SciName"=".")
head(FinalSpeciesList)
dim(FinalSpeciesList)
#- we have 811 unique species

#save species list as csv file
write.csv(FinalSpeciesList, here("data","species_list.csv"))

#-- load mobility data with family info and fix mispelled entries
mobility <- read.csv(here("data","mobility_data_paper - data.csv")) %>% 
  mutate(movement_keyword=replace(movement_keyword, movement_keyword=="migrtory", "migratory"),
  movement_keyword=replace(movement_keyword, movement_keyword=="migratory ", "migratory")) 
table(mobility$movement_keyword)
dim(mobility)
write.csv(mobility, here("data","mobility_data_tourism.csv"))

#filter mobility data to include only our species list
mobility_filter <- mobility %>% dplyr::select(SciName,family,m_index,movement_keyword) %>% filter(SciName %in% FinalSpeciesList$SciName)
head(mobility_filter)
dim(mobility_filter)

table((mobility_filter$family))
table((mobility_filter$movement_keyword))

#fix mispelled entries
mobility_filter <- mobility_filter %>% mutate(movement_keyword=replace(movement_keyword, movement_keyword=="migrtory", "migratory"),
                                              movement_keyword=replace(movement_keyword, movement_keyword=="migratory ", "migratory")) 
table(mobility_filter$movement_keyword)

#load the species id matching
spnamelookup<-read.csv(here("data","aquamaps_spp_ref_revised_2022.csv"))
spnamelookup<-as.data.frame(spnamelookup)
head(spnamelookup)
dim(spnamelookup) 

spnamelookup_v1 <- spnamelookup %>% dplyr::select(resolved_scientific_name,family) %>% dplyr::rename("Family_aqua"="family", "SciName"="resolved_scientific_name") %>% unique()
spnamelookup_v2 <- spnamelookup %>% dplyr::select(aquamaps_sci_name,family) %>% dplyr::rename("Family_aqua"="family", "SciName"="aquamaps_sci_name") %>% unique()
spnamelookup_v3 <- spnamelookup %>% dplyr::select(worms_sci_name,family) %>% dplyr::rename("Family_aqua"="family", "SciName"="worms_sci_name") %>% unique()
spnamelookup_v4 <- spnamelookup %>% dplyr::select(eol_sci_name,family) %>% dplyr::rename("Family_aqua"="family", "SciName"="eol_sci_name") %>% unique()
spnamelookup_v5 <- spnamelookup %>% dplyr::select(col_sci_name,family) %>% dplyr::rename("Family_aqua"="family", "SciName"="col_sci_name") %>% unique()
spnamelookup_v6 <- spnamelookup %>% dplyr::select(gbif_sci_name,family) %>% dplyr::rename("Family_aqua"="family", "SciName"="gbif_sci_name") %>% unique()
spnamelookup_v7 <- spnamelookup %>% dplyr::select(itis_sci_name,family) %>% dplyr::rename("Family_aqua"="family", "SciName"="itis_sci_name") %>% unique()

#family database of aquamap
familydb_aquamap <- rbind(spnamelookup_v1,spnamelookup_v2,spnamelookup_v3,spnamelookup_v4,spnamelookup_v5,spnamelookup_v6,spnamelookup_v7) %>% unique()
head(familydb_aquamap)

#family database of fishbase
familydb_fishbase <- rfishbase::load_taxa() %>% dplyr::select(Species, Family) %>% dplyr::rename("SciName"="Species","Family_fishbase"="Family")
head(familydb_fishbase)

pld_ramesh <- read.csv(here("data","pld_ramesh.csv"))
head(pld_ramesh)
dim(pld_ramesh)

#--filter entries with available pld info. Sum larval and egg durations as per Ramesh et al.
pld_ramesh_filter <- pld_ramesh %>% filter(!is.na(LarvaeDuration) & !is.na(LarvaeDuration)) %>% rowwise() %>% mutate(PLD=sum(LarvaeDuration,EggDuration,na.rm=T)) %>% 
  dplyr::select(scientific,PLD) %>% dplyr::rename("SciName"="scientific") %>% mutate(database="Fish and Inverts (Ramesh)") %>% 
  mutate(SciName=replace(SciName, SciName=="Auxis thazzard", "Auxis thazard"))
head(pld_ramesh_filter)
dim(pld_ramesh_filter) #361 entries

##-- pld data from Marshall
pld_marshall <- read.csv(here("data","pld_marshall.csv"))
head(pld_marshall)
dim(pld_marshall) #806 entries
pld_marshall_filter <- pld_marshall %>% filter(!is.na(PlanktonicTime)) %>% dplyr::select(UpdatedName,PlanktonicTime, Family) %>%
  dplyr::rename("SciName"="UpdatedName", "PLD"="PlanktonicTime") %>% mutate(database="Inverts (Marshall)")
head(pld_marshall_filter)
dim(pld_marshall_filter) #254 entries

#make a family database out of marshall et al. db
familydb_marshall <- pld_marshall_filter %>% dplyr::select(SciName,Family) %>% dplyr::rename("Family_marshall"="Family") 

pld_marshall_filter <- pld_marshall_filter %>% dplyr::select(SciName, PLD, database)

##other databases
#Ren's manual DB
pld_ren <- read.csv(here("data","pld_ren.csv")) %>% mutate(database="Manual input") %>% select(SciName, PLD, database)
head(pld_ren)

#pld_fontoura
pld_fontoura <- read.csv(here("data","pld_fontoura.csv")) %>% select(Genus_species,PLD_mean) %>% dplyr::rename("SciName"="Genus_species", "PLD"="PLD_mean") %>% mutate(database="Fontoura et al. 2022") 
dim(pld_fontoura)
#merge ramesh and marshall and synthesize
pld_merged <- rbind(pld_ramesh_filter,pld_marshall_filter,pld_ren,pld_fontoura) %>% dplyr::select(SciName, PLD) %>% group_by(SciName) %>% summarise(mean_pld=mean(PLD), sd_pld=sd(PLD), n_mean=n())
head(pld_merged)
dim(pld_merged)


#how many matches per database?
left_join(mobility_filter,pld_ramesh_filter,by="SciName") %>% filter(!is.na(PLD)) %>% select(SciName) %>% unique() %>% count()
left_join(mobility_filter,pld_marshall_filter,by="SciName") %>% filter(!is.na(PLD)) %>% select(SciName) %>% unique() %>% count()
left_join(mobility_filter,pld_fontoura,by="SciName") %>% filter(!is.na(PLD)) %>% select(SciName) %>% unique() %>% count()

#na to 0
pld_merged[is.na(pld_merged)] <- 0

#manual check
pld_ramesh_filter %>% filter(SciName=="Centropomus mullus") #ok

#merge with our species list
mobility_pld<-left_join(mobility_filter,pld_merged, by="SciName")
head(mobility_pld)
dim(mobility_pld)#811

#matched species
mobility_pld %>% filter(!is.na(mean_pld)) #103 matched!!!

#test<-merge(mobility_filter,pld_merged, by="SciName", all = TRUE)
#------


#Ok, mobility_pld is our raw file


#------


#species unmatched
mobility_pld %>% filter(is.na(mean_pld)) %>% dim() #708 species with no data

#check the families of those species with no data
sp_nodata <- mobility_pld %>% filter(is.na(mean_pld))
table(sp_nodata$family)
family_nodata <- unique(sp_nodata$family)
family_nodata #176 families with no data

#get family-averages from our main file???
pld_merged_family_s1 <- rbind(pld_ramesh_filter,pld_marshall_filter,pld_ren,pld_fontoura) 
pld_merged_family_s2 <- left_join(pld_merged_family_s1,familydb_aquamap,by="SciName")
pld_merged_family_s3 <- left_join(pld_merged_family_s2,familydb_fishbase,by="SciName")
pld_merged_family_s4 <- left_join(pld_merged_family_s3,familydb_marshall,by="SciName")
head(pld_merged_family_s4)
dim(pld_merged_family_s4)


#read my manually-inputed family names
pld_familynames_RC <- read.csv(here("data","pld_merged_family_add_edited_RC.csv")) %>% select(SciName,Family_manual) %>% 
  mutate(SciName=replace(SciName, SciName=="Auxis thazzard", "Auxis thazard")) %>% dplyr::filter(!is.na(Family_manual)) %>% unique()
dim(pld_familynames_RC)

pld_familynames_fontoura <- read.csv(here("data","pld_fontoura.csv")) %>% select(Genus_species,Family) %>%  dplyr::rename("SciName"="Genus_species", "Family_manual"="Family") %>%
  mutate(Family_manual=replace(Family_manual, SciName=="Sparisoma viride", "Scaridae")) %>%
  mutate(Family_manual=replace(Family_manual, SciName=="Epinephelus merra", "Serranidae")) 
  
pld_familynames_fontoura$Family_manual <- str_to_sentence(pld_familynames_fontoura$Family_manual)
head(pld_familynames_fontoura)
#Sparisoma viride should be Scaridae instead of Labridae
#Epinephelus merra should be Serranidae instead of Epinephelidae

pld_familynames_db <- rbind(pld_familynames_RC,pld_familynames_fontoura) %>% unique()
dim(pld_familynames_db)
table(pld_familynames_db$SciName)

pld_merged_family_add_edited_db <- left_join(pld_merged_family_s4,pld_familynames_db,by="SciName")
dim(pld_merged_family_add_edited_db)
head(pld_merged_family_add_edited_db)

# #save the file and manually add family name
# write.csv(pld_merged_family_s4, here("data","pld_merged_family_add.csv"))
# 
# #load the filled-out pld file with family information
# pld_merged_family_add_edited_RC <- read.csv(here("data","pld_merged_family_add_edited_RC.csv"))
# head(pld_merged_family_add_edited_RC)


#save full pld data as training dataset
pld_full_training <- pld_merged_family_add_edited_db %>% select(SciName,PLD,database,Family=Family_manual) %>% mutate(part_of_stock_list=SciName %in% FinalSpeciesList$SciName)

pld_full_training_unique_meanpld <- pld_full_training %>% select(!database) %>% group_by(SciName, Family, part_of_stock_list) %>% summarise(mean_pld=mean(PLD))
head(pld_full_training_unique_meanpld)
dim(pld_full_training_unique_meanpld)

table(pld_full_training$SciName)
unique(pld_full_training$SciName)
sum(pld_full_training$part_of_stock_list==T)
write.csv(pld_full_training, here("data","pld_full_training.csv"))
write.csv(pld_full_training, "/Users/ren/Documents/GitHub/mpa-fish-flows/data/pld/pld_full_training.csv")
write.csv(pld_full_training, "/Users/ren/Documents/GitHub/mpa-fish-flows/data/pld/pld_full_training_unique_meanpld.csv")

family_nodata #family info of species with no pld data
imputed_PLD_family <- pld_merged_family_add_edited_db %>% group_by(Family_manual) %>% summarise(mean_pld_family=mean(PLD), sd_pld_family=sd(PLD), n_mean_family=n()) %>% filter(Family_manual %in% family_nodata) %>% dplyr::rename("family"="Family_manual") 
head(imputed_PLD_family)

family_nodata[!(family_nodata %in% imputed_PLD_family$family)] #families that need more PLD data

#ok, this is the data frame
mobility_pld_imputed <- left_join(mobility_pld,imputed_PLD_family,by="family")

mobility_pld_imputed <- mobility_pld_imputed %>% mutate(pld = ifelse(is.na(mean_pld), mean_pld_family,mean_pld))

head(mobility_pld_imputed)

ggplot(mobility_pld_imputed, aes(x=pld)) + geom_histogram(binwidth=5) + labs(title="PLD distribution",
                                                                               x ="PLD (days)", y = "Count")

ggplot(mobility_pld_imputed, aes(x=m_index, y=pld)) + geom_point()
mean(mobility_pld_imputed$pld, na.rm=T)

#for now, if NA, add a PLD of 1 () 
mobility_pld_imputed <- mobility_pld_imputed %>% mutate(pld=replace_na(pld,mean(mobility_pld_imputed$pld, na.rm=T)))

write.csv(mobility_pld_imputed, here("data","mobility_pld_imputed.csv"))

#add other parameters to our file
pld_raw <- mobility_pld_imputed %>% select(-pld)
head(pld_raw)

#-- load stock distribution
transformed_stockdistrib <- readRDS(here("data","transformed_stockdistrib.rds"))
head(transformed_stockdistrib)
dim(transformed_stockdistrib) #149547 by 1155
geog_range <- transformed_stockdistrib[6:1155] %>% colSums(na.rm=T)
geog_range_v2 <- as.data.frame(geog_range)
geog_range_v2$stockid <- row.names(geog_range_v2)

geog_range_v2$geog_range<-geog_range_v2$geog_range*50*50
head(geog_range_v2) 

#--- load MegaData --- add biomass density
MegaData<-readRDS(here("data","MegaData_Ray.rds"))
MegaData_filtered <- MegaData %>% filter(INCLUDE==1) %>% dplyr::select(stockid,SciName,r_fin,Kfin)
head(MegaData_filtered)
dim(MegaData_filtered)

MegaData_filtered_v2 <- left_join(MegaData_filtered,geog_range_v2,by="stockid")
dim(MegaData_filtered_v2)
head(MegaData_filtered_v2)

MegaData_filtered_v3 <- MegaData_filtered_v2 %>% group_by(SciName) %>% summarise(r_fin=mean(r_fin), Kfin=sum(Kfin),geog_range=sum(geog_range))
head(MegaData_filtered_v3)
dim(MegaData_filtered_v3)

pld_raw_merged <- left_join(pld_raw,MegaData_filtered_v3,by="SciName")
dim(pld_raw_merged)
head(pld_raw_merged)

write.csv(pld_raw_merged, "/Users/ren/Documents/GitHub/mpa-fish-flows/data/pld/pld_raw_merged.csv")
