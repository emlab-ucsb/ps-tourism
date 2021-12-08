#This code aims to estimate the PLD parameters for species we do not have data on

#to do:
#1. CHECK THE PLD DISTRIBUTIONS IF WE CAN CONSTRAIN IT (BIMODAL)

gc()
rm(list = ls())
library(here)
library(dplyr)
library(tidyverse)

#load our species list
MegaData <- readRDS(file = "/Users/ren/Documents/GitHub/FoodProvison_SupportFiles/Code Food Provision MPA/MegaData_Ray.rds")
head(MegaData)
MagaData_filter <-MegaData %>% filter(INCLUDE==1)
FinalSpeciesList<- unique(MagaData_filter$SciName) %>% data.frame() %>% dplyr::rename("SciName"=".")

head(FinalSpeciesList)
dim(FinalSpeciesList)

#save species list as csv file
write.csv(FinalSpeciesList, here("data","species_list.csv"))

#-- load mobility data with family info 
mobility <- read.csv(here("data","mobility_data_paper - data.csv"))
mobility_filter <- mobility %>% select(SciName,family,m_index,movement_keyword) %>% filter(SciName %in% FinalSpeciesList$SciName)
head(mobility_filter)
dim(mobility_filter)

#fix mispelled entries
mobility_filter <- mobility_filter %>% mutate(movement_keyword=replace(movement_keyword, movement_keyword=="migrtory", "migratory"),
                                              movement_keyword=replace(movement_keyword, movement_keyword=="migratory ", "migratory")) 
table(mobility_filter$movement_keyword)

#--load Ramesh data
pld_ramesh <- read.csv(here("data","pld_ramesh.csv"))
head(pld_ramesh)
dim(pld_ramesh)

#--filter entries with available pld info. Get the mean of plds because there ar duplicates.
pld_ramesh_filter <- pld_ramesh %>% filter(!is.na(LarvaeDuration)) %>% select(scientific,LarvaeDuration) %>% group_by(scientific) %>%
  summarise(mean_pld=mean(LarvaeDuration)) %>% dplyr::rename("SciName"="scientific")
head(pld_ramesh_filter)
dim(pld_ramesh_filter) #265 with zero pld entries, 233 non-zero pld entries

#merge here: "pld_ramesh_filter" and "mobility_filter" using SciName column
pld_ramesh_join <- merge(x = pld_ramesh_filter, y = mobility_filter, by = "SciName", all.x = TRUE)

#--plot pld of ramesh. Check if this is bimodal. hmmmm. Not quite.
ggplot(pld_ramesh_filter, aes(x=mean_pld)) + geom_histogram(binwidth=5) + labs(title="Fish and inverst (Ramesh et al.)",
                                                                               x ="PLD (days)", y = "Count")

#table(pld_ramesh_filter$EggFloating)
#unique(pld_ramesh_filter$scientific) %>% length()

#check how many of Ramesh data matches our species list
match_ramesh <- pld_ramesh_join %>% filter(SciName %in% FinalSpeciesList$SciName) 
dim(match_ramesh) #65 MATCHED

#--matched with non-zero pld. I'm using the long format to avoid errors in averaging.
pld_ramesh_nonzero <- pld_ramesh %>% filter(!is.na(LarvaeDuration)) %>% filter(LarvaeDuration>0) %>% select(scientific,LarvaeDuration) %>% group_by(scientific) %>%
  summarise(mean_pld=mean(LarvaeDuration)) %>% filter(scientific %in% FinalSpeciesList$SciName)
dim(pld_ramesh_nonzero)

#--same as above, but shortcut calculation
pld_ramesh_nonzero_v2 <- match_ramesh %>% filter(mean_pld>0)
dim(pld_ramesh_nonzero_v2) #57 with non-zero entries.

plot(pld_ramesh_nonzero$mean_pld,pld_ramesh_nonzero_v2$mean_pld)#perfect!

#ok, now use match_ramesh



##-- pld data from Marshall
pld_marshall <- read.csv(here("data","pld_marshall.csv"))
head(pld_marshall)
dim(pld_marshall) #806 entries
pld_marshall_filter <- pld_marshall %>% filter(!is.na(PlanktonicTime))
head(pld_marshall_filter)
dim(pld_marshall_filter) #254 entries

ggplot(pld_marshall_filter, aes(x=PlanktonicTime)) + geom_histogram(binwidth=5) + labs(title="Inverts (Marshall et al.)", x ="PLD (days)", y = "Count")

#using the updated name
match_marshall <- pld_marshall_filter %>% filter(UpdatedName %in% FinalSpeciesList$SciName) %>% select(UpdatedName,PlanktonicTime)
unique(match_marshall$UpdatedName) %>% length()

#using the original name
match_marshall_v2 <- pld_marshall_filter %>% filter(OriginalName  %in% FinalSpeciesList$SciName) %>% select(UpdatedName,PlanktonicTime)
unique(match_marshall$UpdatedName) %>% length()

###-- combine the two datasets
#"pld_ramesh_filter" and "pld_marshall_filter"
head(pld_marshall_filter)
pld_ramesh_merge <- pld_ramesh_filter %>% mutate(database="Fish and Inverts (Ramesh)")
pld_marshall_merge <- pld_marshall_filter %>% select(UpdatedName,PlanktonicTime) %>% dplyr::rename("SciName"="UpdatedName","mean_pld"="PlanktonicTime") %>% mutate(database="Inverts (Marshall)")

merge_pld <- rbind(pld_ramesh_merge,pld_marshall_merge)
ggplot(merge_pld , aes(x=mean_pld, color=database)) +
  geom_histogram(fill="white", alpha=0.5, position="identity",binwidth=5)

head(merge_pld)
dim(merge_pld)

#--check family
head(mobility_filter)
dim(mobility_filter)
table(mobility_filter$family)

#--add the family information in the pld data using left join
pld_with_family<-left_join(mobility_filter,merge_pld, by="SciName")
head(pld_with_family)
dim(pld_with_family)

pld_with_family %>% group_by(family) %>% summarize(family_mean_pld=mean(mean_pld,na.rm=TRUE), n=n()) 
pld_with_family %>% group_by(family) %>% filter(!is.na(mean_pld)) %>% summarize(family_mean_pld=mean(mean_pld,na.rm=TRUE), n_mean=n()) 

#--Random forest model using R