#PLD parameters

#1. CHECK THE PLD DISTRIBUTIONS IF WE CAN CONSTRAIN IT (BIMODAL)

#load Ramesh data
gc()
rm(list = ls())
library(here)
library(dplyr)
library(tidyverse)

#load our species list
MegaData <- readRDS(file = "/Users/ren/Documents/GitHub/FoodProvison_SupportFiles/Code Food Provision MPA/MegaData_Ray.rds")
head(MegaData)
MagaData_filter <-MegaData %>% filter(INCLUDE==1)
FinalSpeciesList<- unique(MagaData_filter$SciName) %>% data.frame() %>% rename("specieslist"=".")

head(FinalSpeciesList)
dim(FinalSpeciesList)

#-- load mobility data with family info 
mobility <- read.csv(here("data","mobility_data_paper - data.csv"))
mobility_filter <- mobility %>% select(SciName,family,m_index,movement_keyword) 
head(mobility_filter)
table(mobility_filter$movement_keyword)

pld_ramesh <- read.csv(here("data","pld_ramesh.csv"))
head(pld_ramesh)
dim(pld_ramesh)

#--filter entries with available pld info. Get the mean of plds because there ar duplicates.
pld_ramesh_filter <- pld_ramesh %>% filter(!is.na(LarvaeDuration)) %>% select(scientific,LarvaeDuration) %>% group_by(scientific) %>%
  summarise(mean_pld=mean(LarvaeDuration)) %>% rename("SciName"="scientific")
head(pld_ramesh_filter)
dim(pld_ramesh_filter) #233 non-zero pld entries, 265 with zero pld entries.

#merge here: "pld_ramesh_filter" and "mobility_filter" using SciName column
pld_ramesh_join <- merge(x = pld_ramesh_filter, y = mobility_filter, by = "SciName", all.x = TRUE)

#--plot pld of ramesh. Check if this is bimodal. hmmmm. Not quite.
ggplot(pld_ramesh_filter, aes(x=mean_pld)) + geom_histogram(binwidth=5) + labs(title="Fish and inverst (Ramesh et al.)",
                                                                               x ="PLD (days)", y = "Count")

#table(pld_ramesh_filter$EggFloating)
#unique(pld_ramesh_filter$scientific) %>% length()

#check how many of Ramesh data matches our species list
match_ramesh <- pld_ramesh_join %>% filter(SciName %in% FinalSpeciesList$specieslist) 
dim(match_ramesh) #65 MATCHED

#--matched with non-zero pld. I'm using the long format to avoid errors in averaging.
pld_ramesh_nonzero <- pld_ramesh %>% filter(!is.na(LarvaeDuration)) %>% filter(LarvaeDuration>0) %>% select(scientific,LarvaeDuration) %>% group_by(scientific) %>%
  summarise(mean_pld=mean(LarvaeDuration)) %>% filter(scientific %in% FinalSpeciesList$specieslist)
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

ggplot(pld_marshall_filter, aes(x=PlanktonicTime)) + geom_histogram(binwidth=5) + labs(title="Inverts (Marshall et al.)",
                                                                                        x ="PLD (days)", y = "Count")

match_marshall<-pld_marshall_filter %>% filter(UpdatedName %in% FinalSpeciesList) %>% select(UpdatedName,PlanktonicTime)
unique(match_marshall$UpdatedName) %>% length()


###-- combine the two datasets
#"pld_ramesh_filter" and "
head(pld_marshall_filter)
pld_ramesh_merge <- pld_ramesh_filter %>% mutate(database="Fish and Inverts (Ramesh)")
pld_marshall_merge <- pld_marshall_filter %>% select(UpdatedName,PlanktonicTime) %>% rename("SciName"="UpdatedName","mean_pld"="PlanktonicTime") %>% mutate(database="Inverts (Marshall)")

merge_pld <- rbind(pld_ramesh_merge,pld_marshall_merge)
ggplot(merge_pld , aes(x=mean_pld, color=database)) +
  geom_histogram(fill="white", alpha=0.5, position="identity",binwidth=5)
