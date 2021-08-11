#Pristine seas tourism-MPA model
#Author: Reniel Cabral
#Last Edit: 18 May 2021
#3 August 2021

#To do:


#Biomass model - add MPAs, how much biomass will change?
#1st: is to define current B/Bmsy and equilibrium B/Bmsy given F/Fmsy
#we do not care about catch transition but we care about B/Bmsy transition.
#steady state???

#2nd: 
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

#load EEZ mollweide file
Final_EEZ_file_with_names<-readRDS("/Users/ren/Documents/GitHub/MPAFishFlow/Final_EEZ_file_with_names.rds")
head(Final_EEZ_file_with_names)
#Plot World
Final_EEZ_file_with_names %>% ggplot(aes(x=lon,y=lat,fill=1)) + geom_raster()

land_shp_moll<-readRDS(file = "/Users/ren/Documents/CODES/FoodProvision/land_shp_moll.rds")
MegaData<-readRDS(file = "/Users/ren/Documents/GitHub/FoodProvison_SupportFiles/Code Food Provision MPA/MegaData_Ray.rds")
Cleanmegacell<-readRDS(file = "/Users/ren/Documents/GitHub/FoodProvison_SupportFiles/Code Food Provision MPA/Cleanmegacell_mollweide.rds")
CleanCoordmegacell<-readRDS(file = "/Users/ren/Documents/GitHub/FoodProvison_SupportFiles/Code Food Provision MPA/CleanCoordmegacell_mollweide.rds")
#this can be used to calculate the % of species range in protected area
KprotectedPerCell_Library<-readRDS(file = "/Users/ren/Documents/GitHub/FoodProvison_SupportFiles/Code Food Provision MPA/KprotectedPerCell_Library_mollweide.rds")
MPA_coord<-readRDS(file="/Users/ren/Documents/GitHub/FoodProvison_SupportFiles/Code Food Provision MPA/MPA_coord_mollweide.rds")
MPA_coord$MPA<-1 
head(MPA_coord)
dim(MPA_coord)

#get MPA positions
CleanCoordmegacell_MPA<-left_join(CleanCoordmegacell,MPA_coord,by=c("lon","lat"))
head(CleanCoordmegacell_MPA)
dim(CleanCoordmegacell_MPA)
#positions of 1s
MPAposition<-which(CleanCoordmegacell_MPA$MPA==1)
head(MPAposition)
length(MPAposition)

#join eez and clean coord file with MPA
CleanCoordmegacell_EEZ_wMPA<-left_join(CleanCoordmegacell_MPA,Final_EEZ_file_with_names,by=c("lon","lat"))
#convert NA to 0, then plot MPA file to check
CleanCoordmegacell_EEZ_wMPA$MPA[is.na(CleanCoordmegacell_EEZ_wMPA$MPA)] <- 0
head(CleanCoordmegacell_EEZ_wMPA)
dim(CleanCoordmegacell_EEZ_wMPA)

#PLOT MPAs
CleanCoordmegacell_EEZ_wMPA %>% filter(MPA==1)  %>%  ggplot(aes(x=lon,y=lat,fill=1)) + geom_raster() #ok, great

#we could subset eezs if we want to
EEZposition<-which(CleanCoordmegacell_EEZ_wMPA$sovereign1=="Indonesia")
length(EEZposition)

#Permutations
ncell<-10
perms<-cbind(rep(1:ncell, each=ncell),rep(1:ncell, ncell)) #ok, this is how we could do permutation

#Now, check using actual data
#add sequence id
CleanCoordmegacell_EEZ_wMPA$ID <- seq.int(nrow(CleanCoordmegacell_EEZ_wMPA))
ncell<-max(CleanCoordmegacell_EEZ_wMPA$ID)

#create a biomass matrix library
Biom_matrix<-cbind(rep(1:ncell, each=ncell),rep(1:ncell, ncell)) 
##opps. not working! this operation takes lots of memory!


#Toy model
#Empirically derived parameters
Qd0 <- 100 #current number of dives
P0 <- 50 #current price per dive in USD
C0 <- 150 #choke price in USD
alpha <- 0.5
beta <- 0.5
X0 <- 10 #dive price (USD) where no company will offer their service
nu <- 1.1

#Model intermediate output
deltaB <- 20 #change in biomass
deltaS <- 10 #change in species diversity metric

#Parameter calculations
#we need a and b to compute mu
(a <- (C0*Qd0)/(C0-P0))
(b <- Qd0/(C0-P0))
(mu <- nu*(a-(b*P0))) #assume that MPA will increase the demand for diving by 10%.

(e <- (a-(b*P0))/(P0-X0)) #Slope of the supply curve
(c <- (((b*P0)-a)*X0)/(P0-X0)) #Dive quantity supplied by the industry when the price of diving is zero

(P1 <- (a-c+mu+(alpha*deltaB)+(beta*deltaS))/(e+b)) #Price per dive at site i when the site is converted to an MPA
(Qd1 <- a-(b*P1)+mu+(alpha*deltaB)+(beta*deltaS)) #Number of dives at site i when the site is converted to an MPA
(C1 <- (a+mu+(alpha*deltaB)+(beta*deltaS))/b)

##Tourism model output
#Change in tourism revenue at site i
(P1*Qd1) - (P0*Qd0)

#Change in counsumer surplus
(0.5*(C1-P1)*Qd1) - (0.5*(C0-P0)*Qd0)

#Change in consumer + producer surplus
(Qd0*(C1-C0)) + (0.5*(Qd1-Qd0)*(C1-C0))
