#Pristine seas tourism-MPA model
#Author: Reniel Cabral
#Last Edit: 18 May 2021
#3 August 2021

#To do:
#1. Goal: Biomass model. Adding an MPA, how much biomass will increase at a site?
#2. What I found so far: Maybe challenging to model if per species. For the biomass, we do not need the identity of the species.
#We have a separate model for the biodiversity change.
#3. Simplification --- Biomass as a single species. Need to check how to model dispersal??? For growth, we could get the average r.
#average movement also? well, this assumption cannot capture site-specific differences but maybe we do not need to be accurate.
#4. Add MPA code. Add the current MPA and implement a code where each area is closed and calculate biomass change.

#Biomass model - add MPAs, how much biomass will change?
#1st: define current B/Bmsy and equilibrium B/Bmsy given F/Fmsy
#we do not care about catch transition but we care about B/Bmsy transition.

#pos1 is the source

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

#--extract K per cell
Cleanmegacell<-readRDS(file = "/Users/ren/Documents/GitHub/FoodProvison_SupportFiles/Code Food Provision MPA/Cleanmegacell_mollweide.rds")
head(Cleanmegacell)
dim(Cleanmegacell)
colSums(Cleanmegacell)
MegaData<-readRDS(file = "/Users/ren/Documents/GitHub/FoodProvison_SupportFiles/Code Food Provision MPA/MegaData_Ray.rds")
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

# #Option 2 for computing the distance matrix is slow. Option 1 is the best.
# n_pixel<-100 #placeholder
# dispersal_distance_lim<-110 #km #for now 100 but realistically 1000?
# distance_mat<-list()
# t <- 1
# for (i in 1:n_pixel){
#   for (j in i+1:n_pixel){ #i'll remove the distance of pixel i from itself which is zero
#     distance <- sqrt((biol_data[i,1]-biol_data[j,1])^2+(biol_data[i,2]-biol_data[j,2])^2)/1000
#     if( distance <= dispersal_distance_lim ){
#       distance_mat[[t]] <- c(i,j,distance)
#       t <- t+1
#     }
#   }
# }
# #
# distance_mat_merged2 <- do.call("rbind",distance_mat)
# head(distance_mat_merged2)
# dim(distance_mat_merged2)

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

# # #--This code is working and is based on data.frame! But it seems that data.table is faster. So I will comment this.
# cores<-detectCores()
# registerDoParallel(cores-1)
# ptm <- proc.time()
# for (t in 1:30){
#   biom <- left_join(distance_mat_full_prop, biom, by = "pos1") %>% mutate(Bdist=B*biom_prop) %>% group_by(pos2) %>% select(pos2,Bdist) %>% summarize(B=sum(Bdist))
#   colnames(biom) <- c("pos1","B") #now, make the output biomass as input biomass to our next iteration.
# }
# stopImplicitCluster()
# (proc.time() - ptm)/60 #check process time in minutes
# head(biom)

# #--test how fast is data.table
# biom <- data.table(biom)
# distance_mat_full_prop <- data.table(distance_mat_full_prop)
# head(biom)
# head(distance_mat_full_prop)
# 
# setkey(distance_mat_full_prop,pos1)
# setkey(biom,pos1)
# 
# test1<-distance_mat_full_prop[biom] #ok, this is a merge function by "pos1" variable
# head(test1)
# dim(test1)
# 
# test2<-merge(distance_mat_full_prop,biom, all.x=TRUE)
# head(test2)
# dim(test2)
# 
# ptm <- proc.time()
# Result2 <- merge(distance_mat_full_prop,biom, all.x=TRUE) %>% mutate(Bdist=B*biom_prop) %>% group_by(pos2) %>% dplyr::select(pos2,Bdist) %>% summarize(B=sum(Bdist))
# (proc.time() - ptm)/60 #check process time in minutes
# head(Result2)
# 
# testme<-distance_mat_full_prop[biom] %>% mutate(Bdist=B*biom_prop) %>% group_by(pos2) %>% dplyr::select(pos2,Bdist) %>% summarize(B=sum(Bdist))
# head(testme)
# colnames(testme) <- c("pos1","B")
# #--ok, data.table is much faster for biomass disffusion model! Nearly double the speed. Let us model using data.table!!!

head(biol_data)
E <- biol_data$E #we can make this dynamic. i.e., as MPA size increases, E changes.
MPAcell <- biol_data$MPA
rK<-biol_data %>% dplyr::select(pos1,r,K) %>% as.data.table()
setkey(rK,pos1)
setkey(distance_mat_full_prop,pos1)
distance_mat_full_prK<-distance_mat_full_prop[rK]
#distance_mat_full_prKE<-merge(distance_mat_full_prop,rKE, all.x=TRUE)
head(distance_mat_full_prK)

# #check the file
# distance_mat_full_prK %>% group_by(pos1) %>% summarise(propor=sum(biom_prop)) %>% head()
# 
# #--biomass in next time step = biomass with diffusion - harvest + growth in the form of larval dispersal 
# biom_diff <- biom
# 
# ptm <- proc.time()
# for (t in 1:30){
#   #  biom_test <- merge(distance_mat_full_prKE,biom_diff, all.x=TRUE) %>% mutate(Bdist=B*biom_prop, Growth=biom_prop*r*B*(1-(B/K))) %>% group_by(pos2) %>% select(pos2,Bdist,Growth) %>% summarize(B_add=sum(Bdist),G_add=sum(Growth)) %>% 
#   #    mutate(B=((1-E)*B_add)+G_add) %>% dplyr::rename(pos1 = pos2) %>% select(pos1,B) %>% as.data.table()
#   
#   biom_diff <- distance_mat_full_prK[biom_diff] %>% mutate(Bdist=B*biom_prop, Growth=biom_prop*r*B*(1-(B/K))) %>% group_by(pos2) %>% dplyr::select(pos2,Bdist,Growth) %>% summarize(B_add=sum(Bdist),G_add=sum(Growth)) %>% 
#     mutate(B=((1-(E*(1-MPAcell)))*B_add)+G_add) %>% dplyr::rename(pos1 = pos2) %>% dplyr::select(pos1,B) %>% as.data.table()
#   
#   #colnames(biom_diff) <- c("pos1","B") #now, make the output biomass as input biomass to our next iteration.
#   #biom_diff <- data.table(biom_diff)
# }
# (proc.time() - ptm)/60 #check process time in minutes
# head(biom_diff)
# 
# CleanCoordmegacell_EEZ_wMPA$B<-biom_diff$B 
# CleanCoordmegacell_EEZ_wMPA %>%  ggplot(aes(x=lon,y=lat,fill=B)) + scale_fill_gradientn(colours = colorspace::diverge_hcl(7)) + geom_raster() 
# #ok, great


##***CHUNK***: model with real K and test MPA effect
###OK, now, add the real K and model (use KperCell)
CleanCoordmegacell_EEZ_wMPA$KperCell<-KperCell 
CleanCoordmegacell_EEZ_wMPA %>%  ggplot(aes(x=lon,y=lat,fill=KperCell)) + scale_fill_viridis_c(limits = c(0, max(KperCell))) + geom_raster()
#assume biomass = half of K, E=0.3 with no transfer, and test how MPAs will perform in 30 years.
#Plot biomass transition for demonstration purposes.

#---I'll be back here---
biom_diff <- biom
biom_diff$B <- 0.25*KperCell

biol_data$K <- KperCell
rK<-biol_data %>% dplyr::select(pos1,r,K) %>% as.data.table()
setkey(rK,pos1)
distance_mat_full_prK<-distance_mat_full_prop[rK]

total_biomass<-list()

#cores<-detectCores()
#registerDoParallel(cores-1)
ptm <- proc.time()
for (t in 1:30){
  biom_diff <- distance_mat_full_prK[biom_diff] %>% mutate(Bdist=B*biom_prop, Growth=biom_prop*r*B) %>% group_by(pos2) %>% dplyr::select(pos2,Bdist,Growth) %>% summarize(B_add=sum(Bdist),G_add=sum(Growth)) %>%
    mutate(B=((1-(E*(1-MPAcell)))*B_add)+pmax(G_add*(1-(biom_diff$B/KperCell)),0)) %>% dplyr::rename(pos1 = pos2) %>% dplyr::select(pos1,B) %>% as.data.table()
  total_biomass[[t]]<-sum(biom_diff$B)
}
#stopImplicitCluster()
(proc.time() - ptm)/60 #check process time in minutes
head(biom_diff)
CleanCoordmegacell_EEZ_wMPA$B<-biom_diff$B
CleanCoordmegacell_EEZ_wMPA %>%  ggplot(aes(x=lon,y=lat,fill=B)) +  scale_fill_viridis_c(limits = c(0, max(biom_diff$B))) + geom_raster() #ok, great

#plot biomass trajectory in time
total_biomass_merged <- do.call("rbind",total_biomass)
plot(total_biomass_merged,xlab="Time (years)", ylab="Total Biomass (MT)") #ok, looks good

last(total_biomass_merged)

##***CHUNK*** close each pixel and evaluate global biomass
#now, check the change in global biomass as pixels are closed one at a time.
#calculate total biomass with additional MPA - total biomass without additional MPA
#Final output is a plot this delta biomass... this will feed into the tourism model.

biol_data$K <- KperCell
rK<-biol_data %>% dplyr::select(pos1,r,K) %>% as.data.table()
setkey(rK,pos1)
distance_mat_full_prK<-distance_mat_full_prop[rK]
biomass_withMPA<-list()

#Non-mpa positions
nonMPAposition<-which(MPAcell==0)
length(nonMPAposition)
length(MPAcell)

#ok to put outside since we are doing parallel comp
biom_diff <- biom
biom_diff$B <- 0.25*KperCell
EvaluateMPA <- MPAcell

#cores<-detectCores()
#registerDoParallel(cores-1)
ptm <- proc.time()
doParallel::registerDoParallel(cores=2)

biomass_withMPA <- foreach(i=1:2, .combine='rbind') %dopar% {

  EvaluateMPA[nonMPAposition[i]]<-1
  
  for (t in 1:20){
    biom_diff <- distance_mat_full_prK[biom_diff] %>% mutate(Bdist=B*biom_prop, Growth=biom_prop*r*B) %>% group_by(pos2) %>% dplyr::select(pos2,Bdist,Growth) %>% summarize(B_add=sum(Bdist),G_add=sum(Growth)) %>%
      mutate(B=((1-(E*(1-EvaluateMPA)))*B_add)+pmax(G_add*(1-(biom_diff$B/KperCell)),0)) %>% dplyr::rename(pos1 = pos2) %>% dplyr::select(pos1,B) %>% as.data.table()
  }
  sum(biom_diff$B)
  #biomass_withMPA[[i]]<-sum(biom_diff$B)
}
doParallel::stopImplicitCluster()
(proc.time() - ptm)/60 #check process time in minutes

plot(biomass_withMPA) #ok, looks good


GlobalDeltaBiom<-data.frame(biomass_withMPA)$biomass_withMPA-data.frame(last(total_biomass_merged))$last.total_biomass_merged.

CleanCoordmegacell_EEZ_wMPA %>% dplyr::select(lon, lat, MPA) %>% slice(nonMPAposition[1:length(GlobalDeltaBiom)]) %>% mutate(deltaBiomass=GlobalDeltaBiom) %>%
  ggplot(aes(x=lon,y=lat,fill=deltaBiomass)) + geom_raster()

plot(GlobalDeltaBiom,xlab="pixel #",ylab="delta Biomass (metric ton)")

#Note: need to run the code above for all the pixels in the world. Use the cloud to do the calculation,
#more efficient if the parameters are ready.

##***CHUNK*** Derive B/K per pixel and E per pixel.
head(MegaData)
dim(MegaData)

dim(Cleanmegacell)
head(Cleanmegacell)

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
CleanCoordmegacell_EEZ_wMPA %>% select(lon, lat, MPA) %>% mutate(ERperCell=ERperCell) %>%
  ggplot(aes(x=lon,y=lat,fill=ERperCell)) + scale_fill_viridis_c(limits = c(0, max(ERperCell))) + geom_raster()

#what is the current biomass?
#We need the B/K parameter
plot(MegaData$BK2012)

#Load Costello et al. (2016) database
CostelloData<-read.csv("/Users/ren/Documents/CODES/FoodProvision/Aquamaps/UnlumpedProjectionData.csv", stringsAsFactors = FALSE)
dim(CostelloData)
head(CostelloData,5)

Costello2012<-CostelloData %>% filter(Year=="2012")
table(Costello2012$Dbase)
table(Costello2012$Policy)
table(Costello2012$Scenario)
head(Costello2012)

#MSY from costello of RAM, FAO, and SOFIA
Costello2012 %>% group_by(Dbase,CatchShare) %>% summarise(sum(MSY))

#Manually change species name with related species to match Aquamaps species range data
CostelloDataPrime<- CostelloData %>%
  mutate(SciName=replace(SciName, SciName=="Sardinops melanostictus", "Sardinops sagax")) %>%
  mutate(SciName=replace(SciName, SciName=="Sardinops caeruleus", "Sardinops sagax")) %>%
  mutate(SciName=replace(SciName, SciName=="Sardinops ocellatus", "Sardinops sagax")) %>%
  mutate(SciName=replace(SciName, SciName=="Merluccius capensis, M.paradoxus", "Merluccius capensis")) %>%
  mutate(SciName=replace(SciName, SciName=="Auxis thazard, A. rochei", "Auxis thazard")) %>%
  mutate(SciName=replace(SciName, SciName=="Pleuronectes quadrituberculat.", "Pleuronectes quadrituberculat")) %>%
  mutate(SciName=replace(SciName, SciName=="Pseudopleuronectes herzenst.", "Pseudopleuronectes herzenst")) %>%
  mutate(SciName=replace(SciName, SciName=="Herklotsichthys quadrimaculat.", "Herklotsichthys quadrimaculat")) %>%
  mutate(SciName=replace(SciName, SciName=="Engraulis capensis", "Engraulis encrasicolus")) %>%
  mutate(SciName=replace(SciName, SciName=="Trachypenaeus curvirostris", "Trachysalambria curvirostris")) %>%
  mutate(SciName=replace(SciName, SciName=="Patinopecten yessoensis", "Mizuhopecten yessoensis")) %>%
  mutate(SciName=replace(SciName, SciName=="Penaeus setiferus", "Litopenaeus setiferus")) %>%
  mutate(SciName=replace(SciName, SciName=="Loligo opalescens", "Doryteuthis opalescens")) %>%
  mutate(SciName=replace(SciName, SciName=="Larimichthys croceus", "Larimichthys crocea")) %>%
  mutate(SciName=replace(SciName, SciName=="Loligo gahi", "Doryteuthis gahi")) %>%
  mutate(SciName=replace(SciName, SciName=="Chelon haematocheilus", "Liza haematocheila")) %>%
  mutate(SciName=replace(SciName, SciName=="Anadara granosa", "Tegillarca granosa")) %>%
  mutate(SciName=replace(SciName, SciName=="Penaeus chinensis", "Fenneropenaeus chinensis")) %>%
  mutate(SciName=replace(SciName, SciName=="Penaeus merguiensis", "Fenneropenaeus merguiensis")) %>%
  mutate(SciName=replace(SciName, SciName=="Sebastes marinus", "Sebastes norvegicus")) %>%
  mutate(SciName=replace(SciName, SciName=="Cancer magister", "Metacarcinus magister")) %>%
  mutate(SciName=replace(SciName, SciName=="Loligo pealeii", "Doryteuthis pealeii")) %>%  
  mutate(SciName=replace(SciName, SciName=="Spisula polynyma", "Mactromeris polynyma")) %>%  
  mutate(SciName=replace(SciName, SciName=="Ommastrephes bartramii", "Ommastrephes bartramii")) %>%  
  mutate(SciName=replace(SciName, SciName=="Stichopus japonicus", "Apostichopus japonicus")) %>%  
  mutate(SciName=replace(SciName, SciName=="Penaeus notialis", "Farfantepenaeus notialis")) %>%  
  mutate(SciName=replace(SciName, SciName=="Psetta maxima", "Scophthalmus maximus")) %>%  
  mutate(SciName=replace(SciName, SciName=="Ostrea lutaria", "Ostrea chilensis")) %>% 
  mutate(SciName=replace(SciName, SciName=="Tawera gayi", "Tawera elliptica")) %>%   
  mutate(SciName=replace(SciName, SciName=="Penaeus japonicus", "Marsupenaeus japonicus")) %>%   
  mutate(SciName=replace(SciName, SciName=="Penaeus brasiliensis","Farfantepenaeus aztecus")) %>%   
  mutate(SciName=replace(SciName, SciName=="Mytilus chilensis","Mytilus edulis")) %>% 
  mutate(SciName=replace(SciName, SciName=="Tetrapturus audax","Kajikia audax" )) %>% 
  mutate(SciName=replace(SciName, SciName=="Cheilodactylus bergi","Nemadactylus bergi")) %>% 
  mutate(SciName=replace(SciName, SciName=="Venerupis pullastra","Venerupis corrugata")) %>% 
  mutate(SciName=replace(SciName, SciName=="Penaeus aztecus","Farfantepenaeus aztecus")) %>% 
  mutate(SciName=replace(SciName, SciName=="Penaeus duorarum","Farfantepenaeus duorarum")) %>% 
  mutate(SciName=replace(SciName, SciName=="Penaeus kerathurus","Melicertus kerathurus")) %>% 
  mutate(SciName=replace(SciName, SciName=="Penaeus californiensis","Farfantepenaeus californiensis")) %>% 
  mutate(SciName=replace(SciName, SciName=="Penaeus brevirostris","Farfantepenaeus brevirostris")) %>% 
  mutate(SciName=replace(SciName, SciName=="Penaeus latisulcatus","Melicertus latisulcatus")) %>%     
  mutate(SciName=replace(SciName, SciName=="Penaeus occidentalis","Litopenaeus occidentalis")) %>% 
  mutate(SciName=replace(SciName, SciName=="Penaeus vannamei","Litopenaeus vannamei")) %>% 
  mutate(SciName=replace(SciName, SciName=="Raja naevus","Leucoraja naevus")) %>% 
  mutate(SciName=replace(SciName, SciName=="Jasus novaehollandiae","Jasus edwardsii")) %>% 
  mutate(SciName=replace(SciName, SciName=="Makaira indica","Istiompax indica")) %>% 
  mutate(SciName=replace(SciName, SciName=="Lithodes aequispina","Lithodes aequispinus")) %>% 
  mutate(SciName=replace(SciName, SciName=="Eleginus navaga","Eleginus nawaga")) %>%
  mutate(SciName=replace(SciName, SciName=="Saxidomus giganteus","Saxidomus gigantea")) %>%
  mutate(SciName=replace(SciName, SciName=="Mugil soiuy","Liza haematocheila")) %>%
  mutate(SciName=replace(SciName, SciName=="Xiphopenaeus riveti","Xiphopenaeus kroyeri")) %>%
  mutate(SciName=replace(SciName, SciName=="Pleuronectes vetulus","Parophrys vetulus")) %>%
  mutate(SciName=replace(SciName, SciName=="Raja radiata","Amblyraja radiata")) %>%
  mutate(SciName=replace(SciName, SciName=="Aspitrigla cuculus","Chelidonichthys cuculus")) %>%
  mutate(SciName=replace(SciName, SciName=="Valamugil seheli","Moolgarda seheli")) %>%
  mutate(SciName=replace(SciName, SciName=="Tetrapturus albidus","Kajikia albida")) %>%
  mutate(SciName=replace(SciName, SciName=="Zenopsis nebulosus","Zenopsis nebulosa")) %>%
  mutate(SciName=replace(SciName, SciName=="Arius thalassinus","Netuma thalassinus")) %>%
  mutate(SciName=replace(SciName, SciName=="Parika scaber","Meuschenia scaber")) %>%
  mutate(SciName=replace(SciName, SciName=="Sardinops neopilchardus","Sardinops sagax")) %>%
  mutate(SciName=replace(SciName, SciName=="Raja batis","Dipturus batis")) %>%
  mutate(SciName=replace(SciName, SciName=="Alosa pontica","Alosa immaculata")) %>%
  mutate(SciName=replace(SciName, SciName=="Conger orbignyanus","Conger orbignianus")) %>%
  mutate(SciName=replace(SciName, SciName=="Acanthopagrus schlegeli","Acanthopagrus schlegelii")) %>%
  mutate(SciName=replace(SciName, SciName=="Solea lascaris","Pegusa lascaris")) %>%
  mutate(SciName=replace(SciName, SciName=="Raja circularis","Leucoraja circularis")) %>%
  mutate(SciName=replace(SciName, SciName=="Balistes carolinensis","Balistes capriscus")) %>%
  mutate(SciName=replace(SciName, SciName=="Plesiopenaeus edwardsianus","Aristaeopsis edwardsiana")) %>%
  mutate(SciName=replace(SciName, SciName=="Epinephelus flavolimbatus","Hyporthodus flavolimbatus")) %>%
  mutate(SciName=replace(SciName, SciName=="Epinephelus niveatus","Hyporthodus niveatus")) %>%
  mutate(SciName=replace(SciName, SciName=="Epinephelus nigritus","Hyporthodus nigritus")) %>%
  mutate(SciName=replace(SciName, SciName=="Epinephelus mystacinus","Hyporthodus mystacinus")) %>%
  mutate(SciName=replace(SciName, SciName=="Raja oxyrinchus","Dipturus oxyrinchus")) %>%
  mutate(SciName=replace(SciName, SciName=="Raja fullonica","Leucoraja fullonica")) %>%
  mutate(SciName=replace(SciName, SciName=="Jasus verreauxi","Sagmariasus verreauxi")) %>%
  mutate(SciName=replace(SciName, SciName=="Anadara ovalis","Lunarca ovalis")) %>%
  mutate(SciName=replace(SciName, SciName=="Pseudopentaceros richardsoni","Pentaceros richardsoni")) %>%
  mutate(SciName=replace(SciName, SciName=="Chelidonichthys lastoviza","Trigloporus lastoviza")) %>%
  mutate(SciName=replace(SciName, SciName=="Protothaca staminea","Leukoma staminea")) %>%
  mutate(SciName=replace(SciName, SciName=="Notothenia squamifrons","Lepidonotothen squamifrons")) %>%
  mutate(SciName=replace(SciName, SciName=="Pleuronectes quadrituberculat","Pleuronectes quadrituberculatus")) %>%
  mutate(SciName=replace(SciName, SciName=="Pseudopleuronectes herzenst","Pseudopleuronectes herzensteini")) %>%
  mutate(SciName=replace(SciName, SciName=="Herklotsichthys quadrimaculat","Herklotsichthys quadrimaculatus")) %>%
  filter(k>0) #remove zero carrying capacity
CostelloPresentPrime<- CostelloDataPrime %>% dplyr::filter(Year=="2012")
head(CostelloPresentPrime)

#check the effect of pooling from all dataset vs FAO only
CostelloK_FAO_only<-CostelloDataPrime %>% filter(Year=="2012") %>% filter(Dbase=="FAO") %>%  mutate(k=Biomass/(0.4*BvBmsy)) %>% group_by(SciName) %>% summarize(K=sum(k), B=sum(Biomass), Fstatus=weighted.mean(FvFmsy, MSY), Bstatus=weighted.mean(BvBmsy, MSY)) %>% mutate(BK2012=B/K) %>% dplyr::select(SciName,BK2012) %>% dplyr::rename(BK2012_FAO_only = BK2012)
head(CostelloK_FAO_only)
#CostelloK_unfiltered<-CostelloDataPrime %>% filter(Year=="2012") %>% mutate(k=Biomass/(0.4*BvBmsy)) %>% group_by(SciName) %>% summarize(K=sum(k), B=sum(Biomass), Fstatus=weighted.mean(FvFmsy, MSY), Bstatus=weighted.mean(BvBmsy, MSY)) %>% mutate(BK2012=B/K)
#CostelloK_Join<-left_join(CostelloK_unfiltered,CostelloK_FAO_only, by="SciName")
#head(CostelloK_Join)
#plot(CostelloK_Join$BK2012.x,CostelloK_Join$BK2012.y)

CostelloK<-CostelloDataPrime %>% filter(Year=="2012") %>% mutate(k=Biomass/(0.4*BvBmsy)) %>% group_by(SciName) %>% summarize(K=sum(k), B=sum(Biomass), Fstatus=weighted.mean(FvFmsy, MSY), Bstatus=weighted.mean(BvBmsy, MSY)) %>% mutate(BK2012=B/K)
CostelloK<-left_join(CostelloK,CostelloK_FAO_only, by="SciName")
head(CostelloK)
dim(CostelloK)

plot(CostelloK$BK2012)

###CHUNK
#implement the analytic solution.
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
CleanCoordmegacell_EEZ_wMPA %>% select(lon, lat) %>% mutate(kpercell=rowSums(kpercell_expand)) %>%
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

BvKperCell <- rowSums(biomperStockCell)/rowSums(kpercell_expand)

#plot average
CleanCoordmegacell_EEZ_wMPA %>% dplyr::select(lon, lat, MPA) %>% mutate(BvKperCell=BvKperCell) %>%
  ggplot(aes(x=lon,y=lat,fill=BvKperCell)) + scale_fill_viridis_c(limits = c(0, max(BvKperCell))) + geom_raster()


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

head(MegaData)
MegaData$bvk_fin<-0.3
#--test the code
stock_num<-1
bvk_equi <- func_evaluateMPA(stock_num, Cleanmegacell,biol_data,distance_mat_full,MegaData)
head(bvk_equi)
dim(bvk_equi)
#--store the results and collate later
collate_bvk_equi<-list()
nstock<-2#dim(MegaData)[1]
for (stock_num in 1:nstock){ 
collate_bvk_equi[[stock_num]] <- func_evaluateMPA(stock_num, Cleanmegacell,biol_data,distance_mat_full,MegaData)$bvk_equi
}

collate_bvk_equi_merged <- data.frame(do.call("cbind",collate_bvk_equi))
colnames(collate_bvk_equi_merged)<-MegaData$stockid[1:nstock]
head(collate_bvk_equi_merged)
dim(collate_bvk_equi_merged)
#ok, done testing. Now, do parallel compute

#--parallel compute
nstock<-dim(MegaData)[1]
ptm <- proc.time()
cores<-detectCores()
registerDoParallel(cores/2)

head(MegaData)

which(MegaData$INCLUDE==1)

stock_include<-which(MegaData$INCLUDE==1)#c(1,3,5)#edit this. include only stocks for the analysis
collate_bvk_equi_merged <- foreach(stock_num=stock_include[1:length(stock_include)], .combine='cbind') %dopar% {
  func_evaluateMPA(stock_num, Cleanmegacell,biol_data,distance_mat_full,MegaData)$bvk_equi
}
doParallel::stopImplicitCluster()
(proc.time() - ptm)/60 #check process time in minutes
colnames(collate_bvk_equi_merged)<-MegaData$stockid[stock_include]
head(collate_bvk_equi_merged)
dim(collate_bvk_equi_merged)

#plot
collate_bvk_equi_merged[is.na(collate_bvk_equi_merged)] <- 0
collate_bvk_equi_merged[collate_bvk_equi_merged>1]<-1 #cap to 1
plotme<-rowMeans(collate_bvk_equi_merged)
plot(plotme)

CleanCoordmegacell_EEZ_wMPA %>% ggplot(aes(x=lon,y=lat,fill=plotme)) + geom_raster() #ok, great

#--to do
#calculation of tourism values can be incorporated in the function
#tou

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


# ##***CHUNK***: TOY model. Useful after we have the biological model ready.
# #Toy model
# #Empirically derived parameters
# Qd0 <- 100 #current number of dives
# P0 <- 50 #current price per dive in USD
# C0 <- 150 #choke price in USD
# alpha <- 0.5
# beta <- 0.5
# X0 <- 10 #dive price (USD) where no company will offer their service
# nu <- 1.1
# 
# #Model intermediate output
# deltaB <- 20 #change in biomass
# deltaS <- 10 #change in species diversity metric
# 
# #Parameter calculations
# #we need a and b to compute mu
# (a <- (C0*Qd0)/(C0-P0))
# (b <- Qd0/(C0-P0))
# (mu <- nu*(a-(b*P0))) #assume that MPA will increase the demand for diving by 10%.
# 
# (e <- (a-(b*P0))/(P0-X0)) #Slope of the supply curve
# (c <- (((b*P0)-a)*X0)/(P0-X0)) #Dive quantity supplied by the industry when the price of diving is zero
# 
# (P1 <- (a-c+mu+(alpha*deltaB)+(beta*deltaS))/(e+b)) #Price per dive at site i when the site is converted to an MPA
# (Qd1 <- a-(b*P1)+mu+(alpha*deltaB)+(beta*deltaS)) #Number of dives at site i when the site is converted to an MPA
# (C1 <- (a+mu+(alpha*deltaB)+(beta*deltaS))/b)
# 
# ##Tourism model output
# #Change in tourism revenue at site i
# (P1*Qd1) - (P0*Qd0)
# 
# #Change in counsumer surplus
# (0.5*(C1-P1)*Qd1) - (0.5*(C0-P0)*Qd0)
# 
# #Change in consumer + producer surplus
# (Qd0*(C1-C0)) + (0.5*(Qd1-Qd0)*(C1-C0))
