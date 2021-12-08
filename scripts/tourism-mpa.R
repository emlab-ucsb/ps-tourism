#Pristine seas tourism-MPA model
#Author: Reniel Cabral
#Clean version (18 Nov 2021)
#This code evaluates the build-up of biomass inside the current MPAs.
#After fully parameterizing this code, it is easy to add a chunk of code that evaluates tourism benefits of new MPAs.
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
library(fst)

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
CleanCoordmegacell_EEZ_wMPA %>% select(lon, lat, MPA) %>% mutate(ERperCell=ERperCell) %>%
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