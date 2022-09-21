#This code reformat Juan's distance matrix

### --------------------------------------------------------
### Section 1 - Load packages and set directories ----------
### --------------------------------------------------------
gc()
rm(list = ls())

library(here)
library(dplyr)
library(raster)
library(sf)
library(tidyverse)
library(ggspatial)
library(bigstatsr)
library(doParallel)

source(here("scripts","functions","convert_mollweide.R"))

# Path to the Global Ocean Conservation Priorities directory on the emLab Google Drive
gocp_project_dir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/ocean-conservation-priorities"

# Path to the emLab data directory
emlab_data_dir <- "/Volumes/GoogleDrive/Shared drives/emlab/data"

# Path to the Pristine Seas tourism directory on the emLab Google Drive
this_project_dir <- "/Volumes/GoogleDrive/Shared drives/emlab/projects/current-projects/ps-tourism"

### Section 2 - Load spatial input data -----
ocean_low_res_moll <- raster::raster(file.path(emlab_data_dir, "ocean-conservation-priorities", "inputs", "ocean_low_res_moll.tif"))
dim(ocean_low_res_moll)
plot(ocean_low_res_moll) #361*722=260642 #-- ok, this matches the 

##--raster to df
##-- Make output ocean data frame
# ocean_df <- ocean_low_res_moll %>% 
#   raster::as.data.frame(xy = T) %>% 
#   set_names(c("lon", "lat", "ocean")) %>% 
#   filter(!is.na(ocean)) %>% 
#   rowid_to_column(var = "cell_id") %>% 
#   as_tibble()

ocean_df <- ocean_low_res_moll %>% 
  raster::as.data.frame(xy = T) %>% 
  set_names(c("lon", "lat", "ocean"))%>% 
  rowid_to_column(var = "cell_id") %>% 
  as_tibble()
head(ocean_df)

oceanpixels<-which(!is.na(ocean_df$ocean))
head(oceanpixels,20)# 335 is the first ocean entry
length(oceanpixels)# There are 149547 ocean pixels


##--Need to define this global parameter
pldrangelimit<-1000000

#function for summing matrix lengths
sum_mat_length <- function(distmat){
  temp_sum <- 0
  for (iter in 1:distmat){
    temp_sum <- temp_sum + get(paste0("matrix_length_",iter))
  }
  return(temp_sum)
}

##--FIRST distance matrix
#load Juan's pre-processed distance matrix
ocean_dist_matrix <- big_attach("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/dist_matrix_1.rds")
dim(ocean_dist_matrix) #260642 x 6517 matrix
typeof(ocean_dist_matrix)
plot(ocean_dist_matrix[,1])
plot(ocean_dist_matrix[,354])
sum(ocean_dist_matrix[,354],na.rm=T)

(matrix_length_1<-dim(ocean_dist_matrix)[2])

#mysubset1<-oceanpixels[oceanpixels<=6517]
mysubset1<-oceanpixels[oceanpixels<=matrix_length_1]
max(mysubset1)
(sub1length<-length(mysubset1)) #1311
head(mysubset1)
mysubset1[1]

registerDoParallel(detectCores()/2)
i<-c(1:sub1length)#which(MegaData$INCLUDE==1)#c(1,3,5)#which(MegaData$INCLUDE==1)#c(1,3,5)#edit this. include only stocks for the analysis
#collate_bvk_equi_merged <- foreach(stock_num=stock_include[1:length(stock_include)], .combine='cbind') %dopar% {
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,mysubset1[i]] #in meters. divide by 1000 when converting to km.
  store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i, distance=checkme[which(checkme<=pldrangelimit)])
  store_distlibrary 
}
doParallel::stopImplicitCluster()
head(distlibrary)
max(distlibrary$source)
#save the file
saveRDS(distlibrary, file = here("data","distance-library","dist_matrix_1_filter.rds"))

##--SECOND distance matrix
(sub1length <- max(distlibrary$source))
ocean_dist_matrix_2 <- big_attach("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/dist_matrix_6518.rds")
dim(ocean_dist_matrix_2)#260642 x 6516 #starts at 6518. 6517+6516=13033

(matrix_length_2<-dim(ocean_dist_matrix_2)[2])

#mysubset2 <- oceanpixels[oceanpixels>=6518 & oceanpixels<=13033] #ocean pixels in this particular subset of the matrix
mysubset2 <- oceanpixels[oceanpixels>=(matrix_length_1+1) & oceanpixels<=(matrix_length_1+matrix_length_2)]
sub2length <- length(mysubset2) #2094

#testing<-ocean_dist_matrix_2[1,1]

registerDoParallel(detectCores()/2)
i<-c(1:sub2length)#which(MegaData$INCLUDE==1)#c(1,3,5)#which(MegaData$INCLUDE==1)#c(1,3,5)#edit this. include only stocks for the analysis
#collate_bvk_equi_merged <- foreach(stock_num=stock_include[1:length(stock_include)], .combine='cbind') %dopar% {
distlibrary2 <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix_2[oceanpixels,(mysubset2[i]-matrix_length_1)] #in meters. divide by 1000 when converting to km.
  store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+sub1length, distance=checkme[which(checkme<=pldrangelimit)])
  store_distlibrary 
}
doParallel::stopImplicitCluster()
head(distlibrary2)
#save the file

saveRDS(distlibrary2, file = here("data","distance-library","dist_matrix_6518_filter.rds"))

#explore if our file is correct
max(distlibrary2$source)
min(distlibrary2$source)


##--Third distance matrix
sub12length <- max(distlibrary2$source) #3405
ocean_dist_matrix_3 <- big_attach("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/dist_matrix_13034.rds")
dim(ocean_dist_matrix_3)#260642 x 6516 #starts at 13034. 13033+6516=19549

(matrix_length_3<-dim(ocean_dist_matrix_3)[2])

#mysubset3 <- oceanpixels[oceanpixels>=13034 & oceanpixels<=19549] #ocean pixels in this particular subset of the matrix
mysubset3 <- oceanpixels[oceanpixels>=(matrix_length_1+matrix_length_2+1) & oceanpixels<=(matrix_length_1+matrix_length_2+matrix_length_3)]
sub3length <- length(mysubset3) #2138

registerDoParallel(detectCores()/2)
i<-c(1:sub3length)
distlibrary3 <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix_3[oceanpixels,(mysubset3[i]-(matrix_length_1+matrix_length_2))] #in meters. divide by 1000 when converting to km.
  store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+sub12length, distance=checkme[which(checkme<=pldrangelimit)])
  store_distlibrary 
}
doParallel::stopImplicitCluster()
head(distlibrary3)
saveRDS(distlibrary3, file = here("data","distance-library","dist_matrix_13034_filter.rds"))

##--Fourth distance matrix
sub123length <- max(distlibrary3$source)
ocean_dist_matrix_4 <- big_attach("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/dist_matrix_19550.rds")
matrix_length_4<-dim(ocean_dist_matrix_4)[2]
mysubset4 <- oceanpixels[oceanpixels>=(matrix_length_1+matrix_length_2+matrix_length_3+1) & oceanpixels<=(matrix_length_1+matrix_length_2+matrix_length_3+matrix_length_4)]
sub4length <- length(mysubset4)

registerDoParallel(detectCores()/2)
i<-c(1:sub4length)
distlibrary4 <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix_4[oceanpixels,(mysubset4[i]-(matrix_length_1+matrix_length_2+matrix_length_3))] #in meters. divide by 1000 when converting to km.
  store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+sub123length, distance=checkme[which(checkme<=pldrangelimit)])
  store_distlibrary 
}
doParallel::stopImplicitCluster()
head(distlibrary4)
saveRDS(distlibrary4, file = here("data","distance-library","dist_matrix_19550_filter.rds"))

##--Fifth distance matrix
distmat <- 5
mat_filename <- "dist_matrix_26066.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 6
mat_filename <- "dist_matrix_32582.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 7
mat_filename <- "dist_matrix_39098.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 8
mat_filename <- "dist_matrix_45614.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 9
mat_filename <- "dist_matrix_52130.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 10
mat_filename <- "dist_matrix_58646.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 11
mat_filename <- "dist_matrix_65162.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 12
mat_filename <- "dist_matrix_71678.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 13
mat_filename <- "dist_matrix_78194.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 14
mat_filename <- "dist_matrix_84710.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 15
mat_filename <- "dist_matrix_91226.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 16
mat_filename <- "dist_matrix_97742.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 17
mat_filename <- "dist_matrix_104258.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 18
mat_filename <- "dist_matrix_110774.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 19
mat_filename <- "dist_matrix_117290.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 20
mat_filename <- "dist_matrix_123806.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 21
mat_filename <- "dist_matrix_130322.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 22
mat_filename <- "dist_matrix_136838.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 23
mat_filename <- "dist_matrix_143354.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 24
mat_filename <- "dist_matrix_149870.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 25
mat_filename <- "dist_matrix_156386.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 26
mat_filename <- "dist_matrix_162902.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 27
mat_filename <- "dist_matrix_169418.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 28
mat_filename <- "dist_matrix_175934.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 29
mat_filename <- "dist_matrix_182450.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 30
mat_filename <- "dist_matrix_188966.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 31
mat_filename <- "dist_matrix_195482.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 32
mat_filename <- "dist_matrix_201998.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 33
mat_filename <- "dist_matrix_208514.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 34
mat_filename <- "dist_matrix_215030.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 35
mat_filename <- "dist_matrix_221546.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 36
mat_filename <- "dist_matrix_228062.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 37
mat_filename <- "dist_matrix_234578.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 38
mat_filename <- "dist_matrix_241094.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 39
mat_filename <- "dist_matrix_247610.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

#--
distmat <- 40
mat_filename <- "dist_matrix_254126.rds"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- sum_mat_length(distmat-1)
high_distmat <- sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()
assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))


##Now, combine all the distance matrices then save the file
distmat_filenames <- list.files(path=here("data","distance-library"), pattern=NULL, all.files=FALSE,full.names=TRUE)
distmat_filenames[1]
length(distmat_filenames)#40 files

tables <- lapply(distmat_filenames, readRDS)
combined.df <- do.call(rbind , tables)
dim(combined.df)
head(combined.df)
saveRDS(combined.df, file = here("data","distance-library","merged_dist_matrix.rds"))
