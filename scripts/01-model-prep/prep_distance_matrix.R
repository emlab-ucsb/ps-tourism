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
library(fst)

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
pldrangelimit<-11541949#7100000#1000000 #unit in meters

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
#plot(ocean_dist_matrix[,1])
#plot(ocean_dist_matrix[,354])
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
  #this subset the distance of all the ocean pixel in the world from pixel i
  checkme <- ocean_dist_matrix[oceanpixels,mysubset1[i]] #in meters. divide by 1000 when converting to km.
  #this identify pixel numbers that are within the pld range of pixel i.
  store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i, distance=checkme[which(checkme<=pldrangelimit)])
  store_distlibrary 
}
doParallel::stopImplicitCluster()
head(distlibrary)

#check file
sub1length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink)) #makes sense that they are not equal.
max(distlibrary$sink)
max(distlibrary$source)

#save the file
saveRDS(distlibrary, file = here("data","distance-library","dist_matrix_1_filter.rds"))

matrix_length_total_1 <- matrix_length_1
saveRDS(matrix_length_total_1, file = here("data","distance-library","matrix_lengths","matrix_length_total_1.rds"))

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

#check output
sub2length
length(unique(distlibrary2$source)) 
length(unique(distlibrary2$sink))
max(distlibrary2$sink)
max(distlibrary2$source)

#save the file
saveRDS(distlibrary2, file = here("data","distance-library","dist_matrix_6518_filter.rds"))

matrix_length_total_2 <- matrix_length_1+matrix_length_2
saveRDS(matrix_length_total_2, file = here("data","distance-library","matrix_lengths","matrix_length_total_2.rds"))

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

#check output
sub3length
length(unique(distlibrary3$source)) 
length(unique(distlibrary3$sink))
max(distlibrary3$sink)#must not exceed 149547
max(distlibrary3$source)

saveRDS(distlibrary3, file = here("data","distance-library","dist_matrix_13034_filter.rds"))

matrix_length_total_3 <- matrix_length_1+matrix_length_2+matrix_length_3
saveRDS(matrix_length_total_3, file = here("data","distance-library","matrix_lengths","matrix_length_total_3.rds"))

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

#check output
sub4length
length(unique(distlibrary4$source)) 
length(unique(distlibrary4$sink))
max(distlibrary4$sink)#must not exceed 149547
max(distlibrary4$source)

saveRDS(distlibrary4, file = here("data","distance-library","dist_matrix_19550_filter.rds"))

matrix_length_total_4 <- matrix_length_1+matrix_length_2+matrix_length_3+matrix_length_4
saveRDS(matrix_length_total_4, file = here("data","distance-library","matrix_lengths","matrix_length_total_4.rds"))

#To do here:
#Save matrix lengths so we could load file 

distlibrary4 <- readRDS(here("data","distance-library","dist_matrix_19550_filter.rds"))
matrix_length_total_4 <- readRDS(here("data","distance-library","matrix_lengths","matrix_length_total_4.rds"))

##--Fifth distance matrix
distmat <- 5
mat_filename <- "dist_matrix_26066"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- matrix_length_total_4#sum_mat_length(distmat-1)
high_distmat <- matrix_length_total_4+matrix_length_5#sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

matrix_length_total_5 <- high_distmat#matrix_length_total_4 + matrix_length_5
saveRDS(matrix_length_total_5, file = here("data","distance-library","matrix_lengths","matrix_length_total_5.rds"))

#--
distmat <- 6
mat_filename <- "dist_matrix_32582"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- matrix_length_total_5#sum_mat_length(distmat-1)
high_distmat <- matrix_length_total_5+matrix_length_6#sum_mat_length(distmat)
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

matrix_length_total_6 <- high_distmat
saveRDS(matrix_length_total_6, file = here("data","distance-library","matrix_lengths","matrix_length_total_6.rds"))

#--
distmat <- 7
mat_filename <- "dist_matrix_39098"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

matrix_length_total_7 <- high_distmat
saveRDS(matrix_length_total_7, file = here("data","distance-library","matrix_lengths","matrix_length_total_7.rds"))

#--
distmat <- 8
mat_filename <- "dist_matrix_45614"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

assign(paste0("matrix_length_total_",distmat),high_distmat)
saveRDS(high_distmat, file = here("data","distance-library","matrix_lengths",paste0("matrix_length_total_",distmat,".rds")))

#--
distmat <- 9
mat_filename <- "dist_matrix_52130"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

assign(paste0("matrix_length_total_",distmat),high_distmat)
saveRDS(high_distmat, file = here("data","distance-library","matrix_lengths",paste0("matrix_length_total_",distmat,".rds")))

#--
distmat <- 10
mat_filename <- "dist_matrix_58646"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

assign(paste0("matrix_length_total_",distmat),high_distmat)
saveRDS(high_distmat, file = here("data","distance-library","matrix_lengths",paste0("matrix_length_total_",distmat,".rds")))

#--
distmat <- 11
mat_filename <- "dist_matrix_65162"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)
#sum(unique(distlibrary$source) %in% unique(distlibrary$sink))

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

assign(paste0("matrix_length_total_",distmat),high_distmat)
saveRDS(high_distmat, file = here("data","distance-library","matrix_lengths",paste0("matrix_length_total_",distmat,".rds")))

#--
distmat <- 12
mat_filename <- "dist_matrix_71678"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

assign(paste0("matrix_length_total_",distmat),high_distmat)
saveRDS(high_distmat, file = here("data","distance-library","matrix_lengths",paste0("matrix_length_total_",distmat,".rds")))

#--
distmat <- 13
mat_filename <- "dist_matrix_78194"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

assign(paste0("matrix_length_total_",distmat),high_distmat)
saveRDS(high_distmat, file = here("data","distance-library","matrix_lengths",paste0("matrix_length_total_",distmat,".rds")))

#--
distmat <- 14
mat_filename <- "dist_matrix_84710"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

assign(paste0("matrix_length_total_",distmat),high_distmat)
saveRDS(high_distmat, file = here("data","distance-library","matrix_lengths",paste0("matrix_length_total_",distmat,".rds")))

#--
distmat <- 15
mat_filename <- "dist_matrix_91226"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

assign(paste0("matrix_length_total_",distmat),high_distmat)
saveRDS(high_distmat, file = here("data","distance-library","matrix_lengths",paste0("matrix_length_total_",distmat,".rds")))

#--
distmat <- 16
mat_filename <- "dist_matrix_97742"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

assign(paste0("matrix_length_total_",distmat),high_distmat)
saveRDS(high_distmat, file = here("data","distance-library","matrix_lengths",paste0("matrix_length_total_",distmat,".rds")))

#--
distmat <- 17
mat_filename <- "dist_matrix_104258"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

assign(paste0("matrix_length_total_",distmat),high_distmat)
saveRDS(high_distmat, file = here("data","distance-library","matrix_lengths",paste0("matrix_length_total_",distmat,".rds")))

#--
distmat <- 18
mat_filename <- "dist_matrix_110774"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

assign(paste0("matrix_length_total_",distmat),high_distmat)
saveRDS(high_distmat, file = here("data","distance-library","matrix_lengths",paste0("matrix_length_total_",distmat,".rds")))

#--
distmat <- 19
mat_filename <- "dist_matrix_117290"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

assign(paste0("matrix_length_total_",distmat),high_distmat)
saveRDS(high_distmat, file = here("data","distance-library","matrix_lengths",paste0("matrix_length_total_",distmat,".rds")))

#load files here
distlibrary19 <- readRDS(here("data","distance-library","dist_matrix_117290_filter.rds"))
matrix_length_total_19 <- readRDS(here("data","distance-library","matrix_lengths","matrix_length_total_19.rds"))

#--
distmat <- 20
mat_filename <- "dist_matrix_123806"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

assign(paste0("matrix_length_total_",distmat),high_distmat)
saveRDS(high_distmat, file = here("data","distance-library","matrix_lengths",paste0("matrix_length_total_",distmat,".rds")))

#--
distmat <- 21
mat_filename <- "dist_matrix_130322"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

assign(paste0("matrix_length_total_",distmat),high_distmat)
saveRDS(high_distmat, file = here("data","distance-library","matrix_lengths",paste0("matrix_length_total_",distmat,".rds")))

#--
distmat <- 22
mat_filename <- "dist_matrix_136838"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

assign(paste0("matrix_length_total_",distmat),high_distmat)
saveRDS(high_distmat, file = here("data","distance-library","matrix_lengths",paste0("matrix_length_total_",distmat,".rds")))

#load files here
distlibrary22 <- readRDS(here("data","distance-library","dist_matrix_136838_filter.rds"))
matrix_length_total_22 <- readRDS(here("data","distance-library","matrix_lengths","matrix_length_total_22.rds"))

#--
distmat <- 23
mat_filename <- "dist_matrix_143354"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

assign(paste0("matrix_length_total_",distmat),high_distmat)
saveRDS(high_distmat, file = here("data","distance-library","matrix_lengths",paste0("matrix_length_total_",distmat,".rds")))

#--
distmat <- 24
mat_filename <- "dist_matrix_149870"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

assign(paste0("matrix_length_total_",distmat),high_distmat)
saveRDS(high_distmat, file = here("data","distance-library","matrix_lengths",paste0("matrix_length_total_",distmat,".rds")))

#--
#load files here
distlibrary24 <- readRDS(here("data","distance-library","dist_matrix_149870_filter.rds"))
matrix_length_total_24 <- readRDS(here("data","distance-library","matrix_lengths","matrix_length_total_24.rds"))

distmat <- 25
mat_filename <- "dist_matrix_156386"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

assign(paste0("matrix_length_total_",distmat),high_distmat)
saveRDS(high_distmat, file = here("data","distance-library","matrix_lengths",paste0("matrix_length_total_",distmat,".rds")))

#--
#load files here
distlibrary25 <- readRDS(here("data","distance-library","dist_matrix_156386_filter.rds"))
matrix_length_total_25 <- readRDS(here("data","distance-library","matrix_lengths","matrix_length_total_25.rds"))

distmat <- 26
mat_filename <- "dist_matrix_162902"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

assign(paste0("matrix_length_total_",distmat),high_distmat)
saveRDS(high_distmat, file = here("data","distance-library","matrix_lengths",paste0("matrix_length_total_",distmat,".rds")))


#load files here
distlibrary26 <- readRDS(here("data","distance-library","dist_matrix_162902_filter.rds"))
matrix_length_total_26 <- readRDS(here("data","distance-library","matrix_lengths","matrix_length_total_26.rds"))

#--
distmat <- 27
mat_filename <- "dist_matrix_169418"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

assign(paste0("matrix_length_total_",distmat),high_distmat)
saveRDS(high_distmat, file = here("data","distance-library","matrix_lengths",paste0("matrix_length_total_",distmat,".rds")))

#--
distmat <- 28
mat_filename <- "dist_matrix_175934"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

assign(paste0("matrix_length_total_",distmat),high_distmat)
saveRDS(high_distmat, file = here("data","distance-library","matrix_lengths",paste0("matrix_length_total_",distmat,".rds")))


#load files here
distlibrary28 <- readRDS(here("data","distance-library","dist_matrix_175934_filter.rds"))
matrix_length_total_28 <- readRDS(here("data","distance-library","matrix_lengths","matrix_length_total_28.rds"))

#--
distmat <- 29
mat_filename <- "dist_matrix_182450"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

assign(paste0("matrix_length_total_",distmat),high_distmat)
saveRDS(high_distmat, file = here("data","distance-library","matrix_lengths",paste0("matrix_length_total_",distmat,".rds")))

#--
#load files here
distlibrary29 <- readRDS(here("data","distance-library","dist_matrix_182450_filter.rds"))
matrix_length_total_29 <- readRDS(here("data","distance-library","matrix_lengths","matrix_length_total_29.rds"))

distmat <- 30
mat_filename <- "dist_matrix_188966"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

assign(paste0("matrix_length_total_",distmat),high_distmat)
saveRDS(high_distmat, file = here("data","distance-library","matrix_lengths",paste0("matrix_length_total_",distmat,".rds")))

#--
distlibrary30 <- readRDS(here("data","distance-library","dist_matrix_188966_filter.rds"))
matrix_length_total_30 <- readRDS(here("data","distance-library","matrix_lengths","matrix_length_total_30.rds"))

distmat <- 31
mat_filename <- "dist_matrix_195482"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

assign(paste0("matrix_length_total_",distmat),high_distmat)
saveRDS(high_distmat, file = here("data","distance-library","matrix_lengths",paste0("matrix_length_total_",distmat,".rds")))

#--
distmat <- 32
mat_filename <- "dist_matrix_201998"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

assign(paste0("matrix_length_total_",distmat),high_distmat)
saveRDS(high_distmat, file = here("data","distance-library","matrix_lengths",paste0("matrix_length_total_",distmat,".rds")))

#--
distmat <- 33
mat_filename <- "dist_matrix_208514"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

assign(paste0("matrix_length_total_",distmat),high_distmat)
saveRDS(high_distmat, file = here("data","distance-library","matrix_lengths",paste0("matrix_length_total_",distmat,".rds")))

#--
distmat <- 34
mat_filename <- "dist_matrix_215030"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

assign(paste0("matrix_length_total_",distmat),high_distmat)
saveRDS(high_distmat, file = here("data","distance-library","matrix_lengths",paste0("matrix_length_total_",distmat,".rds")))

#--
distlibrary34 <- readRDS(here("data","distance-library","dist_matrix_215030_filter.rds"))
matrix_length_total_34 <- readRDS(here("data","distance-library","matrix_lengths","matrix_length_total_34.rds"))

distmat <- 35
mat_filename <- "dist_matrix_221546"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

assign(paste0("matrix_length_total_",distmat),high_distmat)
saveRDS(high_distmat, file = here("data","distance-library","matrix_lengths",paste0("matrix_length_total_",distmat,".rds")))

#--
distmat <- 36
mat_filename <- "dist_matrix_228062"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

assign(paste0("matrix_length_total_",distmat),high_distmat)
saveRDS(high_distmat, file = here("data","distance-library","matrix_lengths",paste0("matrix_length_total_",distmat,".rds")))

#--
distmat <- 37
mat_filename <- "dist_matrix_234578"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

assign(paste0("matrix_length_total_",distmat),high_distmat)
saveRDS(high_distmat, file = here("data","distance-library","matrix_lengths",paste0("matrix_length_total_",distmat,".rds")))

#--
distmat <- 38
mat_filename <- "dist_matrix_241094"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

assign(paste0("matrix_length_total_",distmat),high_distmat)
saveRDS(high_distmat, file = here("data","distance-library","matrix_lengths",paste0("matrix_length_total_",distmat,".rds")))

#--
distmat <- 39
mat_filename <- "dist_matrix_247610"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

assign(paste0("matrix_length_total_",distmat),high_distmat)
saveRDS(high_distmat, file = here("data","distance-library","matrix_lengths",paste0("matrix_length_total_",distmat,".rds")))

#--
distmat <- 40
mat_filename <- "dist_matrix_254126"

prev_mat_filtered_totallength <- max(get(paste0("distlibrary",distmat-1))$source)
ocean_dist_matrix <- big_attach(paste0("/Volumes/GoogleDrive/Shared drives/emlab/data/ocean-distance-matrix/",mat_filename,".rds"))
assign(paste0("matrix_length_",distmat),dim(ocean_dist_matrix)[2])
low_distmat <- get(paste0("matrix_length_total_",distmat-1))
high_distmat <- get(paste0("matrix_length_total_",distmat-1))+get(paste0("matrix_length_",distmat))
mysubset_distmat <- oceanpixels[oceanpixels>=(low_distmat+1) & oceanpixels<=(high_distmat)]
sub_distmat_length <- length(mysubset_distmat)

registerDoParallel(detectCores()/2)
i<-c(1:sub_distmat_length)
distlibrary <- foreach(i=i, .combine='rbind') %dopar% {
  checkme <- ocean_dist_matrix[oceanpixels,(mysubset_distmat[i]-low_distmat)]
  (store_distlibrary <- which(checkme<=pldrangelimit) %>% data.frame() %>% set_names("sink") %>% mutate(source=i+prev_mat_filtered_totallength, distance=checkme[which(checkme<=pldrangelimit)]))
}
doParallel::stopImplicitCluster()

#check output
sub_distmat_length
length(unique(distlibrary$source)) 
length(unique(distlibrary$sink))
max(distlibrary$sink)#must not exceed 149547
max(distlibrary$source)

assign(paste0("distlibrary",distmat),distlibrary)
saveRDS(distlibrary, file = here("data","distance-library",paste0(mat_filename,"_filter.rds")))

assign(paste0("matrix_length_total_",distmat),high_distmat)
saveRDS(high_distmat, file = here("data","distance-library","matrix_lengths",paste0("matrix_length_total_",distmat,".rds")))


##Now, combine all the distance matrices then save the file
distmat_filenames <- list.files(path=here("data","distance-library"), pattern=".rds", all.files=FALSE,full.names=TRUE)
#distmat_filenames[1]
#length(distmat_filenames)#40 files
distmat_filenames_part1 <- distmat_filenames[1:5]
distmat_filenames_part2 <- distmat_filenames[6:10]
distmat_filenames_part3 <- distmat_filenames[11:15]
distmat_filenames_part4 <- distmat_filenames[16:20]
distmat_filenames_part5 <- distmat_filenames[21:25]
distmat_filenames_part6 <- distmat_filenames[26:30]
distmat_filenames_part7 <- distmat_filenames[31:35]
distmat_filenames_part8 <- distmat_filenames[36:40]

# #I just want to check if all files have data
# for (i in 1:40){
#   print(head(readRDS(distmat_filenames[i]),2))
# }

###----CONVERT FILE TO FST---load files and save as fts
# for (i in 1:40){
#   myfile <- readRDS(distmat_filenames[i])
#   fst::write.fst(myfile , here("data","distance-library","fst_file",paste0("dist_matrix_part",i,".fst")))
# }


tables_part1 <- lapply(distmat_filenames_part1, readRDS)
combineddf_part1 <- do.call(rbind , tables_part1)
dim(combineddf_part1)
head(combineddf_part1)
fst::write.fst(combineddf_part1 , here("data","distance-library","merged_dist_matrix","merged_dist_matrix_part1.fst"))

tables_part2 <- lapply(distmat_filenames_part2, readRDS)
combineddf_part2 <- do.call(rbind , tables_part2)
fst::write.fst(combineddf_part2 , here("data","distance-library","merged_dist_matrix","merged_dist_matrix_part2.fst"))

tables_part3 <- lapply(distmat_filenames_part3, readRDS)
combineddf_part3 <- do.call(rbind , tables_part3)
fst::write.fst(combineddf_part3 , here("data","distance-library","merged_dist_matrix","merged_dist_matrix_part3.fst"))

tables_part4 <- lapply(distmat_filenames_part4, readRDS)
combineddf_part4 <- do.call(rbind , tables_part4)
fst::write.fst(combineddf_part4 , here("data","distance-library","merged_dist_matrix","merged_dist_matrix_part4.fst"))

tables_part5 <- lapply(distmat_filenames_part5, readRDS)
combineddf_part5 <- do.call(rbind , tables_part5)
fst::write.fst(combineddf_part5 , here("data","distance-library","merged_dist_matrix","merged_dist_matrix_part5.fst"))

tables_part6 <- lapply(distmat_filenames_part6, readRDS)
combineddf_part6 <- do.call(rbind , tables_part6)
fst::write.fst(combineddf_part6 , here("data","distance-library","merged_dist_matrix","merged_dist_matrix_part6.fst"))

tables_part7 <- lapply(distmat_filenames_part7, readRDS)
combineddf_part7 <- do.call(rbind , tables_part7)
fst::write.fst(combineddf_part7 , here("data","distance-library","merged_dist_matrix","merged_dist_matrix_part7.fst"))

tables_part8 <- lapply(distmat_filenames_part8, readRDS)
combineddf_part8 <- do.call(rbind , tables_part8)
fst::write.fst(combineddf_part8 , here("data","distance-library","merged_dist_matrix","merged_dist_matrix_part8.fst"))

#saveRDS(combineddf_part1, file = here("data","distance-library","merged_dist_matrix","merged_dist_matrix.rds"))
