#filter and stitches connectivity matrices
func_stitch_connect_matrix<-function(distmat_files,stock_subset_i,MegaData_filtered_step2,stock_num){
  
  larvaeconnect <- list()
  
  # #parallel
  # registerDoParallel(detectCores()/2)
  # foreach(i=1:length(distmat_filenames)) %dopar% {
  #   larvaeconnect [[i]]<- read.fst(distmat_filenames[i]) %>% filter(source %in% stock_subset_i, sink %in% stock_subset_i, distance<=MegaData_filtered_step2$dispersal_distance_limit[stock_num])
  # }
  # doParallel::stopImplicitCluster()
  
  #converted already to km
  for (i in 1:length(distmat_files)){
    larvaeconnect [[i]]<-read.fst(distmat_files[i]) %>% mutate(distance = distance/1000) %>% filter(source %in% stock_subset_i, sink %in% stock_subset_i, distance<=MegaData_filtered_step2$dispersal_distance_limit[stock_num])
    #larvaeconnect [[i]]<-readRDS(distmat_filenames[i]) %>% mutate(distance = distance/1000) %>% filter(source %in% stock_subset_i, sink %in% stock_subset_i, distance<=MegaData_filtered_step2$dispersal_distance_limit[stock_num])
  }
  
  larvaeconnect_merged <- do.call("rbind",larvaeconnect) %>% as.data.frame()
  
  #included here: filter distance
  distance_mat_full_prop <- larvaeconnect_merged %>% group_by(source) %>% mutate(biom_prop = exp(-( distance^2 / (2*(MegaData_filtered_step2$sigma_larvae[stock_num]^2))) ), biom_prop = biom_prop/sum(biom_prop)) %>% dplyr::select(-distance) %>% as.data.table()
  
  return(distance_mat_full_prop)
  silence(TRUE)
}