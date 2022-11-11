#function for evaluating the biomass inside MPA
#explicitly calculate biomass changes inside MPA in time.

##parameters here
#stock_num <- 1

#--Revised code. Accounts for larval and adult movement.
func_evaluateMPA_explicit<-function(stock_num, transformed_stockdistrib,MegaData_filtered_step_fin,MPA_loc){
  #stock_num - stock number. The idea is to calculate the effect of MPA per stock.
  #transformed_stockdistrib - stock distribution
  #MegaData_filtered_step_fin - biological parameters
  #MPA_loc - location of MPAs, global. Note that what we will do is to map MPAs per species.
  
  
  ##--prep datasets
  Cleanmegacell <- transformed_stockdistrib[,6:1155]
  ##--subset the stock and get the row numbers where entry == 1
  stock_subset_i<-which(Cleanmegacell[,stock_num] > 0)
  ##--load distance matrix and add MPA placeholders with values = 0 
  distance_mat_full_prop_larvae <- fst::read.fst(paste0("/Users/ren/Documents/GitHub/tourism-mpa-support/connect_matrix/",stock_num,"_connect_larvae.fst")) %>% mutate(MPA_sink=0,MPA_source=0) %>% as.data.table()
  
  #test<-distance_mat_full_prop_larvae  %>% group_by(source) %>% summarize(sumbiom=sum(biom_prop))
  #min(test$sumbiom) #ok, this verifies that the "source" is the source of adult or larvae
  #test2<-distance_mat_full_prop_larvae  %>% group_by(sink) %>% summarize(sumbiom=sum(biom_prop))
  #min(test2$sumbiom) #sink means the proportion of biomass they received
    
  # head(distance_mat_full_prop_larvae)
  # sum(distance_mat_full_prop_larvae$MPA_sink) #sum is zero
  
  ##--now, label sinks and source pixels that are MPAs
  #MPA_sink means the sink pixel is an MPA
  #MPA_source means that the source pixel is an MPA
  distance_mat_full_prop_larvae_wMPA <- distance_mat_full_prop_larvae %>% mutate(MPA_sink=replace(MPA_sink,sink %in% MPA_loc$cell_id,1)) %>% mutate(MPA_source=replace(MPA_source,source %in% MPA_loc$cell_id,1))
  # head(distance_mat_full_prop_larvae_wMPA)
  # sum(distance_mat_full_prop_larvae_wMPA $MPA_sink)
  # sum(distance_mat_full_prop_larvae_wMPA $MPA_source)
  
  #adult
  distance_mat_full_prop_adult <- fst::read.fst(paste0("/Users/ren/Documents/GitHub/tourism-mpa-support/connect_matrix_adult/",stock_num,"_connect_adult.fst")) %>% mutate(MPA_sink=0,MPA_source=0) %>% as.data.table()
  distance_mat_full_prop_adult_wMPA <- distance_mat_full_prop_adult %>% mutate(MPA_sink=replace(MPA_sink,sink %in% MPA_loc$cell_id,1)) %>% mutate(MPA_source=replace(MPA_source,source %in% MPA_loc$cell_id,1))
  #distance_mat_full_prop_adult_wMPA <- distance_mat_full_prop_larvae_wMPA
  
  #columns : sink source   biom_prop MPA_sink MPA_source
  
  #--proportion of adult that stays at site i. Consider MPA sites only. params_combined_adult contains the s_ii parameter
  params_combined_adult <- distance_mat_full_prop_adult_wMPA %>% filter(sink==source) %>% dplyr::select(-MPA_source) %>% group_by(sink, source, MPA_sink) %>% summarise(s_ii=sum(biom_prop)) %>% filter(MPA_sink==1)#proportion of adult that stayed

  ##--adult, second term, non-MPA source, destination is MPA, aggregated
  sum_sji_sourcenonMPA <- distance_mat_full_prop_adult_wMPA %>% filter(source != sink) %>% filter(MPA_source==0) %>% filter(MPA_sink==1) %>% group_by(sink) %>% dplyr::select(sink,biom_prop) %>% summarise(sum_sji_sourcenonMPA=sum(biom_prop))
  
  #MPA source, destination is MPA, aggregated data. This will not be used but useful.
  sum_sji_sourceMPA <- distance_mat_full_prop_adult_wMPA %>% filter(source != sink) %>% filter(MPA_source==1) %>% filter(MPA_sink==1) %>% group_by(sink) %>% dplyr::select(sink,biom_prop) %>% summarise(sum_sji_sourceMPA=sum(biom_prop))
  
  #this is the MPA source, MPA destination, non-aggregated data
  sji_MPA_to_MPA <- distance_mat_full_prop_adult_wMPA %>% filter(source != sink) %>% filter(MPA_source==1) %>% filter(MPA_sink==1) %>% select(sink, source, biom_prop)
  
  params_combined_adult_v1 <- left_join(params_combined_adult, sum_sji_sourcenonMPA, by = "sink")
  params_combined_adult_v2 <- left_join(params_combined_adult_v1, sum_sji_sourceMPA, by = "sink")
  params_combined_adult_v2$sum_sji_sourcenonMPA[is.na(params_combined_adult_v2$sum_sji_sourcenonMPA)] <- 0 #replace NAs with zero
  params_combined_adult_v2$sum_sji_sourceMPA[is.na(params_combined_adult_v2$sum_sji_sourceMPA)] <- 0 #replace NAs with zero
  #params_combined_adult_v2 <- params_combined_adult_v2 %>% mutate(totalconnection = s_ii + sum_sji_sourceMPA + sum_sji_sourcenonMPA)
  
  #fixed this larvae term following above
  #proportion of larvae that stays at site i
  params_combined_larvae <- distance_mat_full_prop_larvae_wMPA %>% filter(sink==source) %>% dplyr::select(-MPA_source) %>% group_by(sink, source, MPA_sink) %>% summarise(alpha_ii=sum(biom_prop)) %>% filter(MPA_sink==1)#proportion of larvae that stayed
  
  #larvae, fourth term, non-MPA source, destination is MPA, aggregated
  sum_alphaji_sourcenonMPA <- distance_mat_full_prop_larvae_wMPA %>% filter(source != sink) %>% filter(MPA_source==0) %>% filter(MPA_sink==1) %>% dplyr::select(sink,biom_prop) %>% group_by(sink) %>% summarise(sum_alphaji_sourcenonMPA=sum(biom_prop))
  #MPA source, destination is MPA, aggregated data. This will not be used but useful.
  sum_alphaji_sourceMPA <- distance_mat_full_prop_larvae_wMPA %>% filter(source != sink) %>% filter(MPA_source==1) %>% filter(MPA_sink==1) %>% dplyr::select(sink,biom_prop) %>% group_by(sink) %>% summarise(sum_alphaji_sourceMPA=sum(biom_prop))

  #this is the MPA source, MPA destination, non-aggregated data
  alphaji_MPA_to_MPA <- distance_mat_full_prop_larvae_wMPA %>% filter(source != sink) %>% filter(MPA_source==1) %>% filter(MPA_sink==1) %>% select(sink, source, biom_prop)
  
  params_combined_larvae_v1 <- left_join(params_combined_larvae, sum_alphaji_sourcenonMPA, by = "sink")
  params_combined_larvae_v2 <- left_join(params_combined_larvae_v1, sum_alphaji_sourceMPA, by = "sink")
  params_combined_larvae_v2$sum_alphaji_sourcenonMPA[is.na(params_combined_larvae_v2$sum_alphaji_sourcenonMPA)] <- 0 #replace NAs with zero
  params_combined_larvae_v2$sum_alphaji_sourceMPA[is.na(params_combined_larvae_v2$sum_alphaji_sourceMPA)] <- 0 #replace NAs with zero

  ##--biomass inside MPA, note that the biomass is a single number. Adjust this if K per pixel differs.
  B_nonMPA <- MegaData_filtered_step_fin$bvk_fin[stock_num]*MegaData_filtered_step_fin$Kperpixel[stock_num]
  B_zero <-  ungroup(params_combined_adult_v2) %>% filter(MPA_sink==1) %>% mutate(biomass=B_nonMPA) %>% select(sink, biomass)
  
  ##-- derive model parameter values
  # Sii is the proportion of adult that stays at site i, Bi is the current biomass at site i
  Sii <-  ungroup(params_combined_adult) %>% filter(MPA_sink==1) %>% select(s_ii)
  ALPHAii <- ungroup(params_combined_larvae) %>% filter(MPA_sink==1) %>% select(alpha_ii)
  r <- MegaData_filtered_step_fin$r_fin[stock_num]
  K <- MegaData_filtered_step_fin$Kperpixel[stock_num] #this should be K per pixel

  ##-- we will only calculate changes inside MPA  
  for (t in 1:100){
    #calculate Bi_future using Bi_current
    #the idea here is to use equi biomass with no MPA then with MPA what is the equi biomass? The time aspect here is just for reaching the equi condition.
    
    #First term = Siit*Bit
    #where 
    
    if (t==1) {
      Bi_current <- B_zero
    }

    First <- Sii$s_ii*Bi_current$biomass
    
    #Second term = sum Sji*Bj
    #second term contribution from non-MPAs
    Second_nonMPAcontrib <- params_combined_adult_v2$sum_sji_sourcenonMPA*B_nonMPA 
    
    #second term contribution from MPAs
    #use full join as sji_MPA_to_MPA may not be fully connected
    Second_MPAcontrib <- full_join(sji_MPA_to_MPA,Bi_current,by="sink") %>% mutate(exportbiom=biom_prop*biomass) %>% dplyr::select(sink,exportbiom) %>% group_by(sink) %>% summarise(exportbiom2 = sum(exportbiom)) %>% replace(is.na(.), 0)
    
    #third term is biomass contribution from self-seeded larvae
    Third <- ALPHAii$alpha_ii*r*Bi_current$biomass*(1-(Bi_current$biomass/K))
    
    #same as adult
    Fourth_MPAcontrib_component <- full_join(alphaji_MPA_to_MPA,Bi_current,by="sink") %>% mutate(exportbiom=biom_prop*biomass) %>% dplyr::select(sink,exportbiom) %>% group_by(sink) %>% summarise(exportbiom2 = sum(exportbiom)) %>% replace(is.na(.), 0)
    Fourth <- r*(1-(Bi_current$biomass/K))*((params_combined_larvae_v2$sum_alphaji_sourcenonMPA*B_nonMPA)+Fourth_MPAcontrib_component$exportbiom2)
      
    Bi_current$biomass <- First + Second_nonMPAcontrib + Second_MPAcontrib$exportbiom2 + Third + Fourth  

  }
  
  stock_pixels <- distance_mat_full_prop_adult_wMPA %>% filter(sink==source) %>% dplyr::select(sink)
  stock_pixels_biomass <-left_join(stock_pixels,Bi_current,by="sink") %>% replace(is.na(.), B_nonMPA)
  #plot(stock_pixels_biomass$biomass)
  
  sink <- transformed_stockdistrib %>% select(cell_id) %>% dplyr::rename(sink=cell_id)
  
  #merge to global pixels
  biomass_result <-left_join(sink, stock_pixels_biomass, by="sink")
  
  #max(biomass_result$biomass,na.rm=T)
  #Cap biomass per pixel per species to K
  biomass_result <- biomass_result %>% mutate(biomass = replace(biomass, biomass > MegaData_filtered_step_fin$Kperpixel[stock_num], MegaData_filtered_step_fin$Kperpixel[stock_num]))
    
  
  return(biomass_result)
  silence(TRUE)
  
}