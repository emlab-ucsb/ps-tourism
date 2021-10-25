#fucntion

func_evaluateMPA<-function(stock_num, Cleanmegacell,biol_data, distance_mat_full, MegaData){
 
#--TEST subset to the first stock only
#dim(Cleanmegacell)
#length(Cleanmegacell[,1])#ok. This considers the first stock only

#--subset the stock and get the row numbers where entry == 1
stock_subset_i<-which(Cleanmegacell[,stock_num] > 0) #that's K density map so the values are < 1

#--subset connectivity matrix
#distance_mat_full %>% filter(pos1 %in% stock_subset_i, pos2 %in% stock_subset_i) %>% dim()
distance_mat_full_prop <- distance_mat_full %>% filter(pos1 %in% stock_subset_i, pos2 %in% stock_subset_i) %>% group_by(pos1) %>% mutate(biom_prop = exp(-( dist^2 / (2*(sigma^2))) ), biom_prop = biom_prop/sum(biom_prop)) %>% dplyr::select(-dist) %>% as.data.table()
#distance_mat_full_prop %>% group_by(pos1) %>% summarise(checksum=sum(biom_prop)) #ok, the answer is correct.
#dim(distance_mat_full_prop)

#--add MPA column but make it as pos2 (we want to know if the sink locations are MPAs)
#head(biol_data)
#dim(biol_data)
#length(stock_subset_i)
#biol_data %>% filter(pos1 %in% stock_subset_i) %>% dim() #ok, we can filter the biol_data to contain stock i's position only.

MPA_source <- biol_data %>% filter(pos1 %in% stock_subset_i) %>% select(pos1,MPA) %>% as.data.table()
#MPA_sink <- biol_data %>% filter(pos1 %in% stock_subset_i) %>% select(pos1,MPA) %>% dplyr::rename(pos2 = pos1, MPA_sink = MPA) %>% as.data.table()
MPA_sink <- MPA_source %>% dplyr::rename(pos2 = pos1, MPA_sink = MPA) %>% as.data.table()

#--check if the sink sites are MPAs
setkey(distance_mat_full_prop,pos2)
setkey(MPA_sink,pos2)

distance_mat_full_prop_wMPA_sink <- distance_mat_full_prop[MPA_sink]
#head(distance_mat_full_prop_wMPA_sink)
#dim(distance_mat_full_prop_wMPA_sink)

#--add MPA source
setkey(MPA_source,pos1)
setkey(distance_mat_full_prop_wMPA_sink,pos1)
distance_mat_full_prop_wMPA <- distance_mat_full_prop_wMPA_sink[MPA_source]
#head(distance_mat_full_prop_wMPA)
#dim(distance_mat_full_prop_wMPA)

#--check 1 -> pos2 are the sinks that are MPAs 
#check1<-distance_mat_full_prop_wMPA %>% filter(MPA_sink==1) #print the MPA links
#head(check1)

#--REWIRE
#--if the sink cell (pos2) is an MPA, then we should rewire those. i.e., if MPA_sink=1 (1 or 0 values only), make pos2 equals the pos1 number.
distance_mat_full_prop_wMPA_rewired <- distance_mat_full_prop_wMPA %>% mutate(pos2=replace(pos2, MPA_sink==1, pos1[MPA_sink==1]))

#check2<-distance_mat_full_prop_wMPA_rewired %>% filter(MPA_sink==1)
#head(check2) #this is the rewired file
#dim(check1)
#dim(check2) #ok, same dimension as check1.

#head(distance_mat_full_prop_wMPA_rewired)
#--check if correct
#distance_mat_full_prop_wMPA_rewired %>% group_by(pos1) %>% summarise(checksum=sum(biom_prop)) %>% summarise(max(checksum))
#--ok, looks correct.

#now that we have the connectivity matrix, we will assume the following:
#1. Larval connectivity matrix is the same as adult mobility matrix for now. Parameterize later.

#Since pixels are non-interacting, we can simultaneously evaluate all pixels
#if we want to evaluate a single pixel, we can try pixel # 94 which is a  non-MPA

#--solve equilibrium biomass
#--params for c1
params_combined<-subset(distance_mat_full_prop_wMPA_rewired, distance_mat_full_prop_wMPA_rewired$pos1 == distance_mat_full_prop_wMPA_rewired$pos2) %>%
  select(-MPA_sink) %>% group_by(pos1,pos2,MPA) %>% summarise(s_ii=sum(biom_prop))#proportion of adult that stayed
#dim(params_combined)
#head(params_combined)
#plot(params_combined$s_ii)#this is the self-seed. the max is 1.
#max(params_combined$s_ii)

#--params for c2
#--calculate all inbound connection (outside sites should be non-MPA)
#head(distance_mat_full_prop_wMPA_rewired)
#--remove self connection, then sum biom_prop by sink or pos2

sum_sji<-distance_mat_full_prop_wMPA_rewired %>% filter(pos1!= pos2) %>% select(pos2,biom_prop) %>% group_by(pos2) %>% summarise(sum_sji=sum(biom_prop))
#dim(sum_sji)
#head(sum_sji)
#min(sum_sji)
#max(sum_sji$sum_sji)#1.16. There are definitely sink cells.

params_combined_v2<-left_join(params_combined, sum_sji, by = "pos2")
params_combined_v2$sum_sji[is.na(params_combined_v2$sum_sji)] <- 0 #replace NAs with zero
#head(params_combined_v2)
#min(params_combined_v2$sum_sji)
#max(params_combined_v2$sum_sji)
#plot(params_combined_v2$sum_sji)#this is the sum of all connections going to different sites.

#params_combined_v2 %>% filter(sum_sji>1.16)

#--complete params here
#--this is just for the first stock
c1<-params_combined_v2$s_ii
#head(c1)
c2<- params_combined_v2$sum_sji*MegaData$bvk_fin[stock_num] #[1] is for the first stock
#head(c2)
#length(c1)
#length(c2) #size not equal to c1. There may be zero entries in sum_sji (maybe inside MPA)?

#--for now, s and alpha are equal
c3<-params_combined_v2$s_ii*MegaData$r_fin[stock_num]
#head(c3)
#length(c3)

c4<-c2

#--ok, now solve the equilibrium biomass per pixel.
bvk_equi<- (-(1+c4-c1-c3)+sqrt((1+c4-c1-c3)^2+(4*c3*(c2+c4))))/(2*c3)
#plot(bvk_equi) #there's pixel with bvk>1. There are also zeroes (are those from pixels with biomass = 0?)
#--what we can do is that if B>K, we reset it to K. i.e., those biomass will die because of the lack of food.
#--there are only few pixels (2?) that has B>K so this should not be an issue.
#length(bvk_equi)
#head(bvk_equi)

bvk_result1<-data.frame(pos1=params_combined_v2$pos1,bvk_equi)
bvk_result2<-left_join(data.frame(pos1=biol_data$pos1), bvk_result1, by="pos1")
#head(bvk_result2)

return(bvk_result2)
silence(TRUE)
}
