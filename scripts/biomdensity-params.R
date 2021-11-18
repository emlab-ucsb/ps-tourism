#biomdensity-params
#15 Nov 2021
#this file aims to generate the biomass density at equilibrium per stock

#Costello et al.’s business-as-usual (BAU) “conservation concern” assumption:
#1. Assessed stocks retain their current exploitation rates;
#2. Unassessed conservation concern stocks (i.e., currently overfished stocks or those experiencing overfishing) are subject to open-access fishing dynamics; and
#3. Unassessed nonconservation concern stocks have their exploitation rates set to maintain current biomass.

#Our assumptions:
#1. For stock in RAM database, get current exploitation rate. Then project B/Bmsy with no MPA???
#the biomass density and assume that the biomass density remains constant forever

MegaData<-readRDS(file = "/Users/ren/Documents/GitHub/FoodProvison_SupportFiles/Code Food Provision MPA/MegaData_Ray.rds")
head(MegaData)
#important parameters
#ExploitationRate_BAU1_Ray or Efin_BAU1_Ray
#given exploitation rate, i can calculate b/k. Then plot this with bvkfin.

bvk_calculated <- MegaData %>% filter(INCLUDE==1) %>% summarise(bvk_calc = 1-(ExploitationRate_BAU1_Ray/r_fin), bvk_fin=bvk_fin)
head(bvk_calculated)
plot(bvk_calculated$bvk_calc,bvk_calculated$bvk_fin)
