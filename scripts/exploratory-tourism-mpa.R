gc()
rm(list = ls())

library(dplyr)
library(ggplot2)

#checking the performance of an equation that quantifies tourism benefits and compare them with our analytic results
#s <- 0.5
alphaii <- 0.5
K <- 1000
sum_alpha_ji <- 1-alphaii
r <- 0.5
Bt0 <- 300
result <- list()
index <- 0

#effect of sii
for (s in seq(0.2,1,0.2)){
  B <- Bt0
  for (t in 0:30){
    index <- index + 1
    result[[index]]<- c(s,t,B)
    B <- (s*B) + ((1-s)*Bt0) + (alphaii*r*B*(1-(B/K)))+ (sum_alpha_ji*r*Bt0*(1-(B/K)))
  }}
result_merged <- do.call("rbind",result) 

data.frame(result_merged) %>% dplyr::rename(s=X1,time=X2,biomass=X3) %>% ggplot(aes(time,biomass/K,group=factor(s), shape=factor(s)))+geom_line()+geom_point()+ylim(0,1)+labs(x="Year",y="B/K",shape="s(i->i)")

data.frame(result_merged) %>% filter(X2==30)#these are steady-state values

#now, compare the steady-state values to our analytic results.

for (c1 in seq(0.2,1,0.2)){
c2<-(1-c1)*Bt0/K
c3<-alphaii*r
c4<-r*sum_alpha_ji*Bt0/K

B<- K*(-(1+c4-c1-c3)+sqrt((1+c4-c1-c3)^2+(4*c3*(c2+c4))))/(2*c3)
print(B)
}
#ok, our analyic solution is correct


#Effect of larval dispersal
s <- 0.8 #fix sii
#alphaii <- 0.5
K <- 1000

r <- 0.5
Bt0 <- 300
result_alphaii <- list()
index <- 0

#effect of sii
for (alphaii in seq(0.2,1,0.2)){
  B <- Bt0
  for (t in 0:30){
    index <- index + 1
    sum_alpha_ji <- 1-alphaii
    result_alphaii[[index]]<- c(alphaii,t,B)
    B <- (s*B) + ((1-s)*Bt0) + (alphaii*r*B*(1-(B/K)))+ (sum_alpha_ji*r*Bt0*(1-(B/K)))
  }}
result_alphaii_merged <- do.call("rbind",result_alphaii) 

data.frame(result_alphaii_merged) %>% dplyr::rename(alphaii=X1,time=X2,biomass=X3) %>% ggplot(aes(time,biomass/K,group=factor(alphaii), shape=factor(alphaii)))+geom_line()+geom_point()+ylim(0,1)+labs(x="Year",y="B/K",shape="alpha(i->i)")

#effect of sum_alpja_ji
s <- 0.8 #fix sii
alphaii <- 0.5
K <- 1000

r <- 0.5
Bt0 <- 300
result_sum_alphaji <- list()
index <- 0

#effect of sii
for (sum_alpha_ji in seq(0,5,1)){
  B <- Bt0
  for (t in 0:30){
    index <- index + 1
    result_sum_alphaji[[index]]<- c(sum_alpha_ji,t,B)
    B <- (s*B) + ((1-s)*Bt0) + (alphaii*r*B*(1-(B/K)))+ (sum_alpha_ji*r*Bt0*(1-(B/K)))
  }}
result_sum_alphaji_alphaii_merged <- do.call("rbind",result_sum_alphaji) 

data.frame(result_sum_alphaji_alphaii_merged) %>% dplyr::rename(sum_alphaji=X1,time=X2,biomass=X3) %>% ggplot(aes(time,biomass/K,group=factor(sum_alphaji), shape=factor(sum_alphaji)))+geom_line()+geom_point()+ylim(0,1)+labs(x="Year",y="B/K",shape="sum alpha(j->i)")


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

#Ok, now check B/K of RAM stocks


##
BvKperStock_expand <- matrix(rep(MegaData$BK2012,each=120297),nrow=120297)
BvKperStockCell <- BvKperStock_expand*KperStockCell #r*K per cell
BvKperCell <- rowSums(BvKperStockCell)/rowSums(KperStockCell)
CleanCoordmegacell_EEZ_wMPA %>% select(lon, lat, MPA) %>% mutate(BvKperCell=BvKperCell) %>%
  ggplot(aes(x=lon,y=lat,fill=BvKperCell)) + scale_fill_viridis_c(limits = c(0, max(BvKperCell))) + geom_raster()







