#--
#Code for the paper: "Marine protected areas for dive tourism"
#Last update: 21 April 2023
#-- Code for plotting

gc()
rm(list = ls())

library(ggplot2)
library(here)
library(dplyr)

load(here("scripts","figures","Figure_data.RData"))

#Fig 2
#1) "biomass_data" contains the change in biomass due to MPA. If you want the dive sites only, use "biomass_data_divesites"
#2) "land_shp_moll" for the EEZ land background
#Fig 4
#3) "explore_user_fee_merged"
#Fig 5
#4) the rest of the files

##--Figure 2 plotting
#this requires "biomass_data" and "land_shp_moll"
cuts <-c(0,20,40,60,80,100, max(biomass_data$ratio_biom_divesite, na.rm=T))

b0 <- biomass_data %>% ggplot(aes(x=lon,y=lat,fill=ratio_biom_divesite)) + geom_raster(aes(fill = cut(ratio_biom_divesite, cuts))) +
  scale_fill_brewer(palette = "RdBu", drop = FALSE) + 
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), panel.background = element_blank())+
  geom_sf(data = land_shp_moll,fill="darkgrey", lwd = 0,  inherit.aes = F)+labs(title = "", fill = "% biomass increase")

b1 <- biomass_data %>% ggplot(aes(x=lon,y=lat,fill=ratio_biom_divesite)) + geom_raster(aes(fill = cut(ratio_biom_divesite, cuts))) +
  scale_fill_brewer(palette = "RdBu", drop = FALSE) + 
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), panel.background = element_blank(),legend.position="none")+
  geom_sf(data = land_shp_moll,fill="darkgrey", lwd = 0,  inherit.aes = F)+
  xlim(0.9e7, 1.5e7) +ylim(-2.5e6,3e6)+labs(title = "Southeast Asia", fill = "% biomass increase")

b2 <- biomass_data %>% ggplot(aes(x=lon,y=lat,fill=ratio_biom_divesite)) + geom_raster(aes(fill = cut(ratio_biom_divesite, cuts))) +
  scale_fill_brewer(palette = "RdBu", drop = FALSE) + 
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), panel.background = element_blank(),legend.position="none")+
  geom_sf(data = land_shp_moll,fill="darkgrey", lwd = 0,  inherit.aes = F)+
  xlim(-0.2e7, 0.4e7) +ylim(2.5e6,7.5e6)+labs(title = "Europe", fill = "% biomass increase")

b3 <- biomass_data %>% ggplot(aes(x=lon,y=lat,fill=ratio_biom_divesite)) + geom_raster(aes(fill = cut(ratio_biom_divesite, cuts))) +
  scale_fill_brewer(palette = "RdBu", drop = FALSE) + 
  theme(axis.title.x = element_blank(),axis.title.y = element_blank(), panel.background = element_blank(),legend.position="none")+
  geom_sf(data = land_shp_moll,fill="darkgrey", lwd = 0,  inherit.aes = F)+
  xlim(-1.1e7, -0.5e7) +ylim(-0.5e6,5.2e6)+labs(title = "Carribean", fill = "% biomass ratio")

bottom_row <- plot_grid(b1,b2,b3, nrow=1, labels = c('B', 'C','D'), label_size = 12)
Fig2 <- cowplot::plot_grid(b0,bottom_row, nrow = 2, labels =c('A',''),rel_heights=c(2,1))
Fig2

##--Figure 4 plotting
mean_taxrevenue_db <- explore_user_fee_merged %>% select(dive_tax, tax_revenue, Scenario) %>% group_by(Scenario,dive_tax) %>% summarise(mean_taxrev = mean(tax_revenue))

panel1 <- explore_user_fee_merged %>% ggplot(aes(x=dive_tax,y=tax_revenue/1e9,color=Scenario, group=interaction(Scenario,run)))+geom_line(size=1, alpha=0.01)+
  geom_line(data=mean_taxrevenue_db, aes(x=dive_tax, y=mean_taxrev/1e9, group=Scenario, linetype=Scenario))+theme_classic()+labs(x ="", y = "Dive fee revenue\n(billion US$)")+ theme(legend.position = "none")   
panel1

max_val <- max(explore_user_fee_merged$tax_revenue/1e9)
min_val <- min(explore_user_fee_merged$tax_revenue/1e9)
min_ref <- min_val+0.6*(max_val-min_val)

#difference plot panel 1
data1 <- explore_user_fee_merged %>% select(dive_tax, tax_revenue, Scenario, run) %>% filter(Scenario =="With MPA")
data2 <- explore_user_fee_merged %>% select(dive_tax, tax_revenue, Scenario, run) %>% filter(Scenario =="No MPA")
diff_data <- left_join(data1,data2, by=c("dive_tax","run")) %>% mutate(difference = tax_revenue.x-tax_revenue.y)

mean_tax_revenue_diff <- diff_data %>% group_by(dive_tax) %>% summarise(mean_tax_revenue_diff = mean(difference))
panel1_diff <- diff_data %>% ggplot(aes(x=dive_tax,y=difference/1e9)) + geom_line(aes(group=run),size=1, alpha=0.01)+
  geom_line(data=mean_tax_revenue_diff, aes(x=dive_tax, y=mean_tax_revenue_diff/1e9))+theme_classic()+labs(x ="User fee per dive\n(US$)", y = "Difference\n(billion US$)")+ theme(legend.position = "none")   +
  theme(panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),legend.background = element_rect(fill='transparent'),legend.box.background = element_rect(fill='transparent'))+
  ylim(0,max(diff_data$difference/1e9))
panel1_diff

panel1_inset <- panel1+annotation_custom(grob = ggplotGrob(panel1_diff), xmin=75, xmax=200, ymin=min_ref, ymax=max_val) 

mean_delta_dive_revenue <- explore_user_fee_merged %>% select(dive_tax, delta_dive_revenue, Scenario) %>% group_by(Scenario,dive_tax) %>% summarise(mean_delta_dive_revenue = mean(delta_dive_revenue))
panel2 <- explore_user_fee_merged %>% ggplot(aes(x=dive_tax,y=delta_dive_revenue/1e9,color=Scenario, group=interaction(Scenario,run)))+geom_line(size=1, alpha=0.01)+
  geom_line(data=mean_delta_dive_revenue, aes(x=dive_tax, y=mean_delta_dive_revenue/1e9, group=Scenario, linetype=Scenario))+theme_classic()+labs(x ="", y = "\u0394 dive revenue\n(billion US$)")+ theme(legend.position = "none")   
panel2

max_val <- max(explore_user_fee_merged$delta_dive_revenue/1e9)
min_val <- min(explore_user_fee_merged$delta_dive_revenue/1e9)
min_ref <- min_val+0.6*(max_val-min_val)

#difference plot panel 2
data1 <- explore_user_fee_merged %>% select(dive_tax, delta_dive_revenue, Scenario, run) %>% filter(Scenario =="With MPA")
data2 <- explore_user_fee_merged %>% select(dive_tax, delta_dive_revenue, Scenario, run) %>% filter(Scenario =="No MPA")
diff_data <- left_join(data1,data2, by=c("dive_tax","run")) %>% mutate(difference = delta_dive_revenue.x-delta_dive_revenue.y)

mean_delta_dive_revenue_diff <- diff_data %>% group_by(dive_tax) %>% summarise(mean_delta_dive_revenue_diff = mean(difference))
panel2_diff <- diff_data %>% ggplot(aes(x=dive_tax,y=difference/1e9)) + geom_line(aes(group=run),size=1, alpha=0.01)+
  geom_line(data=mean_delta_dive_revenue_diff, aes(x=dive_tax, y=mean_delta_dive_revenue_diff/1e9))+theme_classic()+labs(x ="User fee per dive\n(US$)", y = "Difference\n(billion US$)")+ theme(legend.position = "none")+
  theme(panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),legend.background = element_rect(fill='transparent'),legend.box.background = element_rect(fill='transparent')) +
  ylim(0,max(diff_data$difference/1e9))
panel2_diff

panel2_inset <- panel2+annotation_custom(grob = ggplotGrob(panel2_diff), xmin=75, xmax=200, ymin=min_ref, ymax=max_val) 

#change consumer surplus plot
mean_delta_consumer_surplus <- explore_user_fee_merged %>% select(dive_tax, delta_consumer_surplus, Scenario) %>% group_by(Scenario,dive_tax) %>% summarise(mean_delta_consumer_surplus = mean(delta_consumer_surplus))
panel3 <- explore_user_fee_merged %>% ggplot(aes(x=dive_tax,y=delta_consumer_surplus/1e9,color=Scenario, group=interaction(Scenario,run)))+geom_line(size=1, alpha=0.01)+
  geom_line(data=mean_delta_consumer_surplus, aes(x=dive_tax, y=mean_delta_consumer_surplus/1e9, group=Scenario, linetype=Scenario))+theme_classic()+labs(x ="User fee per dive\n(US$)", y = "\u0394 consumer surplus\n(billion US$)")+ theme(legend.position = "none")   
panel3

max_val <- max(explore_user_fee_merged$delta_consumer_surplus/1e9)
min_val <- min(explore_user_fee_merged$delta_consumer_surplus/1e9)
min_ref <- min_val+0.6*(max_val-min_val)

#difference plot panel 3
data1<-explore_user_fee_merged %>% select(dive_tax, delta_consumer_surplus, Scenario, run) %>% filter(Scenario =="With MPA")
data2<-explore_user_fee_merged %>% select(dive_tax, delta_consumer_surplus, Scenario, run) %>% filter(Scenario =="No MPA")
diff_data <- left_join(data1,data2, by=c("dive_tax","run")) %>% mutate(difference = delta_consumer_surplus.x-delta_consumer_surplus.y)

mean_delta_consumer_surplus_diff <- diff_data %>% group_by(dive_tax) %>% summarise(mean_delta_consumer_surplus_diff = mean(difference))
panel3_diff <- diff_data %>% ggplot(aes(x=dive_tax,y=difference/1e9)) + geom_line(aes(group=run),size=1, alpha=0.01)+
  geom_line(data=mean_delta_consumer_surplus_diff, aes(x=dive_tax, y=mean_delta_consumer_surplus_diff/1e9))+theme_classic()+labs(x ="User fee per dive\n(US$)", y = "Difference\n(billion US$)")+ theme(legend.position = "none")+
  theme(panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),legend.background = element_rect(fill='transparent'),legend.box.background = element_rect(fill='transparent')) +
  ylim(0,max(diff_data$difference/1e9))
panel3_diff

panel3_inset <- panel3+annotation_custom(grob = ggplotGrob(panel3_diff), xmin=75, xmax=200, ymin=min_ref, ymax=max_val) 

#change number of dive plot
mean_delta_number_dives <- explore_user_fee_merged %>% select(dive_tax, delta_number_dives, Scenario) %>% group_by(Scenario,dive_tax) %>% summarise(mean_delta_number_dives = mean(delta_number_dives))
panel4 <- explore_user_fee_merged %>% ggplot(aes(x=dive_tax,y=delta_number_dives/1e6,color=Scenario, group=interaction(Scenario,run)))+geom_line(size=1, alpha=0.01)+
  geom_line(data=mean_delta_number_dives, aes(x=dive_tax, y=mean_delta_number_dives/1e6, group=Scenario, linetype=Scenario))+theme_classic()+labs(x ="User fee per dive\n(US$)", y = "\u0394 # dives\n(million)")+ theme(legend.position = "none")   
panel4

max_val <- max(explore_user_fee_merged$delta_number_dives/1e6)
min_val <- min(explore_user_fee_merged$delta_number_dives/1e6)
min_ref <- min_val+0.6*(max_val-min_val)

#difference plot panel 4
data1<-explore_user_fee_merged %>% select(dive_tax, delta_number_dives, Scenario, run) %>% filter(Scenario =="With MPA")
data2<-explore_user_fee_merged %>% select(dive_tax, delta_number_dives, Scenario, run) %>% filter(Scenario =="No MPA")
diff_data <- left_join(data1,data2, by=c("dive_tax","run")) %>% mutate(difference = delta_number_dives.x-delta_number_dives.y)

mean_delta_number_dives_diff <- diff_data %>% group_by(dive_tax) %>% summarise(mean_delta_number_dives_diff = mean(difference))
panel4_diff <- diff_data %>% ggplot(aes(x=dive_tax,y=difference/1e6)) + geom_line(aes(group=run),size=1, alpha=0.01)+
  geom_line(data=mean_delta_number_dives_diff, aes(x=dive_tax, y=mean_delta_number_dives_diff/1e6))+theme_classic()+labs(x ="User fee per dive\n(US$)", y = "Difference\n(million)")+ theme(legend.position = "none")+
  theme(panel.background = element_rect(fill='transparent'),plot.background = element_rect(fill='transparent', color=NA),panel.grid.major = element_blank(),panel.grid.minor = element_blank(),legend.background = element_rect(fill='transparent'),legend.box.background = element_rect(fill='transparent'))+
  ylim(0,max(diff_data$difference/1e6))
panel4_diff

panel4_inset <- panel4+annotation_custom(grob = ggplotGrob(panel4_diff), xmin=75, xmax=200, ymin=min_ref, ymax=max_val) 

# extract legend
legend_b <- get_legend(panel1 + guides(color = guide_legend(nrow = 1)) + theme(legend.position = "bottom"))

##-- Plot FIGURE 4 with inset and save
plot_explore_user_fee_v1<- cowplot::plot_grid(panel1_inset,panel2_inset,panel3_inset,panel4_inset, ncol = 2, labels = "AUTO",rel_heights=c(1,1))
Fig4 <- cowplot::plot_grid(plot_explore_user_fee_v1, legend_b, ncol = 1, rel_heights=c(1,.05))
Fig4


##----FIGURE 3, consumer surplus and tax beneficiaries plot, with MPA + average user fee (53 USD per dive)
component_effect_name <- round(effect_name/average_user_fee,4)
component_biomass_effect <- round(mean(effect_biomass)/average_user_fee,4)
component_biodiversity_effect <- round(mean(effect_biodiversity)/average_user_fee,4)

user_fee_opt <- effect_name + effect_biomass + effect_biodiversity

p1_contribution <- data.frame(value = c(component_effect_name, component_biomass_effect, component_biodiversity_effect), Component = c("MPA name","Biomass","Biodiversity")) %>%
  ggplot(aes(x = "", y = value, fill = Component)) + geom_col(color = "black") +
  geom_text(aes(label = percent(value)), position = position_stack(vjust = 0.5)) +
  coord_polar(theta = "y") +  ggtitle("Drivers of MPA benefits")+
  theme_void() + theme(plot.title = element_text(hjust = 0.5))
p1_contribution #contribution of different components (name, biodiv, biomass) to WTP

#Proportion of foreign and local divers per region
#Load this csv file: local_vs_foreign_tourist_origins_by_region
diver_origin <- read.csv(here("data","dive","local_vs_foreign_tourist_origins_by_region.csv")) %>% dplyr::rename(Origin=origin)

#--consumer surplus beneficiaries by region and diver origin
#remove the pixels that are in MPA
ndives_data_v1 <- dives_input[dives_input$cell_id %in% divepixels_unprotected,]
ndives_data_v2 <- left_join(ndives_data_v1,cell_id_with_country_kat_withregion,by="cell_id") %>% select(cell_id,n_dives_extrap,n_dives_extrap_min,n_dives_extrap_max,region_fill)

#consumer_surplus_per_region <- ndives_data_v2 %>% mutate(consumer_surplus = 0.5*n_dives_extrap*(choke_price-price_per_dive_constant[1])) %>% group_by(region_fill) %>% summarise(consumer_surplus_per_region = sum(consumer_surplus)/1e6) %>% drop_na() %>% dplyr::rename(region=region_fill)
#this just tracks the beneficiaries of the change in consumer surplus due to MPA
consumer_surplus_per_region <- ndives_data_v2 %>% mutate(consumer_surplus = change_consumer_suplus) %>% group_by(region_fill) %>% summarise(consumer_surplus_per_region = sum(consumer_surplus)/1e6) %>% drop_na() %>% dplyr::rename(region=region_fill)

consumer_surplus_beneficiary <- left_join(diver_origin,consumer_surplus_per_region, by="region") %>% mutate(consumer_surplus = consumer_surplus_per_region*percent_avg/100)

consumer_surplus_beneficiary$percent_avg<-round(consumer_surplus_beneficiary$percent_avg)

df_sorted <- arrange(consumer_surplus_beneficiary, region, Origin) 

df_cumsum <- df_sorted %>% group_by(region) %>% mutate(label_ypos=cumsum(consumer_surplus))

p2_dissagregated <- df_cumsum %>% ggplot(aes(x=region, y=consumer_surplus, fill=Origin, label=scales::percent(percent_avg/100,accuracy = 1L))) +
  geom_bar(stat="identity") + geom_text(size = 3, position = position_stack(vjust = 0.5))+
  scale_fill_brewer(palette="Paired") + labs(x="",y="Consumer surplus\n(US$ million)")+  
  theme_minimal() + ggtitle("Consumer surplus beneficiaries\nby region and diver origin")+
  theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())

#-- consumer surplus beneficiaries by foreign and local
p2_development <- consumer_surplus_beneficiary  %>% group_by(Origin) %>% summarise(total_consumer_surplus=sum(consumer_surplus)) %>%
  mutate(countT= sum(total_consumer_surplus), Percent_Contribution = total_consumer_surplus/countT) %>%
  ggplot(aes(x = "", y = Percent_Contribution, fill = Origin)) + geom_col(color = "black") +
  geom_text(aes(label = percent(Percent_Contribution)), position = position_stack(vjust = 0.5)) +
  ggtitle("Consumer surplus beneficiaries") + coord_polar(theta = "y") +
  scale_fill_brewer(palette="Blues") + theme_void() + theme(plot.title = element_text(hjust = 0.5))

#tax revenue distribution
p3_development <- cell_developmentstatus %>% group_by(Classification) %>% summarise(Contribution=round(sum(tax_revenue)/10^6))%>%
  filter(Classification %in% c("Developed", "Developing")) %>% mutate(Percent_Contribution = Contribution/sum(Contribution)) %>%
  ggplot(aes(x = "", y = Percent_Contribution, fill = Classification)) + geom_col(color = "black") +
  geom_text(aes(label = percent(Percent_Contribution)), position = position_stack(vjust = 0.5)) +
  ggtitle("User fee revenue beneficiaries") + coord_polar(theta = "y") +
  scale_fill_brewer(palette="Reds") + theme_void() + theme(plot.title = element_text(hjust = 0.5))

##USER FEE REVENUE BY REGION BY DEV CATEGORY
generate_data <- cell_developmentstatus %>% group_by(region_fill, Classification) %>% summarise(total_tax_revenue=sum(tax_revenue)/10^6) %>% drop_na() %>% filter(Classification!="In transition")

# Create the barplot
p3_dissagregated <- ggplot(data=generate_data, aes(x=region_fill, y=total_tax_revenue, fill=Classification)) +
  geom_bar(stat="identity") + scale_fill_brewer(palette="Reds")+
  ggtitle("User fee revenue beneficiaries\nby region and country classification")+
  labs(x="",y="Potential user fee revenue\n(US$ million)")+  
  theme_minimal()+theme(axis.text.x = element_text(angle=90, vjust=0.5,hjust=1))

Fig3a <- cowplot::plot_grid(p1_contribution,p2_development,p3_development, ncol = 1, labels = "AUTO")
Fig3b <- cowplot::plot_grid(p2_dissagregated,p3_dissagregated , ncol = 1, labels = c('D', 'E'))
Fig3 <- cowplot::plot_grid(Fig3a, Fig3b, ncol=2)
Fig3

