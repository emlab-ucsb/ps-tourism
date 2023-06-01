#WTP analysis

##--crowding effect
#load the data

crowding_data <- read.csv(here("data","crowding_data.csv"))
head(crowding_data)

#1.) plot using change number of divers
wtp_crowding_plot_v1 <- ggplot(crowding_data, aes(x=change_number_divers, y= percent_change_WTP*100)) + geom_point() + geom_smooth(method = "lm",formula=y~0+x) + 
  labs(x = "Change in dive numbers", y = "% change in WTP") + theme_classic()

#slope
coef(lm(percent_change_WTP*100 ~ 0+ change_number_divers, data = crowding_data))

#save the figure
ggsave(here("figures","supplementary","wtp_crowding_change_ndive.jpg"),wtp_crowding_plot_v1, width = 8, height = 6, units = "cm")

#2.)plot using percent change n divers
wtp_crowding_plot_v2 <- ggplot(crowding_data, aes(x=percent_change_ndiver, y= percent_change_WTP*100)) + geom_point() + geom_smooth(method = "lm",formula=y~0+x) + 
  labs(x = "% change in dive numbers", y = "% change in WTP") + theme_classic()

#slope
coef(lm(percent_change_WTP*100 ~ 0+ percent_change_ndiver, data = crowding_data))

#save the figure
ggsave(here("figures","supplementary","wtp_crowding_change_percentdive.jpg"),wtp_crowding_plot_v2, width = 8, height = 6, units = "cm")
