func_evaluate_divefee<-function(wtp_combined,base_number_dive,choke_price,price_per_dive,congestion){
  
  if (congestion==1){
    #dive tax by 1$ increments
    index <- 0
    for(dive_tax_scenario in 0:200){
      index <- index+1
      
      #shifted number of dives with MPA and tax
      delta_q_wMPA <- (wtp_combined - dive_tax_scenario)*base_number_dive/(choke_price-price_per_dive)
      shifted_number_dive_wMPA <- base_number_dive + delta_q_wMPA
      #if negative, it will be zero because there is no negative diving
      shifted_number_dive_wMPA[shifted_number_dive_wMPA<0]<-0
      
      #with MPA
      change_dive_revenue_wMPA <- price_per_dive*(shifted_number_dive_wMPA - base_number_dive) # in USD
      change_consumer_suplus_wMPA <- (0.5*((shifted_number_dive_wMPA^2)/base_number_dive)*(choke_price-price_per_dive))-(0.5*base_number_dive*(choke_price-price_per_dive))
      tax_revenue_wMPA <- dive_tax_scenario*shifted_number_dive_wMPA
      explore_user_fee_wMPA[[index]]<-c(dive_tax_scenario,sum(shifted_number_dive_wMPA)-sum(base_number_dive),sum(change_dive_revenue_wMPA),sum(change_consumer_suplus_wMPA),sum(tax_revenue_wMPA))
      
      #no MPA
      delta_q_noMPA <- (-dive_tax_scenario)*base_number_dive/(choke_price-price_per_dive)
      shifted_number_dive_noMPA <- base_number_dive + delta_q_noMPA
      #if negative, it will be zero because there is no negative diving
      shifted_number_dive_noMPA[shifted_number_dive_noMPA<0]<-0
      
      change_dive_revenue_noMPA <- price_per_dive*(shifted_number_dive_noMPA - base_number_dive) # in USD
      change_consumer_suplus_noMPA <- (0.5*((shifted_number_dive_noMPA^2)/base_number_dive)*(choke_price-price_per_dive))-(0.5*base_number_dive*(choke_price-price_per_dive))
      tax_revenue_noMPA <- dive_tax_scenario*shifted_number_dive_noMPA
      explore_user_fee_noMPA[[index]]<-c(dive_tax_scenario,sum(shifted_number_dive_noMPA)-sum(base_number_dive),sum(change_dive_revenue_noMPA),sum(change_consumer_suplus_noMPA),sum(tax_revenue_noMPA))
    }
  }else{
    
    #note::: need to think more about this...
    
    index <- 0
    for(dive_tax_scenario in 0:200){
      index <- index+1
      
      #shifted number of dives with crowding and with tax
      delta_q_wMPA <- (wtp_combined - dive_tax_scenario)*base_number_dive/(choke_price-price_per_dive)
      shifted_number_dive_wMPA <- base_number_dive + delta_q_wMPA#*(1-(reduce_wtp_crowd*dive_group_size*delta_q_crowding/base_number_dive))
      #if negative, it will be zero because there is no negative diving
      shifted_number_dive_wMPA[shifted_number_dive_wMPA<0]<-0
      
      #with MPA
      change_dive_revenue_wMPA <- price_per_dive*(shifted_number_dive_wMPA - base_number_dive) # in USD
      #new_choke_price_withcrowding <- (parameter_a+((wtp_combined-dive_tax)*base_number_dive/(choke_price-price_per_dive)))/parameter_b
      #change_consumer_suplus_withcrowding <- (0.5*shifted_number_dive_withcrowding*(new_choke_price_withcrowding-price_per_dive))-(0.5*base_number_dive*(choke_price-price_per_dive))
      change_consumer_suplus_wMPA <- (0.5*((shifted_number_dive_wMPA^2)/base_number_dive)*(choke_price-price_per_dive))-(0.5*base_number_dive*(choke_price-price_per_dive))
      tax_revenue_wMPA <- dive_tax_scenario*shifted_number_dive_wMPA
      explore_user_fee_wMPA[[index]]<-c(dive_tax_scenario,sum(shifted_number_dive_wMPA)-sum(base_number_dive),sum(change_dive_revenue_wMPA),sum(change_consumer_suplus_wMPA),sum(tax_revenue_wMPA))
      
      #no MPA
      delta_q_noMPA <- (-dive_tax_scenario)*base_number_dive/(choke_price-price_per_dive)
      shifted_number_dive_noMPA <- base_number_dive + delta_q_noMPA#*(1-(reduce_wtp_crowd*dive_group_size*delta_q_crowding_noMPA/base_number_dive))
      #if negative, it will be zero because there is no negative diving
      shifted_number_dive_noMPA[shifted_number_dive_noMPA<0]<-0
      
      #shifted_number_dive_noMPA <- parameter_a - parameter_b*price_per_dive + ((- dive_tax)*base_number_dive/(choke_price-price_per_dive))
      change_dive_revenue_noMPA <- price_per_dive*(shifted_number_dive_noMPA - base_number_dive) # in USD
      #new_choke_price_noMPA <- (parameter_a+((-dive_tax)*base_number_dive/(choke_price-price_per_dive)))/parameter_b
      #change_consumer_suplus_noMPA <- (0.5*shifted_number_dive_withcrowding_noMPA*(new_choke_price_noMPA-price_per_dive))-(0.5*base_number_dive*(choke_price-price_per_dive))
      change_consumer_suplus_noMPA <- (0.5*((shifted_number_dive_noMPA^2)/base_number_dive)*(choke_price-price_per_dive))-(0.5*base_number_dive*(choke_price-price_per_dive))
      tax_revenue_noMPA <- dive_tax_scenario*shifted_number_dive_noMPA
      explore_user_fee_noMPA[[index]]<-c(dive_tax_scenario,sum(shifted_number_dive_noMPA)-sum(base_number_dive),sum(change_dive_revenue_noMPA),sum(change_consumer_suplus_noMPA),sum(tax_revenue_noMPA))
    }
    
  }
  
  #this is the only line that needs to be changed depending on how crowding is accounted for
  explore_user_fee_wMPA_merged <- do.call("rbind",explore_user_fee_wMPA) %>% as.data.frame() %>% setNames(., c("dive_tax", "delta_number_dives", "delta_dive_revenue","delta_consumer_surplus","tax_revenue"))
  explore_user_fee_wMPA_merged$Scenario<-"With MPA"
  
  explore_user_fee_noMPA_merged <- do.call("rbind",explore_user_fee_noMPA) %>% as.data.frame() %>% setNames(., c("dive_tax", "delta_number_dives", "delta_dive_revenue","delta_consumer_surplus","tax_revenue"))
  explore_user_fee_noMPA_merged$Scenario<-"No MPA"
  
  explore_user_fee_merged <- rbind(explore_user_fee_wMPA_merged,explore_user_fee_noMPA_merged)
  
  
  return(explore_user_fee_merged)
  silence(TRUE)
}