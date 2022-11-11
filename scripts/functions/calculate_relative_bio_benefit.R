### -----------------------------------------------------------
# 
# This script contains the function that calculates the relative biodiversity benefit from protecting a series of cells
# 
### -----------------------------------------------------------

calculate_relative_bio_benefit <- function(is_mpa_vect, 
                                           v_out_matrix,
                                           v_in_matrix, 
                                           weights, 
                                           z_bio, 
                                           bau_benefit,
                                           total_benefit_diff){
  
  protected_cells <- matrix(is_mpa_vect, nrow = 1, ncol = length(is_mpa_vect))
  
  sum_v_outs <- v_out_matrix[!protected_cells, ] %>% 
    colSums(na.rm = T)
  
  sum_v_in <- v_in_matrix[protected_cells, ]%>% 
    colSums(na.rm = T)
  
  b_benefit <- sum(weights*(sum_v_in + sum_v_outs)^z_bio)
  
  relative_b_benefit <- b_benefit#(1+((b_benefit - bau_benefit)/bau_benefit))*0.5326662#(b_benefit - bau_benefit)/total_benefit_diff #
  
  return(relative_b_benefit)
}