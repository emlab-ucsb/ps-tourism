### ----------------------------------------------------------------------------
# General support script for tourism analysis
### ----------------------------------------------------------------------------

### Packages -----
library(countrycode) # Country name matching
library(here) # File path handling
library(janitor) # Column name handling
library(cowplot)
library(scales)
library(knitr)
library(viridis)
library(maps)
library(tidyverse)
library(rnaturalearth)
library(rnaturalearthdata)
library(sf)
library(sp)
library(raster)
library(broom)
library(lubridate)

# Code chunk defaults
knitr::opts_chunk$set(echo = FALSE, error = FALSE, message = FALSE, warning = FALSE, include = FALSE)

### Directories -----
# Unfortunately these paths are not general because I haven't fixed the issue caused by the latest GD update
emlab_project_dir <- "/Users/kat/Library/CloudStorage/GoogleDrive-kmillage@ucsb.edu/Shared\ drives/emlab/projects/current-projects/ps-tourism"

emlab_data_dir <- "/Users/kat/Library/CloudStorage/GoogleDrive-kmillage@ucsb.edu/Shared\ drives/emlab/data"

### Maps -----------------------------------------------------------------------
# Basic mapping theme
my_theme_map <- function(base_size = 12, base_family = "") {
  
  theme_bw(base_size = base_size, base_family = base_family) %+replace% 
    theme(axis.line = element_blank(), 
          axis.text = element_blank(),
          axis.ticks = element_blank(), 
          axis.title = element_blank(),
          panel.background = element_blank(), 
          panel.border = element_blank(),
          panel.spacing = unit(0, "lines"), 
          panel.grid = element_line(color = "transparent"),
          plot.background = element_blank(),
          legend.position = 'bottom', 
          legend.text = element_text(size = 10),
          legend.text.align = 0,
          legend.key.height = unit(0.01, 'npc'),
          legend.key.width = unit(0.05, 'npc'),
          legend.background =  element_blank(),
          legend.title = element_text(hjust = 0.5),
          legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit ='cm'))
}

# Save high resolution maps (larger size than plots)
save_maps <- function(plot_name, file_name){ 
  
  ggsave(filename = file_name,
         plot = plot_name,
         bg = 'transparent',
         units = "in",
         width = 7,
         height = 7, 
         dpi = 300)
  
}

### Plots ----------------------------------------------------------------------
# Basic plot theme 
theme_basic <- function(){
  
  graph_theme <- ggplot2::theme_minimal() + 
    ggplot2::theme(panel.border = element_blank(), 
                   #panel.background = element_rect(fill = "#f7f7f7", color = NA), 
                   panel.background = element_rect(fill = "transparent",colour = NA), 
                   plot.background = element_rect(fill = "transparent",colour = NA),
                   panel.grid.minor=element_blank(),
                   legend.position = "bottom", 
                   legend.box = "vertical", 
                   legend.key.height = unit(3, "mm"), 
                   legend.key.width = unit(20, "mm"),
                   legend.title.align = 0.5,
                   text = element_text(family = "Roboto", color = "#363c4c", size = 8),
                   legend.text = element_text(family = "Roboto", color = "#848b9b", size = 8), 
                   legend.title = element_text(family = "Roboto Bold", color = "#363c4c", size = 8), 
                   plot.title = element_text(family = "Roboto Bold", color = "#363c4c", size = 10), 
                   plot.subtitle = element_text(family = "Roboto", color = "#363c4c", size = 10), 
                   axis.title = element_text(family = "Roboto Bold", color = "#363c4c", size = 8), 
                   axis.text = element_text(family = "Roboto", color = "#848b9b", size = 6),
                   strip.text = element_text(family = "Roboto Bold", color = "#363c4c", size = 8))
                   #axis.title.y = element_text(angle=0, vjust = 0.5))
}

# Main text plot theme
theme_main_text <- function(){
  
  graph_theme <- ggplot2::theme_minimal() + 
    ggplot2::theme(panel.border = element_blank(), 
                   panel.background = element_rect(fill = "transparent",colour = NA), 
                   plot.background = element_rect(fill = "transparent",colour = NA),
                   panel.grid.minor=element_blank(),
                   legend.key.height = unit(3, "mm"), 
                   legend.key.width = unit(20, "mm"),
                   legend.title.align = 0.5,
                   text = element_text(family = "Roboto", color = "#000000", size = 10),
                   legend.text = element_text(family = "Roboto", color = "#000000", size = 10), 
                   legend.title = element_text(family = "Roboto Bold", color = "#000000", size = 10), 
                   plot.title = element_text(family = "Roboto Bold", color = "#000000", size = 12), 
                   plot.subtitle = element_text(family = "Roboto", color = "#000000", size = 12), 
                   axis.title = element_text(family = "Roboto Bold", color = "#000000", size = 10), 
                   axis.text = element_text(family = "Roboto", color = "#000000", size = 10),
                   strip.text = element_text(family = "Roboto Bold", color = "#000000", size = 10))
}

# Save high resolution plots
save_plots <- function(plot_name, file_name, width_in = 5, height_in = 4){ 
  
  ggsave(filename = file_name,
         plot = plot_name,
         bg = 'transparent',
         units = "in",
         width = width_in,
         height = height_in, 
         dpi = 300)
  
}

### Mapping --------------------------------------------------------------------
# Basic plot theme for maps - no gridlines or axis text
theme_basic_map <- function(){
  
  graph_theme <- ggplot2::theme_minimal() + 
    ggplot2::theme(panel.border = element_blank(), 
                   panel.background = element_rect(fill = "transparent",colour = NA), 
                   plot.background = element_rect(fill = "transparent",colour = NA),
                   panel.grid.minor=element_blank(),
                   #panel.grid.major=element_blank(),
                   legend.position = "bottom", 
                   legend.box = "vertical", 
                   legend.key.height = unit(3, "mm"), 
                   legend.key.width = unit(20, "mm"),
                   legend.title.align = 0.5,
                   legend.text = element_text(family = "Roboto", color = "#000000", size = 10), 
                   legend.title = element_text(family = "Roboto Bold", color = "#000000", size = 10), 
                   plot.title = element_text(family = "Roboto Bold", color = "#363c4c", size = 10), 
                   plot.subtitle = element_text(family = "Roboto", color = "#363c4c", size = 10), 
                   axis.title = element_blank(), 
                   axis.text = element_blank(),
                   strip.text = element_text(family = "Roboto Bold", color = "#363c4c", size = 8),
                   axis.title.y = element_text(angle=0, vjust = 0.5))
}

# Function to bin lat/lon data based on specified bin size - returns the coordinate of the center of the bin
bin_spatial_data <- function(coordinate, bin_size){
  floor(coordinate/bin_size)*bin_size + (0.5*bin_size)
}

# General map layer for plotting
map_layer <- geom_map(data = map_data("world"), map = map_data("world"), aes(long, lat, map_id = region), color = "white", fill = "black", size = 0.1)

# Mollewide map layer for plotting 
prj_moll <- "+proj=moll +lon_0=0 +x_0=0 +y_0=0 +ellps=WGS84"

# Get natural earth data for countries
world_moll <- ne_countries(scale = "medium", returnclass = "sf") %>%
  sf::st_transform(crs = st_crs(prj_moll))

map_layer_moll <- geom_sf(data = world_moll, aes(group = admin), color = "black", fill = "grey80", size = 0.1)

### Tables ---------------------------------------------------------------------
# Basic table theme
basic_table_html <- function(input_data, column_names, fig_caption, shrink=F, longtable=F, digits = 3){
  
  # Require/load packages to make the table
  require(kableExtra)
  library(tidyverse)
  
  # Defaults: shrink = F, longtable = F
  # Normal output
  if(shrink == FALSE & longtable == FALSE){
    
    output_table <- kableExtra::kable(input_data,
                                      col.names = column_names,
                                      caption = fig_caption,
                                      format = "html",
                                      digits = digits,
                                      align = "l") %>% 
      kableExtra::row_spec(0, bold = T) %>% 
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "responsive"), 
                                full_width = F) %>% 
      kableExtra::kable_classic()
    
    return(output_table)
  }
  
  # If shrink = T, longtable = F (same output as shrink = F & longtable = T)
  if(shrink == TRUE & longtable == FALSE) {
    
    output_table <- kableExtra::kable(input_data,
                                      col.names = column_names,
                                      caption = fig_caption,
                                      format = "html",
                                      digits = digits) %>% 
      kableExtra::row_spec(0, bold = T) %>% 
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "responsive"),
                                fixed_thead = T) %>% 
      kableExtra::scroll_box(width = "100%", height = "400px") %>% 
      kableExtra::kable_classic()
    
    return(output_table)
    
  }
  
  # If shrink = F, longtable = T (same output as shrink = T & longtable = F)
  if(shrink == FALSE & longtable == TRUE) {
    
    output_table <- kableExtra::kable(input_data,
                                      col.names = column_names,
                                      caption = fig_caption,
                                      format = "html",
                                      digits = digits) %>% 
      kableExtra::row_spec(0, bold = T) %>% 
      kableExtra::kable_styling(bootstrap_options = c("striped", "hover", "responsive"),
                                fixed_thead = T) %>% 
      kableExtra::scroll_box(width = "100%", height = "400px") %>% 
      kableExtra::kable_classic()
    
    return(output_table)
    
  }
  
  # Longtable = T, shrink = T
  if(shrink == TRUE & longtable == TRUE){
    
    print("Longtable and shrink output the same HTML table, try using just shrink = T or longtable = T to add a scroll box.")
    
  }
  
}

# Save high resolution table
save_table <- function(table_name, file_name){
  
  save_kable(x = table_name,
             file = file_name,
             density=300,
             zoom=4)
}