### script to make Whittaker plot of ForestGEO sites
### Kristina Anderson-Teixeira
### August 2025

## INSTALL AND LOAD NECESSARY PACKAGES
#install.packages("readr")
#install.packages("devtools")
#devtools::install_github("valentinitnelav/plotbiomes") 
#install.packages("ggplot2")

library(readr)
library(plotbiomes)
library(ggplot2)

## SETWD
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## READ FORESTGEO SITE DATA FROM GITHUB (ForestGEO_site_data.csv; this repository)

# Read ForestGEO_site_data.csv file directly from GitHub
site_data <- read_csv("https://raw.githubusercontent.com/forestgeo/Site-Data/master/basic%20site%20data/ForestGEO_site_data.csv")


# Extract climate data
MAT_MAP <- site_data[,c("MAT", "MAP")]
MAT_MAP <- na.omit(MAT_MAP)
MAT_MAP$MAP <- MAT_MAP$MAP / 10  # mm to cm


## CREATE WHITTAKER PLOT

whittaker_plot<- whittaker_base_plot() +
  # add the temperature - precipitation data points
  geom_point(data = MAT_MAP, 
             aes(x = MAT, 
                 y = MAP), 
             size   = 3,
             shape  = 21,
             colour = "gray95", 
             fill   = "black",
             stroke = 1,
             alpha  = 0.5) +
  theme_bw()

ggsave("ForestGEO_Whittaker_Plot.png", plot = whittaker_plot,
       width = 10, height = 7, dpi = 300)
