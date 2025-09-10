###This is the script to visualize penquins data ###
### Created by: Zoe Sidana Bunnath ###
### Created on: 2025-09-09 ###


### library ####
install.packages("palmerpenguins")
library(palmerpenguins)
library(tidyverse)
glimpse(penguins)

### ggplot ####
ggplot(data=penguins, 
       mapping = aes(x = bill_depth_mm,
                     y = bill_length_mm,
                     color = species)) +
  geom_point()+
  labs(title = "Bill depth and length",
       subtitle = "Dimensions for Adelie, Chinstrap, and Gentoo Penguins",
       x = "Bill depth (mm)", y = "Bill length (mm)",
       color = "Species",
       caption = "Source: Palmer Station LTER / palmerpenguins package")+
  scale_color_viridis_d()