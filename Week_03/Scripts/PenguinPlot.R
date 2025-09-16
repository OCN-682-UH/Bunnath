### Today I am going to plot penguin data ####
### Created by: Zoe Sidana Bunnath ############
### Updated on: 2025-09-14 ####################


#### Load Libraries ######
library(palmerpenguins)
library(tidyverse)
library(here)
library(PNWColors)
library(ggthemes)


### Load data #####
# The data is part of the package and is called penguins
# How else besides glimpse can we inspect the data?
glimpse(penguins)


## data analysis section ####

plot1<-ggplot(data = penguins, 
       mapping = aes(x=bill_depth_mm, # - bill = depth
                     y=bill_length_mm,
                     group = species,
                     color = species))+
geom_point()+
geom_smooth(method = "lm")+
labs(x = "Bill depth (mm)",
      y = "Bill length (mm)",
)+
scale_color_viridis_d()+
scale_x_continuous(breaks = c (14, 17, 21),
                  labels =c ("low","medium","high"))+
  #scale_color_manual(values = c("orange","purple","red")) - this is the normal way to change the color
 scale_color_manual(values = pnw_palette("Bay",3))+
  theme_bw()+
  theme(axis.title = element_text(size = 20, color = "grey30"),
        panel.background = element_rect(fill = "linen"), 
        legend.title = element_text(face = "bold"))

ggsave(here("Week_03","output","penguin.pdf"), width = 7, height = 5)

plot1
        
ggplot(diamonds, aes(carat, price)) +
  geom_point()+
  coord_trans(x = "log10", y = "log10")
