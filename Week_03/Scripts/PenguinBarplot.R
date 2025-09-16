### I am going to plot penguin data using bar plot ####
### Created by: Zoe Sidana Bunnath ############
### Updated on: 2025-09-16 ####################


#### Load Libraries ######
library(palmerpenguins)
library(tidyverse)
library(here)
library(PNWColors)
library(ggthemes)


### Load data #####
# The data is part of the package and is called penguins
glimpse(penguins) # to get a glimpse of the penguin dataset

## data analysis section ####

# Barplot

ggplot(penguins, aes(x = island, fill = species)) +
  geom_bar() +
  labs(title = "Penguin Species by Island", # set label starting with main title
    x = "Island", #x-axis label
    y = "Number of penguins", #y-axis label
    fill = "Species" ) + #legend title
  scale_fill_manual(values = pnw_palette("Sailboat", 3)) + #uses the PNWColors package to get 3 colors from the “Sailboat” palette (since there are 3 penguin species)
  theme(plot.title = element_text(             # format the title
                     face = "bold",           # bold text
                     hjust = 0.5,             # center horizontally
                     size = 18 ),             # increase font size
  
        axis.title.x = element_text(          # format x-axis title ("Island")
                       size = 14,             # increase font size
                       face = "bold"  ),      # bold
 
        axis.title.y = element_text(           # format y-axis title ("Number of penguins")
                       size = 14,
                       face = "bold" ),
  
        axis.text.x = element_text(            # format x-axis tick labels ("Biscoe", etc.)
                      size = 12  ),           # increase font size
  
        axis.text.y = element_text(            # format y-axis tick labels (numbers)
                      size = 12 )) 
)

ggsave(here("Week_03","output","penguinbarplot.png"), width = 7, height = 5)
