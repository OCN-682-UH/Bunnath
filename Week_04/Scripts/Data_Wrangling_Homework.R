### Homework 1: Wrangling Penguin Data ###
### Created by: Zoe Sidana Bunnath ###
### Created on: 2025-09-16 ###

## Load libraries ###
library(palmerpenguins)
library(tidyverse)
library(here)
library(viridis)

### Load data ######
glimpse(penguins)
summary(penguins)

### Step 1: Mean and variance of body mass by species, island, sex ####
data3 <- penguins %>%
  drop_na(sex, body_mass_g) %>%                 # remove NAs in sex and body mass
  group_by(species, island, sex) %>%            # group by species, island, sex
  summarise(
    mean_body_mass = mean(body_mass_g, na.rm = TRUE),   # calculate mean
    variance_body_mass = var(body_mass_g, na.rm = TRUE) # calculate variance
  ) %>%
  select(species, island, sex, mean_body_mass, variance_body_mass)

View(data3)

### Step 2: Exclude males, calculate log body mass, and plot ####
penguins_female <- penguins %>%
  filter(sex != "male") %>%                      # exclude males
  mutate(log_mass = log(body_mass_g)) %>%        # calculate log body mass
  select(species, island, sex, log_mass)         # keep only needed columns

### Plot: Boxplot of log body mass by species and island ####
penguin_plot <- ggplot(penguins_female, aes(x = species, y = log_mass, fill = species)) +
  geom_boxplot() +   # boxplot is good for distribution of continuous data
  scale_fill_viridis(discrete = TRUE, option = "C") +
  labs(
    title = "Log Body Mass of Female Penguins by Species",
    x = "Species",
    y = "Log Body Mass (g)"
  ) +
  theme_classic() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    axis.title.x = element_text(size = 14, face = "bold"),
    axis.title.y = element_text(size = 14, face = "bold")
  )

### Save the plot to output folder ####
ggsave(here("Week_04", "output", "penguin_logmass_plot.png"),
       plot = penguin_plot, width = 7, height = 5)