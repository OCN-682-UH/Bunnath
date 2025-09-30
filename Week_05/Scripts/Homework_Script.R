### I am working on the Conductivity data and Dept Data for the Homework ###
### Created by: Zoe Sidana Bunnath ###
### Updated on: 2025-09-30

### Load Libraries ###
library(tidyverse)
library(here)
library(lubridate)
library(dplyr)
library(ggplot2)
library(tidyr)
library(PNWColors)

### Load data ###

CondData<-read_csv(here("Week_05","data","CondData.csv"))
DepthData<-read_csv(here("Week_05","data","DepthData.csv"))

glimpse(CondData)
glimpse(DepthData)
view(CondData)
view(DepthData)

CondData<- CondData %>% 
  mutate(date = mdy_hms(date), #make the data match the depth dataset format of ymd
       date = round_date(date, "10 seconds")) #round to the nearest 10 seconds

FullData<- inner_join(CondData, DepthData, by = "date")%>% #Join the two dataframes and only exact matches are kept
  mutate(Minute = minute(date))%>% #create a minute column
  group_by(Minute)%>% #group the data by minute
  summarise(mean_date = mean(date, na.rm = TRUE), #average date
            mean_depth = mean(Depth, na.rm = TRUE ), #average depth
            mean_temperature = mean(Temperature, na.rm = TRUE), #average temperature
            mean_salinity = mean(Salinity, na.rm = TRUE)) #find the average of the salinity

#Save the summary file in CSV format
write_csv(FullData, here("Week_05","Output","average_summary.csv"))


# Change data set into long format for ggplot

FullData_long<- FullData %>%
  pivot_longer(cols = c(mean_depth, 
                        mean_temperature, 
                        mean_salinity),
               names_to = "Variable", #new column for variable names
               values_to = "Value") #new column for values

# Define labels for facets
labels <- c(mean_depth = "Depth (m)",
            mean_salinity = "Salinity (PSU)",
            mean_temperature = "Temperature (°C)")

# Make a plot
ggplot(FullData_long, aes(x = mean_date, y = Value, color = Variable)) + # map date to x, value to y, and color the variables
  geom_line(size = 1) +
  facet_wrap(~ Variable, scales = "free_y", ncol = 1, # one facet per variable, each with own y-axis
              labeller = labeller(Variable = labels)) + # custom facet labels (Depth (m), Temp (°C), Salinity (PSU))
  scale_x_datetime(date_labels = "%H:%M",   # # show time in hours:minutes
                    date_breaks = "10 min" ) +  # tick every 10 minutes

  scale_color_manual(values = pnw_palette("Bay", 3)) +  # use color from PNW palette
  labs(title = "Average Depth, Salinity, and Temperature over Time", # main title
        x = "Time (2021-01-15 11:30–12:30 UTC)", # x-axis label
        y = "Average Value (units vary by facet: m, PSU, °C)" ) + # y-axis label
  
  theme(plot.title = element_text(face = "bold", hjust = 0.5), # bold + centered title
    legend.position = "none", # remove legend
    panel.background = element_rect(fill = "gray90", colour = NA), # light gray plot background
    strip.background = element_rect(fill = "beige", colour = NA), # facet strip background
    panel.grid.major = element_line(color = "white"), # white major grid lines
    panel.grid.minor = element_line(color = "white")) # white minor grid lines

#save plot
ggsave(here("Week_05","Output","Average Depth, Salinity, and Temperature over Time_Plot.png"), width = 8, height = 6)
  