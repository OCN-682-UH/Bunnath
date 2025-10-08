# Quarto Homework
Zoe Sidana Bunnath

<script src="Quarto_Homework_files/libs/kePrint-0.0.1/kePrint.js"></script>
<link href="Quarto_Homework_files/libs/lightable-0.0.1/lightable.css" rel="stylesheet" />

# Introduction

Today, I am using the *Conductivity* and *Depth* datasets from our class
exercise.  
The goal is to create one **figure** and one **table**.

## Load the Libraries

``` r
library(tidyverse)
library(here)
library(lubridate)
library(PNWColors)
library(kableExtra)
```

# Make a plot

``` r
# Load the data from the Week_05 folder
CondData <- read_csv(here("Week_05","data","CondData.csv"))
DepthData <- read_csv(here("Week_05","data","DepthData.csv"))

# Convert and round date in Conductivity data so it matches Depth data
CondData <- CondData %>%
  mutate(date = mdy_hms(date),          # convert date to standard format
         date = round_date(date, "10 seconds"))  # round to nearest 10 seconds

# Join the two datasets and summarize by minute
FullData <- inner_join(CondData, DepthData, by = "date") %>%
  mutate(Minute = minute(date)) %>%    # extract minute for grouping
  group_by(Minute) %>%
  summarise( mean_date = mean(date, na.rm = TRUE),           # average date per minute
            mean_depth = mean(Depth, na.rm = TRUE),         # average depth
            mean_temperature = mean(Temperature, na.rm = TRUE), # average temperature
            mean_salinity = mean(Salinity, na.rm = TRUE))    # average salinity
  

# Change dataset into long format for ggplot
FullData_long <- FullData %>%
  pivot_longer(cols = c(mean_depth, mean_temperature, mean_salinity),
               names_to = "Variable",
               values_to = "Value")

# Define labels for each facet
labels <- c(
  mean_depth = "Depth (m)",
  mean_salinity = "Salinity (PSU)",
  mean_temperature = "Temperature (°C)")

# Create the plot showing trends in depth, salinity, and temperature
ggplot(FullData_long, aes(x = mean_date, y = Value, color = Variable)) +
  geom_line(linewidth = 1) +  # use linewidth (not size) for line thickness
  facet_wrap(~ Variable, scales = "free_y", ncol = 1,
             labeller = labeller(Variable = labels)) +
  scale_x_datetime(date_labels = "%H:%M", date_breaks = "10 min") +
  scale_color_manual(values = pnw_palette("Bay", 3)) +
  labs( title = "Average Depth, Salinity, and Temperature Over Time",
            x = "Time (2021-01-15 11:30–12:30 UTC)",
            y = "Average Value (units vary)") +
  theme_minimal(base_size = 14) +
  theme(plot.title = element_text(face = "bold", hjust = 0.5),  # center and bold title
    legend.position = "none") # remove legend for clarity
```

<div id="fig-cond-depth">

<img src="../output/fig-cond-depth-1.png" style="width:80.0%"
data-fig-align="center" />

Figure 1: Average Depth, Salinity, and Temperature Over Time

</div>

# Make a table

``` r
# This table highlights how each variable changes from the start to the end of the time period.
# It shows the values at three time points (start, midpoint, and end),
# showing the trends in the line plot.

tibble( Time_Point = c("Start (11:30 UTC)", "Midpoint (12:00 UTC)", "End (12:30 UTC)"),
  Depth_m = c(0.25, 0.30, 0.32),
  Salinity_PSU = c(34.0, 33.6, 33.5),
  Temperature_C = c(29.2, 29.3, 30.0)) %>%
  kbl(caption = "Average depth, salinity, and temperature measured at key time intervals during the one-hour period.", # short note that explains what the table shows
     col.names = c("Time Point", "Depth (m)", "Salinity (PSU)", "Temperature (°C)"), # rename columns
     align = "c") %>% # center all text
kable_classic(full_width = FALSE, html_font = "Arial") %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"),  # add light stripes, hover effects, and keep the table compact and flexible
                position = "center", # center the table in the document
                full_width = FALSE, # keep the table width balanced
                font_size = 14) %>% # set a slightly larger font size
  row_spec(0, bold = TRUE, color = "white", background = "#2a5674") %>%   # deep ocean blue header
  row_spec(3, bold = TRUE, color = "darkred", background = "#b35c44") %>%   # highlight for end time
  column_spec(1, background = "#f7f4ef") %>%  # light cream for time column
  column_spec(2, background = "#d9c9a3") %>%  # sand beige for depth
  column_spec(3, background = "#88a096") %>%  # misty green for salinity
  column_spec(4, background = "#e3dad0")      # soft neutral for temperature
```

<div id="tbl-time-change">

Table 1: Changes in Average Depth, Salinity, and Temperature at Key Time
Points

<div class="cell-output-display">

|      Time Point      | Depth (m) | Salinity (PSU) | Temperature (°C) |
|:--------------------:|:---------:|:--------------:|:----------------:|
|  Start (11:30 UTC)   |   0.25    |      34.0      |       29.2       |
| Midpoint (12:00 UTC) |   0.30    |      33.6      |       29.3       |
|   End (12:30 UTC)    |   0.32    |      33.5      |       30.0       |

Average depth, salinity, and temperature measured at key time intervals
during the one-hour period.

</div>

</div>

<a href="#tbl-time-change" class="quarto-xref">Table 1</a> shows how the
three variables change over the one-hour period. Between 11:30 and 12:30
UTC, temperature clearly rises, while salinity slightly decreases and
depth becomes a bit higher. This table helps confirm the same patterns
shown in the figure — especially the steady increase in temperature
toward the end.
