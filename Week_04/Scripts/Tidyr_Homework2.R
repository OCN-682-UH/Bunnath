### I am going to tidy the biogeochemistry data from Hawaii ####
### Created by: Zoe Sidana Bunnath #############
### Updated on: 2025-09-23 ####################

#### Load Libraries ######
library(tidyverse)   # load tidyverse for data wrangling and plotting
library(here)        # load here for portable file paths

#### Load data ######
ChemData <- read_csv(here("Week_04", "data", "chemicaldata_maunalua.csv"))  # import dataset
View(ChemData)       # open dataset in spreadsheet view (for checking)
glimpse(ChemData)    # show structure: columns, types, sample values

#### Clean data: remove all NAs ######
ChemData_clean <- ChemData %>% 
  drop_na()          # remove rows with missing values

#### Separate Tide_time column into Tide and Time ######
ChemData_clean <- ChemData_clean %>% 
  separate(col = "Tide_time", into = c("Tide", "Time"), sep = "_")  # split Tide_time into Tide and Time

#### Filter: keep only Low tide as subset (example) ######
ChemData_subset <- ChemData_clean %>% 
  filter(Tide == "Low")   # keep only rows where Tide is "Low"

#### Pivot: longer format for Temp_in and pH ######
ChemData_long <- ChemData_subset %>% 
  pivot_longer(cols = c(Temp_in, pH),  # reshape wide to long format
               names_to = "Variable",    # new column with variable names
               values_to = "Value")      # new column with measurement values

#### Summary statistics: mean and standard deviation ######
Summary_stats <- ChemData_long %>% 
  group_by(Variable) %>% 
  summarise(mean_val = mean(Value, na.rm = TRUE),  # calculate mean
            sd_val   = sd(Value, na.rm = TRUE))    # calculate standard deviation

#### Export summary statistics to output folder ######
write_csv(Summary_stats, here("Week_04", "output", "summary_stats.csv"))  # save summary table as CSV

#### Make a scatterplot ######
scatter_plot <- ggplot(ChemData_long, aes(x = Time, y = Value, color = Variable)) +
  geom_point() +
  labs(title = "Variation of Temperature and pH by Time (Low Tide)",
       x = "Time",
       y = "Value") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),   # bigger bold title
    axis.title.x = element_text(size = 14, face = "bold"), # bigger bold x-axis title
    axis.title.y = element_text(size = 14, face = "bold")  # bigger bold y-axis title
  )

#### Save plot to output folder ######
ggsave(here("Week_04", "output", "scatter_plot.png"),  # save plot as PNG
       plot = scatter_plot, width = 7, height = 5)     # set output size

