# Bad Plot
Zoe Sidana Bunnath

## Introduction

I am using long-term monitoring data on crane observations from Lake
Hornborgasjön in Sweden. For more than 30 years, cranes stopping at Lake
Hornborgasjön (or “Lake Hornborga”) in Västergötland have been counted
from the Hornborgasjön field station during their spring and fall
migrations.

Using this data, I explore one main question: Has the crane population
at Lake Hornborgasjön grown over the past 30 years?

## Load the Libraries

``` r
library(dplyr)       # for data wrangling (filter, mutate, group_by, summarize)
library(ggplot2)     # for plotting
library(lubridate)   # for working with dates
library(tidyr)       # for filling in missing years
library(here)        # for creating file paths
```

## Load the Dataset

``` r
cranes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/main/data/2025/2025-09-30/cranes.csv')

head(cranes)
```

    # A tibble: 6 × 4
      date       observations comment              weather_disruption
      <date>            <dbl> <chr>                <lgl>             
    1 2024-10-03          408 Last count of season FALSE             
    2 2024-09-30        14940 <NA>                 FALSE             
    3 2024-09-26           NA Canceled/No count    TRUE              
    4 2024-09-23        18450 <NA>                 FALSE             
    5 2024-09-19        14900 <NA>                 FALSE             
    6 2024-09-16        12900 <NA>                 FALSE             

## Make a Plot

``` r
cranes %>%
  mutate(year = year(date)) %>%            # extract year from the date column
  group_by(year) %>%                       # group the data by year
  summarize(mean_obs = sum(observations, na.rm = TRUE)) %>%  # use sum instead of mean to show total observations
  ggplot(aes(x = factor(year), y = mean_obs, fill = factor(year))) +  # map year (factor) and total observations
  geom_col(color = "black") +              # create bar chart with black outlines
  geom_text(aes(label = round(mean_obs, 0)), vjust = -0.5, size = 3) +  # add numeric labels above bars
  labs(
    x = "Year",                            # label for x-axis
    y = "Obs",                             # label for y-axis (still short, like the “bad” version)
    title = "Observations"                 # title (still vague, like your bad version)
  ) +
  theme_classic() +                        # use classic theme (strong gridlines)
  theme(
    legend.position = "bottom",            # move legend to bottom
    axis.text.x = element_text(angle = 90, size = 6)  # vertical year labels (hard to read)
  )
```

![](../output/unnamed-chunk-3-1.png)

``` r
# Save the plot
ggsave(here("Week_08","Output","Bad Plot.png"))  
```

## Reasons Why This is a Bad Plot

- The bar chart makes it hard to see how the values change from year to
  year, which isn’t ideal for time series data.

- Using different colors for each year makes the plot look noisy and
  doesn’t really add meaning.

- The color blocks draw attention away from the actual pattern in the
  data.

- The overlapping text labels make it look crowded.

- The title and axis labels are too vague, so it’s not clear what the
  plot is showing.
