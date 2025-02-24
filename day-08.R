# day-08.R
# Author: [Chris Drew]
# Date: [2/24/2025]
# Purpose: Create a faceted plot of cumulative COVID-19 cases & deaths by USA region
library(tidyverse)
url <- 'https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv'
covid <- read_csv(url)
states_regions <- data.frame(
  state = state.name,  # Full state names
  region = state.region # Corresponding region
)

head(states_regions)

covid_states <- covid |> 
  group_by(date, state) |> 
  summarise(
    cases = sum(cases, na.rm = TRUE),
    deaths = sum(deaths, na.rm = TRUE)
  ) |> 
  ungroup()

head(covid_states)

covid_states <- left_join(covid_states, states_regions, by = "state")


head(covid_states)

covid_regions <- covid_states |> 
  group_by(date, region) |> 
  summarise(
    cases = sum(cases, na.rm = TRUE),
    deaths = sum(deaths, na.rm = TRUE)
  ) |> 
  ungroup()


head(covid_regions)

covid_long <- covid_regions |> 
  pivot_longer(cols = c(cases, deaths), 
               names_to = "metric", 
               values_to = "count")


head(covid_long)

ggplot(covid_long, aes(x = date, y = count, color = metric)) +
  geom_line(size = 1) +
  facet_wrap(~ region, scales = "free_y") +
  labs(
    title = "Cumulative COVID-19 Cases & Deaths by USA Region",
    x = "Date",
    y = "Count",
    color = "Metric"
  ) +
  theme_minimal()

ggsave("img/covid_regions_plot.png", width = 10, height = 6)
