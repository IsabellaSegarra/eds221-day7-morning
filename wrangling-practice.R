#clearing environment
rm(list = ls())
#attach packages

library(tidyverse)
library(palmerpenguins)
library(lubridate)#help us work with dates
library(dplyr)

#Data wrangling refresher
head(penguins)
# 1. only include penguins at Briscone and Dream islands
penguins %>%
  filter( site == "Briscoe",
          site == "Dream") %>%
#2. remove the year and sex variables
  select(-c(sex, year)) %>%
# 3. add a new column called body_mass_kg with penguin mass converted from grams to kg
  mutate( "body_mass_kg" == body_mass_g * 0.001) %>%
#4. rename the island variable to location
  rename(island == location)

#--- w/ Ruth---

penguins_new <- penguins %>%
  filter(island %in% c("Briscoe", "Dream")) %>%
  select (-year, -sex) %>%
  mutate("body_mass_kg" == body_mass_g /1000) %>%
  rename(location = island)  %>%

# 1. Limit to only Adelie penguins
# 2. Remove any observations where flipper_length_mm is NA
# 3. Group the data by sex
# 4. Find the mean, sd, and sample size (n()) of flipper lengths for male and females


  filter(species == "Adelie") %>%
  select(!is.na(flipper_length_mm)) %>% #remove rows that are not NA, "!" does the opposite
  group_by(sex) %>%
  summarize(mean = mean(flipper_length_mm),
            standard_deviation = sd(flipper_length_mm),
           sample_size = n())

