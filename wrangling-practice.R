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

#Datapasta load data

animals <- data.frame(
  stringsAsFactors = FALSE,
          location = c("lagoon", "bluff", "creek", "oaks", "bluff"),
           species = c("bobcat", "coyote", "fox", "squirrel", "bobcat"),
          maturity = c("adult", "juvenile", "adult", "juvenile", "adult")
)

sites <- data.frame(
  stringsAsFactors = FALSE,
          location = c("beach", "lagoon", "bluff", "oaks"),
    full_site_name = c("Goleta Beach","UCSB Lagoon",
                       "Ellwood Mesa","Fremont Campground"),
      jurisdiction = c("SB City", "UCSB", "SB City", "USFS")
)

#practice with full_join
  #keeps all rows and adds all columns
full_join(animals, sites)

#practice with left_join
left_join(animals, sites)

#practice with right_join
right_join(animals, sites)

#practice with inner_join
inner_join(animals, sites)

#Filtering joins
semi_join(animals, sites)

animals %>%
filter(location %in% sites$location)

anti_join(animals, sites)

animals %>%
  filter(!location %in% sites$location)

#Practice with lubridate

my_date <- "03-13-1998"

lubridate::mdy(my_date) #fixed date to ISO 8061

#new format for date

my_date <- "08-Jun-1974"

lubridate::dmy(my_date) #dmy= date, month, year

#another format

my_date <- "19160518"

lubridate::ymd(my_date) #ymd= year, month date

lubridate::dmy("09/12/84")

#working with date-times

time <- "2020-08-12 11:18"
ymd_hm(time) #ymd_hm= year, month, date _ hour minute
class(ymd_hm(time)) #stored as POSIXct = good!

#convert to PDT
with_tz(time, "America/Los_Angeles")

#extract info from dates
week(time)
year(time)
day(time)


start_time <- Sys.time()

end_time <- Sys.time()

end_time - start_time
