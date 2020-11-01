#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from usa.ipums.org/usa/index.shtml
# Author: Lingyi Li, Yuehao Huang, Suran Wu, Siyu Chen
# Data: 30 October 2020
# Contact: walter.huang@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the ACS data and saved it to inputs/data
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
# Read in the raw data.
setwd("/Users/walterhuang/Downloads/Nationscape-DataRelease_WeeklyMaterials_DTA/phase_2_v20200814/ns20200625")
raw_data <- read_dta("usa_00003.dta")


# Add the labels
raw_data <- labelled::to_factor(raw_data)

# Just keep some variables that may be of interest (change 
# this depending on your interests)
reduced_data <- 
  raw_data %>% 
  select(#region,
         sex,
         empstat)#, 
         #race, 
         #hispan,
         #marst, 
         #bpl,
         #citizen,
         #educd,
         #labforce,
         #labforce)
         

#### What's next? ####



reduced_data <- 
  reduced_data %>%
  mutate(employment = empstat) %>%
  mutate(gender = sex)%>%
  count(employment, gender)%>%
  filter(employment !=("n/a"))
reduced_data$gender <- as.character(reduced_data$gender)
reduced_data$gender[reduced_data$gender == "female"] <- "Female"
reduced_data$gender <- as.character(reduced_data$gender)
reduced_data$gender[reduced_data$gender == "male"] <- "Male"

reduced_data <- 
  reduced_data %>%
  filter(employment !=c("not in labor force"))








# Saving the census data as a csv file in my
# working directory
write_csv(reduced_data, "census_data.csv")



         