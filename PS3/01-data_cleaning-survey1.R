#### Preamble ####
# Purpose: Prepare and clean the survey data downloaded from www.voterstudygroup.org/publication/nationscape-data-set
# Author: Lingyi Li, Yuehao Huang, Suran Wu, Siyu Chen
# Data: 22 October 2020
# Contact: watler.huang@mail.utoronto.ca
# License: MIT
# Pre-requisites: 
# - Need to have downloaded the data from X and save the folder that you're 
# interested in to inputs/data 
# - Don't forget to gitignore it!


#### Workspace setup ####
library(haven)
library(tidyverse)
setwd("/Users/walterhuang/Downloads/Nationscape-DataRelease_WeeklyMaterials_DTA/phase_2_v20200814/ns20200625")
# Read in the raw data (You might need to change this if you use a different dataset)
raw_data <- read_dta("ns20200625.dta")
varname <- variable.names(raw_data)
# Add the labels
raw_data <- labelled::to_factor(raw_data)
# Just keep some variables
reduced_data <- 
  raw_data %>% 
  select(registration,
         vote_2020,
         gender,
         employment)


#### What else???? ####

reduced_data<-
  reduced_data %>%
  mutate(vote_trump = 
           ifelse(vote_2020=="Donald Trump", 1, 0))
reduced_data <-
  reduced_data %>%
  filter(registration =="Registered")

reduced_data$employment <- as.character(reduced_data$employment)
reduced_data$employment[reduced_data$employment == "Full-time employed"] <- "employed"
reduced_data$employment <- as.character(reduced_data$employment)
reduced_data$employment[reduced_data$employment == "Part-time employed"] <- "employed"
reduced_data$employment <- as.character(reduced_data$employment)
reduced_data$employment[reduced_data$employment == "Self-employed"] <- "employed"
reduced_data$employment <- as.character(reduced_data$employment)
reduced_data$employment[reduced_data$employment == "Homemaker"] <- "employed"
reduced_data$employment <- as.character(reduced_data$employment)
reduced_data$employment[reduced_data$employment == "Unemployed or temporarily on layoff"] <- "unemployed"
reduced_data$employment <- as.character(reduced_data$employment)
reduced_data$employment[reduced_data$employment == "Retired"] <- "not in labor force"
reduced_data$employment <- as.character(reduced_data$employment)
reduced_data$employment[reduced_data$employment == "Student"] <- "not in labor force"
reduced_data$employment <- as.character(reduced_data$employment)
reduced_data$employment[reduced_data$employment == "Permanently disabled"] <- "not in labor force"



reduced_data <-
  reduced_data %>% filter(employment !=c("Other:"))
reduced_data <- 
  reduced_data %>%
  filter(employment !=c("not in labor force"))


# Saving the survey/sample data as a csv file in my
# working directory
write_csv(reduced_data, "survey_data1.csv")

