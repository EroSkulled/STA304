library(cesR)
library(ggplot2)
library(tidyverse)
library(skimr)
library(lme4)
library(brms)


get_ces("ces2019_web")
get_ces("ces2019_phone")

dat1 <- ces2019_web%>% 
  select(pes19_province, cps19_weight_general_restricted,
         cps19_votechoice, cps19_gender, cps19_income_number)%>%
  drop_na() %>%
  filter(cps19_votechoice != 9)%>%
  filter(cps19_votechoice != 8)

dat1$pes19_province<- as.character(dat1$pes19_province)
dat1$pes19_province[dat1$pes19_province == 1] <- "Alberta"
dat1$pes19_province[dat1$pes19_province == 2] <- "British Columbia"
dat1$pes19_province[dat1$pes19_province == 3] <- "Manitoba"
dat1$pes19_province[dat1$pes19_province == 4] <- "New Brunswick"
dat1$pes19_province[dat1$pes19_province == 5] <- "Newfoundland and Labrador"
dat1$pes19_province[dat1$pes19_province == 6] <- "Northwest Territories"
dat1$pes19_province[dat1$pes19_province == 7] <- "Nova Scotia"
dat1$pes19_province[dat1$pes19_province == 8] <- "Nunavut"
dat1$pes19_province[dat1$pes19_province == 9] <- "Ontario"
dat1$pes19_province[dat1$pes19_province == 10] <- "Prince Edward Island"
dat1$pes19_province[dat1$pes19_province == 11] <- "Quebec"
dat1$pes19_province[dat1$pes19_province == 12] <- "Saskatchewan"
dat1$pes19_province[dat1$pes19_province == 13] <- "Yukon"

dat1$cps19_votechoice<- as.character(dat1$cps19_votechoice)
dat1$cps19_votechoice[dat1$cps19_votechoice == 1] <- "Liberal Party"
dat1$cps19_votechoice[dat1$cps19_votechoice == 2] <- "Conservative Party"
dat1$cps19_votechoice[dat1$cps19_votechoice == 3] <- "NDP"
dat1$cps19_votechoice[dat1$cps19_votechoice == 4] <- "Bloc Québécois"
dat1$cps19_votechoice[dat1$cps19_votechoice == 5] <- "Green Party"
dat1$cps19_votechoice[dat1$cps19_votechoice == 6] <- "People's Party"
dat1$cps19_votechoice[dat1$cps19_votechoice == 7] <- "Other Party"

dat1$cps19_gender<- as.character(dat1$cps19_gender)
dat1$cps19_gender[dat1$cps19_gender == 1] <- "Males"
dat1$cps19_gender[dat1$cps19_gender == 2] <- "Females"
dat1$cps19_gender[dat1$cps19_gender == 3] <- "Other"


## process the proportion of voting choice
dat <- dat1 %>%
  group_by(pes19_province) %>%
  count(cps19_votechoice, wt = cps19_weight_general_restricted)%>%
  mutate(prop = n/sum(n))

write_csv(dat, "plot_data1.csv")



pre_data <- dat1
pre_data$province <- dat1$pes19_province
pre_data$gender <- dat1$cps19_gender
pre_data$income <- dat1$cps19_income_number
pre_data$vote_Liberal = 0
pre_data$vote_Liberal[pre_data$cps19_votechoice == "Liberal Party"] <- 1
pre_data$vote_Conservative = 0
pre_data$vote_Conservative[pre_data$cps19_votechoice == "Conservative Party"] <- 1
pre_data<- pre_data%>%
  filter(gender != "Other")%>%
  select(-c(1:5))
write_csv(pre_data, "survey_data1.csv")



model <- glm(vote_Liberal ~ gender,
             data = pre_data, 
             family = binomial())

