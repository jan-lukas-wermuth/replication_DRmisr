# Title:      YRBS Data Preparation
# Author:     Jan-Lukas Wermuth
# Date:       2025-05-06
# Purpose:    This script merges the YRBS data from different 
#             states and adds state-level covariates

rm(list = ls())

library(dplyr)
library(haven)
library(stringr)
library(readxl)
library(naniar)

setwd("~/Dropbox/GLW/replication_DRmisr")

# Merge YRBS data from different states ------------------------------------------------------------
SADC_2019_State_a_m <- read_sav("data/sadc_2019_state_a_m.sav")
SADC_2019_State_n_z <- read_sav("data/sadc_2019_state_n_z.sav")
yrbs_data <- rbind(SADC_2019_State_a_m, SADC_2019_State_n_z)
save(yrbs_data, file = paste("data/yrbs_data.RData"))


# Data cleaning -----------------------------------------------------------
load("data/yrbs_data.RData") # load the 2019 data which has been merged across the states
yrbs_data <- yrbs_data %>% 
  as_tibble() %>% 
  rename(
    state_abb = sitecode,
    state = sitename
  ) %>% 
  mutate(state = gsub(r"{\s*\([^\)]+\)}", "", state)) # Remove state abbreviations in brackets

# Load and merge state-level covariates -----------------------------------
median_income <- as_tibble(read_xlsx("data/median_income_by_year_GL1.xlsx"))
median_income <- median_income %>% 
  rename(
    state_abb = state,
    state = state_name
  ) %>% 
  dplyr::select(c(1,2,4,5))  # Remove StateFIPS variable

unemployment_rate <- as_tibble(read_xlsx("data/unemployment_rate_by_year_GL1.xlsx"))
unemployment_rate <- unemployment_rate %>% 
  mutate_at(c("state"), toupper) %>% # Need to have upper case state to be able to join later
  dplyr::select(2:6) # Remove StateFIPS variable

marijuana_prices <- as_tibble(read_xlsx("data/Prices_Marijuana_wide.xlsx"))
marijuana_prices <- marijuana_prices %>%
  rename(
    mar_prices_high = Prices_high,
    mar_prices_medium = Prices_medium
  ) %>% 
  mutate_at(c("state"), toupper) %>%  # Need to have upper case state to be able to join later
  mutate(state = case_when(
    state == "LDAHO" ~ "IDAHO",
    state == "RHODE LSLAND" ~ "RHODE ISLAND",
    TRUE ~ state # Default case
  ))

questions_state <- as_tibble(read_xlsx("~/Dropbox/GLW/DataSets/YRBS/Questions_per_state.xlsx")) %>% 
  mutate_at(c("state"), toupper)

# Firstly, merge median_income, unemployment_rate and marijuana_prices to circumvent the need to include state_abb in unemployment_rate and marijuana_prices
income_unemployment_marijuana <- median_income %>% 
  left_join(unemployment_rate, by = c("state", "year")) %>% 
  left_join(marijuana_prices, by = c("state", "year")) %>% 
  left_join(questions_state, by = c("state", "year"))

yrbs_data <- yrbs_data %>%
  mutate(state_abb = ifelse(state_abb == "AZB", "AZ", state_abb))

# Secondly, merge all the files with yrbs_data
yrbs_data_combined <- yrbs_data %>% 
  left_join(income_unemployment_marijuana, by = c("state_abb", "year"))

# Create state-level dummy variable for medical legalization
yrbs_data_combined[,"medical"] <- ifelse((yrbs_data_combined$state.x == "Arizona")|
                                                (yrbs_data_combined$state.x == "Alabama")|
                                                (yrbs_data_combined$state.x == "Alaska")|
                                                (yrbs_data_combined$state.x == "Arkansas")|
                                                (yrbs_data_combined$state.x == "California")|
                                                (yrbs_data_combined$state.x == "Colorado")|
                                                (yrbs_data_combined$state.x == "Conneticut")|
                                                (yrbs_data_combined$state.x == "Delaware")|
                                                (yrbs_data_combined$state.x == "Florida")|
                                                (yrbs_data_combined$state.x == "Hawaii")|
                                                (yrbs_data_combined$state.x == "Illinois")|
                                                (yrbs_data_combined$state.x == "Louisiana")|
                                                (yrbs_data_combined$state.x == "Maine")|
                                                (yrbs_data_combined$state.x == "Maryland")|
                                                (yrbs_data_combined$state.x == "Massachusetts")|
                                                (yrbs_data_combined$state.x == "Michigan")|
                                                (yrbs_data_combined$state.x == "Minnesota")|
                                                (yrbs_data_combined$state.x == "Missouri")|
                                                (yrbs_data_combined$state.x == "Montana")|
                                                (yrbs_data_combined$state.x == "Nevada")|
                                                (yrbs_data_combined$state.x == "New Hampshire")|
                                                (yrbs_data_combined$state.x == "New Jersey")|
                                                (yrbs_data_combined$state.x == "New Mexico")|
                                                (yrbs_data_combined$state.x == "New York")|
                                                (yrbs_data_combined$state.x == "North Dakota")|
                                                (yrbs_data_combined$state.x == "Ohio")|
                                                (yrbs_data_combined$state.x == "Oklahoma")|
                                                (yrbs_data_combined$state.x == "Oregon")|
                                                (yrbs_data_combined$state.x == "Pennsylvania")|
                                                (yrbs_data_combined$state.x == "Rhode Island")|
                                                (yrbs_data_combined$state.x == "Utah")|
                                                (yrbs_data_combined$state.x == "Vermont")|
                                                (yrbs_data_combined$state.x == "Washington")|
                                                (yrbs_data_combined$state.x == "West Virginia"), 1, 0)

# Create state-level dummy variable for recreational legalization
yrbs_data_combined[, "recreational"] <- ifelse((yrbs_data_combined$state.x == "Arizona")| # 1 for states that have legalized marijuana until 2021
                                                      (yrbs_data_combined$state.x == "Alaska")|
                                                      (yrbs_data_combined$state.x == "California")|
                                                      (yrbs_data_combined$state.x == "Colorado")|
                                                      (yrbs_data_combined$state.x == "Conneticut")|
                                                      (yrbs_data_combined$year > 2023 & yrbs_data_combined$state.x == "Delaware")|
                                                      (yrbs_data_combined$state.x == "Illinois")|
                                                      (yrbs_data_combined$state.x == "Maine")| 
                                                      (yrbs_data_combined$state.x == "Maryland")| 
                                                      (yrbs_data_combined$state.x == "Massachusetts")| 
                                                      (yrbs_data_combined$state.x == "Michigan")|
                                                      (yrbs_data_combined$year > 2023 & yrbs_data_combined$state.x == "Minnesota")|
                                                      (yrbs_data_combined$state.x == "Missouri")|
                                                      (yrbs_data_combined$state.x == "Montana")|
                                                      (yrbs_data_combined$state.x == "Nevada")|
                                                      (yrbs_data_combined$state.x == "New Jersey")|
                                                      (yrbs_data_combined$state.x == "New Mexico")|
                                                      (yrbs_data_combined$year > 2023 & yrbs_data_combined$state.x == "Ohio")|
                                                      (yrbs_data_combined$state.x == "Oregon")|
                                                      (yrbs_data_combined$state.x == "Rhode Island")|
                                                      (yrbs_data_combined$state.x == "Vermont")|
                                                      (yrbs_data_combined$state.x == "Virginia")|
                                                      (yrbs_data_combined$state.x == "Washington")|
                                                      (yrbs_data_combined$state.x == "New York"), 1, 0) # no observations for Oregon, Vermont, Virginia and Washington

# Create minority dummy out of race4
yrbs_data_combined[, "minority"] <- ifelse(yrbs_data_combined$race4 == 2 | yrbs_data_combined$race4 == 3 | yrbs_data_combined$race4 == 4, 1, 0)

save(yrbs_data_combined, file = "data/yrbs_data_combined.RData")

# Choose relevant years
yrbs_data_combined_201520172019 <- yrbs_data_combined[((yrbs_data_combined$year == 2019 |yrbs_data_combined$year == 2017 | yrbs_data_combined$year == 2015) & yrbs_data_combined$grade == 4) %in% TRUE,]

save(yrbs_data_combined_201520172019, file = "data/yrbs_data_combined_201520172019.RData")

# Define mar_life variable as lifetime marijuana consumption
yrbs_data_combined_201520172019$mar_life <- pmax(as.numeric(as.character(yrbs_data_combined_201520172019$q45)), as.numeric(as.character(yrbs_data_combined_201520172019$q48)), na.rm = TRUE)

# Choose variables and remove NAs (split by marijuana recreational legalization status)
j <- 0
for (i in c("norecreational", "recreational")) {
  assign(paste("yrbs_data_combined_201520172019", i, "mar_life", sep = "_"), yrbs_data_combined_201520172019 %>% 
           dplyr::select(year, mar_life, sex, minority, median_income, unemployment_rate, recreational, mar_prices_medium, questions, age) %>%
           na.omit() %>% 
           dplyr::filter(recreational == j) %>% 
           dplyr::mutate(age_18 = ifelse(age == 7, 1, 0)) %>% 
           dplyr::mutate(year_2019 = ifelse(year == 2019, 1, 0)) %>% 
           dplyr::mutate(year_2017 = ifelse(year == 2017, 1, 0)) %>% 
           dplyr::relocate(sex, .after = minority) %>% dplyr::relocate(age_18, .after = sex))
  j <- j + 1
}

# Initialize lists in which all y-vectors get stored
for (j in c("norecreational", "recreational")) {
  assign(paste("mar_life", "30days_binary", j, sep = "_"), list())
}

# Fill the lists with y-vectors: First element represents 1 vs 2-6, second 1-2 vs 3-6 etc.
for (i in 1:6) { # Note that cigarettes consumption has one answer option more than marijuana consumption!
  for (j in c("norecreational", "recreational")) {
    assign(paste("mar_life", "30days_binary", j, sep = "_"), append(get(paste("mar_life", "30days_binary", j, sep = "_")), list(0 + (as.numeric(as.matrix(get(paste("yrbs_data_combined_201520172019", j, "mar_life", sep = "_"))["mar_life"])) > i))))
  }
}

# Add all the new vectors to the df
for (j in c("norecreational", "recreational")) {
  assign(paste("yrbs_data_combined_201520172019", j, "mar_life", sep = "_"), cbind(get(paste("yrbs_data_combined_201520172019", j, "mar_life", sep = "_")), do.call(cbind, get(paste("mar_life", "30days_binary", j, sep = "_")))))
}

# Rename the columns accordingly and normalize the covariates
for (j in c("norecreational", "recreational")) {
  assign(paste("yrbs_data_combined_201520172019", j, "mar_life", sep = "_"), get(paste("yrbs_data_combined_201520172019", j, "mar_life", sep = "_")) %>% rename(!!!setNames(as.character(1:6), paste("mar_life", 1:6, sep = ""))) %>% select(-2))
  assign(paste("yrbs_data_combined_201520172019", j, "mar_life", sep = "_"), as.data.frame(cbind(scale(get(paste("yrbs_data_combined_201520172019", j, "mar_life", sep = "_"))[,1:12]), get(paste("yrbs_data_combined_201520172019", j, "mar_life", sep = "_"))[,13:18])))
  save(list = paste("yrbs_data_combined_201520172019", j, "mar_life", sep = "_"), file = paste0("data/", paste("yrbs_data_combined_201520172019", j, "mar_life", sep = "_"), ".RData"))
}

# Prepare matrix of covariates
for (j in c("norecreational", "recreational")) {
  assign(paste("X", j, "mar_life", sep = "_"), get(paste("yrbs_data_combined_201520172019", j, "mar_life", sep = "_")) %>% select(-c(paste0("mar_life", 1:6), "recreational", "year", "age", "year_2017", "year_2019")))
  save(list = paste("X", j, "mar_life", sep = "_"), file = paste0("data/", paste("X", j, "mar_life", sep = "_"), ".RData"))
}

