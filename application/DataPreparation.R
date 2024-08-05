# Data preparation for High School 2015-2019

rm(list = ls())

library(dplyr)
library(haven)
library(stringr)
library(readxl)
library(naniar)

# Combine Data ------------------------------------------------------------

# Load all the necessary data and harmonize the names of the columns
load("yrbs_data.RData") # load the 2021 data which has been merged across the states
yrbs_data <- yrbs_data %>% 
  as_tibble() %>% 
  rename(
    state_abb = sitecode,
    state = sitename
  ) %>% 
  mutate(state = gsub(r"{\s*\([^\)]+\)}", "", state)) # Remove state abbreviations in brackets

median_income <- as_tibble(read_xlsx("median_income_by_year_GL1.xlsx"))
median_income <- median_income %>% 
  rename(
    state_abb = state,
    state = state_name
  ) %>% 
  dplyr::select(c(1,2,4,5))  # Remove StateFIPS variable

unemployment_rate <- as_tibble(read_xlsx("unemployment_rate_by_year_GL1.xlsx"))
unemployment_rate <- unemployment_rate %>% 
  mutate_at(c("state"), toupper) %>% # Need to have upper case state to be able to join later
  dplyr::select(2:6) # Remove StateFIPS variable

marijuana_prices <- as_tibble(read_xlsx("Prices_Marijuana_wide.xlsx"))
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

cigarettes_taxes <- as_tibble(read.csv("state_tax_cigarettes.csv"))
cigarettes_taxes <- cigarettes_taxes %>%
  rename(
    state_abb = STATE,
    year = Year,
    tax_pack = State.Tax.per.pack.in.Dollars,
    tax_revenue = Gross.Cigarette.Tax.Revenue.in.Dollars,
    sales_capita = Cigarette.Consumption..Pack.Sales.Per.Capita..in.Pack,
    tax_percent = Federal.and.State.tax.as.a.Percentage.of.Retail.Price.in.Percentage,
    tax_pack_combined = Federal.and.State.Tax.per.pack.in.Dollars,
    cost_avg = Average.Cost.per.pack.in.Dollars
  )

# alcohol_taxes <- as_tibble(read_xlsx("state_alcohol_rates_1.xlsx", sheet = 2, skip = 3))
# alcohol_taxes <- alcohol_taxes[,1:6] # Delete the spurious last column
# alcohol_taxes <- alcohol_taxes %>% # Use the same column names as in the other tibbles
#   rename(
#     year = Year,
#     state_abb = `State abbreviation`,
#     state = `State name`,
#     distilled = `Distilled spirits`,
#     wine = Wine,
#     beer = Beer
#   ) %>% 
#   replace_with_na(list(beer="n.a.", distilled="n.a.", wine="n.a.")) %>% # Replace the n.a. with real NAs
#   mutate(distilled = gsub("\\*", "", distilled)) %>% # Remove the asterisks from the numbers
#   mutate(beer = gsub("\\*", "", beer)) %>% # Remove the asterisks from the numbers
#   mutate(wine = gsub("\\*", "", wine)) %>% # Remove the asterisks from the numbers
#   mutate_at(c("distilled", "beer", "wine"), as.numeric) # Convert to numeric

questions_state <- as_tibble(read_xlsx("Questions_per_state.xlsx")) %>% 
  mutate_at(c("state"), toupper) # Need to have upper case state to be able to join later

# Firstly, merge median_income, unemployment_rate and marijuana_prices to circumvent the need to include state_abb in unemployment_rate and marijuana_prices
income_unemployment_marijuana <- median_income %>% 
  left_join(unemployment_rate, by = c("state", "year")) %>% 
  left_join(marijuana_prices, by = c("state", "year")) %>% 
  left_join(questions_state, by = c("state", "year"))

# Secondly, merge all the files with yrbs_data
yrbs_data_combined <- yrbs_data %>% 
  left_join(income_unemployment_marijuana, by = c("state_abb", "year")) %>% 
  left_join(cigarettes_taxes, by = c("state_abb", "year")) %>% 
  left_join(alcohol_taxes, by = c("state_abb", "year"))

# Create state-level dummy variable for medical legalization
yrbs_data_combined[,"medical"] <- ifelse((yrbs_data_combined$year >= 1998 & yrbs_data_combined$state.x == "Alaska")|
                                           (yrbs_data_combined$year >= 2010 & yrbs_data_combined$state.x == "Arizona")|
                                           (yrbs_data_combined$year >= 2016 & yrbs_data_combined$state.x == "Arkansas")|
                                           (yrbs_data_combined$year >= 1996 & yrbs_data_combined$state.x == "California")|
                                           (yrbs_data_combined$year >= 2000 & yrbs_data_combined$state.x == "Colorado")|
                                           (yrbs_data_combined$year >= 2012 & yrbs_data_combined$state.x == "Conneticut")|
                                           (yrbs_data_combined$year >= 2011 & yrbs_data_combined$state.x == "Delaware")|
                                           (yrbs_data_combined$year >= 2016 & yrbs_data_combined$state.x == "Florida")|
                                           (yrbs_data_combined$year >= 2000 & yrbs_data_combined$state.x == "Hawaii")|
                                           (yrbs_data_combined$year >= 2013 & yrbs_data_combined$state.x == "Illinois")|
                                           (yrbs_data_combined$year >= 2017 & yrbs_data_combined$state.x == "Louisiana")|
                                           (yrbs_data_combined$year >= 1999 & yrbs_data_combined$state.x == "Maine")|
                                           (yrbs_data_combined$year >= 2003 & yrbs_data_combined$state.x == "Maryland")|
                                           (yrbs_data_combined$year >= 2012 & yrbs_data_combined$state.x == "Massachusetts")|
                                           (yrbs_data_combined$year >= 2008 & yrbs_data_combined$state.x == "Michigan")|
                                           (yrbs_data_combined$year >= 2014 & yrbs_data_combined$state.x == "Minnesota")|
                                           (yrbs_data_combined$year >= 2018 & yrbs_data_combined$state.x == "Missouri")|
                                           (yrbs_data_combined$year >= 2004 & yrbs_data_combined$state.x == "Montana")|
                                           (yrbs_data_combined$year >= 2000 & yrbs_data_combined$state.x == "Nevada")|
                                           (yrbs_data_combined$year >= 2013 & yrbs_data_combined$state.x == "New Hampshire")|
                                           (yrbs_data_combined$year >= 2009 & yrbs_data_combined$state.x == "New Jersey")|
                                           (yrbs_data_combined$year >= 2007 & yrbs_data_combined$state.x == "New Mexico")|
                                           (yrbs_data_combined$year >= 2014 & yrbs_data_combined$state.x == "New York")|
                                           (yrbs_data_combined$year >= 2016 & yrbs_data_combined$state.x == "North Dakota")|
                                           (yrbs_data_combined$year >= 2016 & yrbs_data_combined$state.x == "Ohio")|
                                           (yrbs_data_combined$year >= 2018 & yrbs_data_combined$state.x == "Oklahoma")|
                                           (yrbs_data_combined$year >= 1998 & yrbs_data_combined$state.x == "Oregon")|
                                           (yrbs_data_combined$year >= 2016 & yrbs_data_combined$state.x == "Pennsylvania")|
                                           (yrbs_data_combined$year >= 2007 & yrbs_data_combined$state.x == "Rhode Island")|
                                           (yrbs_data_combined$year >= 2018 & yrbs_data_combined$state.x == "Utah")|
                                           (yrbs_data_combined$year >= 2004 & yrbs_data_combined$state.x == "Vermont")|
                                           (yrbs_data_combined$year >= 1998 & yrbs_data_combined$state.x == "Washington")|
                                           (yrbs_data_combined$year >= 2017 & yrbs_data_combined$state.x == "West Virginia"), 1, 0)

yrbs_data_combined[, "recreational"] <- ifelse((yrbs_data_combined$year >= 2014 & yrbs_data_combined$state.x == "Alaska")|
                                                 (yrbs_data_combined$year >= 2016 & yrbs_data_combined$state.x == "Arizona")|
                                                 (yrbs_data_combined$year >= 2012 & yrbs_data_combined$state.x == "Arkansas")|
                                                 (yrbs_data_combined$year >= 2016 & yrbs_data_combined$state.x == "California")|
                                                 (yrbs_data_combined$year >= 2012 & yrbs_data_combined$state.x == "Colorado")|
                                                 (yrbs_data_combined$year >= 2018 & yrbs_data_combined$state.x == "Conneticut")|
                                                 (yrbs_data_combined$year >= 2016 & yrbs_data_combined$state.x == "Delaware")|
                                                 (yrbs_data_combined$year >= 2014 & yrbs_data_combined$state.x == "Florida")|
                                                 (yrbs_data_combined$year >= 2018 & yrbs_data_combined$state.x == "Hawaii")|
                                                 (yrbs_data_combined$year >= 2012 & yrbs_data_combined$state.x == "Illinois"), 1, 0)

# Create race dummies out of race4
yrbs_data_combined[, "black"] <- ifelse(yrbs_data_combined$race4 == 2, 1, 0)
yrbs_data_combined[, "hispanic"] <- ifelse(yrbs_data_combined$race4 == 3, 1, 0)
yrbs_data_combined[, "other"] <- ifelse(yrbs_data_combined$race4 == 4, 1, 0)
yrbs_data_combined[, "minority"] <- ifelse(yrbs_data_combined$race4 == 2 | yrbs_data_combined$race4 == 3 | yrbs_data_combined$race4 == 4, 1, 0)

# Create share of NAs as exclusion restriction
yrbs_data_combined[, "NAs_87"] <- rowSums(is.na(yrbs_data_combined %>% dplyr::select(matches("^q[0-9]"))))

save(yrbs_data_combined, file = "yrbs_data_combined.RData")

# Choose relevant years
yrbs_data_combined_2015 <- yrbs_data_combined[(yrbs_data_combined$year == 2015 & yrbs_data_combined$grade == 4) %in% TRUE,]
yrbs_data_combined_2017 <- yrbs_data_combined[(yrbs_data_combined$year == 2017 & yrbs_data_combined$grade == 4) %in% TRUE,]
yrbs_data_combined_2019_women <- yrbs_data_combined[(yrbs_data_combined$year == 2019 & yrbs_data_combined$grade == 4 & yrbs_data_combined$sex == 1) %in% TRUE,]
yrbs_data_combined_2019_men <- yrbs_data_combined[(yrbs_data_combined$year == 2019 & yrbs_data_combined$grade == 4 & yrbs_data_combined$sex == 2) %in% TRUE,]
yrbs_data_combined_2019 <- yrbs_data_combined[(yrbs_data_combined$year == 2019 & yrbs_data_combined$grade == 4) %in% TRUE,]
yrbs_data_combined_20172019 <- yrbs_data_combined[((yrbs_data_combined$year == 2019 | yrbs_data_combined$year == 2017) & yrbs_data_combined$grade == 4) %in% TRUE,]
yrbs_data_combined_201520172019 <- yrbs_data_combined[((yrbs_data_combined$year == 2019 |yrbs_data_combined$year == 2017 | yrbs_data_combined$year == 2015) & yrbs_data_combined$grade == 4) %in% TRUE,]


save(yrbs_data_combined_2015, file = "yrbs_data_combined_2015.RData")
save(yrbs_data_combined_2017, file = "yrbs_data_combined_2017.RData")
save(yrbs_data_combined_2019_women, file = "yrbs_data_combined_2019_women.RData")
save(yrbs_data_combined_2019_men, file = "yrbs_data_combined_2019_men.RData")
save(yrbs_data_combined_2019, file = "yrbs_data_combined_2019.RData")
save(yrbs_data_combined_20172019, file = "yrbs_data_combined_20172019.RData")
save(yrbs_data_combined_201520172019, file = "yrbs_data_combined_201520172019.RData")

