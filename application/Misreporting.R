# Distribution Regression: One Drug (cigarettes / marijuana) case: Medical Legalization vs No Legalization and Recreational Legalization vs No Legalization -----------------------------------------------------------
rm(list = ls())

library(dplyr)
library(labelled)

load("/data/yrbs_data_combined_20172019.RData")
source("Misreporting_functions.R")

yrbs_data_combined_20172019$cig <- pmax(yrbs_data_combined_20172019$q32, yrbs_data_combined_20172019$q35, yrbs_data_combined_20172019$q37, yrbs_data_combined_20172019$q38, na.rm = TRUE)
colnames(yrbs_data_combined_20172019)[which(colnames(yrbs_data_combined_20172019) == "q47")] <- "mar"
yrbs_data_combined_20172019$mar_life <- pmax(as.factor(yrbs_data_combined_20172019$q45), as.factor(yrbs_data_combined_20172019$q48), na.rm = TRUE)

# Choose variables and remove NAs (split by marijuana medical legalization status)
j <- 0
for (i in c("nomedical", "medical")) {
  assign(paste("yrbs_data_combined_20172019", i, "cig", sep = "_"), yrbs_data_combined_20172019 %>% 
           dplyr::select(year, cig, sex, median_income, unemployment_rate, medical, mar_prices_medium, questions, tax_percent) %>%
           na.omit() %>% 
           dplyr::filter(medical == j))
  assign(paste("yrbs_data_combined_20172019", i, "mar", sep = "_"), yrbs_data_combined_20172019 %>% 
           dplyr::select(year, mar, sex, median_income, unemployment_rate, medical, mar_prices_medium, questions, tax_percent) %>%
           na.omit() %>% 
           dplyr::filter(medical == j))
  assign(paste("yrbs_data_combined_20172019", i, "mar_life", sep = "_"), yrbs_data_combined_20172019 %>% 
           dplyr::select(year, mar_life, sex, median_income, unemployment_rate, medical, mar_prices_medium, questions, tax_percent) %>%
           na.omit() %>% 
           dplyr::filter(medical == j))
  j <- j + 1
}
# Choose variables and remove NAs (split by marijuana recreational legalization status)
j <- 0
for (i in c("norecreational", "recreational")) {
  assign(paste("yrbs_data_combined_20172019", i, "cig", sep = "_"), yrbs_data_combined_20172019 %>% 
           dplyr::select(year, cig, sex, median_income, unemployment_rate, recreational, mar_prices_medium, questions, tax_percent) %>%
           na.omit() %>% 
           dplyr::filter(recreational == j))
  assign(paste("yrbs_data_combined_20172019", i, "mar", sep = "_"), yrbs_data_combined_20172019 %>% 
           dplyr::select(year, mar, sex, median_income, unemployment_rate, recreational, mar_prices_medium, questions, tax_percent) %>%
           na.omit() %>% 
           dplyr::filter(recreational == j))
  assign(paste("yrbs_data_combined_20172019", i, "mar_life", sep = "_"), yrbs_data_combined_20172019 %>% 
           dplyr::select(year, mar_life, sex, median_income, unemployment_rate, recreational, mar_prices_medium, questions, tax_percent) %>%
           na.omit() %>% 
           dplyr::filter(recreational == j))
  j <- j + 1
}

# Initialize lists in which all y-vectors get stored
for (i in c("cig", "mar", "mar_life")) { 
  for (j in c("nomedical", "medical",  "norecreational", "recreational")) {
    assign(paste(i, "30days_binary", j, sep = "_"), list())
  }
}

# Fill the lists with y-vectors: First element represents 1 vs 2-6, second 1-2 vs 3-6 etc.
for (i in 1:6) { # Note that cigarettes consumption has one answer option more than marijuana consumption!
  for (j in c("nomedical", "medical", "norecreational", "recreational")) {
    for (k in c("cig", "mar", "mar_life")) {
      assign(paste(k, "30days_binary", j, sep = "_"), append(get(paste(k, "30days_binary", j, sep = "_")), list(0 + (as.numeric(as.matrix(get(paste("yrbs_data_combined_20172019", j, k, sep = "_"))[k])) > i))))
    }
  }
}

# add all the new vectors to the df
for (i in c("cig", "mar", "mar_life")) { 
  for (j in c("nomedical", "medical",  "norecreational", "recreational")) {
    assign(paste("yrbs_data_combined_20172019", j, i, sep = "_"), cbind(get(paste("yrbs_data_combined_20172019", j, i, sep = "_")), do.call(cbind, get(paste(i, "30days_binary", j, sep = "_")))))
  }
}
# and rename the columns accordingly
for (i in c("cig", "mar", "mar_life")) {
  for (j in c("nomedical", "medical")) {
    assign(paste("yrbs_data_combined_20172019", j, i, sep = "_"), get(paste("yrbs_data_combined_20172019", j, i, sep = "_")) %>% rename(!!!setNames(as.character(1:6), paste(i, 1:6, sep = ""))) %>% select(-2))
    assign(paste("yrbs_data_combined_20172019", j, i, sep = "_"), as.data.frame(cbind(scale(get(paste("yrbs_data_combined_20172019", j, i, sep = "_"))[,1:8]), get(paste("yrbs_data_combined_20172019", j, i, sep = "_"))[,9:14])))
    save(list = paste("yrbs_data_combined_20172019", j, i, sep = "_"), file = paste0("/Workspace/YRBS_DR_mis/DistributionRegression_20172019_med/", paste("yrbs_data_combined_20172019", j, i, sep = "_"), ".RData"))
  }
}
for (i in c("cig", "mar", "mar_life")) {
  for (j in c("norecreational", "recreational")) {
    assign(paste("yrbs_data_combined_20172019", j, i, sep = "_"), get(paste("yrbs_data_combined_20172019", j, i, sep = "_")) %>% rename(!!!setNames(as.character(1:6), paste(i, 1:6, sep = ""))) %>% select(-2))
    assign(paste("yrbs_data_combined_20172019", j, i, sep = "_"), as.data.frame(cbind(scale(get(paste("yrbs_data_combined_20172019", j, i, sep = "_"))[,1:8]), get(paste("yrbs_data_combined_20172019", j, i, sep = "_"))[,9:14])))
    save(list = paste("yrbs_data_combined_20172019", j, i, sep = "_"), file = paste0("/Workspace/YRBS_DR_mis/DistributionRegression_20172019_rec/", paste("yrbs_data_combined_20172019", j, i, sep = "_"), ".RData"))
  }
}

##############################################################
# GJRM for cigarettes and marijuana separately
##############################################################

for (i in c("cig", "mar", "mar_life")) { 
  for (j in c("nomedical", "medical")) {
    for (k in 1:6) {
      if (i == "cig"){
        formula <- list(paste(paste(i, k, sep = ""), "~", "year + sex + median_income + unemployment_rate + tax_percent"),
                        paste(paste(i, k, sep = ""), "~", "year + sex + median_income + unemployment_rate + questions"))
        assign(paste(i, "gjrm", j, k, sep = "_"), GJRM::gjrm(formula = list(as.formula(formula[[1]]), as.formula(formula[[2]])),
                                                                 data = get(paste("yrbs_data_combined_20172019", j, i, sep = "_")), model = "BPO", margins = c("probit", "probit")))
        save(list = paste(i, "gjrm", j, k, sep = "_"), file = paste("/Workspace/YRBS_DR_mis/DistributionRegression_20172019_med", paste(i, "gjrm", j, k, ".RData", sep = "_"), sep = "/"))
      }
      if (i == "mar"){
        formula <- list(paste(paste(i, k, sep = ""), "~", "year + sex + median_income + unemployment_rate + mar_prices_medium"),
                        paste(paste(i, k, sep = ""), "~", "year + sex + median_income + unemployment_rate + questions"))
        assign(paste(i, "gjrm", j, k, sep = "_"), GJRM::gjrm(formula = list(as.formula(formula[[1]]), as.formula(formula[[2]])),
                                                                 data = get(paste("yrbs_data_combined_20172019", j, i, sep = "_")), model = "BPO", margins = c("probit", "probit")))
        save(list = paste(i, "gjrm", j, k, sep = "_"), file = paste("/Workspace/YRBS_DR_mis/DistributionRegression_20172019_med", paste(i, "gjrm", j, k, ".RData", sep = "_"), sep = "/"))
      }
      if (i == "mar_life"){
        formula <- list(paste(paste(i, k, sep = ""), "~", "year + sex + median_income + unemployment_rate + mar_prices_medium"),
                        paste(paste(i, k, sep = ""), "~", "year + sex + median_income + unemployment_rate + questions"))
        assign(paste(i, "gjrm", j, k, sep = "_"), GJRM::gjrm(formula = list(as.formula(formula[[1]]), as.formula(formula[[2]])),
                                                             data = get(paste("yrbs_data_combined_20172019", j, i, sep = "_")), model = "BPO", margins = c("probit", "probit")))
        save(list = paste(i, "gjrm", j, k, sep = "_"), file = paste("/Workspace/YRBS_DR_mis/DistributionRegression_20172019_med", paste(i, "gjrm", j, k, ".RData", sep = "_"), sep = "/"))
      }
    }
  }
}
for (i in c("cig", "mar", "mar_life")) {
  for (j in c("norecreational", "recreational")) {
    for (k in 1:6) {
      if (i == "cig"){
        formula <- list(paste(paste(i, k, sep = ""), "~", "year + sex + median_income + unemployment_rate + tax_percent"),
                        paste(paste(i, k, sep = ""), "~", "year + sex + median_income + unemployment_rate + questions"))
        assign(paste(i, "gjrm", j, k, sep = "_"), GJRM::gjrm(formula = list(as.formula(formula[[1]]), as.formula(formula[[2]])),
                                                             data = get(paste("yrbs_data_combined_20172019", j, i, sep = "_")), model = "BPO", margins = c("probit", "probit")))
        save(list = paste(i, "gjrm", j, k, sep = "_"), file = paste("/Workspace/YRBS_DR_mis/DistributionRegression_20172019_rec", paste(i, "gjrm", j, k, ".RData", sep = "_"), sep = "/"))
      }
      if (i == "mar"){
        formula <- list(paste(paste(i, k, sep = ""), "~", "year + sex + median_income + unemployment_rate + mar_prices_medium"),
                        paste(paste(i, k, sep = ""), "~", "year + sex + median_income + unemployment_rate + questions"))
        assign(paste(i, "gjrm", j, k, sep = "_"), GJRM::gjrm(formula = list(as.formula(formula[[1]]), as.formula(formula[[2]])),
                                                             data = get(paste("yrbs_data_combined_20172019", j, i, sep = "_")), model = "BPO", margins = c("probit", "probit")))
        save(list = paste(i, "gjrm", j, k, sep = "_"), file = paste("/Workspace/YRBS_DR_mis/DistributionRegression_20172019_rec", paste(i, "gjrm", j, k, ".RData", sep = "_"), sep = "/"))
      }
      if (i == "mar_life"){
        formula <- list(paste(paste(i, k, sep = ""), "~", "year + sex + median_income + unemployment_rate + mar_prices_medium"),
                        paste(paste(i, k, sep = ""), "~", "year + sex + median_income + unemployment_rate + questions"))
        assign(paste(i, "gjrm", j, k, sep = "_"), GJRM::gjrm(formula = list(as.formula(formula[[1]]), as.formula(formula[[2]])),
                                                             data = get(paste("yrbs_data_combined_20172019", j, i, sep = "_")), model = "BPO", margins = c("probit", "probit")))
        save(list = paste(i, "gjrm", j, k, sep = "_"), file = paste("/Workspace/YRBS_DR_mis/DistributionRegression_20172019_rec", paste(i, "gjrm", j, k, ".RData", sep = "_"), sep = "/"))
      }
    }
  }
}

# Distribution Regression: Recreational Legalization vs No recreational Legalization -----------------------------------------------------------
rm(list = ls())

library(mnormt)
library(dplyr)

for (i in list.files("/Workspace/YRBS_DR_mis/DistributionRegression_20172019_rec")) { # Load all the GJRM results and the data
  load(file = paste("/Workspace/YRBS_DR_mis/DistributionRegression_20172019_rec", i, sep = "/"))
}
for (i in list.files("/Workspace/YRBS_DR_mis/DistributionRegression_20172019_med")) { # Load all the GJRM results and the data
  load(file = paste("/Workspace/YRBS_DR_mis/DistributionRegression_20172019_med", i, sep = "/"))
}

# Remove medical and recreational variables because they are not needed
for (i in c("cig", "mar", "mar_life")) {
  for (j in c("nomedical", "medical")) {
    assign(paste("X", j, i, sep = "_"), get(paste("yrbs_data_combined_20172019", j, i, sep = "_")) %>% select(-c(paste0(i, 1:6), "medical")))
    save(list = paste("X", j, i, sep = "_"), file = paste0("/Workspace/YRBS_DR_mis/DistributionRegression_20172019_med/", paste("X", j, i, sep = "_"), ".RData"))
  }
}
for (i in c("cig", "mar", "mar_life")) {
  for (j in c("norecreational", "recreational")) {
    assign(paste("X", j, i, sep = "_"), get(paste("yrbs_data_combined_20172019", j, i, sep = "_")) %>% select(-c(paste0(i, 1:6), "recreational")))
    save(list = paste("X", j, i, sep = "_"), file = paste0("/Workspace/YRBS_DR_mis/DistributionRegression_20172019_rec/", paste("X", j, i, sep = "_"), ".RData"))
  }
}

# Bootstrap: Marijuana -----------------------------------------------------------
rm(list = ls())

library(GJRM)
library(parallel)
library(doParallel)

for (i in list.files("/Workspace/YRBS_DR_mis/DistributionRegression_20172019_rec")) { # Load all the GJRM results and the data
  load(file = paste("/Workspace/YRBS_DR_mis/DistributionRegression_20172019_rec", i, sep = "/"))
}

################ Step 1: Obtain Bootstrap draws ####################
# Create Bootstrap draws
B <- 1000

"F_(0_0)_draws_mar" <- matrix(NA, ncol = 5, nrow = B)
"F_(1_0)_draws_mar" <- matrix(NA, ncol = 5, nrow = B)
"F_(0_1)_draws_mar" <- matrix(NA, ncol = 5, nrow = B)
"F_(1_1)_draws_mar" <- matrix(NA, ncol = 5, nrow = B)
for (i in 1:5) {
  `F_(0_0)_draws_mar`[,i] <- `F_(0_0)_b`(i, B, "mar")
  `F_(1_0)_draws_mar`[,i] <- `F_(1_0)_b`(i, B, "mar")
  `F_(0_1)_draws_mar`[,i] <- `F_(0_1)_b`(i, B, "mar")
  `F_(1_1)_draws_mar`[,i] <- `F_(1_1)_b`(i, B, "mar")
}

save("F_(0_0)_draws_mar", file = "/Workspace/YRBS_DR_mis/DistributionRegression_20172019_rec/F_(0_0)_draws_mar.RData")
save("F_(1_0)_draws_mar", file = "/Workspace/YRBS_DR_mis/DistributionRegression_20172019_rec/F_(1_0)_draws_mar.RData")
save("F_(0_1)_draws_mar", file = "/Workspace/YRBS_DR_mis/DistributionRegression_20172019_rec/F_(0_1)_draws_mar.RData")
save("F_(1_1)_draws_mar", file = "/Workspace/YRBS_DR_mis/DistributionRegression_20172019_rec/F_(1_1)_draws_mar.RData")

# Create Bootstrap draws
B <- 1000

"F_(0_0)_draws_mar_life" <- matrix(NA, ncol = 5, nrow = B)
"F_(1_0)_draws_mar_life" <- matrix(NA, ncol = 5, nrow = B)
"F_(0_1)_draws_mar_life" <- matrix(NA, ncol = 5, nrow = B)
"F_(1_1)_draws_mar_life" <- matrix(NA, ncol = 5, nrow = B)
for (i in 1:5) {
  `F_(0_0)_draws_mar_life`[,i] <- `F_(0_0)_b`(i, B, "mar_life")
  `F_(1_0)_draws_mar_life`[,i] <- `F_(1_0)_b`(i, B, "mar_life")
  `F_(0_1)_draws_mar_life`[,i] <- `F_(0_1)_b`(i, B, "mar_life")
  `F_(1_1)_draws_mar_life`[,i] <- `F_(1_1)_b`(i, B, "mar_life")
}

save("F_(0_0)_draws_mar_life", file = "/Workspace/YRBS_DR_mis/DistributionRegression_20172019_rec/F_(0_0)_draws_mar_life.RData")
save("F_(1_0)_draws_mar_life", file = "/Workspace/YRBS_DR_mis/DistributionRegression_20172019_rec/F_(1_0)_draws_mar_life.RData")
save("F_(0_1)_draws_mar_life", file = "/Workspace/YRBS_DR_mis/DistributionRegression_20172019_rec/F_(0_1)_draws_mar_life.RData")
save("F_(1_1)_draws_mar_life", file = "/Workspace/YRBS_DR_mis/DistributionRegression_20172019_rec/F_(1_1)_draws_mar_life.RData")

# Create Bootstrap draws
B <- 1000

"F_(0_0)_draws_cig" <- matrix(NA, ncol = 5, nrow = B)
"F_(1_0)_draws_cig" <- matrix(NA, ncol = 5, nrow = B)
"F_(0_1)_draws_cig" <- matrix(NA, ncol = 5, nrow = B)
"F_(1_1)_draws_cig" <- matrix(NA, ncol = 5, nrow = B)
for (i in 1:5) {
  `F_(0_0)_draws_cig`[,i] <- `F_(0_0)_b`(i, B, "cig")
  `F_(1_0)_draws_cig`[,i] <- `F_(1_0)_b`(i, B, "cig")
  `F_(0_1)_draws_cig`[,i] <- `F_(0_1)_b`(i, B, "cig")
  `F_(1_1)_draws_cig`[,i] <- `F_(1_1)_b`(i, B, "cig")
}

save("F_(0_0)_draws_cig", file = "/Workspace/YRBS_DR_mis/DistributionRegression_20172019_rec/F_(0_0)_draws_cig.RData")
save("F_(1_0)_draws_cig", file = "/Workspace/YRBS_DR_mis/DistributionRegression_20172019_rec/F_(1_0)_draws_cig.RData")
save("F_(0_1)_draws_cig", file = "/Workspace/YRBS_DR_mis/DistributionRegression_20172019_rec/F_(0_1)_draws_cig.RData")
save("F_(1_1)_draws_cig", file = "/Workspace/YRBS_DR_mis/DistributionRegression_20172019_rec/F_(1_1)_draws_cig.RData")


################ Step 2: Compute robust standard errors ####################
rm(list=ls())

for (i in list.files("/Workspace/YRBS_DR_mis/DistributionRegression_20172019_rec")) { # Load all the GJRM results and the data
  load(file = paste("/Workspace/YRBS_DR_mis/DistributionRegression_20172019_rec", i, sep = "/"))
}

################ Plots ####################
library(ggplot2)
library(Cairo)
p <- 0.9
for (drug in c("cig", "mar", "mar_life")) {
  # Overall Effect (1_1) - (0_0)
  x_values <- 0:4
  y_values_lower <- c(`B_(1_1)-(0_0)`(1, p, drug)[1], `B_(1_1)-(0_0)`(2, p, drug)[1], `B_(1_1)-(0_0)`(3, p, drug)[1], `B_(1_1)-(0_0)`(4, p, drug)[1], `B_(1_1)-(0_0)`(5, p, drug)[1])
  y_values_upper <- c(`B_(1_1)-(0_0)`(1, p, drug)[2], `B_(1_1)-(0_0)`(2, p, drug)[2], `B_(1_1)-(0_0)`(3, p, drug)[2], `B_(1_1)-(0_0)`(4, p, drug)[2], `B_(1_1)-(0_0)`(5, p, drug)[2])
  y_values <- c(`F_(1_1)`(1, drug) - `F_(0_0)`(1, drug), `F_(1_1)`(2, drug) - `F_(0_0)`(2, drug), `F_(1_1)`(3, drug) - `F_(0_0)`(3, drug), `F_(1_1)`(4, drug) - `F_(0_0)`(4, drug), `F_(1_1)`(5, drug) - `F_(0_0)`(5, drug))
  data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
  ribbon_data <-
    tibble(
      x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
      y_l = rep(y_values_lower, rep(2, 5)),
      y_u = rep(y_values_upper, rep(2, 5)))
  p1 <- ggplot2::ggplot(data = data) + 
    geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
    geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
    xlab("y") + ylab("p") + theme_bw() + ylim(c(-0.4,0.4))
  
  # Counterfactual Effect (1_1) - (1_0)
  x_values <- 0:4
  y_values_lower <- c(`B_(1_1)-(1_0)`(1, p, drug)[1], `B_(1_1)-(1_0)`(2, p, drug)[1], `B_(1_1)-(1_0)`(3, p, drug)[1], `B_(1_1)-(1_0)`(4, p, drug)[1], `B_(1_1)-(1_0)`(5, p, drug)[1])
  y_values_upper <- c(`B_(1_1)-(1_0)`(1, p, drug)[2], `B_(1_1)-(1_0)`(2, p, drug)[2], `B_(1_1)-(1_0)`(3, p, drug)[2], `B_(1_1)-(1_0)`(4, p, drug)[2], `B_(1_1)-(1_0)`(5, p, drug)[2])
  y_values <- c(`F_(1_1)`(1, drug) - `F_(1_0)`(1, drug), `F_(1_1)`(2, drug) - `F_(1_0)`(2, drug), `F_(1_1)`(3, drug) - `F_(1_0)`(3, drug), `F_(1_1)`(4, drug) - `F_(1_0)`(4, drug), `F_(1_1)`(5, drug) - `F_(1_0)`(5, drug))
  data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
  ribbon_data <-
    tibble(
      x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
      y_l = rep(y_values_lower, rep(2, 5)),
      y_u = rep(y_values_upper, rep(2, 5)))
  p2 <- ggplot2::ggplot(data = data) + 
    geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
    geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
    xlab("y") + ylab("p") + theme_bw() + ylim(c(-0.4,0.4))
  
  # Counterfactual Effect (1_0) - (0_0)
  x_values <- 0:4
  y_values_lower <- c(`B_(1_0)-(0_0)`(1, p, drug)[1], `B_(1_0)-(0_0)`(2, p, drug)[1], `B_(1_0)-(0_0)`(3, p, drug)[1], `B_(1_0)-(0_0)`(4, p, drug)[1], `B_(1_0)-(0_0)`(5, p, drug)[1])
  y_values_upper <- c(`B_(1_0)-(0_0)`(1, p, drug)[2], `B_(1_0)-(0_0)`(2, p, drug)[2], `B_(1_0)-(0_0)`(3, p, drug)[2], `B_(1_0)-(0_0)`(4, p, drug)[2], `B_(1_0)-(0_0)`(5, p, drug)[2])
  y_values <- c(`F_(1_0)`(1, drug) - `F_(0_0)`(1, drug), `F_(1_0)`(2, drug) - `F_(0_0)`(2, drug), `F_(1_0)`(3, drug) - `F_(0_0)`(3, drug), `F_(1_0)`(4, drug) - `F_(0_0)`(4, drug), `F_(1_0)`(5, drug) - `F_(0_0)`(5, drug))
  data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
  ribbon_data <-
    tibble(
      x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
      y_l = rep(y_values_lower, rep(2, 5)),
      y_u = rep(y_values_upper, rep(2, 5)))
  p3 <- ggplot2::ggplot(data = data) + 
    geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
    geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
    xlab("y") + ylab("p") + theme_bw() + ylim(c(-0.4,0.4))
  
  # Counterfactual Effect (1_1) - (0_1)
  x_values <- 0:4
  y_values_lower <- c(`B_(1_1)-(0_1)`(1, p, drug)[1], `B_(1_1)-(0_1)`(2, p, drug)[1], `B_(1_1)-(0_1)`(3, p, drug)[1], `B_(1_1)-(0_1)`(4, p, drug)[1], `B_(1_1)-(0_1)`(5, p, drug)[1])
  y_values_upper <- c(`B_(1_1)-(0_1)`(1, p, drug)[2], `B_(1_1)-(0_1)`(2, p, drug)[2], `B_(1_1)-(0_1)`(3, p, drug)[2], `B_(1_1)-(0_1)`(4, p, drug)[2], `B_(1_1)-(0_1)`(5, p, drug)[2])
  y_values <- c(`F_(1_1)`(1, drug) - `F_(0_1)`(1, drug), `F_(1_1)`(2, drug) - `F_(0_1)`(2, drug), `F_(1_1)`(3, drug) - `F_(0_1)`(3, drug), `F_(1_1)`(4, drug) - `F_(0_1)`(4, drug), `F_(1_1)`(5, drug) - `F_(0_1)`(5, drug))
  data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
  ribbon_data <-
    tibble(
      x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
      y_l = rep(y_values_lower, rep(2, 5)),
      y_u = rep(y_values_upper, rep(2, 5)))
  p4 <- ggplot2::ggplot(data = data) + 
    geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
    geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
    xlab("y") + ylab("p") + theme_bw() + ylim(c(-0.4,0.4))
  
  # Counterfactual Effect (0_1) - (0_0)
  x_values <- 0:4
  y_values_lower <- c(`B_(0_1)-(0_0)`(1, p, drug)[1], `B_(0_1)-(0_0)`(2, p, drug)[1], `B_(0_1)-(0_0)`(3, p, drug)[1], `B_(0_1)-(0_0)`(4, p, drug)[1], `B_(0_1)-(0_0)`(5, p, drug)[1])
  y_values_upper <- c(`B_(0_1)-(0_0)`(1, p, drug)[2], `B_(0_1)-(0_0)`(2, p, drug)[2], `B_(0_1)-(0_0)`(3, p, drug)[2], `B_(0_1)-(0_0)`(4, p, drug)[2], `B_(0_1)-(0_0)`(5, p, drug)[2])
  y_values <- c(`F_(0_1)`(1, drug) - `F_(0_0)`(1, drug), `F_(0_1)`(2, drug) - `F_(0_0)`(2, drug), `F_(0_1)`(3, drug) - `F_(0_0)`(3, drug), `F_(0_1)`(4, drug) - `F_(0_0)`(4, drug), `F_(0_1)`(5, drug) - `F_(0_0)`(5, drug))
  data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
  ribbon_data <-
    tibble(
      x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
      y_l = rep(y_values_lower, rep(2, 5)),
      y_u = rep(y_values_upper, rep(2, 5)))
  p5 <- ggplot2::ggplot(data = data) + 
    geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
    geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
    xlab("y") + ylab("p") + theme_bw() + ylim(c(-0.4,0.4))
  
  ggsave(filename = paste0("/Plots/DistributionRegression_20172019_rec/", drug, "/OverallEffect_rec.pdf"), plot = p1, device = cairo_pdf)
  ggsave(filename = paste0("/Plots/DistributionRegression_20172019_rec/", drug, "/CounterfactualEffect_(1_1)-(1_0)_rec.pdf"), plot = p2, device = cairo_pdf)
  ggsave(filename = paste0("/Plots/DistributionRegression_20172019_rec/", drug, "/CounterfactualEffect_(1_0)-(0_0)_rec.pdf"), plot = p3, device = cairo_pdf)
  ggsave(filename = paste0("/Plots/DistributionRegression_20172019_rec/", drug, "/CounterfactualEffect_(1_1)-(0_1)_rec.pdf"), plot = p4, device = cairo_pdf)
  ggsave(filename = paste0("/Plots/DistributionRegression_20172019_rec/", drug, "/CounterfactualEffect_(0_1)-(0_0)_rec.pdf"), plot = p5, device = cairo_pdf)
  
  # Overall Effect (1_1) - (0_0)
  x_values <- 0:4
  y_values_lower <- c(`B_(1_1)-(0_0)_restr`(1, p, drug)[1], `B_(1_1)-(0_0)_restr`(2, p, drug)[1], `B_(1_1)-(0_0)_restr`(3, p, drug)[1], `B_(1_1)-(0_0)_restr`(4, p, drug)[1], `B_(1_1)-(0_0)_restr`(5, p, drug)[1])
  y_values_upper <- c(`B_(1_1)-(0_0)_restr`(1, p, drug)[2], `B_(1_1)-(0_0)_restr`(2, p, drug)[2], `B_(1_1)-(0_0)_restr`(3, p, drug)[2], `B_(1_1)-(0_0)_restr`(4, p, drug)[2], `B_(1_1)-(0_0)_restr`(5, p, drug)[2])
  y_values <- c(`F_(1_1)`(1, drug) - `F_(0_0)`(1, drug), `F_(1_1)`(2, drug) - `F_(0_0)`(2, drug), `F_(1_1)`(3, drug) - `F_(0_0)`(3, drug), `F_(1_1)`(4, drug) - `F_(0_0)`(4, drug), `F_(1_1)`(5, drug) - `F_(0_0)`(5, drug))
  data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
  ribbon_data <-
    tibble(
      x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
      y_l = rep(y_values_lower, rep(2, 5)),
      y_u = rep(y_values_upper, rep(2, 5)))
  p6 <- ggplot2::ggplot(data = data) + 
    geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
    geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
    xlab("y") + ylab("p") + theme_bw() + ylim(c(-0.4,0.4))
  
  # Counterfactual Effect (1_1) - (1_0)
  x_values <- 0:4
  y_values_lower <- c(`B_(1_1)-(1_0)_restr`(1, p, drug)[1], `B_(1_1)-(1_0)_restr`(2, p, drug)[1], `B_(1_1)-(1_0)_restr`(3, p, drug)[1], `B_(1_1)-(1_0)_restr`(4, p, drug)[1], `B_(1_1)-(1_0)_restr`(5, p, drug)[1])
  y_values_upper <- c(`B_(1_1)-(1_0)_restr`(1, p, drug)[2], `B_(1_1)-(1_0)_restr`(2, p, drug)[2], `B_(1_1)-(1_0)_restr`(3, p, drug)[2], `B_(1_1)-(1_0)_restr`(4, p, drug)[2], `B_(1_1)-(1_0)_restr`(5, p, drug)[2])
  y_values <- c(`F_(1_1)`(1, drug) - `F_(1_0)`(1, drug), `F_(1_1)`(2, drug) - `F_(1_0)`(2, drug), `F_(1_1)`(3, drug) - `F_(1_0)`(3, drug), `F_(1_1)`(4, drug) - `F_(1_0)`(4, drug), `F_(1_1)`(5, drug) - `F_(1_0)`(5, drug))
  data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
  ribbon_data <-
    tibble(
      x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
      y_l = rep(y_values_lower, rep(2, 5)),
      y_u = rep(y_values_upper, rep(2, 5)))
  p7 <- ggplot2::ggplot(data = data) + 
    geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
    geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
    xlab("y") + ylab("p") + theme_bw() + ylim(c(-0.4,0.4))
  
  # Counterfactual Effect (1_0) - (0_0)
  x_values <- 0:4
  y_values_lower <- c(`B_(1_0)-(0_0)_restr`(1, p, drug)[1], `B_(1_0)-(0_0)_restr`(2, p, drug)[1], `B_(1_0)-(0_0)_restr`(3, p, drug)[1], `B_(1_0)-(0_0)_restr`(4, p, drug)[1], `B_(1_0)-(0_0)_restr`(5, p, drug)[1])
  y_values_upper <- c(`B_(1_0)-(0_0)_restr`(1, p, drug)[2], `B_(1_0)-(0_0)_restr`(2, p, drug)[2], `B_(1_0)-(0_0)_restr`(3, p, drug)[2], `B_(1_0)-(0_0)_restr`(4, p, drug)[2], `B_(1_0)-(0_0)_restr`(5, p, drug)[2])
  y_values <- c(`F_(1_0)`(1, drug) - `F_(0_0)`(1, drug), `F_(1_0)`(2, drug) - `F_(0_0)`(2, drug), `F_(1_0)`(3, drug) - `F_(0_0)`(3, drug), `F_(1_0)`(4, drug) - `F_(0_0)`(4, drug), `F_(1_0)`(5, drug) - `F_(0_0)`(5, drug))
  data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
  ribbon_data <-
    tibble(
      x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
      y_l = rep(y_values_lower, rep(2, 5)),
      y_u = rep(y_values_upper, rep(2, 5)))
  p8 <- ggplot2::ggplot(data = data) + 
    geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
    geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
    xlab("y") + ylab("p") + theme_bw() + ylim(c(-0.4,0.4))
  
  # Counterfactual Effect (1_1) - (0_1)
  x_values <- 0:4
  y_values_lower <- c(`B_(1_1)-(0_1)_restr`(1, p, drug)[1], `B_(1_1)-(0_1)_restr`(2, p, drug)[1], `B_(1_1)-(0_1)_restr`(3, p, drug)[1], `B_(1_1)-(0_1)_restr`(4, p, drug)[1], `B_(1_1)-(0_1)_restr`(5, p, drug)[1])
  y_values_upper <- c(`B_(1_1)-(0_1)_restr`(1, p, drug)[2], `B_(1_1)-(0_1)_restr`(2, p, drug)[2], `B_(1_1)-(0_1)_restr`(3, p, drug)[2], `B_(1_1)-(0_1)_restr`(4, p, drug)[2], `B_(1_1)-(0_1)_restr`(5, p, drug)[2])
  y_values <- c(`F_(1_1)`(1, drug) - `F_(0_1)`(1, drug), `F_(1_1)`(2, drug) - `F_(0_1)`(2, drug), `F_(1_1)`(3, drug) - `F_(0_1)`(3, drug), `F_(1_1)`(4, drug) - `F_(0_1)`(4, drug), `F_(1_1)`(5, drug) - `F_(0_1)`(5, drug))
  data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
  ribbon_data <-
    tibble(
      x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
      y_l = rep(y_values_lower, rep(2, 5)),
      y_u = rep(y_values_upper, rep(2, 5)))
  p9 <- ggplot2::ggplot(data = data) + 
    geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
    geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
    xlab("y") + ylab("p") + theme_bw() + ylim(c(-0.4,0.4))
  
  # Counterfactual Effect (0_1) - (0_0)
  x_values <- 0:4
  y_values_lower <- c(`B_(0_1)-(0_0)_restr`(1, p, drug)[1], `B_(0_1)-(0_0)_restr`(2, p, drug)[1], `B_(0_1)-(0_0)_restr`(3, p, drug)[1], `B_(0_1)-(0_0)_restr`(4, p, drug)[1], `B_(0_1)-(0_0)_restr`(5, p, drug)[1])
  y_values_upper <- c(`B_(0_1)-(0_0)_restr`(1, p, drug)[2], `B_(0_1)-(0_0)_restr`(2, p, drug)[2], `B_(0_1)-(0_0)_restr`(3, p, drug)[2], `B_(0_1)-(0_0)_restr`(4, p, drug)[2], `B_(0_1)-(0_0)_restr`(5, p, drug)[2])
  y_values <- c(`F_(0_1)`(1, drug) - `F_(0_0)`(1, drug), `F_(0_1)`(2, drug) - `F_(0_0)`(2, drug), `F_(0_1)`(3, drug) - `F_(0_0)`(3, drug), `F_(0_1)`(4, drug) - `F_(0_0)`(4, drug), `F_(0_1)`(5, drug) - `F_(0_0)`(5, drug))
  data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
  ribbon_data <-
    tibble(
      x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
      y_l = rep(y_values_lower, rep(2, 5)),
      y_u = rep(y_values_upper, rep(2, 5)))
  p10 <- ggplot2::ggplot(data = data) + 
    geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
    geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
    xlab("y") + ylab("p") + theme_bw() + ylim(c(-0.4,0.4))
  
  ggsave(filename = paste0("/Plots/DistributionRegression_20172019_rec/", drug, "/OverallEffect_restr_rec.pdf"), plot = p6, device = cairo_pdf)
  ggsave(filename = paste0("/Plots/DistributionRegression_20172019_rec/", drug, "/CounterfactualEffect_(1_1)-(1_0)_restr_rec.pdf"), plot = p7, device = cairo_pdf)
  ggsave(filename = paste0("/Plots/DistributionRegression_20172019_rec/", drug, "/CounterfactualEffect_(1_0)-(0_0)_restr_rec.pdf"), plot = p8, device = cairo_pdf)
  ggsave(filename = paste0("/Plots/DistributionRegression_20172019_rec/", drug, "/CounterfactualEffect_(1_1)-(0_1)_restr_rec.pdf"), plot = p9, device = cairo_pdf)
  ggsave(filename = paste0("/Plots/DistributionRegression_20172019_rec/", drug, "/CounterfactualEffect_(0_1)-(0_0)_restr_rec.pdf"), plot = p10, device = cairo_pdf)
  
  # (0_0)
  x_values <- 0:4
  y_values_lower <- c(`B_(0_0)`(1, p, drug)[1], `B_(0_0)`(2, p, drug)[1], `B_(0_0)`(3, p, drug)[1], `B_(0_0)`(4, p, drug)[1], `B_(0_0)`(5, p, drug)[1])
  y_values_upper <- c(`B_(0_0)`(1, p, drug)[2], `B_(0_0)`(2, p, drug)[2], `B_(0_0)`(3, p, drug)[2], `B_(0_0)`(4, p, drug)[2], `B_(0_0)`(5, p, drug)[2])
  y_values <- c(`F_(0_0)`(1, drug), `F_(0_0)`(2, drug), `F_(0_0)`(3, drug), `F_(0_0)`(4, drug), `F_(0_0)`(5, drug))
  data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
  ribbon_data <-
    tibble(
      x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
      y_l = rep(y_values_lower, rep(2, 5)),
      y_u = rep(y_values_upper, rep(2, 5)))
  p11 <- ggplot2::ggplot(data = data) + 
    geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
    geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
    xlab("y") + ylab("p") + theme_bw() + ylim(c(0,0.6))
  
  # (1_0)
  x_values <- 0:4
  y_values_lower <- c(`B_(1_0)`(1, p, drug)[1], `B_(1_0)`(2, p, drug)[1], `B_(1_0)`(3, p, drug)[1], `B_(1_0)`(4, p, drug)[1], `B_(1_0)`(5, p, drug)[1])
  y_values_upper <- c(`B_(1_0)`(1, p, drug)[2], `B_(1_0)`(2, p, drug)[2], `B_(1_0)`(3, p, drug)[2], `B_(1_0)`(4, p, drug)[2], `B_(1_0)`(5, p, drug)[2])
  y_values <- c(`F_(1_0)`(1, drug), `F_(1_0)`(2, drug), `F_(1_0)`(3, drug), `F_(1_0)`(4, drug), `F_(1_0)`(5, drug))
  data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
  ribbon_data <-
    tibble(
      x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
      y_l = rep(y_values_lower, rep(2, 5)),
      y_u = rep(y_values_upper, rep(2, 5)))
  p12 <- ggplot2::ggplot(data = data) + 
    geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
    geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
    xlab("y") + ylab("p") + theme_bw() + ylim(c(0,0.6))
  
  # (0_1)
  x_values <- 0:4
  y_values_lower <- c(`B_(0_1)`(1, p, drug)[1], `B_(0_1)`(2, p, drug)[1], `B_(0_1)`(3, p, drug)[1], `B_(0_1)`(4, p, drug)[1], `B_(0_1)`(5, p, drug)[1])
  y_values_upper <- c(`B_(0_1)`(1, p, drug)[2], `B_(0_1)`(2, p, drug)[2], `B_(0_1)`(3, p, drug)[2], `B_(0_1)`(4, p, drug)[2], `B_(0_1)`(5, p, drug)[2])
  y_values <- c(`F_(0_1)`(1, drug), `F_(0_1)`(2, drug), `F_(0_1)`(3, drug), `F_(0_1)`(4, drug), `F_(0_1)`(5, drug))
  data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
  ribbon_data <-
    tibble(
      x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
      y_l = rep(y_values_lower, rep(2, 5)),
      y_u = rep(y_values_upper, rep(2, 5)))
  p13 <- ggplot2::ggplot(data = data) + 
    geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
    geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
    xlab("y") + ylab("p") + theme_bw() + ylim(c(0,0.6))
  
  # (1_1)
  x_values <- 0:4
  y_values_lower <- c(`B_(1_1)`(1, p, drug)[1], `B_(1_1)`(2, p, drug)[1], `B_(1_1)`(3, p, drug)[1], `B_(1_1)`(4, p, drug)[1], `B_(1_1)`(5, p, drug)[1])
  y_values_upper <- c(`B_(1_1)`(1, p, drug)[2], `B_(1_1)`(2, p, drug)[2], `B_(1_1)`(3, p, drug)[2], `B_(1_1)`(4, p, drug)[2], `B_(1_1)`(5, p, drug)[2])
  y_values <- c(`F_(1_1)`(1, drug), `F_(1_1)`(2, drug), `F_(1_1)`(3, drug), `F_(1_1)`(4, drug), `F_(1_1)`(5, drug))
  data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
  ribbon_data <-
    tibble(
      x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
      y_l = rep(y_values_lower, rep(2, 5)),
      y_u = rep(y_values_upper, rep(2, 5)))
  p14 <- ggplot2::ggplot(data = data) + 
    geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
    geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
    xlab("y") + ylab("p") + theme_bw() + ylim(c(0,0.6))
  
  ggsave(filename = paste0("/Plots/DistributionRegression_20172019_rec/", drug, "/(0_0)_rec.pdf"), plot = p11, device = cairo_pdf)
  ggsave(filename = paste0("/Plots/DistributionRegression_20172019_rec/", drug, "/(1_0)_rec.pdf"), plot = p12, device = cairo_pdf)
  ggsave(filename = paste0("/Plots/DistributionRegression_20172019_rec/", drug, "/(0_1)_rec.pdf"), plot = p13, device = cairo_pdf)
  ggsave(filename = paste0("/Plots/DistributionRegression_20172019_rec/", drug, "/(1_1)_rec.pdf"), plot = p14, device = cairo_pdf)
  
  # (0_0)
  x_values <- 0:4
  y_values_lower <- c(`B_(0_0)_restr`(1, p, drug)[1], `B_(0_0)_restr`(2, p, drug)[1], `B_(0_0)_restr`(3, p, drug)[1], `B_(0_0)_restr`(4, p, drug)[1], `B_(0_0)_restr`(5, p, drug)[1])
  y_values_upper <- c(`B_(0_0)_restr`(1, p, drug)[2], `B_(0_0)_restr`(2, p, drug)[2], `B_(0_0)_restr`(3, p, drug)[2], `B_(0_0)_restr`(4, p, drug)[2], `B_(0_0)_restr`(5, p, drug)[2])
  y_values <- c(`F_(0_0)`(1, drug), `F_(0_0)`(2, drug), `F_(0_0)`(3, drug), `F_(0_0)`(4, drug), `F_(0_0)`(5, drug))
  data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
  ribbon_data <-
    tibble(
      x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
      y_l = rep(y_values_lower, rep(2, 5)),
      y_u = rep(y_values_upper, rep(2, 5)))
  p15 <- ggplot2::ggplot(data = data) + 
    geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
    geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
    xlab("y") + ylab("p") + theme_bw() + ylim(c(0,0.6))
  
  # (1_0)
  x_values <- 0:4
  y_values_lower <- c(`B_(1_0)_restr`(1, p, drug)[1], `B_(1_0)_restr`(2, p, drug)[1], `B_(1_0)_restr`(3, p, drug)[1], `B_(1_0)_restr`(4, p, drug)[1], `B_(1_0)_restr`(5, p, drug)[1])
  y_values_upper <- c(`B_(1_0)_restr`(1, p, drug)[2], `B_(1_0)_restr`(2, p, drug)[2], `B_(1_0)_restr`(3, p, drug)[2], `B_(1_0)_restr`(4, p, drug)[2], `B_(1_0)_restr`(5, p, drug)[2])
  y_values <- c(`F_(1_0)`(1, drug), `F_(1_0)`(2, drug), `F_(1_0)`(3, drug), `F_(1_0)`(4, drug), `F_(1_0)`(5, drug))
  data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
  ribbon_data <-
    tibble(
      x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
      y_l = rep(y_values_lower, rep(2, 5)),
      y_u = rep(y_values_upper, rep(2, 5)))
  p16 <- ggplot2::ggplot(data = data) + 
    geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
    geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
    xlab("y") + ylab("p") + theme_bw() + ylim(c(0,0.6))
  
  # (0_1)
  x_values <- 0:4
  y_values_lower <- c(`B_(0_1)_restr`(1, p, drug)[1], `B_(0_1)_restr`(2, p, drug)[1], `B_(0_1)_restr`(3, p, drug)[1], `B_(0_1)_restr`(4, p, drug)[1], `B_(0_1)_restr`(5, p, drug)[1])
  y_values_upper <- c(`B_(0_1)_restr`(1, p, drug)[2], `B_(0_1)_restr`(2, p, drug)[2], `B_(0_1)_restr`(3, p, drug)[2], `B_(0_1)_restr`(4, p, drug)[2], `B_(0_1)_restr`(5, p, drug)[2])
  y_values <- c(`F_(0_1)`(1, drug), `F_(0_1)`(2, drug), `F_(0_1)`(3, drug), `F_(0_1)`(4, drug), `F_(0_1)`(5, drug))
  data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
  ribbon_data <-
    tibble(
      x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
      y_l = rep(y_values_lower, rep(2, 5)),
      y_u = rep(y_values_upper, rep(2, 5)))
  p17 <- ggplot2::ggplot(data = data) + 
    geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
    geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
    xlab("y") + ylab("p") + theme_bw() + ylim(c(0,0.6))
  
  # (1_1)
  x_values <- 0:4
  y_values_lower <- c(`B_(1_1)_restr`(1, p, drug)[1], `B_(1_1)_restr`(2, p, drug)[1], `B_(1_1)_restr`(3, p, drug)[1], `B_(1_1)_restr`(4, p, drug)[1], `B_(1_1)_restr`(5, p, drug)[1])
  y_values_upper <- c(`B_(1_1)_restr`(1, p, drug)[2], `B_(1_1)_restr`(2, p, drug)[2], `B_(1_1)_restr`(3, p, drug)[2], `B_(1_1)_restr`(4, p, drug)[2], `B_(1_1)_restr`(5, p, drug)[2])
  y_values <- c(`F_(1_1)`(1, drug), `F_(1_1)`(2, drug), `F_(1_1)`(3, drug), `F_(1_1)`(4, drug), `F_(1_1)`(5, drug))
  data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
  ribbon_data <-
    tibble(
      x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
      y_l = rep(y_values_lower, rep(2, 5)),
      y_u = rep(y_values_upper, rep(2, 5)))
  p18 <- ggplot2::ggplot(data = data) + 
    geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
    geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
    xlab("y") + ylab("p") + theme_bw() + ylim(c(0,0.6))
  
  ggsave(filename = paste0("/Plots/DistributionRegression_20172019_rec/", drug, "/(0_0)_restr_rec.pdf"), plot = p15, device = cairo_pdf)
  ggsave(filename = paste0("/Plots/DistributionRegression_20172019_rec/", drug, "/(1_0)_restr_rec.pdf"), plot = p16, device = cairo_pdf)
  ggsave(filename = paste0("/Plots/DistributionRegression_20172019_rec/", drug, "/(0_1)_restr_rec.pdf"), plot = p17, device = cairo_pdf)
  ggsave(filename = paste0("/Plots/DistributionRegression_20172019_rec/", drug, "/(1_1)_restr_rec.pdf"), plot = p18, device = cairo_pdf)
}

