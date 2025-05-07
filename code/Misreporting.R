# Title:      First Estimation Step: Equation (11)
# Author:     Jan-Lukas Wermuth
# Date:       2025-05-06
# Purpose:    This script performs the estimation described in 
#             the paper in equation (11). The results are needed
#             for upper tail probability and decomposition estimates
#             in Figure 1 and 2 for all the plots in which misreporting
#             is allowed. For the estimates needed in Figure 3 and 4,
#             replace "BPO" with "BPO0" in all the gjrm-commands.

rm(list = ls())

library(dplyr)
library(GJRM)

setwd("~/Dropbox/GLW/replication_DRmisr")
load("data/yrbs_data_combined_201520172019_norecreational_mar_life.RData")
load("data/yrbs_data_combined_201520172019_recreational_mar_life.RData")
load("data/X_norecreational_mar_life.RData")
load("data/X_recreational_mar_life.RData")

# GJRM Estimation: Equation (11) ------------------------------------------
for (j in c("norecreational", "recreational")) {
  for (k in 1:6) {
    formula <- list(paste(paste("mar_life", k, sep = ""), "~", "minority + sex + age_18 + median_income + unemployment_rate + mar_prices_medium"),
                    paste(paste("mar_life", k, sep = ""), "~", "minority + sex + age_18 + median_income + unemployment_rate + questions"))
    assign(paste("mar_life", "gjrm", j, k, sep = "_"), GJRM::gjrm(formula = list(as.formula(formula[[1]]), as.formula(formula[[2]])),
                                                         data = get(paste("yrbs_data_combined_201520172019", j, "mar_life", sep = "_")), model = "BPO", margins = c("probit", "probit")))
    save(list = paste("mar_life", "gjrm", j, k, sep = "_"), file = paste("results/GJRM_results", paste("mar_life", "gjrm", j, k, ".RData", sep = "_"), sep = "/"))
  }
}

