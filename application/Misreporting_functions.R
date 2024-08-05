# Survival Functions together with counterfactual survival functions
# j: Input of the survival function
"F_(0_0)" <- function(j, drug){
  if (drug == "cig"){
    X_mat <- as.matrix(cbind(1, select(get(paste("X_norecreational", drug, sep = "_")),-c("mar_prices_medium", "questions"))))
    Z_mat <- as.matrix(cbind(1, select(get(paste("X_norecreational", drug, sep = "_")),-c("tax_percent", "mar_prices_medium"))))
  }
  if (drug == "mar" | drug == "mar_life"){
    X_mat <- as.matrix(cbind(1, select(get(paste("X_norecreational", drug, sep = "_")),-c("tax_percent", "questions"))))
    Z_mat <- as.matrix(cbind(1, select(get(paste("X_norecreational", drug, sep = "_")),-c("tax_percent", "mar_prices_medium"))))
  }
  x_length <- ncol(X_mat)
  z_length <- ncol(Z_mat)
  theta <- get(paste(drug, "gjrm_norecreational", j, sep = "_"))$theta
  coefficients <- get(paste(drug, "gjrm_norecreational", j, sep = "_"))$coefficients
  mean(pmnorm(cbind(X_mat %*% coefficients[1:x_length],
                    Z_mat %*% coefficients[(x_length + 1) : (x_length + z_length)]),
              varcov = matrix(c(1, theta, theta, 1), ncol = 2)))
}
"F_(1_0)" <- function(j, drug){
  if (drug == "cig"){
    X_mat <- as.matrix(cbind(1, select(get(paste("X_norecreational", drug, sep = "_")),-c("mar_prices_medium", "questions"))))
    Z_mat <- as.matrix(cbind(1, select(get(paste("X_norecreational", drug, sep = "_")),-c("tax_percent", "mar_prices_medium"))))
  }
  if (drug == "mar" | drug == "mar_life"){
    X_mat <- as.matrix(cbind(1, select(get(paste("X_norecreational", drug, sep = "_")),-c("tax_percent", "questions"))))
    Z_mat <- as.matrix(cbind(1, select(get(paste("X_norecreational", drug, sep = "_")),-c("tax_percent", "mar_prices_medium"))))
  }
  x_length <- ncol(X_mat)
  z_length <- ncol(Z_mat)
  theta <- get(paste(drug, "gjrm_recreational", j, sep = "_"))$theta
  coefficients <- get(paste(drug, "gjrm_recreational", j, sep = "_"))$coefficients
  mean(pmnorm(cbind(X_mat %*% coefficients[1:x_length],
                    Z_mat %*% coefficients[(x_length + 1) : (x_length + z_length)]),
              varcov = matrix(c(1, theta, theta, 1), ncol = 2)))
}
"F_(0_1)" <- function(j, drug){
  if (drug == "cig"){
    X_mat <- as.matrix(cbind(1, select(get(paste("X_recreational", drug, sep = "_")),-c("mar_prices_medium", "questions"))))
    Z_mat <- as.matrix(cbind(1, select(get(paste("X_recreational", drug, sep = "_")),-c("tax_percent", "mar_prices_medium"))))
  }
  if (drug == "mar" | drug == "mar_life"){
    X_mat <- as.matrix(cbind(1, select(get(paste("X_recreational", drug, sep = "_")),-c("tax_percent", "questions"))))
    Z_mat <- as.matrix(cbind(1, select(get(paste("X_recreational", drug, sep = "_")),-c("tax_percent", "mar_prices_medium"))))
  }
  x_length <- ncol(X_mat)
  z_length <- ncol(Z_mat)
  theta <- get(paste(drug, "gjrm_norecreational", j, sep = "_"))$theta
  coefficients <- get(paste(drug, "gjrm_norecreational", j, sep = "_"))$coefficients
  mean(pmnorm(cbind(X_mat %*% coefficients[1:x_length],
                    Z_mat %*% coefficients[(x_length + 1) : (x_length + z_length)]),
              varcov = matrix(c(1, theta, theta, 1), ncol = 2)))
}
"F_(1_1)" <- function(j, drug){
  if (drug == "cig"){
    X_mat <- as.matrix(cbind(1, select(get(paste("X_recreational", drug, sep = "_")),-c("mar_prices_medium", "questions"))))
    Z_mat <- as.matrix(cbind(1, select(get(paste("X_recreational", drug, sep = "_")),-c("tax_percent", "mar_prices_medium"))))
  }
  if (drug == "mar" | drug == "mar_life"){
    X_mat <- as.matrix(cbind(1, select(get(paste("X_recreational", drug, sep = "_")),-c("tax_percent", "questions"))))
    Z_mat <- as.matrix(cbind(1, select(get(paste("X_recreational", drug, sep = "_")),-c("tax_percent", "mar_prices_medium"))))
  }
  x_length <- ncol(X_mat)
  z_length <- ncol(Z_mat)
  theta <- get(paste(drug, "gjrm_recreational", j, sep = "_"))$theta
  coefficients <- get(paste(drug, "gjrm_recreational", j, sep = "_"))$coefficients
  mean(pmnorm(cbind(X_mat %*% coefficients[1:x_length],
                    Z_mat %*% coefficients[(x_length + 1) : (x_length + z_length)]),
              varcov = matrix(c(1, theta, theta, 1), ncol = 2)))
}

save("F_(0_0)", file = "/Workspace/YRBS_DR_mis/DistributionRegression_20172019_rec/F_(0_0).RData")
save("F_(1_0)", file = "/Workspace/YRBS_DR_mis/DistributionRegression_20172019_rec/F_(1_0).RData")
save("F_(0_1)", file = "/Workspace/YRBS_DR_mis/DistributionRegression_20172019_rec/F_(0_1).RData")
save("F_(1_1)", file = "/Workspace/YRBS_DR_mis/DistributionRegression_20172019_rec/F_(1_1).RData")



# Bootstrap -----------------------------------------------------------
# Step 1: Obtain Bootstrap draws
###### "F_(0_0)" bootstrapped
# Function which allows to compute one bootstrapped function
# j: Input of the Survival function
# B: Bootstrap draws
"F_(0_0)_b" <- function(j, B, drug){
  if (drug == "cig"){
    X_mat <- as.matrix(cbind(1, select(get(paste("X_norecreational", drug, sep = "_")),-c("mar_prices_medium", "questions"))))
    Z_mat <- as.matrix(cbind(1, select(get(paste("X_norecreational", drug, sep = "_")),-c("tax_percent", "mar_prices_medium"))))
  }
  if (drug == "mar" | drug == "mar_life"){
    X_mat <- as.matrix(cbind(1, select(get(paste("X_norecreational", drug, sep = "_")),-c("tax_percent", "questions"))))
    Z_mat <- as.matrix(cbind(1, select(get(paste("X_norecreational", drug, sep = "_")),-c("tax_percent", "mar_prices_medium"))))
  }
  n <- nrow(X_mat)
  
  # Start cluster for parallel computing
  cl <- makeCluster(detectCores() - 1, type = "PSOCK")
  registerDoParallel(cl)
  res <- foreach(i = 1:B, .combine = 'c', .export = paste("yrbs_data_combined_20172019_norecreational", drug, sep = "_"), .packages = 'mnormt') %dopar% {
    set.seed(i)
    mult_weights_covariates <- sample(x = 1:n, size = n, replace = TRUE)
    X_mat <- X_mat[mult_weights_covariates,]
    Z_mat <- Z_mat[mult_weights_covariates,]
    x_length <- ncol(X_mat)
    z_length <- ncol(Z_mat)
    formula_mar <- list(paste(paste(drug, j, sep = ""), "~", "year + median_income + sex + unemployment_rate + mar_prices_medium"),
                        paste(paste(drug, j, sep = ""), "~", "year + median_income + sex + unemployment_rate + questions"))
    formula_mar_life <- list(paste(paste(drug, j, sep = ""), "~", "year + median_income + sex + unemployment_rate + mar_prices_medium"),
                        paste(paste(drug, j, sep = ""), "~", "year + median_income + sex + unemployment_rate + questions"))
    formula_cig <- list(paste(paste(drug, j, sep = ""), "~", "year + median_income + sex + unemployment_rate + tax_percent"),
                        paste(paste(drug, j, sep = ""), "~", "year + median_income + sex + unemployment_rate + questions"))
    gjrm_res <- GJRM::gjrm(formula = list(as.formula(get(paste("formula", drug, sep = "_"))[[1]]), as.formula(get(paste("formula", drug, sep = "_"))[[2]])),
                           data = get(paste("yrbs_data_combined_20172019_norecreational", drug, sep = "_")), model = "BPO", margins = c("probit", "probit"), weights = rmultinom(1, size = nrow(get(paste("yrbs_data_combined_20172019_norecreational", drug, sep = "_"))), prob = rep(1/nrow(get(paste("yrbs_data_combined_20172019_norecreational", drug, sep = "_"))), nrow(get(paste("yrbs_data_combined_20172019_norecreational", drug, sep = "_"))))))
    theta <- gjrm_res$theta
    coefficients <- gjrm_res$coefficients
    mean(mnormt::pmnorm(cbind(X_mat %*% coefficients[1:x_length],
                              Z_mat %*% coefficients[(x_length + 1) : (x_length + z_length)]),
                        varcov = matrix(c(1, theta, theta, 1), ncol = 2)))
  }
  stopCluster(cl)
  return(res)
}

###### "F_(1_0)" bootstrapped
# Function which allows to compute one bootstrapped function
# j: Input of the Survival function
# B: Bootstrap draws
"F_(1_0)_b" <- function(j, B, drug){ 
  if (drug == "cig"){
    X_mat <- as.matrix(cbind(1, select(get(paste("X_norecreational", drug, sep = "_")),-c("mar_prices_medium", "questions"))))
    Z_mat <- as.matrix(cbind(1, select(get(paste("X_norecreational", drug, sep = "_")),-c("tax_percent", "mar_prices_medium"))))
  }
  if (drug == "mar" | drug == "mar_life"){
    X_mat <- as.matrix(cbind(1, select(get(paste("X_norecreational", drug, sep = "_")),-c("tax_percent", "questions"))))
    Z_mat <- as.matrix(cbind(1, select(get(paste("X_norecreational", drug, sep = "_")),-c("tax_percent", "mar_prices_medium"))))
  }
  n <- nrow(X_mat)
  
  # Start cluster for parallel computing
  cl <- makeCluster(detectCores() - 1, type = "PSOCK") 
  registerDoParallel(cl)
  res <- foreach(i = 1:B, .combine = 'c', .export = paste("yrbs_data_combined_20172019_recreational", drug, sep = "_"), .packages = 'mnormt') %dopar% {
    set.seed(i)
    mult_weights_covariates <- sample(x = 1:n, size = n, replace = TRUE)
    X_mat <- X_mat[mult_weights_covariates,]
    Z_mat <- Z_mat[mult_weights_covariates,]
    x_length <- ncol(X_mat)
    z_length <- ncol(Z_mat)
    formula_mar <- list(paste(paste(drug, j, sep = ""), "~", "year + median_income + sex + unemployment_rate + mar_prices_medium"),
                        paste(paste(drug, j, sep = ""), "~", "year + median_income + sex + unemployment_rate + questions"))
    formula_mar_life <- list(paste(paste(drug, j, sep = ""), "~", "year + median_income + sex + unemployment_rate + mar_prices_medium"),
                             paste(paste(drug, j, sep = ""), "~", "year + median_income + sex + unemployment_rate + questions"))
    formula_cig <- list(paste(paste(drug, j, sep = ""), "~", "year + median_income + sex + unemployment_rate + tax_percent"),
                        paste(paste(drug, j, sep = ""), "~", "year + median_income + sex + unemployment_rate + questions"))
    gjrm_res <- GJRM::gjrm(formula = list(as.formula(get(paste("formula", drug, sep = "_"))[[1]]), as.formula(get(paste("formula", drug, sep = "_"))[[2]])),
                           data = get(paste("yrbs_data_combined_20172019_recreational", drug, sep = "_")), model = "BPO", margins = c("probit", "probit"), weights = rmultinom(1, size = nrow(get(paste("yrbs_data_combined_20172019_recreational", drug, sep = "_"))), prob = rep(1/nrow(get(paste("yrbs_data_combined_20172019_recreational", drug, sep = "_"))), nrow(get(paste("yrbs_data_combined_20172019_recreational", drug, sep = "_"))))))
    theta <- gjrm_res$theta
    coefficients <- gjrm_res$coefficients
    mean(mnormt::pmnorm(cbind(X_mat %*% coefficients[1:x_length],
                              Z_mat %*% coefficients[(x_length + 1) : (x_length + z_length)]),
                        varcov = matrix(c(1, theta, theta, 1), ncol = 2)))
  }
  stopCluster(cl)
  return(res)
}
###### "F_(0_1)" bootstrapped
# Function which allows to compute one bootstrapped function
# j: Input of the Survival function
# B: Bootstrap draws
"F_(0_1)_b" <- function(j, B, drug){
  if (drug == "cig"){
    X_mat <- as.matrix(cbind(1, select(get(paste("X_recreational", drug, sep = "_")),-c("mar_prices_medium", "questions"))))
    Z_mat <- as.matrix(cbind(1, select(get(paste("X_recreational", drug, sep = "_")),-c("tax_percent", "mar_prices_medium"))))
  }
  if (drug == "mar" | drug == "mar_life"){
    X_mat <- as.matrix(cbind(1, select(get(paste("X_recreational", drug, sep = "_")),-c("tax_percent", "questions"))))
    Z_mat <- as.matrix(cbind(1, select(get(paste("X_recreational", drug, sep = "_")),-c("tax_percent", "mar_prices_medium"))))
  }
  n <- nrow(X_mat)
  
  # Start cluster for parallel computing
  cl <- makeCluster(detectCores() - 1, type = "PSOCK") 
  registerDoParallel(cl)
  res <- foreach(i = 1:B, .combine = 'c', .export = paste("yrbs_data_combined_20172019_norecreational", drug, sep = "_"), .packages = 'mnormt') %dopar% {
    set.seed(i)
    mult_weights_covariates <- sample(x = 1:n, size = n, replace = TRUE)
    X_mat <- X_mat[mult_weights_covariates,]
    Z_mat <- Z_mat[mult_weights_covariates,]
    x_length <- ncol(X_mat)
    z_length <- ncol(Z_mat)
    formula_mar <- list(paste(paste(drug, j, sep = ""), "~", "year + median_income + sex + unemployment_rate + mar_prices_medium"),
                    paste(paste(drug, j, sep = ""), "~", "year + median_income + sex + unemployment_rate + questions"))
    formula_mar_life <- list(paste(paste(drug, j, sep = ""), "~", "year + median_income + sex + unemployment_rate + mar_prices_medium"),
                             paste(paste(drug, j, sep = ""), "~", "year + median_income + sex + unemployment_rate + questions"))
    formula_cig <- list(paste(paste(drug, j, sep = ""), "~", "year + median_income + sex + unemployment_rate + tax_percent"),
                        paste(paste(drug, j, sep = ""), "~", "year + median_income + sex + unemployment_rate + questions"))
    gjrm_res <- GJRM::gjrm(formula = list(as.formula(get(paste("formula", drug, sep = "_"))[[1]]), as.formula(get(paste("formula", drug, sep = "_"))[[2]])),
                           data = get(paste("yrbs_data_combined_20172019_norecreational", drug, sep = "_")), model = "BPO", margins = c("probit", "probit"), weights = rmultinom(1, size = nrow(get(paste("yrbs_data_combined_20172019_norecreational", drug, sep = "_"))), prob = rep(1/nrow(get(paste("yrbs_data_combined_20172019_norecreational", drug, sep = "_"))), nrow(get(paste("yrbs_data_combined_20172019_norecreational", drug, sep = "_"))))))
    theta <- gjrm_res$theta
    coefficients <- gjrm_res$coefficients
    mean(mnormt::pmnorm(cbind(X_mat %*% coefficients[1:x_length],
                              Z_mat %*% coefficients[(x_length + 1) : (x_length + z_length)]),
                        varcov = matrix(c(1, theta, theta, 1), ncol = 2)))
  }
  stopCluster(cl)
  return(res)
}

###### "F_(1_1)" bootstrapped
# Function which allows to compute one bootstrapped function
# j: Input of the Survival function
# B: Bootstrap draws
"F_(1_1)_b" <- function(j, B, drug){
  if (drug == "cig"){
    X_mat <- as.matrix(cbind(1, select(get(paste("X_recreational", drug, sep = "_")),-c("mar_prices_medium", "questions"))))
    Z_mat <- as.matrix(cbind(1, select(get(paste("X_recreational", drug, sep = "_")),-c("tax_percent", "mar_prices_medium"))))
  }
  if (drug == "mar" | drug == "mar_life"){
    X_mat <- as.matrix(cbind(1, select(get(paste("X_recreational", drug, sep = "_")),-c("tax_percent", "questions"))))
    Z_mat <- as.matrix(cbind(1, select(get(paste("X_recreational", drug, sep = "_")),-c("tax_percent", "mar_prices_medium"))))
  }  
  n <- nrow(X_mat)
  
  # Start cluster for parallel computing
  cl <- makeCluster(detectCores() - 1, type = "PSOCK") 
  registerDoParallel(cl)
  res <- foreach(i = 1:B, .combine = 'c', .export = paste("yrbs_data_combined_20172019_recreational", drug, sep = "_"), .packages = 'mnormt') %dopar% {
    set.seed(i)
    mult_weights_covariates <- sample(x = 1:n, size = n, replace = TRUE)
    X_mat <- X_mat[mult_weights_covariates,]
    Z_mat <- Z_mat[mult_weights_covariates,]
    x_length <- ncol(X_mat)
    z_length <- ncol(Z_mat)
    formula_mar <- list(paste(paste(drug, j, sep = ""), "~", "year + median_income + sex + unemployment_rate + mar_prices_medium"),
                        paste(paste(drug, j, sep = ""), "~", "year + median_income + sex + unemployment_rate + questions"))
    formula_mar_life <- list(paste(paste(drug, j, sep = ""), "~", "year + median_income + sex + unemployment_rate + mar_prices_medium"),
                             paste(paste(drug, j, sep = ""), "~", "year + median_income + sex + unemployment_rate + questions"))
    formula_cig <- list(paste(paste(drug, j, sep = ""), "~", "year + median_income + sex + unemployment_rate + tax_percent"),
                        paste(paste(drug, j, sep = ""), "~", "year + median_income + sex + unemployment_rate + questions"))
    gjrm_res <- GJRM::gjrm(formula = list(as.formula(get(paste("formula", drug, sep = "_"))[[1]]), as.formula(get(paste("formula", drug, sep = "_"))[[2]])),
                           data = get(paste("yrbs_data_combined_20172019_recreational", drug, sep = "_")), model = "BPO", margins = c("probit", "probit"), weights = rmultinom(1, size = nrow(get(paste("yrbs_data_combined_20172019_recreational", drug, sep = "_"))), prob = rep(1/nrow(get(paste("yrbs_data_combined_20172019_recreational", drug, sep = "_"))), nrow(get(paste("yrbs_data_combined_20172019_recreational", drug, sep = "_"))))))
    theta <- gjrm_res$theta
    coefficients <- gjrm_res$coefficients
    mean(mnormt::pmnorm(cbind(X_mat %*% coefficients[1:x_length],
                              Z_mat %*% coefficients[(x_length + 1) : (x_length + z_length)]),
                        varcov = matrix(c(1, theta, theta, 1), ncol = 2)))
  }
  stopCluster(cl)
  return(res)
}


################ Step 2: Compute robust standard errors ####################
# Insert j %in% 1:5
# "F_(0_0)"
"s_k_00" <- function(j, drug){
  (quantile(get(paste("F_(0_0)_draws", drug, sep = "_"))[,j], probs = 0.75) - quantile(get(paste("F_(0_0)_draws", drug, sep = "_"))[,j], probs = 0.25)) / (qnorm(0.75) - qnorm(0.25))
}
# "F_(1_0)"
"s_k_10" <- function(j, drug){
  (quantile(get(paste("F_(1_0)_draws", drug, sep = "_"))[,j], probs = 0.75) - quantile(get(paste("F_(1_0)_draws", drug, sep = "_"))[,j], probs = 0.25)) / (qnorm(0.75) - qnorm(0.25))
}
# "F_(0_1)"
"s_k_01" <- function(j, drug){
  (quantile(get(paste("F_(0_1)_draws", drug, sep = "_"))[,j], probs = 0.75) - quantile(get(paste("F_(0_1)_draws", drug, sep = "_"))[,j], probs = 0.25)) / (qnorm(0.75) - qnorm(0.25))
}
# "F_(1_1)"
"s_k_11" <- function(j, drug){
  (quantile(get(paste("F_(1_1)_draws", drug, sep = "_"))[,j], probs = 0.75) - quantile(get(paste("F_(1_1)_draws", drug, sep = "_"))[,j], probs = 0.25)) / (qnorm(0.75) - qnorm(0.25))
}

################ Step 3: Compute critical value ####################
# Compute critical value: Overall Effect (1_1) - (0_0)
"c_(1_1)-(0_0)" <- function(p, drug){
  mat <- cbind(
    abs(get(paste("F_(1_1)_draws", drug, sep = "_"))[, 1] - `F_(1_1)`(1, drug)) / `s_k_11`(1, drug),
    abs(get(paste("F_(1_1)_draws", drug, sep = "_"))[, 2] - `F_(1_1)`(2, drug)) / `s_k_11`(2, drug),
    abs(get(paste("F_(1_1)_draws", drug, sep = "_"))[, 3] - `F_(1_1)`(3, drug)) / `s_k_11`(3, drug),
    abs(get(paste("F_(1_1)_draws", drug, sep = "_"))[, 4] - `F_(1_1)`(4, drug)) / `s_k_11`(4, drug),
    abs(get(paste("F_(1_1)_draws", drug, sep = "_"))[, 5] - `F_(1_1)`(5, drug)) / `s_k_11`(5, drug),
    abs(get(paste("F_(0_0)_draws", drug, sep = "_"))[, 1] - `F_(0_0)`(1, drug)) / `s_k_00`(1, drug),
    abs(get(paste("F_(0_0)_draws", drug, sep = "_"))[, 2] - `F_(0_0)`(2, drug)) / `s_k_00`(2, drug),
    abs(get(paste("F_(0_0)_draws", drug, sep = "_"))[, 3] - `F_(0_0)`(3, drug)) / `s_k_00`(3, drug),
    abs(get(paste("F_(0_0)_draws", drug, sep = "_"))[, 4] - `F_(0_0)`(4, drug)) / `s_k_00`(4, drug),
    abs(get(paste("F_(0_0)_draws", drug, sep = "_"))[, 5] - `F_(0_0)`(5, drug)) / `s_k_00`(5, drug)
  )
  dist <- apply(mat, MARGIN = 1, max)
  quantile(dist, probs = p)
}

# Compute critical value: Counterfactual Effect (1_1) - (1_0)
"c_(1_1)-(1_0)" <- function(p, drug){
  mat <- cbind(
    abs(get(paste("F_(1_1)_draws", drug, sep = "_"))[, 1] - `F_(1_1)`(1, drug)) / `s_k_11`(1, drug),
    abs(get(paste("F_(1_1)_draws", drug, sep = "_"))[, 2] - `F_(1_1)`(2, drug)) / `s_k_11`(2, drug),
    abs(get(paste("F_(1_1)_draws", drug, sep = "_"))[, 3] - `F_(1_1)`(3, drug)) / `s_k_11`(3, drug),
    abs(get(paste("F_(1_1)_draws", drug, sep = "_"))[, 4] - `F_(1_1)`(4, drug)) / `s_k_11`(4, drug),
    abs(get(paste("F_(1_1)_draws", drug, sep = "_"))[, 5] - `F_(1_1)`(5, drug)) / `s_k_11`(5, drug),
    abs(get(paste("F_(1_0)_draws", drug, sep = "_"))[, 1] - `F_(1_0)`(1, drug)) / `s_k_10`(1, drug),
    abs(get(paste("F_(1_0)_draws", drug, sep = "_"))[, 2] - `F_(1_0)`(2, drug)) / `s_k_10`(2, drug),
    abs(get(paste("F_(1_0)_draws", drug, sep = "_"))[, 3] - `F_(1_0)`(3, drug)) / `s_k_10`(3, drug),
    abs(get(paste("F_(1_0)_draws", drug, sep = "_"))[, 4] - `F_(1_0)`(4, drug)) / `s_k_10`(4, drug),
    abs(get(paste("F_(1_0)_draws", drug, sep = "_"))[, 5] - `F_(1_0)`(5, drug)) / `s_k_10`(5, drug)
  )
  dist <- apply(mat, MARGIN = 1, max)
  quantile(dist, probs = p)
}

# Compute critical value: Counterfactual Effect (1_0) - (0_0)
"c_(1_0)-(0_0)" <- function(p, drug){
  mat <- cbind(
    abs(get(paste("F_(1_0)_draws", drug, sep = "_"))[, 1] - `F_(1_0)`(1, drug)) / `s_k_10`(1, drug),
    abs(get(paste("F_(1_0)_draws", drug, sep = "_"))[, 2] - `F_(1_0)`(2, drug)) / `s_k_10`(2, drug),
    abs(get(paste("F_(1_0)_draws", drug, sep = "_"))[, 3] - `F_(1_0)`(3, drug)) / `s_k_10`(3, drug),
    abs(get(paste("F_(1_0)_draws", drug, sep = "_"))[, 4] - `F_(1_0)`(4, drug)) / `s_k_10`(4, drug),
    abs(get(paste("F_(1_0)_draws", drug, sep = "_"))[, 5] - `F_(1_0)`(5, drug)) / `s_k_10`(5, drug),
    abs(get(paste("F_(0_0)_draws", drug, sep = "_"))[, 1] - `F_(0_0)`(1, drug)) / `s_k_00`(1, drug),
    abs(get(paste("F_(0_0)_draws", drug, sep = "_"))[, 2] - `F_(0_0)`(2, drug)) / `s_k_00`(2, drug),
    abs(get(paste("F_(0_0)_draws", drug, sep = "_"))[, 3] - `F_(0_0)`(3, drug)) / `s_k_00`(3, drug),
    abs(get(paste("F_(0_0)_draws", drug, sep = "_"))[, 4] - `F_(0_0)`(4, drug)) / `s_k_00`(4, drug),
    abs(get(paste("F_(0_0)_draws", drug, sep = "_"))[, 5] - `F_(0_0)`(5, drug)) / `s_k_00`(5, drug)
  )
  dist <- apply(mat, MARGIN = 1, max)
  quantile(dist, probs = p)
}
# Compute critical value: Counterfactual Effect (1_1) - (0_1)
"c_(1_1)-(0_1)" <- function(p, drug){
  mat <- cbind(
    abs(get(paste("F_(1_1)_draws", drug, sep = "_"))[, 1] - `F_(1_1)`(1, drug)) / `s_k_11`(1, drug),
    abs(get(paste("F_(1_1)_draws", drug, sep = "_"))[, 2] - `F_(1_1)`(2, drug)) / `s_k_11`(2, drug),
    abs(get(paste("F_(1_1)_draws", drug, sep = "_"))[, 3] - `F_(1_1)`(3, drug)) / `s_k_11`(3, drug),
    abs(get(paste("F_(1_1)_draws", drug, sep = "_"))[, 4] - `F_(1_1)`(4, drug)) / `s_k_11`(4, drug),
    abs(get(paste("F_(1_1)_draws", drug, sep = "_"))[, 5] - `F_(1_1)`(5, drug)) / `s_k_11`(5, drug),
    abs(get(paste("F_(0_1)_draws", drug, sep = "_"))[, 1] - `F_(0_1)`(1, drug)) / `s_k_01`(1, drug),
    abs(get(paste("F_(0_1)_draws", drug, sep = "_"))[, 2] - `F_(0_1)`(2, drug)) / `s_k_01`(2, drug),
    abs(get(paste("F_(0_1)_draws", drug, sep = "_"))[, 3] - `F_(0_1)`(3, drug)) / `s_k_01`(3, drug),
    abs(get(paste("F_(0_1)_draws", drug, sep = "_"))[, 4] - `F_(0_1)`(4, drug)) / `s_k_01`(4, drug),
    abs(get(paste("F_(0_1)_draws", drug, sep = "_"))[, 5] - `F_(0_1)`(5, drug)) / `s_k_01`(5, drug)
  )
  dist <- apply(mat, MARGIN = 1, max)
  quantile(dist, probs = p)
}

# Compute critical value: Counterfactual Effect (1_0) - (0_0)
"c_(0_1)-(0_0)" <- function(p, drug){
  mat <- cbind(
    abs(get(paste("F_(0_1)_draws", drug, sep = "_"))[, 1] - `F_(0_1)`(1, drug)) / `s_k_01`(1, drug),
    abs(get(paste("F_(0_1)_draws", drug, sep = "_"))[, 2] - `F_(0_1)`(2, drug)) / `s_k_01`(2, drug),
    abs(get(paste("F_(0_1)_draws", drug, sep = "_"))[, 3] - `F_(0_1)`(3, drug)) / `s_k_01`(3, drug),
    abs(get(paste("F_(0_1)_draws", drug, sep = "_"))[, 4] - `F_(0_1)`(4, drug)) / `s_k_01`(4, drug),
    abs(get(paste("F_(0_1)_draws", drug, sep = "_"))[, 5] - `F_(0_1)`(5, drug)) / `s_k_01`(5, drug),
    abs(get(paste("F_(0_0)_draws", drug, sep = "_"))[, 1] - `F_(0_0)`(1, drug)) / `s_k_00`(1, drug),
    abs(get(paste("F_(0_0)_draws", drug, sep = "_"))[, 2] - `F_(0_0)`(2, drug)) / `s_k_00`(2, drug),
    abs(get(paste("F_(0_0)_draws", drug, sep = "_"))[, 3] - `F_(0_0)`(3, drug)) / `s_k_00`(3, drug),
    abs(get(paste("F_(0_0)_draws", drug, sep = "_"))[, 4] - `F_(0_0)`(4, drug)) / `s_k_00`(4, drug),
    abs(get(paste("F_(0_0)_draws", drug, sep = "_"))[, 5] - `F_(0_0)`(5, drug)) / `s_k_00`(5, drug)
  )
  dist <- apply(mat, MARGIN = 1, max)
  quantile(dist, probs = p)
}


################ Step 4: Compute preliminary joint DF-bands ####################
# Cdf-band for level p: Overall effect (1_1) - (0_0)
"B_(1_1)_(0_0)" <- function(j, p, drug){
  as.numeric(c(`F_(1_1)`(j, drug) - `c_(1_1)-(0_0)`(p, drug) * `s_k_11`(j, drug), `F_(1_1)`(j, drug) + `c_(1_1)-(0_0)`(p, drug) * `s_k_11`(j, drug)))
}
"B_(0_0)_(1_1)" <- function(j, p, drug){
  as.numeric(c(`F_(0_0)`(j, drug) - `c_(1_1)-(0_0)`(p, drug) * `s_k_00`(j, drug), `F_(0_0)`(j, drug) + `c_(1_1)-(0_0)`(p, drug) * `s_k_00`(j, drug)))
}
"B_(1_1)-(0_0)" <- function(j, p, drug){
  `B_(1_1)_(0_0)`(j, p, drug) - c(`B_(0_0)_(1_1)`(j, p, drug)[2], `B_(0_0)_(1_1)`(j, p, drug)[1])
}

# Cdf-band for level p: Counterfactual effect (1_1) - (1_0)
"B_(1_1)_(1_0)" <- function(j, p, drug){
  as.numeric(c(`F_(1_1)`(j, drug) - `c_(1_1)-(1_0)`(p, drug) * `s_k_11`(j, drug), `F_(1_1)`(j, drug) + `c_(1_1)-(1_0)`(p, drug) * `s_k_11`(j, drug)))
}
"B_(1_0)_(1_1)" <- function(j, p, drug){
  as.numeric(c(`F_(1_0)`(j, drug) - `c_(1_1)-(1_0)`(p, drug) * `s_k_10`(j, drug), `F_(1_0)`(j, drug) + `c_(1_1)-(1_0)`(p, drug) * `s_k_10`(j, drug)))
}
"B_(1_1)-(1_0)" <- function(j, p, drug){
  `B_(1_1)_(1_0)`(j, p, drug) - c(`B_(1_0)_(1_1)`(j, p, drug)[2], `B_(1_0)_(1_1)`(j, p, drug)[1])
}

# Cdf-band for level p: Counterfactual effect (1_0) - (0_0)
"B_(1_0)_(0_0)" <- function(j, p, drug){
  as.numeric(c(`F_(1_0)`(j, drug) - `c_(1_0)-(0_0)`(p, drug) * `s_k_10`(j, drug), `F_(1_0)`(j, drug) + `c_(1_0)-(0_0)`(p, drug) * `s_k_10`(j, drug)))
}
"B_(0_0)_(1_0)" <- function(j, p, drug){
  as.numeric(c(`F_(0_0)`(j, drug) - `c_(1_0)-(0_0)`(p, drug) * `s_k_00`(j, drug), `F_(0_0)`(j, drug) + `c_(1_0)-(0_0)`(p, drug) * `s_k_00`(j, drug)))
}
"B_(1_0)-(0_0)" <- function(j, p, drug){
  `B_(1_0)_(0_0)`(j, p, drug) - c(`B_(0_0)_(1_0)`(j, p, drug)[2], `B_(0_0)_(1_0)`(j, p, drug)[1])
}

# Cdf-band for level p: Counterfactual effect (1_1) - (0_1)
"B_(1_1)_(0_1)" <- function(j, p, drug){
  as.numeric(c(`F_(1_1)`(j, drug) - `c_(1_1)-(0_1)`(p, drug) * `s_k_11`(j, drug), `F_(1_1)`(j, drug) + `c_(1_1)-(0_1)`(p, drug) * `s_k_11`(j, drug)))
}
"B_(0_1)_(1_1)" <- function(j, p, drug){
  as.numeric(c(`F_(0_1)`(j, drug) - `c_(1_1)-(0_1)`(p, drug) * `s_k_01`(j, drug), `F_(0_1)`(j, drug) + `c_(1_1)-(0_1)`(p, drug) * `s_k_01`(j, drug)))
}
"B_(1_1)-(0_1)" <- function(j, p, drug){
  `B_(1_1)_(0_1)`(j, p, drug) - c(`B_(0_1)_(1_1)`(j, p, drug)[2], `B_(0_1)_(1_1)`(j, p, drug)[1])
}

# Cdf-band for level p: Counterfactual effect (0_1) - (0_0)
"B_(0_1)_(0_0)" <- function(j, p, drug){
  as.numeric(c(`F_(0_1)`(j, drug) - `c_(0_1)-(0_0)`(p, drug) * `s_k_01`(j, drug), `F_(0_1)`(j, drug) + `c_(0_1)-(0_0)`(p, drug) * `s_k_01`(j, drug)))
}
"B_(0_0)_(0_1)" <- function(j, p, drug){
  as.numeric(c(`F_(0_0)`(j, drug) - `c_(0_1)-(0_0)`(p, drug) * `s_k_00`(j, drug), `F_(0_0)`(j, drug) + `c_(0_1)-(0_0)`(p, drug) * `s_k_00`(j, drug)))
}
"B_(0_1)-(0_0)" <- function(j, p, drug){
  `B_(0_1)_(0_0)`(j, p, drug) - c(`B_(0_0)_(0_1)`(j, p, drug)[2], `B_(0_0)_(0_1)`(j, p, drug)[1])
}

# Cdf-band for level p: Overall effect (1_1) - (0_0)
"B_(1_1)_(0_0)_restr" <- function(j, p, drug){
  as.numeric(c(max(0, `F_(1_1)`(j, drug) - `c_(1_1)-(0_0)`(p, drug) * `s_k_11`(j, drug)), min(1, `F_(1_1)`(j, drug) + `c_(1_1)-(0_0)`(p, drug) * `s_k_11`(j, drug))))
}
"B_(0_0)_(1_1)_restr" <- function(j, p, drug){
  as.numeric(c(max(0, `F_(0_0)`(j, drug) - `c_(1_1)-(0_0)`(p, drug) * `s_k_00`(j, drug)), min(1, `F_(0_0)`(j, drug) + `c_(1_1)-(0_0)`(p, drug) * `s_k_00`(j, drug))))
}
"B_(1_1)-(0_0)_restr" <- function(j, p, drug){
  `B_(1_1)_(0_0)_restr`(j, p, drug) - c(`B_(0_0)_(1_1)_restr`(j, p, drug)[2], `B_(0_0)_(1_1)_restr`(j, p, drug)[1])
}

# Cdf-band for level p: Counterfactual effect (1_1) - (1_0)
"B_(1_1)_(1_0)_restr" <- function(j, p, drug){
  as.numeric(c(max(0, `F_(1_1)`(j, drug) - `c_(1_1)-(1_0)`(p, drug) * `s_k_11`(j, drug)), min(1, `F_(1_1)`(j, drug) + `c_(1_1)-(1_0)`(p, drug) * `s_k_11`(j, drug))))
}
"B_(1_0)_(1_1)_restr" <- function(j, p, drug){
  as.numeric(c(max(0, `F_(1_0)`(j, drug) - `c_(1_1)-(1_0)`(p, drug) * `s_k_10`(j, drug)), min(1, `F_(1_0)`(j, drug) + `c_(1_1)-(1_0)`(p, drug) * `s_k_10`(j, drug))))
}
"B_(1_1)-(1_0)_restr" <- function(j, p, drug){
  `B_(1_1)_(1_0)_restr`(j, p, drug) - c(`B_(1_0)_(1_1)_restr`(j, p, drug)[2], `B_(1_0)_(1_1)_restr`(j, p, drug)[1])
}

# Cdf-band for level p: Counterfactual effect (1_0) - (0_0)
"B_(1_0)_(0_0)_restr" <- function(j, p, drug){
  as.numeric(c(max(0, `F_(1_0)`(j, drug) - `c_(1_0)-(0_0)`(p, drug) * `s_k_10`(j, drug)), min(1, `F_(1_0)`(j, drug) + `c_(1_0)-(0_0)`(p, drug) * `s_k_10`(j, drug))))
}
"B_(0_0)_(1_0)_restr" <- function(j, p, drug){
  as.numeric(c(max(0, `F_(0_0)`(j, drug) - `c_(1_0)-(0_0)`(p, drug) * `s_k_00`(j, drug)), min(1, `F_(0_0)`(j, drug) + `c_(1_0)-(0_0)`(p, drug) * `s_k_00`(j, drug))))
}
"B_(1_0)-(0_0)_restr" <- function(j, p, drug){
  `B_(1_0)_(0_0)_restr`(j, p, drug) - c(`B_(0_0)_(1_0)_restr`(j, p, drug)[2], `B_(0_0)_(1_0)_restr`(j, p, drug)[1])
}

# Cdf-band for level p: Counterfactual effect (1_1) - (0_1)
"B_(1_1)_(0_1)_restr" <- function(j, p, drug){
  as.numeric(c(max(0, `F_(1_1)`(j, drug) - `c_(1_1)-(0_1)`(p, drug) * `s_k_11`(j, drug)), min(1, `F_(1_1)`(j, drug) + `c_(1_1)-(0_1)`(p, drug) * `s_k_11`(j, drug))))
}
"B_(0_1)_(1_1)_restr" <- function(j, p, drug){
  as.numeric(c(max(0, `F_(0_1)`(j, drug) - `c_(1_1)-(0_1)`(p, drug) * `s_k_01`(j, drug)), min(1, `F_(0_1)`(j, drug) + `c_(1_1)-(0_1)`(p, drug) * `s_k_01`(j, drug))))
}
"B_(1_1)-(0_1)_restr" <- function(j, p, drug){
  `B_(1_1)_(0_1)_restr`(j, p, drug) - c(`B_(0_1)_(1_1)_restr`(j, p, drug)[2], `B_(0_1)_(1_1)_restr`(j, p, drug)[1])
}

# Cdf-band for level p: Counterfactual effect (0_1) - (0_0)
"B_(0_1)_(0_0)_restr" <- function(j, p, drug){
  as.numeric(c(max(0, `F_(0_1)`(j, drug) - `c_(0_1)-(0_0)`(p, drug) * `s_k_01`(j, drug)), min(1, `F_(0_1)`(j, drug) + `c_(0_1)-(0_0)`(p, drug) * `s_k_01`(j, drug))))
}
"B_(0_0)_(0_1)_restr" <- function(j, p, drug){
  as.numeric(c(max(0, `F_(0_0)`(j, drug) - `c_(0_1)-(0_0)`(p, drug) * `s_k_00`(j, drug)), min(1, `F_(0_0)`(j, drug) + `c_(0_1)-(0_0)`(p, drug) * `s_k_00`(j, drug))))
}
"B_(0_1)-(0_0)_restr" <- function(j, p, drug){
  `B_(0_1)_(0_0)_restr`(j, p, drug) - c(`B_(0_0)_(0_1)_restr`(j, p, drug)[2], `B_(0_0)_(0_1)_restr`(j, p, drug)[1])
}

# Functions for survival functions (and not their differences)
# Compute critical value: (0_0)
"c_(0_0)" <- function(p, drug){
  mat <- cbind(
    abs(get(paste("F_(0_0)_draws", drug, sep = "_"))[, 1] - `F_(0_0)`(1, drug)) / `s_k_00`(1, drug),
    abs(get(paste("F_(0_0)_draws", drug, sep = "_"))[, 2] - `F_(0_0)`(2, drug)) / `s_k_00`(2, drug),
    abs(get(paste("F_(0_0)_draws", drug, sep = "_"))[, 3] - `F_(0_0)`(3, drug)) / `s_k_00`(3, drug),
    abs(get(paste("F_(0_0)_draws", drug, sep = "_"))[, 4] - `F_(0_0)`(4, drug)) / `s_k_00`(4, drug),
    abs(get(paste("F_(0_0)_draws", drug, sep = "_"))[, 5] - `F_(0_0)`(5, drug)) / `s_k_00`(5, drug)
  )
  dist <- apply(mat, MARGIN = 1, max)
  quantile(dist, probs = p)
}
# Compute critical value: (1_0)
"c_(1_0)" <- function(p, drug){
  mat <- cbind(
    abs(get(paste("F_(1_0)_draws", drug, sep = "_"))[, 1] - `F_(1_0)`(1, drug)) / `s_k_10`(1, drug),
    abs(get(paste("F_(1_0)_draws", drug, sep = "_"))[, 2] - `F_(1_0)`(2, drug)) / `s_k_10`(2, drug),
    abs(get(paste("F_(1_0)_draws", drug, sep = "_"))[, 3] - `F_(1_0)`(3, drug)) / `s_k_10`(3, drug),
    abs(get(paste("F_(1_0)_draws", drug, sep = "_"))[, 4] - `F_(1_0)`(4, drug)) / `s_k_10`(4, drug),
    abs(get(paste("F_(1_0)_draws", drug, sep = "_"))[, 5] - `F_(1_0)`(5, drug)) / `s_k_10`(5, drug)
  )
  dist <- apply(mat, MARGIN = 1, max)
  quantile(dist, probs = p)
}
# Compute critical value: (0_1)
"c_(0_1)" <- function(p, drug){
  mat <- cbind(
    abs(get(paste("F_(0_1)_draws", drug, sep = "_"))[, 1] - `F_(0_1)`(1, drug)) / `s_k_01`(1, drug),
    abs(get(paste("F_(0_1)_draws", drug, sep = "_"))[, 2] - `F_(0_1)`(2, drug)) / `s_k_01`(2, drug),
    abs(get(paste("F_(0_1)_draws", drug, sep = "_"))[, 3] - `F_(0_1)`(3, drug)) / `s_k_01`(3, drug),
    abs(get(paste("F_(0_1)_draws", drug, sep = "_"))[, 4] - `F_(0_1)`(4, drug)) / `s_k_01`(4, drug),
    abs(get(paste("F_(0_1)_draws", drug, sep = "_"))[, 5] - `F_(0_1)`(5, drug)) / `s_k_01`(5, drug)
  )
  dist <- apply(mat, MARGIN = 1, max)
  quantile(dist, probs = p)
}
# Compute critical value: (1_1)
"c_(1_1)" <- function(p, drug){
  mat <- cbind(
    abs(get(paste("F_(1_1)_draws", drug, sep = "_"))[, 1] - `F_(1_1)`(1, drug)) / `s_k_11`(1, drug),
    abs(get(paste("F_(1_1)_draws", drug, sep = "_"))[, 2] - `F_(1_1)`(2, drug)) / `s_k_11`(2, drug),
    abs(get(paste("F_(1_1)_draws", drug, sep = "_"))[, 3] - `F_(1_1)`(3, drug)) / `s_k_11`(3, drug),
    abs(get(paste("F_(1_1)_draws", drug, sep = "_"))[, 4] - `F_(1_1)`(4, drug)) / `s_k_11`(4, drug),
    abs(get(paste("F_(1_1)_draws", drug, sep = "_"))[, 5] - `F_(1_1)`(5, drug)) / `s_k_11`(5, drug)
  )
  dist <- apply(mat, MARGIN = 1, max)
  quantile(dist, probs = p)
}

"B_(0_0)" <- function(j, p, drug){
  as.numeric(c(`F_(0_0)`(j, drug) - `c_(0_0)`(p, drug) * `s_k_00`(j, drug), `F_(0_0)`(j, drug) + `c_(0_0)`(p, drug) * `s_k_00`(j, drug)))
}
"B_(1_0)" <- function(j, p, drug){
  as.numeric(c(`F_(1_0)`(j, drug) - `c_(1_0)`(p, drug) * `s_k_10`(j, drug), `F_(1_0)`(j, drug) + `c_(1_0)`(p, drug) * `s_k_10`(j, drug)))
}
"B_(0_1)" <- function(j, p, drug){
  as.numeric(c(`F_(0_1)`(j, drug) - `c_(0_1)`(p, drug) * `s_k_01`(j, drug), `F_(0_1)`(j, drug) + `c_(0_1)`(p, drug) * `s_k_01`(j, drug)))
}
"B_(1_1)" <- function(j, p, drug){
  as.numeric(c(`F_(1_1)`(j, drug) - `c_(1_1)`(p, drug) * `s_k_11`(j, drug), `F_(1_1)`(j, drug) + `c_(1_1)`(p, drug) * `s_k_11`(j, drug)))
}
"B_(0_0)_restr" <- function(j, p, drug){
  as.numeric(c(max(0, `F_(0_0)`(j, drug) - `c_(0_0)`(p, drug) * `s_k_00`(j, drug)), min(1, `F_(0_0)`(j, drug) + `c_(0_0)`(p, drug) * `s_k_00`(j, drug))))
}
"B_(1_0)_restr" <- function(j, p, drug){
  as.numeric(c(max(0, `F_(1_0)`(j, drug) - `c_(1_0)`(p, drug) * `s_k_10`(j, drug)), min(1, `F_(1_0)`(j, drug) + `c_(1_0)`(p, drug) * `s_k_10`(j, drug))))
}
"B_(0_1)_restr" <- function(j, p, drug){
  as.numeric(c(max(0, `F_(0_1)`(j, drug) - `c_(0_1)`(p, drug) * `s_k_01`(j, drug)), min(1, `F_(0_1)`(j, drug) + `c_(0_1)`(p, drug) * `s_k_01`(j, drug))))
}
"B_(1_1)_restr" <- function(j, p, drug){
  as.numeric(c(max(0, `F_(1_1)`(j, drug) - `c_(1_1)`(p, drug) * `s_k_11`(j, drug)), min(1, `F_(1_1)`(j, drug) + `c_(1_1)`(p, drug) * `s_k_11`(j, drug))))
}

