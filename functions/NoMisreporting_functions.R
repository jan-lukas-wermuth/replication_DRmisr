# Title:      Function Collection for the Script NoMisreporting.R
# Author:     Jan-Lukas Wermuth
# Date:       2025-05-06
# Purpose:    This script contains all the function definitions
#             that are needed to construct the estimated survival
#             probabilities for Y without allowing for misreporting.
#             It also contains the function definitions necessary to
#             compute the simultaneous confidence bands.

# Upper tail probability functions together with counterfactual upper tail probability functions
# j: Input of the survival function
"F_(0|0)" <- function(j, drug){
  X_mat <- as.matrix(cbind(1, select(get(paste("X_norecreational", drug, sep = "_")),-c("questions"))))
  Z_mat <- as.matrix(cbind(1, select(get(paste("X_norecreational", drug, sep = "_")),-c("mar_prices_medium"))))
  x_length <- ncol(X_mat)
  z_length <- ncol(Z_mat)
  coefficients <- get(paste(drug, "DR_norecreational", j, sep = "_"))$coefficients
  mean(pnorm(cbind(X_mat %*% coefficients)))
}
"F_(1|0)" <- function(j, drug){
  X_mat <- as.matrix(cbind(1, select(get(paste("X_norecreational", drug, sep = "_")),-c("questions"))))
  Z_mat <- as.matrix(cbind(1, select(get(paste("X_norecreational", drug, sep = "_")),-c("mar_prices_medium"))))
  x_length <- ncol(X_mat)
  z_length <- ncol(Z_mat)
  coefficients <- get(paste(drug, "DR_recreational", j, sep = "_"))$coefficients
  mean(pnorm(cbind(X_mat %*% coefficients[1:x_length])))
}
"F_(0|1)" <- function(j, drug){
  X_mat <- as.matrix(cbind(1, select(get(paste("X_recreational", drug, sep = "_")),-c("questions"))))
  Z_mat <- as.matrix(cbind(1, select(get(paste("X_recreational", drug, sep = "_")),-c("mar_prices_medium"))))
  x_length <- ncol(X_mat)
  z_length <- ncol(Z_mat)
  coefficients <- get(paste(drug, "DR_norecreational", j, sep = "_"))$coefficients
  mean(pnorm(cbind(X_mat %*% coefficients[1:x_length])))
}
"F_(1|1)" <- function(j, drug){
  X_mat <- as.matrix(cbind(1, select(get(paste("X_recreational", drug, sep = "_")),-c("questions"))))
  Z_mat <- as.matrix(cbind(1, select(get(paste("X_recreational", drug, sep = "_")),-c("mar_prices_medium"))))
  x_length <- ncol(X_mat)
  z_length <- ncol(Z_mat)
  coefficients <- get(paste(drug, "DR_recreational", j, sep = "_"))$coefficients
  mean(pnorm(cbind(X_mat %*% coefficients[1:x_length])))
}

# Bootstrap: Marijuana -----------------------------------------------------------
###### "F_(0|0)" bootstrapped
# Function which allows to compute one bootstrapped function
# j: Input of the survival function
# B: Bootstrap draws
"F_(0|0)_b" <- function(j, B, drug){
  X_mat <- as.matrix(cbind(1, select(get(paste("X_norecreational", drug, sep = "_")),-c("questions"))))
  Z_mat <- as.matrix(cbind(1, select(get(paste("X_norecreational", drug, sep = "_")),-c("mar_prices_medium"))))
  n <- nrow(X_mat)
  data <- get(paste("yrbs_data_combined_20172019_norecreational", drug, sep = "_"))
  
  # Start cluster for parallel computing
  cl <- makeCluster(detectCores() - 1, type = "PSOCK")
  registerDoParallel(cl)
  res <- foreach(i = 1:B, .combine = 'c', .export = paste("yrbs_data_combined_20172019_norecreational", drug, sep = "_")) %dopar% {
    set.seed(i)
    mult_weights_covariates <- sample(x = 1:n, size = n, replace = TRUE)
    X_mat <- X_mat[mult_weights_covariates,]
    Z_mat <- Z_mat[mult_weights_covariates,]
    x_length <- ncol(X_mat)
    z_length <- ncol(Z_mat)
    formula <- paste(paste(drug, j, sep = ""), "~", "year + median_income + sex + unemployment_rate + mar_prices_medium")
    DR_res <- glm(formula = formula, family = binomial(link = "probit"), data = data, weights = rmultinom(1, size = nrow(data), prob = rep(1/nrow(data), nrow(data))))
    coefficients <- DR_res$coefficients
    mean(pnorm(cbind(X_mat %*% coefficients)))
}
  stopCluster(cl)
  return(res)
}

###### "F_(1|0)" bootstrapped
# Function which allows to compute one bootstrapped function
# j: Input of the survival function
# B: Bootstrap draws
"F_(1|0)_b" <- function(j, B, drug){ 
  X_mat <- as.matrix(cbind(1, select(get(paste("X_norecreational", drug, sep = "_")),-c("questions"))))
  Z_mat <- as.matrix(cbind(1, select(get(paste("X_norecreational", drug, sep = "_")),-c("mar_prices_medium"))))
  n <- nrow(X_mat)
  data <- get(paste("yrbs_data_combined_20172019_recreational", drug, sep = "_"))
  
  # Start cluster for parallel computing
  cl <- makeCluster(detectCores() - 1, type = "PSOCK") 
  registerDoParallel(cl)
  res <- foreach(i = 1:B, .combine = 'c', .export = paste("yrbs_data_combined_20172019_recreational", drug, sep = "_")) %dopar% {
    set.seed(i)
    mult_weights_covariates <- sample(x = 1:n, size = n, replace = TRUE)
    X_mat <- X_mat[mult_weights_covariates,]
    Z_mat <- Z_mat[mult_weights_covariates,]
    x_length <- ncol(X_mat)
    z_length <- ncol(Z_mat)
    formula <- paste(paste(drug, j, sep = ""), "~", "year + median_income + sex + unemployment_rate + mar_prices_medium")
    DR_res <- glm(formula = formula, family = binomial(link = "probit"), data = data, weights = rmultinom(1, size = nrow(data), prob = rep(1/nrow(data), nrow(data))))
    coefficients <- DR_res$coefficients
    mean(pnorm(cbind(X_mat %*% coefficients)))
  }
  stopCluster(cl)
  return(res)
}
###### "F_(0|1)" bootstrapped
# Function which allows to compute one bootstrapped function
# j: Input of the survival function
# B: Bootstrap draws
"F_(0|1)_b" <- function(j, B, drug){
  X_mat <- as.matrix(cbind(1, select(get(paste("X_recreational", drug, sep = "_")),-c("questions"))))
  Z_mat <- as.matrix(cbind(1, select(get(paste("X_recreational", drug, sep = "_")),-c("mar_prices_medium"))))
  n <- nrow(X_mat)
  data <- get(paste("yrbs_data_combined_20172019_norecreational", drug, sep = "_"))
  
  # Start cluster for parallel computing
  cl <- makeCluster(detectCores() - 1, type = "PSOCK") 
  registerDoParallel(cl)
  res <- foreach(i = 1:B, .combine = 'c', .export = paste("yrbs_data_combined_20172019_norecreational", drug, sep = "_")) %dopar% {
    set.seed(i)
    mult_weights_covariates <- sample(x = 1:n, size = n, replace = TRUE)
    X_mat <- X_mat[mult_weights_covariates,]
    Z_mat <- Z_mat[mult_weights_covariates,]
    x_length <- ncol(X_mat)
    z_length <- ncol(Z_mat)
    formula <- paste(paste(drug, j, sep = ""), "~", "year + median_income + sex + unemployment_rate + mar_prices_medium")
    DR_res <- glm(formula = formula, family = binomial(link = "probit"), data = data, weights = rmultinom(1, size = nrow(data), prob = rep(1/nrow(data), nrow(data))))
    coefficients <- DR_res$coefficients
    mean(pnorm(cbind(X_mat %*% coefficients)))
  }
  stopCluster(cl)
  return(res)
}


###### "F_(1|1)" bootstrapped
# Function which allows to compute one bootstrapped function
# j: Input of the survival function
# B: Bootstrap draws
"F_(1|1)_b" <- function(j, B, drug){
  X_mat <- as.matrix(cbind(1, select(get(paste("X_recreational", drug, sep = "_")),-c("questions"))))
  Z_mat <- as.matrix(cbind(1, select(get(paste("X_recreational", drug, sep = "_")),-c("mar_prices_medium"))))
  n <- nrow(X_mat)
  data <- get(paste("yrbs_data_combined_20172019_recreational", drug, sep = "_"))
  
  # Start cluster for parallel computing
  cl <- makeCluster(detectCores() - 1, type = "PSOCK") 
  registerDoParallel(cl)
  res <- foreach(i = 1:B, .combine = 'c', .export = paste("yrbs_data_combined_20172019_recreational", drug, sep = "_")) %dopar% {
    set.seed(i)
    mult_weights_covariates <- sample(x = 1:n, size = n, replace = TRUE)
    X_mat <- X_mat[mult_weights_covariates,]
    Z_mat <- Z_mat[mult_weights_covariates,]
    x_length <- ncol(X_mat)
    z_length <- ncol(Z_mat)
    formula <- paste(paste(drug, j, sep = ""), "~", "year + median_income + sex + unemployment_rate + mar_prices_medium")
    DR_res <- glm(formula = formula, family = binomial(link = "probit"), data = data, weights = rmultinom(1, size = nrow(data), prob = rep(1/nrow(data), nrow(data))))
    coefficients <- DR_res$coefficients
    mean(pnorm(cbind(X_mat %*% coefficients)))
  }
  stopCluster(cl)
  return(res)
}

################ Step 2: Compute robust standard errors ####################
# Insert j %in% 1:5
# "F_(0|0)"
"s_k_00" <- function(j, drug){
  (quantile(get(paste("F_(0|0)_draws", drug, sep = "_"))[,j], probs = 0.75) - quantile(get(paste("F_(0|0)_draws", drug, sep = "_"))[,j], probs = 0.25)) / (qnorm(0.75) - qnorm(0.25))
}
# "F_(1|0)"
"s_k_10" <- function(j, drug){
  (quantile(get(paste("F_(1|0)_draws", drug, sep = "_"))[,j], probs = 0.75) - quantile(get(paste("F_(1|0)_draws", drug, sep = "_"))[,j], probs = 0.25)) / (qnorm(0.75) - qnorm(0.25))
}
# "F_(0|1)"
"s_k_01" <- function(j, drug){
  (quantile(get(paste("F_(0|1)_draws", drug, sep = "_"))[,j], probs = 0.75) - quantile(get(paste("F_(0|1)_draws", drug, sep = "_"))[,j], probs = 0.25)) / (qnorm(0.75) - qnorm(0.25))
}
# "F_(1|1)"
"s_k_11" <- function(j, drug){
  (quantile(get(paste("F_(1|1)_draws", drug, sep = "_"))[,j], probs = 0.75) - quantile(get(paste("F_(1|1)_draws", drug, sep = "_"))[,j], probs = 0.25)) / (qnorm(0.75) - qnorm(0.25))
}

################ Step 3: Compute critical value ####################
# Compute critical value: Overall Effect (1|1) - (0|0)
"c_(1|1)-(0|0)" <- function(p, drug){
  mat <- cbind(
    abs(get(paste("F_(1|1)_draws", drug, sep = "_"))[, 1] - `F_(1|1)`(1, drug)) / `s_k_11`(1, drug),
    abs(get(paste("F_(1|1)_draws", drug, sep = "_"))[, 2] - `F_(1|1)`(2, drug)) / `s_k_11`(2, drug),
    abs(get(paste("F_(1|1)_draws", drug, sep = "_"))[, 3] - `F_(1|1)`(3, drug)) / `s_k_11`(3, drug),
    abs(get(paste("F_(1|1)_draws", drug, sep = "_"))[, 4] - `F_(1|1)`(4, drug)) / `s_k_11`(4, drug),
    abs(get(paste("F_(1|1)_draws", drug, sep = "_"))[, 5] - `F_(1|1)`(5, drug)) / `s_k_11`(5, drug),
    abs(get(paste("F_(0|0)_draws", drug, sep = "_"))[, 1] - `F_(0|0)`(1, drug)) / `s_k_00`(1, drug),
    abs(get(paste("F_(0|0)_draws", drug, sep = "_"))[, 2] - `F_(0|0)`(2, drug)) / `s_k_00`(2, drug),
    abs(get(paste("F_(0|0)_draws", drug, sep = "_"))[, 3] - `F_(0|0)`(3, drug)) / `s_k_00`(3, drug),
    abs(get(paste("F_(0|0)_draws", drug, sep = "_"))[, 4] - `F_(0|0)`(4, drug)) / `s_k_00`(4, drug),
    abs(get(paste("F_(0|0)_draws", drug, sep = "_"))[, 5] - `F_(0|0)`(5, drug)) / `s_k_00`(5, drug)
  )
  dist <- apply(mat, MARGIN = 1, max)
  quantile(dist, probs = p)
}

# Compute critical value: Counterfactual Effect (1|1) - (1|0)
"c_(1|1)-(1|0)" <- function(p, drug){
  mat <- cbind(
    abs(get(paste("F_(1|1)_draws", drug, sep = "_"))[, 1] - `F_(1|1)`(1, drug)) / `s_k_11`(1, drug),
    abs(get(paste("F_(1|1)_draws", drug, sep = "_"))[, 2] - `F_(1|1)`(2, drug)) / `s_k_11`(2, drug),
    abs(get(paste("F_(1|1)_draws", drug, sep = "_"))[, 3] - `F_(1|1)`(3, drug)) / `s_k_11`(3, drug),
    abs(get(paste("F_(1|1)_draws", drug, sep = "_"))[, 4] - `F_(1|1)`(4, drug)) / `s_k_11`(4, drug),
    abs(get(paste("F_(1|1)_draws", drug, sep = "_"))[, 5] - `F_(1|1)`(5, drug)) / `s_k_11`(5, drug),
    abs(get(paste("F_(1|0)_draws", drug, sep = "_"))[, 1] - `F_(1|0)`(1, drug)) / `s_k_10`(1, drug),
    abs(get(paste("F_(1|0)_draws", drug, sep = "_"))[, 2] - `F_(1|0)`(2, drug)) / `s_k_10`(2, drug),
    abs(get(paste("F_(1|0)_draws", drug, sep = "_"))[, 3] - `F_(1|0)`(3, drug)) / `s_k_10`(3, drug),
    abs(get(paste("F_(1|0)_draws", drug, sep = "_"))[, 4] - `F_(1|0)`(4, drug)) / `s_k_10`(4, drug),
    abs(get(paste("F_(1|0)_draws", drug, sep = "_"))[, 5] - `F_(1|0)`(5, drug)) / `s_k_10`(5, drug)
  )
  dist <- apply(mat, MARGIN = 1, max)
  quantile(dist, probs = p)
}

# Compute critical value: Counterfactual Effect (1|0) - (0|0)
"c_(1|0)-(0|0)" <- function(p, drug){
  mat <- cbind(
    abs(get(paste("F_(1|0)_draws", drug, sep = "_"))[, 1] - `F_(1|0)`(1, drug)) / `s_k_10`(1, drug),
    abs(get(paste("F_(1|0)_draws", drug, sep = "_"))[, 2] - `F_(1|0)`(2, drug)) / `s_k_10`(2, drug),
    abs(get(paste("F_(1|0)_draws", drug, sep = "_"))[, 3] - `F_(1|0)`(3, drug)) / `s_k_10`(3, drug),
    abs(get(paste("F_(1|0)_draws", drug, sep = "_"))[, 4] - `F_(1|0)`(4, drug)) / `s_k_10`(4, drug),
    abs(get(paste("F_(1|0)_draws", drug, sep = "_"))[, 5] - `F_(1|0)`(5, drug)) / `s_k_10`(5, drug),
    abs(get(paste("F_(0|0)_draws", drug, sep = "_"))[, 1] - `F_(0|0)`(1, drug)) / `s_k_00`(1, drug),
    abs(get(paste("F_(0|0)_draws", drug, sep = "_"))[, 2] - `F_(0|0)`(2, drug)) / `s_k_00`(2, drug),
    abs(get(paste("F_(0|0)_draws", drug, sep = "_"))[, 3] - `F_(0|0)`(3, drug)) / `s_k_00`(3, drug),
    abs(get(paste("F_(0|0)_draws", drug, sep = "_"))[, 4] - `F_(0|0)`(4, drug)) / `s_k_00`(4, drug),
    abs(get(paste("F_(0|0)_draws", drug, sep = "_"))[, 5] - `F_(0|0)`(5, drug)) / `s_k_00`(5, drug)
  )
  dist <- apply(mat, MARGIN = 1, max)
  quantile(dist, probs = p)
}

# Compute critical value: Counterfactual Effect (1|1) - (0|1)
"c_(1|1)-(0|1)" <- function(p, drug){
  mat <- cbind(
    abs(get(paste("F_(1|1)_draws", drug, sep = "_"))[, 1] - `F_(1|1)`(1, drug)) / `s_k_11`(1, drug),
    abs(get(paste("F_(1|1)_draws", drug, sep = "_"))[, 2] - `F_(1|1)`(2, drug)) / `s_k_11`(2, drug),
    abs(get(paste("F_(1|1)_draws", drug, sep = "_"))[, 3] - `F_(1|1)`(3, drug)) / `s_k_11`(3, drug),
    abs(get(paste("F_(1|1)_draws", drug, sep = "_"))[, 4] - `F_(1|1)`(4, drug)) / `s_k_11`(4, drug),
    abs(get(paste("F_(1|1)_draws", drug, sep = "_"))[, 5] - `F_(1|1)`(5, drug)) / `s_k_11`(5, drug),
    abs(get(paste("F_(0|1)_draws", drug, sep = "_"))[, 1] - `F_(0|1)`(1, drug)) / `s_k_01`(1, drug),
    abs(get(paste("F_(0|1)_draws", drug, sep = "_"))[, 2] - `F_(0|1)`(2, drug)) / `s_k_01`(2, drug),
    abs(get(paste("F_(0|1)_draws", drug, sep = "_"))[, 3] - `F_(0|1)`(3, drug)) / `s_k_01`(3, drug),
    abs(get(paste("F_(0|1)_draws", drug, sep = "_"))[, 4] - `F_(0|1)`(4, drug)) / `s_k_01`(4, drug),
    abs(get(paste("F_(0|1)_draws", drug, sep = "_"))[, 5] - `F_(0|1)`(5, drug)) / `s_k_01`(5, drug)
  )
  dist <- apply(mat, MARGIN = 1, max)
  quantile(dist, probs = p)
}

# Compute critical value: Counterfactual Effect (0|1) - (0|0)
"c_(0|1)-(0|0)" <- function(p, drug){
  mat <- cbind(
    abs(get(paste("F_(0|1)_draws", drug, sep = "_"))[, 1] - `F_(0|1)`(1, drug)) / `s_k_01`(1, drug),
    abs(get(paste("F_(0|1)_draws", drug, sep = "_"))[, 2] - `F_(0|1)`(2, drug)) / `s_k_01`(2, drug),
    abs(get(paste("F_(0|1)_draws", drug, sep = "_"))[, 3] - `F_(0|1)`(3, drug)) / `s_k_01`(3, drug),
    abs(get(paste("F_(0|1)_draws", drug, sep = "_"))[, 4] - `F_(0|1)`(4, drug)) / `s_k_01`(4, drug),
    abs(get(paste("F_(0|1)_draws", drug, sep = "_"))[, 5] - `F_(0|1)`(5, drug)) / `s_k_01`(5, drug),
    abs(get(paste("F_(0|0)_draws", drug, sep = "_"))[, 1] - `F_(0|0)`(1, drug)) / `s_k_00`(1, drug),
    abs(get(paste("F_(0|0)_draws", drug, sep = "_"))[, 2] - `F_(0|0)`(2, drug)) / `s_k_00`(2, drug),
    abs(get(paste("F_(0|0)_draws", drug, sep = "_"))[, 3] - `F_(0|0)`(3, drug)) / `s_k_00`(3, drug),
    abs(get(paste("F_(0|0)_draws", drug, sep = "_"))[, 4] - `F_(0|0)`(4, drug)) / `s_k_00`(4, drug),
    abs(get(paste("F_(0|0)_draws", drug, sep = "_"))[, 5] - `F_(0|0)`(5, drug)) / `s_k_00`(5, drug)
  )
  dist <- apply(mat, MARGIN = 1, max)
  quantile(dist, probs = p)
}


################ Step 4: Compute preliminary joint DF-bands ####################
# Cdf-band for level p: Overall effect (1|1) - (0|0)
"B_(1|1)_(0|0)" <- function(j, p, drug){
  as.numeric(c(`F_(1|1)`(j, drug) - `c_(1|1)-(0|0)`(p, drug) * `s_k_11`(j, drug), `F_(1|1)`(j, drug) + `c_(1|1)-(0|0)`(p, drug) * `s_k_11`(j, drug)))
}
"B_(0|0)_(1|1)" <- function(j, p, drug){
  as.numeric(c(`F_(0|0)`(j, drug) - `c_(1|1)-(0|0)`(p, drug) * `s_k_00`(j, drug), `F_(0|0)`(j, drug) + `c_(1|1)-(0|0)`(p, drug) * `s_k_00`(j, drug)))
}
"B_(1|1)-(0|0)" <- function(j, p, drug){
  `B_(1|1)_(0|0)`(j, p, drug) - c(`B_(0|0)_(1|1)`(j, p, drug)[2], `B_(0|0)_(1|1)`(j, p, drug)[1])
}

# Cdf-band for level p: Counterfactual effect (1|1) - (1|0)
"B_(1|1)_(1|0)" <- function(j, p, drug){
  as.numeric(c(`F_(1|1)`(j, drug) - `c_(1|1)-(1|0)`(p, drug) * `s_k_11`(j, drug), `F_(1|1)`(j, drug) + `c_(1|1)-(1|0)`(p, drug) * `s_k_11`(j, drug)))
}
"B_(1|0)_(1|1)" <- function(j, p, drug){
  as.numeric(c(`F_(1|0)`(j, drug) - `c_(1|1)-(1|0)`(p, drug) * `s_k_10`(j, drug), `F_(1|0)`(j, drug) + `c_(1|1)-(1|0)`(p, drug) * `s_k_10`(j, drug)))
}
"B_(1|1)-(1|0)" <- function(j, p, drug){
  `B_(1|1)_(1|0)`(j, p, drug) - c(`B_(1|0)_(1|1)`(j, p, drug)[2], `B_(1|0)_(1|1)`(j, p, drug)[1])
}

# Cdf-band for level p: Counterfactual effect (1|0) - (0|0)
"B_(1|0)_(0|0)" <- function(j, p, drug){
  as.numeric(c(`F_(1|0)`(j, drug) - `c_(1|0)-(0|0)`(p, drug) * `s_k_10`(j, drug), `F_(1|0)`(j, drug) + `c_(1|0)-(0|0)`(p, drug) * `s_k_10`(j, drug)))
}
"B_(0|0)_(1|0)" <- function(j, p, drug){
  as.numeric(c(`F_(0|0)`(j, drug) - `c_(1|0)-(0|0)`(p, drug) * `s_k_00`(j, drug), `F_(0|0)`(j, drug) + `c_(1|0)-(0|0)`(p, drug) * `s_k_00`(j, drug)))
}
"B_(1|0)-(0|0)" <- function(j, p, drug){
  `B_(1|0)_(0|0)`(j, p, drug) - c(`B_(0|0)_(1|0)`(j, p, drug)[2], `B_(0|0)_(1|0)`(j, p, drug)[1])
}

# Cdf-band for level p: Counterfactual effect (1|1) - (0|1)
"B_(1|1)_(0|1)" <- function(j, p, drug){
  as.numeric(c(`F_(1|1)`(j, drug) - `c_(1|1)-(0|1)`(p, drug) * `s_k_11`(j, drug), `F_(1|1)`(j, drug) + `c_(1|1)-(0|1)`(p, drug) * `s_k_11`(j, drug)))
}
"B_(0|1)_(1|1)" <- function(j, p, drug){
  as.numeric(c(`F_(0|1)`(j, drug) - `c_(1|1)-(0|1)`(p, drug) * `s_k_01`(j, drug), `F_(0|1)`(j, drug) + `c_(1|1)-(0|1)`(p, drug) * `s_k_01`(j, drug)))
}
"B_(1|1)-(0|1)" <- function(j, p, drug){
  `B_(1|1)_(0|1)`(j, p, drug) - c(`B_(0|1)_(1|1)`(j, p, drug)[2], `B_(0|1)_(1|1)`(j, p, drug)[1])
}

# Cdf-band for level p: Counterfactual effect (0|1) - (0|0)
"B_(0|1)_(0|0)" <- function(j, p, drug){
  as.numeric(c(`F_(0|1)`(j, drug) - `c_(0|1)-(0|0)`(p, drug) * `s_k_01`(j, drug), `F_(0|1)`(j, drug) + `c_(0|1)-(0|0)`(p, drug) * `s_k_01`(j, drug)))
}
"B_(0|0)_(0|1)" <- function(j, p, drug){
  as.numeric(c(`F_(0|0)`(j, drug) - `c_(0|1)-(0|0)`(p, drug) * `s_k_00`(j, drug), `F_(0|0)`(j, drug) + `c_(0|1)-(0|0)`(p, drug) * `s_k_00`(j, drug)))
}
"B_(0|1)-(0|0)" <- function(j, p, drug){
  `B_(0|1)_(0|0)`(j, p, drug) - c(`B_(0|0)_(0|1)`(j, p, drug)[2], `B_(0|0)_(0|1)`(j, p, drug)[1])
}

# Cdf-band for level p: Overall effect (1|1) - (0|0)
"B_(1|1)_(0|0)_restr" <- function(j, p, drug){
  as.numeric(c(max(0, `F_(1|1)`(j, drug) - `c_(1|1)-(0|0)`(p, drug) * `s_k_11`(j, drug)), min(1, `F_(1|1)`(j, drug) + `c_(1|1)-(0|0)`(p, drug) * `s_k_11`(j, drug))))
}
"B_(0|0)_(1|1)_restr" <- function(j, p, drug){
  as.numeric(c(max(0, `F_(0|0)`(j, drug) - `c_(1|1)-(0|0)`(p, drug) * `s_k_00`(j, drug)), min(1, `F_(0|0)`(j, drug) + `c_(1|1)-(0|0)`(p, drug) * `s_k_00`(j, drug))))
}
"B_(1|1)-(0|0)_restr" <- function(j, p, drug){
  `B_(1|1)_(0|0)_restr`(j, p, drug) - c(`B_(0|0)_(1|1)_restr`(j, p, drug)[2], `B_(0|0)_(1|1)_restr`(j, p, drug)[1])
}

# Cdf-band for level p: Counterfactual effect (1|1) - (1|0)
"B_(1|1)_(1|0)_restr" <- function(j, p, drug){
  as.numeric(c(max(0, `F_(1|1)`(j, drug) - `c_(1|1)-(1|0)`(p, drug) * `s_k_11`(j, drug)), min(1, `F_(1|1)`(j, drug) + `c_(1|1)-(1|0)`(p, drug) * `s_k_11`(j, drug))))
}
"B_(1|0)_(1|1)_restr" <- function(j, p, drug){
  as.numeric(c(max(0, `F_(1|0)`(j, drug) - `c_(1|1)-(1|0)`(p, drug) * `s_k_10`(j, drug)), min(1, `F_(1|0)`(j, drug) + `c_(1|1)-(1|0)`(p, drug) * `s_k_10`(j, drug))))
}
"B_(1|1)-(1|0)_restr" <- function(j, p, drug){
  `B_(1|1)_(1|0)_restr`(j, p, drug) - c(`B_(1|0)_(1|1)_restr`(j, p, drug)[2], `B_(1|0)_(1|1)_restr`(j, p, drug)[1])
}

# Cdf-band for level p: Counterfactual effect (1|0) - (0|0)
"B_(1|0)_(0|0)_restr" <- function(j, p, drug){
  as.numeric(c(max(0, `F_(1|0)`(j, drug) - `c_(1|0)-(0|0)`(p, drug) * `s_k_10`(j, drug)), min(1, `F_(1|0)`(j, drug) + `c_(1|0)-(0|0)`(p, drug) * `s_k_10`(j, drug))))
}
"B_(0|0)_(1|0)_restr" <- function(j, p, drug){
  as.numeric(c(max(0, `F_(0|0)`(j, drug) - `c_(1|0)-(0|0)`(p, drug) * `s_k_00`(j, drug)), min(1, `F_(0|0)`(j, drug) + `c_(1|0)-(0|0)`(p, drug) * `s_k_00`(j, drug))))
}
"B_(1|0)-(0|0)_restr" <- function(j, p, drug){
  `B_(1|0)_(0|0)_restr`(j, p, drug) - c(`B_(0|0)_(1|0)_restr`(j, p, drug)[2], `B_(0|0)_(1|0)_restr`(j, p, drug)[1])
}

# Cdf-band for level p: Counterfactual effect (1|1) - (0|1)
"B_(1|1)_(0|1)_restr" <- function(j, p, drug){
  as.numeric(c(max(0, `F_(1|1)`(j, drug) - `c_(1|1)-(0|1)`(p, drug) * `s_k_11`(j, drug)), min(1, `F_(1|1)`(j, drug) + `c_(1|1)-(0|1)`(p, drug) * `s_k_11`(j, drug))))
}
"B_(0|1)_(1|1)_restr" <- function(j, p, drug){
  as.numeric(c(max(0, `F_(0|1)`(j, drug) - `c_(1|1)-(0|1)`(p, drug) * `s_k_01`(j, drug)), min(1, `F_(0|1)`(j, drug) + `c_(1|1)-(0|1)`(p, drug) * `s_k_01`(j, drug))))
}
"B_(1|1)-(0|1)_restr" <- function(j, p, drug){
  `B_(1|1)_(0|1)_restr`(j, p, drug) - c(`B_(0|1)_(1|1)_restr`(j, p, drug)[2], `B_(0|1)_(1|1)_restr`(j, p, drug)[1])
}

# Cdf-band for level p: Counterfactual effect (0|1) - (0|0)
"B_(0|1)_(0|0)_restr" <- function(j, p, drug){
  as.numeric(c(max(0, `F_(0|1)`(j, drug) - `c_(0|1)-(0|0)`(p, drug) * `s_k_01`(j, drug)), min(1, `F_(0|1)`(j, drug) + `c_(0|1)-(0|0)`(p, drug) * `s_k_01`(j, drug))))
}
"B_(0|0)_(0|1)_restr" <- function(j, p, drug){
  as.numeric(c(max(0, `F_(0|0)`(j, drug) - `c_(0|1)-(0|0)`(p, drug) * `s_k_00`(j, drug)), min(1, `F_(0|0)`(j, drug) + `c_(0|1)-(0|0)`(p, drug) * `s_k_00`(j, drug))))
}
"B_(0|1)-(0|0)_restr" <- function(j, p, drug){
  `B_(0|1)_(0|0)_restr`(j, p, drug) - c(`B_(0|0)_(0|1)_restr`(j, p, drug)[2], `B_(0|0)_(0|1)_restr`(j, p, drug)[1])
}


# Functions for survival functions (and not their differences)
# Compute critical value: (0|0)
"c_(0|0)" <- function(p, drug){
  mat <- cbind(
    abs(get(paste("F_(0|0)_draws", drug, sep = "_"))[, 1] - `F_(0|0)`(1, drug)) / `s_k_00`(1, drug),
    abs(get(paste("F_(0|0)_draws", drug, sep = "_"))[, 2] - `F_(0|0)`(2, drug)) / `s_k_00`(2, drug),
    abs(get(paste("F_(0|0)_draws", drug, sep = "_"))[, 3] - `F_(0|0)`(3, drug)) / `s_k_00`(3, drug),
    abs(get(paste("F_(0|0)_draws", drug, sep = "_"))[, 4] - `F_(0|0)`(4, drug)) / `s_k_00`(4, drug),
    abs(get(paste("F_(0|0)_draws", drug, sep = "_"))[, 5] - `F_(0|0)`(5, drug)) / `s_k_00`(5, drug)
  )
  dist <- apply(mat, MARGIN = 1, max)
  quantile(dist, probs = p)
}
# Compute critical value: (1|0)
"c_(1|0)" <- function(p, drug){
  mat <- cbind(
    abs(get(paste("F_(1|0)_draws", drug, sep = "_"))[, 1] - `F_(1|0)`(1, drug)) / `s_k_10`(1, drug),
    abs(get(paste("F_(1|0)_draws", drug, sep = "_"))[, 2] - `F_(1|0)`(2, drug)) / `s_k_10`(2, drug),
    abs(get(paste("F_(1|0)_draws", drug, sep = "_"))[, 3] - `F_(1|0)`(3, drug)) / `s_k_10`(3, drug),
    abs(get(paste("F_(1|0)_draws", drug, sep = "_"))[, 4] - `F_(1|0)`(4, drug)) / `s_k_10`(4, drug),
    abs(get(paste("F_(1|0)_draws", drug, sep = "_"))[, 5] - `F_(1|0)`(5, drug)) / `s_k_10`(5, drug)
  )
  dist <- apply(mat, MARGIN = 1, max)
  quantile(dist, probs = p)
}
# Compute critical value: (0|1)
"c_(0|1)" <- function(p, drug){
  mat <- cbind(
    abs(get(paste("F_(0|1)_draws", drug, sep = "_"))[, 1] - `F_(0|1)`(1, drug)) / `s_k_01`(1, drug),
    abs(get(paste("F_(0|1)_draws", drug, sep = "_"))[, 2] - `F_(0|1)`(2, drug)) / `s_k_01`(2, drug),
    abs(get(paste("F_(0|1)_draws", drug, sep = "_"))[, 3] - `F_(0|1)`(3, drug)) / `s_k_01`(3, drug),
    abs(get(paste("F_(0|1)_draws", drug, sep = "_"))[, 4] - `F_(0|1)`(4, drug)) / `s_k_01`(4, drug),
    abs(get(paste("F_(0|1)_draws", drug, sep = "_"))[, 5] - `F_(0|1)`(5, drug)) / `s_k_01`(5, drug)
  )
  dist <- apply(mat, MARGIN = 1, max)
  quantile(dist, probs = p)
}
# Compute critical value: (1|1)
"c_(1|1)" <- function(p, drug){
  mat <- cbind(
    abs(get(paste("F_(1|1)_draws", drug, sep = "_"))[, 1] - `F_(1|1)`(1, drug)) / `s_k_11`(1, drug),
    abs(get(paste("F_(1|1)_draws", drug, sep = "_"))[, 2] - `F_(1|1)`(2, drug)) / `s_k_11`(2, drug),
    abs(get(paste("F_(1|1)_draws", drug, sep = "_"))[, 3] - `F_(1|1)`(3, drug)) / `s_k_11`(3, drug),
    abs(get(paste("F_(1|1)_draws", drug, sep = "_"))[, 4] - `F_(1|1)`(4, drug)) / `s_k_11`(4, drug),
    abs(get(paste("F_(1|1)_draws", drug, sep = "_"))[, 5] - `F_(1|1)`(5, drug)) / `s_k_11`(5, drug)
  )
  dist <- apply(mat, MARGIN = 1, max)
  quantile(dist, probs = p)
}

# Cdf-band for level p
"B_(0|0)" <- function(j, p, drug){
  as.numeric(c(`F_(0|0)`(j, drug) - `c_(0|0)`(p, drug) * `s_k_00`(j, drug), `F_(0|0)`(j, drug) + `c_(0|0)`(p, drug) * `s_k_00`(j, drug)))
}
"B_(1|0)" <- function(j, p, drug){
  as.numeric(c(`F_(1|0)`(j, drug) - `c_(1|0)`(p, drug) * `s_k_10`(j, drug), `F_(1|0)`(j, drug) + `c_(1|0)`(p, drug) * `s_k_10`(j, drug)))
}
"B_(0|1)" <- function(j, p, drug){
  as.numeric(c(`F_(0|1)`(j, drug) - `c_(0|1)`(p, drug) * `s_k_01`(j, drug), `F_(0|1)`(j, drug) + `c_(0|1)`(p, drug) * `s_k_01`(j, drug)))
}
"B_(1|1)" <- function(j, p, drug){
  as.numeric(c(`F_(1|1)`(j, drug) - `c_(1|1)`(p, drug) * `s_k_11`(j, drug), `F_(1|1)`(j, drug) + `c_(1|1)`(p, drug) * `s_k_11`(j, drug)))
}
"B_(0|0)_restr" <- function(j, p, drug){
  as.numeric(c(max(0, `F_(0|0)`(j, drug) - `c_(0|0)`(p, drug) * `s_k_00`(j, drug)), min(1, `F_(0|0)`(j, drug) + `c_(0|0)`(p, drug) * `s_k_00`(j, drug))))
}
"B_(1|0)_restr" <- function(j, p, drug){
  as.numeric(c(max(0, `F_(1|0)`(j, drug) - `c_(1|0)`(p, drug) * `s_k_10`(j, drug)), min(1, `F_(1|0)`(j, drug) + `c_(1|0)`(p, drug) * `s_k_10`(j, drug))))
}
"B_(0|1)_restr" <- function(j, p, drug){
  as.numeric(c(max(0, `F_(0|1)`(j, drug) - `c_(0|1)`(p, drug) * `s_k_01`(j, drug)), min(1, `F_(0|1)`(j, drug) + `c_(0|1)`(p, drug) * `s_k_01`(j, drug))))
}
"B_(1|1)_restr" <- function(j, p, drug){
  as.numeric(c(max(0, `F_(1|1)`(j, drug) - `c_(1|1)`(p, drug) * `s_k_11`(j, drug)), min(1, `F_(1|1)`(j, drug) + `c_(1|1)`(p, drug) * `s_k_11`(j, drug))))
}
  


