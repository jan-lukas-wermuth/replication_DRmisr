# Title:      Second Estimation Step: Code for Figure 1 b,e,h and 2 b,e,h (3 a,c,e and 4 a,c,e)
# Author:     Jan-Lukas Wermuth
# Date:       2025-05-06
# Purpose:    This script performs the bootstrap for the plots for Y
#             with misreporting and constructs the corresponding
#             simultaneous confidence bands together with all the plots.

rm(list = ls())

library(ggplot2)
library(Cairo)
library(labelled)
library(mnormt)
library(dplyr)
library(GJRM)
library(doParallel)

setwd("~/Dropbox/GLW/replication_DRmisr")

load("data/yrbs_data_combined_201520172019_norecreational_mar_life.RData")
load("data/yrbs_data_combined_201520172019_recreational_mar_life.RData")
load("data/X_norecreational_mar_life.RData")
load("data/X_recreational_mar_life.RData")
invisible(lapply(list.files("results/GJRM_results", pattern = "\\.RData$", full.names = TRUE), function(x) load(x, envir = globalenv())))
source("functions/MisreportingY_functions.R")

# Bootstrap -----------------------------------------------------------
B <- 1000
"F_(0|0)_draws_mar_life" <- matrix(NA, ncol = 5, nrow = B)
"F_(1|0)_draws_mar_life" <- matrix(NA, ncol = 5, nrow = B)
"F_(0|1)_draws_mar_life" <- matrix(NA, ncol = 5, nrow = B)
"F_(1|1)_draws_mar_life" <- matrix(NA, ncol = 5, nrow = B)
for (i in 1:5) {
  `F_(0|0)_draws_mar_life`[,i] <- `F_(0|0)_b`(i, B, "mar_life")
  `F_(1|0)_draws_mar_life`[,i] <- `F_(1|0)_b`(i, B, "mar_life")
  `F_(0|1)_draws_mar_life`[,i] <- `F_(0|1)_b`(i, B, "mar_life")
  `F_(1|1)_draws_mar_life`[,i] <- `F_(1|1)_b`(i, B, "mar_life")
}

save("F_(0|0)_draws_mar_life", file = "results/Bootstrap_results/Y_misr/F_(0|0)_draws_mar_life.RData")
save("F_(1|0)_draws_mar_life", file = "results/Bootstrap_results/Y_misr/F_(1|0)_draws_mar_life.RData")
save("F_(0|1)_draws_mar_life", file = "results/Bootstrap_results/Y_misr/F_(0|1)_draws_mar_life.RData")
save("F_(1|1)_draws_mar_life", file = "results/Bootstrap_results/Y_misr/F_(1|1)_draws_mar_life.RData")

# Plots -------------------------------------------------------------------
p <- 0.9
drug <- "mar_life"
# Overall Effect (1|1) - (0|0)
x_values <- 0:4
y_values_lower <- c(`B_(1|1)-(0|0)`(1, p, drug)[1], `B_(1|1)-(0|0)`(2, p, drug)[1], `B_(1|1)-(0|0)`(3, p, drug)[1], `B_(1|1)-(0|0)`(4, p, drug)[1], `B_(1|1)-(0|0)`(5, p, drug)[1])
y_values_upper <- c(`B_(1|1)-(0|0)`(1, p, drug)[2], `B_(1|1)-(0|0)`(2, p, drug)[2], `B_(1|1)-(0|0)`(3, p, drug)[2], `B_(1|1)-(0|0)`(4, p, drug)[2], `B_(1|1)-(0|0)`(5, p, drug)[2])
y_values <- c(`F_(1|1)`(1, drug) - `F_(0|0)`(1, drug), `F_(1|1)`(2, drug) - `F_(0|0)`(2, drug), `F_(1|1)`(3, drug) - `F_(0|0)`(3, drug), `F_(1|1)`(4, drug) - `F_(0|0)`(4, drug), `F_(1|1)`(5, drug) - `F_(0|0)`(5, drug))
data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
ribbon_data <-
  tibble(
    x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
    y_l = rep(y_values_lower, rep(2, 5)),
    y_u = rep(y_values_upper, rep(2, 5)))
p1 <- ggplot2::ggplot(data = data) + 
  geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
  geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
  xlab("y") + ylab("p") + theme_bw() + ylim(c(-0.15,0.15))

# Counterfactual Effect (1|1) - (1|0)
x_values <- 0:4
y_values_lower <- c(`B_(1|1)-(1|0)`(1, p, drug)[1], `B_(1|1)-(1|0)`(2, p, drug)[1], `B_(1|1)-(1|0)`(3, p, drug)[1], `B_(1|1)-(1|0)`(4, p, drug)[1], `B_(1|1)-(1|0)`(5, p, drug)[1])
y_values_upper <- c(`B_(1|1)-(1|0)`(1, p, drug)[2], `B_(1|1)-(1|0)`(2, p, drug)[2], `B_(1|1)-(1|0)`(3, p, drug)[2], `B_(1|1)-(1|0)`(4, p, drug)[2], `B_(1|1)-(1|0)`(5, p, drug)[2])
y_values <- c(`F_(1|1)`(1, drug) - `F_(1|0)`(1, drug), `F_(1|1)`(2, drug) - `F_(1|0)`(2, drug), `F_(1|1)`(3, drug) - `F_(1|0)`(3, drug), `F_(1|1)`(4, drug) - `F_(1|0)`(4, drug), `F_(1|1)`(5, drug) - `F_(1|0)`(5, drug))
data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
ribbon_data <-
  tibble(
    x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
    y_l = rep(y_values_lower, rep(2, 5)),
    y_u = rep(y_values_upper, rep(2, 5)))
p2 <- ggplot2::ggplot(data = data) + 
  geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
  geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
  xlab("y") + ylab("p") + theme_bw() + ylim(c(-0.15,0.15))

# Counterfactual Effect (1|0) - (0|0)
x_values <- 0:4
y_values_lower <- c(`B_(1|0)-(0|0)`(1, p, drug)[1], `B_(1|0)-(0|0)`(2, p, drug)[1], `B_(1|0)-(0|0)`(3, p, drug)[1], `B_(1|0)-(0|0)`(4, p, drug)[1], `B_(1|0)-(0|0)`(5, p, drug)[1])
y_values_upper <- c(`B_(1|0)-(0|0)`(1, p, drug)[2], `B_(1|0)-(0|0)`(2, p, drug)[2], `B_(1|0)-(0|0)`(3, p, drug)[2], `B_(1|0)-(0|0)`(4, p, drug)[2], `B_(1|0)-(0|0)`(5, p, drug)[2])
y_values <- c(`F_(1|0)`(1, drug) - `F_(0|0)`(1, drug), `F_(1|0)`(2, drug) - `F_(0|0)`(2, drug), `F_(1|0)`(3, drug) - `F_(0|0)`(3, drug), `F_(1|0)`(4, drug) - `F_(0|0)`(4, drug), `F_(1|0)`(5, drug) - `F_(0|0)`(5, drug))
data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
ribbon_data <-
  tibble(
    x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
    y_l = rep(y_values_lower, rep(2, 5)),
    y_u = rep(y_values_upper, rep(2, 5)))
p3 <- ggplot2::ggplot(data = data) + 
  geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
  geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
  xlab("y") + ylab("p") + theme_bw() + ylim(c(-0.15,0.15))

# Counterfactual Effect (1|1) - (0|1)
x_values <- 0:4
y_values_lower <- c(`B_(1|1)-(0|1)`(1, p, drug)[1], `B_(1|1)-(0|1)`(2, p, drug)[1], `B_(1|1)-(0|1)`(3, p, drug)[1], `B_(1|1)-(0|1)`(4, p, drug)[1], `B_(1|1)-(0|1)`(5, p, drug)[1])
y_values_upper <- c(`B_(1|1)-(0|1)`(1, p, drug)[2], `B_(1|1)-(0|1)`(2, p, drug)[2], `B_(1|1)-(0|1)`(3, p, drug)[2], `B_(1|1)-(0|1)`(4, p, drug)[2], `B_(1|1)-(0|1)`(5, p, drug)[2])
y_values <- c(`F_(1|1)`(1, drug) - `F_(0|1)`(1, drug), `F_(1|1)`(2, drug) - `F_(0|1)`(2, drug), `F_(1|1)`(3, drug) - `F_(0|1)`(3, drug), `F_(1|1)`(4, drug) - `F_(0|1)`(4, drug), `F_(1|1)`(5, drug) - `F_(0|1)`(5, drug))
data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
ribbon_data <-
  tibble(
    x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
    y_l = rep(y_values_lower, rep(2, 5)),
    y_u = rep(y_values_upper, rep(2, 5)))
p4 <- ggplot2::ggplot(data = data) + 
  geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
  geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
  xlab("y") + ylab("p") + theme_bw() + ylim(c(-0.15,0.15))

# Counterfactual Effect (0|1) - (0|0)
x_values <- 0:4
y_values_lower <- c(`B_(0|1)-(0|0)`(1, p, drug)[1], `B_(0|1)-(0|0)`(2, p, drug)[1], `B_(0|1)-(0|0)`(3, p, drug)[1], `B_(0|1)-(0|0)`(4, p, drug)[1], `B_(0|1)-(0|0)`(5, p, drug)[1])
y_values_upper <- c(`B_(0|1)-(0|0)`(1, p, drug)[2], `B_(0|1)-(0|0)`(2, p, drug)[2], `B_(0|1)-(0|0)`(3, p, drug)[2], `B_(0|1)-(0|0)`(4, p, drug)[2], `B_(0|1)-(0|0)`(5, p, drug)[2])
y_values <- c(`F_(0|1)`(1, drug) - `F_(0|0)`(1, drug), `F_(0|1)`(2, drug) - `F_(0|0)`(2, drug), `F_(0|1)`(3, drug) - `F_(0|0)`(3, drug), `F_(0|1)`(4, drug) - `F_(0|0)`(4, drug), `F_(0|1)`(5, drug) - `F_(0|0)`(5, drug))
data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
ribbon_data <-
  tibble(
    x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
    y_l = rep(y_values_lower, rep(2, 5)),
    y_u = rep(y_values_upper, rep(2, 5)))
p5 <- ggplot2::ggplot(data = data) + 
  geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
  geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
  xlab("y") + ylab("p") + theme_bw() + ylim(c(-0.15,0.15))

ggsave(filename = paste0("results/plots/Y_misr/OverallEffect_rec.pdf"), plot = p1, device = cairo_pdf, width = 5, height = 5)
ggsave(filename = paste0("results/plots/Y_misr/CounterfactualEffect_(1_1)-(1_0)_rec.pdf"), plot = p2, device = cairo_pdf, width = 5, height = 5)
ggsave(filename = paste0("results/plots/Y_misr/CounterfactualEffect_(1_0)-(0_0)_rec.pdf"), plot = p3, device = cairo_pdf, width = 5, height = 5)
ggsave(filename = paste0("results/plots/Y_misr/CounterfactualEffect_(1_1)-(0_1)_rec.pdf"), plot = p4, device = cairo_pdf, width = 5, height = 5)
ggsave(filename = paste0("results/plots/Y_misr/CounterfactualEffect_(0_1)-(0_0)_rec.pdf"), plot = p5, device = cairo_pdf, width = 5, height = 5)

# Overall Effect (1|1) - (0|0)
x_values <- 0:4
y_values_lower <- c(`B_(1|1)-(0|0)_restr`(1, p, drug)[1], `B_(1|1)-(0|0)_restr`(2, p, drug)[1], `B_(1|1)-(0|0)_restr`(3, p, drug)[1], `B_(1|1)-(0|0)_restr`(4, p, drug)[1], `B_(1|1)-(0|0)_restr`(5, p, drug)[1])
y_values_upper <- c(`B_(1|1)-(0|0)_restr`(1, p, drug)[2], `B_(1|1)-(0|0)_restr`(2, p, drug)[2], `B_(1|1)-(0|0)_restr`(3, p, drug)[2], `B_(1|1)-(0|0)_restr`(4, p, drug)[2], `B_(1|1)-(0|0)_restr`(5, p, drug)[2])
y_values <- c(`F_(1|1)`(1, drug) - `F_(0|0)`(1, drug), `F_(1|1)`(2, drug) - `F_(0|0)`(2, drug), `F_(1|1)`(3, drug) - `F_(0|0)`(3, drug), `F_(1|1)`(4, drug) - `F_(0|0)`(4, drug), `F_(1|1)`(5, drug) - `F_(0|0)`(5, drug))
data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
ribbon_data <-
  tibble(
    x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
    y_l = rep(y_values_lower, rep(2, 5)),
    y_u = rep(y_values_upper, rep(2, 5)))
p6 <- ggplot2::ggplot(data = data) + 
  geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
  geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
  xlab("y") + ylab("p") + theme_bw() + ylim(c(-0.15,0.15))

# Counterfactual Effect (1|1) - (1|0)
x_values <- 0:4
y_values_lower <- c(`B_(1|1)-(1|0)_restr`(1, p, drug)[1], `B_(1|1)-(1|0)_restr`(2, p, drug)[1], `B_(1|1)-(1|0)_restr`(3, p, drug)[1], `B_(1|1)-(1|0)_restr`(4, p, drug)[1], `B_(1|1)-(1|0)_restr`(5, p, drug)[1])
y_values_upper <- c(`B_(1|1)-(1|0)_restr`(1, p, drug)[2], `B_(1|1)-(1|0)_restr`(2, p, drug)[2], `B_(1|1)-(1|0)_restr`(3, p, drug)[2], `B_(1|1)-(1|0)_restr`(4, p, drug)[2], `B_(1|1)-(1|0)_restr`(5, p, drug)[2])
y_values <- c(`F_(1|1)`(1, drug) - `F_(1|0)`(1, drug), `F_(1|1)`(2, drug) - `F_(1|0)`(2, drug), `F_(1|1)`(3, drug) - `F_(1|0)`(3, drug), `F_(1|1)`(4, drug) - `F_(1|0)`(4, drug), `F_(1|1)`(5, drug) - `F_(1|0)`(5, drug))
data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
ribbon_data <-
  tibble(
    x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
    y_l = rep(y_values_lower, rep(2, 5)),
    y_u = rep(y_values_upper, rep(2, 5)))
p7 <- ggplot2::ggplot(data = data) + 
  geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
  geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
  xlab("y") + ylab("p") + theme_bw() + ylim(c(-0.15,0.15))

# Counterfactual Effect (1|0) - (0|0)
x_values <- 0:4
y_values_lower <- c(`B_(1|0)-(0|0)_restr`(1, p, drug)[1], `B_(1|0)-(0|0)_restr`(2, p, drug)[1], `B_(1|0)-(0|0)_restr`(3, p, drug)[1], `B_(1|0)-(0|0)_restr`(4, p, drug)[1], `B_(1|0)-(0|0)_restr`(5, p, drug)[1])
y_values_upper <- c(`B_(1|0)-(0|0)_restr`(1, p, drug)[2], `B_(1|0)-(0|0)_restr`(2, p, drug)[2], `B_(1|0)-(0|0)_restr`(3, p, drug)[2], `B_(1|0)-(0|0)_restr`(4, p, drug)[2], `B_(1|0)-(0|0)_restr`(5, p, drug)[2])
y_values <- c(`F_(1|0)`(1, drug) - `F_(0|0)`(1, drug), `F_(1|0)`(2, drug) - `F_(0|0)`(2, drug), `F_(1|0)`(3, drug) - `F_(0|0)`(3, drug), `F_(1|0)`(4, drug) - `F_(0|0)`(4, drug), `F_(1|0)`(5, drug) - `F_(0|0)`(5, drug))
data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
ribbon_data <-
  tibble(
    x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
    y_l = rep(y_values_lower, rep(2, 5)),
    y_u = rep(y_values_upper, rep(2, 5)))
p8 <- ggplot2::ggplot(data = data) + 
  geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
  geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
  xlab("y") + ylab("p") + theme_bw() + ylim(c(-0.15,0.15))

# Counterfactual Effect (1|1) - (0|1)
x_values <- 0:4
y_values_lower <- c(`B_(1|1)-(0|1)_restr`(1, p, drug)[1], `B_(1|1)-(0|1)_restr`(2, p, drug)[1], `B_(1|1)-(0|1)_restr`(3, p, drug)[1], `B_(1|1)-(0|1)_restr`(4, p, drug)[1], `B_(1|1)-(0|1)_restr`(5, p, drug)[1])
y_values_upper <- c(`B_(1|1)-(0|1)_restr`(1, p, drug)[2], `B_(1|1)-(0|1)_restr`(2, p, drug)[2], `B_(1|1)-(0|1)_restr`(3, p, drug)[2], `B_(1|1)-(0|1)_restr`(4, p, drug)[2], `B_(1|1)-(0|1)_restr`(5, p, drug)[2])
y_values <- c(`F_(1|1)`(1, drug) - `F_(0|1)`(1, drug), `F_(1|1)`(2, drug) - `F_(0|1)`(2, drug), `F_(1|1)`(3, drug) - `F_(0|1)`(3, drug), `F_(1|1)`(4, drug) - `F_(0|1)`(4, drug), `F_(1|1)`(5, drug) - `F_(0|1)`(5, drug))
data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
ribbon_data <-
  tibble(
    x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
    y_l = rep(y_values_lower, rep(2, 5)),
    y_u = rep(y_values_upper, rep(2, 5)))
p9 <- ggplot2::ggplot(data = data) + 
  geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
  geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
  xlab("y") + ylab("p") + theme_bw() + ylim(c(-0.15,0.15))

# Counterfactual Effect (0|1) - (0|0)
x_values <- 0:4
y_values_lower <- c(`B_(0|1)-(0|0)_restr`(1, p, drug)[1], `B_(0|1)-(0|0)_restr`(2, p, drug)[1], `B_(0|1)-(0|0)_restr`(3, p, drug)[1], `B_(0|1)-(0|0)_restr`(4, p, drug)[1], `B_(0|1)-(0|0)_restr`(5, p, drug)[1])
y_values_upper <- c(`B_(0|1)-(0|0)_restr`(1, p, drug)[2], `B_(0|1)-(0|0)_restr`(2, p, drug)[2], `B_(0|1)-(0|0)_restr`(3, p, drug)[2], `B_(0|1)-(0|0)_restr`(4, p, drug)[2], `B_(0|1)-(0|0)_restr`(5, p, drug)[2])
y_values <- c(`F_(0|1)`(1, drug) - `F_(0|0)`(1, drug), `F_(0|1)`(2, drug) - `F_(0|0)`(2, drug), `F_(0|1)`(3, drug) - `F_(0|0)`(3, drug), `F_(0|1)`(4, drug) - `F_(0|0)`(4, drug), `F_(0|1)`(5, drug) - `F_(0|0)`(5, drug))
data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
ribbon_data <-
  tibble(
    x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
    y_l = rep(y_values_lower, rep(2, 5)),
    y_u = rep(y_values_upper, rep(2, 5)))
p10 <- ggplot2::ggplot(data = data) + 
  geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
  geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
  xlab("y") + ylab("p") + theme_bw() + ylim(c(-0.15,0.15))


ggsave(filename = paste0("results/plots/Y_misr/OverallEffect_restr_rec.pdf"), plot = p6, device = cairo_pdf, width = 5, height = 5)
ggsave(filename = paste0("results/plots/Y_misr/CounterfactualEffect_(1_1)-(1_0)_restr_rec.pdf"), plot = p7, device = cairo_pdf, width = 5, height = 5)
ggsave(filename = paste0("results/plots/Y_misr/CounterfactualEffect_(1_0)-(0_0)_restr_rec.pdf"), plot = p8, device = cairo_pdf, width = 5, height = 5)
ggsave(filename = paste0("results/plots/Y_misr/CounterfactualEffect_(1_1)-(0_1)_restr_rec.pdf"), plot = p9, device = cairo_pdf, width = 5, height = 5)
ggsave(filename = paste0("results/plots/Y_misr/CounterfactualEffect_(0_1)-(0_0)_restr_rec.pdf"), plot = p10, device = cairo_pdf, width = 5, height = 5)


# We add 4 graphics for F_(0|0), F_(1|0), F_(0|1) and F_(1|1)
################ Plots ####################
library(ggplot2)
library(Cairo)
# (0|0)
x_values <- 0:4
y_values_lower <- c(`B_(0|0)`(1, p, drug)[1], `B_(0|0)`(2, p, drug)[1], `B_(0|0)`(3, p, drug)[1], `B_(0|0)`(4, p, drug)[1], `B_(0|0)`(5, p, drug)[1])
y_values_upper <- c(`B_(0|0)`(1, p, drug)[2], `B_(0|0)`(2, p, drug)[2], `B_(0|0)`(3, p, drug)[2], `B_(0|0)`(4, p, drug)[2], `B_(0|0)`(5, p, drug)[2])
y_values <- c(`F_(0|0)`(1, drug), `F_(0|0)`(2, drug), `F_(0|0)`(3, drug), `F_(0|0)`(4, drug), `F_(0|0)`(5, drug))
data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
ribbon_data <-
  tibble(
    x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
    y_l = rep(y_values_lower, rep(2, 5)),
    y_u = rep(y_values_upper, rep(2, 5)))
p11 <- ggplot2::ggplot(data = data) + 
  geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
  geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
  xlab("y") + ylab("p") + theme_bw() + ylim(c(0,1))

# (1|0)
x_values <- 0:4
y_values_lower <- c(`B_(1|0)`(1, p, drug)[1], `B_(1|0)`(2, p, drug)[1], `B_(1|0)`(3, p, drug)[1], `B_(1|0)`(4, p, drug)[1], `B_(1|0)`(5, p, drug)[1])
y_values_upper <- c(`B_(1|0)`(1, p, drug)[2], `B_(1|0)`(2, p, drug)[2], `B_(1|0)`(3, p, drug)[2], `B_(1|0)`(4, p, drug)[2], `B_(1|0)`(5, p, drug)[2])
y_values <- c(`F_(1|0)`(1, drug), `F_(1|0)`(2, drug), `F_(1|0)`(3, drug), `F_(1|0)`(4, drug), `F_(1|0)`(5, drug))
data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
ribbon_data <-
  tibble(
    x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
    y_l = rep(y_values_lower, rep(2, 5)),
    y_u = rep(y_values_upper, rep(2, 5)))
p12 <- ggplot2::ggplot(data = data) + 
  geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
  geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
  xlab("y") + ylab("p") + theme_bw() + ylim(c(0,1))

# (0|1)
x_values <- 0:4
y_values_lower <- c(`B_(0|1)`(1, p, drug)[1], `B_(0|1)`(2, p, drug)[1], `B_(0|1)`(3, p, drug)[1], `B_(0|1)`(4, p, drug)[1], `B_(0|1)`(5, p, drug)[1])
y_values_upper <- c(`B_(0|1)`(1, p, drug)[2], `B_(0|1)`(2, p, drug)[2], `B_(0|1)`(3, p, drug)[2], `B_(0|1)`(4, p, drug)[2], `B_(0|1)`(5, p, drug)[2])
y_values <- c(`F_(0|1)`(1, drug), `F_(0|1)`(2, drug), `F_(0|1)`(3, drug), `F_(0|1)`(4, drug), `F_(0|1)`(5, drug))
data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
ribbon_data <-
  tibble(
    x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
    y_l = rep(y_values_lower, rep(2, 5)),
    y_u = rep(y_values_upper, rep(2, 5)))
p13 <- ggplot2::ggplot(data = data) + 
  geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
  geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
  xlab("y") + ylab("p") + theme_bw() + ylim(c(0,1))

# (1|1)
x_values <- 0:4
y_values_lower <- c(`B_(1|1)`(1, p, drug)[1], `B_(1|1)`(2, p, drug)[1], `B_(1|1)`(3, p, drug)[1], `B_(1|1)`(4, p, drug)[1], `B_(1|1)`(5, p, drug)[1])
y_values_upper <- c(`B_(1|1)`(1, p, drug)[2], `B_(1|1)`(2, p, drug)[2], `B_(1|1)`(3, p, drug)[2], `B_(1|1)`(4, p, drug)[2], `B_(1|1)`(5, p, drug)[2])
y_values <- c(`F_(1|1)`(1, drug), `F_(1|1)`(2, drug), `F_(1|1)`(3, drug), `F_(1|1)`(4, drug), `F_(1|1)`(5, drug))
data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
ribbon_data <-
  tibble(
    x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
    y_l = rep(y_values_lower, rep(2, 5)),
    y_u = rep(y_values_upper, rep(2, 5)))
p14 <- ggplot2::ggplot(data = data) + 
  geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
  geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
  xlab("y") + ylab("p") + theme_bw() + ylim(c(0,1))

ggsave(filename = paste0("results/plots/Y_misr/(0_0)_rec.pdf"), plot = p11, device = cairo_pdf, width = 5, height = 5)
ggsave(filename = paste0("results/plots/Y_misr/(1_0)_rec.pdf"), plot = p12, device = cairo_pdf, width = 5, height = 5)
ggsave(filename = paste0("results/plots/Y_misr/(0_1)_rec.pdf"), plot = p13, device = cairo_pdf, width = 5, height = 5)
ggsave(filename = paste0("results/plots/Y_misr/(1_1)_rec.pdf"), plot = p14, device = cairo_pdf, width = 5, height = 5)


x_values <- 0:4
y_values_lower <- c(`B_(0|0)_restr`(1, p, drug)[1], `B_(0|0)_restr`(2, p, drug)[1], `B_(0|0)_restr`(3, p, drug)[1], `B_(0|0)_restr`(4, p, drug)[1], `B_(0|0)_restr`(5, p, drug)[1])
y_values_upper <- c(`B_(0|0)_restr`(1, p, drug)[2], `B_(0|0)_restr`(2, p, drug)[2], `B_(0|0)_restr`(3, p, drug)[2], `B_(0|0)_restr`(4, p, drug)[2], `B_(0|0)_restr`(5, p, drug)[2])
y_values <- c(`F_(0|0)`(1, drug), `F_(0|0)`(2, drug), `F_(0|0)`(3, drug), `F_(0|0)`(4, drug), `F_(0|0)`(5, drug))
data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
ribbon_data <-
  tibble(
    x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
    y_l = rep(y_values_lower, rep(2, 5)),
    y_u = rep(y_values_upper, rep(2, 5)))
p15 <- ggplot2::ggplot(data = data) + 
  geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
  geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
  xlab("y") + ylab("p") + theme_bw() + ylim(c(0,1))

# (1|0)
x_values <- 0:4
y_values_lower <- c(`B_(1|0)_restr`(1, p, drug)[1], `B_(1|0)_restr`(2, p, drug)[1], `B_(1|0)_restr`(3, p, drug)[1], `B_(1|0)_restr`(4, p, drug)[1], `B_(1|0)_restr`(5, p, drug)[1])
y_values_upper <- c(`B_(1|0)_restr`(1, p, drug)[2], `B_(1|0)_restr`(2, p, drug)[2], `B_(1|0)_restr`(3, p, drug)[2], `B_(1|0)_restr`(4, p, drug)[2], `B_(1|0)_restr`(5, p, drug)[2])
y_values <- c(`F_(1|0)`(1, drug), `F_(1|0)`(2, drug), `F_(1|0)`(3, drug), `F_(1|0)`(4, drug), `F_(1|0)`(5, drug))
data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
ribbon_data <-
  tibble(
    x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
    y_l = rep(y_values_lower, rep(2, 5)),
    y_u = rep(y_values_upper, rep(2, 5)))
p16 <- ggplot2::ggplot(data = data) + 
  geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
  geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
  xlab("y") + ylab("p") + theme_bw() + ylim(c(0,1))

# (0|1)
x_values <- 0:4
y_values_lower <- c(`B_(0|1)_restr`(1, p, drug)[1], `B_(0|1)_restr`(2, p, drug)[1], `B_(0|1)_restr`(3, p, drug)[1], `B_(0|1)_restr`(4, p, drug)[1], `B_(0|1)_restr`(5, p, drug)[1])
y_values_upper <- c(`B_(0|1)_restr`(1, p, drug)[2], `B_(0|1)_restr`(2, p, drug)[2], `B_(0|1)_restr`(3, p, drug)[2], `B_(0|1)_restr`(4, p, drug)[2], `B_(0|1)_restr`(5, p, drug)[2])
y_values <- c(`F_(0|1)`(1, drug), `F_(0|1)`(2, drug), `F_(0|1)`(3, drug), `F_(0|1)`(4, drug), `F_(0|1)`(5, drug))
data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
ribbon_data <-
  tibble(
    x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
    y_l = rep(y_values_lower, rep(2, 5)),
    y_u = rep(y_values_upper, rep(2, 5)))
p17 <- ggplot2::ggplot(data = data) + 
  geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
  geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
  xlab("y") + ylab("p") + theme_bw() + ylim(c(0,1))

# (1|1)
x_values <- 0:4
y_values_lower <- c(`B_(1|1)_restr`(1, p, drug)[1], `B_(1|1)_restr`(2, p, drug)[1], `B_(1|1)_restr`(3, p, drug)[1], `B_(1|1)_restr`(4, p, drug)[1], `B_(1|1)_restr`(5, p, drug)[1])
y_values_upper <- c(`B_(1|1)_restr`(1, p, drug)[2], `B_(1|1)_restr`(2, p, drug)[2], `B_(1|1)_restr`(3, p, drug)[2], `B_(1|1)_restr`(4, p, drug)[2], `B_(1|1)_restr`(5, p, drug)[2])
y_values <- c(`F_(1|1)`(1, drug), `F_(1|1)`(2, drug), `F_(1|1)`(3, drug), `F_(1|1)`(4, drug), `F_(1|1)`(5, drug))
data <- data.frame(x = x_values, y_l = y_values_lower, y_u = y_values_upper, y = y_values)
ribbon_data <-
  tibble(
    x = c(0, 1 - .Machine$double.eps, 1, 2 - .Machine$double.eps, 2, 3 - .Machine$double.eps, 3, 4 - .Machine$double.eps, 4, 5 - .Machine$double.eps),
    y_l = rep(y_values_lower, rep(2, 5)),
    y_u = rep(y_values_upper, rep(2, 5)))
p18 <- ggplot2::ggplot(data = data) + 
  geom_segment(aes(x = x, y = y, xend = 1:5, yend = y)) +
  geom_ribbon(data = ribbon_data, aes(x = x, ymin = y_l, ymax = y_u), alpha = 0.5) +
  xlab("y") + ylab("p") + theme_bw() + ylim(c(0,1))

ggsave(filename = paste0("results/plots/Y_misr/(0_0)_restr_rec.pdf"), plot = p15, device = cairo_pdf, width = 5, height = 5)
ggsave(filename = paste0("results/plots/Y_misr/(1_0)_restr_rec.pdf"), plot = p16, device = cairo_pdf, width = 5, height = 5)
ggsave(filename = paste0("results/plots/Y_misr/(0_1)_restr_rec.pdf"), plot = p17, device = cairo_pdf, width = 5, height = 5)
ggsave(filename = paste0("results/plots/Y_misr/(1_1)_restr_rec.pdf"), plot = p18, device = cairo_pdf, width = 5, height = 5)


