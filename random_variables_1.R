library(tidyverse)
library(microbenchmark)
library(Rcpp)
library(profvis)
ggplot2::theme_set(theme_bw())
library(riskRegression) #For colCumSum

source("~/Desktop/Skole/Compstat/Assignment2/compStat2/MC_1.R")
source("~/Desktop/Skole/Compstat/Assignment2/compStat2/MC_IS.R")
#Results initial implementation
res <- ruin_probability(n = 100, N = 10000)

#Get confidence-interval for the estimator and plot

sigma_hat <- sd(res$outcomes)


p1 <- ggplot(data = as.data.frame(res), aes(x = seq_along(outcomes), y = cumsum(outcomes)/1:10000)) +
  geom_line(size = 1, col = "firebrick")

p1 + geom_ribbon(
  mapping = aes(
    ymin = pmax(res$running_ruin_prob - 1.96 * sigma_hat / sqrt(1:res$number_of_sim),0), 
    ymax = pmin(res$running_ruin_prob + 1.96 * sigma_hat / sqrt(1:res$number_of_sim),1)
  ), fill = "firebrick", alpha = 0.2) + ylab(expression(hat(p)(100)[MC])) + xlab("N") +
  theme(axis.title = element_text(size = 20)) + theme(axis.text = element_text(size = 14))

#--------------------------------------------------------------------------------------------------------#
# Initial benchmarking and profiling
#profvis::profvis({source("~/Desktop/Skole/Compstat/Assignment2/compStat2/MC_1.R")})

init_bench <- microbenchmark::microbenchmark(ruin_probability(n = 100, N = 10000),
                               ruin_probability_vec1(n = 100, N = 10000),
                               ruin_probability_vec2(n = 100, N = 10000), times = 100)

xtable::xtable(summary(init_bench))

init_bench %>% as_tibble(expr = .$expr, time = .$time) %>% filter(time<4e+08) %>%  mutate(time = time*0.000000001) %>%
  ggplot(aes(x = time, fill = expr)) + geom_density(alpha = 0.7) +
  ylab("") + theme(axis.text = element_text(face="bold"))  +
  scale_fill_discrete(labels=c('ruin_prob', 'ruin_prob_vec1', 'ruin_prob_vec2')) +
  xlab("Time (seconds)")



# Results importance sampling

res_IS <- ruin_importance(100, 10000, -0.5)

# Find optimal theta for fixed sample size
N_1 <- c(3750, 7500, 15000, 30000)
#Rough search
thetas <- seq(-0.5, 0.15, length.out = 50)
var_mat <- numeric(length(thetas)*length(N_1))

dim(var_mat) <- c(length(thetas), length(N_1))

for (i in seq_along(N_1)){
for (j in seq_along(thetas)){
  var_mat[j, i] <- ruin_importance(100, N_1[i], thetas[j])$var_T
}
}

colnames(var_mat) <- N_1

tibble(theta = thetas, as_tibble(var_mat)) %>%
  pivot_longer(cols = -theta, names_to = "N", values_to = "Variance") %>% 
  ggplot(aes(x = theta, y = log(Variance), col = N)) + 
  geom_smooth(se = F, size = 0.7) + 
  theme(axis.title = element_text(size = 20)) + theme(axis.text = element_text(size = 14)) + 
  xlab(expression(theta))

#Fine search
thetasf <- seq(-0.1, -0.01, length.out = 50)
var_matf <- numeric(length(thetasf)*length(N_1))

dim(var_matf) <- c(length(thetasf), length(N_1))

for (i in seq_along(N_1)){
  for (j in seq_along(thetasf)){
    var_matf[j, i] <- ruin_importance(100, N_1[i], thetasf[j])$var_T
  }
}

colnames(var_matf) <- N_1

tibble(theta = thetasf, as_tibble(var_matf)) %>%
  pivot_longer(cols = -theta, names_to = "N", values_to = "Variance") %>% 
  ggplot(aes(x = theta, y = log(Variance), col = N)) + 
  geom_smooth(se = F) + 
  theme(axis.title = element_text(size = 20)) + theme(axis.text = element_text(size = 14)) + 
  xlab(expression(theta))

#Optimal theta
theta_opt <- tibble(theta = thetasf, as_tibble(var_matf)) %>%
  pivot_longer(cols = -theta, names_to = "N", values_to = "Variance") %>%
  group_by(N) %>%
  slice_min(Variance) %>% 
  ungroup() %>% 
  summarise(theta_opt = mean(theta)) %>% pull()

# Optimal theta computation time
# time_mat <- numeric(length(thetas)*length(N_1))
# 
# dim(time_mat) <- c(length(thetas), length(N_1))
# 
# for (i in seq_along(N_1)){
#   for (j in seq_along(thetasf)){
#     time_mat[j, i] <- summary(microbenchmark::microbenchmark(ruin_importance(100, N_1[i], thetas[j]), times = 50))$median
#   }
# }
# 
# as_tibble(time_mat) %>% tibble(., theta = thetas) %>%
#   pivot_longer(-theta) %>%
#   mutate(N = case_when(name == "V1" ~ "3750",name == "V2" ~ "7500",name == "V3" ~ "15000",name == "V4" ~ "30000"), time = value) %>% 
#   ggplot(aes(x = theta, y = time, col = N)) +
#   geom_line(size = 1) + theme(axis.title = element_text(size = 20)) + theme(axis.text = element_text(size = 14)) + 
#   xlab(expression(theta))




#Compare normal MC to IS-MC, variance
var_seq <- c(1000,2500,5000, 7500, 10000, 12500,15000, 30000, 50000, 75000, 100000)

var_comp <- numeric(2*length(var_seq))

dim(var_comp) <- c(length(var_seq), 2)

for (i in 1:length(var_seq)){
 var_comp[i, 1] <- var(ruin_probability(100, var_seq[i])$outcomes)
 var_comp[i, 2] <- ruin_importance(100, var_seq[i], theta_opt)$var_T
}

tibble(MC = var_comp[, 1], IS = var_comp[, 2], N = var_seq) %>% 
  pivot_longer(-N, names_to = "type", values_to = "Variance") %>% 
  ggplot(aes(x = N, y = log(Variance), col = type)) + geom_smooth(se = FALSE)


#Benchmarking
benchmark_1 <- microbenchmark::microbenchmark(ruin_probability(100, 1000),
                                             ruin_probability(100, 2500),
                                             ruin_probability(100, 5000),
                                             ruin_probability(100, 15000),
                                             ruin_probability(100, 30000),
                                             ruin_probability(100, 50000),
                                             ruin_importance(100, 1000, theta_opt),
                                             ruin_importance(100, 2500, theta_opt),
                                             ruin_importance(100, 5000, theta_opt),
                                             ruin_importance(100, 15000, theta_opt),
                                             ruin_importance(100, 30000, theta_opt),
                                             ruin_importance(100, 50000, theta_opt))


tibble(expr = benchmark_1$expr, time = benchmark_1$time/1000000000) %>%
  mutate(N = as.integer(case_when(str_detect(expr, "1000") ~ "1000",
                                  str_detect(expr, "2500") ~ "2500",
                                  str_detect(expr, "5000") &
                                  str_detect(expr, "0000") == FALSE &
                                  str_detect(expr, "15") == FALSE  ~ "5000",
                                  str_detect(expr, "15000") ~ "15000",
                                  str_detect(expr, "30000") ~ "30000",
                                  str_detect(expr, "50000") ~ "50000")),
         type = ifelse(str_detect(expr, "importance"), "IS", "MC")) %>% 
  ggplot(aes(x = N, y = time, col = type)) + geom_smooth(span = 0.4, se = F)

# Rcpp implementation
Rcpp::sourceCpp(file = "MC_Rcpp.cpp")
ruin_probability_cpp(100, 500000)

#Benchmark rcpp vs R implementation
cppvR <- microbenchmark(ruin_probability(n = 100, N = 20000),
               ruin_probability_vec1(n = 100, N = 20000),
               ruin_probability_vec2(n = 100, N = 20000),
               ruin_probability_cpp(n = 100, N = 20000))

cppvR %>% as_tibble() %>% mutate(time = time/1000000000) %>% ggplot(aes(x = time, fill = expr)) + geom_density(alpha = .6) +
  scale_fill_discrete(labels=c('R for loop', 'R partly vectorized', 'R vectorized', "Rcpp"))

cppvRrange <- microbenchmark(ruin_probability(n = 100, N = 1000),
                             ruin_probability_vec1(n = 100, N = 1000),
                             ruin_probability_vec2(n = 100, N = 1000),
                             ruin_probability_cpp(n = 100, N = 1000),
                             ruin_probability(n = 100, N = 5000),
                             ruin_probability_vec1(n = 100, N = 5000),
                             ruin_probability_vec2(n = 100, N = 5000),
                             ruin_probability_cpp(n = 100, N = 5000),
                             ruin_probability(n = 100, N = 12000),
                             ruin_probability_vec1(n = 100, N = 12000),
                             ruin_probability_vec2(n = 100, N = 12000),
                             ruin_probability_cpp(n = 100, N = 12000),
                             ruin_probability(n = 100, N = 30000),
                             ruin_probability_vec1(n = 100, N = 30000),
                             ruin_probability_vec2(n = 100, N = 30000),
                             ruin_probability_cpp(n = 100, N = 30000),
                             ruin_probability(n = 100, N = 44000),
                             ruin_probability_vec1(n = 100, N = 44000),
                             ruin_probability_vec2(n = 100, N = 44000),
                             ruin_probability_cpp(n = 100, N = 44000),
                             ruin_probability(n = 100, N = 60000),
                             ruin_probability_vec1(n = 100, N = 60000),
                             ruin_probability_vec2(n = 100, N = 60000),
                             ruin_probability_cpp(n = 100, N = 60000))

cppvRrange %>% as_tibble() %>% mutate(N = case_when(str_detect(expr, "1000") == TRUE ~ 1000,
                                      str_detect(expr, "5000") == TRUE ~ 5000,
                                      str_detect(expr, "44000") == TRUE ~ 44000,
                                      str_detect(expr, "30000") == TRUE ~ 30000,
                                      str_detect(expr, "12000") == TRUE ~ 12000,
                                      str_detect(expr, "60000") == TRUE ~ 60000),
                                      Implementation = case_when(str_detect(expr, "vec2") == TRUE ~ "R vectorized",
                                                                 str_detect(expr, "vec1") == TRUE ~"R partly vectorized",
                                                                 str_detect(expr, "cpp") == TRUE ~ "Rcpp")) %>% 
                                      mutate(Implementation = ifelse(is.na(Implementation), "R for loop", Implementation),
                                             time = time/1000000000) %>% select(time, N, Implementation) %>% 
  group_by(N, Implementation) %>% summarise(time = median(time)) %>% 
  ggplot(aes(x = N, y = time, col = Implementation)) + geom_smooth(se = F) + theme(axis.text = element_text(size = 12, face="bold"),
                                                                                   axis.title = element_text(face = "bold", size = 16),
                                                                                   legend.text = element_text(face = "bold"),
                                                                                   legend.title = element_text(face = "bold", size = 15))
  


