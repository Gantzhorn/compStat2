library(tidyverse)
theme_set(theme_bw())
library(microbenchmark)
library(bench)
library(profvis)

#Monte Carlo integration:
#Simulate random variable
capital_flow_calculator <- function(n){
  X <- runif(n, -1.9, 2)
  S <- cumsum(X) + 30
  list(x = seq_along(S), y = S, h = any(S<0))
}

h <- function(n){
  X <- capital_flow_calculator(n)
  X$h
}

#Check that it works 
# a <- capital_flow_calculator(100)
# ggplot(data = as.data.frame(a), aes(x = x, y = y)) + geom_line(size = 1, col = "firebrick")


#Calculate probability of ruin
ruin_probability <- function(n = 100, N = 10000){
  ruin_vec <- numeric(N)
  for (i in 1:N){
    #X <- capital_flow_calculator(n)
    ruin_vec[i] <- h(n)
  }
  list(ruin_prob = mean(ruin_vec), number_of_ruins = sum(ruin_vec), running_ruin_prob = cumsum(ruin_vec)/(1:N), 
       number_of_sim = N, outcomes = ruin_vec)
}

res <- ruin_probability(N = 100000)

#Get confidence-interval for the estimator and plot

sigma_hat <- sd(res$outcomes)


p1 <- ggplot(data = as.data.frame(res), aes(x = seq_along(running_ruin_prob), y = running_ruin_prob)) +
  geom_line(size = 1, col = "firebrick") + ylim(0, 0.005)

p1 + geom_ribbon(
    mapping = aes(
      ymin = pmax(res$running_ruin_prob - 1.96 * sigma_hat / sqrt(1:res$number_of_sim),0), 
      ymax = pmin(res$running_ruin_prob + 1.96 * sigma_hat / sqrt(1:res$number_of_sim),1)
    ), fill = "firebrick", alpha = 0.2)

  
#--------------------------------------------------------------------------------------------------------#
quant_func <- function(theta){
  force(theta)
  
  k <- (exp(2*theta)-exp(theta*(-1.9)))
  
  function(x){
  log(k * x + exp(theta * (-1.9))) / theta
  }
}

ruin_importance <- function(n, m, theta){
  Q_1theta <- quant_func(theta)
  
  G <- Q_1theta(runif(n*m))

  dim(G) <- c(n, m)
  
  w_star <- exp(-theta * colSums(G))
  
  w <- w_star/sum(w_star)
  
  I <- logical(m)
  
  for (i in 1:m){
    S <- 30 + cumsum(G[, i])
    I[i] <- any(S<0)
  }
  
  mu_IS <- cumsum(I*w_star)/cumsum(w_star)
  mu_IS
  c_bar <- mean(w_star)
  var_IS <- var(I*w_star)
  gamma <- cov(I*w_star, w_star)
  var_w <- var(w_star)
  
  var_T <-  c_bar^(-2)*(var_IS + mu_IS[m]*var_w-2*mu_IS[m]*gamma)/m
  confint_low <- mu_IS[m] - 1.96*var_T
  confint_high <- mu_IS[m] + 1.96*var_T
  out <- list(outcomes = I, p_est = mu_IS[m], mu_IS = mu_IS, var_T = var_T, confint_low = confint_low, confint_high = confint_high)
  out
}

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
  geom_point() + geom_smooth(se = F)

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
  geom_point(size = 1) + geom_smooth(se = F)

#Optimal theta
theta_opt <- tibble(theta = thetasf, as_tibble(var_matf)) %>%
  pivot_longer(cols = -theta, names_to = "N", values_to = "Variance") %>%
  group_by(N) %>%
  slice_min(Variance) %>% 
  ungroup() %>% 
  summarise(theta_opt = mean(theta)) %>% pull()

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
