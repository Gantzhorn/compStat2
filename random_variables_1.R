library(tidyverse)
theme_set(theme_bw())
library(microbenchmark)
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
  out <- list(p_est = mu_IS[m], mu_IS = mu_IS, var_T = var_T, confint_low = confint_low, confint_high = confint_high)
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
  geom_line(size = 1)

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
  geom_line(size = 1)

#Optimal theta
tibble(theta = thetasf, as_tibble(var_matf)) %>%
  pivot_longer(cols = -theta, names_to = "N", values_to = "Variance") %>%
  group_by(N) %>%
  slice_min(Variance)
s