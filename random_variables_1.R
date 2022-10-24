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


h <- ggplot(data = as.data.frame(res), aes(x = seq_along(running_ruin_prob), y = running_ruin_prob)) +
  geom_line(size = 1, col = "firebrick") + ylim(0, 0.005)

h + geom_ribbon(
    mapping = aes(
      ymin = pmax(res$running_ruin_prob - 1.96 * sigma_hat / sqrt(1:res$number_of_sim),0), 
      ymax = pmin(res$running_ruin_prob + 1.96 * sigma_hat / sqrt(1:res$number_of_sim),1)
    ), fill = "firebrick", alpha = 0.2)

  
#--------------------------------------------------------------------------------------------------------#

# Importance sampling
q_IS <- function(theta = 0.2,  n = 100, N = 1000){
  val <- numeric(N)
  ruin <- numeric(N)
  for (i in 1:N){
  outcome <- capital_flow_calculator(n)[n]
  val <- outcome$y
  ruin <- 
  }
  
  # h_vec <- numeric(N)
  # for (i in 1:N){
  #   h_vec[i] <- h(n)
  # }
  g_theta_n <- numeric(N)
  norm <- numeric(N)
  for (i in 1:N){
  x <- runif(n, -1.9, 2)
  g_theta_n[i] <- exp(theta*sum(x))
  }
  
  
  norm <- dlnorm(g_theta_n, meanlog = 0, sdlog = 1)
  
  wstar <- g_theta_n/norm
  w <- wstar/sum(wstar)

  out <- sum(h_vec*w)
  out
}




