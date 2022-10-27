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
# g_1theta <- function(theta = 0.1, x){
#   exp(theta*x)*theta/(exp(2*theta)-exp(-1.9*theta))
# }

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
  
  out <- cumsum(I*w_star)/cumsum(w_star)
  out
}








theta1 <- 0.5

test <- quant_func(theta1)

invtest <- function(x, theta = theta1 ){
  (exp(theta*x)-exp(-1.9*theta))/(exp(2*theta)-exp(theta*(-1.9)))
}

