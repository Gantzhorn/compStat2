quant_func <- function(theta){
  force(theta)
  
  k <- (exp(2*theta)-exp(theta*(-1.9)))
  
  function(x){
    log(k * x + exp(theta * (-1.9))) / theta
  }
}

ruin_importance <- function(n, N, theta){
  Q_1theta <- quant_func(theta)
  
  G <- Q_1theta(runif(n*N))
  
  dim(G) <- c(n, N)
  
  w_star <- exp(-theta * colSums(G))
  
  w <- w_star/sum(w_star)
  
  I <- logical(N)
  
  for (i in 1:N){
    S <- 30 + cumsum(G[, i])
    I[i] <- any(S<0)
  }
  
  mu_IS <- cumsum(I*w_star)/cumsum(w_star)
  mu_IS
  c_bar <- mean(w_star)
  var_IS <- var(I*w_star)
  gamma <- cov(I*w_star, w_star)
  var_w <- var(w_star)
  
  var_T <-  c_bar^(-2)*(var_IS + mu_IS[N]*var_w-2*mu_IS[N]*gamma)/N
  confint_low <- mu_IS[N] - 1.96*var_T
  confint_high <- mu_IS[N] + 1.96*var_T
  out <- list(outcomes = I, p_est = mu_IS[N], mu_IS = mu_IS, var_T = var_T, confint_low = confint_low, confint_high = confint_high)
  out
}
