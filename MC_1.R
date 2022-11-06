#Monte Carlo integration:
#Simulate random variable
capital_flow_calculator <- function(n){
  X <- runif(n, -1.9, 2)
  S <- cumsum(X) + 30
  list(x = seq_along(S), y = S, h = any(S<0))
}

#Calculate probability of ruin
ruin_probability <- function(n = 100, N){
  ruin_vec <- numeric(N)
  for (i in 1:N){
    ruin_vec[i] <- capital_flow_calculator(n)$h
  }
  list(ruin_prob = mean(ruin_vec), number_of_ruins = sum(ruin_vec), running_ruin_prob = cumsum(ruin_vec)/(1:N), 
       number_of_sim = N, outcomes = ruin_vec)
}

#Vectorized version
ruin_probability_vec1 <- function(n = 100, N){
  X <- runif(n*N, min = -1.9, max = 2)
  dim(X) <- c(n, N)
  S <- apply(X, 1, function(x){cumsum(x)}) + 30
  #I <- sapply(S, function(x){any(x<0)})
  I <- ifelse(S < 0, TRUE, FALSE)
  ruin_vec <- colSums(I)>0
  list(ruin_prob = mean(ruin_vec), number_of_ruins = sum(ruin_vec), running_ruin_prob = cumsum(ruin_vec)/(1:N), 
       number_of_sim = N, outcomes = ruin_vec)
}

ruin_probability_vec2 <- function(n = 100, N){
  X <- runif(n*N, min = -1.9, max = 2)
  dim(X) <- c(n, N)
  S <- riskRegression::colCumSum(X) + 30
  I <- ifelse(S < 0, TRUE, FALSE)
  ruin_vec <- colSums(I)>0
  list(ruin_prob = mean(ruin_vec), number_of_ruins = sum(ruin_vec), running_ruin_prob = cumsum(ruin_vec)/(1:N), 
       number_of_sim = N, outcomes = ruin_vec)#, path = S)
}

ruin_probability(n = 100, N = 10000)

ruin_probability_vec1(n = 100, N = 10000)

ruin_probability_vec2(n = 100, N = 10000)

# Check that it works
# resvec2 <- ruin_probability_vec2(n = 100, N = 100)
# p2 <- resvec2$path %>% as_tibble() %>%  mutate(n=row_number()) %>% pivot_longer(-n)
# p2 %>% ggplot(aes(x = n, y = value, group = name)) + geom_line(size = 0.3) + geom_hline(yintercept = 0, col = "red", linetype = "dashed", size = 1.5) +
#   xlab("n") + ylab(expression(S[n])) + 
#   theme(axis.title = element_text(size = 20)) + theme(axis.text = element_text(size = 14))

  