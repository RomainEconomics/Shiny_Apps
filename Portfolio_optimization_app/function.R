library(tidyverse)

# All the functions come from the package IntroCompFinR and may have only be 
# slightly modified
# (https://rdrr.io/rforge/IntroCompFinR/)

expected_return <- function(Return_assets, weights) {
  
  er <- as.vector(crossprod(Return_assets,weights))
  
  return(er)
}
expected_sd <- function(Cov_mat, weights) {
  
  sd_tangency_pf <- as.vector(sqrt(t(weights) %*% Cov_mat %*% weights))
  
}

tangency_portfolio <- function(Return_assets, Cov_mat_inv, Rf = 0.005) {
  
  asset_names <- names(Return_assets)
  
  w_t <- Cov_mat_inv %*% (Return_assets - Rf) # tangency portfolio
  w_t <- as.vector(w_t/sum(w_t))   # normalize weights
  names(w_t) <- asset_names
  
  return(w_t)
}

global_min_portfolio <- function(Return_assets, Cov_mat) {
  
  N <- length(Return_assets)
  asset_names <- names(Return_assets)
  
  cov_mat_inv <- solve(Cov_mat)
  # Calculate Global Min Portfolio
  # cov_mat_inv  # cov.mat
  one_vec <- rep(1,N)
  w_gmin <- rowSums(cov_mat_inv) / sum(cov_mat_inv)
  w_gmin <- as.vector(w_gmin)
  
  names(w_gmin) <- names(Return_assets)
  return(w_gmin)
}

efficient_portfolio <- function(Return_assets, Cov_mat, target_return = 0.1) {
  
  N <- length(Return_assets)
  
  one_vec <- rep(1, N)
  top <- cbind(2*Cov_mat, Return_assets, one_vec)
  bot <- cbind(rbind(Return_assets, one_vec), matrix(0,2,2))
  A <- rbind(top, bot)
  b_target <- as.matrix(c(rep(0, N), target_return, 1))
  x <- solve(A, b_target)
  w <- x[1:N]
  names(w) <- names(Return_assets)
  
  return(w)
  
}

efficient_frontier <- function(Return_assets, Cov_mat, nport = 40) {
  
  alpha.min <- -2
  alpha.max <-  1.5
  
  w_gmin <- global_min_portfolio(Return_assets, Cov_mat)
  
  # Calculate portfolio frontier
  port_names <- rep("port",nport)
  ns <- seq(1,nport)
  port_names <- paste(port_names,ns)
  
  # compute efficient frontier as convex combinations of two efficient portfolios
  # 1st efficient port: global min var portfolio
  # 2nd efficient port: min var port with ER = max of ER for all assets
  Ra_max <- max(Return_assets)
  port_max <- efficient_portfolio(Return_assets,Cov_mat,Ra_max)
  w_max <- port_max
  a <- seq(from=alpha.min,to=alpha.max,length=nport) # convex combinations
  we_mat <- a %o% w_gmin + (1-a) %o% w_max	         # rows are efficient portfolios
  er_e <- we_mat %*% Return_assets							     # expected returns of efficient portfolios
  er_e <- as.vector(er_e)
  
  
  asset_names <- names(Return_assets)
  names(er_e) <- port_names
  cov_e <- we_mat %*% Cov_mat %*% t(we_mat) # cov mat of efficient portfolios
  sd_e <- sqrt(diag(cov_e))					        # std devs of efficient portfolios
  sd_e <- as.vector(sd_e)
  names(sd_e) <- port_names
  dimnames(we_mat) <- list(port_names,asset_names)
  
  res <- tibble(er_e = er_e, sd_e = sd_e)
  return(res)
}

