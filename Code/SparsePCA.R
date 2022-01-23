library(pacman)
p_load(tidyverse)

soft_treshold <- function(a, lambda){
  return(sign(a)*(abs(a) - lambda > 0)*(abs(a) - lambda))
}

update_v <- function(a, lambda){
  sv <- soft_treshold(a,lambda)
  v <- sv/norm(sv, type = "2")
  return(v)
}

search_lambda <- function(Xv, c2, lambda_min, lambda_max, epsilon, verbose = FALSE){
    
  if(verbose == TRUE){ 
  print("lambda_min")
  print(lambda_min)
  print("lambda_max")
  print(lambda_max)
  }
  
  min_c2 <- norm(update_v(Xu, lambda_max), type = "1")
  max_c2 <- norm(update_v(Xu, lambda_min), type = "1")
  
  if(c2 < min_c2 | c2 > max_c2){
    return("c2 not feasible")
  }
  
  
  lambda_center <- (lambda_max+lambda_min)/2
  
  center <- update_v(Xu, lambda_center)
  
  v1 <- norm(center, type = "1")

  if(abs(v1 - c2) < epsilon){
    return(list(v, lambda_center))
  }else if(v1 < c2){
    return(search_lambda(Xu, c2, lambda_min, lambda_center, epsilon))
  }else if(v1 > c2){
    return(search_lambda(Xu, c2, lambda_center, lambda_max, epsilon))
  }
}


X <- fifa %>% select(-c("name", "club", "Position", "eur_value", "eur_wage", "eur_release_clause"))
X <- scale(X)
X <- as.matrix(X)



SparsePCA <- function(X, c2, M){
  
v <- svd(X, nu = 1, nv = 1)$v
i = 0
while(i<M){
  i = i + 1
  
  Xv <- X %*% v
  u <- (Xv)/norm(Xv, type="2")
  Xu <- t(X)%*%u
  v <- search_lambda(Xu, c2, 1, sqrt(dim(X)[2]), 0.00001)[[1]]
}
sigma <- t(u)%*%X%*%v
res <- list("u" = u, "v" =  v, "sigma" = sigma)
return(res)
}


matrix_deflation <- function(X, rank, c2, M){
  rank_obj <- vector("list", rank)
  
  R <- X
  for(i in seq(rank)){
    rank_obj[[i]] <- SparsePCA(R, c2, M)
    v <- rank_obj[[i]]$v
    sigma <- rank_obj[[i]]$sigma
    u <- rank_obj[[i]]$u
    
    X_tilde <- sigma[1]*u%*%t(v)
    R <- R - X_tilde
  }
  return(rank_obj)
}

test <- matrix_deflation(X, 10, 5, 10)

test
