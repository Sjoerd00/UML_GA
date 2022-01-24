###############################################################################
#Purpose: Implement a Sparce PCA algo, using penalized matrix decomposition

#Version:
#       1 - Sparse rank-1 PCA
#       2 - Multifactor matrix decomposition



library(pacman)
p_load(tidyverse)

#############################
#soft_threshold: functiont to perform soft thresholding
#     input: a,         a numeric vector 
#            lambda,    the soft thresholding parameter

#     output: v,        post-transformation vector
soft_treshold <- function(a, lambda){
  return(sign(a)*(abs(a) - lambda > 0)*(abs(a) - lambda))
}

#update_v : function that updates the inputted vector, by applying the soft threshold
#             transformation and normalizing the vector
#     input: a,         a numeric vector 
#            lambda,    the soft thresholding parameter
#         
#     output: v,        post-transformation vector

update_v <- function(a, lambda){
  sv <- soft_treshold(a,lambda)
  v <- sv/norm(sv, type = "2")
  return(v)
}

#search_lambda: perform binary search on the values of lambda that optimize the vector v,
#                   given Xu (X%*%u)
#       inputs: Xu,  inner product of matrix to be approximated and the vector U (obtained from
#                  SVD)
#           c2:         the penalization, the closest to 1, the bigger the effect
#           lambda_min: the left limit of the binary search (should be <=0)
#           lambda_min: the left limit of the binary search (should be < max(abs(xu)))
#           epsilon:     approximation tolerance
#           verbose:     whether to print the limiting lambdas at each interation. Use for debbug 
#                       purposes

search_lambda_recursive <- function(Xu, c2, lambda_min, lambda_max, epsilon, verbose = FALSE){
  
  if(verbose == TRUE){ 
    print("lambda_min")
    print(lambda_min)
    print("lambda_max")
    print(lambda_max)
  }
  
  min_c2 <- norm(update_v(Xu, lambda_max), type = "1")
  max_c2 <- norm(update_v(Xu, lambda_min), type = "1")
  
  
  lambda_center <- (lambda_max+lambda_min)/2
  
  center <- update_v(Xu, lambda_center)
  
  v1 <- norm(center, type = "1")
  
  if(abs(v1 - c2) < epsilon){
    res <- list(center, lambda_center)
    return(res)
  }else if(v1 < c2){
    return(search_lambda(Xu, c2, lambda_min, lambda_center, epsilon))
  }else if(v1 > c2){
    return(search_lambda(Xu, c2, lambda_center, lambda_max, epsilon))
  }
}



#######

search_lambda <- function(Xu, c2, lambda_min, lambda_max, epsilon, verbose = FALSE, k = 150){
  
  if(verbose == TRUE){ 
    print("lambda_min")
    print(lambda_min)
    print("lambda_max")
    print(lambda_max)
  }
  
  min_c2 <- norm(update_v(Xu, lambda_max), type = "1")
  max_c2 <- norm(update_v(Xu, lambda_min), type = "1")
  
  
  lambda_center <- (lambda_max+lambda_min)/2
  
  center <- update_v(Xu, lambda_center)
  
  v1 <- norm(center, type = "1")
  
  if(abs(v1 - c2) < epsilon | k == 0){
    res <- list(center, lambda_center)
    return(res)
  }else if(v1 < c2){
    return(search_lambda(Xu, c2, lambda_min, lambda_center, epsilon, k = k-1))
  }else if(v1 > c2){
    return(search_lambda(Xu, c2, lambda_center, lambda_max, epsilon, k = k-1))
  }
}

#SparsePCA: algorithm to find the Sparse rank-1 PCA of a matrix
#       inputs:
#               X: the matrix to be approximated
#               c2: the cost parameter
#               M: number of iterations

SparsePCA <- function(X, c2, M){
  
  v <- svd(X, nu = 1, nv = 1)$v
  i = 0
  while(i<M){
    i = i + 1
    
    Xv <- X %*% v
    u <- (Xv)/norm(Xv, type="2")
    Xu <- t(X)%*%u
    v <- search_lambda(Xu, c2, 0, max(abs(Xu))-1e-5, 0.00001)[[1]]
    print(v)
  }
  sigma <- t(u)%*%X%*%v
  res <- list("u" = u, "v" =  v, "sigma" = sigma)
  return(res)
}

#Matrix_deflation: algorithm to apply the Sparse rank-1 PCA algorithm serially
#                  and obtain further PCs using the WTH algorithm
#       inputs:
#               X: the matrix to be approximated
#               rank: the rank of the approximation
#               c2: the cost parameter
#               M: number of iterations

matrix_deflation <- function(X, rank, c2, M){
  rank_obj <- vector("list", rank)
  
  R <- X
  mv <- NULL
  for(j in seq(rank)){
    rank_obj[[j]] <- SparsePCA(R, c2, M)
    v <- rank_obj[[j]]$v
    sigma <- rank_obj[[j]]$sigma
    u <- rank_obj[[j]]$u
    
    X_tilde <- sigma[1]*u%*%t(v)
    R <- R - X_tilde
    mv <- c(mv, v)
  }
  mv <- matrix(unlist(mv), ncol = rank, nrow = 29)
  ve <- variance_explained(X,mv)
  res <- list(rank_obj, mv, ve)
  return(res)
}

################################################################################


variance_explained <- function(X, V){
  K <- ncol(V)
  ve <- NULL
  for(k in 1:K){
  Vk <- matrix(V[,1:k], ncol=k)
  Xk <- X%*%Vk%*%solve(t(Vk)%*%Vk)%*%t(Vk)
  
  ve <- c(ve, sum(diag(t(Xk)%*%Xk)))}
  pve <- ve/sum(diag((t(X)%*%X)))
  return(pve)
}


