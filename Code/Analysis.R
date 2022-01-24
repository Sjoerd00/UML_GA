source("SparsePCA.R")
load("FIFA2017_NL.RData")
set.seed(1)

X <- fifa %>% select(-c("name", "club", "Position", "eur_value", "eur_wage", "eur_release_clause"))
X <- scale(X)
X <- as.matrix(X)

test <- matrix_deflation(X, 3, sqrt(29), 10)

ma <- matrix(test[2], nrow = 29, ncol=3)



#Comparison

w_sparse <- SPC(X, sumabsv = sqrt(29), K = 3,
              center = FALSE, trace = FALSE, niter = 10, cnames = TRUE)
norm(w_sparse$v[,1], type = "2")
norm(test[[1]]$v, type = "2")


w_sparse$v
test[[1]]$v

SPC


