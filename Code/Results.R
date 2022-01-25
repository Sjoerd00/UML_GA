library(pacman)
p_load(tidyverse,
       fastDummies,
       PMA,
       UBbipl)
load("~/GitHub/UML_GA/Code/FIFA2017_NL.RData")

df <- fifa %>% fastDummies::dummy_columns('Position')
X <- select(df, -c(name, club, Position, eur_value, eur_wage, eur_release_clause)) %>% scale()


# PCA
PCAobj <- prcomp(X)
summary(PCAobj)
plot(PCAobj, type='l')

# PCA on Correlation Martrix
Xcorr <- cor(X)
PCAobj2 <- prcomp(Xcorr, rank)
summary(PCAobj2)
plot(PCAobj2, type='l')
  
# Rotation
rotmat  <- varimax(PCAobj$rotation[,1:5])$rotmat
PCAvarmax <- PCAobj$rotation[,1:5] %*% rotmat

rotmat2  <- varimax(PCAobj2$rotation[,1:2])$rotmat
PCAvarmax2 <- PCAobj2$rotation[,1:2] %*% rotmat2

# Sparse PCA
spc <- SPC(X, sumabsv=sqrt(33), K=5)
spc$v

# Plots
plot(PCAbipl(X[,1:5]))
plot(PCAbipl(Xcorr[1:29,1:29]))
