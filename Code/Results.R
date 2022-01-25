library(pacman)
p_load(tidyverse,
       fastDummies,
       PMA,
       UBbipl,
       dplyr,
       datawizard)
load("~/GitHub/UML_GA/Code/FIFA2017_NL.RData")

summary(fifa)

fifa$clubNR <- as.numeric(fifa$club)
df <- fifa %>% fastDummies::dummy_columns('Position')
df <- fifa %>% datawizard::demean("crossing", group="club")
X <- dplyr::select(df, -c(name, club, Position, eur_value, eur_wage, eur_release_clause)) %>% scale

# PCA
PCAobj <- prcomp(X, scale. = FALSE, rank=2)
summary(PCAobj)
plot(PCAobj, type='l')

# PCA on Correlation Matrix for kaiser rule
Xcorr <- cor(X)
PCAobj2 <- prcomp(Xcorr, 2)
summary(PCAobj2)
plot(PCAobj2, type='l')
  
# Rotation
rotmat  <- varimax(PCAobj$rotation[,1:2])$rotmat
PCAvarmax <- PCAobj$rotation[,1:2] %*% rotmat

rotmat2  <- varimax(PCAobj2$rotation[,1:2])$rotmat
PCAvarmax2 <- PCAobj2$rotation[,1:2] %*% rotmat2

# Sparse PCA
# add optimal Caio values
spc <- SPC(X, sumabsv=sqrt(dim(X)[2]), K=2)
spc$v

pmd <- PMD(X, sumabsu = 1, sumabsv = 1)
pmd

# Plots
plot(PCAbipl(X))
plot(PCAbipl(Xcorr[1:29,1:29]))
