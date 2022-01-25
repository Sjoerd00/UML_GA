library(pacman)
p_load(tidyverse,
       fastDummies,
       PMA,
       UBbipl)
load("~/GitHub/UML_GA/Code/FIFA2017_NL.RData")

df <- fifa %>% fastDummies::dummy_columns('Position')
X <- select(df, -c(name, club, Position, eur_value, eur_wage, eur_release_clause)) %>% scale()


# PCA
PCAobj <- prcomp(X, rank=5)
PCAbipl(X)
summary(PCAobj)

# Sparse PCA
varimax(X)
biplot(X)

spc <- SPC(X, sumabsv=sqrt(33), K=5)
spc$v

# Plots
