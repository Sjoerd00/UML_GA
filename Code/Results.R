library(pacman)
p_load(tidyverse,
       fastDummies)
load("~/GitHub/UML_GA/Code/FIFA2017_NL.RData")

df <- fifa %>% fastDummies::dummy_columns('Position')
df <- select(df, -c(''))