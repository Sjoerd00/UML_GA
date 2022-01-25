library(pacman)
p_load(tidyverse,
       fastDummies,
       PMA,
       UBbipl,
       dplyr,
       datawizard,
       miceadds,
       car)
load("~/GitHub/UML_GA/Code/FIFA2017_NL.RData")

## Data
summary(fifa)

fifa$clubNR <- as.numeric(fifa$club)
df <- fifa %>% fastDummies::dummy_columns('Position')
X <- dplyr::select(df, -c(name, club, Position, eur_value, eur_wage, eur_release_clause)) %>% scale

dfDemean <- fifa %>% group_by(club) %>%
  mutate(across(n[4:35], ~ .x - mean(.x), .names = "{col}"))

X <- dplyr::select(as.data.frame(dfDemean), -c(name, club, Position, eur_value, eur_wage, eur_release_clause, clubNR)) %>% scale

## PCA
PCAobj <- prcomp(X, scale. = FALSE, rank=2)
summary(PCAobj)
plot(PCAobj, type='l')

## PCA on Correlation Matrix for kaiser rule
Xcorr <- cor(X)
PCAobj2 <- prcomp(Xcorr, 2)
summary(PCAobj2)
plot(PCAobj2, type='l')
  
## Rotation
rotmat  <- varimax(PCAobj$rotation[,1:2])$rotmat
PCAvarmax <- PCAobj$rotation[,1:2] %*% rotmat

rotmat2  <- varimax(PCAobj2$rotation[,1:2])$rotmat
PCAvarmax2 <- PCAobj2$rotation[,1:2] %*% rotmat2

## Sparse PCA
spc <- SPC(X, sumabsv=sqrt(dim(X)[2]), K=2)
spc$v

pmd <- PMD(X, sumabsu = sqrt(47), sumabsv = sqrt(47))
pmd$v *100

## PC of a player
playerScores <- X %*% PCAobj2$rotation[,1:2]
playerScores<-cbind(playerScores, df[,c("eur_value", "eur_wage")])
playerCorr <- cor(playerScores)

## PC of a team
clubMeans <- GroupMean(X, df$club)
clubDims <- as.matrix(clubMeans[,2:dim(clubMeans)[2]]) %*% PCAobj2$rotation[,1:2]


## Plots
plot(PCAbipl(X))
biplot(PCAobj2)

plot(-clubDims[,1], clubDims[,2], xlim=c(-1.5,1.5))
abline(v = 0, col = "black", lwd = 1)
abline(h = 0, col = "black")
text(-clubDims[,1], clubDims[,2], clubMeans$group, cex=0.6, pos=3, col="red")

