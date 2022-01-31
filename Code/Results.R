library(pacman)
p_load(tidyverse, fastDummies,PMA,UBbipl,dplyr,miceadds,car,scales,ggplot2,reshape,
       doBy,weights,forcats
)
load("~/GitHub/UML_GA/Code/FIFA2017_NL.RData")

frac_var <- function(x)
  x ^ 2 / sum(x ^ 2)

## Data
summary(fifa)
summaryBy(value ~ custid,
          data = mydf,
          FUN = list(mean, max, min, median, sd))

fifa$clubNR <- as.numeric(fifa$club)
df <- fifa %>% fastDummies::dummy_columns('Position')
X <- dplyr::select(
  df,
  -c(name,club,Position,eur_value,eur_wage,eur_release_clause,Position_FW,
    Position_Mid, Position_Def, Position_Gk, clubNR
  )
) %>% scale

## PCA on Correlation Matrix for kaiser rule
Xcorr <- cor(X)
PCAobj2 <- prcomp(Xcorr, 2)
summary(PCAobj2)
plot(PCAobj2, type = 'l')

## Rotation
rotmat  <- varimax(PCAobj$rotation[, 1:2])$rotmat
PCAvarmax <- PCAobj$rotation[, 1:2] %*% rotmat

rotmat2  <- varimax(PCAobj2$rotation[, 1:2])$rotmat
PCAvarmax2 <- PCAobj2$rotation[, 1:2] %*% rotmat2

## Sparse PCA
# how to show vairance explained?
spc <- SPC(X, sumabsv = sqrt(dim(X)[2]), K = 9)

## PC of a team
# calculation wage proportion
salarySum <- GroupSum(df$eur_wage, df$club)
salarySum <- left_join(df, salarySum, by = c("club" = "group"))
salarySum$propWage <- salarySum$eur_wage / salarySum$data1

clubSum <- GroupSum(X * salarySum$propWage , df$club)
clubMeans <- GroupMean(X , df$club)

# calculation principle component score weighted average and regular average
clubDimsSum <-
  as.matrix(clubSum[, 2:dim(clubSum)[2]]) %*% PCAobj2$rotation[, 1:2]
clubDimsAvg <-
  as.matrix(clubMeans[, 2:dim(clubMeans)[2]]) %*% PCAobj2$rotation[, 1:2]

## PC of a player
# calculation of principle component score player
playerScores <- X %*% PCAobj2$rotation[, 1:2]
playerScores[,1] <- -playerScores[, 1]
playerScores <- cbind(playerScores, df[, c("club",
                                           "Position_Mid",
                                           "Position_Def",
                                           "Position_Gk")], 
                      scale(df[, c("eur_value")]))
playerCorr <- cor(playerScores[1:5])
# principle component regression PCA based
summary(lm(eur_value ~ . , playerScores))

## PC player Sparse
# calculation of sparse principle component score player
sparsePCscore <- -X %*% spc$v[,1:2]
sparsePCscore <- cbind(sparsePCscore, df[, c("club",
                                             "Position_Mid",
                                             "Position_Def",
                                             "Position_Gk")],
                       scale(df[, c("eur_value")]))
playerCorrSparse <- cor(sparsePCscore[1:2])
# principle component regression SPCA based
summary(lm(eur_value ~ . , sparsePCscore))

## Plots
# Scree Sparse PCA
c(spc$prop.var.explained[1], diff(spc$prop.var.explained, 1)) %>% as_tibble() %>%
  mutate(Comp = colnames(PCAobj2$x)[1:9]) %>%
  ggplot(aes(x = Comp, y = value)) +
  geom_bar(stat = "identity", fill = "#4DC5F9") +
  geom_hline(yintercept = 0.1, linetype = 2) +
  xlab("Principal Components") +
  scale_y_continuous(
    name = "Variance Explained",
    breaks = seq(0, 0.8, 0.1),
    labels = percent_format(accuracy = 5L)
  ) +
  theme_classic(base_size = 14)

# Scree PCA
PCAobj2$sdev %>%
  as_tibble() %>%
  frac_var() %>%
  mutate(Comp = colnames(PCAobj2$x)) %>%
  slice(1:9) %>%
  ggplot(aes(x = Comp, y = value)) +
  geom_bar(stat = "identity", fill = "#4DC5F9") +
  geom_hline(yintercept = 0.1, linetype = 2) +
  xlab("Principal Components") +
  scale_y_continuous(
    name = "Variance Explained",
    breaks = seq(0, 0.8, 0.1),
    labels = percent_format(accuracy = 5L)
  ) +
  theme_classic(base_size = 14)

# Loadings Heatmaps
rownamesHeat1 <- c("crossing","finishing","heading accuracy","short_passing","volleys",
                   "dribbling","curve","free kick accuracy", "long passing", "ball_control",
                   "acceleration","sprint speed", "agility", "reactions", "balance", "shot_power", "jumping",
                   "stamina", "strength", "long_shots", "aggression", "interceptions", "positioning","vision",
                   "penalties", "composure","marking","standing tackle", "sliding tackle" )
PCAvarmax2s <- PCAobj2$rotation[, 1:2][order(-PCAobj2$rotation[, 1]), ]
colnames(PCAvarmax2s) <- c("PC Offensive", "PC Defensive")

t <- PCAvarmax2s %>% as.data.frame %>% rownames_to_column() %>% melt

ggplot(t, aes(variable, rowname, fill = value)) +
  aes(y = fct_inorder(rowname)) +
  scale_fill_gradient2(low = "darkblue", high = "darkgreen", guide = "colorbar") +
  theme_grey(base_size = 22)+
  geom_tile() +
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())

SparseLoadings <-
  spc$v[, 1:2] %>% as.data.frame(row.names = rownames(PCAvarmax2s))
SparseLoadings <- SparseLoadings[order(spc$v[, 1]),]
t2 <- SparseLoadings %>% rownames_to_column() %>% melt

ggplot(t2, aes(variable, rowname, fill = value)) +
  aes(y = fct_inorder(rowname)) +
  scale_fill_gradient2(low = "darkblue", high = "darkgreen", guide = "colorbar") +
  theme_grey(base_size = 22)+
  geom_tile()+
  theme(axis.title.x=element_blank(),
        axis.title.y=element_blank())


### Team Plot
# plot wage-weighted average teams 
plot(
  -clubDimsSum[, 1],
  clubDimsSum[, 2],
  xlim = c(-2.5, 2.5),
  ylim = c(-2.5, 2.5),
  xlab = "Offensive Principle Component",
  ylab = "Defensive Principle Component"
)
text(
  -clubDimsSum[, 1],
  clubDimsSum[, 2],
  clubMeans$group,
  cex = 0.6,
  pos = 3,
  col = "red"
)
title(main = "Team Scores on Offensive and Defensive Principle Components")

# Plot regular average teams
plot(
  -clubDimsAvg[, 1],
  clubDimsAvg[, 2],
  xlim = c(-2.5, 2.5),
  ylim = c(-1.5, 1.5),
  xlab = "Offensive Principle Component",
  ylab = "Defensive Principle Component"
)
text(
  -clubDimsAvg[, 1],
  clubDimsAvg[, 2],
  clubSum$group,
  cex = 0.6,
  pos = 3,
  col = "red"
)
title(main = "Team Scores on Offensive and Defensive Principle Components")
