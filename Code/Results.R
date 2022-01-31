library(pacman)
p_load(tidyverse, fastDummies,PMA,UBbipl,dplyr,miceadds,car,scales,ggplot2,reshape,
       doBy,weights,forcats
)
load("~/GitHub/UML_GA/Code/FIFA2017_NL.RData")

## Data
summary(fifa)
summaryBy(value ~ custid,
          data = mydf,
          FUN = list(mean, max, min, median, sd))

fifa$clubNR <- as.numeric(fifa$club)
df <- fifa %>% fastDummies::dummy_columns('Position')
X <- dplyr::select(
  df,
  -c(
    name,
    club,
    Position,
    eur_value,
    eur_wage,
    eur_release_clause,
    Position_FW,
    Position_Mid,
    Position_Def,
    Position_Gk,
    clubNR,
  )
) %>% scale

## PCA
PCAobj <- prcomp(X, scale. = FALSE, rank = 2)
summary(PCAobj)
plot(PCAobj, type = 'l')

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
spc <- SPC(Xcorr, sumabsv = sqrt(dim(X)[2]), K = 2)
summary(spc)

## PC of a team
salarySum <- GroupSum(df$eur_wage, df$club)
salarySum <- left_join(df, salarySum, by = c("club" = "group"))
salarySum$propWage <- salarySum$eur_wage / salarySum$data1

clubSum <- GroupSum(X * salarySum$propWage , df$club)
clubMeans <- GroupMean(X , df$club)

clubDimsSum <-
  as.matrix(clubSum[, 2:dim(clubSum)[2]]) %*% PCAobj2$rotation[, 1:2]
clubDimsAvg <-
  as.matrix(clubMeans[, 2:dim(clubMeans)[2]]) %*% PCAobj2$rotation[, 1:2]

## PC of a player
playerScores <- X %*% PCAobj2$rotation[, 1:2]
# playerScores <- playerScores[, 1:5]
playerScores <- cbind(playerScores, df[, c("club",
                                           "Position_Mid",
                                           "Position_Def",
                                           "Position_Gk")], 
                      scale(df[, c("eur_value")]))
playerCorr <- cor(playerScores[1:5])
summary(lm(eur_value ~ . , playerScores))

## PC player Sparse
sparsePCscore <- X %*% spc$v
sparsePCscore <- cbind(sparsePCscore, df[, c("club",
                                             "Position_Mid",
                                             "Position_Def",
                                             "Position_Gk")],
                       scale(df[, c("eur_value")]))
playerCorrSparse <- cor(sparsePCscore[1:2])
summary(lm(eur_value ~ . , sparsePCscore))

## Plots

frac_var <- function(x)
  x ^ 2 / sum(x ^ 2)
# Scree
PCAobj2$sdev %>%
  as_tibble() %>%
  frac_var() %>%
  mutate(Comp = colnames(PCAobj2$x)) %>%
  slice(1:9) %>%
  ggplot(aes(x = Comp, y = value)) +
  geom_bar(stat = "identity", fill = "#4DC5F9") +
  geom_hline(yintercept = 0.03, linetype = 2) +
  xlab("Principal Components") +
  scale_y_continuous(
    name = "Variance Explained",
    breaks = seq(0, 0.8, 0.1),
    labels = percent_format(accuracy = 5L)
  ) +
  theme_classic(base_size = 14)

# Loadings Heatmap
PCAvarmax2s <- PCAvarmax2[order(-PCAvarmax2[, 1]), ]
t <- PCAvarmax2s %>% as.data.frame %>% rownames_to_column() %>% melt

ggplot(t, aes(variable, rowname, fill = value)) +
  aes(y = fct_inorder(rowname)) +
  scale_fill_gradient2(low = "darkblue", high = "darkgreen", guide = "colorbar") +
  geom_tile()

SparseLoadings <-
  spc$v %>% as.data.frame(row.names = rownames(PCAvarmax2))
SparseLoadings <- SparseLoadings[order(spc$v[, 1]),]
t2 <- SparseLoadings %>% rownames_to_column() %>% melt

ggplot(t2, aes(variable, rowname, fill = value)) +
  aes(y = fct_inorder(rowname)) +
  scale_fill_gradient2(low = "darkblue", high = "darkgreen", guide = "colorbar") +
  geom_tile()



# BiPlot
# scale=500
# ggplot(data=vst_pca_all, mapping=aes(x=PC1, y=PC2)) +
#   geom_point(size = 3, aes(shape = Replicate, color = Time)) +
#   geom_vline(xintercept = 0, linetype=2) +
#   geom_hline(yintercept = 0, linetype=2) +
#   geom_segment(data=genes.selected, mapping=aes(xend=scale*PC1,yend=scale*PC2), x=0, y=0, arrow=arrow(), color="grey") +
#   geom_label(data=genes.selected, mapping=aes(x=scale*PC1,y=scale*PC2, label=Gene_ID), size=2, hjust="outward", vjust="outward") +
#   theme_bw() +
#   theme(panel.grid.major = element_blank(),  panel.grid.minor = element_blank())


# Team Plot
plot(
  -clubDimsSum[, 1],
  clubDimsSum[, 2],
  xlim = c(-2.5, 2.5),
  ylim = c(-2.5, 2.5),
  xlab = "Offensive Principle Component",
  ylab = "Defensive Principle Component"
)
# abline(v = 0, col = "black", lwd = 1, lty=2)
# abline(h = 0, col = "black", lwd = 1, lty=2)
text(
  -clubDimsSum[, 1],
  clubDimsSum[, 2],
  clubMeans$group,
  cex = 0.6,
  pos = 3,
  col = "red"
)
title(main = "Team Scores on Offensive and Defensive Principle Components")

plot(
  -clubDimsAvg[, 1],
  clubDimsAvg[, 2],
  xlim = c(-2.5, 2.5),
  ylim = c(-1.5, 1.5),
  xlab = "Offensive Principle Component",
  ylab = "Defensive Principle Component"
)
# abline(v = 0, col = "black", lwd = 1, lty=2)
# abline(h = 0, col = "black", lwd = 1, lty=2)
text(
  -clubDimsAvg[, 1],
  clubDimsAvg[, 2],
  clubSum$group,
  cex = 0.6,
  pos = 3,
  col = "red"
)
title(main = "Team Scores on Offensive and Defensive Principle Components")
