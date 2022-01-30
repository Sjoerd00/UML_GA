library(pacman)
p_load(tidyverse,
       fastDummies,
       PMA,
       UBbipl,
       dplyr,
       datawizard,
       miceadds,
       car,
       scales,
       ggplot2,
       reshape,
       doBy,
       weights)
load("~/GitHub/UML_GA/Code/FIFA2017_NL.RData")

## Data
summary(fifa)
summaryBy(value ~ custid, data = mydf, 
          FUN = list(mean, max, min, median, sd))

n <- names(fifa)

fifa$clubNR <- as.numeric(fifa$club)
df <- fifa %>% fastDummies::dummy_columns('Position')
X <- dplyr::select(df, -c(name, club, Position, eur_value, eur_wage,
                          eur_release_clause,  Position_FW,
                          Position_Mid,Position_Def,Position_Gk, clubNR)) %>% scale

# dfDemean <- fifa %>% group_by(Position) %>% 
#   mutate(across(n[4:35], ~ .x - mean(.x), .names = "{col}"))
# 
# X <- dplyr::select(as.data.frame(dfDemean), -c(name, club, Position, eur_value, 
#                                                eur_wage, eur_release_clause, clubNR)) %>% scale

## PCA
PCAobj <- prcomp(X, scale. = FALSE, rank=3)
summary(PCAobj)
plot(PCAobj, type='l')

## PCA on Correlation Matrix for kaiser rule
Xcorr <- cor(X)
PCAobj2 <- prcomp(Xcorr, 3)
summary(PCAobj2)
plot(PCAobj2, type='l')
  
## Rotation
rotmat  <- varimax(PCAobj$rotation[,1:5])$rotmat
PCAvarmax <- PCAobj$rotation[,1:5] %*% rotmat

rotmat2  <- varimax(PCAobj2$rotation[,1:5])$rotmat
PCAvarmax2 <- PCAobj2$rotation[,1:5] %*% rotmat2

## Sparse PCA
spc <- SPC(X, sumabsv=sqrt(dim(X)[2]), K=2)

## PC of a team
salarySum <- GroupSum(df$eur_wage, df$club)
salarySum <- left_join(df, salarySum, by = c("club"="group"))
salarySum$propWage <- salarySum$eur_wage / salarySum$data1

clubSum <- GroupSum(X*salarySum$propWage , df$club)
clubMeans <- GroupMean(X , df$club)

clubDimsSum <- as.matrix(clubSum[,2:dim(clubSum)[2]]) %*% PCAobj2$rotation[,1:2]
clubDimsAvg <- as.matrix(clubMeans[,2:dim(clubMeans)[2]]) %*% PCAobj2$rotation[,1:2]

## PC of a player
df[,c("eur_wage_sc")] <- scale(df[,c("eur_wage")])
playerScores <- X %*% PCAobj2$rotation[,1:2]
# playerScores <- playerScores[, 1:5]
playerScores<-cbind(playerScores, df[,c( "eur_wage_sc", "club", 
                                         "Position_Mid","Position_Def","Position_Gk")])
playerCorr <- cor(playerScores[1:5])
summary(lm(eur_wage_sc~. , playerScores))

## PC player Sparse
sparsePCscore <- X %*% spc$v
sparsePCscore<-cbind(sparsePCscore, df[,c( "eur_wage_sc", "club", 
                                         "Position_Mid","Position_Def","Position_Gk")])
playerCorrSparse <- cor(sparsePCscore[1:2])
summary(lm(eur_wage_sc~. , sparsePCscore))

## Plots

frac_var <- function(x) x^2/sum(x^2)
# Scree
PCAobj2$sdev %>% 
  as_tibble() %>% 
  frac_var() %>% 
  mutate(Comp = colnames(PCAobj2$x)) %>% 
  slice(1:9) %>% 
  ggplot(aes(x=Comp, y = value)) + 
  geom_bar(stat = "identity", fill = "#4DC5F9") +
  geom_hline(yintercept = 0.03, linetype=2) +
  xlab("Principal Components") +
  scale_y_continuous(name = "Variance Explained", breaks = seq(0,0.8,0.1), labels = percent_format(accuracy = 5L)) +
  theme_classic(base_size = 14)

# Loadings Heatmap
PCAvarmax2<- PCAvarmax2[order(-PCAvarmax2[,1]),]
t<-PCAvarmax2 %>% as.data.frame %>% rownames_to_column() %>% melt

ggplot(t, aes(variable, rowname, fill= value)) + 
  scale_fill_gradient2(low="darkblue", high="darkgreen", guide="colorbar") +
  geom_tile()

# heatmap for PCA loadings
# hmap_t2_t0_long <- hmap_t2_t0 %>%
#   rownames_to_column(var = "Gene") %>%
#   gather(Sample_ID, VST, -Gene) %>% 
#   full_join(md, by = "Sample_ID")  
# 
# hmap_t2_t0_long <- hmap_t2_t0 %>%
#   rownames_to_column(var = "Gene") %>%
#   gather(Sample_ID, VST, -Gene) %>% 
#   full_join(md, by = "Sample_ID")  
# 
# vst_pca_all <- vst_pca$x %>%
#   as.data.frame() %>%
#   rownames_to_column(var = "Sample_ID") %>%
#   full_join(md, by = "Sample_ID")
# 
# hmap_t2_t0_long$Sample_Name <- factor(hmap_t2_t0_long$Sample_Name, levels = 
#                                         c("t0_A","t0_B","t0_C","t2_A","t2_B","t2_C","t6_A","t6_B","t6_C","t24_A","t24_B","t24_C"))
# hmap_t2_t0_long$Time <- factor(hmap_t2_t0_long$Time, levels = c("t0","t2","t6","t24"))
# hmap_t2_t0_long$Replicate <- factor(hmap_t2_t0_long$Replicate, levels = c("A","B","C"))
# hmap_t2_t0_long$Gene <- factor(hmap_t2_t0_long$Gene, levels = row.names(hmap_t2_t0))
# 
# PCAvarmax2$Gene <- factor(hmap_t2_t0_long$Gene, levels = row.names(hmap_t2_t0))
# ggplot(as.data.frame(PCAvarmax2)) +
#   geom_tile(aes(x = Sample_Name, y = Gene, fill = VST)) +
#   scale_fill_gradientn(colours = rainbow(5)) +
#   scale_x_discrete(limits = c("V1","V2")) +
#   theme(axis.text.y = element_blank(), axis.ticks = element_blank()) 


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
plot(-clubDimsSum[,1], clubDimsSum[,2], xlim=c(-2.5,2.5), ylim=c(-1.5,1.5),
     xlab="Offensive Principle Component", ylab="Defensive Principle Component")
# abline(v = 0, col = "black", lwd = 1, lty=2)
# abline(h = 0, col = "black", lwd = 1, lty=2)
text(-clubDimsSum[,1], clubDimsSum[,2], clubMeans$group, cex=0.6, pos=3, col="red")
title(main="Team Scores on Offensive and Defensive Principle Components")

plot(-clubDimsAvg[,1], clubDimsAvg[,2], xlim=c(-2.5,2.5), ylim=c(-1.5,1.5),
     xlab="Offensive Principle Component", ylab="Defensive Principle Component")
# abline(v = 0, col = "black", lwd = 1, lty=2)
# abline(h = 0, col = "black", lwd = 1, lty=2)
text(-clubDimsAvg[,1], clubDimsAvg[,2], clubSum$group, cex=0.6, pos=3, col="red")
title(main="Team Scores on Offensive and Defensive Principle Components")
