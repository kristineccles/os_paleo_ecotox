###########################################################
# PCA PAC Sediment- Compare Sites
# Written in R 3.5.0
# By: Kristin Eccles
###########################################################
# Load Libraries 
library(factoextra)
library(corrplot)
library(ggplot2)
library(psych)
library(ade4)
library(adegraphics)
library(vegan)
library(MASS)
library(ggfortify)
library(ggpubr)
library(sjPlot)

# Load data
df_perc=read.csv("sediment_pca_percent_rename.csv")

# Subset data
ATH=subset(df_perc, Treatment=="ATH")
BL=subset(df_perc, Treatment=="BL")
LLB=subset(df_perc, Treatment=="LLB")
SAL=subset(df_perc, Treatment=="SAL")

###########################################################
#### ATH ####
prcomp_ATH<-prcomp(ATH[,5:56])

bi_ATH1=fviz_pca_biplot(prcomp_ATH, geom=c("point", "text"),
                   axes = c(1, 2),
                   repel = TRUE,
                   alpha.var ="contrib", 
                   palette=c("#b20000", "#000066"),
                   select.var = list(contrib = 12),
                   fill.ind = ATH$Period, 
                   col.var="black",
                   label="var",
                   pointshape = 21, 
                   pointsize = 3,
                   title=" ")+
  labs(fill = "Mining", alpha = "Contribution") # Change legend title
bi_ATH1

bi_ATH2=fviz_pca_biplot(prcomp_ATH, geom=c("point", "text"),
                       axes = c(3, 4),
                       repel = TRUE,
                       alpha.var ="contrib", 
                       palette=c("#b20000", "#000066"),
                       select.var = list(contrib = 9),
                       fill.ind = ATH$Period, 
                       col.var="black",
                       label="var",
                       pointshape = 21, 
                       pointsize = 3,
                       title=" ",
                       labelsize = 4)+ 
  theme(text = element_text(size = 15))+
  labs(fill = "Mining", alpha = "Contribution") # Change legend title
bi_ATH2

scree_ath1=fviz_contrib(prcomp_ATH, "var", axes = 1, title=" ")
scree_ath1
scree_ath2=fviz_contrib(prcomp_ATH, "var", axes = 2, title=" ")
scree_ath2
################################################
#### BL ####
prcomp_BL<-prcomp(BL[,5:56])

bi_BL1=fviz_pca_biplot(prcomp_BL, geom=c("point", "text"),
                        axes = c(1, 2),
                        repel = TRUE,
                        alpha.var ="contrib", 
                        palette=c("#b20000", "#000066"),
                        select.var = list(contrib = 11),
                        fill.ind = BL$Period, 
                       col.var="black",
                       label="var",
                        pointshape = 21, 
                       pointsize = 3,
                       title=" ",
                       labelsize = 4)+ 
  theme(text = element_text(size = 15))+
  labs(fill = "Mining", alpha = "Contribution") # Change legend title
bi_BL1

bi_BL2=fviz_pca_biplot(prcomp_BL, geom=c("point", "text"),
                        axes = c(3, 4),
                        repel = TRUE,
                        alpha.var ="contrib", 
                        palette=c("#b20000", "#000066"),
                        select.var = list(contrib = 12),
                        fill.ind = BL$Period, 
                       col.var="black",
                       label="var",
                        pointshape = 21, 
                       pointsize = 3,
                       title=" ",
                       labelsize = 4)+ 
  theme(text = element_text(size = 15))+
  labs(fill = "Mining", alpha = "Contribution") # Change legend title
bi_BL2

scree_bl1=fviz_contrib(prcomp_BL, "var", axes = 1, title=" ")
scree_bl1
scree_bl2=fviz_contrib(prcomp_BL, "var", axes = 2, title=" ")
scree_bl2
################################################
#### SAL ####
prcomp_SAL<-prcomp(SAL[,5:56])

bi_SAL1=fviz_pca_biplot(prcomp_SAL, geom=c("point", "text"),
                       axes = c(1, 2),
                       repel = TRUE,
                       alpha.var ="contrib", 
                       palette=c("#b20000", "#000066"),
                       select.var = list(contrib = 7),
                       fill.ind = SAL$Period, 
                       col.var="black",
                       label="var",
                       pointshape = 21, 
                       title=" ",
                       labelsize = 4)+ 
  theme(text = element_text(size = 15))+
  labs(fill = "Mining", alpha = "Contribution") # Change legend title
bi_SAL1

bi_SAL2=fviz_pca_biplot(prcomp_SAL, geom=c("point", "text"),
                       axes = c(3, 4),
                       repel = TRUE,
                       alpha.var ="contrib", 
                       palette=c("#b20000", "#000066"),
                       select.var = list(contrib = 6),
                       fill.ind = SAL$Period, 
                       col.var="black",
                       label="var",
                       pointshape = 21, 
                       pointsize = 3,
                       title=" ",
                       labelsize = 4)+ 
  theme(text = element_text(size = 15))+
  labs(fill = "Mining", alpha = "Contribution") # Change legend title
bi_SAL2

scree_sal1=fviz_contrib(prcomp_SAL, "var", axes = 1, title=" ")
scree_sal1
scree_sal2=fviz_contrib(prcomp_SAL, "var", axes = 2, title=" ")
scree_sal2
####################################################################
### LLB ####
prcomp_LLB<-prcomp(LLB[,5:56])

bi_LLB1=fviz_pca_biplot(prcomp_LLB, geom=c("point", "text"),
                        axes = c(1, 2),
                        repel = TRUE,
                        alpha.var ="contrib", 
                        palette=c("#b20000", "#000066"),
                        select.var = list(contrib = 10),
                        fill.ind = LLB$Period, 
                        col.var="black",
                        label="var",
                        pointshape = 21, 
                        pointsize = 3,
                        title=" ",
                        labelsize = 4)+ 
  theme(text = element_text(size = 15))+
  labs(fill = "Mining", alpha = "Contribution") # Change legend title
bi_LLB1

bi_LLB2=fviz_pca_biplot(prcomp_LLB, geom=c("point", "text"),
                        axes = c(3, 4),
                        repel = TRUE,
                        alpha.var ="contrib", 
                        palette=c("#b20000", "#000066"),
                        select.var = list(contrib = 11),
                        fill.ind = LLB$Period, 
                        col.var="black",
                        label="var",
                        pointshape = 21, 
                        pointsize = 3,
                        title=" ",
                        labelsize = 4)+ 
  theme(text = element_text(size = 15))+
  labs(fill = "Mining", alpha = "Contribution") # Change legend title
bi_LLB2

scree_llb1=fviz_contrib(prcomp_LLB, "var", axes = 1, title=" ")
scree_llb1
scree_llb2=fviz_contrib(prcomp_LLB, "var", axes = 2, title=" ")
scree_llb2

####################################################################
# Pub quality figures
plot2=ggarrange(bi_ATH1, bi_ATH2,bi_BL1,bi_BL2, bi_SAL1, bi_SAL2, bi_LLB1, bi_LLB2,
                ncol = 2,
                nrow = 4,
                labels = c("Lake Athabasca"," ", 
                           " Burnt Lake", " ", 
                           " Saline Lake", " ",
                           " Lac La Biche", " "),
                font.label = list(size = 20))
plot2
#Plot figures with dpi=300
save_plot("biplot_by_site_updated.tif", plot2, width = 30, height = 30, dpi = 300)

plot3=ggarrange(scree_ath1, scree_ath2,scree_bl1,scree_bl2, scree_sal1, scree_sal2, scree_llb1, scree_llb2,
                nrow = 4,
                ncol=2,
                labels = c("Lake Athabasca"," ", 
                           " Burnt Lake", " ", 
                           " Saline Lake", " ",
                           " Lac La Biche", " "),
                font.label = list(size = 20))
plot3
#Plot figures with dpi=300
save_plot("scree_by_site.tif", plot3, width = 60, height = 30, dpi = 300)
