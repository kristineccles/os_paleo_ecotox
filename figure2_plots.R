#######################################################
# Plots for figure 1
# Date: March 4th, 2020,
# By: Kristin Eccles

########################################################
# load libraries
library(ggplot2)
library(reshape2)
library(dplyr)

# load data
df2 <- read.csv("figure2_data.csv")

df2$chemical <-factor(df2$chemical, levels = c("C0-Naphthalene",
                                              "C1-Naphthalene",
                                              "C2-Naphthalene",
                                              "C3-Naphthalene",
                                              "C4-Naphthalene",
                                              "C0-Fluorene",
                                              "C1-Fluorene",
                                              "C2-Fluorene",
                                              "C3-Fluorene",
                                              "C0-Phenanthrene/Anthracene",
                                              "C1-Phenanthrene/Anthracene",
                                              "C2-Phenanthrene/Anthracene",
                                              "C3-Phenanthrene/Anthracene",
                                              "C4-Phenanthrene/Anthracene",
                                              "C0-Dibenzonthiophenes",
                                              "C1-Dibenzothiophenes",
                                              "C2-Dibenzothiophenes",
                                              "C3-Dibenzothiophenes",
                                              "C4-Dibenzothiophenes",
                                              "C0-Fluoranthene/Pyrene",
                                              "C1-Fluoranthene/Pyrene",
                                              "C2-Fluoranthene/Pyrene",
                                              "C3-Fluoranthene/Pyrene",
                                              "C4-Fluoranthene/Pyrene",
                                              "C0-Benz[a]anthracene/Chrysene",
                                              "C1-Benz[a]anthracene/Chrysene",
                                              "C2-Benz[a]anthracene/Chrysene",
                                              "C3-Benz[a]anthracene/Chrysene",
                                              "C4-Benz[a]anthracene/Chrysene"))

df2$period <- factor(df2$period, levels=c("Pre", "Post"))

figure2 <- ggplot(df2) + 
  geom_bar(aes(fill=period, y=concentration, x=chemical), position="dodge", stat="identity", alpha=0.8) +
  scale_fill_manual(values=c("#000066", "#b20000"), name = "Period")+
  geom_point(aes(x = ef_id, y = ef/0.001), size = 0.75, color="black", alpha=0.5) + 
  geom_line(aes(x = ef_id, y = ef/0.001), size = 0.75, color="black", alpha=0.5) + 
  scale_y_continuous(sec.axis = sec_axis(trans=~.*0.001,name = "Enrichment Factor"))+
  facet_wrap(~site, scales="free_y") +
  theme(legend.position="none") +
  xlab("Compounds")+
  ylab("[PAC] ng/g TOC")+
  theme_bw(base_size=16)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
figure2 

save_plot("figure2 .tif", figure2 , width = 35, height = 20, dpi = 300)

