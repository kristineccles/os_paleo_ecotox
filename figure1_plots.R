#################################################
# Plots for figure 1
# Date: March 4th, 2020,
# By: Kristin Eccles
#Notes: TOC normalized in main body, DW normalized in SI
########################################################
# load libraries
library(ggplot2)
library(reshape2)
library(dplyr)
library(ggpubr)
library(sjPlot)

# load data
df1 <- read.csv("figure1_data.csv")

df_dw_norm <- df1[,1:5]
dw_stack <- melt(df_dw_norm[,c(1,4:5)], id.vars=1)
dw_stack <- left_join(dw_stack, df_dw_norm[,c("id", "site", "year")], keep=FALSE)

df_toc_norm <- df1[, c(1:3, 8:9)]
toc_stack <- melt(df_toc_norm[,c(1,4:5)], id.vars=1)
toc_stack <- left_join(toc_stack, df_toc_norm[,c("id", "site", "year")], keep=FALSE)

# For main body
figure1_toc <-ggplot(data= toc_stack, aes(y = year, x = value, color=variable)) +
  geom_path(size=0.75) +
  geom_point(size=1) +
  scale_color_manual(values=c("navy", "#56B4E9"), name = "Group", labels = c("Total PAC", "Total APAC"))+
  facet_wrap(~site, strip.position="bottom", ncol=1) +
  labs(x = "[PAC] ng/g TOC", y = "Year")+
  geom_hline(yintercept=1967, color="red", lty=2)+
  scale_x_continuous(position = "top") +
  theme_bw(base_size=16)
figure1_toc

save_plot("figure1_toc.tif", figure1_toc, width = 15, height = 30, dpi = 300)


#For SI
figure1_dw <-ggplot(data= dw_stack, aes(y = year, x = value, color=variable)) +
  geom_path(size=0.75) +
  geom_point(size=1) +
  scale_color_manual(values=c("navy", "#56B4E9"), name = "Group", labels = c("Total PAC", "Total APAC"))+
  facet_wrap(~site, strip.position="bottom") +
  labs(x = "[PAC] ng/g DW", y = "Year")+
  geom_hline(yintercept=1967, color="red", lty=2)+
  scale_x_continuous(position = "top") +
  theme_bw(base_size=16)
figure1_dw

save_plot("figure1_dw.tif", figure1_dw, width = 20, height = 20, dpi = 300)
