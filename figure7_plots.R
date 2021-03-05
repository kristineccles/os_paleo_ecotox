#################################################
# Plots for figure 7
# Date: March 4th, 2020,
# By: Kristin Eccles

########################################################
# load libraries
library(ggplot2)
library(reshape2)
library(dplyr)

# load data
df7 <- read.csv("figure7_data.csv")



df7_stack <- melt(df7[,c(1,4:6)], id.vars=1)
df7_stack <- left_join(df7_stack, df7[,c("id", "site", "year")], keep=FALSE)
df7_stack$variable <- recode(df7_stack$variable, BioBaPeq = "BioBaPeq (PAH CALUX)")
df7_stack$variable <- recode(df7_stack$variable, ChemBapeq.BioBapeq = "ChemBapeq/BioBapeq")


df7_stack$variable_2 <- factor(df7_stack$variable, labels = c("BaPeq (ng BaP eq./g organic carbon)", 
                                                              "BaPeq (ng BaP eq./g organic carbon)", 
                                                              "BaPeq(%)"))
# For main body
figure7 <-ggplot(data= df7_stack, aes(y = year, x = value)) +
  geom_path(size=0.75) +
  geom_point(size=1) +
  scale_color_manual(values=c("navy", "#56B4E9"), name = "Group", labels = c("Total PAC", "Total APAC"))+
  facet_grid(site~variable, scales="free_x", switch="x")+
  ylab("Year")+
  xlab("BaPeq (ng BaP eq./g organic carbon)", 
       "BaPeq (ng BaP eq./g organic carbon", 
       "BaPeq(%)")+
  geom_hline(yintercept=1967, color="red", lty=2)+
  scale_x_continuous(position = "top", labels =  scales::comma) +
  theme_bw(base_size=16)
figure7

save_plot("figure7.tif", figure7, width = 20, height = 20, dpi = 300)
