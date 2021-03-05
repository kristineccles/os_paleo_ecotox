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

ggplot(df2) + 
  geom_bar(aes(fill=period, y=concentration, x=chemical), position="dodge", stat="identity") +
  scale_fill_manual(values=c("navy", "#56B4E9"), name = "Period", labels = c("Post", "Pre"))+
  geom_line(aes(x = chemical, y = ef), size = 1.5, color="red", group = 1) + 
  scale_y_continuous(sec.axis = sec_axis(~. *0.0005,name = "EF"))+
  facet_wrap(~site, scales="free_y") +
  theme(legend.position="none") +
  xlab("")+
  theme_bw(base_size=16)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


