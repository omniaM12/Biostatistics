library(dplyr)
library(tidyverse)
library(tidyr)
library(finalfit)
library(ggplot2)
library(FSA)

nacan<-read.csv("nat.cand.csv")
View(nacan)

natcanp<-nacan%>% pivot_longer(cols=c("ACV", "GD","CNO", "TTO","EOL"), names_to="Extract",
                                values_to="sensitivity")
                                
explanatory=c("sensitivity")
dependent=c("Extract")

natcdd<-natcanp%>% summary_factorlist(dependent = dependent, explanatory = explanatory, p= TRUE)
nats<-data.frame(natcdd)
write.table(nats, file = "nats.csv ", sep = ",",  qmethod = "double", row.names = F)


sensitivity<-recode(natcanp$sensitivity, R="0", S="1")
totalnancan<-data.frame(sensitivity,natcanp)
totalnancan$sensitivity<-as.numeric(totalnancan$sensitivity)
shapiro.test(totalnancan$sensitivity)
kruskal.test(sensitivity~ Extract,totalnancan)

ggplot(totalnancan, aes(fill= sensitivity.1 ,x=Extract)) + 
  geom_bar(position="dodge", stat="count") + 
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))
