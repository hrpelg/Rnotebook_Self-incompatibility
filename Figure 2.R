##############################################
  ## Figure 2 ##  
   
##########################################

#load libraries

library(ggplot2)
library(tidyr)
library(ggpubr)
library(extrafont)
library(tidyverse)

 
setwd("/workspace/hrpelg/SI_paper_figures")
si<-read.table ("S01b_point_index_difference_plot.txt",header=TRUE, sep='\t')
si2<-read.table ("S01b_Fluo_Cycles_plot.txt",header=TRUE, sep='\t')
si3 <-read.table ("S02x_dif_fluorescense_temperature.txt", header=TRUE, sep='\t')
si4 <-read.table ("S02x_Fluorescense_Cycle.txt", header=TRUE, sep='\t')
si5 <-read.table ("S09_Fluorescense_cycles.txt", header=TRUE, sep='\t')
si6 <-read.table ("S09_dif_fluoresencese_Temperature.txt", header=TRUE, sep='\t')
si7 <-read.table ("S23_Fluorescense_Cycle1.txt", header=TRUE, sep='\t')
si8 <-read.table ("S23_fluorescense_temperature.txt", header=TRUE, sep='\t')
si9 <-read.table ("S5_cycles_short.txt", header=TRUE, sep='\t')
si10 <-read.table ("S5_fluorescence.txt", header=TRUE, sep='\t')
si12 <-read.table ("S28_S23_Cycles.txt", header=TRUE, sep='\t')
si13<-read.table ("S28_S23_temp.txt", header=TRUE, sep='\t')
si14 <-read.table ("S8_cycles.txt", header=TRUE, sep='\t')
si15 <-read.table ("S8_temp.txt", header=TRUE, sep='\t')
outfile = file.path("/workspace/hrpelg/SI_paper_figures","test")
library(png)
tiff("S1_melting_figures.tiff",width = 900, height = 500, units = "px", pointsize = 12)

labels_genotypes <- c(
 "S1" = expression(S[1]), 
  "S20" = expression(S[20]), 
  "S24" = expression(S[24]),
  "S20S24"=expression(paste(S[20],"S"[24])),
  "S2"= expression(S[2]),
  "S5"= expression(S[5]), 
  "S8"= expression(S[8]), 
  "S9"=expression(S[9]),
  "S23"=expression(S[23]),
  "S28"=expression(S[28])
)

#Difference plot S1#

title= expression(paste(S[1], S[20], S[24], " Melting Peaks"))
p1<- ggplot(si, aes(x=temperature, y=df.dt, colour=(genotype), shape=(genotype))) + geom_point(aes(shape=genotype, color=genotype), size= 2) + scale_color_manual(labels =labels_genotypes, values=c("darksalmon","darkgreen", "darkorchid", "red4")) +scale_shape_manual(labels =labels_genotypes, values=c(3, 16, 5, 17)) + ylab ("(-dF/dt) Fluorescence (465-510)") + xlab ("Temperature (°C)") + theme(legend.title = element_text(size=14, face="bold"), legend.text = element_text(size = 14)) +
  labs(title=title) + theme(plot.title = element_text(hjust = 0.5,face="bold", size= 18)) + theme(axis.text=element_text(size=14)) +  theme(axis.title.y = element_text(size = 13,face="bold")) + theme(axis.title.x = element_text(size = 12,face="bold"))

#Fluorescence cycles plot#


title= expression(paste(S[1], S[20], S[24], " Fluorescence History"))
p2<- ggplot(si2, aes(x=Cycles, y=Fluorescence, colour=(genotype), shape=(genotype))) + geom_point(aes(shape=genotype, color=genotype), size= 2) + scale_color_manual(labels =labels_genotypes, values=c("darkgrey","darksalmon","darkgreen", "darkorchid", "red4")) +scale_shape_manual(labels =labels_genotypes, values=c(1,3, 16, 5, 17)) + ylab ("Fluorescence (465-510)") + xlab ("Cycles") + theme(legend.title = element_text(size=14, face="bold"), legend.text = element_text(size = 14)) +
  labs(title=title) + theme(plot.title = element_text(hjust = 0.5,face="bold", size= 18)) + theme(axis.text=element_text(size=14)) +  theme(axis.title.y = element_text(size = 13,face="bold")) + theme(axis.title.x = element_text(size = 12,face="bold"))


#ggarrange(p2,p1,ncol=2,nrow=1, common.legend=TRUE, legend="bottom")

#dev.off()

#Difference plot S2#
tiff("S2_melting_figures.tiff",width = 900, height = 500, units = "px", pointsize = 12)
title= expression(paste(S[2], " Melting Peaks"))
p3<- ggplot(si3, aes(x=temperature, y=df.dt, colour=(genotype), shape=(genotype))) + geom_point(aes(shape=genotype, color=genotype), size= 1) + scale_color_manual(labels =labels_genotypes, values=c("deepskyblue")) +scale_shape_manual(labels =labels_genotypes, values=c(15)) + ylab ("(-dF/dt) Fluorescence (465-510)") + xlab ("Temperature (°C)") + theme(legend.title = element_text(size=14, face="bold"), legend.text = element_text(size = 14)) +
  labs(title=title) + theme(plot.title = element_text(hjust = 0.5,face="bold", size= 18)) + theme(axis.text=element_text(size=14)) +  theme(axis.title.y = element_text(size = 13,face="bold")) + theme(axis.title.x = element_text(size = 12,face="bold"))

title= expression(paste(S[2], " Fluorescence History"))
p4<- ggplot(si4, aes(x=Cycles, y=Fluorescence, colour=(genotype), shape=(genotype))) + geom_point(aes(shape=genotype, color=genotype), size= 2) + scale_color_manual(labels =labels_genotypes, values=c("darkgrey", "deepskyblue")) +scale_shape_manual(labels =labels_genotypes, values=c(1,15)) + ylab ("Fluorescence (465-510)") + xlab ("Cycles") + theme(legend.title = element_text(size=14, face="bold"), legend.text = element_text(size = 14)) +
  labs(title=title) + theme(plot.title = element_text(hjust = 0.5,face="bold", size= 18)) + theme(axis.text=element_text(size=14)) +  theme(axis.title.y = element_text(size = 13,face="bold")) + theme(axis.title.x = element_text(size = 12,face="bold"))

#ggarrange(p4,p3,ncol=2,nrow=1, common.legend=TRUE, legend="bottom")

#dev.off()

#Difference plot S9#

tiff("S9_melting_figures.tiff",width = 900, height = 500, units = "px", pointsize = 12)
title= expression(paste(S[9], " Melting Peaks"))
p5<- ggplot(si6, aes(x=temperature, y=df.dt, colour=(genotype), shape=(genotype))) + geom_point(aes(shape=genotype, color=genotype), size= 1) + scale_color_manual(labels =labels_genotypes, values=c("blue")) +scale_shape_manual(labels =labels_genotypes, values=c(15)) + ylab ("(-dF/dt) Fluorescence (465-510)") + xlab ("Temperature (°C)") + theme(legend.title = element_text(size=14, face="bold"), legend.text = element_text(size = 14)) +
  labs(title=title) + theme(plot.title = element_text(hjust = 0.5,face="bold", size= 18)) + theme(axis.text=element_text(size=14)) +  theme(axis.title.y = element_text(size = 13,face="bold")) + theme(axis.title.x = element_text(size = 12,face="bold"))

title= expression(paste(S[9], " Fluorescence History"))
p6<- ggplot(si7, aes(x=Cycles, y=Fluorescence, colour=(genotype), shape=(genotype))) + geom_point(aes(shape=genotype, color=genotype), size= 2) + scale_color_manual(labels =labels_genotypes, values=c("darkgrey", "blue")) +scale_shape_manual(labels =labels_genotypes, values=c(1,15)) + ylab ("Fluorescence (465-510)") + xlab ("Cycles") + theme(legend.title = element_text(size=14, face="bold"), legend.text = element_text(size = 14)) +
  labs(title=title) + theme(plot.title = element_text(hjust = 0.5,face="bold", size= 18)) + theme(axis.text=element_text(size=14)) +  theme(axis.title.y = element_text(size = 13,face="bold")) + theme(axis.title.x = element_text(size = 12,face="bold"))
#p6
#ggarrange(p6,p5,ncol=2,nrow=1, common.legend=TRUE, legend="bottom")

#dev.off()

#Difference plot S23#

tiff("S23_melting_figures.tiff",width = 900, height = 500, units = "px", pointsize = 12)
title= expression(paste(S[23], " Melting Peaks"))
p7<- ggplot(si8, aes(x=temperature, y=df.dt, colour=(genotype), shape=(genotype))) + geom_point(aes(shape=genotype, color=genotype), size= 1) + scale_color_manual(labels =labels_genotypes, values=c("yellow")) +scale_shape_manual(labels =labels_genotypes, values=c(15)) + ylab ("(-dF/dt) Fluorescence (465-510)") + xlab ("Temperature (°C)") + theme(legend.title = element_text(size=14, face="bold"), legend.text = element_text(size = 14)) +
  labs(title=title) + theme(plot.title = element_text(hjust = 0.5,face="bold", size= 18)) + theme(axis.text=element_text(size=14)) +  theme(axis.title.y = element_text(size = 13,face="bold")) + theme(axis.title.x = element_text(size = 12,face="bold"))

title= expression(paste(S[23], " Fluorescence History"))
p9<- ggplot(si7, aes(x=Cycles, y=Fluorescence, colour=(genotype), shape=(genotype))) + geom_point(aes(shape=genotype, color=genotype), size= 2) + scale_color_manual(labels =labels_genotypes, values=c("darkgrey", "yellow")) +scale_shape_manual(labels =labels_genotypes, values=c(1,15)) + ylab ("Fluorescence (465-510)") + xlab ("Cycles") + theme(legend.title = element_text(size=14, face="bold"), legend.text = element_text(size = 14)) +
  labs(title=title) + theme(plot.title = element_text(hjust = 0.5,face="bold", size= 18)) + theme(axis.text=element_text(size=14)) +  theme(axis.title.y = element_text(size = 13,face="bold")) + theme(axis.title.x = element_text(size = 12,face="bold"))

#ggarrange(p9,p7,ncol=2,nrow=1, common.legend=TRUE, legend="bottom")

#dev.off()

#Difference plot S5#

tiff("S5_melting_figures.tiff",width = 900, height = 500, units = "px", pointsize = 12)
title= expression(paste(S[5], " Melting Peaks"))
p11<- ggplot(si10, aes(x=temperature, y=df.dt, colour=(genotype), shape=(genotype))) + geom_point(aes(shape=genotype, color=genotype), size= 1) + scale_color_manual(labels =labels_genotypes, values=c("orange")) +scale_shape_manual(labels =labels_genotypes, values=c(15)) + ylab ("(-dF/dt) Fluorescence (465-510)") + xlab ("Temperature (°C)") + theme(legend.title = element_text(size=14, face="bold"), legend.text = element_text(size = 14)) +
  labs(title=title) + theme(plot.title = element_text(hjust = 0.5,face="bold", size= 18)) + theme(axis.text=element_text(size=14)) +  theme(axis.title.y = element_text(size = 13,face="bold")) + theme(axis.title.x = element_text(size = 12,face="bold"))

title= expression(paste(S[5], " Fluorescence History"))
p10<- ggplot(si9, aes(x=Cycles, y=Fluorescence, colour=(genotype), shape=(genotype))) + geom_point(aes(shape=genotype, color=genotype), size= 2) + scale_color_manual(labels =labels_genotypes, values=c("darkgrey", "orange")) +scale_shape_manual(labels =labels_genotypes, values=c(1,15)) + ylab ("Fluorescence (465-510)") + xlab ("Cycles") + theme(legend.title = element_text(size=14, face="bold"), legend.text = element_text(size = 14)) +
  labs(title=title) + theme(plot.title = element_text(hjust = 0.5,face="bold", size= 18)) + theme(axis.text=element_text(size=14)) +  theme(axis.title.y = element_text(size = 13,face="bold")) + theme(axis.title.x = element_text(size = 12,face="bold"))



#ggarrange(p10,p11,ncol=2,nrow=1, common.legend=TRUE, legend="bottom")


#dev.off()

#Difference plot S28#

tiff("S28_melting_figures.tiff",width = 900, height = 500, units = "px", pointsize = 12)
title= expression(paste(S[28], " Melting Peaks"))
p13<- ggplot(si13, aes(x=temperature, y=df.dt, colour=(genotype), shape=(genotype))) + geom_point(aes(shape=genotype, color=genotype), size= 1) + scale_color_manual(labels =labels_genotypes, values=c("yellow", "gray48")) +scale_shape_manual(labels =labels_genotypes, values=c(15, 15)) + ylab ("(-dF/dt) Fluorescence (465-510)") + xlab ("Temperature (°C)") + theme(legend.title = element_text(size=14, face="bold"), legend.text = element_text(size = 14)) +
  labs(title=title) + theme(plot.title = element_text(hjust = 0.5,face="bold", size= 18)) + theme(axis.text=element_text(size=14)) +  theme(axis.title.y = element_text(size = 13,face="bold")) + theme(axis.title.x = element_text(size = 12,face="bold"))

title= expression(paste(S[28], " Fluorescence History"))
p14<- ggplot(si12, aes(x=Cycles, y=Fluorescence, colour=(genotype), shape=(genotype))) + geom_point(aes(shape=genotype, color=genotype), size= 2) + scale_color_manual(labels =labels_genotypes, values=c("darkgrey", "yellow", "gray48")) +scale_shape_manual(labels =labels_genotypes, values=c(1,15, 15)) + ylab ("Fluorescence (465-510)") + xlab ("Cycles") + theme(legend.title = element_text(size=14, face="bold"), legend.text = element_text(size = 14)) +
  labs(title=title) + theme(plot.title = element_text(hjust = 0.5,face="bold", size= 18)) + theme(axis.text=element_text(size=14)) +  theme(axis.title.y = element_text(size = 13,face="bold")) + theme(axis.title.x = element_text(size = 12,face="bold"))


#ggarrange(p14,p13,ncol=2,nrow=1, common.legend=TRUE, legend="bottom")

#dev.off() 
#Difference plot S8#

tiff("S8_melting_figures.tiff",width = 900, height = 500, units = "px", pointsize = 12)
title= expression(paste(S[8], " Melting Peaks"))
p15<- ggplot(si15, aes(x=temperature, y=df.dt, colour=(genotype), shape=(genotype))) + geom_point(aes(shape=genotype, color=genotype), size= 1) + scale_color_manual(labels =labels_genotypes, values=c("darkorchid4")) +scale_shape_manual(labels =labels_genotypes, values=c(15, 15)) + ylab ("(-dF/dt) Fluorescence (465-510)") + xlab ("Temperature (°C)") + theme(legend.title = element_text(size=14, face="bold"), legend.text = element_text(size = 14)) +
  labs(title=title) + theme(plot.title = element_text(hjust = 0.5,face="bold", size= 18)) + theme(axis.text=element_text(size=14)) +  theme(axis.title.y = element_text(size = 13,face="bold")) + theme(axis.title.x = element_text(size = 12,face="bold"))

title= expression(paste(S[8], " Fluorescence History"))
p16<- ggplot(si14, aes(x=Cycles, y=Fluorescence, colour=(genotype), shape=(genotype))) + geom_point(aes(shape=genotype, color=genotype), size= 2) + scale_color_manual(labels =labels_genotypes, values=c("darkgrey", "darkorchid4")) +scale_shape_manual(labels =labels_genotypes, values=c(1, 15)) + ylab ("Fluorescence (465-510)") + xlab ("Cycles") + theme(legend.title = element_text(size=14, face="bold"), legend.text = element_text(size = 14)) +
  labs(title=title) + theme(plot.title = element_text(hjust = 0.5,face="bold", size= 18)) + theme(axis.text=element_text(size=14)) +  theme(axis.title.y = element_text(size = 13,face="bold")) + theme(axis.title.x = element_text(size = 12,face="bold"))


#ggarrange(p16,p15,ncol=2,nrow=1, common.legend=TRUE, legend="bottom")

#dev.off()

#General figure, profile#

tiff("Allele-specific HRM makers.tiff",width = 900, height = 1600, units = "px", pointsize = 3)


ggarrange(p2, p1, p4, p3, p6, p5, p9, p7, p10, p11, p14, p13, p16, p15 ,ncol=2,nrow=7, common.legend=FALSE, legend="bottom")


dev.off()

