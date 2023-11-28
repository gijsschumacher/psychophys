library(plyr)
library(ggplot2)
library(dplyr)
library(ggthemes)
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

load( "All Syncs/emg.RData")
#load( "All Syncs/scl.RData") # this dataset is not shared, so this cannot be replicated. This is also not reported in the paper
selection <- c("lab replication", "Lab 20")
cor.subset <- emg[is.na(emg$positive)==FALSE & emg$cor.n.errors<1 & emg$last.sync==0 & emg$cor.stat.outlier==0 & emg$study%in%selection & emg$datacollection%in%c("iaps", "threat", "congruence"),]
zyg.subset <- emg[is.na(emg$positive)==FALSE & emg$zyg.n.errors<1 & emg$last.sync==0 & emg$zyg.stat.outlier==0 & emg$study%in%selection & emg$datacollection%in%c("iaps", "threat", "congruence"),]

scl.subset <- scl[is.na(scl$positive)==FALSE & scl$scl.n.errors<1 & scl$last.sync==0 & is.na(scl$scl.increase.pc)==FALSE & scl$condition2!="birds" & scl$scl.increase.pc%in%c("Inf","-Inf")==FALSE & scl$study%in%selection & scl$datacollection%in%c("iaps", "threat", "congruence"),]
scl.subset$scl.stat.outlier <- ifelse(abs(scale(scl.subset$scl.increase.pc))>3, 1, 0)
scl.subset <- scl.subset[scl.subset$scl.stat.outlier==0,]

cor.subset <- cor.subset[order(cor.subset$study_respondent, cor.subset$syncs, cor.subset$sync.units),]
zyg.subset <- zyg.subset[order(zyg.subset$study_respondent, zyg.subset$syncs, zyg.subset$sync.units),]
#scl.subset <- scl.subset[order(scl.subset$study_respondent, scl.subset$syncs, scl.subset$sync.units),]

cor <- ddply(cor.subset, .(condition2), summarize,
             mean=mean(cor.hampel.increase),
             sd=sd(cor.hampel.increase),
             length=length(cor.hampel.increase),
             se=sd / sqrt(length),
             lo=mean - 1.96*se,
             hi=mean + 1.96*se)
colnames(cor) <- paste0("cor", colnames(cor))

zyg <- ddply(zyg.subset, .(condition2), summarize,
             mean=mean(zyg.increase.pc),
             sd=sd(zyg.increase.pc),
             length=length(zyg.increase.pc),
             se=sd / sqrt(length),
             lo=mean - 1.96*se,
             hi=mean + 1.96*se)
colnames(zyg) <- paste0("zyg", colnames(zyg))

# scl <- ddply(scl.subset, .(condition2), summarize,
#              mean=mean(scl.increase.pc),
#              sd=sd(scl.increase.pc),
#              length=length(scl.increase.pc),
#              se=sd / sqrt(length),
#              lo=mean - 1.96*se,
#              hi=mean + 1.96*se)
# colnames(scl) <- paste0("scl", colnames(scl))

#joint.data <- data.frame(cor, zyg, scl)
joint.data <- data.frame(cor, zyg)
colnames(joint.data)[1] <- "visual"
iaps <- openxlsx::read.xlsx("Markers/overview conditions.xlsx")
selection <- c("A", "B", "C", "D", "E", "F",2)
iaps$valence[iaps$condition%in%selection] <- (iaps$valence[iaps$condition%in%selection]-1) * (100/8) 
iaps$arousal[iaps$condition%in%selection] <- (iaps$arousal[iaps$condition%in%selection]-1) * (100/8) 
joint.data <- left_join(joint.data, iaps, by="visual")

# A few values are missing; add them here
joint.data$valence[joint.data$visual=="baby3"] <- (8.2-1) * (100/8)
joint.data$valence[joint.data$visual=="basket"] <- (4.94-1) * (100/8) 
joint.data$valence[joint.data$visual=="dog1"] <- (3.55-1) * (100/8)
joint.data$valence[joint.data$visual=="dog2"] <- (4.21-1) * (100/8)
joint.data$valence[joint.data$visual=="grandpa"] <- (8.03-1) * (100/8)
joint.data$valence[joint.data$visual=="knive"] <- (2.73-1) * (100/8)
joint.data$valence[joint.data$visual=="spoon"] <- (5.04-1) * (100/8)
joint.data$valence[joint.data$visual=="tumor"] <- (1.46-1) * (100/8)

joint.data$valence.lab <- ifelse(joint.data$valence<40,"Negative",
                          ifelse(joint.data$valence>60, "Positive", "Neutral"))

# ggplot(joint.data, aes(x=cormean, y=sclmean, fill=valence.lab, colour=valence.lab)) +
#   geom_point() +
#   geom_errorbarh(aes(xmin=corlo, xmax=corhi)) + 
#   geom_errorbar(aes(ymin=scllo, ymax=sclhi)) + 
#   geom_hline(yintercept=100, colour="red") +
#   geom_vline(xintercept=100, colour="red") +
#   xlab("Negative-Positive (corrugator only)") +
#   ylab("Low-High Arousal") +
#   ggtitle("Valence-Arousal model with IAPS pictures")

cor.zyg <- ggplot(joint.data, aes(x=cormean, y=zygmean, fill=valence.lab, colour=valence.lab)) +
  geom_point(aes(shape=valence.lab)) +
  geom_errorbarh(aes(xmin=corlo, xmax=corhi)) + 
  geom_errorbar(aes(ymin=zyglo, ymax=zyghi)) + 
  geom_hline(yintercept=100, colour="red") +
  geom_vline(xintercept=100, colour="red") +
  scale_colour_manual(values=cbbPalette[c(1,2,6)]) + 
  xlab("Corrugator") +
  ylab("Zygomaticus") +
  #ggtitle("Positive-Negative with IAPS pictures") +
  theme(legend.title = element_blank(),
        legend.position=c(0.8,0.8))


#outdir <- "C:/Users/gschuma1/Dropbox/Apps/Overleaf/Doing Psychophysiology research in political science"

ggsave(cor.zyg, file=paste0(outdir, "/Figures/cor_zyg.jpg"), dpi=900)
cor(joint.data$zygmean, joint.data$valence, use="pairwise.complete.obs")
cor(joint.data$cormean, joint.data$valence, use="pairwise.complete.obs")

# Add comparison data -------
selection <- c("lab replication", "Lab 20")
cor.subset2 <- emg[is.na(emg$positive)==FALSE & emg$last.sync==0 & emg$study%in%selection & emg$datacollection%in%c("iaps", "threat", "congruence"),]
zyg.subset2 <- emg[is.na(emg$positive)==FALSE & emg$last.sync==0 & emg$study%in%selection & emg$datacollection%in%c("iaps", "threat", "congruence"),]

cor.subset2 <- cor.subset2[order(cor.subset2$study_respondent, cor.subset2$syncs, cor.subset2$sync.units),]
zyg.subset2 <- zyg.subset2[order(zyg.subset2$study_respondent, zyg.subset2$syncs, zyg.subset2$sync.units),]

cor2 <- ddply(cor.subset2, .(condition2), summarize,
             mean=mean(cor.hampel.increase),
             sd=sd(cor.hampel.increase),
             length=length(cor.hampel.increase),
             se=sd / sqrt(length),
             lo=mean - 1.96*se,
             hi=mean + 1.96*se)
colnames(cor2) <- paste0("cor2", colnames(cor2))

zyg2 <- ddply(zyg.subset2, .(condition2), summarize,
             mean=mean(zyg.increase.pc),
             sd=sd(zyg.increase.pc),
             length=length(zyg.increase.pc),
             se=sd / sqrt(length),
             lo=mean - 1.96*se,
             hi=mean + 1.96*se)
colnames(zyg2) <- paste0("zyg2", colnames(zyg2))

colnames(cor2)[1] <- "visual"
colnames(zyg2)[1] <- "visual"
joint.data <- left_join(joint.data, cor2, by="visual")
joint.data <- left_join(joint.data, zyg2, by="visual")

cor(joint.data$zyg2mean, joint.data$valence, use="pairwise.complete.obs")
cor(joint.data$cor2mean, joint.data$valence, use="pairwise.complete.obs")
cor(joint.data$cor2mean, joint.data$cormean, use="pairwise.complete.obs")
cor(joint.data$zyg2mean, joint.data$zygmean, use="pairwise.complete.obs")

