library(ggplot2)
library(lme4)
library(lmerTest)
library(MESS)

# Basic calculation of regression to mean effect -----------
seconds <- seq(0,8,1)

simulate.cor <- function(time.effect, treatment.effect){

# tumor treatment effect is 21.4 (2.45); and time effect is -1 (0.55)
  
predictions <- (seconds*time.effect + treatment.effect)
predictions[1] <- 0 # This is the first second, so there is no treatment yet

average <- mean(predictions[2:9]) # exclude the pretreatment observation
return(average)
### simulate this to create a whole range of responses.
### Then analyze at what point results start to become insignificant
}

treatment.effects <- rnorm(1000, mean=20,sd=10)
time.effects <- rnorm(1000, mean=-1, sd=1)

test <- sapply(seq(1,1000,1), function(x) simulate.cor(time.effects[x],treatment.effects[x]))


### If treatment effect is 4.5, and time effect = 1 then effect becomes zero. 
### cannot always assume largest effect is beginnign.

set.seed(1506)
load("All Syncs/emg.RData")
tumor.baby <- emg[which(emg$condition2=="tumor" | emg$condition2=="baby3"),]
tumor.baby.avg <- aggregate(tumor.baby$cor.increase.pc[tumor.baby$cor.n.errors<1], by=list(tumor.baby$study_respondent[tumor.baby$cor.n.errors<1], tumor.baby$condition2[tumor.baby$cor.n.errors<1]), mean)
tumor.baby.avg$type <- ifelse(tumor.baby.avg$x < 75, "strong relaxation",
                              ifelse(tumor.baby.avg$x >= 75 & tumor.baby.avg$x<98, "relaxation",
                                     ifelse(tumor.baby.avg$x >= 98 & tumor.baby.avg$x<102, "no change",
                                            ifelse(tumor.baby.avg$x >= 102 & tumor.baby.avg$x<110, "activation","strong activation"))))
types <- unique(tumor.baby.avg$type)                                          
tumor.baby.avg$type2 <- ifelse(tumor.baby.avg$type=="strong relaxation", "relaxation",
                               ifelse(tumor.baby.avg$type=="strong activation", "activation", 
                                      ifelse(tumor.baby.avg$type=="no change", "relaxation", tumor.baby.avg$type)))


# Simulation with random draws from multiplied dataset --------------------
simulated.data <- do.call("rbind", replicate(n=100, tumor.baby.avg, simplify=FALSE))
simulated.data$type2 <- ifelse(simulated.data$type=="strong relaxation", "relaxation",
                               ifelse(simulated.data$type=="strong activation", "activation", 
                                      ifelse(simulated.data$type=="no change", "relaxation", simulated.data$type)))

probs.activation <- seq(0.05,0.75,0.05)
probs.activation <- cbind(probs.activation, 1-probs.activation, 0.13, 0.87)
colnames(probs.activation) <- c("tumor.pr.act", "tumor.pr.relax", "baby.pr.act", "baby.pr.relax")
probs.activation <- do.call("rbind", replicate(n=100, probs.activation, simplify=FALSE))

# Simulation --------------------------------------------------
emg.small <- emg[emg$condition2%in%c("tumor","baby3"),which(colnames(emg)%in%c("study_respondent", "cor.increase.pc", "condition2", "sync.units"))]
emg.small <- emg.small[order(emg.small$condition2, emg.small$study_respondent, emg.small$sync.units),]

simulate2.mean.ml <- function(prob){
  probs <- round(prob*100,0)
    
  draws <- rbind(data.frame(respondent=tumor.baby.avg$Group.1[sample(which(tumor.baby.avg$Group.2=="tumor" & tumor.baby.avg$type2=="activation") , size=probs[1], replace=TRUE)], condition="tumor"),
              data.frame(respondent=tumor.baby.avg$Group.1[sample(which(tumor.baby.avg$Group.2=="tumor" & tumor.baby.avg$type2=="relaxation") , size=probs[2], replace=TRUE)], condition="tumor"),
              data.frame(respondent=tumor.baby.avg$Group.1[sample(which(tumor.baby.avg$Group.2=="baby3" & tumor.baby.avg$type2=="activation") , size=probs[3], replace=TRUE)], condition="baby3"),
              data.frame(respondent=tumor.baby.avg$Group.1[sample(which(tumor.baby.avg$Group.2=="baby3" & tumor.baby.avg$type2=="relaxation") , size=probs[4], replace=TRUE)], condition="baby3"))
  
  responses <- apply(draws, 1, function(x) emg.small[which(emg.small$study_respondent==x[1] & emg.small$condition2==x[2]),])
  for(i in 1:200){
    responses[[i]]$id <- i
  }
  responses <- do.call("rbind", responses)
  
  ml.model <- lmer(scale(cor.increase.pc) ~ condition2 + (1|study_respondent), data=responses, REML=T, control=strictControl)

  maxes <- aggregate(responses$cor.increase.pc, by=list(responses$id), max)  
  colnames(maxes) <- c("id", "maxes")
  maxes$condition2 <- responses$condition2[responses$sync.units==1]
  max.model <- lm(scale(maxes) ~ condition2, data=maxes)

  maxes$means <- aggregate(responses$cor.increase.pc, by=list(responses$id), mean)[,2]  
  means.model <- lm(scale(means) ~ condition2, data=maxes)
  
  maxes$aucs <- sapply(1:200, function(x) auc(responses$sync.units[which(responses$id==x)], responses$cor.increase.pc[which(responses$id==x)], type="spline"))
  aucs.model <- lm(scale(aucs) ~ condition2, data=maxes)
  
  #average <- mean(responses$cor.increase.pc[responses$condition2=="tumor"])
  output <- c(coef(summary(ml.model))[2,1],coef(summary(ml.model))[2,5], 
  coef(means.model)[2],summary(means.model)[[4]][2,4],
  coef(max.model)[2],summary(max.model)[[4]][2,4],
  coef(aucs.model)[2],summary(aucs.model)[[4]][2,4],
  probs)
  names(output)[1:8] <- c("multilevel_b", "multilevel_p", 
                        "means_b", "means_p",
                        "max_b", "max_p",
                        "auc_b", "auc_p") 
  print("end")
  return(output) 
}

strictControl <- lmerControl(optCtrl = list(
  algorithm = "NLOPT_LN_NELDERMEAD",
  xtol_abs = 1e-12,
  ftol_abs = 1e-12))

mean.multi.regs <- lapply(1:dim(probs.activation)[1], function(x) simulate2.mean.ml(probs.activation[x,]))
mean.multi.regs <- do.call("rbind", mean.multi.regs)
mean.multi.regs <- data.frame(mean.multi.regs)

b <- c(mean.multi.regs$multilevel_b, mean.multi.regs$means_b,
       mean.multi.regs$max_b, mean.multi.regs$auc_b)
p <- c(mean.multi.regs$multilevel_p, mean.multi.regs$means_p,
       mean.multi.regs$max_p, mean.multi.regs$auc_p)
tumor.act <- rep(mean.multi.regs$tumor.pr.act,4)
label <- c(rep("Multilevel", length(b)/4),rep("Mean",length(b)/4),
         rep("Max", length(b)/4), rep("AUC", length(b)/4))

multi.reg <- data.frame(b,p,label, tumor.act)
multi.reg$sig <- ifelse(multi.reg$p<0.05,1,0)
multi.reg$sig2 <- ifelse(multi.reg$sig==0, "p>.05", "p<.05")

# Make a single big plot: meanvsml.jpg --------------------------------------------------
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

sigs <- aggregate(multi.reg$sig, by=list(multi.reg$label), mean)
colnames(sigs)[1] <- "label"
sigs$label2 <- paste0(round(sigs[,2]*100,1),"%")
sigs$x <- 70
sigs$y <- 0

bigplot <- ggplot(multi.reg, aes(x=tumor.act, y=b, group=sig2, colour=sig2)) +
  geom_point() +
  facet_wrap(.~label) +
  xlab("Proportion participants with mean activation") +
  ylab("Treatment effect") +
  scale_colour_manual(values=cbbPalette) + 
  theme(legend.title = element_blank())

bigplot <- bigplot + geom_text(data=sigs, mapping=aes(x=x, y=y, label=label2), inherit.aes=FALSE)

#outdir <- "C:/Users/gschuma1/Dropbox/Apps/Overleaf/Doing Psychophysiology research in political science"
ggsave(bigplot, file=paste0(outdir,"/Figures/meanvsml.jpg"), width=10, height=8, dpi=900)
# Note that minor differences may occur in how this image looks like in the paper, because it is based on simulated data
       