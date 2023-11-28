library(fabricatr)
library(lme4)
library(lmerTest)
library(emmeans)
library(dplyr)
library(effectsize)
library(ggplot2)

strictControl <- lmerControl(optCtrl = list(
  algorithm = "NLOPT_LN_NELDERMEAD",
  xtol_abs = 1e-12,
  ftol_abs = 1e-12))

# Simulation treatment only--------------

sync.units <- seq(1,8,1)
treatment.effect <- 20
n <- 10000 / 8 / 2 # desired n / time units / two treatments

simulate.treatment.effect <- function(treatment.effect){
group1 <- rnorm(n, mean=100+treatment.effect, sd=1)
group2 <- rnorm(n, mean=100, sd=1)
data <- c(group1,group2)

multilevel <- unlist(lapply(data, function(x){
                     selection <- x
                     effect.time <- rnorm(1, -0.45, 0.05)
                     decay <- effect.time*sync.units
                     output <- selection+decay
                     return(output)}))
multilevel <- data.frame(multilevel, rep(sync.units,n*2), rep(as.character(rep(seq(1,n,1),each=8)),2), rep(c(1,0), each=(n*8)))
colnames(multilevel) <- c("cor.increase.pc", "sync.units", "study_respondent", "treatment")

m1.mod.cor.lmer <- lmer(cor.increase.pc ~ 1 + treatment + sync.units + (1|study_respondent), REML=T, control=strictControl,data=multilevel)
d <- cohens_d(x=multilevel$cor.increase.pc, y=factor(multilevel$treatment)) # wrong ordering of treatment, correct in next line
output <- c(coef(summary(m1.mod.cor.lmer))[2,1],coef(summary(m1.mod.cor.lmer))[2,5], treatment.effect, d$Cohens_d*-1)
names(output) <- c("b", "p", "treatment", "d")
  return(output)
}

simulation <- replicate(n=100,simulate.treatment.effect(0.05))
simulation <- data.frame(t(simulation))
simulation$sig <- ifelse(simulation$p<0.05,1,0)
simulation$correct <- ifelse(simulation$sig==1 & simulation$b>0,1,0)

simulation <- replicate(n=100,simulate.treatment.effect(0.1))
simulation <- data.frame(t(simulation))
simulation$sig <- ifelse(simulation$p<0.05,1,0)
simulation$correct <- ifelse(simulation$sig==1 & simulation$b>0,1,0)
table(simulation$correct)
summary(simulation$d[simulation$correct==1])


# simulation with moderator -----------


simulate.treatment.effect <- function(treatment.effect, prob.dif){
  p1 <- c(prob.dif, 1-prob.dif)
  group1 <- rnorm(round(n/2), mean=100+treatment.effect, sd=1) # the treatment effect only takes place in half the sample that is treated
  group2 <- rnorm(n*2-length(group1), mean=100, sd=1)
  data <- c(group1,group2)
  moderator <- c(sample(x=c(0,1),size=length(group1),replace=TRUE, prob=c(p1[1],p1[2])),
                 sample(x=c(0,1),size=length(group2),replace=TRUE, prob=c(p1[2],p1[1])))
  
  multilevel <- unlist(lapply(data, function(x){
    selection <- x
    effect.time <- rnorm(1, -0.45, 0.05)
    decay <- effect.time*sync.units
    output <- selection+decay
    return(output)}))
  multilevel <- data.frame(multilevel, rep(sync.units,n*2), rep(as.character(rep(seq(1,n,1),each=8)),2), rep(c(1,0), each=(n*8)), rep(moderator,each=8))
  colnames(multilevel) <- c("cor.increase.pc", "sync.units", "study_respondent", "treatment", "moderator")
  
  m1.mod.cor.lmer <- lmer(cor.increase.pc ~ 1 + treatment + moderator + treatment*moderator + sync.units + (1|study_respondent), REML=T, control=strictControl,data=multilevel)
  #d <- cohens_d(x=multilevel$cor.increase.pc, y=factor(multilevel$treatment)) # wrong ordering of treatment, correct in next line
  output <- c(coef(summary(m1.mod.cor.lmer))[5,1],coef(summary(m1.mod.cor.lmer))[5,5], treatment.effect, prob.dif)
  names(output) <- c("b", "p", "treatment", "prob.dif")
  return(output)
}

simulation <- replicate(n=100,simulate.treatment.effect(0.2,0.6))
simulation <- data.frame(t(simulation))
simulation$sig <- ifelse(simulation$p<0.05,1,0)
simulation$correct <- ifelse(simulation$sig==1 & simulation$b>0,1,0)
table(simulation$correct)
