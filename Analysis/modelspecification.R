library(ggplot2)
library(lme4)
library(lmerTest)
library(lmtest)
library(multilevelTools)
library(JWileymisc)
library(texreg)
library(extraoperators)


strictControl <- lmerControl(optCtrl = list(
  algorithm = "NLOPT_LN_NELDERMEAD",
  xtol_abs = 1e-12,
  ftol_abs = 1e-12))

# Null model specification ------------------------------------------------
emg.analysis <- emg[emg$treatment==1 & emg$last.sync==0,]

emg.analysis$sync.sq <- emg.analysis$sync.units^2 
emg.analysis$sync.sq3 <- emg.analysis$sync.units^3

m0cor.glm <- glm(cor.increase.pc ~ 1, family="gaussian", data=emg.analysis, subset=cor.n.errors<1 & cor.stat.outlier==0)
m0cor.lmer <- lmer(cor.increase.pc ~ 1 + (1|study_respondent), REML=T, data=emg.analysis, subset=cor.n.errors<1 & cor.stat.outlier==0)
AIC(logLik(m0cor.glm))
AIC(logLik(m0cor.lmer))
    
m1cor.lmer <- lmer(cor.increase.pc ~ 1 + sync.units + syncs + (1|study_respondent), REML=T, control=strictControl,data=emg.analysis, subset=cor.n.errors<1 & cor.stat.outlier==0)
m2cor.lmer <- lmer(cor.increase.pc ~ 1 + sync.units + syncs + (sync.units|study_respondent), REML=T, control=strictControl,data=emg.analysis, subset=cor.n.errors<1 & cor.stat.outlier==0)
m3cor.lmer <- lmer(cor.increase.pc ~ 1 + sync.units + syncs + location + (sync.units|study_respondent), REML=T, control=strictControl,data=emg.analysis, subset=cor.n.errors<1 & cor.stat.outlier==0)
m4cor.lmer <- lmer(cor.increase.pc ~ 1 + sync.units + syncs + location + factor(computer) + (sync.units|study_respondent), REML=T, control=strictControl,data=emg.analysis, subset=cor.n.errors<1 & cor.stat.outlier==0) # not converging
m5cor.lmer <- lmer(cor.increase.pc ~ 1 + sync.units + syncs + factor(computer) + (sync.units|study_respondent), REML=T, control=strictControl,data=emg.analysis, subset=cor.n.errors<1 & cor.stat.outlier==0)
m6cor.lmer <- lmer(cor.increase.pc ~ 1 + sync.units + syncs + computer + (computer|study_respondent), REML=T, control=strictControl,data=emg.analysis, subset=cor.n.errors<1 & cor.stat.outlier==0) # model does not converge
m7cor.lmer <- lmer(cor.increase.pc ~ 1 + sync.units + syncs + location + temperature + (sync.units|study_respondent), REML=T, control=strictControl,data=emg.analysis, subset=cor.n.errors<1 & cor.stat.outlier==0)
m8cor.lmer <- lmer(cor.increase.pc ~ 1 + sync.units + syncs + location + temperature + (location|study_respondent), REML=T, control=strictControl,data=emg.analysis, subset=cor.n.errors<1 & cor.stat.outlier==0)
m9cor.lmer <- lmer(cor.increase.pc ~ 1 + sync.units + sync.sq + sync.sq3 + syncs + location + temperature + (sync.units|study_respondent), REML=T, control=strictControl,data=emg.analysis, subset=cor.n.errors<1 & cor.stat.outlier==0)


anova(m0cor.lmer, m1cor.lmer, test = "Chisq", refit = F)
anova(m1cor.lmer, m2cor.lmer, test = "Chisq", refit = F)
anova(m2cor.lmer, m3cor.lmer, test = "Chisq", refit = F)
anova(m3cor.lmer, m4cor.lmer, test = "Chisq", refit = F)
anova(m3cor.lmer, m5cor.lmer, test = "Chisq", refit = F) # Model 3 is better
anova(m7cor.lmer, m5cor.lmer, test = "Chisq", refit = F) # Model 3 is better

anova(m3cor.lmer, m7cor.lmer, test = "Chisq", refit = F) # tiny improvement, but okay
anova(m7cor.lmer, m9cor.lmer, test = "Chisq", refit = F) # model does not significantly improve

### model 7 wins

m0zyg.glm <- glm(zyg.increase.pc ~ 1, family="gaussian", data=emg.analysis, subset=zyg.n.errors<1 & zyg.stat.outlier==0)
m0zyg.lmer <- lmer(zyg.increase.pc ~ 1 + (1|study_respondent), REML=T, data=emg.analysis, subset=zyg.n.errors<1 & zyg.stat.outlier==0)
AIC(logLik(m0zyg.glm))
AIC(logLik(m0zyg.lmer))
    
m3zyg.lmer <- lmer(zyg.increase.pc ~ 1 + sync.units + syncs + location + (sync.units|study_respondent), REML=T, control=strictControl,data=emg.analysis, subset=zyg.n.errors<1 & zyg.stat.outlier==0)
m5zyg.lmer <- lmer(zyg.increase.pc ~ 1 + sync.units + syncs + factor(computer) + (sync.units|study_respondent), REML=T, control=strictControl,data=emg.analysis, subset=zyg.n.errors<1 & zyg.stat.outlier==0)

m6zyg.lmer <- lmer(zyg.increase.pc ~ 1 + sync.units + syncs + temperature + factor(computer) + (sync.units|study_respondent), REML=T, control=strictControl,data=emg.analysis, subset=zyg.n.errors<1 & zyg.stat.outlier==0)
anova(m3zyg.lmer, m5zyg.lmer, test = "Chisq", refit = F) # 5 = better

m7zyg.lmer <- lmer(zyg.increase.pc ~ 1 + sync.units + syncs + location + temperature + (sync.units|study_respondent), REML=T, control=strictControl,data=emg.analysis, subset=zyg.n.errors<1 & zyg.stat.outlier==0)
anova(m5zyg.lmer, m6zyg.lmer, test = "Chisq", refit = F) # no difference
anova(m5zyg.lmer, m7zyg.lmer, test = "Chisq", refit = F) # no difference but prefer model 7 as it is consistent with corrugator modelling
anova(m6zyg.lmer, m7zyg.lmer, test = "Chisq", refit = F) # this is also better, probably preregister this as robustness  check

# Produce tables to compare complicated model to simpler model ----------------------
cor.analysis <- emg[emg$treatment==1 & emg$cor.n.errors<1 & emg$last.sync==0,]
zyg.analysis <- emg[emg$treatment==1 & emg$zyg.n.errors<1 & emg$last.sync==0,]

strictControl <- lmerControl(optCtrl = list(
  algorithm = "NLOPT_LN_NELDERMEAD",
  xtol_abs = 1e-12,
  ftol_abs = 1e-12))

initial.model.cor <- lmer(cor.increase.pc ~ 1 + sync.units + syncs + location + temperature + (sync.units|study_respondent), REML=T, control=strictControl,data=cor.analysis, subset=cor.stat.outlier==0)
replacement.model.cor <- lmer(cor.increase.pc ~ 1 + sync.units + syncs + location + temperature + (1|study_respondent), REML=T, control=strictControl,data=cor.analysis, subset=cor.stat.outlier==0)

initial.model.zyg <- lmer(zyg.increase.pc ~ 1 + sync.units + syncs + location + temperature + (sync.units|study_respondent), REML=T, control=strictControl,data=zyg.analysis, subset=zyg.stat.outlier==0)
replacement.model.zyg <- lmer(zyg.increase.pc ~ 1 + sync.units + syncs + location + temperature + (1|study_respondent), REML=T, control=strictControl,data=zyg.analysis, subset=zyg.stat.outlier==0)

texreg(list(initial.model.cor, replacement.model.cor, initial.model.zyg, replacement.model.zyg),
       file=paste0(outdir,"/Tables/baseline_comparison.tex"),
       stars=0.05,
       custom.header = list("Corrugator"= 1:2, "Zygomaticus"=3:4),
       custom.model.names=c("Preregistered", "Adopted", "Preregistered", "Adopted"),
       custom.coef.names = c("Intercept", "Seconds in experiment", "Seconds in treatment", "EO", "Lab", "Lowlands", "Nijmegen", "Tilburg", "TT Assen", "Temperature"),
       caption="Preregistered and adopted multilevel models",
       caption.above=TRUE,
       label="app:prereg",
       digits=3,
       float.pos="!htbp")

### Comparison effect sizes preregistered models vs adopted models
load("Analysis/main.analyses.RData")

coefs.main <- lapply(main.analyses, function(x){
  if(colnames(x@frame)[2]%in%c("label", "emotion")){
    output <- data.frame(rbind(c(coef(summary(x))[2,1],coef(summary(x))[2,5],rownames(coef(summary(x)))[2] ),
                               c(coef(summary(x))[3,1],coef(summary(x))[3,5],rownames(coef(summary(x)))[3])))
    row.names(output) <- coef(summary(x))[c(2,3)]
  } else {output <- data.frame(coef(summary(x))[2,1],coef(summary(x))[2,5],rownames(coef(summary(x)))[2])
  }
  colnames(output) <- c("b", "p", "label")
  return(output)
})

load("Analysis/main.analyses_correction.RData")

coefs.corrected <- lapply(robustness.check2, function(x){
  if(colnames(x@frame)[2]%in%c("label", "emotion")){
    output <- data.frame(rbind(c(coef(summary(x))[2,1],coef(summary(x))[2,5],rownames(coef(summary(x)))[2] ),
                    c(coef(summary(x))[3,1],coef(summary(x))[3,5],rownames(coef(summary(x)))[3])))
    row.names(output) <- coef(summary(x))[c(2,3)]
  } else {output <- data.frame(coef(summary(x))[2,1],coef(summary(x))[2,5],rownames(coef(summary(x)))[2])
  }
  colnames(output) <- c("b", "p", "label")
  return(output)
})

coefs.main <- do.call("rbind", coefs.main)
coefs.main$type <- "preregistered"
coefs.corrected <- do.call("rbind", coefs.corrected)
coefs.corrected$type <- "adopted" 
coefs <- rbind(coefs.main, coefs.corrected)
coefs[,1] <- as.numeric(coefs[,1])
coefs[,2] <- as.numeric(coefs[,2])
rownames(coefs) <- NULL
