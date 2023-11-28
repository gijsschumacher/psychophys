library(ggplot2)
library(lme4)
library(lmerTest)
library(lmtest)
load("All Syncs/emg.RData")


winsorize <- function(var, prob){
  low <- paste0(100-prob,"%")
  high <- paste0(prob,"%")  
  quantiles <- quantile(var, probs=seq(0,1,0.05), na.rm=TRUE)
  
  new.var <- ifelse(var<quantiles[low],quantiles[low],
                    ifelse(var>quantiles[high],quantiles[high],var))
  return(new.var)
}


# Make tumor EMG response pattern  ----------------------------------------------------------
tumor <- emg[which(emg$condition2=="tumor"),]

stats <- function(x){
  mean <- mean(x)
  se <- sd(x) / sqrt(length(x))
  lo <- mean - 1.96*se
  hi <- mean + 1.96*se
  output <- c(mean, lo, hi)
  return(output)
}

pc <- do.call(data.frame,aggregate(tumor$cor.increase.pc[tumor$cor.n.errors<1], by=list(tumor$sync.units[tumor$cor.n.errors<1]), stats))
raw <- do.call(data.frame,aggregate(tumor$cor.increase.raw[tumor$cor.n.errors<1], by=list(tumor$sync.units[tumor$cor.n.errors<1]), stats))
z <- do.call(data.frame,aggregate(tumor$cor.z[tumor$cor.n.errors<1], by=list(tumor$sync.units[tumor$cor.n.errors<1]), stats))
#smooth.pc <- do.call(data.frame,aggregate(tumor$cor.smooth.increase.pc[tumor$cor.n.errors<1], by=list(tumor$sync.units[tumor$cor.n.errors<1]), stats))
#smooth.raw <- do.call(data.frame,aggregate(tumor$cor.smooth.increase.raw[tumor$cor.n.errors<1], by=list(tumor$sync.units[tumor$cor.n.errors<1]), stats))

pc[9,] <- c(0, 100, NA, NA) # 9 = error, add 0-point baseline
raw[9,] <- c(0, 0, NA, NA) # 9 = error, add 0-point baseline

#ggplot(pc, aes(x=Group.1, y=x.mean)) +
#  geom_line() +
#  geom_ribbon(data=pc, aes(ymin=x.lo, ymax=x.hi), alpha=.1) +
#  geom_point(data=tumor[tumor$cor.n.errors<1,], aes(x=sync.units, y=cor.increase.pc))

# Plot individual plots
#plot.tumor <- function(respondent){
#  data <- tumor[which(tumor$study_respondent==respondent),which(colnames(tumor)%in%c("sync.units", "cor.increase.pc"))]
#  
#  data[9,] <- c(0,100)
  
#  plot <- ggplot(data, aes(x=sync.units, y=cor.increase.pc)) +
#    geom_line()
  
#  ggsave(plot, file=paste0("C:/Users/gschuma1/surfdrive/Papers/Multimodal Physiology/Simulations/tumor plots/",plot.name))
#}

respondents <- unique(tumor$study_respondent)
#lapply(respondents, plot.tumor)

mean.max <- function(x){
  mean <- mean(x)
  max <- max(x)
  n.above.100 <- length(which(x>100))
  return(c(mean,max,n.above.100))
}

means.maxes <- do.call(data.frame,aggregate(tumor$cor.increase.pc[tumor$cor.n.errors<1 & tumor$last.sync==0], by=list(tumor$study_respondent[tumor$cor.n.errors<1  & tumor$last.sync==0]), mean.max))
means.maxes$low.average <- ifelse(means.maxes$x.1<100 & means.maxes$x.3>1, 1, 0)
means.maxes$low.than.100 <- ifelse(means.maxes$x.1<100,1,0)

low.mean.high.peak <- tumor$cor.increase.pc[which(tumor$study_respondent=="Lab 20_124")]
low.mean.no.peak <- tumor$cor.increase.pc[which(tumor$study_respondent=="Lab 20_212")]
high.mean.high.peak <- tumor$cor.increase.pc[which(tumor$study_respondent=="Lab 20_150")]
low.mean.high.peak[9] <- 100
high.mean.high.peak[9] <- 100

cor.increase <- data.frame(c(pc[,2],low.mean.high.peak,low.mean.no.peak,high.mean.high.peak),                                   
                          rep(seq(1,9,1),4),
                          rep(c("mean", "Low Mean-High Peak", "Low Mean-No Peak", "High Mean-High Peak"), each=9))
colnames(cor.increase) <- c("corrugator.increase", "second", "type")

cor.increase$corrugator.increase[which(cor.increase$second==9)] <- 100
cor.increase$second[which(cor.increase$second==9)] <- 0
cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")


plot <- ggplot(cor.increase, aes(x=second , y=corrugator.increase, group=type, colour=type)) +
  geom_line(size=1.5) +
  xlab("Seconds in tumor treatment") +
  ylab("microVolt Corrugator activity compared to baseline") +
  theme(legend.position="off") +
  #theme(text = element_text(size=24)) +
  scale_colour_manual(values=cbbPalette)  

#outdir <- "C:/Users/gschuma1/Dropbox/Apps/Overleaf/Doing Psychophysiology research in political science"

ggsave(plot, file=paste0(outdir,"/Figures/tumor.response.jpg"), dpi=900)


# Zygomaticus response ----------------------------------------------------
# baby <- emg[which(emg$condition2=="baby3"),]
# 
# pc <- do.call(data.frame,aggregate(baby$zyg.increase.pc[baby$zyg.n.errors<1], by=list(baby$sync.units[baby$zyg.n.errors<1]), stats))
# 
# means.maxes <- do.call(data.frame,aggregate(baby$zyg.increase.pc[baby$zyg.n.errors<1 & baby$last.sync==0], by=list(baby$study_respondent[baby$zyg.n.errors<1  & baby$last.sync==0]), mean.max))
# means.maxes$low.average <- ifelse(means.maxes$x.1<100 & means.maxes$x.3>1, 1, 0)
# means.maxes$low.than.100 <- ifelse(means.maxes$x.1<100,1,0)
# 
# low.mean.high.peak <- baby$zyg.increase.pc[which(baby$study_respondent=="Lab 20_216")]
# low.mean.no.peak <- baby$zyg.increase.pc[which(baby$study_respondent=="Lab 20_209")]
# high.mean.high.peak <- baby$zyg.increase.pc[which(baby$study_respondent=="Lab 20_201")]
# high.mean.high.peak[9] <- 100
# 
# zyg.increase <- data.frame(c(pc[,2],low.mean.high.peak,low.mean.no.peak,high.mean.high.peak),                                   
#                            rep(seq(1,9,1),4),
#                            rep(c("mean", "Low Mean-High Peak", "Low Mean-No Peak", "High Mean-High Peak"), each=9))
# colnames(zyg.increase) <- c("zygomaticus.increase", "second", "type")
# 
# zyg.increase$zygomaticus.increase[which(zyg.increase$second==9)] <- 100
# zyg.increase$second[which(zyg.increase$second==9)] <- 0
# 
# plot <- ggplot(zyg.increase, aes(x=second , y=zygomaticus.increase, group=type, colour=type)) +
#   geom_line() +
#   xlab("Seconds in baby treatment") +
#   ylab("Increase in zygomaticus activity compared to baseline") +
#   ggtitle("Four different patterns of zygomaticus response to baby picture")
# 
# ggsave(plot, file="baby.response.jpg")


# Develop multilevel model corrugator --------------------------------------------------------------
library(multilevelTools)
library(JWileymisc)
library(extraoperators)

tumor.baby <- emg[which(emg$condition2=="tumor" | emg$condition2=="baby3"),]
tumor.baby.redux <- tumor.baby[which(tumor.baby$cor.n.errors<1 & tumor.baby$last.sync==0),]
tumor.baby.redux$cor.stat.outlier <- ifelse(scale(tumor.baby.redux$cor.increase.pc)>5,1,0)
  
# Is random intercept justified?
m0.glm <- glm(cor.increase.pc ~ 1, family="gaussian", data=tumor.baby.redux)
m0.lmer <- lmer(cor.increase.pc ~ 1 + (1|study_respondent), REML=T, data=tumor.baby.redux)

AIC(logLik(m0.glm))
AIC(logLik(m0.lmer)) # this one is lower (14882.96 vs 14993.4) so study_respondent is justified

m0a.lmer <- lmer(cor.increase.pc ~ 1 + sync.units + (1|study_respondent), REML=T, data=tumor.baby.redux)
m0b.lmer <- lmer(cor.increase.pc ~ 1 + sync.units + (1 + sync.units|study_respondent), REML=T, data=tumor.baby.redux)
m0c.lmer <- lmer(cor.increase.pc ~ 1 + syncs + (1|study_respondent), REML=T, data=tumor.baby.redux)

anova(m0.lmer, m0a.lmer, test = "Chisq", refit = F)
anova(m0a.lmer, m0b.lmer, test = "Chisq", refit = F) # this is the best-fit
anova(m0c.lmer, m0a.lmer, test = "Chisq", refit = F)

m0d.lmer <- lmer(cor.increase.pc ~ 1 + syncs + sync.units + (sync.units|study_respondent), REML=T, data=tumor.baby.redux)
m0e.lmer <- lmer(cor.increase.pc ~ 1 + syncs + sync.units + (1|study_respondent), REML=T, data=tumor.baby.redux)

anova(m0b.lmer, m0d.lmer, test = "Chisq") # this is the best-fit
anova(m0d.lmer, m0e.lmer, test = "Chisq") # d = best fit

# Model d is the best model, now diagnostics
plot(m0d.lmer, study_respondent ~ resid(.), abline = 0 )
leveneTest(residuals(m0d.lmer) ~ tumor.baby.redux$study_respondent)
shapiro.test(tumor.baby.redux$cor.increase.pc)

 # clear heteroskedasticity & nonnormality
# Solution 1: introduce weights
library(nlme)
m0d.lme <- lme(cor.increase.pc ~ syncs + sync.units, random = ~1|study_respondent, data = tumor.baby.redux, method = "ML")
m0dw.lme <- update(m0d.lme, weights = varIdent(form = ~ 1|study_respondent))
anova(m0d.lme, m0dw.lme)

## Model improves a lot but, doesn't work with sync.unit + study_respondent levels
 
# Solution 2: omit outliers
tumor.baby.woutliers <- tumor.baby.redux[-which(scale(tumor.baby.redux$cor.increase.pc)>4),]
hist(tumor.baby.woutliers$cor.increase.pc)

m0d.lmer <- lmer(cor.increase.pc ~ 1 + syncs + sync.units + (sync.units|study_respondent), REML=T, data=tumor.baby.woutliers)
leveneTest(residuals(m0d.lmer) ~ tumor.baby.woutliers$study_respondent) # still heteroskedastic
shapiro.test(tumor.baby.woutliers$cor.increase.pc) # still nonnormal

# Solution 3: Log-transformation
m0d.lmer <- lmer(log(cor.increase.pc) ~ 1 + syncs + sync.units + (sync.units|study_respondent), REML=T, data=tumor.baby.redux)
leveneTest(residuals(m0d.lmer) ~ tumor.baby.redux$study_respondent) # still heteroskedastic
shapiro.test(log(tumor.baby.redux$cor.increase.pc)) # still nonnormal

m0d.lmer <- lmer(log(cor.increase.pc) ~ 1 + syncs + sync.units + (sync.units|study_respondent), REML=T, data=tumor.baby.woutliers)
leveneTest(residuals(m0d.lmer) ~ tumor.baby.woutliers$study_respondent) # still heteroskedastic
shapiro.test(log(tumor.baby.woutliers$cor.increase.pc)) # still nonnormal

# Solution 4: Robust standard errors
library(robustlmm)
m0dr.lmer <- rlmer(cor.increase.pc ~ 1 + syncs + sync.units + (sync.units|study_respondent), REML=T, data=tumor.baby.redux)
m0cr.lmer <- rlmer(cor.increase.pc ~ 1 + syncs + sync.units + (1|study_respondent), REML=T, data=tumor.baby.redux)
compare(m0dr.lmer,m0cr.lmer)

# solution 5: winsorizing
strictControl <- lmerControl(optCtrl = list(
  algorithm = "NLOPT_LN_NELDERMEAD",
  xtol_abs = 1e-12,
  ftol_abs = 1e-12))

tumor.baby.redux$cor.increase.pc.win <- winsorize(tumor.baby.redux$cor.increase.pc, 95)
m0d.lmer <- lmer(cor.increase.pc.win ~ 1 + syncs + sync.units + (sync.units|study_respondent), REML=T, data=tumor.baby.redux, control = strictControl)
leveneTest(residuals(m0d.lmer) ~ tumor.baby.redux$study_respondent) # still heteroskedastic
shapiro.test(tumor.baby.redux$cor.increase.pc.win) # still nonnormal

# Does it matter for the treatment effect?
ml.models <- list()
ml.models[[1]] <- lme(cor.increase.pc ~ condition2 + syncs + sync.units, random = ~1|study_respondent, data = tumor.baby.woutliers, method = "ML", control=strictControl, weights = varIdent(form = ~ 1|study_respondent))
ml.models[[2]] <- lmer(cor.increase.pc ~ 1 + condition2 + syncs + sync.units + (sync.units|study_respondent), REML=T, data=tumor.baby.woutliers)
ml.models[[3]] <- lmer(log(cor.increase.pc) ~ 1 + condition2 + syncs + sync.units + (sync.units|study_respondent), REML=T, data=tumor.baby.woutliers, control=strictControl)
ml.models[[4]] <- rlmer(cor.increase.pc ~ 1 + condition2 + syncs + sync.units + (sync.units|study_respondent), REML=T, data=tumor.baby.woutliers, control=strictControl)
ml.models[[5]] <- lmer(cor.increase.pc.win ~ 1 + condition2 +  syncs + sync.units + (sync.units|study_respondent), REML=T, data=tumor.baby.redux, control = strictControl)
ml.models[[6]] <- lmer(cor.increase.pc ~ 1 + condition2 + syncs + sync.units + (1|study_respondent), REML=T, control = strictControl, data=tumor.baby.redux)

#
par(mfrow = c(2, 2))           # display plots in 2 rows and 2 columns
plot(ml.models[[1]] , pch = 20, col = "black", lty = "dotted"); par(mfrow = c(1, 1))

qqnorm(ml.models[[1]], pch = 20, col = "black")  # nice diagonal
qqnorm(predict(ml.models[[2]]), pch = 20, col = "black") # okay-ish
qqnorm(predict(ml.models[[3]]), pch = 20, col = "black") #big bump in middle
qqnorm(predict(ml.models[[4]]), pch = 20, col = "black") # big bump in middle
qqnorm(predict(ml.models[[5]]), pch = 20, col = "black") # okay
qqnorm(predict(ml.models[[6]]), pch = 20, col = "black") # somewhat okay

plot(ml.models[[1]]) # some clusters to the left
plot(ml.models[[2]]) # looks okay
plot(ml.models[[3]]) # some clusters to the left
plot(ml.models[[4]]) # some clusters to the left
plot(ml.models[[5]]) # random but winsorized clear
plot(ml.models[[6]])  # few clear outileres


# In sum removing outliers or winsorizing works well. But general model is also okay
effects <- rbind(coef(summary(ml.models[[2]]))[2,],
      coef(summary(ml.models[[5]]))[2,],
      coef(summary(ml.models[[6]]))[2,])
colnames(effects)[2] <- "se"
effects <- data.frame(effects)
effects$lo <- effects$Estimate - 1.96*effects$se
effects$hi <- effects$Estimate + 1.96*effects$se
effects$label <- c("outlier removal", "winsorizing", "only preprocessing")


# Multilevel model for zygomaticus data -----------------------------------
tumor.baby.redux <- tumor.baby[which(tumor.baby$zyg.n.errors<1 & tumor.baby$last.sync==0),]
tumor.baby.redux$zyg.stat.outlier <- ifelse(scale(tumor.baby.redux$zyg.increase.pc)>5,1,0)

# Is random intercept justified?
m0.glm <- glm(zyg.increase.pc ~ 1, family="gaussian", data=tumor.baby.redux)
m0.lmer <- lmer(zyg.increase.pc ~ 1 + (1|study_respondent), REML=T, data=tumor.baby.redux)

AIC(logLik(m0.glm))
AIC(logLik(m0.lmer)) # this one is lower (20102.34 vs 19962.57) so study_respondent is justified

m0a.lmer <- lmer(zyg.increase.pc ~ 1 + sync.units + (1|study_respondent), REML=T, data=tumor.baby.redux)
m0b.lmer <- lmer(zyg.increase.pc ~ 1 + sync.units + (sync.units|study_respondent), REML=T, control=strictControl, data=tumor.baby.redux) 
m0c.lmer <- lmer(zyg.increase.pc ~ 1 + syncs + (1|study_respondent), REML=T, data=tumor.baby.redux)

anova(m0.lmer, m0a.lmer, test = "Chisq", refit = F)
anova(m0a.lmer, m0b.lmer, test = "Chisq", refit = F) # this is the best-fit
anova(m0c.lmer, m0a.lmer, test = "Chisq", refit = F)

m0d.lmer <- lmer(zyg.increase.pc ~ 1 + syncs + sync.units + (sync.units|study_respondent), REML=T,control=strictControl, data=tumor.baby.redux)
m0e.lmer <- lmer(zyg.increase.pc ~ 1 + syncs + sync.units + (1|study_respondent), REML=T, data=tumor.baby.redux)
anova(m0e.lmer, m0a.lmer, test = "Chisq", refit = F)



# Compare different DVs and make preprocessing visual ---------------------------------------------------
tumor.baby.redux2 <- tumor.baby[which(tumor.baby$last.sync==0),]

models <- list()
dvs <- c("cor.increase.raw", "cor.increase.pc", "cor.z", "cor.hampel.increase")
samples <- c("all", "cor.error.bigchange<1", "cor.error.nochange<1", "cor.n.errors<2", "cor.n.errors<1", "winsorized", "outlier removal")
options <- expand.grid(dvs, samples)

do.model <- function(sample.type, dv){
  if(sample.type=="all"){sample <- tumor.baby.redux2
  } else if(sample.type%in%c("winsorized", "outlier removal")){sample <- tumor.baby.redux2[which(tumor.baby.redux2$cor.n.errors<1),]
    } else {sample <- eval(parse(text=paste("tumor.baby.redux2[tumor.baby.redux2$",sample.type,",]")))}
  sample$x <- eval(parse(text=paste0("sample$", dv)))
  if(sample.type=="winsorized"){ sample$x <- winsorize(sample$x,95) }
  if(sample.type=="outlier removal") {sample <- sample[-which(scale(sample$x)>4),]}
  # note the model with sync.units as random effect doesn't converge
  model <- lmer(x ~ 1 + condition2 + sync.units + syncs + (1|study_respondent), control=strictControl, REML=T, data=sample)
  output <- coef(summary(model))[2,]
  n <- dim(model@frame)[1]
  return(c(output,n))
}

# standard models
models.cor <- lapply(1:dim(options)[1], function(x) do.model(sample.type=options[x,2],dv=options[x,1]))
# winsorized models.cor
models.cor <- do.call("rbind", models.cor)
models.cor <- data.frame(models.cor, options)
models.cor$lo <- models.cor$Estimate - 1.96*models.cor$Std..Error
models.cor$hi <- models.cor$Estimate + 1.96*models.cor$Std..Error
models.cor$samples2 <- ifelse(models.cor$Var2=="all","None\nN=1579",
                   ifelse(models.cor$Var2=="cor.error.bigchange<1","Cliffs\n1440",
                   ifelse(models.cor$Var2=="cor.error.nochange<1", "Flat\n1517",
                   ifelse(models.cor$Var2=="cor.n.errors<2", "Errors\n1455",
                   ifelse(models.cor$Var2=="cor.n.errors<1", "1 error\n1393", 
                   ifelse(models.cor$Var2=="winsorized", "Winsor\n1393", 
                   ifelse(models.cor$Var2=="outlier removal", "Outliers\n1384", models.cor$Var2)))))))

models.cor$samples2 <- factor(models.cor$samples2, levels=c("None\nN=1579", "Cliffs\n1440", "Flat\n1517", "Errors\n1455", "1 error\n1393", "Winsor\n1393", "Outliers\n1384"))

models.cor$dv <- ifelse(models.cor$Var1=="cor.increase.raw", "Raw",
             ifelse(models.cor$Var1=="cor.increase.pc", "Percent",
             ifelse(models.cor$Var1=="cor.z", "Z-score",
             ifelse(models.cor$Var1=="cor.hampel.increase", "Hampel", models.cor$Var1))))
  

tumor.baby.redux2 <- tumor.baby[which(tumor.baby$last.sync==0),]

do.model <- function(sample.type, dv){
  if(sample.type=="all"){sample <- tumor.baby.redux2
  } else if(sample.type%in%c("winsorized", "outlier removal")){sample <- tumor.baby.redux2[which(tumor.baby.redux2$zyg.n.errors<1),]
  } else {sample <- eval(parse(text=paste("tumor.baby.redux2[tumor.baby.redux2$",sample.type,",]")))}
  sample$x <- eval(parse(text=paste0("sample$", dv)))
  if(sample.type=="winsorized"){ sample$x <- winsorize(sample$x,95) }
  if(sample.type=="outlier removal") {sample <- sample[-which(scale(sample$x)>4),]}
  # note the model with sync.units as random effect doesn't converge
  model <- lmer(x ~ 1 + condition2 + sync.units + syncs + (1|study_respondent), control=strictControl, REML=T, data=sample)
  output <- coef(summary(model))[2,]
  n <- dim(model@frame)[1]
  return(c(output,n))
}

models.zyg <- list()
dvs <- c("zyg.increase.raw", "zyg.increase.pc", "zyg.z", "zyg.hampel.increase")
samples <- c("all", "zyg.error.bigchange<1", "zyg.error.nochange<1", "zyg.n.errors<2", "zyg.n.errors<1", "winsorized", "outlier removal")
options <- expand.grid(dvs, samples)
models.zyg <- lapply(1:dim(options)[1], function(x) do.model(sample.type=options[x,2],dv=options[x,1]))

models.zyg <- do.call("rbind", models.zyg)
models.zyg <- data.frame(models.zyg, options)
models.zyg$lo <- models.zyg$Estimate - 1.96*models.zyg$Std..Error
models.zyg$hi <- models.zyg$Estimate + 1.96*models.zyg$Std..Error
models.zyg$samples2 <- ifelse(models.zyg$Var2=="all","None\nN=1579",
                          ifelse(models.zyg$Var2=="zyg.error.bigchange<1","Cliffs\n1440",
                                 ifelse(models.zyg$Var2=="zyg.error.nochange<1", "Flat\n1486",
                                        ifelse(models.zyg$Var2=="zyg.n.errors<2", "Errors\n1409",
                                               ifelse(models.zyg$Var2=="zyg.n.errors<1", "1 error\n1333", 
                                                      ifelse(models.zyg$Var2=="winsorized", "Winsor\n1333", 
                                                             ifelse(models.zyg$Var2=="outlier removal", "Outliers\n1324", models.zyg$Var2)))))))
models.zyg$samples2 <- factor(models.zyg$samples2, levels=c("None\nN=1579", "Cliffs\n1440", "Flat\n1486", "Errors\n1409", "1 error\n1333", "Winsor\n1333", "Outliers\n1324"))

models.zyg$dv <- ifelse(models.zyg$Var1=="zyg.increase.raw", "Raw",
                    ifelse(models.zyg$Var1=="zyg.increase.pc", "Percent",
                           ifelse(models.zyg$Var1=="zyg.z", "Z-score", 
                                  ifelse(models.zyg$Var1=="zyg.hampel.increase","Hampel", models.zyg$Var1))))

models <- rbind(models.cor, models.zyg)
models <- data.frame(models)
models <- models[c(2,18,20,24,28,30,46,50,54),]
models$activity <- c(rep("Corrugator",5), rep("Zygomaticus",4))
models$samples3 <- as.character(models$samples2)
models$samples3[3] <- "Hampel\n1393"
models$samples3 <- factor(models$samples3, levels=c("None\nN=1579", "1 error\n1393", "Hampel\n1393","Winsor\n1393", "Outliers\n1384", 
                                                    "1 error\n1333", "Winsor\n1333",  "Outliers\n1324"))
#levels(models$samples2) <- levels(models$samples2)[c(1,)]
preprocessing <- ggplot(models, aes(x=samples3, y=Estimate)) +
  geom_point() +
  geom_errorbar(data=models, aes(ymin=lo, ymax=hi), width=.2) + 
  facet_grid(.~activity, scales="free") +
  xlab("Data removal or transformation") +
  ylab("Estimated change in corrugator activity (microVolt)") + 
  theme(strip.text=element_text(size=12),
        axis.title.x = element_text(size=12),
        axis.text.x=element_text(size=10),
        axis.title.y=element_text(size=12),
        axis.text.y=element_text(size=10)) +
  geom_hline(yintercept=0)
  #ggtitle("Difference corrugator activity tumor vs baby\nwith different preprocessing steps")

ggsave(preprocessing, file=paste0(outdir, "/Figures/preprocessing.pdf"), dpi=900)


# Does adding standard deviation matter ----------------------------------------------------------
m0d.lmer <- lmer(cor.increase.pc ~ 1 + syncs + sync.units + (sync.units|study_respondent), REML=T, data=tumor.baby.redux)
m0g.lmer <- lmer(cor.increase.pc ~ 1 + syncs + sync.units + cor_sd + (sync.units|study_respondent), REML=T, data=tumor.baby.redux)

anova(m0d.lmer,m0g.lmer)

m0d.lmer <- lmer(cor.increase.pc ~ 1 + syncs + sync.units + (sync.units|study_respondent), REML=T, data=tumor.baby.woutliers)
m0g.lmer <- lmer(cor.increase.pc ~ 1 + syncs + sync.units + cor_sd + (sync.units|study_respondent), REML=T, data=tumor.baby.woutliers)

anova(m0d.lmer,m0g.lmer)

# no substantial improvement


#### Correlations between measures -------------
cor(tumor.baby.redux$cor.increase.pc, tumor.baby.redux$cor.increase.raw)
cor(tumor.baby.redux$cor.z,tumor.baby.redux$cor.increase.pc)
cor(tumor.baby.redux$cor.z,tumor.baby.redux$cor.increase.raw)
cor(tumor.baby.redux$cor.hampel.increase,tumor.baby.redux$cor.increase.pc)

