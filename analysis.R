load("C:/Users/gschuma1/surfdrive/Papers/Multimodal Physiology/All Syncs/emg.RData")
library(stringr)
library(lme4)
library(lmerTest)

# Basic setup -------------------------------------------------------------
strictControl <- lmerControl(optCtrl = list(
  algorithm = "NLOPT_LN_NELDERMEAD",
  xtol_abs = 1e-12,
  ftol_abs = 1e-12))

get.winsorized <- function(respondent, var, prob) {
  data <- emg[which(emg$study_respondent==respondent),]
  var <- data[,which(colnames(emg)==var)]
  if(is.na(var[1])){
    values <- rep(NA,length(var))
  } else{
    low <- paste0(100-prob,"%")
    high <- paste0(prob,"%")
    quantiles <- quantile(var, probs=seq(0,1,0.05), na.rm=TRUE)

    values <- ifelse(var<quantiles[low],quantiles[low],
                      ifelse(var>quantiles[high],quantiles[high],var))
  }
  return(values)
}

cor.analysis <- emg[emg$treatment==1 & emg$cor.n.errors<1 & emg$last.sync==0,]
zyg.analysis <- emg[emg$treatment==1 & emg$zyg.n.errors<1 & emg$last.sync==0,]

# Make variables easier for analysis
cor.analysis$lab <- ifelse(cor.analysis$location=="lab",1,0)
cor.analysis$emotion <- ifelse(cor.analysis$positive==1, 1,
                        ifelse(cor.analysis$negative==1, 2, 
                        ifelse(cor.analysis$negative==0 & cor.analysis$positive==0,0,NA)))
cor.analysis$cor.increase.pc.win <- get.winsorized(cor.analysis$cor.increase.pc,95)
zyg.analysis$zyg.increase.pc.win <- get.winsorized(zyg.analysis$zyg.increase.pc,95)

# Q1 ----------------------------------------------------------------------
treatment.chars <- c("political", "label", "face","emotion", "sound", "lab")
DVs <- c("cor.increase.pc", "cor.increase.pc.win", "zyg.increase.pc", "zyg.increase.pc.win")
options <- expand.grid(DVs, treatment.chars)

main.analysis <- function(dv, treatment){
formula <- as.formula(paste0(dv, "~ 1 +", treatment, "+ sync.units + syncs + location + temperature + (sync.units|study_respondent)"))
if(str_sub(options[2,1],-3, -1)=="win"){model <- lmer(formula, REML=T, control=strictControl,data=cor.analysis)
  } else {model <- lmer(formula, REML=T, control=strictControl,data=cor.analysis, subset=cor.stat.outlier==0)
}
return(model)
}

main.analyses <- lapply(1:dim(options)[1], function(x) main.analysis(dv=options[x,1],treatment=options[x,2]))

robustness <- function(dv, treatment){
if(substr(options[1,1],1,3)=="zyg"){
formula <- as.formula(paste0(dv, "~ 1 +", treatment, "+ sync.units + syncs + temperature + factor(computer) + (sync.units|study_respondent)"))
if(str_sub(options[2,1],-3, -1)=="win"){model <- lmer(formula, REML=T, control=strictControl,data=zyg.analysis)
  } else {model <- lmer(formula, REML=T, control=strictControl,data=zyg.analysis, subset=zyg.stat.outlier==0)
}
return(model)
}}

robustness.analyses <- lapply(1:dim(options)[1], function(x) robustness(dv=options[x,1],treatment=options[x,2]))



# Q2 ----------------------------------------------------------------------
moderators <- c("female","polknow","age","alcohol","student","leftright","cynicism",
"political_interest","partisanship.strength","votechoice_lr","education")
DVs <- c("cor.increase.pc", "cor.increase.pc.win", "zyg.increase.pc", "zyg.increase.pc.win")

options <- expand.grid(DVs,treatment.chars, moderators)

moderator.analysis <- function(dv, treatment, moderator){
  formula <- as.formula(paste0(dv, "~ 1 +", treatment, "+", moderator, "+", treatment, "*", moderator,  "+ sync.units + syncs + location + temperature + (sync.units|study_respondent)"))
  if(str_sub(options[2,1],-3, -1)=="win"){model <- lmer(formula, REML=T, control=strictControl,data=cor.analysis)
    } else {model <- lmer(formula, REML=T, control=strictControl,data=cor.analysis, subset=cor.stat.outlier==0)
  }  
  return(model)
}

moderator.analyses <- lapply(1:dim(options)[1], function(x) moderator.analysis(dv=options[x,1],treatment=options[x,2], moderator=options[x,3]))
