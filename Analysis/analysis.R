library(stargazer)
library(stringr)
library(lme4)
library(lmerTest)
library(ggplot2)
library(ggthemes)

load( "All Syncs/emg.RData")
# Basic setup -------------------------------------------------------------
strictControl <- lmerControl(optCtrl = list(
  algorithm = "NLOPT_LN_NELDERMEAD",
  xtol_abs = 1e-12,
  ftol_abs = 1e-12))


winsorize <- function(var, prob){
  low <- paste0(100-prob,"%")
  high <- paste0(prob,"%")  
  quantiles <- quantile(var, probs=seq(0,1,0.05), na.rm=TRUE)
  
  new.var <- ifelse(var<quantiles[low],quantiles[low],
                    ifelse(var>quantiles[high],quantiles[high],var))
  return(new.var)
}

cor.analysis <- emg[emg$treatment==1 & emg$cor.n.errors<1 & emg$last.sync==0,]
zyg.analysis <- emg[emg$treatment==1 & emg$zyg.n.errors<1 & emg$last.sync==0,]

# Make variables easier for analysis
cor.analysis$lab <- ifelse(cor.analysis$location=="lab",1,0)
cor.analysis$emotion <- factor(ifelse(cor.analysis$positive==1, 1,
                        ifelse(cor.analysis$negative==1, 2, 
                        ifelse(cor.analysis$negative==0 & cor.analysis$positive==0,0,NA))))
cor.analysis$cor.increase.pc.win <- winsorize(cor.analysis$cor.increase.pc, 95)

cor.analysis$cor.hampel.increase <- cor.analysis$cor.hampel.increase
cor.analysis$cor.increase.pc.win <- cor.analysis$cor.increase.pc.win

zyg.analysis$zyg.increase.pc.win <- winsorize(zyg.analysis$zyg.increase.pc, 95)
zyg.analysis$lab <- ifelse(zyg.analysis$location=="lab",1,0)
zyg.analysis$emotion <- factor(ifelse(zyg.analysis$positive==1, 1,
                               ifelse(zyg.analysis$negative==1, 2, 
                                      ifelse(zyg.analysis$negative==0 & zyg.analysis$positive==0,0,NA))))

zyg.analysis$zyg.increase.pc <- zyg.analysis$zyg.increase.pc
zyg.analysis$zyg.increase.pc.win <- zyg.analysis$zyg.increase.pc.win

cor.analysis$female[which(cor.analysis$female==2)] <- 1
zyg.analysis$female[which(zyg.analysis$female==2)] <- 1

cor.analysis$leftright <- scale(cor.analysis$leftright)
cor.analysis$age <- scale(cor.analysis$age)

zyg.analysis$leftright <- scale(zyg.analysis$leftright)
zyg.analysis$age <- scale(zyg.analysis$age)

# Preregistered Q1 ----------------------------------------------------------------------
# treatment.chars <- c("political", "label", "face","emotion", "sound", "lab")
# DVs <- c("cor.increase.pc", "cor.increase.pc.win", "zyg.increase.pc", "zyg.increase.pc.win")
# options <- expand.grid(DVs, treatment.chars)
# 
# main.analysis <- function(dv, treatment){
# if(substr(dv,1,3)=="zyg" & treatment=="emotion"){formula <- as.formula(paste0(dv, "~ 1 +", treatment, "+ sync.units + syncs + (sync.units|study_respondent)"))
#   } else{formula <- as.formula(paste0(dv, "~ 1 +", treatment, "+ sync.units + syncs + location + temperature + (sync.units|study_respondent)"))
#     }
# 
# if(substr(dv,1,3)=="cor"){
#     if(str_sub(dv,-3, -1)=="win"){model <- lmer(formula, REML=T, control=strictControl,data=cor.analysis)
#     } else {model <- lmer(formula, REML=T, control=strictControl,data=cor.analysis, subset=cor.stat.outlier==0)
#       }
# } else if(str_sub(dv,-3, -1)=="win"){model <- lmer(formula, REML=T, control=strictControl,data=zyg.analysis)
# } else {model <- lmer(formula, REML=T, control=strictControl,data=zyg.analysis, subset=zyg.stat.outlier==0)
# }
# return(model)
# }
# 
# 
# main.analyses <- list()
# for(i in 1:24){
#   main.analyses[[i]] <- main.analysis(dv=options[i,1],treatment=options[i,2])
#   print(i)
# }

#save(main.analyses, file="main.analyses.RData")

# robustness <- function(dv, treatment){
# if(substr(dv,1,3)=="zyg"){
# formula <- as.formula(paste0(dv, "~ 1 +", treatment, "+ sync.units + syncs + temperature + factor(computer) + (sync.units|study_respondent)"))
# if(str_sub(dv,-3, -1)=="win"){model <- lmer(formula, REML=T, control=strictControl,data=zyg.analysis)
#   } else {model <- lmer(formula, REML=T, control=strictControl,data=zyg.analysis, subset=zyg.stat.outlier==0)
# }
# return(model)
# }}
# 
# robustness.analyses <- lapply(1:dim(options)[1], function(x) robustness(dv=options[x,1],treatment=options[x,2]))
# 
# prereg.model.extra1 <-summary(lmer(cor.hampel.increase ~ political + label + face + sound + lab + sync.units + syncs + location + temperature + (sync.units|study_respondent), REML=T, control=strictControl, data=cor.analysis, subset=cor.stat.outlier==0))
# prereg.model.extra2 <-summary(lmer(cor.increase.pc.win ~ political + label + face + sound + lab + sync.units + syncs + location + temperature + (sync.units|study_respondent), REML=T, control=strictControl, data=cor.analysis))
# 
# prereg.models.extra <- c(robustness.analyses, prereg.model.extra1, prereg.model.extra2)
# save(prereg.models.extra, file="extra.preregistered.models.RData")


# Preregistered Q2 ----------------------------------------------------------------------
# moderators <- c("female","polknow","age","alcohol","student","leftright","cynicism",
# "political_interest","votechoice_lr","education")
# DVs <- c("cor.hampel.increase", "cor.increase.pc.win", "zyg.increase.pc", "zyg.increase.pc.win")
# 
# options <- expand.grid(DVs,treatment.chars, moderators)
# 
# moderator.analysis <- function(dv, treatment, moderator){
#   if(substr(dv,1,3)=="zyg" & treatment=="emotion"){formula <- as.formula(paste0(dv, "~ 1 +", treatment, "+", moderator, "+", moderator,"*",treatment, "+sync.units + syncs + (sync.units|study_respondent)"))
#   } else{formula <- as.formula(paste0(dv, "~ 1 +", treatment, "+", moderator, "+", moderator,"*",treatment,  "+ sync.units + syncs + location + temperature + (sync.units|study_respondent)"))
#   }
#   if(substr(dv,1,3)=="cor"){
#     if(str_sub(dv,-3, -1)=="win"){model <- lmer(formula, REML=T, control=strictControl,data=cor.analysis)
#     } else {model <- lmer(formula, REML=T, control=strictControl,data=cor.analysis, subset=cor.stat.outlier==0)
#     }
#   } else if(str_sub(dv,-3, -1)=="win"){model <- lmer(formula, REML=T, control=strictControl,data=zyg.analysis)
#   } else {model <- lmer(formula, REML=T, control=strictControl,data=zyg.analysis, subset=zyg.stat.outlier==0)
#   }
#   return(model)
# }
# 
# #moderator.analyses.politics <- list()
# #for(i in 1:dim(pols)[1]){
# #  moderator.analyses.politics[[i]] <- moderator.analysis(dv=pols[i,1],treatment=pols[i,2], moderator=pols[i,3])
# #}
# 
# pols <- options[which(options$Var2=="political"),]
# moderator.analyses.politics <- lapply(1:dim(pols)[1], function(x) moderator.analysis(dv=options[x,1],treatment=options[x,2], moderator=options[x,3]))
# face <- options[which(options$Var2=="face"),]
# moderator.analyses.face<- lapply(1:dim(face)[1], function(x) moderator.analysis(dv=face[x,1],treatment=face[x,2], moderator=face[x,3]))
# emotion <- options[which(options$Var2=="emotion"),]
# moderator.analyses.emotion <- lapply(1:dim(emotion)[1], function(x) moderator.analysis(dv=emotion[x,1],treatment=emotion[x,2], moderator=emotion[x,3]))
# 
# #save(moderator.analyses.politics, file="moderator.analyses.politics.RData")
# #save(moderator.analyses.face, file="moderator.analyses.face.RData")
# #save(moderator.analyses.emotion, file="moderator.analyses.emotion.RData")
# 
# #save(moderator.analyses, file="moderator.analyses.RData")
# 
# selection <- options[which(options$Var2!="political" & options$Var2!="face"),]
# selection <- selection[which(selection$Var3%in%c("female", "age", "alcohol", "student", "leftright", "education")),]
# moderator.analyses.selection <- list()
# for(i in 1:16){
#   moderator.analyses.selection[[i]] <- moderator.analysis(dv=selection[i,1],treatment=selection[i,2], moderator=selection[i,3])
#   print(i)
# }  
# 
# save(moderator.analyses.selection, file="moderator.analyses.selection.RData")

# Correction to preregistered model Q1: -----------------
# Because the preregistered models didn't converge. We ran slightly simpler models (taking out the time higher level) and report them here. The code that was commented out was the original preregistered code
treatment.chars <- c("political", "label", "face","emotion", "sound", "lab")
DVs <- c("cor.hampel.increase", "cor.increase.pc.win", "zyg.increase.pc", "zyg.increase.pc.win")
options <- expand.grid(DVs, treatment.chars)

main.analysis <- function(dv, treatment){
  if(substr(dv,1,3)=="zyg" & treatment=="emotion"){formula <- as.formula(paste0(dv, "~ 1 +", treatment, "+ sync.units + syncs + (1|study_respondent)"))
  } else{formula <- as.formula(paste0(dv, "~ 1 +", treatment, "+ sync.units + syncs + location + temperature + (1|study_respondent)"))
  }
  
  if(substr(dv,1,3)=="cor"){
    if(str_sub(dv,-3, -1)=="win"){model <- lmer(formula, REML=T, control=strictControl,data=cor.analysis)
    } else {model <- lmer(formula, REML=T, control=strictControl,data=cor.analysis, subset=cor.stat.outlier==0)
    }
  } else if(str_sub(dv,-3, -1)=="win"){model <- lmer(formula, REML=T, control=strictControl,data=zyg.analysis)
  } else {model <- lmer(formula, REML=T, control=strictControl,data=zyg.analysis, subset=zyg.stat.outlier==0)
  }
  return(model)
}


robustness.check2 <- list()
for(i in 1:24){
  robustness.check2[[i]] <- main.analysis(dv=options[i,1],treatment=options[i,2])
  print(i)
}

save(robustness.check2, file="Analysis/main.analyses_correction.RData")


# Correction to preregistered model Q2: -----------------
moderators <- c("female","polknow","age","alcohol","student","leftright","cynicism",
                "political_interest","votechoice_lr","education")
DVs <- c("cor.hampel.increase", "cor.increase.pc.win", "zyg.increase.pc", "zyg.increase.pc.win")

options <- expand.grid(DVs,treatment.chars, moderators)

moderator.analysis <- function(dv, treatment, moderator){
  if(substr(dv,1,3)=="zyg" & treatment=="emotion"){formula <- as.formula(paste0(dv, "~ 1 +", treatment, "+", moderator, "+", moderator,"*",treatment, "+sync.units + syncs + (1|study_respondent)"))
  } else{formula <- as.formula(paste0(dv, "~ 1 +", treatment, "+", moderator, "+", moderator,"*",treatment,  "+ sync.units + syncs + location + temperature + (1|study_respondent)"))
  }
  if(substr(dv,1,3)=="cor"){
    if(str_sub(dv,-3, -1)=="win"){model <- lmer(formula, REML=T, control=strictControl,data=cor.analysis)
    } else {model <- lmer(formula, REML=T, control=strictControl,data=cor.analysis, subset=cor.stat.outlier==0)
    }
  } else if(str_sub(dv,-3, -1)=="win"){model <- lmer(formula, REML=T, control=strictControl,data=zyg.analysis)
  } else {model <- lmer(formula, REML=T, control=strictControl,data=zyg.analysis, subset=zyg.stat.outlier==0)
  }
  return(model)
}

selection <- options[which(options$Var2%in%c("emotion")),]
selection <- selection[which(selection$Var3%in%c("female", "age", "leftright", "education")),]
selection <- selection[which(selection$Var1%in%c("cor.hampel.increase", "zyg.increase.pc")),]

moderator.analyses.selection <- list()
for(i in 1:8){
  moderator.analyses.selection[[i]] <- moderator.analysis(dv=selection[i,1],treatment=selection[i,2], moderator=selection[i,3])
  print(i)
}  

#save(moderator.analyses.selection, file="moderator.analyses_correction.RData")
save(moderator.analyses.selection, file="Analysis/moderator.analyses_selection.RData")

# Moderator direct effects --------------------
moderators <- c("female", "age", "student", "alcohol", "leftright", "education")
options <- expand.grid(DVs, moderators)

m1 <- lmer(cor.hampel.increase ~ female + age + alcohol + leftright + education + location + sync.units + syncs + (1|study_respondent), data=cor.analysis, REML=T, control=strictControl, subset=cor.stat.outlier==0)
m2 <- lmer(cor.increase.pc.win ~ female + age + alcohol + leftright + education + location + sync.units + syncs + (1|study_respondent), data=cor.analysis, REML=T, control=strictControl)
m3 <- lmer(zyg.increase.pc ~ female + age + alcohol + leftright + education + location  + sync.units + syncs + (1|study_respondent), data=zyg.analysis, REML=T, control=strictControl, subset=zyg.stat.outlier==0)
m4 <- lmer(zyg.increase.pc.win ~ female + age +  alcohol + leftright + education + location + sync.units + syncs + (1|study_respondent), data=zyg.analysis, REML=T, control=strictControl)

#outdir <- "C:/Users/gschuma1/Dropbox/Apps/Overleaf/Doing Psychophysiology research in political science"

texreg(list(m1,m2,m3,m4),
       file=paste0(outdir,"/Tables/moderator_direct.tex"),
       stars=0.05,
       custom.model.names=c("Corrugator", "Cor(winsorized)", "Zygomaticus", "Zyg(winsorized)"),
       custom.coef.names = c("Intercept","Female", "Age", "Alcohol", "leftright", "Sec voc vs sec","High voc vs sec", "University vs sec", "EO", "Lab", "Lowlands","Nijmegen", "Tilburg", "TT Assen", "seconds in exp", "seconds in treatment"),
       caption="Multilevel regression models measuring impact participant characteristics on fEMG",
       caption.above=TRUE,
       label="app:moderator_direct",
       digits=3,
       float.pos="!htbp")


# Restricted set
selection <- unique(cor.analysis$condition2[cor.analysis$location=="Nijmegen" & cor.analysis$datacollection=="iaps"])
subset <- cor.analysis[which(cor.analysis$condition2%in%selection & cor.analysis$study=="Lab 19"),]

res.m1 <- lmer(cor.hampel.increase ~  female + age + alcohol + education + sync.units + syncs + location + (1|study_respondent), data=subset, REML=T, control=strictControl, subset=cor.stat.outlier==0)
res.m2 <- lmer(cor.increase.pc.win ~ female + age + alcohol + education + sync.units + syncs + (1|study_respondent), data=cor.analysis, REML=T, control=strictControl)

#outdir <- "C:/Users/gschuma1/Dropbox/Apps/Overleaf/Doing Psychophysiology research in political science"


#### Additional analyses ------------------
summary(lmer(cor.hampel.increase ~ emotion*leftright + sync.units + syncs + location + temperature + (1|study_respondent), REML=T, control=strictControl,data=cor.analysis, subset=cor.stat.outlier==0))

summary(lmer(cor.hampel.increase ~ 1 + (1|study_respondent), REML=T, control=strictControl,data=cor.analysis, subset=cor.stat.outlier==0)) # get within-subject SD = 32.63
summary(lmer(zyg.increase.pc ~ 1 + (1|study_respondent), REML=T, control=strictControl,data=zyg.analysis, subset=zyg.stat.outlier==0)) # get within-subject SD = 95.82
summary(lmer(cor.increase.pc.win ~ 1 + (1|study_respondent), REML=T, control=strictControl,data=cor.analysis)) # get within-subject SD = 23.84
summary(lmer(zyg.increase.pc.win ~ 1 + (1|study_respondent), REML=T, control=strictControl,data=zyg.analysis)) # get within-subject SD = 22.05

