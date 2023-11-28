library(ggplot2)
library(sjmisc)
library(emmeans)
library(sjPlot)
library(stringr)

#outdir <- "C:/Users/gschuma1/Dropbox/Apps/Overleaf/Doing Psychophysiology research in political science"

# Q1 make figure ----------------------------------------------------------
load("Analysis/main.analyses_correction.RData")
coefs <- lapply(robustness.check2, function(x){
  if(colnames(x@frame)[2]%in%c("label", "emotion")){
    output <- rbind(c(coef(summary(x))[2,1],coef(summary(x))[2,2],coef(summary(x))[2,5]),
                    c(coef(summary(x))[3,1],coef(summary(x))[3,2],coef(summary(x))[3,5]))
    } else {output <- c(coef(summary(x))[2,1],coef(summary(x))[2,2],coef(summary(x))[2,5])}
  return(output)
})

coefs <- do.call("rbind", coefs)
coefs <- data.frame(coefs)
colnames(coefs) <- c("b", "se", "p")
ex.options <- rbind(options[1:4,],
                    options[c(5,5,6,6,7,7,8,8),],
                    options[9:12,],
                    options[c(13,13,14,14,15,15,16,16),], # squeeze in two factors
                    options[17:24,])
coefs <- data.frame(coefs, ex.options)
rownames(coefs) <- NULL
coefs$temp1 <- ifelse(rownames(coefs)%in%c("18","20","22","24"),1,0)
coefs$temp2 <- ifelse(rownames(coefs)%in%c("6","8","10","12"),1,0)

coefs$lo <- coefs$b - 1.96*coefs$se
coefs$hi <- coefs$b + 1.96*coefs$se
coefs$dv <- ifelse(str_sub(as.character(coefs$Var1),1,3)=="cor","DV: Corrugator", "DV: Zygomaticus")
coefs$transform <- ifelse(str_sub(as.character(coefs$Var1),-3,-1)=="win", "Win", "Out")  
coefs$labels <- ifelse(coefs$Var2=="political", "IV: Political/\nNot political",
                ifelse(coefs$Var2=="label" & coefs$temp2==0, "IV: Video/\nImage",
                ifelse(coefs$Var2=="label" & coefs$temp2==1, "IV: Word/\nImage",       
                ifelse(coefs$Var2=="face", "IV: Face/\nIssue",
                ifelse(coefs$Var2=="sound", "IV: Sound/\nNo sound",
                ifelse(coefs$Var2=="lab", "IV: Lab/\nFieldlab",
                ifelse(coefs$Var2=="emotion" & coefs$temp1==0, "IV: Positive/\nNeutral",       
                ifelse(coefs$Var2=="emotion" & coefs$temp1==1, "IV:Negative/\nNeutral",coefs$Var2))))))))
coefs$labels <- factor(coefs$labels, levels=c("IV: Lab/\nFieldlab", "IV:Negative/\nNeutral", "IV: Positive/\nNeutral", "IV: Video/\nImage", "IV: Word/\nImage", "IV: Sound/\nNo sound", "IV: Political/\nNot political","IV: Face/\nIssue"))

main.analyses.plot <- ggplot(coefs, aes(x=transform, y=b)) +
  geom_point() + 
  geom_errorbar(aes(ymin=lo, ymax=hi), width=.2) +
  facet_grid(dv~labels, scales="free") + ylab("Character effect") + xlab("")

ggsave(main.analyses.plot, file=paste0(outdir,"/Figures/main.analyses.plot.jpg"), dpi=900)

coefs.subset <- coefs[1:28,]
subset.analyses.plot <- ggplot(coefs.subset, aes(x=transform, y=b)) +
  geom_point() + 
  geom_errorbar(aes(ymin=lo, ymax=hi), width=.2) + geom_hline(yintercept=0, colour="red") +
  facet_grid(dv~labels, scales="free") + ylab("Characteristic effect expressed as microVolt change from baseline") + xlab("")

ggsave(subset.analyses.plot, file=paste0(outdir,"/Figures/subset.analyses.plot.pdf"))

# Q3: Moderators ---------------------------------------------------
load("Analysis/moderator.analyses_selection.RData")
result <- list()

for(i in 1:length(moderator.analyses.selection)){
  if(i<3){
    means <- emmeans(moderator.analyses.selection[[i]],~female*emotion)
    result[[i]] <- data.frame(contrast(means,"revpairwise",by="emotion"))[,c(2,3,4)]}
  if(i%in%c(3,4)){result[[i]] <- data.frame(emtrends(moderator.analyses.selection[[i]],~emotion, var="age"))[,c(1,2,3)]}
  if(i%in%c(5,6)){result[[i]] <- data.frame(emtrends(moderator.analyses.selection[[i]],~emotion, var="leftright"))[,c(1,2,3)]}
  if(i%in%c(7,8)){
    means <- emmeans(moderator.analyses.selection[[i]],~education*emotion)
    result[[i]] <- data.frame(contrast(means,"revpairwise",by="emotion"))[c(1,3,5,7,9,11,13,15,17),c(2,3,4)]}
  colnames(result[[i]]) <- c("emotion","estimate","se")
  result[[i]]$var <- selection[i,3]
  }

#### make emotional content plot -----------------------
emotion.analysis <- dplyr::bind_rows(result)
emotion.analysis$x <- factor(ifelse(emotion.analysis$emotion==0, "Neutral",
                      ifelse(emotion.analysis$emotion==1, "Positive", "Negative")), levels=c("Negative", "Neutral", "Positive"))
emotion.analysis$group <- c(rep("Female\nvs Male",6),
                            rep("Age",6),rep("Left-\nRight",6), 
                            rep(c("Sec vs\nSec Voc","High vs\nSec Voc","Uni vs\nSec Voc"),6))
emotion.analysis$conf.lo <- emotion.analysis$estimate - 1.96*emotion.analysis$se
emotion.analysis$conf.hi <- emotion.analysis$estimate + 1.96*emotion.analysis$se
  

emotion.analysis$group <- factor(emotion.analysis$group,
                          levels=c("Age","Female\nvs Male","Left-\nRight", "Sec vs\nSec Voc","High vs\nSec Voc","Uni vs\nSec Voc"))
emotion.analysis$x <- ifelse(emotion.analysis$x=="Negative","neg",
                      ifelse(emotion.analysis$x=="Positive","pos","neu"))
emotion.analysis$dv <- c(rep("Corrugator",3),rep("Zygomaticus",3),
                    rep("Corrugator",3),rep("Zygomaticus",3),
                    rep("Corrugator",3),rep("Zygomaticus",3),
                    rep("Corrugator",9),rep("Zygomaticus",9))
#emotion.analysis$iv <- c(rep("female",12), rep("age",12), rep("ideology",8), rep("education",24))
#emotion.analysis$x2 <- paste0(emotion.analysis$group,"\n",emotion.analysis$x)

emotion <- ggplot(data=emotion.analysis, aes(x=x, y=estimate)) +
  geom_point(position = position_dodge(width=1)) + 
  geom_errorbar(data=emotion.analysis, aes(ymin=conf.lo, ymax=conf.hi), position = position_dodge(width=1), width=.2) +  
  facet_grid(dv~group, scales="free") +
  xlab("Treatment characteristics") +
  ylab("Marginal effect") +
  geom_hline(yintercept=0, colour="red") +
  theme(legend.position="bottom",
        legend.title=element_blank(),
        axis.text.x=element_text(size=6)
        )


ggsave(emotion, file=paste0(outdir,"/Figures/emotion.pdf"), dpi=900)
### formal hypothesis test interactions ----------------

model <- moderator.analyses.selection[[21]] # age-corrugator
t1 <- emtrends(model, pairwise ~  emotion, var="age") # no difference between treatment across range of agreeableness

model <- moderator.analyses.selection[[85]] # education-corrugator
t1 <- emmeans(model, pairwise ~education*emotion) 

model <- moderator.analyses.selection[[5]] # education-corrugator
t1 <- emmeans(model, pairwise ~female*emotion) 

model <- moderator.analyses.selection[[69]] # education-corrugator
t1 <- emtrends(model, pairwise ~emotion, var="leftright")

emmip(model, age ~  emotion, cov.reduce=range)

(mylist <- list(age=c(16,83),emotion=c(0,1,2)))
t2  <- emmeans(model, ~age*emotion, at=mylist)
t3 <- contrast(t2, "pairwise",by=names(mylist)[1]) # do low agreeable respond more strongly to one of the treatments? 
t4 <- contrast(t2, "pairwise",by=names(mylist)[2]) 

#categorical
t2  <- emmeans(model, ~age*emotion)
contrast(t2, "revpairwise",by="age",adjust="none")
contrast(t2, "revpairwise",by="emotion",adjust="none")




#### Remainder -------------------------
# plot_model(moderator.analyses.politics[[5]], type="pred", terms=c("polknow", "political [0,1]"), title="Corrugator Activity across political knowledge", axis.title=c("Political knowledge", "Activity"))
# plot_model(moderator.analyses.politics[[7]], type="pred", terms=c("polknow", "political [0,1]"), title="Zygomaticus Activity across political knowledge", axis.title=c("Political knowledge", "Activity"))
# 
# plot_model(moderator.analyses.politics[[13]], type="pred", terms=c("alcohol", "political [0,1]"), title="Corrugator Activity across alcohol promillage", axis.title=c("Promillage", "Activity"))
# #plot_model(moderator.analyses.politics[[15]], type="pred", terms=c("alcohol", "political [0,1]"), title="Zygomaticus Activity across political knowledge", axis.title=c("Promillage", "Activity"))
# 
# plot_model(moderator.analyses.politics[[21]], type="pred", terms=c("leftright", "political [0,1]"),title="Corrugator Activity across left-right", axis.title=c("Left-Right", "Activity"))
# plot_model(moderator.analyses.politics[[23]], type="pred", terms=c("leftright", "political [0,1]"),title="Zygomaticus Activity across left-right", axis.title=c("Left-Right", "Activity"))
# 
# plot_model(moderator.analyses.politics[[25]], type="pred", terms=c("cynicism", "political [0,1]"),title="Corrugator Activity across cynicism", axis.title=c("Cynicism", "Activity"))
# plot_model(moderator.analyses.politics[[27]], type="pred", terms=c("cynicism", "political [0,1]"),title="Zygomaticus Activity across cynicism", axis.title=c("Cynicism", "Activity"))
# 
# plot_model(moderator.analyses.politics[[29]], type="pred", terms=c("political_interest", "political [0,1]"),title="Corrugator Activity across political interest", axis.title=c("Political interest", "Activity"))
# plot_model(moderator.analyses.politics[[31]], type="pred", terms=c("political_interest", "political [0,1]"),title="Zygomaticus Activity across political interest", axis.title=c("Political interest", "Activity"))
# 
# plot_model(moderator.analyses.politics[[33]], type="pred", terms=c("votechoice_lr [Center,Left, Right,Other]", "political [0,1]"),title="Corrugator Activity for different voters", axis.title=c("Vote", "Activity"))
# plot_model(moderator.analyses.politics[[35]], type="pred", terms=c("votechoice_lr [Center,Left, Right,Other]", "political [0,1]"),title="Zygomaticus Activity for different voters", axis.title=c("Vote", "Activity"))
# 
# plot_model(moderator.analyses.politics[[37]], type="pred", terms=c("education", "political [0,1]"),title="Corrugator Activity across education levels", axis.title=c("Education level", "Activity"))
# plot_model(moderator.analyses.politics[[39]], type="pred", terms=c("education", "political [0,1]"),title="Zygomaticus Activity across education levels", axis.title=c("Education level", "Activity"))
# 
# 
# plot_model(moderator.analyses.face[[5]],type="pred", terms=c("polknow", "face [0,1]"), title="Corrugator Activity across political knowledge", axis.title=c("Political knowledge", "Activity"))
# plot_model(moderator.analyses.face[[7]], type="pred", terms=c("polknow", "face [0,1]"), title="Zygomaticus Activity across political knowledge", axis.title=c("Political knowledge", "Activity"))
# 
# 
# plot_model(moderator.analyses.face[[29]], type="pred", terms=c("political_interest", "face [0,1]"),title="Corrugator Activity across political interest", axis.title=c("Political interest", "Activity"))
# plot_model(moderator.analyses.face[[31]], type="pred", terms=c("political_interest", "face [0,1]"),title="Zygomaticus Activity across political interest", axis.title=c("Political interest", "Activity"))
# 
# 
# plot_model(moderator.analyses.face[[33]], type="pred", terms=c("votechoice_lr [Center,Left, Right,Other]", "face [0,1]"),title="Corrugator Activity for different voters", axis.title=c("Vote", "Activity"))
# plot_model(moderator.analyses.face[[35]], type="pred", terms=c("votechoice_lr [Center,Left, Right,Other]", "face [0,1]"),title="Zygomaticus Activity for different voters", axis.title=c("Vote", "Activity"))
# 
# plot_model(moderator.analyses.face[[37]], type="pred", terms=c("education", "face [0,1]"),title="Corrugator Activity across education levels", axis.title=c("Education level", "Activity"))
# plot_model(moderator.analyses.face[[39]], type="pred", terms=c("education", "face [0,1]"),title="Zygomaticus Activity across education levels", axis.title=c("Education level", "Activity"))
# 
# check.models <- c(5,7,21,23,25,27,29,31,33,35,37,39)
# labels <- rep(c("polknow", "leftright","cynicism","political_interest", "votechoice_lr", "education"), each=2)
# plot_model(moderator.analyses.emotion[[check.models[12]]], type="pred", terms=c(labels[[11]], "emotion[0,1,2]"),title="Zygomaticus Activity across education", axis.title=c("Education", "Activity"))
