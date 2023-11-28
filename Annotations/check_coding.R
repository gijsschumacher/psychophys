library(readxl)
library(icr)
library(stringr)
library(dplyr)

# Load data ---------------------------------------------------------------
coder1_cor <- read_excel("Annotations/codingalltreatments_coder1.xlsx", sheet="cor")
coder1_labii <- read_excel("Annotations/codingalltreatments_coder1.xlsx", sheet="labii")

coder2_labii <- read_excel("Annotations/codingalltreatments_coder2.xlsx", sheet="labii")
coder2_cor <- read_excel("Annotations/codingalltreatments_coder2.xlsx", sheet="cor")
coder2_zyg <- read_excel("Annotations/codingalltreatments_coder2.xlsx", sheet="zyg")
coder2_scl <- read_excel("Annotations/codingalltreatments_coder2.xlsx", sheet="scl_new")

coder3_zyg <- read_excel("Annotations/codingalltreatments_coder3.xlsx", sheet="zyg")
coder3_scl <- read_excel("Annotations/codingalltreatments_coder3.xlsx", sheet="scl")

coder4_zyg <- read_excel("Annotations/codingalltreatments_coder4.xlsx", sheet="zyg")
coder4_scl <- read_excel("Annotations/codingalltreatments_coder4.xlsx", sheet="scl new")


# Comparison corrugator ---------------------------------------------------
#respondents <- str_sub(list.files(path="timeplots/cor/"),start=6, end=-5)
load("Annotations/coded_respondents.RData")

coder1 <- ifelse(respondents%in%coder1_cor$study_respondent,1,0)
coder2 <- ifelse(respondents%in%coder2_cor$study_respondent,1,0)

cor.coding <- rbind(coder1, coder2)
krippalpha(cor.coding, metric="nominal") # alpha = 0.884, n=585

errors <- which(apply(cor.coding,2, sum)==2) # both identified as having problem
deviants <- which(apply(cor.coding,2, sum)==1) # only identified as having a problem
respondents[deviants] # cross-check results
# based on comparing results: Lab 20_199 seems to be coded as an error by mistake; Lowlands_8 spike at end is no problem; Lowlands_69 only log book shows error, but line is normal
# so keep these and throw out others.

coder1_code <- ifelse(respondents%in%coder1_cor$study_respondent,
                 coder1_cor$visual_error,0)
coder2_code <- ifelse(respondents%in%gijs_cor$study_respondent,
                 gijs_cor$visual_error_code,0)

cor.errors <- data.frame(respondents, coder1_code, coder2_code)
cor.errors[which(cor.errors$respondents%in%c("Lab 20_199", "Lowlands_8", "Lowlands_69")),2:3] <- matrix(0, 3, 2) # see notes lines 31-32 (above)
cor.errors$cor.error.bigchange <- ifelse(coder1_code==1 & coder2_code==1,2,
                                  ifelse(coder1_code==1 | coder2_code==1,1,0))
cor.errors$cor.error.nochange <- ifelse(coder1_code==2 & coder2_code==2,2,
                                         ifelse(coder1_code==2 | coder2_code==2,1,0))
cor.errors$cor.error.other <- ifelse(coder1_code==3 & coder2_code==3,2,
                                        ifelse(coder1_code==3 | coder2_code==3,1,0))
cor.errors$cor.n.errors <- cor.errors$cor.error.bigchange + cor.errors$cor.error.nochange + cor.errors$cor.error.other
colnames(cor.errors)[1] <- "study_respondent"

# Comparison labii --------------------------------------------------------
respondents <- str_sub(list.files(path="timeplots/labii/"),start=7, end=-5)

coder1 <- ifelse(respondents%in%coder1_labii$study_respondent,1,0)
coder2 <- ifelse(respondents%in%gijs_labii$study_respondent,1,0)

labii.coding <- rbind(coder1, coder2)
krippalpha(labii.coding, metric="nominal") # alpha = 0.818, n=95

errors <- which(apply(labii.coding,2, sum)==2) # both identified as having problem
deviants <- which(apply(labii.coding,2, sum)==1) # only identified as having a problem
respondents[deviants] # cross-check results
# based on comparing results: Lab 19_26 seems to be normal
# so keep these and throw out others.

coder1_code <- ifelse(respondents%in%coder1_labii$study_respondent,
                      coder1_labii$visual_error,0)
coder2_code <- ifelse(respondents%in%coder2_labii$study_respondent,
                      coder2_labii$visual_error_code,0)

labii.errors <- data.frame(respondents, coder1_code, coder2_code)
labii.errors[which(labii.errors$respondents%in%c("Lab 19_26")),2:3] <- matrix(0, 1, 2) # see notes lines 31-32 (above)
labii.errors$labii.error.bigchange <- ifelse(coder1_code==1 & coder2_code==1,2,
                                         ifelse(coder1_code==1 | coder2_code==1,1,0))
labii.errors$labii.error.nochange <- ifelse(coder1_code==2 & coder2_code==2,2,
                                        ifelse(coder1_code==2 | coder2_code==2,1,0))
labii.errors$labii.error.other <- ifelse(coder1_code==3 & coder2_code==3,2,
                                     ifelse(coder1_code==3 | coder2_code==3,1,0))
labii.errors$labii.n.errors <- labii.errors$labii.error.bigchange + labii.errors$labii.error.nochange + labii.errors$labii.error.other

colnames(labii.errors)[1] <- "study_respondent"

# Comparison zygomaticus  --------------------------------------------------------
respondents <- str_sub(list.files(path="timeplots/zyg/"),start=5, end=-5)

# coder4 also coded extremely high peaks, but coder3 did not. Take out the high peak ones.
coder4_respondents <- coder4_zyg$study_respondent[which(coder4_zyg$visual_error_code!=3)]

coder1 <- ifelse(respondents%in%coder4_respondents,1,0) 
coder2 <- ifelse(respondents%in%coder3_zyg$study_respondent,1,0)
coder3 <- ifelse(respondents%in%coder2_zyg$study_respondent,1,0)

zyg.coding <- rbind(coder1, coder2,coder3)
krippalpha(zyg.coding[c(1,3),], metric="nominal") # code alpha obtained from comparing these, not others.
### alpha = 0.778
coder1_code <- ifelse(respondents%in%coder4_respondents,
                      coder4_zyg$visual_error_code,0)
coder2_code <- ifelse(respondents%in%coder2_zyg$study_respondent,
                      coder2_zyg$visual_error_code,0)

zyg.errors <- data.frame(respondents, coder1_code, coder2_code)
zyg.errors[which(zyg.errors$respondents%in%c("Lab 19_26")),2:3] <- matrix(0, 1, 2) # see notes lines 31-32 (above)
zyg.errors$zyg.error.bigchange <- ifelse(coder1_code==1 & coder2_code==1,2,
                                             ifelse(coder1_code==1 | coder2_code==1,1,0))
zyg.errors$zyg.error.nochange <- ifelse(coder1_code==2 & coder2_code==2,2,
                                            ifelse(coder1_code==2 | coder2_code==2,1,0))
zyg.errors$zyg.error.other <- ifelse(coder1_code==3 & coder2_code==3,2,
                                         ifelse(coder1_code==3 | coder2_code==3,1,0))
zyg.errors$zyg.n.errors <- zyg.errors$zyg.error.bigchange + zyg.errors$zyg.error.nochange + zyg.errors$zyg.error.other

colnames(zyg.errors)[1] <- "study_respondent"

# What about coder4's third error code?
coder4_extreme_values1 <- coder4_zyg$study_respondent[which(coder4_zyg$visual_error_code==3)]
coder4_extreme_values2 <- coder4_zyg$study_respondent[which(coder4_zyg$highpeaks==1)]

zyg.errors$zyg.high.peak <- ifelse(zyg.errors$study_respondent%in%coder4_extreme_values2,1,0)


# Write EMG data ----------------------------------------------------------
emg <- left_join(emg, cor.errors[,-c(2,3)], by="study_respondent")
emg <- left_join(emg, zyg.errors[,-c(2,3)], by="study_respondent")
emg <- left_join(emg, labii.errors[,-c(2,3)], by="study_respondent")

cor.mean.werrors <- mean(emg$cor.increase.pc[emg$cor.n.errors<1], na.rm=T)
cor.sd.werrors <- sd(emg$cor.increase.pc[emg$cor.n.errors<1], na.rm=T)
cor.temp <- (emg$cor.increase.pc-cor.mean.werrors) / cor.sd.werrors
cor.stat.outlier <- ifelse(cor.temp>4,1,0)

emg$cor.stat.outlier <- cor.stat.outlier

zyg.mean.werrors <- mean(emg$zyg.increase.pc[emg$zyg.n.errors<1], na.rm=T)
zyg.sd.werrors <- sd(emg$zyg.increase.pc[emg$zyg.n.errors<1], na.rm=T)
zyg.temp <- (emg$zyg.increase.pc-zyg.mean.werrors) / zyg.sd.werrors
zyg.stat.outlier <- ifelse(zyg.temp>4,1,0)

emg$zyg.stat.outlier <- zyg.stat.outlier

save(emg, file="All Syncs/emg.RData")

# Comparison scl  --------------------------------------------------------
load("All Syncs/scl.RData")
respondents <- str_sub(list.files(path="timeplots/scl/"),start=5, end=-5)

#coder1 <- ifelse(respondents%in%coder3_scl$study_respondent[coder3_scl$visual_error_code==2],1,0)
coder2 <- ifelse(respondents%in%coder4_scl$study_respondent[coder4_scl$noresponse==1|  coder4_scl$`deep drops`==1],1,0)
coder3 <- ifelse(respondents%in%coder2_scl$study_respondent[coder2_scl$noresponse==1|  coder2_scl$`deep drops`==1],1,0)

autocoder1 <- ifelse(respondents%in%unique(scl$study_respondent[scl$scl<=0]),1,0) # to detect no response and deep drops
#temp <- dplyr::ddply(scl, .(study_respondent), summarize, cor(scl, scl.ma.5sec, use="pairwise.complete.obs"))
#autocoder2 <- ifelse(respondents%in%temp[which(temp[,2]<0.8),1],1,0)

respondents <- str_sub(list.files(path="timeplots/scl/"),start=5, end=-5)
respondents.subset <- respondents[1:443] # omit lowlands data here
coder2 <- ifelse(respondents.subset%in%coder4_scl$study_respondent[coder4_scl$noresponse==1|  coder4_scl$`deep drops`==1],1,0)
coder3 <- ifelse(respondents.subset%in%coder2_scl$study_respondent[coder2_scl$noresponse==1|  coder2_scl$`deep drops`==1],1,0)

scl.coding <- rbind(coder2, coder3) 
krippalpha(scl.coding[c(1,2),], metric="nominal")# k=0.88 with automated method.

coder1_code <- ifelse(respondents%in%coder4_scl$study_respondent,
                      coder4_scl$visual_error_code,0)
coder2_code <- ifelse(respondents%in%coder2_scl$study_respondent,
                      coder2_scl$visual_error_code,0)

scl.errors <- data.frame(respondents, coder1_code, coder2_code)

scl.errors$scl.error.nochange <- ifelse(coder1_code==1 & coder2_code==1,2,
                                         ifelse(coder1_code==1 | coder2_code==1,1,0))
scl.errors$scl.error.noise <- ifelse(coder1_code==2 & coder2_code==2,2,
                                        ifelse(coder1_code==2 | coder2_code==2,1,0))
scl.errors$scl.error.other <- ifelse(coder1_code>2 & coder2_code>2,2,
                                     ifelse(coder1_code>2 | coder2_code>2,1,0))
scl.errors$scl.n.errors <- scl.errors$scl.error.nochange + scl.errors$scl.error.other + scl.errors$scl.error.noise

colnames(scl.errors)[1] <- "study_respondent"
scl <- left_join(scl, scl.errors[,-c(2,3)], by="study_respondent")

save(scl, file="C:/Users/gschuma1/surfdrive/Papers/Multimodal Physiology/All Syncs/scl.RData")
