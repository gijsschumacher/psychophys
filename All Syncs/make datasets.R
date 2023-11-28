rm(list=ls())
library(stringr)
library(readxl)
library(car)


# Integrate corrugator and zygomaticus data ------------------------------------------------------------
# Here we are loading the raw data, I made some changes here because the original folders have some more raw data than what is published
#csvs <- list.files(list.dirs()[c(4,8,12,16)], pattern="*.csv", full.names=TRUE)
csvs <- list.files(list.dirs(), pattern="*.csv", full.names=TRUE)
types <- str_split_fixed(str_split_fixed(csvs,"/",4)[,4],"[.]",2)[,1]

emg1 <- do.call("rbind",lapply(csvs[which(types=="EMG1_RAW_1")], function(x){
                    data <- read.csv(x)
                    data$study <- str_split_fixed(x, "/", 4)[,3]
                    return(data)}))

emg2 <- do.call("rbind",lapply(csvs[which(types=="EMG2_RAW_1")], function(x){
                   data <- read.csv(x)
                   data$study <- str_split_fixed(x, "/", 4)[,3]
                   return(data)}))


emg <- merge(emg1,emg2, by=c("study", "respondent", "syncs", "sync.units"), all.x=TRUE)

emg <- emg[,-c(5,7,8,10)]
colnames(emg)[5:6] <- c("cor", "zyg")
emg$study <- ifelse(emg$study=="Lab 19-20","Lab 19", emg$study)
emg$study <- ifelse(emg$study=="Lab 19" & emg$respondent>=115,"Lab 20", emg$study)
emg$labii <- ifelse(emg$study=="Lab 19", emg$zyg, NA)
emg$zyg <- ifelse(emg$study=="Lab 19", NA, emg$zyg)
emg$study_respondent <- paste0(emg$study, "_", emg$respondent)
respondents <- unique(emg$study_respondent)
emg$total.sec <- unlist(lapply(respondents, function(x) seq(1,length(which(emg$study_respondent==x)),1)))


# Clean datasets ----------------------------------------------------------

# First delete cases with too many/too little syncs
check.n.syncs <- aggregate(emg$syncs, by=list(emg$study, emg$respondent), max)
check.n.syncs$max <- ifelse(check.n.syncs$Group.1=="Lowlands",10,
                            ifelse(check.n.syncs$Group.1=="Groot Nationaal Onderzoek",42, 
                                   ifelse(check.n.syncs$Group.1=="lab replication",40, 
                                          ifelse(check.n.syncs$Group.1=="Lab 19" & check.n.syncs$Group.2<51,64,
                                                 ifelse(check.n.syncs$Group.1=="Lab 19" & check.n.syncs$Group.2>50,148, 
                                                        ifelse(check.n.syncs$Group.1=="Lab 20",178,NA)))))) 
mismatch.resp <- check.n.syncs[which(check.n.syncs$max!=check.n.syncs$x),c(1,2)]
wrong.syncs.emg <- which(paste0(emg$study,"_",emg$respondent)%in%paste0(mismatch.resp$Group.1,"_",mismatch.resp$Group.2))

emg <- emg[-wrong.syncs.emg,]

emg <- emg[-which(emg$study_respondent%in%c("Lab 19_27", "Lab 20_131")),] # relevant data missing and deleted at some point

# Insert conditions -------------------------------------------------------
markers.lab20 <- read_excel(path="Markers/Markers mimicry.xlsx")
markers.lab20 <- markers.lab20[,-c(3,4,5,6,7)]
markers.lab20$study <- "Lab 20"  

markers.lab19 <- read_excel(path="Markers/Markers.xlsx")
markers.lab19 <- markers.lab19[,-c(8,9,10)]
markers.lab19$study <- "Lab 19"

markers.labreplication <- read_excel(path="Markers/Meaning of Markers.xlsx") 
markers.labreplication$study <- "lab replication"

markers.gno <- read_excel(path="Markers/Markers_gno.xlsx")
markers.gno <- markers.gno[,-c(2,3)]
markers.gno$study <- "Groot Nationaal Onderzoek"

markers.lowlands <- read.csv("Markers/markers.csv")
markers.lowlands[1,1] <- 1  
markers.lowlands[,1] <- as.numeric(markers.lowlands[,1])
markers.lowlands$study <- "Lowlands"
colnames(markers.lowlands)[1] <- "no"

markers <- rbind(markers.lab20,markers.lab19,markers.gno,markers.lowlands, markers.labreplication)
colnames(markers)[1] <- "syncs"
markers$label[which(markers$label=="photo")] <- "picture"

#save(markers, file="markers.RData")

emg <- merge(emg,markers,by=c("syncs","study"))


# Match conditions with lowlands, GNO and lab replication ------------------------------------------
# This part cannot be replicated because it takes all kinds of datasets from different places
load("C:/Users/gschuma1/surfdrive/Papers/Published Work/Anti-establishment project/Physiological data - combined/dataset.RData")

data_selection <- data_comb[,-c(8,9:17,20:30,41:52,56,58:60,62:68,70:73,78:86,93:94,102:152)]

data_selection$study <- ifelse(str_split_fixed(data_selection$respondent.study,"_",2)[,2]==1,"Lowlands", 
                               ifelse(str_split_fixed(data_selection$respondent.study,"_",2)[,2]==3,"Groot Nationaal Onderzoek", "lab replication"))
data_selection$study_respondent <- paste0(data_selection$study, "_", data_selection$respondent)

load("C:/Users/gschuma1/surfdrive/Papers/Published Work/Anti-establishment project/Lab Replication/Data & Analysis/full.dataset.RData")
temp <- full.dataset[[1]]
temp <- temp[,c(1,100:111,167)]
temp$study_respondent <- paste0("lab replication", "_", temp$respondent)

data_selection <- merge(data_selection, temp, by="study_respondent", all.x=TRUE)
data_selection$dom.first <- ifelse(data_selection$dom.type==0 & data_selection$which.pol==0,"Non-dom_Bodskov",
                                   ifelse(data_selection$dom.type==0 & data_selection$which.pol==1,"Non-dom_Haekkerup",
                                          ifelse(data_selection$dom.type==1 & data_selection$which.pol==0,"Dom_Bodskov","Dom_Haekkerup")))
data_selection$dom.second <- ifelse(data_selection$dom.type==0 & data_selection$which.pol==0,"Dom_Haekkerup",
                                    ifelse(data_selection$dom.type==0 & data_selection$which.pol==1,"Dom_Bodskov",
                                           ifelse(data_selection$dom.type==1 & data_selection$which.pol==0,"Non-dom_Haekkerup","Non-dom_Bodskov")))


    
    
load("C:/Users/gschuma1/surfdrive/Papers/Published Work/Disgust and Politics/Analysis/full.data.Rdata")
full.data <- full.data[,c(1:8,65:78,193:194,217,224:227,233:240,257:262,345:347)]
full.data$study <- "Lab 19"
full.data$study_respondent <- paste0(full.data$study, "_", full.data$respondent)
lab19 <- full.data


load("C:/Users/gschuma1/surfdrive/Papers/Published Work/Facial Mimicry/Analysis/full.data.RData") 
full.data <- full.data[,c(1:75,191:221)]

full.data$know1_correct<-ifelse(full.data$know1=="4", 1,0) #5 years
full.data$know2_correct<-ifelse(full.data$know2=="2", 1,0) #VVD
full.data$know3_correct<-ifelse(full.data$know3=="5 ", 1,0) #Juncker
full.data$know4_correct<-ifelse(full.data$know4=="2", 1,0) #Macron
full.data$know5_correct<-ifelse(full.data$know5=="1", 1,0) #Georgieva
full.data$know<-(full.data$know1_correct + full.data$know2_correct + full.data$know3_correct + full.data$know4_correct + full.data$know5_correct)/5

full.data$gender <- ifelse(full.data$Sex == "1", 0, ifelse(full.data$Sex == "2", 1,NA)) 
full.data$Age_1 <- as.numeric(full.data$Age_1) + 18

cols.num <- c("NfC_1","NfC_2","NfC_3","NfC_4","NfC_5","NfC_6","NfC_7","NfC_8","NfC_9","NfC_10")
full.data[cols.num] <- sapply(full.data[cols.num],as.numeric)
full.data$NfC_3 <- full.data$NfC_3*-1 + 8
full.data$NfC_4 <- full.data$NfC_4*-1 + 8
full.data$NfC_5 <- full.data$NfC_5*-1 + 8
full.data$NfC_6 <- full.data$NfC_6*-1 + 8
full.data$NfC_9 <- full.data$NfC_9*-1 + 8

full.data$NfC <- rowMeans(data.frame(full.data$NfC_1, full.data$NfC_2, full.data$NfC_3, full.data$NfC_4, full.data$NfC_5, full.data$NfC_6, full.data$NfC_7, full.data$NfC_8, full.data$NfC_9, full.data$NfC_10, na.rm=TRUE))

cols.num <- c("NfA_1","NfA_2","NfA_3","NfA_4","NfA_5","NfA_6","NfA_7","NfA_8","NfA_9","NfA_10")
full.data[cols.num] <- sapply(full.data[cols.num],as.numeric)
full.data$NfA_1  <- full.data$NfA_1*-1 + 8
full.data$NfA_5  <- full.data$NfA_1*-1 + 8
full.data$NfA_6  <- full.data$NfA_1*-1 + 8
full.data$NfA_7  <- full.data$NfA_1*-1 + 8
full.data$NfA_8  <- full.data$NfA_1*-1 + 8

full.data$NfA <- rowMeans(data.frame(full.data$NfA_1, full.data$NfA_2, full.data$NfA_3, full.data$NfA_4, full.data$NfA_5, full.data$NfA_6, full.data$NfA_7, full.data$NfA_8, full.data$NfA_9, full.data$NfA_10, na.rm=TRUE))
full.data$study_respondent <- paste0("Lab 20", "_", full.data$respondent)
temp3 <- full.data[,c(56,57,58,59,114,115,76,77,79,112,113,116)]
#lab20 <- full.data [,c(1,60:75,) ] # this part cannot be replicated; we need to select conditions here that seem to be missing

load(file="C:/Users/gschuma1/surfdrive/Papers/Congruence/Data/congruence_presentation.RData")

load("C:/Users/gschuma1/surfdrive/Papers/Salience and arousal/Analysis/full.data.RData")
arousal <- full.data
arousal$study_respondent <- paste0("Lab 20", "_", arousal$respondent)

temp1 <- data_selection[,c(1,4:5, 7,9:12,21:22,31:43,58)]
colnames(temp1)[c(2,5:8,18:24)] <- c("temperature", "political_interest", "leftright", "vote choice", "age", "education", "immigration_attitude", "environment_attitude","cynicism","inequality_attitude","eu_attitude", "sdo")
temp1$education <- factor(recode(temp1$education,
                                 '"1"="Secondary";
                          "2"="Secondary Vocational";
                          "3"="Higher Vocational";
                          "4"="University"'), levels=c("Secondary", "Secondary Vocational", "Higher Vocational", "University"))
temp1$polknow <- scale(temp1$polknow)
temp1$location <- ifelse(temp1$location=="labreplication", "lab",temp1$location)
temp1$reward <- ifelse(temp1$location=="lab",NA,"Voluntary")

temp2 <- lab19[,c(7,8,24:25, 28:35,37,44,48)]
colnames(temp2)[c(4,5,6,7)] <- c("education","temperature", "location","polknow")
temp2$polknow <- scale(temp2$polknow)
temp2$location <- ifelse(temp2$location==0, "Nijmegen","lab")

#temp3 <- lab20[,c(2:5,22:24,27:31)]
temp3 <- lab20
colnames(temp3)[c(7,8,9,10,11)] <- c("temperature","age", "education", "polknow", "female")
temp3$polknow <- scale(temp3$polknow)
temp3$location <- "lab"
temp3$education <- factor(recode(temp3$education,
                                 '"2"="Secondary";
                          "3"="Secondary Vocational";
                          "4"="Higher Vocational";
                          "5"="University"'), levels=c("Secondary", "Secondary Vocational", "Higher Vocational", "University"))



rewards <- openxlsx::read.xlsx("C:/Users/gschuma1/surfdrive/Data/Data Collection spring 2020/Logbook_lab_2019_2020.xlsx")
rewards$study_respondent <- paste0("Lab 20_",rewards$Participant.number)
temp3 <- merge(temp3,rewards[,c(4,6)], by="study_respondent", all.x=TRUE)
colnames(temp3)[14] <- "reward"
temp3$temperature <- as.numeric(temp3$temperature)

datasets <- dplyr::bind_rows(temp1,temp2, temp3)
datasets[which(datasets[,25]=="Credits"),25] <- "Credit"
datasets[which(datasets[,25]=="Money "),25] <- "Money"

# Now identify the treatments shown -----

emg$treatment.order <- as.numeric(str_split_fixed(emg$treatment.order, "treatment",2)[,2])

emg$condition <- NA 
condition <- matrix(NA, dim(emg)[1])

for(i in 1:dim(emg)[1]){
  if(is.na(emg$treatment.order[i])==FALSE) {
    if(emg$study[i]=="Lowlands"){
      condition[i] <- unlist(strsplit(as.character(data_selection$treatments[which(data_selection$study_respondent==emg$study_respondent[i])]),""))[emg$treatment.order[i]]}
    if(emg$study[i]=="Groot Nationaal Onderzoek"){
      condition[i] <- unlist(strsplit(as.character(data_selection$treatments[which(data_selection$study_respondent==emg$study_respondent[i])]),""))[emg$treatment.order[i]]}
    if(emg$study[i]=="lab replication"){
      if(emg$treatment.order[i]<5){condition[i] <- unlist(strsplit(as.character(data_selection$treatments[which(data_selection$study_respondent==emg$study_respondent[i])]),""))[emg$treatment.order[i]]}
      if(emg$treatment.order[i]==5){condition[i] <- data_selection$dom.first[which(data_selection$study_respondent==emg$study_respondent[i])] }
      if(emg$treatment.order[i]==6){condition[i] <- data_selection$dom.second[which(data_selection$study_respondent==emg$study_respondent[i])] } 
      if(emg$treatment.order[i]>6){condition[i] <- data_selection[which(data_selection$study_respondent==emg$study_respondent[i]),52:57][emg$treatment.order[i]-6]} # scary pictures
    }
    if(emg$study[i]=="Lab 19"){
      if(emg$treatment.order[i]==1) {condition[i] <- ifelse(lab19$condition.in[which(lab19$study_respondent==emg$study_respondent[i])]==1, "inparty", "outparty")}
      if(emg$treatment.order[i]==2) {condition[i] <- ifelse(lab19$condition.in[which(lab19$study_respondent==emg$study_respondent[i])]==1, "outparty", "inparty")} 
      if(emg$treatment.order[i]==4) {condition[i] <- ifelse(lab19$condition.mv.in[which(lab19$study_respondent==emg$study_respondent[i])]==1, "inparty_mv", "outparty_mv")}
      if(emg$treatment.order[i]==7) {condition[i] <- ifelse(lab19$condition.mv.in[which(lab19$study_respondent==emg$study_respondent[i])]==1, "outparty_mv", "inparty_mv")}
      if(emg$treatment.order[i]%in%c(3,6)) {condition[i] <- "birds"}
      if(emg$treatment.order[i]%in%seq(8,14,1)) {condition[i] <- lab19[which(lab19$study_respondent==emg$study_respondent[i]),16:22][emg$treatment.order[i]-7]} # iaps
      if(emg$treatment.order[i]%in%seq(15,42,1)) {condition[i] <- congruence[which(congruence$study_respondent==emg$study_respondent[i]),emg$treatment.order[i]-14]} # congruence
    }
    if(emg$study[i]=="Lab 20"){
      if(emg$treatment.order[i]==17) {condition[i] <- "ocean"}
      if(emg$treatment.order[i]%in%c(5:7, 18:20)) {condition[i] <- lab20[which(lab20$study_respondent==emg$study_respondent[i]),which(emg$treatment.order[i]==c(5:7, 18:20))+5]} #facmim
      if(emg$treatment.order[i]%in%c(1:4))  {condition[i] <- lab20[which(lab20$study_respondent==emg$study_respondent[i]),emg$treatment.order[i] + 11]} # iaps 
      if(emg$treatment.order[i]%in%c(8:13,21:26)) {condition[i] <- arousal[which(arousal$study_respondent==emg$study_respondent[i])[1], which(emg$treatment.order[i]==c(8:13,21:26))+1]}# arousal
      if(emg$treatment.order[i]%in%c(14:16,27:29)) {condition[i] <- lab20[which(lab20$study_respondent==emg$study_respondent[i]),which(emg$treatment.order[i]==c(14:16, 27:29))+15]} # adfes
      if(emg$treatment.order[i]%in%c(30:57)) {condition[i] <- congruence[which(congruence$study_respondent==emg$study_respondent[i]),emg$treatment.order[i]-29]}
    }
  }
}

condition <- unlist(condition)
emg$condition <- condition

condition.labels <- openxlsx::read.xlsx("C:/Users/gschuma1/surfdrive/Papers/Multimodal Physiology/All Syncs/conditions.xlsx")

emg <- emg[,-which(colnames(emg)=="political")]

emg <- merge(emg, condition.labels, by=c("study", "condition"), all=T)


emg <- merge(emg, datasets, by="study_respondent", all.x=T)
emg$treatment <- ifelse(is.na(emg$treatment.order),0,1)

emg <- emg[order(emg$study_respondent,emg$syncs,emg$sync.units),]

emg <- emg[,-c(5,11)] # clean up a bit
emg <- emg[,c(1,2,27,20,28,4,5,9,3,14,54,10,11:13,15:19,6:8,21:26,29:53)]
save(emg, file="All Syncs/emg.RData")




# Now do SCL as well ----------------------------------------------------
# These results are not mentioned in the publication, and hasn't been double-checked.
scl <- do.call("rbind",lapply(csvs[which(types=="SCL")], function(x){
  data <- read.csv(x)
  data$study <- str_split_fixed(x, "/", 3)[,2]
  return(data)}))

scl <- scl[,-c(1,5)]
colnames(scl)[1] <- "scl"

scl$study <- ifelse(scl$study=="Lab 19-20","Lab 19", scl$study)
scl$study <- ifelse(scl$study=="Lab 19" & scl$respondent>=115,"Lab 20", scl$study)
scl$study_respondent <- paste0(scl$study, "_", scl$respondent)
respondents <- unique(scl$study_respondent)
scl$total.sec <- unlist(lapply(respondents, function(x) seq(1,length(which(scl$study_respondent==x)),1)))

wrong.syncs.scl <- which(paste0(scl$study,"_",scl$respondent)%in%paste0(mismatch.resp$Group.1,"_",mismatch.resp$Group.2))
scl <- scl[-wrong.syncs.scl,]
scl <- scl[-which(scl$study_respondent%in%c("Lab 19_27", "Lab 20_131")),] # relevant data missing and deleted at some point

#scl$scl[scl$study_respondent%in%c("Lowlands_40", "lab_replication_3", "lab_replication_13", "lab_replication_42"
#                                  )] <- NA
scl <- merge(scl,markers,by=c("syncs","study"))
scl$treatment.order <- as.numeric(str_split_fixed(scl$treatment.order, "treatment",2)[,2])
#scl$condition <- NA

scl <- merge(scl, datasets, by="study_respondent", all.x=T)
scl$treatment <- ifelse(is.na(scl$treatment.order),0,1)

# scl afmaken
condition <- matrix(NA, dim(scl)[1])

for(i in 1:dim(scl)[1]){
  if(is.na(scl$treatment.order[i])==FALSE) {
    if(scl$study[i]=="Lowlands"){
      condition[i] <- unlist(strsplit(as.character(data_selection$treatments[which(data_selection$study_respondent==scl$study_respondent[i])]),""))[scl$treatment.order[i]]}
    if(scl$study[i]=="Groot Nationaal Onderzoek"){
      condition[i] <- unlist(strsplit(as.character(data_selection$treatments[which(data_selection$study_respondent==scl$study_respondent[i])]),""))[scl$treatment.order[i]]}
    if(scl$study[i]=="lab replication"){
      if(scl$treatment.order[i]<5){condition[i] <- unlist(strsplit(as.character(data_selection$treatments[which(data_selection$study_respondent==scl$study_respondent[i])]),""))[scl$treatment.order[i]]}
      if(scl$treatment.order[i]==5){condition[i] <- data_selection$dom.first[which(data_selection$study_respondent==scl$study_respondent[i])] }
      if(scl$treatment.order[i]==6){condition[i] <- data_selection$dom.second[which(data_selection$study_respondent==scl$study_respondent[i])] } 
      if(scl$treatment.order[i]>6){condition[i] <- data_selection[which(data_selection$study_respondent==scl$study_respondent[i]),52:57][scl$treatment.order[i]-6]} # scary pictures
    }
    if(scl$study[i]=="Lab 19"){
      if(scl$treatment.order[i]==1) {condition[i] <- ifelse(lab19$condition.in[which(lab19$study_respondent==scl$study_respondent[i])]==1, "inparty", "outparty")}
      if(scl$treatment.order[i]==2) {condition[i] <- ifelse(lab19$condition.in[which(lab19$study_respondent==scl$study_respondent[i])]==1, "outparty", "inparty")} 
      if(scl$treatment.order[i]==4) {condition[i] <- ifelse(lab19$condition.mv.in[which(lab19$study_respondent==scl$study_respondent[i])]==1, "inparty_mv", "outparty_mv")}
      if(scl$treatment.order[i]==7) {condition[i] <- ifelse(lab19$condition.mv.in[which(lab19$study_respondent==scl$study_respondent[i])]==1, "outparty_mv", "inparty_mv")}
      if(scl$treatment.order[i]%in%c(3,6)) {condition[i] <- "birds"}
      if(scl$treatment.order[i]%in%seq(8,14,1)) {condition[i] <- lab19[which(lab19$study_respondent==scl$study_respondent[i]),16:22][scl$treatment.order[i]-7]} # iaps
      if(scl$treatment.order[i]%in%seq(15,42,1)) {condition[i] <- congruence[which(congruence$study_respondent==scl$study_respondent[i]),scl$treatment.order[i]-14]} # congruence
    }
    if(scl$study[i]=="Lab 20"){
      if(scl$treatment.order[i]==17) {condition[i] <- "ocean"}
      if(scl$treatment.order[i]%in%c(5:7, 18:20)) {condition[i] <- lab20[which(lab20$study_respondent==scl$study_respondent[i]),which(scl$treatment.order[i]==c(5:7, 18:20))+5]} #facmim
      if(scl$treatment.order[i]%in%c(1:4))  {condition[i] <- lab20[which(lab20$study_respondent==scl$study_respondent[i]),scl$treatment.order[i] + 11]} # iaps 
      if(scl$treatment.order[i]%in%c(8:13,21:26)) {condition[i] <- arousal[which(arousal$study_respondent==scl$study_respondent[i])[1], which(scl$treatment.order[i]==c(8:13,21:26))+1]}# arousal
      if(scl$treatment.order[i]%in%c(14:16,27:29)) {condition[i] <- lab20[which(lab20$study_respondent==scl$study_respondent[i]),which(scl$treatment.order[i]==c(14:16, 27:29))+15]} # adfes
      if(scl$treatment.order[i]%in%c(30:57)) {condition[i] <- congruence[which(congruence$study_respondent==scl$study_respondent[i]),scl$treatment.order[i]-29]}
    }
  }
}

condition <- unlist(condition)
scl$condition <- condition

condition.labels <- openxlsx::read.xlsx("C:/Users/gschuma1/surfdrive/Papers/Multimodal Physiology/All Syncs/conditions.xlsx")

scl <- scl[,-which(colnames(scl)=="political")]
scl <- scl[,-c(6,8)] # clean up a bit

#save(scl, file="scl.RData") # sometimes it doesnt'have enough memory for the next command, save, exit and load again works

scl <- merge(scl, condition.labels, by=c("study", "condition"), all=T)

scl <- scl[order(scl$study_respondent,scl$syncs,scl$sync.units),]

scl <- scl[,c(3, 1, 19, 12, 20, 4, 6, 7, 2, 47, 46, 8, 9:11,48:52,5,13:18,21:45)]
save(scl, file="scl.RData")
