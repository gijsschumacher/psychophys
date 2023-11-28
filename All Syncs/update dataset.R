load("All Syncs/emg.RData")
load("All Syncs/scl.RData")

# Insert employment coding ------------------------------------------------

library(car)
emg$employment <- car::recode(emg$employment, '"1"="Full-time employed";
                                          "2"="Part-time employed";
                                          "3"="Independent employment";
                                          "4"="Retired";
                                          "5"="Housewife/man";
                                          "6"="Student";
                                          "7"="Looking for work"')

scl$employment <- car::recode(scl$employment, '"1"="Full-time employed";
                                          "2"="Part-time employed";
                                          "3"="Independent employment";
                                          "4"="Retired";
                                          "5"="Housewife/man";
                                          "6"="Student";
                                          "7"="Looking for work"')

emg$student <- ifelse(emg$employment=="Student",1,0)
emg$votechoice_comb <- ifelse(emg$study%in%c("Lab 19", "Lab 20"),emg$inparty,emg$`vote choice`)
emg$votechoice_comb <- ifelse(emg$votechoice_comb=="Groen Links", "Groenlinks", emg$votechoice_comb)

emg$votechoice_lr <- ifelse(emg$votechoice_comb%in%c("50+", "Christenunie", "D66", "DENK", "CU"),"Center",
                     ifelse(emg$votechoice_comb%in%c("CDA", "Forum voor Democratie", "FvD", "PVV", "SGP", "VVD"),"Right",
                     ifelse(emg$votechoice_comb%in%c("Anders", "Blanco", "Stem niet"),"Other",
                     ifelse(emg$votechoice_comb%in%c("Groenlinks", "Partij voor de Dieren", "PvdA", "PvdD", "SP"),"Left", NA))))

scl$student <- ifelse(emg$employment=="Student",1,0)
scl$votechoice_comb <- ifelse(scl$study%in%c("Lab 19", "Lab 20"),scl$inparty,scl$`vote choice`)
scl$votechoice_comb <- ifelse(scl$votechoice_comb=="Groen Links", "Groenlinks", scl$votechoice_comb)

scl$votechoice_lr <- ifelse(scl$votechoice_comb%in%c("50+", "Christenunie", "D66", "DENK", "CU"),"Center",
                            ifelse(scl$votechoice_comb%in%c("CDA", "Forum voor Democratie", "FvD", "PVV", "SGP", "VVD"),"Right",
                                   ifelse(scl$votechoice_comb%in%c("Anders", "Blanco", "Stem niet"),"Other",
                                          ifelse(scl$votechoice_comb%in%c("Groenlinks", "Partij voor de Dieren", "PvdA", "PvdD", "SP"),"Left", NA))))


# Reverse cynicism ---------------------------------------------------------
emg$cynicism <- emg$cynicism*-1 + 9
scl$cynicism <- scl$cynicism*-1 + 9


# Add standard deviation --------------------------------------------------
sd.by.respondent <- aggregate(data.frame(emg$cor,emg$zyg), by=list(emg$study_respondent), sd)
colnames(sd.by.respondent) <- c("study_respondent", "cor_sd", "zyg_sd")
emg <- merge(emg, sd.by.respondent, by="study_respondent")


# Add left-right vote variable ----------------


# Remove respondent Groot Nationaal Onderzoek 184 .... did not finish Qualtrics
emg <- emg[-which(emg$study_respondent=="Groot Nationaal Onderzoek_184"),]
scl <- scl[-which(scl$study_respondent=="Groot Nationaal Onderzoek_184"),]


emg$computer <- ifelse(emg$location=="lab",2,
                       ifelse(emg$location=="Nijmegen",5,emg$computer))
scl$computer <- ifelse(scl$location=="lab",2,
                       ifelse(scl$location=="Nijmegen",5,scl$computer))


emg$alcohol <- ifelse(emg$location=="lab",0,emg$alcohol)
  
# Add left-right questions lab 19 and lab 20 -------------
load("C:/Users/gschuma1/surfdrive/Papers/Congruence/Data/congruence_survey.RData")
colnames(survey)[2] <- "leftright"
survey.selection <- as.data.frame(survey[,c(2,3,14:18)])

emg$leftright <- as.numeric(emg$leftright)
emg <- left_join(emg, survey.selection, by="study_respondent", keep=FALSE)
emg$leftright.x <- ifelse(emg$study%in%c("Lab 19", "Lab 20"),emg$leftright.y,emg$leftright.x)
colnames(emg)[which(colnames(emg)=="leftright.x")] <- "leftright"
emg <- emg[,-which(colnames(emg)=="leftright.y")]

scl$leftright <- as.numeric(scl$leftright)
scl <- left_join(scl, survey.selection, by="study_respondent", keep=FALSE)
scl$leftright.x <- ifelse(scl$study%in%c("Lab 19", "Lab 20"),scl$leftright.y,scl$leftright.x)
colnames(scl)[which(colnames(scl)=="leftright.x")] <- "leftright"
scl <- scl[,-which(colnames(scl)=="leftright.y")]

# Add self-reported emotions ----------------
load("C:/Users/gschuma1/surfdrive/Papers/Facial Mimicry/Analysis/full.data.RData") 
full.data <- full.data[,c(1:55)]
full.data$study_respondent <- paste0("Lab 20","_",full.data$respondent)

angry.items <- c(2,5,8)
happy.items <- c(1,7,9)
anxious.items <- c(3,4,6) 

emg$fm_enthousiasm <- NA
emg$fm_anger <- NA
emg$fm_anxiety <- NA

for(i in 1:dim(emg)[1]){
  if(emg$condition2[i]%in%c("inparty_happy", "outparty_happy",
                         "inparty_neutral", "outparty_neutral",
                         "inparty_angry", "outparty_angry")){
 row <- which(full.data$study_respondent==emg$study_respondent[i])
 cols <- which(stringr::str_detect(colnames(full.data), emg$condition2[i])) 

    emg$fm_enthousiasm[i] <- as.numeric(apply(full.data[row,cols[happy.items]],1,mean))
    emg$fm_anger[i] <- as.numeric(apply(full.data[row,cols[angry.items]],1,mean))
    emg$fm_anxiety[i] <- as.numeric(apply(full.data[row,cols[anxious.items]],1,mean))
  }} 
 
  
save(emg, file="All Syncs/emg.RData")
save(scl, file="All Syncs/scl.RData")

