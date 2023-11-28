load("emg.RData")

# Add baselines per respondent EMG --------------------------------------------
make.baseline.lowlands <- function(respondent){
  selection <- which(emg$label=="fixation" & emg$study_respondent==respondent)
  selection <- selection[-length(selection)] # omit the last observation
  cor.baseline <- median(emg$cor[selection])
  zyg.baseline <- median(emg$zyg[selection])
  output <- data.frame(cor.baseline, zyg.baseline, respondent)
  return(output)
}

make.baseline.treatment.lowlands <- function(respondent){
  begin.syncs <- sapply(c(3,5,7,9), function(x) min(emg$total.sec[emg$syncs==x & emg$study_respondent==respondent]))
  time.before <- sapply(begin.syncs, function(x) x - seq(2,31,1))
  cor.baselines <- apply(time.before, 2, function(x) median(emg$cor[which(emg$total.sec%in%x & emg$study_respondent==respondent)]))
  zyg.baselines <- apply(time.before, 2, function(x) median(emg$zyg[which(emg$total.sec%in%x & emg$study_respondent==respondent)]))
  output <- data.frame(c(3,5,7,9),cor.baselines,zyg.baselines, respondent)
}

respondents <- unique(emg$study_respondent[emg$study=="Lowlands"])
emg.baseline <- do.call("rbind",lapply(respondents, make.baseline.lowlands))
emg.baseline.alt <- do.call("rbind",lapply(respondents, make.baseline.treatment.lowlands))

colnames(emg.baseline.alt) <- c("syncs", "cor.baseline.ll.alt", "zyg.baseline.ll.alt", "study_respondent")
colnames(emg.baseline) <- c("cor.baseline.ll","zyg.baseline.ll", "study_respondent")
emg.baseline.ll <- merge(emg.baseline.alt, emg.baseline, by="study_respondent", all.x=T)

make.baseline <- function(respondent){
  emg.respondent <- emg[which(emg$study_respondent==respondent),]
  syncs <- unique(emg.respondent$syncs[which(emg.respondent$treatment==1)])-1
  medians <- sapply(syncs, function(x){
    cor.data <- emg.respondent$cor[emg.respondent$syncs==x]
    zyg.data <- emg.respondent$zyg[emg.respondent$syncs==x]
    labii.data <- emg.respondent$labii[emg.respondent$syncs==x]
    cor.baseline <- median(cor.data[-length(cor.data)])
    zyg.baseline <- median(zyg.data[-length(cor.data)])
    labii.baseline <- median(labii.data[-length(cor.data)])
    return(data.frame(cor.baseline, zyg.baseline, labii.baseline))
  })
  medians <- data.frame(unlist(medians[1,]),unlist(medians[2,]),unlist(medians[3,]), syncs+1, respondent) ### do +1 here to match them later with the experimental condition
  return(medians)
}

respondents <- unique(emg$study_respondent[emg$study!="Lowlands"])
emg.baseline2 <- do.call("rbind",lapply(respondents, make.baseline))
colnames(emg.baseline2) <- c("cor.baseline","zyg.baseline","labii.baseline", "syncs","study_respondent")
emg.baseline.merged <- merge(emg.baseline.ll, emg.baseline2, by=c("study_respondent", "syncs"), all=T)

emg <- dplyr::left_join(emg, emg.baseline.merged, by=c("study_respondent","syncs"), all=T)
save(emg, file="emg.RData")


# 
# cor(emg$cor.baseline.ll, emg$cor.baseline.ll.alt, use="pairwise.complete.obs")
# cor(emg$zyg.baseline.ll, emg$zyg.baseline.ll.alt, use="pairwise.complete.obs")
# 
# cor(emg$cor.baseline.ll[emg$syncs==3], emg$cor.baseline.ll.alt[emg$syncs==3], use="pairwise.complete.obs")
# cor(emg$cor.baseline.ll[emg$syncs==5], emg$cor.baseline.ll.alt[emg$syncs==5], use="pairwise.complete.obs")
# cor(emg$cor.baseline.ll[emg$syncs==7], emg$cor.baseline.ll.alt[emg$syncs==7], use="pairwise.complete.obs")
# cor(emg$cor.baseline.ll[emg$syncs==9], emg$cor.baseline.ll.alt[emg$syncs==9], use="pairwise.complete.obs")
# 
# cor(emg$zyg.baseline.ll[emg$syncs==3], emg$zyg.baseline.ll.alt[emg$syncs==3], use="pairwise.complete.obs")
# cor(emg$zyg.baseline.ll[emg$syncs==5], emg$zyg.baseline.ll.alt[emg$syncs==5], use="pairwise.complete.obs")
# cor(emg$zyg.baseline.ll[emg$syncs==7], emg$zyg.baseline.ll.alt[emg$syncs==7], use="pairwise.complete.obs")
# cor(emg$zyg.baseline.ll[emg$syncs==9], emg$zyg.baseline.ll.alt[emg$syncs==9], use="pairwise.complete.obs")
# 

# Make SCL baselines ------------------------------------------------------
# Add baselines per respondent SCL --------------------------------------------
make.baseline.lowlands <- function(respondent){
  selection <- which(scl$label=="fixation" & scl$study_respondent==respondent)
  selection <- selection[-length(selection)] # omit the last observation
  scl.baseline <- median(scl$scl[selection])
  output <- data.frame(scl.baseline, respondent)
  return(output)
}

make.baseline.treatment.lowlands <- function(respondent){
  begin.syncs <- sapply(c(3,5,7,9), function(x) min(scl$total.sec[scl$syncs==x & scl$study_respondent==respondent]))
  time.before <- sapply(begin.syncs, function(x) x - seq(2,31,1))
  scl.baselines <- apply(time.before, 2, function(x) median(scl$scl[which(scl$total.sec%in%x & scl$study_respondent==respondent)]))
  output <- data.frame(c(3,5,7,9),scl.baselines, respondent)
}

respondents <- unique(scl$study_respondent[scl$study=="Lowlands"])
scl.baseline <- do.call("rbind",lapply(respondents, make.baseline.lowlands))
scl.baseline.alt <- do.call("rbind",lapply(respondents, make.baseline.treatment.lowlands))

colnames(scl.baseline.alt) <- c("syncs", "scl.baseline.ll.alt", "study_respondent")
colnames(scl.baseline) <- c("scl.baseline.ll", "study_respondent")
scl.baseline.ll <- merge(scl.baseline.alt, scl.baseline, by="study_respondent", all.x=T)

make.baseline <- function(respondent){
  scl.respondent <- scl[which(scl$study_respondent==respondent),]
  syncs <- unique(scl.respondent$syncs[which(scl.respondent$treatment==1)])-1
  medians <- sapply(syncs, function(x){
    scl.data <- scl.respondent$scl[scl.respondent$syncs==x]
    scl.baseline <- median(scl.data[-length(scl.data)])
    return(scl.baseline)
  })
  medians <- data.frame(medians, syncs+1, respondent) ### do +1 here to match them later with the experimental condition
  return(medians)
}

respondents <- unique(scl$study_respondent[scl$study!="Lowlands"])
scl.baseline2 <- do.call("rbind",lapply(respondents, make.baseline))
colnames(scl.baseline2) <- c("scl.baseline", "syncs","study_respondent")
scl.baseline.merged <- merge(scl.baseline.ll, scl.baseline2, by=c("study_respondent", "syncs"), all=T)

scl <- dplyr::left_join(scl, scl.baseline.merged, by=c("study_respondent","syncs"), all=T)
save(scl, file="scl.RData")


