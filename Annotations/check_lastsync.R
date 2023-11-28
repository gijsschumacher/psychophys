load("All Syncs/emg.RData")

last.sync <- aggregate(emg$sync.units, by=list(emg$syncs, emg$study_respondent), FUN=max)
colnames(last.sync) <- c("syncs", "study_respondent", "last.sync")

last.sync2 <- matrix(0, dim(emg)[1])
penultimate.sync <- matrix(0, dim(emg)[1])

for(i in 1:dim(last.sync)[1]){
  selection <- which(last.sync$study_respondent[i]==emg$study_respondent &
                     last.sync$syncs[i]==emg$syncs &
                     last.sync$last.sync[i]==emg$sync.units)
  last.sync2[selection] <- 1
  penultimate.sync[selection-1] <- 1
  print(i)
}

total.sec.sq <- emg$total.sec^2
sync.units.sq <- emg$sync.units^2
emg$last.sync <- last.sync2
emg$penultimate.sync <- penultimate.sync
emg$penultimate.sync <- ifelse(emg$last.sync==1,0,penultimate.sync)

#summary(lm(cor ~ total.sec.sq + sync.units.sq + sync.units + total.sec + penultimate.sync + last.sync2, data=emg)) ### clear evidence that last sync is systematically off

load("All Syncs/scl.RData")
last.sync <- aggregate(scl$sync.units, by=list(scl$syncs, scl$study_respondent), FUN=max)
colnames(last.sync) <- c("syncs", "study_respondent", "last.sync")

last.sync2 <- matrix(0, dim(scl)[1])
#penultimate.sync <- matrix(0, dim(scl)[1])

scl <- scl[order(scl$study_respondent, scl$syncs, scl$sync.units),]

# Warning inefficient code: takes long.
for(i in 1:dim(last.sync)[1]){
  selection <- which(last.sync$study_respondent[i]==scl$study_respondent &
                       last.sync$syncs[i]==scl$syncs &
                       last.sync$last.sync[i]==scl$sync.units)
  last.sync2[selection - 0:10] <- 1
  #penultimate.sync[selection-1] <- 1
  print(i)
}

scl$last.sync <- last.sync2
#scl$penultimate.sync <- penultimate.sync
#scl$penultimate.sync <- ifelse(scl$last.sync==1,0,penultimate.sync)

total.sec.sq <- scl$total.sec^2
sync.units.sq <- scl$sync.units^2

#summary(lm(scl ~ total.sec.sq + sync.units.sq + sync.units + total.sec + last.sync, data=scl)) ### clear evidence that last sync is systematically off

save(emg, file="All Syncs/emg.RData")
save(scl, file="All Syncs/scl.RData")
