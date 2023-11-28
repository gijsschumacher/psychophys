
# Calculate EMG activity measures -----------------------------------------
load("emg.RData")

emg$cor.increase.raw <- ifelse(emg$study=="Lowlands", emg$cor - emg$cor.baseline.ll, emg$cor - emg$cor.baseline)
emg$zyg.increase.raw <- ifelse(emg$study=="Lowlands", emg$zyg - emg$zyg.baseline.ll, emg$zyg - emg$zyg.baseline)
emg$labii.increase.raw <- emg$labii - emg$labii.baseline

emg$cor.increase.raw.alt <- ifelse(emg$study=="Lowlands", emg$cor - emg$cor.baseline.ll.alt, emg$cor - emg$cor.baseline)
emg$zyg.increase.raw.alt <- ifelse(emg$study=="Lowlands", emg$zyg - emg$zyg.baseline.ll.alt, emg$zyg - emg$zyg.baseline)

emg$cor.increase.pc <- ifelse(emg$study=="Lowlands", (emg$cor / emg$cor.baseline.ll)*100, (emg$cor / emg$cor.baseline)*100)
emg$cor.increase.pc.alt <- ifelse(emg$study=="Lowlands", (emg$cor / emg$cor.baseline.ll.alt)*100, (emg$cor / emg$cor.baseline)*100)

emg$zyg.increase.pc <- ifelse(emg$study=="Lowlands", (emg$zyg / emg$zyg.baseline.ll)*100, (emg$zyg / emg$zyg.baseline)*100)
emg$zyg.increase.pc.alt <- ifelse(emg$study=="Lowlands", (emg$zyg / emg$zyg.baseline.ll.alt)*100, (emg$zyg / emg$zyg.baseline)*100)

emg$labii.increase.pc <- (emg$labii / emg$labii.baseline)*100

scale.by.respondent <- function(respondent, var){
  z <- scale(emg[emg$study_respondent==respondent, which(colnames(emg)==var)])
  return(z)
}
respondents <- unique(emg$study_respondent)

emg$cor.z <- do.call("rbind", lapply(respondents, scale.by.respondent, "cor"))
emg$zyg.z <- do.call("rbind", lapply(respondents, scale.by.respondent, "zyg"))
emg$labii.z <- do.call("rbind", lapply(respondents, scale.by.respondent, "labii"))

# Implementing hampel algorithm
median.replacer <- function(var, ind, k){
  replacement <- matrix(NA, length(ind))
  for(i in 1:length(ind)){
    replacement[i] <- median(var[(ind[i]-10) : (ind[i]+10)])
  }
  return(replacement)
}

hampel.by.respondent <- function(respondent, var){
#for (i in 1:length(respondents)){  
data.small <- emg[emg$study_respondent==respondent, which(colnames(emg)==var)]
  if(is.na(data.small)[1]==FALSE){
  var.hampel <- pracma::hampel(data.small, k=20, t0=4)
  output <- as.matrix(var.hampel$y)
  if(length(var.hampel$ind)>0) {output[var.hampel$ind] <- median.replacer(output, var.hampel$ind, k=20)
  }
  } else{output <- as.matrix(data.small)}
  return(output)
}

emg$cor.hampel <- do.call("rbind", lapply(respondents, hampel.by.respondent, "cor"))
emg$zyg.hampel <- do.call("rbind", lapply(respondents, hampel.by.respondent, "zyg"))

emg$cor.hampel.increase <- ifelse(emg$study=="Lowlands", (emg$cor.hampel / emg$cor.baseline.ll)*100, (emg$cor.hampel / emg$cor.baseline)*100)
emg$zyg.hampel.increase <- ifelse(emg$study=="Lowlands", (emg$zyg.hampel / emg$zyg.baseline.ll)*100, (emg$zyg.hampel / emg$zyg.baseline)*100)

save(emg, file="emg.RData")

## Winsorizing (not used) -------------------------------------------------------------
# get.winsorized <- function(respondent, var, prob) {
#   data <- emg[which(emg$study_respondent==respondent),]
#   var <- data[,which(colnames(emg)==var)]
#   if(is.na(var[1])){
#     values <- rep(NA,length(var))  
#   } else{
#     low <- paste0(100-prob,"%")
#     high <- paste0(prob,"%")  
#     quantiles <- quantile(var, probs=seq(0,1,0.05), na.rm=TRUE)
#     
#     values <- ifelse(var<quantiles[low],quantiles[low],
#                       ifelse(var>quantiles[high],quantiles[high],var))
#   }
#   return(values)
# }
# 
# 
# cor.winsorized <- lapply(respondents, get.winsorized, var="cor", prob=95)
# zyg.winsorized <- lapply(respondents, get.winsorized, var="zyg", prob=95)
# labii.winsorized <- lapply(respondents, get.winsorized, var="labii", prob=95)
# 
# emg$cor.winsorized <- unlist(winsorized.cor)
# emg$zyg.winsorized <- unlist(winsorized.zyg)
# emg$labii.winsorized <- unlist(winsorized.labii)
# 
# emg$cor.winsorized.increase.raw <- ifelse(emg$study=="Lowlands", emg$cor.winsorized - emg$cor.baseline.ll, emg$cor.winsorized - emg$cor.baseline)
# emg$zyg.winsorized.increase.raw <- ifelse(emg$study=="Lowlands", emg$zyg.winsorized - emg$zyg.baseline.ll, emg$zyg.winsorized - emg$zyg.baseline)
# emg$labii.winsorized.increase.raw <- emg$labii - emg$labii.baseline
# 
# emg$cor.winsorized.increase.raw.alt <- ifelse(emg$study=="Lowlands", emg$cor.winsorized - emg$cor.baseline.ll.alt, emg$cor.winsorized - emg$cor.baseline)
# emg$zyg.winsorized.increase.raw.alt <- ifelse(emg$study=="Lowlands", emg$zyg.winsorized - emg$zyg.baseline.ll.alt, emg$zyg.winsorized - emg$zyg.baseline)
# 
# emg$cor.winsorized.increase.pc <- ifelse(emg$study=="Lowlands", (emg$cor.winsorized / emg$cor.baseline.ll)*100, (emg$cor.winsorized / emg$cor.baseline)*100)
# emg$cor.winsorized.increase.pc.alt <- ifelse(emg$study=="Lowlands", (emg$cor.winsorized / emg$cor.baseline.ll.alt)*100, (emg$cor.winsorized / emg$cor.baseline)*100)
# 
# emg$zyg.winsorized.increase.pc <- ifelse(emg$study=="Lowlands", (emg$zyg.winsorized / emg$zyg.baseline.ll)*100, (emg$zyg.winsorized / emg$zyg.baseline)*100)
# emg$zyg.winsorized.increase.pc.alt <- ifelse(emg$study=="Lowlands", (emg$zyg.winsorized / emg$zyg.baseline.ll.alt)*100, (emg$zyg.winsorized / emg$zyg.baseline)*100)
# 
# emg$labii.winsorized.increase.pc <- (emg$labii.winsorized / emg$labii.baseline)*100
# 




# # Calculate SCL activity measures (not in paper) ---------------------------------------
load("scl.RData")

scl$scl.increase.raw <- ifelse(scl$study=="Lowlands", scl$scl - scl$scl.baseline.ll, scl$scl - scl$scl.baseline)

scl$scl.increase.raw.alt <- ifelse(scl$study=="Lowlands", scl$scl - scl$scl.baseline.ll.alt, scl$scl - scl$scl.baseline)

scl$scl.increase.pc <- ifelse(scl$study=="Lowlands", (scl$scl / scl$scl.baseline.ll)*100, (scl$scl / scl$scl.baseline)*100)
scl$scl.increase.pc.alt <- ifelse(scl$study=="Lowlands", (scl$scl / scl$scl.baseline.ll.alt)*100, (scl$scl / scl$scl.baseline)*100)

scl$scl.z <- scale(scl$scl)


## SCL: Loess corrected lines (not in paper) ---------------------------------------------------


get.ma <- function(respondent, var, type) {
  data <- scl[which(scl$study_respondent==respondent),]
  var <- data[,which(colnames(scl)==var)]
  if(is.na(var[1])){
    values <- rep(NA,length(var))  
  } else{
    if(type=="5sec"){ma <- stats::filter(data$scl, c(rep(1/8/60, 1*60), rep(1/4/60, 3*60), rep(1/8/60, 1*60)), sides=2)}
    if(type=="1sec"){ma <- stats::filter(data$scl, c(rep(1/8/10, 1*10), rep(1/4/10, 3*10), rep(1/8/10, 1*10)), sides=2)}
  }
  if(any(data$total.sec==seq(1,dim(data)[1],1))==FALSE){ma <- "error"}
  return(ma)
}

respondents <- unique(scl$study_respondent)

scl.ma.5sec <- lapply(respondents, get.ma, var="scl", type="5sec") 
scl.ma.1sec <- lapply(respondents, get.ma, var="scl", type="1sec") 

scl$scl.ma.5sec <- unlist(scl.ma.5sec)
scl$scl.ma.1sec <- unlist(scl.ma.1sec) 

scl$scl.ma5.increase.raw <- ifelse(scl$study=="Lowlands", scl$scl.ma.5sec - scl$scl.baseline.ll, scl$scl.ma.5sec - scl$scl.baseline)
scl$scl.ma1.increase.raw <- ifelse(scl$study=="Lowlands", scl$scl.ma.1sec - scl$scl.baseline.ll, scl$scl.ma.1sec - scl$scl.baseline)

scl$scl.ma5.raw.alt <- ifelse(scl$study=="Lowlands", scl$scl.ma.5sec - scl$scl.baseline.ll.alt, scl$scl.ma.5sec - scl$scl.baseline)
scl$scl.ma1.raw.alt <- ifelse(scl$study=="Lowlands", scl$scl.ma.1sec - scl$scl.baseline.ll.alt, scl$scl.ma.1sec - scl$scl.baseline)

scl$scl.ma5.increase.pc <- ifelse(scl$study=="Lowlands", (scl$scl.ma.5sec / scl$scl.baseline.ll)*100, (scl$scl.ma.5sec / scl$scl.baseline)*100)
scl$scl.ma5.increase.pc.alt <- ifelse(scl$study=="Lowlands", (scl$scl.ma.5sec / scl$scl.baseline.ll.alt)*100, (scl$scl.ma.5sec / scl$scl.baseline)*100)
scl$scl.ma1.increase.pc <- ifelse(scl$study=="Lowlands", (scl$scl.ma.1sec / scl$scl.baseline.ll)*100, (scl$scl.ma.1sec / scl$scl.baseline)*100)
scl$scl.ma1.increase.pc.alt <- ifelse(scl$study=="Lowlands", (scl$scl.ma.1sec / scl$scl.baseline.ll.alt)*100, (scl$scl.ma.1sec / scl$scl.baseline)*100)


sd.by.respondent <- aggregate(scl$scl, by=list(scl$study_respondent), sd)
colnames(sd.by.respondent) <- c("study_respondent", "scl_sd")
scl <- dplyr::left_join(scl, sd.by.respondent, by="study_respondent")

scl$scl.increase.z <- scl$scl.increase.raw / scl$scl_sd
scl$scl.increase.z1sec <- scl$scl.ma1.increase.raw / scl$scl_sd
scl$scl.increase.z5sec <- scl$scl.ma5.increase.raw / scl$scl_sd

save(scl, file="scl.RData")

