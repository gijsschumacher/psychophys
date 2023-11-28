library(stargazer)
emg_by_p <- emg[which(emg$syncs==1 & emg$sync.units==1),]

# stargazer(data.frame(
# emg_by_p$female,
# emg_by_p$polknow,
# emg_by_p$age,
# emg_by_p$alcohol,
# emg_by_p$student,
# emg_by_p$leftright,
# emg_by_p$cynicism,
# emg_by_p$political_interest,
# emg_by_p$partisanship.strength), summary.stat=c("n", "mean","sd","min", "max")
# )

emg_by_p$education.label <- factor(ifelse(emg_by_p$education=="Secondary Vocational","Secondary\nVocational",
                                   ifelse(emg_by_p$education=="Higher Vocational","Higher\nVocational",as.character(emg_by_p$education))), 
                                   labels=c("Secondary\nVocational", "Secondary", "Higher\nVocational", "University"))

desc.edu <- ggplot(emg_by_p[-which(is.na(emg_by_p$education)),], aes(x=education.label, fill=education.label)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("Percentage") + xlab("Finished Education level") +
  theme(legend.position = "none")


#emg_by_p <- 

emg_by_p$votechoice_lr <- factor(emg_by_p$votechoice_lr, levels=c("Left","Center","Right","Other"))

desc.vote <- ggplot(emg_by_p, aes(x=votechoice_lr, fill=votechoice_lr)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) + 
  scale_y_continuous(labels=scales::percent) +
  ylab("Percentage") + xlab("Vote at last election") +
  theme(legend.position = "none")

ggsave(desc.vote, file=paste0(outdir,"/Figures/desc_vote.jpg"))
ggsave(desc.edu, file=paste0(outdir,"/Figures/desc_edu.jpg"))
