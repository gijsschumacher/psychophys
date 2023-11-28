library(texreg)
load("Analysis/main.analyses.RData")
load("Analysis/main.analyses_correction.RData")

# Politics model -------------
texreg(list(main.analyses[[1]], robustness.check2[[1]], main.analyses[[2]], robustness.check2[[2]]),
       file=paste0(outdir,"/Tables/cor_politics.tex"),
       stars=0.05,
       custom.header = list("Outlier removed"= 1:2, "Winsorized"=3:4),
       custom.model.names=c("Preregistered", "Adopted", "Preregistered", "Adopted"),
       custom.coef.names = c("Intercept", "Political treatment", "Seconds in experiment", "Seconds in treatment", "EO", "Lab", "Lowlands", "Nijmegen", "Tilburg", "TT Assen", "Temperature"),
       caption="Multilevel model explaining corrugator activity in political vs nonpolitical treatments",
       caption.above=TRUE,
       label="app2:cor_politics",
       digits=3,
       float.pos="!htbp"
)

texreg(list(main.analyses[[3]], robustness.check2[[3]], main.analyses[[4]], robustness.check2[[4]]),
       file=paste0(outdir,"/Tables/zyg_politics.tex"),
       stars=0.05,
       custom.header = list("Outlier removed"= 1:2, "Winsorized"=3:4),
       custom.model.names=c("Preregistered", "Adopted", "Preregistered", "Adopted"),
       custom.coef.names = c("Intercept", "Political treatment", "Seconds in experiment", "Seconds in treatment", "Lowlands","Temperature"),
       caption="Multilevel model explaining zygomaticus activity in political vs nonpolitical treatments",
       caption.above=TRUE,
       label="app2:zyg_politics",
       digits=3,
       float.pos="!htbp"
)

# Video model ----------
texreg(list(main.analyses[[5]], robustness.check2[[5]], main.analyses[[6]], robustness.check2[[6]]),
       file=paste0(outdir,"/Tables/cor_video.tex"),
       stars=0.05,
       custom.header = list("Outlier removed"= 1:2, "Winsorized"=3:4),
       custom.model.names=c("Preregistered", "Adopted", "Preregistered", "Adopted"),
       custom.coef.names = c("Intercept", "Video vs picture","Word vs picture", "Seconds in experiment", "Seconds in treatment", "EO", "Lab", "Lowlands", "Nijmegen", "Tilburg", "TT Assen", "Temperature"),
       caption="Multilevel model explaining corrugator activity in treatments with video, word or pictures",
       caption.above=TRUE,
       label="app2:cor_video",
       digits=3,
       float.pos="!htbp"
)

texreg(list(main.analyses[[7]], robustness.check2[[7]], main.analyses[[8]], robustness.check2[[8]]),
       file=paste0(outdir,"/Tables/zyg_video.tex"),
       stars=0.05,
       custom.header = list("Outlier removed"= 1:2, "Winsorized"=3:4),
       custom.model.names=c("Preregistered", "Adopted", "Preregistered", "Adopted"),
       custom.coef.names = c("Intercept", "Video vs picture", "Word vs picture", "Seconds in experiment", "Seconds in treatment", "Lowlands","Temperature"),
       caption="Multilevel model explaining zygomaticus activity in treatments with video, word or pictures",
       caption.above=TRUE,
       label="app2:zyg_video",
       digits=3,
       float.pos="!htbp"
)

# Face treatments ----------------
texreg(list(main.analyses[[9]], robustness.check2[[9]], main.analyses[[10]], robustness.check2[[10]]),
       file=paste0(outdir,"/Tables/cor_face.tex"),
       stars=0.05,
       custom.header = list("Outlier removed"= 1:2, "Winsorized"=3:4),
       custom.model.names=c("Preregistered", "Adopted", "Preregistered", "Adopted"),
       custom.coef.names = c("Intercept", "Face vs No face", "Seconds in experiment", "Seconds in treatment", "EO", "Lab", "Lowlands", "Nijmegen", "Tilburg", "TT Assen", "Temperature"),
       caption="Multilevel model explaining corrugator activity in face vs no face treatments",
       caption.above=TRUE,
       label="app2:cor_video",
       digits=3,
       float.pos="!htbp"
)

texreg(list(main.analyses[[11]], robustness.check2[[11]], main.analyses[[12]], robustness.check2[[12]]),
       file=paste0(outdir,"/Tables/zyg_face.tex"),
       stars=0.05,
       custom.header = list("Outlier removed"= 1:2, "Winsorized"=3:4),
       custom.model.names=c("Preregistered", "Adopted", "Preregistered", "Adopted"),
       custom.coef.names = c("Intercept", "Face vs No face", "Seconds in experiment", "Seconds in treatment", "Lowlands","Temperature"),
       caption="Multilevel model explaining zygomaticus activity in face vs no face treatments",
       caption.above=TRUE,
       label="app2:zyg_face",
       digits=3,
       float.pos="!htbp"
)

# Positive emotions ---------------
texreg(list(main.analyses[[13]], robustness.check2[[14]], main.analyses[[15]], robustness.check2[[15]]),
       file=paste0(outdir,"/Tables/cor_positive.tex"),
       stars=0.05,
       custom.header = list("Outlier removed"= 1:2, "Winsorized"=3:4),
       custom.model.names=c("Preregistered", "Adopted", "Preregistered", "Adopted"),
       custom.coef.names = c("Intercept", "Positive vs Neutral","Negative vs Neutral", "Seconds in experiment", "Seconds in treatment", "EO", "Lab",  "Nijmegen", "Tilburg", "TT Assen", "Temperature"),
       caption="Multilevel model explaining corrugator activity in treatments with different valence",
       caption.above=TRUE,
       label="app2:cor_positive",
       digits=3,
       float.pos="!htbp"
)

texreg(list(main.analyses[[15]], robustness.check2[[15]], main.analyses[[16]], robustness.check2[[16]]),
       file=paste0(outdir,"/Tables/zyg_positive.tex"),
       stars=0.05,
       custom.header = list("Outlier removed"= 1:2, "Winsorized"=3:4),
       custom.model.names=c("Preregistered", "Adopted", "Preregistered", "Adopted"),
       custom.coef.names = c("Intercept", "Positive vs Neutral", "Negative vs Neutral", "Seconds in experiment", "Seconds in treatment"),
       caption="Multilevel model explaining zygomaticus activity in treatments with different valence",
       caption.above=TRUE,
       label="app2:zyg_positive",
       digits=3,
       float.pos="!htbp"
)

# Sound treatment -----
texreg(list(main.analyses[[17]], robustness.check2[[17]], main.analyses[[18]], robustness.check2[[18]]),
       file=paste0(outdir,"/Tables/cor_sound.tex"),
       stars=0.05,
       custom.header = list("Outlier removed"= 1:2, "Winsorized"=3:4),
       custom.model.names=c("Preregistered", "Adopted", "Preregistered", "Adopted"),
       custom.coef.names = c("Intercept", "Sound vs No sound", "Seconds in experiment", "Seconds in treatment", "EO", "Lab", "Lowlands", "Nijmegen", "Tilburg", "TT Assen", "Temperature"),
       caption="Multilevel model explaining corrugator activity in sound vs no sound treatments",
       caption.above=TRUE,
       label="app2:cor_sound",
       digits=3,
       float.pos="!htbp"
)

texreg(list(main.analyses[[19]], robustness.check2[[19]], main.analyses[[20]], robustness.check2[[20]]),
       file=paste0(outdir,"/Tables/zyg_sound.tex"),
       stars=0.05,
       custom.header = list("Outlier removed"= 1:2, "Winsorized"=3:4),
       custom.model.names=c("Preregistered", "Adopted", "Preregistered", "Adopted"),
       custom.coef.names = c("Intercept", "Sound vs No sound", "Seconds in experiment", "Seconds in treatment", "Lowlands","Temperature"),
       caption="Multilevel model explaining zygomaticus activity in sound vs no sound treatments",
       caption.above=TRUE,
       label="app2:zyg_sound",
       digits=3,
       float.pos="!htbp"
)

# Lab treatment ----
texreg(list(main.analyses[[21]], robustness.check2[[21]], main.analyses[[22]], robustness.check2[[22]]),
       file=paste0(outdir,"/Tables/cor_lab.tex"),
       stars=0.05,
       custom.header = list("Outlier removed"= 1:2, "Winsorized"=3:4),
       custom.model.names=c("Preregistered", "Adopted", "Preregistered", "Adopted"),
       custom.coef.names = c("Intercept", "Lab vs No lab", "Seconds in experiment", "Seconds in treatment", "EO", "Lowlands", "Nijmegen", "Tilburg", "TT Assen", "Temperature"),
       caption="Multilevel model explaining corrugator activity in lab vs no lab treatments",
       caption.above=TRUE,
       label="app2:cor_lab",
       digits=3,
       float.pos="!htbp"
)

texreg(list(main.analyses[[23]], robustness.check2[[23]], main.analyses[[24]], robustness.check2[[24]]),
       file=paste0(outdir,"/Tables/zyg_lab.tex"),
       stars=0.05,
       custom.header = list("Outlier removed"= 1:2, "Winsorized"=3:4),
       custom.model.names=c("Preregistered", "Adopted", "Preregistered", "Adopted"),
       custom.coef.names = c("Intercept", "Lab vs No lab", "Seconds in experiment", "Seconds in treatment", "Temperatuur"),
       caption="Multilevel model explaining zygomaticus activity in lab vs no lab treatments",
       caption.above=TRUE,
       label="app2:zyg_lab",
       digits=3,
       float.pos="!htbp"
)
