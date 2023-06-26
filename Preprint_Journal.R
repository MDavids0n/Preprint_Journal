packages_needed_for_analysis <- c("data.table","openxlsx","dplyr","netmeta","metafor","forestplot")
package.check <- lapply(
  packages_needed_for_analysis,
  FUN = function(x) {
    if (!require(x, character.only = TRUE)) {
      install.packages(x, dependencies = TRUE)
      library(x, character.only = TRUE)
      require(x, character.only = TRUE)
    }
  }
)

# MORTALITY
Obj2_mort<-as.data.table(read.xlsx(paste0("C:\\Users\\mauri\\METHODS Dropbox\\Mauricia Davidson\\preprint-journalMD.xlsx"),
                                   cols = c(1:23,24:26,44,49)))
colnames(Obj2_mort)<-make.names(colnames(Obj2_mort))
Obj2_mort$All_cause_mortality_events_D28 <- as.numeric(Obj2_mort$All_cause_mortality_events_D28)
Obj2_mort$n_Analyzed_<- as.numeric(Obj2_mort$n_Analyzed_)
for (i in 1:(nrow(Obj2_mort))) {
  if (Obj2_mort$mortality_d28_N[i] != "*") {
    Obj2_mort$x[i]<-Obj2_mort$mortality_d28_N[i]
  } else {
    Obj2_mort$x[i]<-Obj2_mort$n_Analyzed_[i]
  }
}
Obj2_mort$x<-as.numeric(Obj2_mort$x)
testMort<-as.data.table(pairwise(treat = stats_name, event = All_cause_mortality_events_D28, n = x, data = Obj2_mort, studlab = MD_ID, sm = "OR"))
filtered<-testMort[!testMort$Same_analysis1 %in% "N"]
filtered$treat1=trimws(filtered$treat1)
filtered$treat2=trimws(filtered$treat2)
filtered$IDComparison=paste(filtered$ID_stats,"_",filtered$treat1,"_",filtered$treat2, sep="")
filtered$IDComparison=trimws(filtered$IDComparison)
### Identify pp-pub pairs with same events and n analyzed
# identify ALL instances of duplicated values btwn events and n analyzed and ID comparison (for a given 
# comparison with the same ID stat, keep the rows with identical events and n analyzed)
samenNm<-filtered[(duplicated(filtered[,c(6,7,8,9,57)])|duplicated(filtered[,c(6,7,8,9,57)],fromLast = T)),]
# Identify pp-pub pairs with diff events and n analyzed; Remove pp-pub pairs with same events and n analyzed
diffnNm<-filtered[!(duplicated(filtered[,c(6,7,8,9,57)])|duplicated(filtered[,c(6,7,8,9,57)],fromLast = T)),] 
# identify trials with new or deleted outcome data between pp and pub (same ID comparison, one TE = NA)
NYm<-subset(diffnNm, IDComparison %in% IDComparison[is.na(TE)])
# identify trials with different EE btwn pp and pub (same ID comparison, diff TE)
Mort<-subset(diffnNm, !IDComparison %in% IDComparison[is.na(TE)])

# CLINICAL IMPROVEMENT
Obj2_CI<-as.data.table(read.xlsx(paste0("C:\\Users\\mauri\\METHODS Dropbox\\Mauricia Davidson\\preprint-journalMD.xlsx"),
                                 cols = c(1:23,28:30,45,49)))
colnames(Obj2_CI)<-make.names(colnames(Obj2_CI))
Obj2_CI$Clinical_improvement_D28_n <- as.numeric(Obj2_CI$Clinical_improvement_D28_n)
Obj2_CI$n_Analyzed_<- as.numeric(Obj2_CI$n_Analyzed_)
for (i in 1:(nrow(Obj2_CI))) {
  if (Obj2_CI$clinic_imp_d28_N[i] != "*") {
    Obj2_CI$x[i]<-Obj2_CI$clinic_imp_d28_N[i]
  } else {
    Obj2_CI$x[i]<-Obj2_CI$n_Analyzed_[i]
  }
}
Obj2_CI$x<-as.numeric(Obj2_CI$x)
testCI<-as.data.table(pairwise(treat = stats_name, event = Clinical_improvement_D28_n, n = x, data = Obj2_CI, studlab = MD_ID, sm = "OR"))
filtCI<-testCI[!testCI$Same_analysis1 %in% "N"]
filtCI$treat1=trimws(filtCI$treat1)
filtCI$treat2=trimws(filtCI$treat2)
filtCI$IDComparison=paste(filtCI$ID_stats,"_",filtCI$treat1,"_",filtCI$treat2, sep="")
filtCI$IDComparison=trimws(filtCI$IDComparison)
samenNc<-filtCI[(duplicated(filtCI[,c(6,7,8,9,57)])|duplicated(filtCI[,c(6,7,8,9,57)],fromLast = T)),]
diffnNc<-filtCI[!(duplicated(filtCI[,c(6,7,8,9,57)])|duplicated(filtCI[,c(6,7,8,9,57)],fromLast = T)),] 
NYc<-subset(diffnNc, IDComparison %in% IDComparison[is.na(TE)])
CI<-subset(diffnNc, !IDComparison %in% IDComparison[is.na(TE)])

# WHO SCORE 7 & ABOVE
Obj2_WHO7<-as.data.table(read.xlsx(paste0("C:\\Users\\mauri\\METHODS Dropbox\\Mauricia Davidson\\preprint-journalMD.xlsx"),
                                   cols = c(1:23,31:33,46,49)))
colnames(Obj2_WHO7)<-make.names(colnames(Obj2_WHO7))
Obj2_WHO7$SCORE_7_and_above_D28 <- as.numeric(Obj2_WHO7$SCORE_7_and_above_D28)
Obj2_WHO7$n_Analyzed_<- as.numeric(Obj2_WHO7$n_Analyzed_)
for (i in 1:(nrow(Obj2_WHO7))) {
  if (Obj2_WHO7$score7_d28_N[i] != "*") {
    Obj2_WHO7$x[i]<-Obj2_WHO7$score7_d28_N[i]
  } else {
    Obj2_WHO7$x[i]<-Obj2_WHO7$n_Analyzed_[i]
  }
}
Obj2_WHO7$x<-as.numeric(Obj2_WHO7$x)
testWHO7<-as.data.table(pairwise(treat = stats_name, event = SCORE_7_and_above_D28, n = x, data = Obj2_WHO7, studlab = MD_ID, sm = "OR"))
filtWHO7<-testWHO7[!testWHO7$Same_analysis1 %in% "N"]
filtWHO7$treat1=trimws(filtWHO7$treat1)
filtWHO7$treat2=trimws(filtWHO7$treat2)
filtWHO7$IDComparison=paste(filtWHO7$ID_stats,"_",filtWHO7$treat1,"_",filtWHO7$treat2, sep="")
filtWHO7$IDComparison=trimws(filtWHO7$IDComparison)
samenN7<-filtWHO7[(duplicated(filtWHO7[,c(6,7,8,9,56)])|duplicated(filtWHO7[,c(6,7,8,9,56)],fromLast = T)),]
diffnN7<-filtWHO7[!(duplicated(filtWHO7[,c(6,7,8,9,56)])|duplicated(filtWHO7[,c(6,7,8,9,56)],fromLast = T)),] 
NY7<-subset(diffnN7, IDComparison %in% IDComparison[is.na(TE)])
WHO7<-subset(diffnN7, !IDComparison %in% IDComparison[is.na(TE)])

# ADVERSE EVENTS
Obj2_AE<-as.data.table(read.xlsx(paste0("C:\\Users\\mauri\\METHODS Dropbox\\Mauricia Davidson\\preprint-journalMD.xlsx"),
                                 cols = c(1:23,34,36:37,47,49)))
colnames(Obj2_AE)<-make.names(colnames(Obj2_AE))
Obj2_AE$Total_AE<- as.numeric(Obj2_AE$Total_AE)
Obj2_AE$n_Analyzed_<- as.numeric(Obj2_AE$n_Analyzed_)
for (i in 1:(nrow(Obj2_AE))) {
  if (Obj2_AE$safety_N[i] != "*") {
    Obj2_AE$x[i]<-Obj2_AE$safety_N[i]
  } else {
    Obj2_AE$x[i]<-Obj2_AE$n_Analyzed_[i]
  }
}
Obj2_AE$x<-as.numeric(Obj2_AE$x)
testAE<-as.data.table(pairwise(treat = stats_name, event = Total_AE, n = x, data = Obj2_AE, studlab = MD_ID, sm = "OR"))
filtAE<-testAE[!testAE$Same_analysis1 %in% "N"]
filtAE$treat1=trimws(filtAE$treat1)
filtAE$treat2=trimws(filtAE$treat2)
filtAE$IDComparison=paste(filtAE$ID_stats,"_",filtAE$treat1,"_",filtAE$treat2, sep="")
filtAE$IDComparison=trimws(filtAE$IDComparison)
samenNa<-filtAE[(duplicated(filtAE[,c(6,7,8,9,57)])|duplicated(filtAE[,c(6,7,8,9,57)],fromLast = T)),]
diffnNa<-filtAE[!(duplicated(filtAE[,c(6,7,8,9,57)])|duplicated(filtAE[,c(6,7,8,9,57)],fromLast = T)),] 
NYa<-subset(diffnNa, IDComparison %in% IDComparison[is.na(TE)])
AE<-subset(diffnNa, !IDComparison %in% IDComparison[is.na(TE)])

# SERIOUS ADVERSE EVENTS
Obj2_SAE<-as.data.table(read.xlsx(paste0("C:\\Users\\mauri\\METHODS Dropbox\\Mauricia Davidson\\preprint-journalMD.xlsx"),
                                  cols = c(1:23,35,36:37,48,49)))
colnames(Obj2_SAE)<-make.names(colnames(Obj2_SAE))
Obj2_SAE$Serious_AE<- as.numeric(Obj2_SAE$Serious_AE)
Obj2_SAE$n_Analyzed_<- as.numeric(Obj2_SAE$n_Analyzed_)
for (i in 1:(nrow(Obj2_SAE))) {
  if (Obj2_SAE$safety_N[i] != "*") {
    Obj2_SAE$x[i]<-Obj2_SAE$safety_N[i]
  } else {
    Obj2_SAE$x[i]<-Obj2_SAE$n_Analyzed_[i]
  }
}
Obj2_SAE$x<-as.numeric(Obj2_SAE$x)
testSAE<-as.data.table(pairwise(treat = stats_name, event = Serious_AE, n = x, data = Obj2_SAE, studlab = MD_ID, sm = "OR"))
filtSAE<-testSAE[!testSAE$Same_analysis1 %in% "N"]
filtSAE$treat1=trimws(filtSAE$treat1)
filtSAE$treat2=trimws(filtSAE$treat2)
filtSAE$IDComparison=paste(filtSAE$ID_stats,"_",filtSAE$treat1,"_",filtSAE$treat2, sep="")
filtSAE$IDComparison=trimws(filtSAE$IDComparison)
samenNs<-filtSAE[(duplicated(filtSAE[,c(6,7,8,9,57)])|duplicated(filtSAE[,c(6,7,8,9,57)],fromLast = T)),]
diffnNs<-filtSAE[!(duplicated(filtSAE[,c(6,7,8,9,57)])|duplicated(filtSAE[,c(6,7,8,9,57)],fromLast = T)),] 
NYs<-subset(diffnNs, IDComparison %in% IDComparison[is.na(TE)])
SAE<-subset(diffnNs, !IDComparison %in% IDComparison[is.na(TE)])

# Combine individual data tables
target <- Reduce(intersect, list(colnames(Mort),colnames(CI),colnames(WHO7),colnames(AE),colnames(SAE)))
Mort<-as.data.frame(Mort)
nMort<-Mort[target]
nMort["which_outcome"]="Mort"
CI<-as.data.frame(CI)
nCI<-CI[target]
nCI["which_outcome"]="CI"
WHO7<-as.data.frame(WHO7)
nWHO7<-WHO7[target]
nWHO7["which_outcome"]="WHO7"
AE<-as.data.frame(AE)
nAE<-AE[target]
nAE["which_outcome"]="AE"
SAE<-as.data.frame(SAE)
nSAE<-SAE[target]
nSAE["which_outcome"]="SAE"
dscrp=rbind.data.frame(nCI,nMort,nWHO7,nAE,nSAE)
# non-duplicated Author, Year in forest plot (every other cell is blank in author and year)
dscrp$First_author<-paste0(dscrp$First_author,",")
dscrp$First_author[seq(0, nrow(dscrp),2)]<-""
dscrp$First_author=trimws(dscrp$First_author)
dscrp$Year[seq(0, nrow(dscrp),2)]<-""

res<-rma(measure="OR", yi=TE, sei=seTE, data=dscrp)
col = NA
col= ifelse(dscrp$Type_publication=="published paper","#003333",col)
col= ifelse(dscrp$Type_publication=="preprint","#669999",col)
dscrp$col=col
pch = NA
pch= ifelse(dscrp$Type_publication=="published paper",19,pch)
pch= ifelse(dscrp$Type_publication=="preprint",15,pch)
dscrp$pch=pch

dscrp$intervention <- paste(dscrp$event1,dscrp$n1,sep="/")
dscrp$control <- paste(dscrp$event2,dscrp$n2,sep="/")
dscrp$type<-c("Preprint", "Journal")
ilab <- cbind.data.frame(dscrp$type,dscrp$intervention,dscrp$control)
par(cex=1.1, font=3, col="black", family ="mono")
forest(res, xlim=c(-7,4), at=log(c(0.1, 0.5, 1, 2, 10)), atransf=exp,
       ilab = ilab,
       ilab.xpos=c(-4.9,-3.95,-3.1), ilab.pos = 4,
       cex=0.75, ylim=c(-1.5, 53.5),
       rows=c(49:48,46:45,41:40,38:37,35:34,32:31,29:28,24:23,21:20,16:15,13:12,10:9,5:4,2:1),
       slab=paste(dscrp$First_author, dscrp$Year), psize=1.25,colout=dscrp$col, pch=dscrp$pch,
       mlab = "", col = "white", border = "white", xlab="Odds Ratio")
par(cex=0.75,font=2)
text(-7, c(50.5,42.5,25.5,17.5,6.5), pos=4, c("Clinical improvement D28",
                                              "All-cause mortality D28",
                                              "WHO-CPS level 7 or above D28",
                                              "Incidence of any adverse events",
                                              "Incidence of serious adverse events"))
op <- par(cex=0.75, font=2)
text(c(-6.74,-4.55,-3.5,-2.66,3.3), 53.25, c("Study\n ","Source\n ","Intervention\nn/N", "Control\nn/N","Odds Ratio [95% CI]\n "))
legend(2.3, -0.5,"invisible pooled est",col = "white",box.col="white", text.col="white")
legend(-7, -0.2, c("Preprint RCT","Peer-reviewed journal RCT"),
       box.lwd=1, col=c("#669999","#003333"), box.col="white",pch=c(15, 19), cex=1)
#save 1200x1000

# Characteristics - preprint to peer-reviewed journal
All101<-as.data.table(fread(paste0("C:\\Users\\mauri\\METHODS Dropbox\\Mauricia Davidson\\preprint-journalMD.csv"),na.strings=c("*")))
colnames(All101)<-make.names(colnames(All101))
All101$Pub_date_online<- as.Date(All101$Pub_date_online, "%m/%d/%Y")
All101<-All101[!All101$Same_analysis %in% "N"]

pp101<-All101[All101$Type_publication=="preprint"]
c<-pp101 %>% distinct(ID_stats, .keep_all = TRUE)
summary(c$Sample_size)
c$Prospective_Registr<-as.factor(c$Prospective_Registr)
c$Fund<-as.factor(c$Fund)
c$Center<-as.factor(c$Center)
c$ROB_6_for_website<-as.factor(c$ROB_6_for_website)
for (i in 1:nrow(c)) {
  if(c$Research_question[i] == "Mild outpatients"| c$Research_question[i] == "Outpatients") {
    c[i,Setting := "Outpatients"]
  } else {
    c[i,Setting :="Hospitalized patients"]
  }
}
c$Setting<-as.factor(c$Setting)
c$Blind<-as.factor(c$Blind)
c$HIC_LIC<-as.factor(c$HIC_LIC)
summary(c$Prospective_Registr)
summary(c$Fund)
summary(c$Center)
summary(c$ROB_6_for_website)
summary(c$Setting)
summary(c$Blind)
summary(c$HIC_LIC)
# Publication time < 6 mos
nrow(c[c$Pub_date_online <= "2020-09-30"])
# Publication time: 6-12 mos
nrow(c[c$Pub_date_online >= "2020-10-01" & c$Pub_date_online <= "2021-03-31"])
# Publication time > 12 mos
nrow(c[c$Pub_date_online >= "2021-04-01"])

#Delay btwn preprint & journal publication (remove trial ID_stats from same RCT)
Each101<-All101 %>% distinct(MD_ID, .keep_all = TRUE)
for (i in 2:nrow(Each101)) {
  if(Each101$ID_stats[i] == Each101$ID_stats[i-1]) {
    Each101[i,Delay := Each101$Pub_date_online[i] - Each101$Pub_date_online[i-1]]
  } else {
    Each101[i,Delay := ""]
  }
}
Each101$Delay<-as.numeric(Each101$Delay)
summary(Each101$Delay)
