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
Obj2_mort<-as.data.table(read.xlsx(paste0(".\\preprint-journalMD.xlsx"),
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
samenNm<-filtered[(duplicated(filtered[,c(6,7,8,9,56)])|duplicated(filtered[,c(6,7,8,9,56)],fromLast = T)),]
# Identify pp-pub pairs with diff events and n analyzed; Remove pp-pub pairs with same events and n analyzed
diffnNm<-filtered[!(duplicated(filtered[,c(6,7,8,9,56)])|duplicated(filtered[,c(6,7,8,9,56)],fromLast = T)),] 
# identify trials with new or deleted outcome data between pp and pub (same ID comparison, one TE = NA)
NYm<-subset(diffnNm, IDComparison %in% IDComparison[is.na(TE)])
# identify trials with different EE btwn pp and pub (same ID comparison, diff TE)
Mort<-subset(diffnNm, !IDComparison %in% IDComparison[is.na(TE)])

# CLINICAL IMPROVEMENT
Obj2_CI<-as.data.table(read.xlsx(paste0(".\\preprint-journalMD.xlsx"),
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
samenNc<-filtCI[(duplicated(filtCI[,c(6,7,8,9,56)])|duplicated(filtCI[,c(6,7,8,9,56)],fromLast = T)),]
diffnNc<-filtCI[!(duplicated(filtCI[,c(6,7,8,9,56)])|duplicated(filtCI[,c(6,7,8,9,56)],fromLast = T)),] 
NYc<-subset(diffnNc, IDComparison %in% IDComparison[is.na(TE)])
CI<-subset(diffnNc, !IDComparison %in% IDComparison[is.na(TE)])

# WHO SCORE 7 & ABOVE
Obj2_WHO7<-as.data.table(read.xlsx(paste0(".\\preprint-journalMD.xlsx"),
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
samenN7<-filtWHO7[(duplicated(filtWHO7[,c(6,7,8,9,55)])|duplicated(filtWHO7[,c(6,7,8,9,55)],fromLast = T)),]
diffnN7<-filtWHO7[!(duplicated(filtWHO7[,c(6,7,8,9,55)])|duplicated(filtWHO7[,c(6,7,8,9,55)],fromLast = T)),] 
NY7<-subset(diffnN7, IDComparison %in% IDComparison[is.na(TE)])
WHO7<-subset(diffnN7, !IDComparison %in% IDComparison[is.na(TE)])

# ADVERSE EVENTS
Obj2_AE<-as.data.table(read.xlsx(paste0(".\\preprint-journalMD.xlsx"),
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
samenNa<-filtAE[(duplicated(filtAE[,c(6,7,8,9,56)])|duplicated(filtAE[,c(6,7,8,9,56)],fromLast = T)),]
diffnNa<-filtAE[!(duplicated(filtAE[,c(6,7,8,9,56)])|duplicated(filtAE[,c(6,7,8,9,56)],fromLast = T)),] 
NYa<-subset(diffnNa, IDComparison %in% IDComparison[is.na(TE)])
AE<-subset(diffnNa, !IDComparison %in% IDComparison[is.na(TE)])

# SERIOUS ADVERSE EVENTS
Obj2_SAE<-as.data.table(read.xlsx(paste0(".\\preprint-journalMD.xlsx"),
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
samenNs<-filtSAE[(duplicated(filtSAE[,c(6,7,8,9,56)])|duplicated(filtSAE[,c(6,7,8,9,56)],fromLast = T)),]
diffnNs<-filtSAE[!(duplicated(filtSAE[,c(6,7,8,9,56)])|duplicated(filtSAE[,c(6,7,8,9,56)],fromLast = T)),] 
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
dscrp$First_author<-paste0(dscrp$First_author)
dscrp$First_author[seq(0, nrow(dscrp),2)]<-""
dscrp$First_author=trimws(dscrp$First_author)
dscrp$Year[seq(0, nrow(dscrp),2)]<-""
dscrp[1,16]<-"Horby et al."
dscrp[1,18]<-"2021 (20),(21)"
dscrp[3,16]<-"Mobarak et al."
dscrp[3,18]<-"2021 (22),(23)"
dscrp[5,16]<-"Horby et al."
dscrp[5,18]<-"2021 (24),(25)"
dscrp[7,16]<-"Zhang et al."
dscrp[7,18]<-"2020 (26),(27)"
dscrp[9,16]<-"Murai et al."
dscrp[9,18]<-"2020 (28),(29)"
dscrp[11,16]<-"Horby et al."
dscrp[11,18]<-"2021 (20),(21)"
dscrp[13,16]<-"Mobarak et al."
dscrp[13,18]<-"2021 (22),(23)"
dscrp[15,16]<-"Temesgen et al."
dscrp[15,18]<-"2021 (30),(31)"
dscrp[17,16]<-"Horby et al."
dscrp[17,18]<-"2021 (24),(25)"
dscrp[19,16]<-"Temesgen et al."
dscrp[19,18]<-"2021 (30),(31)"
dscrp[21,16]<-"Kyriazopoulou et al."
dscrp[21,18]<-"2021 (32),(33)"
dscrp[23,16]<-"Murai et al."
dscrp[23,18]<-"2020 (28),(29)"
dscrp[25,16]<-"Quinn et al."
dscrp[25,18]<-"2021 (34),(35)"
dscrp[27,16]<-"Sullivan et al."
dscrp[27,18]<-"2021 (36),(37)"
dscrp[29,16]<-"Temesgen et al."
dscrp[29,18]<-"2021 (30),(31)"
dscrp[31,16]<-"Kyriazopoulou et al."
dscrp[31,18]<-"2021 (32),(33)"

res<-rma(measure="OR", yi=TE, sei=seTE, data=dscrp)
col = NA
col= ifelse(dscrp$Type_publication=="preprint","red2",col)
col= ifelse(dscrp$Type_publication=="published paper","blue2",col)
dscrp$col=col
pch = NA
pch= ifelse(dscrp$Type_publication=="published paper",19,pch)
pch= ifelse(dscrp$Type_publication=="preprint",15,pch)
dscrp$pch=pch

dscrp$intervention <- paste(dscrp$event1,dscrp$n1,sep=" / ")
dscrp$control <- paste(dscrp$event2,dscrp$n2,sep=" / ")
dscrp$type<-c("Preprint", "Article")
ilab <- cbind.data.frame(dscrp$type,dscrp$intervention,dscrp$control)
par(cex=1.2, font=3, col="black", family ="mono")

ilab.pos <- c(-4.85,-3.9,-2.9,2.5)
dscrp$OR <- format(round(exp(dscrp$TE),digits = 2),nsmall=2)
dscrp$l.OR <- format(round(exp(dscrp$TE-1.96*dscrp$seTE),digits = 2),nsmall=2)
dscrp$u.OR <- format(round(exp(dscrp$TE+1.96*dscrp$seTE),digits = 2),nsmall=2)
dscrp$res <- paste(dscrp$OR," [",dscrp$l.OR,", ",dscrp$u.OR,"]",sep = "")
ilab$res <- dscrp$res

filename <- "forest_plot.tiff"
tiff(filename=filename,res = 300,width = 4500,height = 4000)
forest(res, xlim=c(-7,3.55), at=log(c(0.1, 0.5, 1, 2, 10)), atransf=exp,
       ilab = ilab,
       ilab.xpos=ilab.pos, ilab.pos = 4,
       cex=1, ylim=c(-2.5, 59.5),
       rows=c(55:54,52:51,49:48,44:43,41:40,38:37,35:34,32:31,29:28,24:23,21:20,16:15,13:12,10:9,5:4,2:1),
       slab=paste(dscrp$First_author, dscrp$Year), psize=1.25,colout=dscrp$col, pch=dscrp$pch,
       mlab = "", col = "white", border = "white", xlab="Odds Ratio",addfit = F,annotate = F)
par(font=2)
text(-7, c(56.5,45.5,25.5,17.5,6.5), pos=4, c("Clinical improvement D28",
                                              "All-cause mortality D28",
                                              "WHO-CPS level 7 or above D28",
                                              "Incidence of any adverse events",
                                              "Incidence of serious adverse events"),cex = 1)
op <- par(font=2)
text(c(-6.34,-4.55,-3.5,-2.5,3.05), 59.25, c("Study (Reference)\n ","Publication\nType","Intervention\nn / N", "Control\nn / N","OR [95% CI]\n "),cex = 1.2)
par(font=3)
text(-7,-4.5,"Only RCTs in which discrepancies were found are shown",cex=1,pos=4)
par(font=2)
legend(-7,-0.5, c("Preprint","Journal article"),
       box.lwd=1, col=c("red2","blue2"), box.col="white",pch=c(15, 19), cex=1)
dev.off()

# Characteristics - preprint to peer-reviewed journal
All109<-as.data.table(fread(paste0(".\\preprint-journalMD.csv"),na.strings=c("*")))
colnames(All109)<-make.names(colnames(All109))
All109$Pub_date_online<- as.Date(All109$Pub_date_online, "%m/%d/%Y")
All109<-All109[!All109$Same_analysis %in% "N"]

pp109<-All109[All109$Type_publication=="preprint"]
c<-pp109 %>% distinct(ID_stats, .keep_all = TRUE)
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
Each109<-All109 %>% distinct(MD_ID, .keep_all = TRUE)
for (i in 2:nrow(Each109)) {
  if(Each109$ID_stats[i] == Each109$ID_stats[i-1]) {
    Each109[i,Delay := Each109$Pub_date_online[i] - Each109$Pub_date_online[i-1]]
  } else {
    Each109[i,Delay := ""]
  }
}
Each109$Delay<-as.numeric(Each109$Delay)
summary(Each109$Delay)

#calculate median SS for 68 preprint only trials
data<-as.data.table(read.xlsx(paste0(".\\RCTdb_20220720.xlsx"),"PPonly", cols = c(1,3,340)))
colnames(data)<-make.names(colnames(data))
d<-data %>% distinct(ID_stats, .keep_all = TRUE)
summary(d$Sample_size)

#calculate median SS for 177 preprint trials (109 pp-pub, 68 pp only)
e<-as.data.frame(c$Sample_size)
f<-as.data.frame(d$Sample_size)
colnames(e)[1] ="Sample_size"
colnames(f)[1] ="Sample_size"
g<-rbind(e,f)
summary(g$Sample_size)


#Delay btwn preprint & journal publication (of 81 consistent RCTs)
cons<-All109[All109$Status %in% "consistent"]
conspp<-cons[cons$Type_publication=="preprint"]
c<-conspp %>% distinct(ID_stats, .keep_all = TRUE)
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
nrow(c[c$Pub_date_online <= "2020-09-30"])
nrow(c[c$Pub_date_online >= "2020-10-01" & c$Pub_date_online <= "2021-03-31"])
nrow(c[c$Pub_date_online >= "2021-04-01"])
cons<-cons %>% distinct(MD_ID, .keep_all = TRUE)
for (i in 2:nrow(cons)) {
  if(cons$ID_stats[i] == cons$ID_stats[i-1]) {
    cons[i,Delay := cons$Pub_date_online[i] - cons$Pub_date_online[i-1]]
  } else {
    cons[i,Delay := ""]
  }
}
cons$Delay<-as.numeric(cons$Delay)
summary(cons$Delay)

gp1<-as.data.frame(cons$Delay)
gp1 <- na.omit(gp1)
group <- "RCTs with consistent data"
gp1$group <-group
colnames(gp1)[1] ="Measurement"

#Delay btwn preprint & journal publication (of 8 RCTs w/ discrepancy in TE)
dte<-All109[All109$Status %in% c("discrepTE", "add_delete_discrepTE")]
dtepp<-dte[dte$Type_publication=="preprint"]
c<-dtepp %>% distinct(ID_stats, .keep_all = TRUE)
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
nrow(c[c$Pub_date_online <= "2020-09-30"])
nrow(c[c$Pub_date_online >= "2020-10-01" & c$Pub_date_online <= "2021-03-31"])
nrow(c[c$Pub_date_online >= "2021-04-01"])
dte<-dte %>% distinct(MD_ID, .keep_all = TRUE)
for (i in 2:nrow(dte)) {
  if(dte$ID_stats[i] == dte$ID_stats[i-1]) {
    dte[i,Delay := dte$Pub_date_online[i] - dte$Pub_date_online[i-1]]
  } else {
    dte[i,Delay := ""]
  }
}
dte$Delay<-as.numeric(dte$Delay)
summary(dte$Delay)

gp2<-as.data.frame(dte$Delay)
gp2 <- na.omit(gp2)
group <- "RCTs with change in effect estimate"
gp2$group <-group
colnames(gp2)[1] ="Measurement"

#Delay btwn preprint & journal publication (of 20 RCTs w/ added or deleted outcome)
addel<-All109[All109$Status %in% c("add_delete", "add_delete_discrepTE")]
addelpp<-addel[addel$Type_publication=="preprint"]
c<-addelpp %>% distinct(ID_stats, .keep_all = TRUE)
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
nrow(c[c$Pub_date_online <= "2020-09-30"])
nrow(c[c$Pub_date_online >= "2020-10-01" & c$Pub_date_online <= "2021-03-31"])
nrow(c[c$Pub_date_online >= "2021-04-01"])
addel<-addel %>% distinct(MD_ID, .keep_all = TRUE)
for (i in 2:nrow(addel)) {
  if(addel$ID_stats[i] == addel$ID_stats[i-1]) {
    addel[i,Delay := addel$Pub_date_online[i] - addel$Pub_date_online[i-1]]
  } else {
    addel[i,Delay := ""]
  }
}
addel$Delay<-as.numeric(addel$Delay)
summary(addel$Delay)

gp3<-as.data.frame(addel$Delay)
gp3 <- na.omit(gp3)
group <- "RCTs with added/deleted outcome"
gp3$group <-group
colnames(gp3)[1] ="Measurement"

my_data<-rbind(gp1,gp2,gp3)
my_data$group<-as.factor(my_data$group)
levels(my_data$group)
group_by(my_data,group) %>%
  summarise(count = n(),
    mean = mean(Measurement, na.rm = TRUE),
    sd = sd(Measurement, na.rm = TRUE),
    median = median(Measurement, na.rm = TRUE),
    IQR = IQR(Measurement, na.rm = TRUE))

install.packages("ggpubr")
# Box plots
# ++++++++++++++++++++
# Plot weight by group and color by group
library("ggplot2")
library("ggpubr")
filename <- "box_plot.tiff"
tiff(filename=filename,res = 300,width = 2900,height = 2400)
ggboxplot(my_data, x = "group", y = "Measurement", 
          color = "group", palette = c("#00AFBB", "#E7B800", "#FC4E07"),
          order = c("RCTs with consistent data","RCTs with change in effect estimate", "RCTs with added/deleted outcome"),
          ylab = "Days", xlab = "")+
  stat_compare_means()
dev.off()
