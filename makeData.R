library(foreign)

diseases <- c("tb")
diseaseNames <- list(
    EN=c("Tuberculosis"),
    GR=c("Φυματίωση"))
names(diseaseNames[["EN"]]) <- diseases
names(diseaseNames[["GR"]]) <- diseases
diseases <- list(EN=diseases, GR=diseases)
names(diseases[["EN"]]) <- diseaseNames[["EN"]]
names(diseases[["GR"]]) <- diseaseNames[["GR"]]


groupings <- c("none", "gender", "agegrp", "ethnikof", "anosokf", "entopisf")
groupingNames <- list(
    EN=c("None", "Gender", "Age group", "Nationality", "Immunosuppression", "Location"),
    GR=c("Ουδέν", "Φύλο", "Ηλικιακή ομάδα", "Εθνικότητα", "Ανοσοκαταστολή", "Εντόπιση"))
names(groupingNames[["EN"]]) <- groupings
names(groupingNames[["GR"]]) <- groupings
groupings <- list(EN=groupings, GR=groupings)
names(groupings[["EN"]]) <- groupingNames[["EN"]]
names(groupings[["GR"]]) <- groupingNames[["GR"]]


groupingLevels <- list(
  "gender" = list(
    EN = c("Men", "Women"),
    GR = c("Άνδρες", "Γυναίκες")
  ),
  "agegrp" = list(
    EN = c("0-14", "15-44", "45-64", ">=65", "Unknown"),
    GR = c("0-14", "15-44", "45-64", ">=65", "Άγνωστο")
  ),
  "ethnikof" = list(
    EN = c("Greek", "Foreign", "Unknown"),
    GR = c("Έλληνες", "Αλλοδαποί", "Αγνωστο")
  ),
  "anosokf" = list(
    EN = c("No immunosuppression", "Immunosuppression", "HIV", "Unknown"),
    GR = c("Όχι ανοσοκαταστολή", "Ανοσοκαταστολή", "HIV", "Άγνωστο")
  ),
  "entopisf" = list(
    EN = c("Pulmonary", "Extrapulmonary", "Both", "Unknown location"),
    GR = c("Πνευμονική", "Εξωπνευμονική", "Και τα δύο", "Άγνωστη εντόπιση")
  )
)


availableGroupings <- list(
  tb = c("none", "gender", "agegrp", "ethnikof", "anosokf", "entopisf")
)



dat <- list()

dat[["tb"]] <- merge(
  read.epiinfo(file("rawdata/geniki3.rec", encoding="windows-1253")),
  read.epiinfo(file("rawdata/dilofyma.rec", encoding="windows-1253")),
  by="aa")

for(x in names(dat)) {
  dat[[x]]$eponymo.x <- NULL
  dat[[x]]$onoma.x <- NULL

  dat[[x]]$yeardil <- as.integer(format(dat[[x]]$hmedil, "%Y"))
  dat[[x]]$monthdil <- as.integer(format(dat[[x]]$hmedil, "%m"))
  dat[[x]]$ymdil <- as.integer(format(dat[[x]]$hmedil, "%Y%m"))
  dat[[x]]$gender <- factor(dat[[x]]$fylo, levels=1:2, labels=c("Αρρεν","Θήλυ"))
  
  dat[[x]]$age <- dat[[x]]$hlikia
  dat[[x]]$age[which(dat[[x]]$thlikia==2)] <- floor(dat[[x]]$age[which(dat[[x]]$thlikia==2)]/12)
  dat[[x]]$age[which(dat[[x]]$thlikia==3)] <- floor(dat[[x]]$age[which(dat[[x]]$thlikia==3)]/365)
  dat[[x]]$age[is.na(dat[[x]]$age)] <- floor((dat[[x]]$hmegen[is.na(dat[[x]]$age)] - dat[[x]]$hmegen[is.na(dat[[x]]$age)])/365.25)
  dat[[x]]$age[dat[[x]]$age>100] <- NA
  dat[[x]]$agegrp <- addNA(cut(dat[[x]]$age, breaks=c(0, 15, 45, 65, 200), right=FALSE, labels=c("0-14", "15-44", "45-64", ">=65")))
  levels(dat[[x]]$agegrp)[5] <- "Αγν.Ηλικία"

}



dat$tb$age <- dat$tb$hlikia
dat$tb$age[which(dat$tb$thlikia==2)] <- floor(dat$tb$age[which(dat$tb$thlikia==2)]/12)
dat$tb$age[which(dat$tb$thlikia==3)] <- floor(dat$tb$age[which(dat$tb$thlikia==3)]/365)
dat$tb$age[is.na(dat$tb$age)] <- floor((dat$tb$hmegen[is.na(dat$tb$age)] - dat$tb$hmegen[is.na(dat$tb$age)])/365.25)
dat$tb$age[dat$tb$age>100] <- NA
dat$tb$agegrp <- addNA(cut(dat$tb$age, breaks=c(0, 15, 45, 65, 200), right=FALSE, labels=c("0-14", "15-44", "45-64", ">=65")))
levels(dat$tb$agegrp)[5] <- "Αγν.Ηλικία"

dat$tb$ethnikof <- factor(dat$tb$ethniko, labels=c("Έλληνες", "Αλλοδαποί", "Αγν.Εθν."))
dat$tb$ethnikof[is.na(dat$tb$ethnikof)] <- "Αγν.Εθν."

dat$tb$entopis[is.na(dat$tb$entopis)] <- 9
dat$tb$entopisf <- factor(dat$tb$entopis, labels=c("Πνευμονική", "Εξωπνευμονική", "Πν & εξωπν", "Άγν. εντόπιση"))

dat$tb$kratisif <- factor(dat$tb$kratisi, labels=c("Φυλακές", "Κρατητήρια ΑΤ", "Κέντρα Κράτησης", "Κέντρα 1ης Υποδοχής"))

dat$tb$anosok[match(names(table(subset(dat$tb, anosok==0)$anosok1))[-(1:3)], dat$tb$anosok1)] <- 1
dat$tb$anosok[dat$tb$anosok==8] <- 9
dat$tb$anosokf <- factor(dat$tb$anosok, labels=c("Όχι ανοσοκαταστολή", "Ανοσοκαταστολή", "HIV", "Άγνωστο"))
dat$tb$anosokf[is.na(dat$tb$anosokf)] <- "Άγνωστο"



analysis.time <- Sys.time()


save.image("shiny/data/alldata.RData")
