load("data/alldata.RData")
load("data/map_perif.RData")
load("data/map_nomoi.RData")

library(RColorBrewer)
library(sp)

barPlotYM <- function(disease, from=200401, to=as.integer(format(Sys.Date(), "%Y%m")), 
        groupBy=NA, grouptitle=NA, lang="GR",
        col=brewer.pal(8,"Set2"), border=brewer.pal(8,"Dark2")) {
  from <- max(200401, from)
  to <- min(as.integer(format(Sys.Date(), "%Y%m")), to)
  ticks <- as.integer(format(
             seq.Date(
               as.Date(sprintf("%s01", from), "%Y%m%d"), 
               as.Date(sprintf("%s01", to), "%Y%m%d"),
             by="month"),
           "%Y%m"))
  tickLab <- ticks
  if (length(tickLab)>60) {
    tickLab[!((tickLab %% 100) %in% c(1))] <- NA
  } else if (length(tickLab)>24) {
    tickLab[!((tickLab %% 100) %in% c(1,7))] <- NA
  }
  s <- subset(dat[[disease]], ymdil>=from & ymdil<=to)
  barPlotTDIST(disease, s, "ymdil", ticks, tickLab, groupBy, grouptitle, lang, col, border)
}


barPlotY <- function(disease, from=2004, to=as.integer(format(Sys.Date(), "%Y")), 
        groupBy=NA, grouptitle=NA, lang="GR",
        col=brewer.pal(8,"Set2"), border=brewer.pal(8,"Dark2")) {
  from <- max(2004, from)
  to <- min(as.integer(format(Sys.Date(), "%Y")), to)
  ticks <- from:to
  tickLab <- ticks
  s <- subset(dat[[disease]], yeardil>=from & yeardil<=to)
  barPlotTDIST(disease, s, "yeardil", ticks, tickLab, groupBy, grouptitle, lang, col, border)
}


barPlotTDIST <- function(disease, s, g, ticks, tickLab,
        groupBy=NA, grouptitle=NA, lang="GR",
        col=brewer.pal(8,"Set2"), border=brewer.pal(8,"Dark2")) {
  if (!(groupBy %in% names(s))) groupBy <- NA
  if (is.na(groupBy)) {
    bp <- barplot(table(factor(s[[g]], levels=ticks)), col=col[1], 
      main=diseaseNames[[lang]][disease],
      border=border[1], las=2, names.arg=tickLab)
  } else {
    if (exists("groupingLevels") && (groupBy %in% names(groupingLevels))) {
      grpl <- groupingLevels[[groupBy]]
    } else {
      grpl <- levels(s[[groupBy]])
    }
    par(mar=c(5,4,7,2))
    bp <- barplot(table(s[[groupBy]],factor(s[[g]], levels=ticks)), 
      #main=diseaseNames[[lang]][disease],
      col=col[1:length(levels(s[[groupBy]]))], 
      border=border[1:length(levels(s[[groupBy]]))], las=2, names.arg=tickLab)
    mtext(diseaseNames[[lang]][disease], side=3, line=4.5, cex=1.2, font=2)
    par(xpd=TRUE)
    legend("topright", legend=grpl[[lang]], inset=c(0,-0.2),
      fill=col[1:length(levels(s[[groupBy]]))], 
      border=border[1:length(levels(s[[groupBy]]))], bty="n")
    
  }
  if (sum(is.na(tickLab))>0) {
    axis(1, at=c(bp)[!is.na(tickLab)], labels=NA, lwd=0, lwd.ticks=1)
  }
}


mapPlotY <- function(disease, from=2004, to=as.integer(format(Sys.Date(), "%Y")), 
        lang="GR", palette="PuRd") {
  from <- max(2004, from)
  to <- min(as.integer(format(Sys.Date(), "%Y")), to)
  s <- subset(dat[[disease]], yeardil>=from & yeardil<=to)
  inc <- table(map_nomoi$esye_perif[match(toupper(s$nomokat), map_nomoi$esye)])[map_perif$esye_perif]*100000/map_perif$pop/(to-from+1)
  print(inc)
  grad <- as.integer(cut(inc, pretty(inc,4), include.lowest=TRUE))
  col <- brewer.pal(max(grad,na.rm=TRUE),"PuRd")
  par(mar=c(0,0,2,0))
  plot(map_perif, col=col[grad])
  mtext(diseaseNames[[lang]][disease], 
    side=3, line=0, cex=1.2, font=2)
  legend("bottomleft", 
    legend=paste(rev(rev(pretty(inc,4))[-1]), "-", pretty(inc,4)[-1], "/100.000", sep=""), 
    inset=0.05, fill=col, bty="n")
}
