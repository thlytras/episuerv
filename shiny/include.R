load("data/alldata.RData")
load("data/map_perif.RData")
load("data/map_nomoi.RData")

library(RColorBrewer)
library(sp)


regions <- c(NA, as.character(map_perif@data$esye_perif))
regionsNames <- list(
    EN=c("Whole country", as.character(map_perif@data$engname)),
    GR=c("Σύνολο χώρας", as.character(map_perif@data$grename)))
names(regionsNames[["EN"]]) <- regions
names(regionsNames[["GR"]]) <- regions
regions <- list(EN=regions, GR=regions)
names(regions[["EN"]]) <- regionsNames[["EN"]]
names(regions[["GR"]]) <- regionsNames[["GR"]]


barPlotYM <- function(disease, from=200401, to=as.integer(format(Sys.Date(), "%Y%m")), 
        groupBy=NA, grouptitle=NA, region=NA, lang="GR", plot=TRUE,
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
  return(barPlotTDIST(disease, s, "ymdil", ticks, tickLab, groupBy, grouptitle, region, lang, plot, col, border))
}


barPlotY <- function(disease, from=2004, to=as.integer(format(Sys.Date(), "%Y")), 
        groupBy=NA, grouptitle=NA, region=NA, lang="GR", plot=TRUE,
        col=brewer.pal(8,"Set2"), border=brewer.pal(8,"Dark2")) {
  from <- max(2004, from)
  to <- min(as.integer(format(Sys.Date(), "%Y")), to)
  ticks <- from:to
  tickLab <- ticks
  s <- subset(dat[[disease]], yeardil>=from & yeardil<=to)
  return(barPlotTDIST(disease, s, "yeardil", ticks, tickLab, groupBy, grouptitle, region, lang, plot, col, border))
}


barPlotTDIST <- function(disease, s, g, ticks, tickLab,
        groupBy=NA, grouptitle=NA, region=NA, lang="GR", plot=TRUE,
        col=brewer.pal(8,"Set2"), border=brewer.pal(8,"Dark2")) {
  if (!(groupBy %in% names(s))) groupBy <- NA
  if (!is.na(region)) s <- s[which(as.character(map_nomoi$esye_perif[match(toupper(s$nomokat), map_nomoi$esye)])==region),]
  if (is.na(groupBy)) {
    tb <- table(factor(s[[g]], levels=ticks))
    if (plot) {
      bp <- barplot(tb, col=col[1], 
        main=diseaseNames[[lang]][disease],
        border=border[1], las=2, names.arg=tickLab)
    }
    tb <- as.data.frame(tb)
  } else {
    if (exists("groupingLevels") && (groupBy %in% names(groupingLevels))) {
      grpl <- groupingLevels[[groupBy]]
    } else {
      grpl <- levels(s[[groupBy]])
    }
    tb <- table(s[[groupBy]],factor(s[[g]], levels=ticks))
    if (plot) {
      par(mar=c(5,4,7,2))
      bp <- barplot(tb, 
        col=col[1:length(levels(s[[groupBy]]))], 
        border=border[1:length(levels(s[[groupBy]]))], las=2, names.arg=tickLab)
      mtext(diseaseNames[[lang]][disease], side=3, line=4.5, cex=1.2, font=2)
      par(xpd=TRUE)
      legend("topright", legend=grpl[[lang]], inset=c(0,-0.2),
        fill=col[1:length(levels(s[[groupBy]]))], 
        border=border[1:length(levels(s[[groupBy]]))], bty="n")
    }
    tb <- as.data.frame.matrix(t(tb))
  }
  if (plot && sum(is.na(tickLab))>0) {
    axis(1, at=c(bp)[!is.na(tickLab)], labels=NA, lwd=0, lwd.ticks=1)
  }
  return(tb)
}


mapPlotY <- function(disease, from=2004, to=as.integer(format(Sys.Date(), "%Y")), 
        lang="GR", plot=TRUE, palette="PuRd") {
  from <- max(2004, from)
  to <- min(as.integer(format(Sys.Date(), "%Y")), to)
  s <- subset(dat[[disease]], yeardil>=from & yeardil<=to)
  inc <- table(map_nomoi$esye_perif[match(toupper(s$nomokat), map_nomoi$esye)])[map_perif$esye_perif]*100000/map_perif$pop/(to-from+1)
  print(inc)
  grad <- as.integer(cut(inc, pretty(inc,4), include.lowest=TRUE))
  col <- brewer.pal(max(grad,na.rm=TRUE),"PuRd")
  if (plot) {
    par(mar=c(0,0,2,0))
    plot(map_perif, col=col[grad])
    mtext(diseaseNames[[lang]][disease], 
      side=3, line=0, cex=1.2, font=2)
    legend("bottomleft", 
      legend=paste(rev(rev(pretty(inc,4))[-1]), "-", pretty(inc,4)[-1], "/100.000", sep=""), 
      inset=0.05, fill=col, bty="n")
  }
  return(cbind(map_perif@data[,c("grename","engname")], incidence=c(inc)))
}



incPlotY <- function(disease, from=2004, to=as.integer(format(Sys.Date(), "%Y")), 
        region=NA, lang="GR", plot=TRUE,
        col=c(brewer.pal(9,"Set1"), brewer.pal(8,"Dark2")[c(1,2,4,6,8)])) {
  from <- max(2004, from)
  to <- min(as.integer(format(Sys.Date(), "%Y")), to)
  ticks <- from:to
  tickLab <- ticks
  s <- subset(dat[[disease]], yeardil>=from & yeardil<=to)
  s$perif <- map_nomoi$esye_perif[match(toupper(s$nomokat), map_nomoi$esye)]
  if (is.na(region[1])) {
    tb <- table(factor(s$yeardil, levels=ticks))
    tb <- as.matrix(tb / sum(map_perif$pop) * 100000)
    # plot it
  } else {
    tb <- table(factor(s$yeardil, levels=ticks), s$perif)[,region, drop=FALSE]
    tb <- t(t(tb) / map_perif$pop[match(colnames(tb), map_perif$esye_perif)])
    tb <- tb*100000
    layout(matrix(1:2,nrow=1), widths=c(2,1))
  }
  if (plot) {
    plot(0, type="n", xlim=c(1,nrow(tb)), ylim=c(0, max(tb)), bty="l", 
      xaxt="n", xlab=NA,
      ylab=c(EN="Cases per 100.000 population per year", GR="Κρούσματα ανά 100.000 πληθυσμού κατ' έτος")[lang])
    axis(1, at=1:nrow(tb), labels=rownames(tb), las=2)
    mtext(c(EN="Year", GR="Έτος")[lang], side=1, line=3.2)
    for (i in 1:ncol(tb)) {
      points(tb[,i], col=col[i], type="o", lwd=3)
    }
    if (ncol(tb)>1 || !is.null(colnames(tb))) {
      par(mar=c(0,0,0,0))
      plot(0, type="n", axes=F, xlab=NA, ylab=NA)
      par(xpd=TRUE)
      lgnd <- as.character(list(
        EN=map_perif$engname[match(as.character(colnames(tb)), map_perif$esye_perif)],
        GR=map_perif$grename[match(as.character(colnames(tb)), map_perif$esye_perif)])[[lang]])
      legend("left", legend=lgnd, col=col[1:ncol(tb)], lwd=3, bty="n")
    }
  }
  if (is.null(colnames(tb)[1])) {
    colnames(tb)[1] <- c(EN="Whole Country", GR="Σύνολο χώρας")[lang]
  } else {
    colnames(tb) <- lgnd
  }
  return(as.data.frame.matrix(tb))
}
