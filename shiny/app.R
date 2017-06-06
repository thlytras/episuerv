source("include.R")

library(shiny)
library(WriteXLS)

lang <- "GR"

ui <- shinyUI(fluidPage(
  
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "bootstrap.css"),
    tags$link(rel = "stylesheet", type = "text/css", href = "mine.css")
  ),
  
  uiOutput("ui_title"),
  uiOutput("ui_toprow"),
  uiOutput("ui_navlistpanel")
))

server <- shinyServer(function(input, output, session) {
  
  output$ui_toprow <- renderUI({
    fluidRow(
      column(6, uiOutput("ui_lang")),
      column(6, uiOutput("ui_lastUpd"))
    )
  })
  
  output$ui_navlistpanel <- renderUI({
    if (exists("input") && ("lang" %in% names(input))) lang <- input$lang
    curyear <- as.integer(format(Sys.Date(), "%Y"))
    curyear2 <- as.Date(c(sprintf("%s-1-15", curyear-1), sprintf("%s-12-15", curyear-1)))
    curyear3 <- as.Date(sprintf("%s-15", format(Sys.Date(), "%Y-%m")))
    curdisYM <- input$disease1YM; if (is.null(curdisYM)) curdisYM <- "tb"
    curdisY <- input$disease1Y; if (is.null(curdisY)) curdisY <- "tb"
    
    mytabs <- list(
      "",
#       tabPanel(c(EN="Plot", GR="Διάγραμμα")[lang], plotOutput("mainPlot")),
#       tabPanel(c(EN="Table", GR="Πίνακας")[lang], 
#                uiOutput("ui_mainTable"),
#                tableOutput("mainTable"),
#                downloadButton("downloadMainTable", "Download"), br(), br()
#       ),
      navbarMenu(c(EN="Basic analyses", GR="Βασικές αναλύσεις")[lang],
      
        tabPanel(
          c(  EN="Cases by year and month", 
              GR="Κρούσματα κατ'έτος και μήνα")[lang],
          sidebarLayout(
            sidebarPanel(
              selectInput("disease1YM", 
                          c(EN="Disease:", GR="Νόσημα:")[lang], 
                          choices = diseases[[lang]],
                          selected = "tb"),
              sliderInput("tYearYM",
                          c(EN="Year", GR="Έτος")[lang],
                          min = 2004, max = curyear-1, value=curyear-1, step=1,
                          ticks=FALSE, sep=""),
              sliderInput("tDistRangeYM",
                          c(EN="Date range", GR="Ημερολογιακό εύρος")[lang],
                          min = as.Date("2004-01-15","%Y-%m-%d"),
                          max = curyear3, value=curyear2, ticks=FALSE,
                          timeFormat="%Y-%m", step=30.4375),
              selectInput("groupedBy1YM", 
                          c(EN="Group by:", GR="Ομαδοποίηση κατά:")[lang], 
                          choices = groupings[[lang]][groupings[[lang]] %in% availableGroupings[[curdisYM]]],
                          selected = "none"),
              selectInput("region1YM",
                          c(EN="Geographical region:", GR="Γεωγραφική περιοχή:")[lang], 
                          choices = regions[[lang]],
                          selected = NA),
              img(src='keelpno.png', width=199, height=157, 
                  style="display: block; margin-left: auto; margin-right: auto;")
            ),
            mainPanel(
              plotOutput("plotTDistYM"),
              br(),
              downloadButton("downloadTDistYM", "Download")
            )
          )
        ),
        
        tabPanel(
          c(  EN="Cases by year",
              GR="Κρούσματα κατ'έτος")[lang],
          sidebarLayout(
            sidebarPanel(
              selectInput("disease1Y",
                          c(EN="Disease:", GR="Νόσημα:")[lang],
                          choices = diseases[[lang]],
                          selected = "tb"),
              sliderInput("tDistRangeY",
                          c(EN="Year(s)", GR="Έτος/-η:")[lang],
                          min = 2004,
                          max = as.integer(format(curyear3,"%Y")),
                          value = as.integer(format(curyear2,"%Y"))[1] + c(-5,0),
                          ticks=FALSE, step=1, sep=""),
              selectInput("groupedBy1Y", 
                          c(EN="Group by:", GR="Ομαδοποίηση κατά:")[lang], 
                          choices = groupings[[lang]][groupings[[lang]] %in% availableGroupings[[curdisY]]],
                          selected = "none"),
              selectInput("region1Y",
                          c(EN="Geographical region:", GR="Γεωγραφική περιοχή:")[lang], 
                          choices = regions[[lang]],
                          selected = NA),
              img(src='keelpno.png', width=199, height=157,
                  style="display: block; margin-left: auto; margin-right: auto;")
            ),
            mainPanel(
              plotOutput("plotTDistY"),
              br(),
              downloadButton("downloadTDistY", "Download")
            )
          )
        ),

        tabPanel(
          c(  EN="Incidence by year",
              GR="Επίπτωση κατ'έτος")[lang],
          sidebarLayout(
            sidebarPanel(
              selectInput("disease2Y",
                          c(EN="Disease:", GR="Νόσημα:")[lang],
                          choices = diseases[[lang]],
                          selected = "tb"),
              sliderInput("tDistRange2Y",
                          c(EN="Year(s)", GR="Έτος/-η:")[lang],
                          min = 2004,
                          max = as.integer(format(curyear3,"%Y")),
                          value = as.integer(format(curyear2,"%Y"))[1] + c(-5,0),
                          ticks=FALSE, step=1, sep=""),
              selectInput("region2Y",
                          c(EN="Geographical region:", GR="Γεωγραφική περιοχή:")[lang], 
                          choices = regions[[lang]][-1],
                          selected = NULL, multiple=TRUE),
              img(src='keelpno.png', width=199, height=157,
                  style="display: block; margin-left: auto; margin-right: auto;")
            ),
            mainPanel(
              plotOutput("plotTDist2Y"),
              br(),
              downloadButton("downloadTDist2Y", "Download")
            )
          )
        ),
        
        tabPanel(
          c(  EN="Geographical distribution",
              GR="Γεωγραφική κατανομή")[lang],
          sidebarLayout(
            sidebarPanel(
              selectInput("disease2Geo",
                          c(EN="Disease:", GR="Νόσημα:")[lang],
                          choices = diseases[[lang]],
                          selected = "tb"),
              sliderInput("tDistRangeGeo",
                          c(EN="Year(s)", GR="Έτος/-η:")[lang],
                          min = 2004,
                          max = as.integer(format(curyear3,"%Y")),
                          value = as.integer(format(curyear2,"%Y"))[1] + c(-5,0),
                          ticks=FALSE, step=1, sep=""),
              img(src='keelpno.png', width=199, height=157,
                  style="display: block; margin-left: auto; margin-right: auto;")
            ),
            mainPanel(
              plotOutput("plotGeoDist"),
              br(),
              downloadButton("downloadGeoDist", "Download")
            )
          )
        )
        
      )
    )
    do.call(navbarPage, c(mytabs, id="mytbp"))
  })
  
  
  output$ui_title <- renderUI({
    if (exists("input") && ("lang" %in% names(input))) lang <- input$lang
    if (lang=="GR") {
      titlePanel("episuerv - Απεικόνιση δεδομένων επιδημιολογικής επιτήρησης")
    } else {
      titlePanel("episuerv - Visualization of epidemiological surveillance data")
    }
  })
  
  
  output$ui_lang <- renderUI({
    radioButtons("lang", NA,# c(GR="Γλώσσα", EN="Language")[lang], 
                 c("Ελληνικά" = "GR", "English" = "EN"), inline=TRUE)
  })
  
  
  output$ui_lastUpd <- renderUI({
    if (exists("input") && ("lang" %in% names(input))) lang <- input$lang
    helpText(
      c(EN="Last data update: ", GR="Τελευταία ενημέρωση στοιχείων: ")[lang],
      format(analysis.time, "%Y-%m-%d %H:%M"), style="text-align:center")
  })
  
  
  # Allow year slider to control date range slider
  observe({
    tgtyear <- as.integer(input$tYearYM)
    if (length(tgtyear)==0) tgtyear <- as.integer(format(Sys.Date(), "%Y"))-1
    updateSliderInput(session, "tDistRangeYM", value=as.Date(paste(tgtyear, c("1-15","12-15"), sep="-")))
  })
  
  
  output$plotTDistYM <- renderPlot({
    if (exists("input") && ("lang" %in% names(input))) lang <- input$lang
    a <- as.integer(format(input$tDistRangeYM, "%Y%m"))
    r <- input$region1YM; r[r=="NA"] <- NA
    if (length(a>0)) {
      grp <- input$groupedBy1YM; if (grp=="none") grp <- NA
      barPlotYM(input$disease1YM, from=a[1], to=a[2], groupBy=grp, region=r, lang=lang)
    }
  })
  
  output$plotTDistY <- renderPlot({
    if (exists("input") && ("lang" %in% names(input))) lang <- input$lang
    a <- as.integer(input$tDistRangeY)
    r <- input$region1Y; r[r=="NA"] <- NA
    if (length(a>0)) {
      grp <- input$groupedBy1Y; if (grp=="none") grp <- NA
      barPlotY(input$disease1Y, from=a[1], to=a[2], groupBy=grp, region=r, lang=lang)
    }
  })

  output$plotTDist2Y <- renderPlot({
    if (exists("input") && ("lang" %in% names(input))) lang <- input$lang
    a <- as.integer(input$tDistRange2Y)
    r <- input$region2Y; r[is.null(r)] <- NA
    if (length(a>0)) {
      incPlotY(input$disease2Y, from=a[1], to=a[2], region=r, lang=lang)
    }
  })
  
  output$plotGeoDist <- renderPlot({
    if (exists("input") && ("lang" %in% names(input))) lang <- input$lang
    a <- as.integer(input$tDistRangeGeo)
    if (length(a>0)) {
      mapPlotY(input$disease2Geo, from=a[1], to=a[2], lang=lang)
    }
  })

  output$downloadTDistYM <- downloadHandler(
    filename = function() { 
      "downloaded_data.xls"
    },
    content = function(file) {
      if (exists("input") && ("lang" %in% names(input))) lang <- input$lang
      a <- as.integer(format(input$tDistRangeYM, "%Y%m"))
      r <- input$region1YM; r[r=="NA"] <- NA
      grp <- input$groupedBy1YM; if (grp=="none") grp <- NA
      out <- barPlotYM(input$disease1YM, from=a[1], to=a[2], groupBy=grp, region=r, plot=FALSE, lang=lang)
      WriteXLS("out", file, "data", row.names=TRUE)
    }
  )

  output$downloadTDistY <- downloadHandler(
    filename = function() { 
      "downloaded_data.xls"
    },
    content = function(file) {
      if (exists("input") && ("lang" %in% names(input))) lang <- input$lang
      a <- as.integer(input$tDistRangeY)
      r <- input$region1Y; r[r=="NA"] <- NA
      grp <- input$groupedBy1Y; if (grp=="none") grp <- NA
      out <- barPlotY(input$disease1Y, from=a[1], to=a[2], groupBy=grp, region=r, plot=FALSE, lang=lang)
      WriteXLS("out", file, "data", row.names=TRUE)
    }
  )
  
  output$downloadTDist2Y <- downloadHandler(
    filename = function() { 
      "downloaded_data.xls"
    },
    content = function(file) {
      if (exists("input") && ("lang" %in% names(input))) lang <- input$lang
      a <- as.integer(input$tDistRange2Y)
      r <- input$region2Y; r[is.null(r)] <- NA
      out <- incPlotY(input$disease2Y, from=a[1], to=a[2], region=r, plot=FALSE, lang=lang)
      WriteXLS("out", file, "data", row.names=TRUE)
    }
  )
  
  output$downloadGeoDist <- downloadHandler(
    filename = function() { 
      "downloaded_data.xls"
    },
    content = function(file) {
      if (exists("input") && ("lang" %in% names(input))) lang <- input$lang
      a <- as.integer(input$tDistRangeGeo)
      out <- mapPlotY(input$disease2Geo, from=a[1], to=a[2], plot=FALSE, lang=lang)
      WriteXLS("out", file, "data")
    }
  )
  

#   output$ui_syndromes <- renderUI({
#     if (exists("input") && ("lang" %in% names(input))) lang <- input$lang
#     syndroSelected <- 1
#     if (exists("input") && ("syndrome" %in% names(input))) syndroSelected <- input$syndrome
#     syndroChoices <- syndroDesc$syndroID
#     names(syndroChoices) <- syndroDesc[,lang]
#       selectInput("syndrome", label = c(EN="Syndrome", GR="Σύνδρομο")[lang], 
#                   choices = syndroChoices, 
#                   selected = syndroSelected)
#   })
#   
#   
#   output$ui_camps <- renderUI({
#     if (exists("input") && ("lang" %in% names(input))) lang <- input$lang
#     campChoices <- camps$codecamp
#     names(campChoices) <- paste(camps$codecamp, "-", camps[,lang])
#     campChoices <- c("all", campChoices)
#     names(campChoices)[1] <- c(EN="All camps", GR="Όλα")[lang]
#     campSelected <- "all"
#     if (exists("input") && ("camp" %in% names(input))) campSelected <- input$camp
#     selectInput("camp", label = c(EN="Camp", GR="Καταυλισμός")[lang], 
#                 choices = campChoices,
#                 selected = campSelected)
#   })
#   
#   
#   output$ui_rangeDates <- renderUI({
#     if (exists("input") && ("lang" %in% names(input))) lang <- input$lang
#     if (exists("input") && ("date_range" %in% names(input))) rangeDates <- input$date_range
#     if (lang=="GR") {
#       dateRangeInput("date_range", "Εύρος ημερομηνιών", 
#                      start=rangeDates[1], end=rangeDates[2], 
#                      min=rangeDates.ini[1], max=rangeDates.ini[2], format="dd-mm-yyyy",
#                      lang="el", separator=" έως ")
#     } else {
#       dateRangeInput("date_range", "Date range", 
#                      start=rangeDates[1], end=rangeDates[2], 
#                      min=rangeDates.ini[1], max=rangeDates.ini[2], format="dd-mm-yyyy",
#                      lang="en")
#     }
#   })
#   
#   
#   output$mainPlot <- renderPlot({
#     if (exists("input") && ("lang" %in% names(input))) lang <- input$lang
#     if (!exists("input") || !("camp" %in% names(input))) return()
#     if (input$camp=="all") {
#       myfit <- subset(fits[[as.integer(input$syndrome)]], dates<=input$date_range[2])
#       attr(myfit, "title") <- attr(fits[[as.integer(input$syndrome)]], "title")
#     } else {
#       myfit <- subset(fitsD[[input$camp]][[as.integer(input$syndrome)]], dates<=input$date_range[2])
#       attr(myfit, "title") <- attr(fitsD[[input$camp]][[as.integer(input$syndrome)]], "title")
#     }
#     plotOne(myfit, goback=as.integer(diff(input$date_range)), lang=lang)
#   })
#   
#   
#   output$ui_mainTable <- renderUI({
#     if (exists("input") && ("lang" %in% names(input))) lang <- input$lang
#     if (!exists("input") || !("camp" %in% names(input))) return()
#     myfit <- getMainTable()
#     if (input$camp=="all") {
#       campstring <- c(EN="All camps. ", GR="Όλα τα κέντρα. ")[lang]
#     } else {
#       campstring <- paste(input$camp, "-", camps[input$camp,lang], ".")
#     }
#     p(
#       syndroDesc[input$syndrome,lang], ". ", campstring, br(),
#       c(EN="From ", GR="Από ")[lang], 
#       format(input$date_range[1], "%d-%m-%Y"), 
#       c(EN=" to ", GR=" έως ")[lang], 
#       format(input$date_range[2], "%d-%m-%Y"), ":    ",
#       strong(sum(myfit[,2], na.rm=TRUE)),
#       c(EN=" cases, ", GR=" περιστατικά, ")[lang], 
#       strong(sum(myfit[,3], na.rm=TRUE)),
#       c(EN=" visits.", GR=" επισκέψεις.")[lang], 
#       style="font-size:1.2em")
#   })
# 
#   getMainTable <- reactive({
#     if (exists("input") && ("lang" %in% names(input))) lang <- input$lang
#     if (!exists("input") || !("camp" %in% names(input))) return()
#     if (input$camp=="all") {
#       myfit <- subset(fits[[as.integer(input$syndrome)]], dates>=input$date_range[1] & dates<=input$date_range[2])
#       attr(myfit, "title") <- attr(fits[[as.integer(input$syndrome)]], "title")
#     } else {
#       myfit <- subset(fitsD[[input$camp]][[as.integer(input$syndrome)]], dates>=input$date_range[1] & dates<=input$date_range[2])
#       attr(myfit, "title") <- attr(fitsD[[input$camp]][[as.integer(input$syndrome)]], "title")
#     }
#     #myfit$excess <- with(myfit, pmax(0,round(x-Pnb)))
#     myfit <- myfit[,c("dates","x","n","p","zscore","alerts","alarms")]
#     myfit$dates <- as.character(myfit$dates)
#     myfit$alerts <- with(myfit, c("","NAI")[alerts+1])
#     myfit$alarms <- with(myfit, c("","NAI")[alarms+1])
#     myfit$x <- as.integer(myfit$x)
#     myfit$n <- as.integer(myfit$n)
#     myfit$p <- as.character(round(myfit$p * 100, 1))
#     myfit$zscore <- round(myfit$zscore, 3)
#     if (lang=="GR") {
#       names(myfit) <- c("Ημ/νία", "Περιστατικά", "Σύνολο επισκέψεων", "Αναλογική νοσηρότητα (%)", "Z-score", "Ειδοποίηση", "Εγρήγορση")
#     } else {
#       names(myfit) <- c("Date", "Cases", "Total\nvisits", "Proportional\nmorbidity (%)", "Z-score", "Warning", "Alert")
#     }
#     return(myfit)
#   })
#   
#   
#   output$downloadMainTable <- downloadHandler(
#     filename = function() { 
#       paste("mainTable-", input$camp, "-", 
#             format(input$date_range[1], "%Y%m%d"), "-",
#             format(input$date_range[2], "%Y%m%d"), ".xls", sep="") 
#     },
#     content = function(file) {
#       out <- getMainTable()
#       out[,4] <- as.numeric(out[,4])
#       sheetname <- paste("mainTable-", input$camp, "-", 
#                          format(input$date_range[1], "%Y%m%d"), "-",
#                          format(input$date_range[2], "%Y%m%d"), sep="") 
#       WriteXLS("out", file, sheetname)
#     }
#   )
#   
#   
#   output$mainTable <- renderTable({
#     getMainTable()
#   }, align="lcccccc", striped=TRUE, digits=3, na="")
# 
#   
#   output$ui_campTable <- renderUI({
#     if (exists("input") && ("lang" %in% names(input))) lang <- input$lang
#     p(c(EN="Date: ", GR="Ημερομηνία: ")[lang], strong(format(input$date_range[2], "%d-%m-%Y")), 
#       em(c(EN="(upper limit of date range)", GR="(άνω όριο εύρους ημερομηνιών). ")[lang], style="font-size:0.8em"),
#       br(), syndroDesc[input$syndrome,lang],
#       style="font-size:1.2em")
#   })
#   
#   
#   getCampTable <- reactive({
#     if (exists("input") && ("lang" %in% names(input))) lang <- input$lang
#     myfit <- lapply(names(fitsD), function(cmp){
#       f <- subset(fitsD[[cmp]][[as.integer(input$syndrome)]], dates==input$date_range[2])
#       if (nrow(f)!=0) {
#         f$camp <- camps[cmp,lang]
#       }
#       return(f)
#     })
#     myfit <- do.call(rbind, myfit)
#     #myfit$excess <- with(myfit, pmax(0,round(x-Pnb)))
#     myfit <- myfit[,c("camp","x","n","p","zscore","alerts","alarms")]
#     myfit$alerts <- with(myfit, c("","NAI")[alerts+1])
#     myfit$alarms <- with(myfit, c("","NAI")[alarms+1])
#     myfit$x <- as.integer(myfit$x)
#     myfit$n <- as.integer(myfit$n)
#     myfit <- subset(myfit, !is.na(n))
#     myfit$p <- as.character(round(myfit$p * 100, 1))
#     myfit$zscore <- round(myfit$zscore, 3)
#     if (lang=="GR") {
#       names(myfit) <- c("Κέντρο", "Περιστατικά", "Σύνολο επισκέψεων", "Αναλογική νοσηρότητα (%)", "Z-score", "Ειδοποίηση", "Εγρήγορση")
#     } else {
#       names(myfit) <- c("Camp", "Cases", "Total\nvisits", "Proportional\nmorbidity (%)", "Z-score", "Warning", "Alert")
#     }
#     return(myfit)
#   })
# 
#   
#   output$downloadCampTable <- downloadHandler(
#     filename = function() { 
#       paste("campTable-", input$camp, "-", 
#             format(input$date_range[2], "%Y%m%d"), ".xls", sep="") 
#     },
#     content = function(file) {
#       out <- getCampTable()
#       out[,4] <- as.numeric(out[,4])
#       sheetname <- paste("campTable-", input$camp, "-", 
#                          format(input$date_range[2], "%Y%m%d"), sep="") 
#       WriteXLS("out", file, sheetname)
#     }
#   )
#   
#   
#   output$campTable <- renderTable({
#     getCampTable()
#   }, align="lcccccc", striped=TRUE, digits=3, na="")
  

})

shinyApp(ui = ui, server = server)

