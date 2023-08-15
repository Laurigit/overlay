#tab_overlay_prellut

required_data("STG_PELISTATSIT")
rPeli <- reactiveValues(id = NA)
required_data("ADM_CURRENT_TURN")
turnData <- reactiveValues(turn = 0, data = ADM_CURRENT_TURN)
required_data("ADM_CURRENT_DMG")
damagedata <- reactiveValues(data = ADM_CURRENT_DMG)

life_totals <- reactive({
  res <- calc_life_totals(damagedata$data, 20)
  res
})




#observe if file is updated
fileUpdates <- reactiveValues(dmg = "", temp = "", turn = "", pelistats = "")



observe({

  invalidateLater(1000, session)
  temp <- as.character(file.info("../common_data/temp_data_storage.csv")$mtime)

  dmg <- as.character(file.info("../common_data/current_dmg.csv")$mtime)
  turn <- as.character(file.info("../common_data/current_turn.csv")$mtime)

  pelistats <- as.character(file.info("../common_data/UID_UUSI_PELI.RData")$mtime)

  if (temp != fileUpdates$temp) {
    required_data("ADM_DI_HIERARKIA")
    updateData("SRC_TEMP_DATA_STORAGE", ADM_DI_HIERARKIA,
               globalenv()
    )
    fileUpdates$temp <- temp
    rPeli$id <- ADM_TEMP_DATA_STORAGE[muuttuja == "Peli_ID", arvo]
  }

  if (pelistats != fileUpdates$pelistats) {
    required_data("ADM_DI_HIERARKIA")
    updateData("SRC_PELISTATSIT", ADM_DI_HIERARKIA,
               globalenv()
    )
    fileUpdates$pelistats <- pelistats
  }

  if (dmg != fileUpdates$dmg) {
    required_data("ADM_DI_HIERARKIA")
    updateData("SRC_CURRENT_DMG", ADM_DI_HIERARKIA,
               globalenv()
    )
    fileUpdates$dmg <- dmg
    damagedata$data <- ADM_CURRENT_DMG
  }

  if (turn != fileUpdates$turn) {
    required_data("ADM_DI_HIERARKIA")
    updateData("SRC_CURRENT_TURN", ADM_DI_HIERARKIA,
               globalenv()
    )
    fileUpdates$turn <- turn

    turnData$data <- ADM_CURRENT_TURN
    print(turnData$data)
  }


})


output$overlay_left_col <- renderUI({
  required_data(c("ADM_TURN_SEQ"))
  print("OVERLAY LEFT COL UPDATED")
  curr_turn <- turnData$data[, max(TSID)]
  # if (input$vasen == "Lauri") {

  lifetVasen <- life_totals()$Lifetotal[Omistaja_NM == "Lauri", Life_total]


  aloittaja <- getAloittaja(STG_PELISTATSIT, rPeli$id)

  if ( curr_turn > 0) {
    vuorotekstiAlku <- ADM_TURN_SEQ[TSID == curr_turn, Turn_text]
    if (aloittaja$Aloittaja_ID == "L") {
      Aloittaja <- "L"
      Nostaja <- "M"
    } else {
      Aloittaja <- "M"
      Nostaja <- "L"
    }

    if (ADM_TURN_SEQ[TSID == curr_turn, Starters_turn] == TRUE) {
      pelaaja_vuorossa <- Aloittaja
    } else {
      pelaaja_vuorossa <- Nostaja
    }


    vuoroTeksti <- paste0(pelaaja_vuorossa, " ", vuorotekstiAlku)
  } else {
    vuoroTeksti <- "Not started"
  }



  # }
  fluidPage(
    fluidRow(

      box(HTML(paste0('<div align="center"><font size="7" color="white"> <b>',
                      lifetVasen,
                      '</b></font></div>')),
          background = "blue",
          width = NULL)
    ),


    fluidRow(

      box(HTML(paste0('<div align="center"><font size="7" color="white"> <b>',
                      vuoroTeksti,
                      '</b></font></div>')),
          background = "blue",
          width = NULL)
    ),
)



})


output$valueBoxRows <- renderUI({
  required_data("ADM_TURN_SEQ")
  if (nrow(damagedata$data) > 0 ) {
    accpetd_dmg_row_all <- calc_life_totals(damagedata$data)$aggr_accepted
    accpetd_dmg_row <- accpetd_dmg_row_all[nrow(accpetd_dmg_row_all)]
    colori <- ifelse(accpetd_dmg_row[, Amount] > 0, "orange", "green")
    ikoni <- ifelse(accpetd_dmg_row[, Combat_dmg] == 1, "hand-rock", "bolt")
    targetti <- str_sub(accpetd_dmg_row[, Target_player], 1, 1)
    soursa <- str_sub(accpetd_dmg_row[, Dmg_source], 1, 1)
    maara <- -(accpetd_dmg_row[, Amount])
    vuoro <- accpetd_dmg_row[, TSID]
    #suunta riippuen damagen vastaanottajasta

    if (targetti == "L") {
      teksti <- "L "
    } else {
      teksti <- "R"
    }
  } else {
    teksti <- "-"
    vuoro <- 0
    ikoni <- "stop"
    colori <- "black"
    maara <- ""
    subTitle <- ""
  }

  aloittaja <- getAloittaja(STG_PELISTATSIT, rPeli$id)
  if ( vuoro > 0) {
    vuorotekstiAlku <- ADM_TURN_SEQ[TSID == vuoro, Turn_text]
    if (aloittaja$Aloittaja_ID == "L") {
      Aloittaja <- "L"
      Nostaja <- "R"
    } else {
      Aloittaja <- "R"
      Nostaja <- "L"
    }

    if (ADM_TURN_SEQ[TSID == vuoro, Starters_turn] == TRUE) {
      pelaaja_vuorossa <- Aloittaja
    } else {
      pelaaja_vuorossa <- Nostaja
    }


    subTitle <- paste0("Life changed at turn: ", pelaaja_vuorossa, " ", vuorotekstiAlku)
  }


  box(valueBox(value = tags$p(paste0("   ", teksti, " ", maara), style = "font-size: 125%;"),
               subtitle = tags$p(subTitle, style = "font-size: 125%;"),
               icon = icon(ikoni),
               color = colori,
               width = NULL), width = NULL)

})


output$valueBoxRows_prev <- renderUI({

  required_data("ADM_TURN_SEQ")

  if(nrow(damagedata$data) > 2) {
    accpetd_dmg_row_all <- calc_life_totals(damagedata$data)$aggr_accepted
    accpetd_dmg_row <- accpetd_dmg_row_all[nrow(accpetd_dmg_row_all) - 1]
    colori <- ifelse(accpetd_dmg_row[, Amount] > 0, "orange", "green")
    ikoni <- ifelse(accpetd_dmg_row[, Combat_dmg] == 1, "hand-rock", "bolt")
    targetti <- str_sub(accpetd_dmg_row[, Target_player], 1, 1)
    soursa <- str_sub(accpetd_dmg_row[, Dmg_source], 1, 1)
    maara <- -(accpetd_dmg_row[, Amount])
    vuoro <- accpetd_dmg_row[, TSID]
    #suunta riippuen damagen vastaanottajasta

    if (targetti == "Lauri") {
      teksti <- "L "
    } else {
      teksti <- "R"
    }
  } else {
    teksti <- "-"
    colori <- "black"
    ikoni <- "stop"
    vuoro <- 0
    maara <- ""
    subTitle <- ""
  }


  if ( vuoro > 0) {
    vuorotekstiAlku <- ADM_TURN_SEQ[TSID == vuoro, Turn_text]
    aloittaja <- getAloittaja(STG_PELISTATSIT, rPeli$id)
    if (aloittaja$Aloittaja_ID == "L") {
      Aloittaja <- "L"
      Nostaja <- "R"
    } else {
      Aloittaja <- "R"
      Nostaja <- "L"
    }

    if (ADM_TURN_SEQ[TSID == vuoro, Starters_turn] == TRUE) {
      pelaaja_vuorossa <- Aloittaja
    } else {
      pelaaja_vuorossa <- Nostaja
    }


    subTitle <- paste0("Life changed at turn: ", pelaaja_vuorossa, " ", vuorotekstiAlku)
  }


  box(valueBox(value = tags$p(paste0("   ", teksti, " ", maara), style = "font-size: 125%;"),
               subtitle = tags$p(subTitle, style = "font-size: 125%;"),
               icon = icon(ikoni),
               color = colori,
               width = NULL), width = NULL)

})


aika_text_reactive = reactiveValues(aika = 0, i = 0)
observe({
  required_data("ADM_TEMP_DATA_STORAGE")
  tempData <- ADM_TEMP_DATA_STORAGE

  invalidateLater(1000, session)
  pelialkuAika <- tempData[muuttuja == "Aloitus_DT", as.POSIXct(arvo, tz = "EET")]
  aikaNyt <- now(tz = "EET")
  sekunnit_yht<- as.integer(difftime(aikaNyt, pelialkuAika, units = c("secs")))
  minuutit_yht<-floor(sekunnit_yht/60)
  sekunnit<-sekunnit_yht-60*minuutit_yht
  tunnit <- floor(minuutit_yht / 60)
  minuutit <- minuutit_yht - 60 * tunnit
  sekunnit_fix <- str_pad(sekunnit, 2, pad = "0")
  minuutit_fix <- str_pad(minuutit, 2, pad = "0")
  tunnit_text <- ifelse(tunnit > 0,
                        paste0(str_pad(tunnit, 2, pad = "0"),":"),
                        "")

  aika_text_reactive$aika <- paste0(tunnit_text, minuutit_fix,":",sekunnit_fix)
})

output$overlay_right_col <- renderUI({
  # if (input$vasen == "Lauri") {

  lifetOikea <-   life_totals()$Lifetotal[Omistaja_NM == "Martti", Life_total]


  # }
  fluidPage(
    fluidRow(

      box(HTML(paste0('<div align="center"><font size="7" color="white"> <b>',
                      lifetOikea,
                      '</b></font></div>')),
          background = "blue",
          width = NULL)
    ),
    fluidRow(
      box(HTML(paste0('<div align="center"><font size="7" color="white"> <b>',
                      aika_text_reactive$aika,
                      '</b></font></div>')),
          background = "blue",
          width = NULL)),

    uiOutput("valueBoxRows"),
    uiOutput("valueBoxRows_prev")
  )

  #  column(2,
  #         valueBox(input$laurin_mulligan, subtitle = "Mulls", color = "maroon", width = NULL))

  #  plotOutput("EV_plot_ovelary"),
  #  uiOutput("PakkaRightBox_overlay")
  # ,
  # box(HTML(paste0('<div align="center"><font size="7" color="white"> <b>',
  #                 lifetOikea,
  #                 '</b></font></div>')),
  #     background = "blue",
  #     width = "100%")




  # column(3, box(HTML(paste0('<div align="center"><font size="7" color="white"> <b>',
  #                           pakkaVasen,
  #                           '</b></font></div>')),
  #               background = "maroon",
  #               width = "100%")),

})


