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


output$overlay_sarjataulukko <- renderDataTable({
  required_data(c("STG_PELISTATSIT", "STG_PAKAT"))


  Data <-  UID_SARJATAULUKKO(FALSE, STG_PELISTATSIT, STG_PAKAT)
  # print(eR_UID_SARJATAULUKKO())

  peli_ID_temp <- rPeli$id
  if(is.na(peli_ID_temp)) {
    peli_ID_temp <- STG_PELISTATSIT[, max(Peli_ID)]
  }
  peliDivari <- STG_PELISTATSIT[Peli_ID == peli_ID_temp, max(Divari)]

  DivariData <- Data[[peliDivari]]
  #print(DivariData)
  if(!is.null(DivariData)) {
    outputData <- DivariData[, .(Pakka_NM,
                                 Matches,
                                 Wins)]
  } else {
    outputData <- data.table(not_played = "-")
  }

 # data.table(outputData, colnames = NULL)
  #mtcars <- data.table(mtcars, rownames = FALSE, colnames = FALSE)
  noCol <- datatable(outputData, rownames = FALSE,colnames=NULL, selection = 'none',
                     options = list(columnDefs = list(list(className = 'dt-center', targets = 0:2)),
                                                      dom = 't',
                                    ordering = F))
  print(noCol)
  return(noCol)
  #print(Data)
},    options = list(
  paging = FALSE,
  searching = FALSE,
  info = FALSE
   ),  rownames = FALSE)


output$turnaustilanne_overlay <- renderValueBox({
#take dep
  print(fileUpdates$pelistats)
  ###
  sarjataulukkoData <-   UID_SARJATAULUKKO(FALSE, STG_PELISTATSIT, STG_PAKAT)
  total <- do.call(rbind, sarjataulukkoData)
  Lvoitot <- total[Omistaja_ID == "L", sum(Score)]
  Mvoitot <- total[Omistaja_ID == "M" , sum(Score)]
  tilanneteksti <- paste0(Lvoitot,
                          "-",
                          Mvoitot)
  box(valueBox(tilanneteksti,"Score" ,icon = icon("trophy",lib = "font-awesome"), width = 12, color = "blue"))
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
        uiOutput("PakkaLeftBox_overlay")) ,


    fluidRow(

             box(HTML(paste0('<div align="center"><font size="7" color="white"> <b>',
                                   vuoroTeksti,
                                              '</b></font></div>')),
                                  background = "blue",
                                  width = NULL)
    ),
    fluidRow(uiOutput("PakkaVSBox_overlay")))



  })


output$valueBoxRows <- renderUI({
  required_data("ADM_TURN_SEQ")
  if (nrow(damagedata$data) > 0 ) {
  accpetd_dmg_row_all <- calc_life_totals(damagedata$data)$aggr_accepted
  accpetd_dmg_row <- accpetd_dmg_row_all[nrow(accpetd_dmg_row_all)]
  colori <- ifelse(accpetd_dmg_row[, Amount] > 0, "maroon", "green")
  ikoni <- ifelse(accpetd_dmg_row[, Combat_dmg] == 1, "hand-rock", "bolt")
  targetti <- str_sub(accpetd_dmg_row[, Target_player], 1, 1)
  soursa <- str_sub(accpetd_dmg_row[, Dmg_source], 1, 1)
  maara <- abs(accpetd_dmg_row[, Amount])
  vuoro <- accpetd_dmg_row[, TSID]
  #suunta riippuen damagen vastaanottajasta
  if (targetti == "Lauri") {
    teksti <- paste0(targetti, " <- ", soursa)
  } else {
    teksti <-  paste0(soursa, " -> ", targetti)
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
      Nostaja <- "M"
    } else {
      Aloittaja <- "M"
      Nostaja <- "L"
    }

    if (ADM_TURN_SEQ[TSID == vuoro, Starters_turn] == TRUE) {
      pelaaja_vuorossa <- Aloittaja
    } else {
      pelaaja_vuorossa <- Nostaja
    }


    subTitle <- paste0(pelaaja_vuorossa, " ", vuorotekstiAlku)
  }


 box(valueBox(value = tags$p(paste0("   ", maara, "  ", teksti), style = "font-size: 125%;"),
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
  colori <- ifelse(accpetd_dmg_row[, Amount] > 0, "maroon", "green")
  ikoni <- ifelse(accpetd_dmg_row[, Combat_dmg] == 1, "hand-rock", "bolt")
  targetti <- str_sub(accpetd_dmg_row[, Target_player], 1, 1)
  soursa <- str_sub(accpetd_dmg_row[, Dmg_source], 1, 1)
  maara <- abs(accpetd_dmg_row[, Amount])
  vuoro <- accpetd_dmg_row[, TSID]
  #suunta riippuen damagen vastaanottajasta
  if (targetti == "Lauri") {
    teksti <- paste0(targetti, " <- ", soursa)
  } else {
    teksti <-  paste0(soursa, " -> ", targetti)
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
      Nostaja <- "M"
    } else {
      Aloittaja <- "M"
      Nostaja <- "L"
    }

    if (ADM_TURN_SEQ[TSID == vuoro, Starters_turn] == TRUE) {
      pelaaja_vuorossa <- Aloittaja
    } else {
      pelaaja_vuorossa <- Nostaja
    }


    subTitle <- paste0(pelaaja_vuorossa, " ", vuorotekstiAlku)
  }


  box(valueBox(value = tags$p(paste0("   ", maara, "  ", teksti), style = "font-size: 125%;"),
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
 uiOutput("PakkaRightBox_overlay"))

    ,
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

output$PakkaLeftBox_overlay <- renderUI({
  if (!is.na(rPeli$id)) {
    result <- getDeckStats("Lauri", UID_UUSI_PELI, rPeli$id)
    result_data <- result$data
    #  print("output$PakkaLeftBox")

    #luo riippuvuus
    ############
    box(
      solidHeader = FALSE,
      collapsible = FALSE,
      width = NULL,
      boxProfile(
        src = paste0(result_data$Most_same_card, ".jpg"),
        title = result_data$Deck,
        boxProfileItemList(
          bordered = TRUE,
          boxProfileItem(
            title = "Win%",
            description = result_data$`Win%`
          ),
          boxProfileItem(
            title = "Win%-MA",
            description = result_data$`Win%-MA`
          ),
          boxProfileItem(
            title = "Streak",
            description = result_data$Streak
          ),
          boxProfileItem(
            title = "Cards",
            description = result_data$Cards
          ),
          boxProfileItem(
            title = "Shuffle8",
            description = result_data$Shuffle8
          )
        )
      )
    )
  } else {
    "Not scheduled"
  }
})


output$PakkaRightBox_overlay <- renderUI({
  #luo riippuvuus

  if (!is.na(rPeli$id)) {
    # browser()
    ############
    result <- getDeckStats("Martti", UID_UUSI_PELI, rPeli$id)
    result_data <- result$data

    box(
      tags$head(tags$style(HTML('
                                .boxProfileItem {
                                font-family: "Georgia", Times, "Times New Roman", serif;
                                font-weight: bold;
                                font-size: 24px;
                                }
                                '))),
      solidHeader = FALSE,
      collapsible = FALSE,
      width = NULL,
      boxProfile(
        src = paste0(result_data$Most_same_card, ".jpg"),
        title = result_data$Deck,
        boxProfileItemList(
          bordered = TRUE,
          boxProfileItem(
            title = "Win%",
            description = result_data$`Win%`
          ),
          boxProfileItem(
            title = "Win%-MA",
            description = result_data$`Win%-MA`
          ),
          boxProfileItem(
            title = "Streak",
            description = result_data$Streak
          ),
          boxProfileItem(
            title = "Cards",
            description = result_data$Cards
          ),
          boxProfileItem(
            title = "Shuffle8",
            description = result_data$Shuffle8
          )
        )
      )
      )
} else {
  "Not scheduled"
}

  })


output$PakkaVSBox_overlay <- renderUI({
  #required_data("UID_UUSI_PELI", TRUE)
  #rm(eR_UID_UUSI_PELI)

  if (!is.na(rPeli$id)) {
    #luo riippuvuus

    ############
    #eR_UID_UUSI_PELI <- required_reactive("UID_UUSI_PELI", "eR_UID_UUSI_PELI")
    result <- getVSStatsHtml(UID_UUSI_PELI, "Lauri", rPeli$id)
    result_data <- result$data
    #box(HTML(result), background = "aqua", width = NULL, align = "middle")
    box(

      solidHeader = FALSE,
      collapsible = FALSE,
      width = NULL,
      boxProfile(
        src = paste0(result_data$get_aloittaja_image, ".jpg"),
        title = result_data$otsikko,
        boxProfileItemList(
          bordered = TRUE,
          boxProfileItem(
            title = "Win%",
            description = result_data$`Win%`
          ),
          boxProfileItem(
            title = "Win%-MA",
            description = result_data$`Win%-MA`
          ),
          boxProfileItem(
            title = "Streak",
            description = result_data$Streak
          ),
          boxProfileItem(
            title = "Games",
            description = result_data$Games
          ),
          boxProfileItem(
            title = "Prediction",
            description = result_data$Prediction
          )
        )
      )
    )
  } else {
    "Not scheduled"
  }
})
