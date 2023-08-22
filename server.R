
################
#Life counter data
#turn = TSID, Atual trn = turn in magic game.


###############

user_logged <- reactiveValues(count = 0)


shinyServer(function(input, output, session) {

  #load_scripts.R
 # print(session$clientData)



  paivittyva_custom_tournament <- my_reactivePoll(session, "CUSTOM_TOURNAMENT", "SELECT * FROM CUSTOM_TOURNAMENT", 2500, con)

  STG_CUSTOM_TOURNAMENT <- reactiveValues(data = data.table(dbSelectAll("CUSTOM_TOURNAMENT", con)))
  observe({

    STG_CUSTOM_TOURNAMENT$data <- paivittyva_custom_tournament()

  })

  paivittyva_statsi <- my_reactivePoll(session, "UID_UUSI_PELI", "SELECT * FROM UID_UUSI_PELI", 2500, con)

  STG_PELISTATSIT <- reactiveValues(data = data.table(dbSelectAll("UID_UUSI_PELI", con)))
  observe({
    STG_PELISTATSIT$data <- paivittyva_statsi()

  })



# func_login <- function(input_user_count, clientDataInput) {
#   cdata <- clientDataInput
#     login <- cdata[["url_search"]]
#
#     nimi <- word(login, 2, sep = "=")
#     print("nimi")
#     print(login)
#     print(nimi)
#     if (login == "") {
#         if (input_user_count == 1) {
#         result <- "Lauri"
#       } else {
#         result <- "Martti"
#       }
#     } else {
#     result <- nimi
#   }
#   return(result)
# }
# #user_logged$count <- user_logged$count + 1
# isolate(user_logged$count <- user_logged$count + 1)
# session$user <- isolate(func_login(user_logged$count, session$clientData))
  session$user <- "overlay"
if (session$user == "overlay") {
js$hidehead('none')
shinyjs::addClass(selector = "body", class = "sidebar-collapse")
#PALAUTA MINUT
#updateTabItems(session,"sidebarmenu", "tab_overlay")
###########
updateTabItems(session,"sidebarmenu", "tab_overlay_prellut")
}


  sourcelist <- data.table(polku = c(dir("./scripts/", recursive = TRUE)))
  sourcelist[, rivi := seq_len(.N)]
  suppressWarnings(sourcelist[, kansio := strsplit(polku, split = "/")[[1]][1], by = rivi])
  sourcelist <- sourcelist[!grep("load_scripts.R", polku)]
  sourcelist[, kansio := ifelse(str_sub(kansio, -2, -1) == ".R", "root", kansio)]

  input_kansio_list <- c(
                         "tab",
                         "root"
                         )
  for(input_kansio in input_kansio_list) {
    dir_list <- sourcelist[kansio == input_kansio, polku]
    for(filename in dir_list) {
      result = tryCatch({
        print(paste0("sourcing at server", filename))
        source(paste0("./scripts/", filename), local = TRUE)
        print(paste0("sourced at server", filename))
      }, error = function(e) {
        print(paste0("error in loading file: ", filename))
      })
    }
  }



 # load("./external_files/tilastoAsetukset.R")
  #load("./external_files/saavutusAsetukset.R")


  # output$results = renderPrint({
  #   intToUtf8(input$mydata[[1]])
  # })


  # observeEvent(input$mydata, {
  #   ekakirjain <- str_sub(intToUtf8(input$mydata[[1]], 1, 1))
  #   print(ekakirjain)
  #   if (ekakirjain == "x") {
  #     shinyjs::hide(id = "hideBox")
  #   } else {_a
  #     shinyjs::show(id = "hideBox")
  #   }
  # })

  # local_keymap <- reactiveValues(env = "normal", aika = now(), prev_key = "")
  #
  # observe({
  #   print("ENVI NORMAALIKSI")
  #   take_dep <- turnData$turn
  #   take_dep <- damage_data$data
  #   local_keymap$env <- "normal"
  #   temp <- isolate(keymap$data)
  #   temp[, Nappain := ""]
  #   keymap$data <- temp
  # })

  # observeEvent(input$mydata, {
  #   required_data("ADM_KEY_MAP")
  #   aakkoPainallus_input <- intToUtf8(input$mydata[[1]])
  #   isolate(enviro <- local_keymap$env)
  #   isolate(enviro_aikaEro <- as.numeric(now()) - as.numeric(local_keymap$aika))
  #   local_keymap$aika <- now()
  #   print("AIKAERo")
  #   isolate(print(enviro_aikaEro))
  #   if (local_keymap$prev_key != aakkoPainallus_input | enviro_aikaEro > 1) {
  #
  #     local_keymap$prev_key <- aakkoPainallus_input
  #     painaja_uus <- session$user
  #   #  tempData <-  keymap$data
  #   #  tempData[Painaja == painaja_uus, ':=' (aakkoPainallus = aakkoPainallus_input,
  #                                             #  Aika = now())]
  #   #  tempData <-  keymap$data
  #    # my_keypress <- tempData[Painaja == session$user, aakkoPainallus]
  #     toiminnot <- ADM_KEY_MAP[Nappain == aakkoPainallus_input]
  #     print("ekavaihe")
  #     print(toiminnot)
  #     if (nrow(toiminnot) > 0 ){
  #
  #       if (enviro_aikaEro > 20) {
  #
  #         local_keymap$env <- "normal"
  #         enviro <- "normal"
  #         print("envi muuttu aikaeron takia normaaliksi")
  #       }
  #
  #       my_action_row <- toiminnot[env == enviro]
  #       print("envin jalkeen action row")
  #       print(my_action_row)
  #       print("envi oli")
  #       isolate(print(local_keymap$env))
  #       if (nrow(my_action_row) > 0) {
  #         my_action_row[, ':=' (Painaja = painaja_uus,
  #                  PainoAika = now())]
  #
  #           vihunData <-  keymap$data[Painaja != painaja_uus]
  #           print(my_action_row)
  #           print(vihunData)
  #           uusData <- rbind(my_action_row, vihunData)
  #           keymap$data <- uusData
  #       }
  #     }
  #   } else {
  #     warning("painettu sama nappi")
  #   }
  # })


  #envs are "normal", shift, deal9+, lose9+


  # observe({
  #   req(keymap$data)
  # if (session$user %in% c("Lauri", "Martti")) {
  #   my_action_row <- keymap$data[Painaja == session$user]
  #
  #   #do we need validation
  #   print("toka vaihe")
  #   print(my_action_row)
  #
  #     if (my_action_row[, valid_pair] != "") {
  #       #we need validation, check opponent input
  #       my_valid_pair <- my_action_row[, valid_pair]
  #       opp_button_id <- keymap$data[Painaja != session$user & session$user %in% c("Lauri", "Martti"), button_id]
  #       Opp_time <- keymap$data[Painaja != session$user & session$user %in% c("Lauri", "Martti"), PainoAika]
  #       my_time <- my_action_row[, PainoAika]
  #       aikaErotus <- abs(difftime(my_time, Opp_time))
  #       warning(paste0("näppäinten ero oli", aikaErotus))
  #       if (opp_button_id == my_valid_pair & aikaErotus < 1.5) {
  #
  #         accept_input <- TRUE
  #       } else {
  #         accept_input <- FALSE
  #       }
  #     } else if (keymap$data[which.max(PainoAika), Painaja] != session$user) {
  #       #we dont need validation, but need to check if I pressed the button
  #       accept_input <- FALSE
  #     } else {
  #       accept_input <- TRUE
  #     }
  #
  #
  #   if (accept_input == TRUE) {
  #     #click actionutton or something else
  #     #if no button id, then dont press anything. change environment only if
  #
  #     if (nchar(my_action_row[, button_id]) > 0) {
  #       if (my_action_row[, type] == "") {
  #
  #       #  print("enabled status")
  #       #  print(isolate(my_action_row[, button_id]))
  #         #if button is enabled or we dont monitor it, then click it
  #         isolate(if (is.null(actButtonStatus[[my_action_row[, button_id]]])) {
  #           print("Nappi painettu")
  #
  #
  #           odotusaika <-  max(as.numeric(last_simulated_click$time + 0 - now()), 0)
  #           last_simulated_click$time <- now()
  #           Sys.sleep(odotusaika)
  #
  #           click(my_action_row[, button_id])
  #         } else {
  #           if (actButtonStatus[[my_action_row[, button_id]]] == TRUE) {
  #             print("Nappi painettu")
  #             #nappi oli enabled
  #             odotusaika <-  max(as.numeric(last_simulated_click$time + 0 - now()), 0)
  #             last_simulated_click$time <- now()
  #             Sys.sleep(odotusaika)
  #              click(my_action_row[, button_id])
  #           }
  #         })
  #     } else if (my_action_row[, type] == "RadioGroupButtons") {
  #        # browser()
  #         group_id <- my_action_row[, button_id]
  #         button_name <-  my_action_row[, sub_id]
  #         curr_value <- isolate(eval(parse(text = paste0("input$", group_id))))
  #         #check if button is selected
  #         selected <- button_name %in% curr_value
  #         if (selected == TRUE) {
  #           new_value <- curr_value[!curr_value %in% button_name]
  #         } else {
  #           new_value <- c(curr_value, button_name)
  #         }
  #         updateRadioGroupButtons(session, group_id, selected = new_value)
  #
  #       }
  #     }
  #     #set environment
  #    isolate(if (my_action_row[, set_env] != local_keymap$env & my_action_row[, set_env]  != "") {
  #      print("UUS ENVI ON")
  #       local_keymap$env <- my_action_row[, set_env]
  #       isolate(print(local_keymap$env))
  #     })
  #   }
  # }
  # })



  #required_data("STAT_DMG_TURN_ALL")
#  required_data("ADM_TURN_DATA_ALL")
  # sourcelist <- dir("./scripts/")
  # tab_sources <- sourcelist[grepl("tab", sourcelist)]
  #
  #
  # for(filename in tab_sources) {
  #   source(paste0("./scripts/", filename), local = TRUE)
  # }



   #obserEventit
  refresh_counter <- reactiveValues(a = 0)
  observeEvent(input$refresh,{
    refresh_counter$a <- refresh_counter$a + 1
  }, ignoreInit = TRUE, ignoreNULL = TRUE)








observeEvent(input$loginbutton, {
  shinyalert(
    callbackR = function(x) {
      session$user <- x
    },

    title = "Login", text = "Type owner name", type = "input", closeOnEsc = TRUE,
    closeOnClickOutside = FALSE, html = TRUE, showCancelButton = TRUE,
    showConfirmButton = TRUE, inputType = "text",
     confirmButtonText = "OK",
    confirmButtonCol = "#AEDEF4", cancelButtonText = "Cancel", timer = 0,
    animation = TRUE, imageUrl = NULL, imageWidth = 100,
    imageHeight = 100, className = "",
    callbackJS = NULL)

})
output$Username <- renderText({

  req(session)
  invalidateLater(10000, session)
  session$user
})

#tätä voi käyttää, jos haluaa tallentaa inputtien arvot.
# observeEvent(input$arvo_peli,{
# input_values <<- lapply(reactiveValuesToList(input), unclass)
# saveR_and_send(input_values, "input", "input_values.R")
# })
#load("./external_files/input_values.R")

})
