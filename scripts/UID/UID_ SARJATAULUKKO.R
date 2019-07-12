#UID_SARJATAULUKKO <-
# input_Turnaus_NO <- 42
# input_BO_mode <- FALSE
# required_data(c("STG_PAKAT", "STG_PELISTATSIT"))
# required_functions("BO_conversio")
# testres <- UID_SARJATAULUKKO(27, TRUE, STG_PELISTATSIT, STG_PAKAT)
# testres
# ADM_PELIT <- STG_PELISTATSIT

UID_SARJATAULUKKO <- function(input_BO_mode, ADM_PELIT, STG_PAKAT) {
  required_functions("BO_conversio")


  input_total_mode <-  FALSE

  if (input_BO_mode == FALSE) {
    turnausData_temp <- ADM_PELIT[1 == 1]
    turnausData_temp[, Tasapeli := 0]
  } else {
    #convbo
    converted <- BO_conversio(ADM_PELIT[1 == 1])
    turnausData_temp <- converted
  }
  if(is.null(input_total_mode)) {
    input_total_mode <- FALSE
  }

  turnausData <- turnausData_temp
 # print(turnausData[, Voittaja])

  sspakat <- STG_PAKAT[ Side == 0, .(Pakka_NM, Pakka_ID)]
  joinPakat <- sspakat[turnausData, on = .(Pakka_ID)]
    divarit <- joinPakat[, .N, by = Divari][, N := NULL]
    divari_List <- NULL
    for (divari_loop in divarit[, Divari]) {
      listNM <- divari_loop
    divari_List[[listNM]] <-  joinPakat[Divari == divari_loop, .(
                                                                      Matches = sum(!is.na(Voittaja)),
                                                                      Wins = sum(Voittaja, na.rm = TRUE),
                                                                        Draws = sum(Tasapeli, na.rm = TRUE)

                                                                        ), by = .(Pakka_ID, Pakka_NM, Divari, Omistaja_ID)]
    divari_List[[listNM]][, ':=' (Losses = Matches - Wins - Draws,
                               Score = (Wins + Draws * 0.5),
                               Pakka_ID = NULL)]
    setorder(divari_List[[listNM]], -Score, Matches)

    }
   # print("UID_SARJATAULUKKO")
   # print(divari_List)
    return(divari_List)
  }
