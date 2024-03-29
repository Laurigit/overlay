#ADM_TEMP_DATA_STORAGE
required_data(c("STG_TEMP_DATA_STORAGE"))
Peli_ID_valittu <- STG_TEMP_DATA_STORAGE[muuttuja == "Peli_ID", arvo]
pelidata_db_lta <- dbSelectAll("UID_UUSI_PELI", con)
pelidata <- pelidata_db_lta[Peli_ID == Peli_ID_valittu & Omistaja_ID == "L", .(Laurin_pakka = Pakka_ID,   Martin_pakka = Vastustajan_Pakka_ID, melt = "melt")]

melttaa <- melt.data.table(pelidata, id.vars = "melt")

uudet_rivit <- data.table(muuttuja = melttaa[, variable], arvo = melttaa[, value])
stg_storage <- STG_TEMP_DATA_STORAGE[1==1]
appendaa <- rbind(stg_storage, uudet_rivit)
ADM_TEMP_DATA_STORAGE <- appendaa

