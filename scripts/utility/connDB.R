connDB <- function(con) {
  con <- tryCatch({

    res <- dbFetch(dbSendQuery(con, "SHOW TABLES"))

    con
  }, error = function(ef) {

    con <<- dbConnect(MySQL(),
                      user = 'root',
                      password = 'betmtg_pw',
                      host = '35.228.73.82',
                      port = 3306,
                      dbname = 'betmtg2')
  })
  return(con)
}
