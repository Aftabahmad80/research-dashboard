library(DBI)
library(RMySQL)

connect_db <- function(){
  dbConnect(
    RMySQL::MySQL(),
    dbname = "research_db",
    host = "localhost",
    user = "root",
    password = ""
  )
}