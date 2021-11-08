# create_db
#
# Use this script only once: to set up the initial database and scheme
# If your Postgres database is already set up and running, you should be able to simply 'source' this script
# and it will automatically create the database 'qsdev' and a table 'glucose_records'
# Note: nothing bad should happen if you source this on an existing database (i.e. nothing will happen)

# WARNING: Be sure you know the value of R_CONFIG_ACTIVE before running this script.
# It will blindly create whatever database is in config::get("dataconnection")
#
# library(tidyverse)
#
# GLUCOSE_LAZY_FRAME <- dbplyr::tbl_lazy(tibble(time=lubridate::now(),
#                                          scan = 0.0,
#                                          hist = 0.0,
#                                          strip = 0.0,
#                                          value = 0.0,
#                                          food = "",
#                                          user_id = 0.0),
#                                        con = dbplyr::simulate_postgres())
# GLUCOSE_DATA_FRAME <-
#   tibble(time=lubridate::now(), scan = 0.0, hist = 0.0, strip = 0.0, value = 0.0, food = "", user_id = 0.0)
# NOTES_DATA_FRAME <-
#   tibble(Start=lubridate::now(), End =lubridate::now(), Activity = "Event", Comment = NA, Z = NA, user_id = 0)
# USER_DATA_FRAME <-
#   tibble(first_name = "first", last_name = "last", birthdate = as.Date("1900-01-01"), libreview_status = as.character(NA), user_id = 0)

# set the active configuration globally via Renviron.site or Rprofile.site
# Sys.setenv(R_CONFIG_ACTIVE = "tastercloud")
#Sys.setenv(R_CONFIG_ACTIVE = "local")  # save to local postgres
# Sys.setenv(R_CONFIG_ACTIVE = "localtest") # a database useful for testing
# Sys.setenv(R_CONFIG_ACTIVE = "cloud") # save to cloud
# Sys.setenv(R_CONFIG_ACTIVE = "default") # save to sqlite
# Sys.setenv(R_CONFIG_ACTIVE = "cloud")



#' @title Create a database object
#' @description With this object, there is no need to keep track of database connections in order to access
#' the Tastermonial database. Once initialized, you simply use the `$` methods to pull the glucose and notes
#' records results. If the database doesn't exist, initializing this object will create it for you.
#' @param db_config string name of configuration to be used  for this instance.
#' @import DBI magrittr dplyr
#' @export
taster_db <- function(db_config = "default") {

  conn_args = config::get(value = "dataconnection", config = db_config)
  con <- DBI::dbConnect(
    drv = conn_args$driver,
    user = conn_args$user,
    host = conn_args$host,
    port = conn_args$port,
    # dbname = conn_args$dbname,
    password = conn_args$password
  )

  GLUCOSE_DATA_FRAME <-
    tibble(timestamp=lubridate::now(), scan = 0.0, hist = 0.0, strip = 0.0, value = 0.0, food = "", user_id = 0.0)
  NOTES_DATA_FRAME <-
    tibble(Start=lubridate::now(), End =lubridate::now(), Activity = "Event", Comment = "meal", Z = NA, user_id = 0)
  USER_DATA_FRAME <-
    tibble(first_name = "first", last_name = "last", birthdate = as.Date("1900-01-01"), libreview_status = as.character(NA), user_id = 0)


  if (class(con) == "PqConnection"){
  newdb_sqlstring <-
    paste0(
      "CREATE DATABASE ",
      conn_args$dbname,
      "
            WITH
            OWNER = postgres
            ENCODING = 'UTF8'
            CONNECTION LIMIT = -1;"
    )

  ## Add a new database named with the value of conn_args$dbname if none exists on this server
  if (conn_args$dbname %in%
      DBI::dbGetQuery(con, "SELECT datname FROM pg_database WHERE datistemplate = false;")$datname)
  { message("database already exists")
    DBI::dbDisconnect(con)

  } else {
    DBI::dbSendQuery(con, newdb_sqlstring)
  }
  }

  con <- DBI::dbConnect(
    drv = conn_args$driver,
    user = conn_args$user,
    host = conn_args$host,
    port = conn_args$port,
    dbname = conn_args$dbname,
    password = conn_args$password
  )

    psi_make_table_with_index(conn_args = conn_args,
                              table_name = "glucose_records",
                              table = GLUCOSE_DATA_FRAME,
                              index = "timestamp")
    psi_make_table_with_index(conn_args = conn_args,
                              table_name = "notes_records",
                              table = NOTES_DATA_FRAME,
                              index = "Comment")
    psi_make_table_with_index(conn_args = conn_args,
                              table_name = "user_list",
                              table = USER_DATA_FRAME,
                              index = "user_id")




  thisEnv <- environment()


  db <- list(
    thisEnv = thisEnv,
    con = con,
    disconnect = function() {
      DBI::dbDisconnect(con)
    },
    glucose_records = dplyr::tbl(con, "glucose_records"),
    notes_records = dplyr::tbl(con, "notes_records"),
    notes_records_df = function() {
      collect(tbl(con, "notes_records"))
    },
    list_objects = function() {
      dbName <- conn_args$dbname
      dbHost <- conn_args$host

      objects <- DBI::dbListObjects(con)
      tables <- DBI::dbListTables(con)
      return(list(dbName=dbName, dbHost=dbHost, objects=objects, tables=tables))
    },
    #' @describeIn  table_df returns a valid table
    table_df = function(table_name = "glucose_records") {
      dplyr::collect(dplyr::tbl(con, table_name))
    }
  )



  ## Define the value of the list within the current environment.
  assign('this',db,envir=thisEnv)

  structure(db, class = "data.frame",
            con  = con )

}


#' @title Create or delete a new database as required
#' @description Intended for situations where you want to start all over,
#' this function will use direct SQL calls to either create a brand new database
#' or, if optionally `drop=TRUE`, wipe out any existing database.
#' IMPORTANT: database connection is dropped regardless, so if you need a connection,
#' be sure reconnect.
#' @param conn_args connection
#' @param db_name character string name for proposed new database
#' @param drop Nuke a database with this name if it already exists (default = `FALSE`)
#' @param force override the warning about creating a database with the magic name `qsdb` (default = `FALSE`)
make_new_database_if_necessary <- function(conn_args = config::get("dataconnection"),
                                           db_name,
                                           drop = FALSE,
                                           force = FALSE) {


  if(db_name=="qsdb") {
    message("are you absolutely certain?")
    message("I won't let you do this unless you call with the flag `force=TRUE`")

    return(NULL)
  }

  if(class(conn_args$driver) == "SQLiteDriver") {
    con <- DBI::dbConnect(
      drv = conn_args$driver,
      dbname = conn_args$dbname
    )
    return(con)
  }
  con <- DBI::dbConnect(
    drv = conn_args$driver,
    user = conn_args$user,
    host = conn_args$host,
    port = conn_args$port,
    # dbname = conn_args$dbname,
    password = conn_args$password
  )


  new_db_sql <-
    sprintf(
      "CREATE DATABASE %s WITH OWNER = %s ENCODING = 'UTF8' CONNECTION LIMIT = -1;",
      db_name,
      "postgres"
    )

  nuke_db_sql <-  sprintf(
    "DROP DATABASE IF EXISTS %s;",
    db_name
  )

  if (db_name %in% DBI::dbGetQuery(con,
                                  "SELECT datname FROM pg_database WHERE datistemplate = false;")$datname)
  {    message(sprintf("found database %s", db_name))
    if (drop) {

      message(sprintf("Nuking %s",  db_name))
      rs <- DBI::dbSendStatement(con, nuke_db_sql)
      DBI::dbClearResult(rs)
    }
    else  message(sprintf("But I'll do nothing about it"))

  }
  else   {message(sprintf("no database named %s", db_name))
    if (drop) {
      message(sprintf("I'll make a new one named %s", db_name))
      rs <- DBI::dbSendStatement(con, new_db_sql)
      DBI::dbClearResult(rs)
    }
  }
  DBI::dbDisconnect(con)
}


#' @title Make new database tables if necessary
#' @param conn_args connection
#' @param table_name character string name for the table.
#' @param table a valid glucose data frame. Never use the default value unless you are testing.
#' @import DBI
#' @return NULL if table already exists. Otherwise creates the table and returns TRUE invisibly.
psi_make_table_if_necessary <- function(conn_args = config::get("dataconnection"),
                                        table_name = "glucose_records",
                                        table = GLUCOSE_DATA_FRAME) {
  con <- DBI::dbConnect(
    drv = conn_args$driver,
    user = conn_args$user,
    host = conn_args$host,
    port = conn_args$port,
    dbname = conn_args$dbname,
    password = conn_args$password
  )

  if (DBI::dbExistsTable(con, table_name))
  {message(paste0("Table '",table_name,"' already exists"))
  } else {
   # DBI::dbCreateTable(con, table_name, table)
    message(paste("Writing to table", table_name))
    DBI::dbWriteTable(con, name = table_name, value = table, overwrite=TRUE)
  }

  DBI::dbDisconnect(con)

  return(NULL)

}

#' Read contents of `table` from the database and return it as a dataframe
#' @title Show table as a dataframe
#' @param conn_args valid data connection
#' @param table_name character string of the table name (default: `glucose_records`)
#' @import DBI
#' @return dataframe


#' @title Make a new database tables with `index`
#' @param conn_args connection
#' @param table_name character string name for the table.
#' @param table a valid glucose data frame. Never use the default value unless you are testing.
#' @param index (list) table column to be used for index
#' @import DBI
#' @return NULL if table already exists. Otherwise creates the table and returns TRUE invisibly.
psi_make_table_with_index <- function(conn_args = config::get("dataconnection"),
                                      table_name = "experiments",
                                      table = NULL, # a dataframe
                                      index = NULL
                                      ){
  con <- DBI::dbConnect(
    drv = conn_args$driver,
    user = conn_args$user,
    host = conn_args$host,
    port = conn_args$port,
    dbname = conn_args$dbname,
    password = conn_args$password
  )

  if (DBI::dbExistsTable(con, table_name))
  {message(paste0("Table '",table_name,"' already exists"))
  } else {

    message(sprintf("Writing new table %s with index %s", table_name, index))
    # Lets you create an index
    dplyr::copy_to(con, df = table, name = table_name, index = index, temporary = FALSE)
  }

  DBI::dbDisconnect(con)
}
