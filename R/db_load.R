# Load database

# Set one of the following environment variables
# Sys.setenv(R_CONFIG_ACTIVE = "shinyapps")
#Sys.setenv(R_CONFIG_ACTIVE = "local")  # save to local postgres
# Sys.setenv(R_CONFIG_ACTIVE = "sandbox")
# Sys.setenv(R_CONFIG_ACTIVE = "localtest") # a database useful for testing


#' @title Crate a taster_db object and load all available Libreview glucose files from scratch
#' @param ps_database name of Postgres SQL catalog (aka database)
#' @export
load_db <- function(ps_database = "sandbox"){

  db <- taster_db(ps_database)

  # First build the user_list of all users known to the system (and their user_id)
  write_user_list_from_scratch(con=db$con, user_list = user_df_from_libreview())

  fill_glucose_records_from_scratch(con = db$con)

  fill_taster_notes_from_scratch(con = db$con, taster_notes_df = run_taster_notes())

  nutrisense_glucose <- load_nutrisense_csv_from_directory()
  message("write nutrisense records to glucose_records")
  DBI::dbWriteTable(db$con, "glucose_records", nutrisense_glucose, append = TRUE)


  return(db)

}

#' @title Create a taster_db object and load all available Libreview glucose files from scratch
#' @param db_name name of Postgres SQL catalog (aka database)
#' @export
load_db_efficient <- function(db_name = "sqldb"){

  db <- taster_db(db_name)

  # First build the user_list of all users known to the system (and their user_id)
  write_user_list_from_scratch(con=db$con, user_list = user_df_from_libreview())

  fill_glucose_records_from_scratch(con = db$con)

  fill_taster_notes_from_scratch(con = db$con, taster_notes_df = run_taster_notes())

  nutrisense_glucose <- load_nutrisense_csv_from_directory()
  message("write nutrisense records to glucose_records")
  DBI::dbWriteTable(db$con, "glucose_records", nutrisense_glucose, append = TRUE)


  return(db)

}

psi_make_glucose_table_with_index <- function (glucose_table) {
  conn_args = config::get("dataconnection")
  con <- DBI::dbConnect(
    drv = conn_args$driver,
    user = conn_args$user,
    host = conn_args$host,
    port = conn_args$port,
    dbname = conn_args$dbname,
    password = conn_args$password
  )

  psi_make_table_with_index(table_name = "glucose_records",
                            table = glucose_table,
                            index = list("timestamp")
  )

}

#' @title Write a glucose dataframe to the database
#' @description
#' WARNING: always delete the table before running this on a `user_id` that's already in the database.
#' (it's not finished yet and doesn't take into account previous entries)
#' @param new_table valid formatted glucose dataframe
psi_write_glucose <- function(conn_args = config::get("dataconnection"),
                              user_id = 1235,
                              new_table=glucose_df_from_libreview_csv(user_id = 1235)) {

  con <- DBI::dbConnect(
    drv = conn_args$driver,
    user = conn_args$user,
    host = conn_args$host,
    port = conn_args$port,
    dbname = conn_args$dbname,
    password = conn_args$password
  )

  ID <- user_id

  psi_make_table_if_necessary(conn_args = conn_args, table = new_table)


  maxDate <- tbl(con, table_name) %>% filter(user_id == ID) %>%
    filter(time == max(time, na.rm = TRUE)) %>% pull(time)

  maxDate <- if (length(maxDate > 0)) maxDate else NA

  new_records <-
    new_table %>% dplyr::filter(time > {if(is.na(maxDate)) min(time) else maxDate}) %>%
    dplyr::filter(user_id == ID)



  message(sprintf("write %d glucose records\n", nrow(new_records)))

  # uncomment the following line to do the actual write to db
  DBI::dbWriteTable(con, name = "glucose_records", value = new_records, row.names = FALSE, append = TRUE)

  # uncomment the following line
  # DBI::dbWriteTable(con, name = "notes_records", value = notes_records, row.names = FALSE, overwrite = TRUE)


  DBI::dbDisconnect(con)

}



#' @title Write a Notes CSV to the notes table in the database
#' @description
#' WARNING: this may not work correctly. debug before using
#' psi_write_notes(user_id = 1234, new_table = notes_df_from_glucose_table(user_id = 1234))
#' @param user_id user ID
#' @param new_table valid formatted notes dataframe
#' @param dry_run (default = TRUE). Run without actually writing to the database
psi_write_notes <- function(conn_args = config::get("dataconnection"),
                            user_id = 1235,
                            new_table=notes_df_from_csv(user_id = 1235),
                            dry_run = TRUE) {

  con <- DBI::dbConnect(
    drv = conn_args$driver,
    user = conn_args$user,
    host = conn_args$host,
    port = conn_args$port,
    dbname = conn_args$dbname,
    password = conn_args$password
  )

  ID <- user_id
  message("write notes records")

  if (DBI::dbExistsTable(con, "notes_records"))
  {message("Table 'notes_records' already exists")
  } else {
    message(paste("Writing to notes_records"))
    DBI::dbWriteTable(con, name = "notes_records", value = new_table, overwrite=TRUE)
  }

  maxDate <-
    tbl(con, "notes_records") %>%
    filter(user_id == ID) %>%
    filter(Start == max(Start, na.rm = TRUE)) %>% collect() %>%
    pull(Start)

  maxDate <- if(length(maxDate)>0) maxDate else {tbl(con, "notes_records") %>%
      filter(Start == min(Start)) %>% collect() %>% pull(Start) }

  new_records <-
    new_table %>%  dplyr::filter(user_id == ID) %>%
    dplyr::filter(Start > {if(is.na(maxDate)) min(Start) else maxDate})




  if(dry_run){
    message("not going to actually write this")
  } else {
    # uncomment the following line to do the actual write to db
    DBI::dbWriteTable(con, name = "notes_records", value = new_records, row.names = FALSE, append = TRUE)
  }
  return(new_records)


  DBI::dbDisconnect(con)
}


#' @title Fill database after dropping
#' @description
#'  For debugging and dev purposes only. Loads the database tables from scratch.
#'  @param conn_args database connection
#'  @param drop nuke the exiting table if true (default)
#'  @noRd
psi_fill_database_from_scratch <- function(conn_args = config::get("dataconnection"),
                                           drop = TRUE) {

  con <- DBI::dbConnect(
    drv = conn_args$driver,
    user = conn_args$user,
    host = conn_args$host,
    port = conn_args$port,
    dbname = conn_args$dbname,
    password = conn_args$password
  )

  if(drop) {
    message("removing glucose records table")
    DBI::dbRemoveTable(con, "glucose_records")
    message("removing notes records")
    DBI::dbRemoveTable(con, "notes_records")
  }
  martha_glucose <- system.file("extdata", package = "psiCGM", "Firstname1Lastname1_glucose.csv")
  richard_glucose <- system.file("extdata", package = "psiCGM", "Firstname2Lastname2_glucose.csv")
  message("write Martha glucose records")
  psi_write_glucose(conn_args = conn_args,
                    user_id = 1235,
                    new_table=glucose_df_from_libreview_csv(file = martha_glucose, user_id = 1235))

  message("Write Martha notes")
  psi_write_notes(user_id = 1235, new_table=notes_df_from_csv(user_id = 1235))

  message("write Richard glucose records")
  psi_write_glucose(conn_args = conn_args,
                    user_id = 1234,
                    new_table=glucose_df_from_libreview_csv(file = richard_glucose, user_id = 1234))

  #  message("Append Richard Notes records (from glucose_records")
  DBI::dbWriteTable(con, name = "notes_records", value = notes_df_from_glucose_table(user_id=1234), row.names = FALSE, append = TRUE)
  # psi_write_notes(user_id = 1234, new_table = notes_df_from_glucose_table(user_id = 1234), dry_run = FALSE)

  message("finished writing")

}






