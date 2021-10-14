# Glucose


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

  maxDate <- psiCGM:::max_date_for_user(conn_args, user_id = ID)
  new_records <-
    new_table %>% dplyr::filter(time > {if(is.na(maxDate)) min(time) else maxDate}) %>%
    dplyr::filter(user_id == ID)



  message("write glucose records")

  # uncomment the following line to do the actual write to db
  DBI::dbWriteTable(con, name = "glucose_records", value = new_records, row.names = FALSE, append = TRUE)

  # uncomment the following line
  # DBI::dbWriteTable(con, name = "notes_records", value = notes_records, row.names = FALSE, overwrite = TRUE)


  DBI::dbDisconnect(con)

}

#' @title Read all CSV files again and enter them into the database
#' @return dataframe
psi_fill_glucose_records_from_scratch <- function(conn_args = config::get("dataconnection"),
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
  }

  all_glucose_records <- load_libreview_csv_from_directory()
  DBI::dbWriteTable(con, name = "glucose_records", value = all_glucose_records, row.names = FALSE, overwrite = TRUE)

  return(all_glucose_records)

}


#' @title Unified dataframe for all glucose CSV files in `path`
#' @description
#' Read glucose files in `path` and return one big dataframe with all glucose values.
#' As a neat trick, look up the `user_id` based on entries in `user_df_from_libreview` and
#' add that to the dataframe.
#' Assumes it's a valid file if it has the string "glucose" in its name.
#' @param path file path to a directory of libreview CSV files.
#' @return dataframe including `user_id` matching those for data in the notes_records
#' @export
load_libreview_csv_from_directory <- function(path = config::get("tastermonial")$datadir) {

  datafiles <- list.files(path)
  datafiles <- datafiles[datafiles %>% str_detect("glucose")]
  exceptions <- read_csv(file.path(path,"Tastermonial_Exceptions.csv")) %>% mutate(fullname=paste(first_name, last_name))

  df <- NULL
  for (d in datafiles) {
    f <-  file.path(path, d)
    libreview_name <- name_from_libreview_file(f)
    new_tz <-  if (libreview_name %in% exceptions$fullname) {
      new_tz <- filter(exceptions,fullname == libreview_name) %>% pull(timezone)
      if(!is.null(new_tz)) new_tz else Sys.timezone()
    } else Sys.timezone()
    ID <- lookup_id_from_name(libreview_name)
    message(sprintf("Reading ID = %s", ID))
    g_df <- cgmr::glucose_df_from_libreview_csv(file = f,
                                          user_id = ID)
    df <- bind_rows(df, g_df)
  }
  return(df)
  # count the number of unique user_id like this:
  # df %>% group_by(user_id) %>% summarize(n())
}

