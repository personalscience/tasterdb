# Glucose


#' @title Write a glucose dataframe to the database
#' @description
#' For backwards compatibility, it allows a table in cgmr original glucose_records format.
#' @param con valid database connection
#' @param glucose_df new valid glucose dataframe to write to this database 'glucose_records' table.
#' @param dry_run logical (default = TRUE) don't actually write to database.
#' @param user_id user ID
#' @return the records written to the database
#' @import  dplyr
#' @importFrom magrittr %>%
db_write_glucose <- function(con, glucose_df, user_id = 1234, dry_run = TRUE ) {

  ID = user_id

  new_records <- if (!("timestamp" %in% names(glucose_df))) {
    glucose_df %>% dplyr::rename("timestamp" = "time")
  } else glucose_df


  maxDate <- tbl(con, "glucose_records") %>% filter(.data[["user_id"]] == ID) %>%
    filter(.data[["timestamp"]] == max(.data[["timestamp"]], na.rm = TRUE)) %>% pull("timestamp")

  maxDate <- if (length(maxDate > 0)) maxDate else NA

  new_records <-
    new_records %>%
    dplyr::filter(.data[["timestamp"]] > {if(is.na(maxDate)) min(.data[["timestamp"]]) else maxDate}) %>%

    bind_cols(user_id=ID)



  if (dry_run){
    message(sprintf("Want to write %d records to glucose_table", nrow(new_records)))
  } else {
    DBI::dbWriteTable(con,
                      name = "glucose_records",
                      value = new_records,
                      row.names = FALSE,
                      append = TRUE)
  }


  return(new_records)
}

#' @title Write a notes dataframe to the database
#' @description
#' For backwards compatibility, it allows a table in cgmr original notes_records format.
#' @param con valid database connection
#' @param notes_df new valid glucose dataframe to write to this database 'glucose_records' table.
#' @param dry_run logical (default = TRUE) don't actually write to database.
#' @param user_id user ID
#' @return the records written to the database
#' @import  dplyr
#' @importFrom magrittr %>%
db_write_notes <- function(con, notes_df, user_id = 1234, dry_run = TRUE ) {

  ID = user_id

  new_records <- if (("SOMETHING" %in% names(notes_df))) {  # NOT USED but may be handy later
    notes_df %>% dplyr::rename("timestamp" = "time")
  } else notes_df


  maxDate <- tbl(con, "notes_records") %>% filter(.data[["user_id"]] == ID) %>%
    filter(.data[["Start"]] == max(.data[["Start"]], na.rm = TRUE)) %>% pull("Start")

  maxDate <- if (length(maxDate > 0)) maxDate else NA

  new_records <-
    new_records %>%
    dplyr::filter(.data[["Start"]] > {if(is.na(maxDate)) min(.data[["Start"]]) else maxDate}) %>%

    bind_cols(user_id=ID)



  if (dry_run){
    message(sprintf("Want to write %d records to notes_table", nrow(new_records)))
  } else {
    DBI::dbWriteTable(con,
                      name = "notes_records",
                      value = new_records,
                      row.names = FALSE,
                      append = TRUE)
  }


  return(new_records)
}


#' @title Read all CSV files again and enter them into the database
#' @return dataframe
fill_glucose_records_from_scratch <- function(con,
                                                  drop = TRUE) {



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
#' @importFrom magrittr %>%
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
      message(sprintf("timezone exception for %s is %s", libreview_name, new_tz))
      if(!is.null(new_tz)) new_tz else Sys.timezone()
    } else Sys.timezone()
    ID <- user_id_for_name(libreview_name)
    message(sprintf("Reading ID = %s", ID))
    g_df <- cgmr::glucose_df_from_libreview_csv(file = f,
                                          user_id = ID)
    g_df$time <- lubridate::force_tz(g_df$time, tz= new_tz )
    df <- bind_rows(df, g_df)
  }
  return(df)
  # count the number of unique user_id like this:
  # df %>% group_by(user_id) %>% summarize(n())
}

#' @title Unified dataframe for all glucose CSV files in `path`
#' @description
#' Read glucose files in `path` and return one big dataframe with all glucose values.
#' @importFrom magrittr %>%
#' @export
load_nutrisense_csv_from_directory <- function(path = config::get("tastermonial")$datadir) {

  datafiles <- list.files(path)
  datafiles <- datafiles[datafiles %>% str_detect("nutrisense")]
  exceptions <- read_csv(file.path(path,"Tastermonial_Exceptions.csv")) %>% mutate(fullname=paste(first_name, last_name))

  df <- NULL
  for (d in datafiles) {
    f <-  file.path(path, d)
    nutrisense_results <- cgmr::nutrisense_results(f)
    nutrisense_name <- nutrisense_results[["username"]]

    new_tz <-  if (nutrisense_name %in% exceptions$fullname) {
      new_tz <- filter(exceptions,fullname == nutrisense_name) %>% pull(timezone)
      message(sprintf("timezone exception for %s is %s", nutrisense_name, new_tz))
      if(!is.null(new_tz)) new_tz else Sys.timezone()
    } else Sys.timezone()
    ID <- user_id_for_name(nutrisense_name)
    message(sprintf("Reading ID = %s", ID))
    g_df <- nutrisense_results[["glucose_raw"]]
    g_df[["user_id"]] <- ID
    g_df$time <- lubridate::force_tz(g_df$time, tz= new_tz )
    df <- bind_rows(df, g_df)
  }
  return(df)
  # count the number of unique user_id like this:
  # df %>% group_by(user_id) %>% summarize(n())
}
