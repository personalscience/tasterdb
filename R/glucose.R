# Glucose


#' @title Write a glucose dataframe to the database
#' @description
#' (it's not finished yet and doesn't take into account previous entries)
#' @import  dplyr
#' @importFrom magrittr %>%
#' @param new_table valid formatted glucose dataframe
write_glucose <- function(new_table) {

  con <- DBI::dbConnect(
    drv = conn_args$driver,
    user = conn_args$user,
    host = conn_args$host,
    port = conn_args$port,
    dbname = conn_args$dbname,
    password = conn_args$password
  )

  new_records <- if (!("timestamp" %in% names(new_table))) {
    new_table %>% dplyr::rename("timestamp" = "time")
  } else new_table

  DBI::dbWriteTable(con, name = "glucose_records", value = new_records, row.names = FALSE, append = TRUE)

  # uncomment the following line
  # DBI::dbWriteTable(con, name = "notes_records", value = notes_records, row.names = FALSE, overwrite = TRUE)



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
