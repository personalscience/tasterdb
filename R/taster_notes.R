# reading notes


#' Read

#' Read a raw Tastermonial CGM log (as downloaded from retool)
#' @param filepath path to a CSV download
#' @param format Switch to choose old versions of the format
taster_raw <- function( filepath = file.path(config::get("tastermonial")$datadir, "table-data.csv"),
                        format = NULL) {

  results_df <- if (!is.null(format)){
    readr::read_csv(filepath, col_types = cols(
      User = col_character(),
      Email = col_character(),
      `Scan time` = col_character(),
      `Before tags` = col_character(),
      `After tags` = col_character(),
      Food = col_character(),
      `Net Carb` = col_double(),
      Fiber = col_double(),
      `Start time` = col_character(),
      `End time` = col_character(),
      Notes = col_character()
    ))
  } else {

  readr::read_csv(filepath, col_types = cols(
    endEatingDate = col_character(),
    productFdcID = col_double(),
    barcode = col_double(),
    notes = col_character(),
    scannedTimeStamp = col_character(),
    afterTags = col_character(),
    isArchive = col_logical(),
    user = col_character(),
    netCarb = col_double(),
    carb = col_double(),
    servings = col_double(),
    fiber = col_double(),
    image = col_character(),
    beforeTags = col_character(),
    startEatingDate = col_character(),
    type = col_character(),
    name = col_character(),
    `_id` = col_character(),
    `__metadata` = col_character()
  ))
  }

  return(results_df)
}

#' @title find user_id from first few chars in a name
#' @description Tastermonial users are kept in a JSON object with $email $name and $uid fields.
#' Unfortunately, the names are often not consistent with what we find in Libreview (e.g. Bude is Buda).
#' so this function attempts to match people based on the first name in the libreview list that matches initials.
#' @import dplyr stringr
#' @importFrom magrittr %>%
id_from_initial <- function(name_initials) {
  name_lookup_table <- user_df_from_db() %>% transmute(name = paste(first_name,last_name), user_id )
  names <- name_lookup_table %>% dplyr::filter(stringr::str_detect(name,name_initials)) %>% pull(user_id)
  first_hit <- as.numeric(first(names))
  return(if(is.na(first_hit)) 0 else first_hit)

}

#' @title usernames
#' @param taster_raw_data the CSV downloaded from retool
taster_usernames <- function(taster_raw_data) {
  map(taster_raw_data, function(x) {unlist(jsonlite::fromJSON(x)[["name"]])}) %>% unlist() %>% str_trim() %>% unique()
}
#taster_usernames(taster_raw()$user)

taster_emails <- function(taster_raw_data) {
  map(taster_raw_data, function(x) {unlist(jsonlite::fromJSON(x)[["email"]])}) %>% unlist() %>% str_trim() %>% unique()
}
#taster_emails(taster_raw()$user)

name_from_taster <- function(json_object) {
  parsed <- jsonlite::parse_json(json_object)[["name"]]
  if (is.null(parsed)) NA
  else return(stringr::str_trim(parsed, side = "both"))
}

#' @title Find user_id from a Tastermonial Json object
#' @param json_object a JSON-formatted object
#' @export
id_from_taster <- function(json_object) {
  parsed <- jsonlite::parse_json(json_object)[["name"]]
  if (is.null(parsed)) 0
  else return(id_from_initial(stringr::str_sub(stringr::str_trim(parsed, side = "both"),1,3)))

  # map_dbl(taster_names %>% map_chr(function(x){ if(!is.null(x)) str_sub(x, 1,3)
  #   else NA}), id_from_initial)

}

### How I made the conversion file
# t <- taster_raw_all %>%
#   transmute(name = str_to_upper(str_squish(str_replace_all(name, "[^A-Za-z0-9']", " "))),
#             productFdcID=as.character(productFdcID),
#             type,barcode)
# pids <- t %>% drop_na(productFdcID) %>% distinct(pid =productFdcID) %>% pull(pid)
# up <- t %>% filter(productFdcID %in% pids) %>% distinct(name,productFdcID)
# up %>% group_by(pid = productFdcID) %>% summarize(n=n(), names = paste0(name)) %>% mutate(pid = paste0("\'",pid)) %>%
#   clipr::write_clip(object_type = c("table"))
#
# name_convert_file <- read_csv(file=file.path(config::get("tastermonial")$datadir, "Tastermonial Name Mapping.csv"), col_types = "cdcc") %>%
#   transmute(pid = str_replace_all(pid, "\'",""),names,simpleName)


#' @title Classify a Tastermonial food into limited categories
#' @description Data from the Tastermonial Retool database (see `taster_from_retool_csv()`) is
#' not standardized and can often be quite messy.  Before converting it to the `notes_records` format,
#' this function will apply the transformation from `Tastermonial Name Mapping.csv` to
#' return a canonical name for each food type.
#' @param foodname a string representation of a name
#' @importFrom magrittr %>%
#' @return character string representing simplifed name
#' @export
taster_classify_food <- function(foodname) {

  # a CSV file with columns `pid`, `names`, and `simpleName` to convert from each format
  taster_names_convert_table <- read_csv(file=file.path(config::get("tastermonial")$datadir,
                                                        "Tastermonial Name Mapping.csv"), col_types = "cdcc") %>%
    mutate(name = Comment)


  if(!is.null(foodname)){
    s <- taster_names_convert_table %>% filter(name == foodname) #%>% pull(simpleName)
    if(nrow(s)>0) return(s %>% pull(simpleName))
    else return("other")}
  else return(NA)

}

#' @title Fill all Tastermonial notes
#' @param con database connection
#' @param drop nuke current table and start over if `TRUE` (default)
#' @importFrom magrittr %>%
#' @param taster_note_df a notes dataframe
fill_taster_notes_from_scratch <- function(con, taster_notes_df, drop = TRUE) {


  if(drop) {
    message("removing notes records table")
    DBI::dbRemoveTable(con, "notes_records")
  }


  DBI::dbWriteTable(con, "notes_records", taster_notes_df, append = TRUE)
  message("wrote Taster Notes")
  tbl(con, "notes_records") %>% distinct(`user_id`)


}

#' @title Read notes from old "typeform" csv
#' @import  dplyr stringr lubridate
#' @importFrom magrittr %>%
#' @return dataframe
taster_old_typeform <- function() {

  taster_raw2_df <- taster_raw(filepath = file.path(config::get("tastermonial")$datadir, "FoodLogFireBasetable-data_ingestsept6.csv"))
  message("process Firebase-formatted data")
  taster_notes2_df <- taster_raw2_df %>% filter(!is.na(user))  %>%
    transmute(Start = lubridate::parse_date_time(startEatingDate,
                                                 tz = Sys.timezone(),
                                                 orders = c("mdy HM", "Ymd HM")),
              End = as_datetime(NA),
              Activity = "Food",
              Comment = str_to_upper(str_squish(str_replace_all(name, "[^A-Za-z,]", " "))),
              Z = as.numeric(NA) ,
              user_id = purrr::map_dbl(user, id_from_taster))

  taster_notes2_df$Comment <- map_chr(taster_notes2_df$Comment, taster_classify_food)

  return(taster_notes2_df)

}

#' @title (one-time only) Make a version of the Typeform data with correct timezone info
#' @description This function should only be needed one time. Use it to write a new file
#' and have that be the source for future access to the old Typeform data.
#' @return dataframe
transform_old_typeform <- function() {

  exceptions <- read_csv(file.path(config::get("tastermonial")$datadir,"Tastermonial_Exceptions.csv")) %>% mutate(fullname=paste(first_name, last_name))


  old <- taster_old_typeform()
  old$username <- old$user_id %>% map_chr(function(x) {name_for_user_id(x)})

  old$tz <- map_chr(old, function(x) {})

  new_tz <-  if (libreview_name %in% exceptions$fullname) {
    new_tz <- filter(exceptions,fullname == libreview_name) %>% pull(timezone)
    message(sprintf("timezone exception for %s is %s", libreview_name, new_tz))
    if(!is.null(new_tz)) new_tz else Sys.timezone()
  } else Sys.timezone()

}

#' @title Read notes from the iPhone app csv
#' @description The Tastermonial iPhone app sends each user scan to a database that I access
#' through Retool, a site that lets you download all user data as a single CSV file,
#' stored in the Tastermonial data directory as `table-data.csv`.  This function will read
#' that csv file and return a valid `notes_records` dataframe.
#' There's no need to correct for time zone because the Retool data is already
#' timestamped with UTC.
#' Important: Every food item read from Retool is converted into one of a limited number
#' of "experiment" types based on the transformation table called in `taster_classify_food()`
#' @import  dplyr stringr lubridate
#' @importFrom magrittr %>%
#' @return dataframe
#' @export
taster_from_retool_csv <- function() {
  taster_raw_df <- taster_raw(filepath = file.path(config::get("tastermonial")$datadir, "table-data.csv"))
  taster_notes_df1 <- taster_raw_df %>% transmute(Start = lubridate::parse_date_time(startEatingDate,
                                                                                     orders = c("dmY HM p z", "dmY HM z")),
                                                  End = as_datetime(NA),
                                                  Activity = "Food",
                                                  Comment = str_to_upper(str_squish(str_replace_all(name, "[^A-Za-z,]", " "))),
                                                  Z = as.numeric(NA),
                                                  user_id = purrr::map_dbl(user, id_from_taster))

  taster_notes_df1$Comment <- map_chr(taster_notes_df1$Comment, taster_classify_food)

  return(taster_notes_df1)

}

#' @title Read notes from the iPhone app csv 2112 Version
#' @description The Tastermonial iPhone app sends each user scan to a database that I access
#' through Retool, a site that lets you download all user data as a single CSV file,
#' stored in the Tastermonial data directory as `table-data.csv`.  This function will read
#' that csv file and return a valid `notes_records` dataframe.
#' There's no need to correct for time zone because the Retool data is already
#' timestamped with UTC.
#' Important: Every food item read from Retool is converted into one of a limited number
#' of "experiment" types based on the transformation table called in `taster_classify_food()`
#' @import  dplyr stringr lubridate
#' @importFrom magrittr %>%
#' @return dataframe
#' @export
taster_from_retool_csv_21 <- function(filepath =file.path(config::get("tastermonial")$datadir, "table-data-new.csv") ) {
  taster_raw_df <- taster_raw(filepath = filepath,
                              format = "21")
  taster_notes_df1 <- taster_raw_df %>%
    transmute(Start = lubridate::parse_date_time(`Start time`,
                                                 orders = c("dmY HM p z", "dmY HM z")),
              End = as_datetime(NA),
              Activity = "Food",
              Comment = str_to_upper(str_squish(str_replace_all(Food, "[^A-Za-z,]", " "))),
              Z = as.numeric(NA),
              user_id = purrr::map_dbl(User, id_from_initial))

  #taster_notes_df1$Comment <- map_chr(taster_notes_df1$Comment, taster_classify_food)

  return(taster_notes_df1)

}

#' Notes Dataframe From Specified User (2111 Format)
#' @param filepath path to a CSV file in 2111 format
#' @param username char string full name for user in CSV file
notes_df_from_taster2111 <- function(filepath = file.path(config::get("tastermonial")$datadir,
                                                          "table-data-new.csv"),
                                     username = "Richard Sprague",
                                     user_id = -1) {
  taster_raw_df <- taster_raw(filepath = file.path(config::get("tastermonial")$datadir, "table-data-new.csv"),
                              format = "21")

  taster_user_df <- taster_raw_df %>% filter(User == username)

  result <-  taster_user_df %>%
    transmute(Start = lubridate::parse_date_time(`Start time`,
                                                 orders = c("dmY HM p z", "dmY HM z")),
              End = as_datetime(NA),
              Activity = "Food",
              Comment = str_to_upper(str_squish(str_replace_all(Food, "[^A-Za-z,]", " "))),
              Z = as.numeric(NA),
              user_id = user_id)


  return(result)

}

#' @title main function to read notes
#' @import  dplyr stringr lubridate
#' @importFrom magrittr %>%
#' @export
run_taster_notes <- function(){

  message("read table_data.csv")

  taster_notes_df1 <- taster_from_retool_csv()
  taster_notes_from_old_format <- taster_old_typeform()

  # TODO taster_notes2_df must correct for time zone.

  message("combine notes data")

  taster_notes_df <- bind_rows(taster_notes_from_old_format,taster_notes_df1)

  return(taster_notes_df)
}

#' @title make a dataframe for experiments
#' @description Currently just reads the experiments as written to a CSV
#' Ideally this lets you generate a new CSV as appropriate
#' Needs to be filled in with code that reads a list of
#' experiments and prepares them for writing to the database
experiment_df <- function(filepath = file.path(config::get("tastermonial")$datadir, "tastermonial_experiments.csv")){

  exp_csv <- readr::read_csv(filepath)
  return(exp_csv)
}
