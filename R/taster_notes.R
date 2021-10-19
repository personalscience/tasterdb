# reading notes


#' Read a raw Tastermonial CGM log (as downloaded from retool)
#' @param filepath path to a CSV download
taster_raw <- function( filepath = file.path(config::get("tastermonial")$datadir, "table-data.csv")) {

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

#' @title find user_id from first few chars in a name
#' @description Tastermonial users are kept in a JSON object with $email $name and $uid fields.
#' Unfortunately, the names are often not consistent with what we find in Libreview (e.g. Bude is Buda).
#' so this function attempts to match people based on the first name in the libreview list that matches initials.
#' @import dplyr stringr
#' @importFrom magrittr %>%
id_from_initial <- function(name_initials) {
  name_lookup_table <- user_df_from_libreview() %>% transmute(name = paste(first_name,last_name), user_id )
  names <- name_lookup_table %>% dplyr::filter(stringr::str_detect(name,name_initials)) %>% pull(user_id)
  first_hit <- as.numeric(first(names))
  return(if(is.na(first_hit)) 0 else first_hit)

}

#' @title usernames
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


#' Classify a Tastermonial food into limited categories
#' @param foodname a string representation of a name
#' @importFrom magrittr %>%
#' @return character string representing simplifed name
taster_classify_food <- function(foodname) {

  #' a CSV file with columns `pid`, `names`, and `simpleName` to convert from each format
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
#' @importFrom magrittr %>%
#' @param taster_note_df a notes dataframe
fill_taster_notes_from_scratch <- function(con, taster_notes_df, drop = TRUE) {


  if(drop) {
    message("removing notes records table")
    DBI::dbRemoveTable(con, "notes_records")
  }


  DBI::dbWriteTable(con, "notes_records", taster_notes_df, append = TRUE)
  message("wrote Taster Notes")
  tbl(con, "notes_records") %>% distinct(user_id)


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
#' @description There's no need to correct for time zone because the Retool data is already
#' timestamped with UTC.
#' @import  dplyr stringr lubridate
#' @importFrom magrittr %>%
#' @return dataframe
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
