# create users

#' @title Return user list from Tastermonial Libreview download
#' @description A Libreview "practice" stores all its user information in a single
#' CSV file, which this function will convert into a canonical dataframe.
#' @param file the main file downloaded from a Libreview practice ID
user_df_from_csv <- function(file = file.path(config::get("tastermonial")$datadir, "Tastermonial_allPatients_dashboard.csv")){
  user_df <- readr::read_csv(file = file,
                             skip =1,
                             col_types = cols()) %>%
    transmute(first_name = `First Name`,
              last_name = `Last Name`,
              birthdate = lubridate::mdy(`Date of Birth`),
              latest_data = `Last Available Data`,
              libreview_status = `LibreView User Status`
    )

  return(user_df)
}


extra_user_df <- read_csv(file = file.path(config::get("tastermonial")$datadir, "Tastermonial_Extra_Users.csv"),
                          col_types = "cccccd") %>% mutate(birthdate = lubridate::mdy(birthdate))

#' @title Users known to Libreview Practice Portal
#' @description
#' A dataframe of all users and their ids, taken from the Libreview practice portal
user_df_from_libreview <-
  user_df_from_csv() %>% mutate(user_id = row_number() + 1000) %>%
  dplyr::anti_join(extra_user_df,
                   by = c("first_name", "last_name")) %>% bind_rows(extra_user_df)
#usethis::use_data(user_df_from_libreview, overwrite = TRUE)

# psi User Management Functions

#' @title All user records in the database
#' @param conn_args database connection
#' @return dataframe of all user records
#' @export
user_df_from_db <- function(conn_args = config::get("dataconnection")){
  con <- DBI::dbConnect(
    drv = conn_args$driver,
    user = conn_args$user,
    host = conn_args$host,
    port = conn_args$port,
    dbname = conn_args$dbname,
    password = conn_args$password
  )

  users_df <- tbl(con, "user_list" ) %>% collect()

  DBI::dbDisconnect(con)
  return(users_df)

}

#'@title Find username associated with an ID
#'@param user_id user ID
#'@return character string of the username for that ID
#'@export
username_for_id <- function(user_id) {
  ID = user_id
  if (ID == 0) return("Unknown Name")
  else
    user_df_from_libreview %>% filter(user_id == ID)  %>%
    select(first_name,last_name) %>%
    as.character() %>%
    stringr::str_flatten(collapse = " ")

}

#' @title name of the person associated with a Libreview glucose file.
#' @description
#' Given a valid Libreview file, return a string of the form first_name last_name
#' @param filepath path to the CSV file
#' @return a space-separated character string made of first_name last_name
#' @export
name_from_libreview_file <- function(filepath) {
  first2 <- readLines(con=filepath,2)
  if (first2[1] %>% str_detect("Patient"))
  {name <- str_split(first2[2],pattern=",")[[1]][1]}
  else name <- str_split(first2[1],pattern=",")[[1]][5]
  return(str_squish(name))
}

#' @title user_id of a valid name string
#' @description
#' Assuming the name string is already in the user database, returns the user_id
#' @param name a string representation of the name you want to look up
#' @return user_id user ID from `user_df_from_libreview`
#' @export
user_id_for_name <- function(name) {
  name_split <- str_split(name, pattern = " ", simplify = TRUE)
  first <- first(name_split)
  last <- paste(name_split[-1], collapse=" ")
  ID <- user_df_from_libreview %>% filter(first_name == first &
                                       stringr::str_detect(last_name, last)) %>%
    pull(user_id)
  return(if(length(ID)>0) ID else NULL)
  # return(paste("your name",first_name,last_name))

}

