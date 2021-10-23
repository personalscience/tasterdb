

extra_user_df <- readr::read_csv(file = file.path(config::get("tastermonial")$datadir, "Tastermonial_Extra_Users.csv"),
                                                    col_types = "cccccd") %>% dplyr::mutate(birthdate = lubridate::mdy(birthdate))

#' @title Users known to Libreview Practice Portal
#' @description
#' A dataframe of all users and their ids, taken from the Libreview practice portal
user_df_from_libreview <-
  user_df_from_csv() %>% mutate(user_id = row_number() + 1000) %>%
  dplyr::anti_join(extra_user_df,
                   by = c("first_name", "last_name")) %>% bind_rows(extra_user_df)
usethis::use_data(user_df_from_libreview, overwrite = TRUE)
