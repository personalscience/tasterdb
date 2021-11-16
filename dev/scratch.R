
# https://cynkra.github.io/dm/articles/dm.html

#Sys.setenv(R_CONFIG_ACTIVE = "local")
#Sys.setenv(R_CONFIG_ACTIVE = "sandbox")
Sys.setenv(R_CONFIG_ACTIVE = "shinyapps")
Sys.setenv(R_CONFIG_ACTIVE = "sqldb")

config::get()$dataconnection$dbname

conn_args <- config::get("dataconnection")
con <- DBI::dbConnect(
  drv = conn_args$driver,
  user = conn_args$user,
  host = conn_args$host,
  port = conn_args$port,
  dbname = conn_args$dbname,
  password = conn_args$password
)

conn_args

local_db <- taster_db(db_config = "default")
local_db$notes_records %>% count()

sqldb <- taster_db(db_config = "sqldb")

tbl(sqldb$con, "glucose_records") %>% count()
tbl(local_db$con, "glucose_records") %>% count()

cgmr::food_times_df(sqldb$table_df("glucose_records"),
                    sqldb$notes_records_df(),
                    foodname = "Clif Bar Chocolate")

sqldb$notes_records_df()

GLUCOSE_RECORDS <- tbl(con, "glucose_records") %>% collect()
NOTES_RECORDS <- tbl(con, "notes_records") %>% collect()

sdb <- load_db(ps_database = "sqldb")
pdb <- load_db(ps_database = "shinyapps")


 ldb <- load_db(ps_database = "local")
# sdb <- load_db("shinyapps")

 ldb <- taster_db(db_config = "local")
 ldb$notes_records %>% filter(user_id == 1234) %>% count()


r_glucose <- cgmr::glucose_df_from_libreview_csv(file.path(config::get()$tastermonial$datadir,
                                                  "RichardSprague_glucose_9-13-2021.csv")) %>%
   lubridate::force_tz(time, tz= "America/Los_Angeles" )




r_notes <- r_glucose %>% cgmr::notes_df_from_glucose_table() %>% filter(Start > "2021-06-01")
r_notes$Comment <- map_chr(stringr::str_to_upper(r_notes$Comment), taster_classify_food)



DBI::dbWriteTable(pdb$con,
                  name = "notes_records",
                  value = r_notes,
                  row.names = FALSE,
                  append = TRUE)

tbl(ldb$con, "notes_records") %>% filter(user_id == 1234) %>% pull(Start) %>% head()
