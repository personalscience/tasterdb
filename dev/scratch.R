
# https://cynkra.github.io/dm/articles/dm.html

#Sys.setenv(R_CONFIG_ACTIVE = "local")
#Sys.setenv(R_CONFIG_ACTIVE = "sandbox")
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


taster_db(db_config="sqldb")

make_new_database_if_necessary(db_name = "mydb", drop=TRUE)


new_db_sql <-
  sprintf(
    "CREATE DATABASE %s WITH OWNER = %s ENCODING = 'UTF8' CONNECTION LIMIT = -1;",
    conn_args$dbname,
    "postgres"
  )

config::get()


# ldb <- load_db("local")
# sdb <- load_db("shinyapps")


# clif_start <- ldb$table_df("notes_records") %>% filter(user_id == 1003) %>% filter(str_detect(Comment,"Clif")) %>% pull(Start) %>% last()
#
# ldb$glucose_records %>% filter(user_id == 1003) %>% filter(time >= clif_start) %>% collect() %>% filter(time <= (clif_start + lubridate::minutes(120)))
#
# ldb$notes_records_df() %>% filter(user_id == 1501) %>% arrange(Start) %>% print(n=Inf) %>% pull(Start)
# ln <- run_taster_notes()
# ln %>% filter(user_id == 1501) %>% arrange(Start) %>% print(n=Inf) %>% pull(Start)
#
# lshiny <- load_db("shinyapps")

# old <- taster_old_typeform()
# old %>% filter(user_id == 1502) %>% arrange(Comment) %>% print(n= Inf)
# exceptions <- read_csv(file.path(config::get("tastermonial")$datadir,"Tastermonial_Exceptions.csv")) %>% mutate(fullname=paste(first_name, last_name))
#
# old$username <- old$user_id %>% map_chr(function(x) {name_for_user_id(x)}) #%>% group_by(user_id) %>% distinct(username)
#
# old %>% filter(username %in% exceptions$fullname) %>% mutate(tz = exceptions$timezone)
#
# old %>% group_by(user_id) %>% distinct(username)
#
# old$Start
# notes <- run_taster_notes()

# db <- taster_db("sandbox")
# db$table_df("user_list")
# write_user_list_from_scratch(con=db$con)
# db$table_df("user_list")
# fill_glucose_records_from_scratch(con = db$con)
# db$table_df("glucose_records")
# fill_taster_notes_from_scratch(con = db$con, taster_notes_df = run_taster_notes())
# db$table_df("notes_records") %>% pull(user_id) %>% unique() %>% sort()
#
# DBI::dbWriteTable(db$con, "glucose_records", load_nutrisense_csv_from_directory(), append = TRUE)
# db$table_df("glucose_records") %>% distinct(user_id) %>% pull(user_id) %>% sort()
# dbDisconnect(db$con)

#
# path = config::get("tastermonial")$datadir
# datafiles <- list.files(path)
# datafiles <- datafiles[datafiles %>% str_detect("nutrisense")]
# cgmr::nutrisense_results(file.path(path,datafiles[3]))
#
# load_nutrisense_csv_from_directory()
