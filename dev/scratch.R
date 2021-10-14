
Sys.setenv(R_CONFIG_ACTIVE = "local")
Sys.setenv(R_CONFIG_ACTIVE = "sandbox")

config::get()

conn_args <- config::get("dataconnection")
con <- DBI::dbConnect(
  drv = conn_args$driver,
  user = conn_args$user,
  host = conn_args$host,
  port = conn_args$port,
  dbname = conn_args$dbname,
  password = conn_args$password
)


db <- taster_db("sandbox")
dm::check_key(db$notes_records_df())

my_dm <- dm::dm_from_src(con)
my_dm
names(my_dm)
my_dm$notes_records
dplyr::count(my_dm$user_list)
db
db$notes_records_df()
db$table_df("notes_records")
db$con
config::get("default")
