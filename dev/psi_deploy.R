# deploy to shinyapps.io

# install.packages(c("tidyverse", "DBI", "config", "RPostgres", "shiny", "devtools"))
# install.packages(c("devtools", "roxygen2", "testthat", "knitr"))
# install.packages(c("bslib"))
# install.packages(c("firebase"))

rsconnect::setAccountInfo(name=config::get("shiny")$name,
                          token=config::get("shiny")$token,
                          secret=config::get("shiny")$secret)


remove.packages("psiCGM")
devtools::install_github("personalscience/psi-shiny-cgm",
                        ref = "dev",
                        upgrade = "never") #577dc4100cac3940") #,
                        # upgrade = "never")


remove.packages("cgmr")
devtools::install_github("personalscience/cgmr",
                         ref = "HEAD",
                         upgrade = "never")

rsconnect::deployApp(#appDir = file.path(getwd(),"R"),
                     appName = "Tastermonial",
                     appFiles = c("ui.R","server.R","global.R",
                                  "mod_goddessUI.R",
                                  "mod_CSV.R",
                                  "mod_foodTaster_compare.R",
                                  "www/psi_shiny.css",
                                  "config.yml",
                                  "firebase.rds"),
                     forceUpdate = TRUE
                     )



#devtools::load_all("~/dev/psi/psiCGM/")
