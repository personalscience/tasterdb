# Test database creation

active_env <- Sys.getenv("R_CONFIG_ACTIVE")
Sys.setenv(R_CONFIG_ACTIVE = "localtest")

test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})



Sys.setenv(R_CONFIG_ACTIVE = active_env )
