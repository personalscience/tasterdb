
users <- user_df_from_libreview()

test_that("username lookup", {
  expect_equal(user_id_for_name("Richard Sp"), 1234)
})
