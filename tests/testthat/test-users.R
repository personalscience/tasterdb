
users <- user_df_from_libreview()

test_that("username lookup from database", {
  expect_equal(user_id_for_name("Richard Sp"), 1234)
  expect_equal(name_for_user_id(1234), "Richard Sprague")
  expect_equal(id_from_initial("Ri"), 1234)
})

test_that("username lookup from local Libreview CSV file", {
  expect_equal(nrow(filter(users, user_id == 1234)),1)
  expect_equal(as.character(filter(users, user_id == 1234)[,"first_name"]), "Richard")
})
