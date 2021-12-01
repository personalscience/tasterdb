

notes_2111 <- notes_df_from_taster2111(username = "Richard Sprague")

test_that("notes 2111 reads correctly", {
  expect_equal(notes_2111[10,4]$Comment, "CLIF CHOCOLATE CHIP")
})
