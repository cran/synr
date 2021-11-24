context("Consistency scoring")
library(synr)

test_that("Participant date is set and retrieved correctly", {
  p <- Participant$new(id="1")
  p$set_date("2020-10-27")
  expect_equal(p$test_date, as.Date("2020-10-27"))
})
