context("ParticipantGroup misc. functionality")
library(synr)

test_that("has_participants() returns TRUE if there is one participant with an id, added with add_participant", {
  p <- Participant$new(id="1")
  pg <- ParticipantGroup$new()
  pg$add_participant(p)
  expect_true(pg$has_participants())
})


test_that("add_participant() throws error if trying to add participant with no id", {
  p <- Participant$new()
  pg <- ParticipantGroup$new()
  expect_error(pg$add_participant(p))
})


test_that("has_participants() returns FALSE if there are no participants in the group", {
  pg <- ParticipantGroup$new()
  expect_false(pg$has_participants())
})


test_that("has_participants() returns TRUE if there is one participant with an id, added as a list
          with add_participants", {
  p <- Participant$new(id="1")
  pg <- ParticipantGroup$new()
  pg$add_participants(list(p))
  expect_true(pg$has_participants())
})


test_that("get_ids() returns correct ids for two participants", {
  p1 <- Participant$new(id="1")
  p2 <- Participant$new(id="2")
  pg <- ParticipantGroup$new()
  pg$add_participants(list(p1, p2))
  out_ids <- pg$get_ids()
  expect_equal(out_ids[1], '1')
  expect_equal(out_ids[2], '2')
})
