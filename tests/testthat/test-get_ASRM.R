test_that("throwing error due to empty input", {
  expect_error(get_asrm(data.frame(matrix(ncol=0, nrow=0))))
})

test_that("throwing error due to absent essential cols", {
  expect_error(get_asrm(data.frame(PIN = "a", item = 0)))
})

test_that("throwing error due to empty entry in pin col", {
  expect_error(get_asrm(data.frame(PIN = c("a",""), item = c(1,2), complete = c("y", "n"), response = c("a", "b"))))
})

test_that("throwing error due to NA in item col", {
  expect_error(get_asrm(data.frame(PIN = c("a","b"), item = c(NA,2), complete = c("y", "n"), response = c("a", "b"))))
})

test_that("throwing error due to NA in response col", {
  expect_error(get_asrm(data.frame(PIN = c("a","b"), item = c(NA,2), complete = c("y", "n"), response = c("a", NA))))
})

test_that("throwing error due to range constraints in repsonses", {
  expect_error(get_asrm(data.frame(PIN = c("a","b"), item = c(1,2), complete = c("y", "n"), response = c(0, 5))))
})

test_that("throwing error due to range constraints in items", {
  expect_error(get_asrm(data.frame(PIN = c("a","b"), item = c(1,6), complete = c("y", "n"), response = c(0, 4))))
})

test_that("throwing warning due to factor in response", {
  expect_warning(get_asrm(data.frame(PIN = c("a","b"), item = c(1,5), complete = c("y", "n"), response = as.factor(c(0, 4)))))
})

test_that("throwing warning due to factor in item", {
  expect_warning(get_asrm(data.frame(PIN = c("a","b"), item = as.factor(c(1,5)), complete = c("y", "n"), response = c(0, 4))))
})

test_that("output", {
  expect_identical(get_asrm(data.frame(PIN = c(rep("00001", 5), rep("00002", 5)), item = rep(1:5,2), complete = rep("y", 10), response = c(rep(0,5),2,3,1,3,2 ) )),
                   data.frame(PIN=c("00001", "00002"), asrm_sum = c(0, 11), asrm_cat = c(0,1), stringsAsFactors = F))
})
