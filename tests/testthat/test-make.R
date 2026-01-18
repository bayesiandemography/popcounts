
## 'make_i_matches_sex' -------------------------------------------------------

test_that("'make_i_matches_sex' works with valid input", {
  fine <- c("Female", "Male", "Female", "Male")
  coarse <- c("Male", "Female", "Total")
  ans_obtained <- make_i_matches_sex(current = current,
                                      target = target)
  ans_expected <- list(3:4, integer(), 1L, 1L)
  expect_identical(ans_obtained, ans_expected)
})


## 'make_i_matches_sex' ------------------------------------------------------

test_that("'make_i_matches_time' works with valid input", {
  current <- c(2001, 2006, 2002, 2002)
  target <- c(2002, 2005, 2001, 2001)
  ans_obtained <- make_i_matches_time(current = current,
                                      target = target)
  ans_expected <- list(3:4, integer(), 1L, 1L)
  expect_identical(ans_obtained, ans_expected)
})
