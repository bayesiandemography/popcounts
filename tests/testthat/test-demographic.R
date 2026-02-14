
## 'aggregate_Lx_ages_lt' -------------------------------------------------

test_that("'aggregate_Lx_ages_lt' works with valid inputs - aggregating child and open", {
  Lx <- c(0.95, 2.9, 3.5, 2.75, 1.4, 0.4,
          0.93, 2.8, 3.4, 2.5, 1.3, 0.5)
  age_open <- 15L
  ans_obtained <- aggregate_Lx_ages_lt(Lx = Lx,
                                       age_open = age_open)
  ans_expected <- c(0.95 + 2.9, 3.5, 2.75, 1.4 + 0.4,
                    0.93 + 2.8, 3.4, 2.5, 1.3 + 0.5)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'aggregate_Lx_ages_lt' works with valid inputs - not aggregating", {
  Lx <- c(0.95, 2.9, 3.5, 2.75, 1.4, 0.4,
          0.93, 2.8, 3.4, 2.5, 1.3, 0.5)
  age_open <- 20L
  ans_obtained <- aggregate_Lx_ages_lt(Lx = Lx,
                                       age_open = age_open)
  ans_expected <- Lx[c(1, 3:6, 7, 9:12)]
  ans_expected[c(1, 6)] <- ans_expected[c(1, 6)] + Lx[c(2, 8)]
  expect_identical(ans_obtained, ans_expected)
})


## 'aggregate_Lx_ages_single' -------------------------------------------------

test_that("'aggregate_Lx_ages_single' works with valid inputs - aggregating", {
  Lx <- c(0.95, 0.9, 0.8, 0.75, 0.4,
          0.93, 0.8, 0.7, 0.5, 0.3)
  age_open <- 3L
  ans_obtained <- aggregate_Lx_ages_single(Lx = Lx,
                                           age_open = age_open)
  ans_expected <- c(0.95, 0.9, 0.8, 0.75 + 0.4,
                    0.93, 0.8, 0.7, 0.5 + 0.3)
  expect_identical(ans_obtained, ans_expected)
})

test_that("'aggregate_Lx_ages_single' works with valid inputs - not aggregating", {
  Lx <- c(0.95, 0.9, 0.8, 0.75, 0.4,
          0.93, 0.8, 0.7, 0.5, 0.3)
  age_open <- 4L
  ans_obtained <- aggregate_Lx_ages_single(Lx = Lx,
                                           age_open = age_open)
  ans_expected <- Lx
  expect_identical(ans_obtained, ans_expected)
})


## 'alpha_to_e0_lt' -----------------------------------------------------------

test_that("'alpha_to_e0_lt' works with valid inputs - numeric", {
  alpha <- 0.5
  lx_std <- c(1, 0.9, 0.8, 0.75, 0.4,
              1, 0.8, 0.7, 0.5, 0.3)
  pr_fem <- 0.49
  ans <- alpha_to_e0_lt(alpha = alpha,
                        lx_std = lx_std,
                        pr_fem = pr_fem)
  expect_true(is.numeric(ans))
})

test_that("'alpha_to_e0_lt' works with valid inputs - rvec", {
  set.seed(0)
  alpha <- rvec::rnorm_rvec(n = 1, n_draw = 10)
  lx_std <- c(1, 0.9, 0.8, 0.75, 0.4,
              1, 0.8, 0.7, 0.5, 0.3)
  pr_fem <- 0.49
  ans <- alpha_to_e0_lt(alpha = alpha,
                        lx_std = lx_std,
                        pr_fem = pr_fem)
  expect_true(rvec::is_rvec(ans))
})


## 'alpha_to_e0_single' -------------------------------------------------------

test_that("'alpha_to_e0_single' works with valid inputs - numeric", {
  alpha <- 0.5
  lx_std <- c(1, 0.9, 0.8, 0.75, 0.4,
              1, 0.8, 0.7, 0.5, 0.3)
  pr_fem <- 0.49
  ans <- alpha_to_e0_single(alpha = alpha,
                            lx_std = lx_std,
                            pr_fem = pr_fem)
  expect_true(is.numeric(ans))
})

test_that("'alpha_to_e0_single' works with valid inputs - rvec", {
  set.seed(0)
  alpha <- rvec::rnorm_rvec(n = 1, n_draw = 10)
  lx_std <- c(1, 0.9, 0.8, 0.75, 0.4,
              1, 0.8, 0.7, 0.5, 0.3)
  pr_fem <- 0.49
  ans <- alpha_to_e0_single(alpha = alpha,
                            lx_std = lx_std,
                            pr_fem = pr_fem)
  expect_true(rvec::is_rvec(ans))
})


## 'alpha_to_lx' --------------------------------------------------------------

test_that("'alpha_to_lx' works with valid inputs", {
  alpha <- 0.5
  lx_std <- c(1, 0.9, 0.8, 0.75, 0.4,
              1, 0.8, 0.7, 0.5, 0.3)
  ans <- alpha_to_lx(alpha = alpha,
                     lx_std = lx_std)
  expect_true(is.numeric(ans))
  expect_true(all(ans >= 0))
  expect_true(all(ans <= 1))
})


## 'e0_to_alpha_lt' -----------------------------------------------------------

test_that("'e0_to_alpha_lt' works with numeric", {
  e0 <- 28.3
  lx_std <- lx_west_7_abridged$lx
  pr_fem <- 0.49
  ans <- e0_to_alpha_lt(e0 = e0,
                        lx_std = lx_std,
                        pr_fem = pr_fem)
  e0_implied <- alpha_to_e0_lt(ans,
                               lx_std = lx_std,
                               pr_fem = pr_fem)
  expect_equal(e0, e0_implied, tolerance = 0.001)
})

test_that("'e0_to_alpha_lt' works with rvec", {
  e0 <- rvec::rvec(matrix(c(28.3, 27.2, 30.1), nr = 1))
  lx_std <- lx_west_7_abridged$lx
  pr_fem <- 0.49
  ans <- e0_to_alpha_lt(e0 = e0,
                        lx_std = lx_std,
                        pr_fem = pr_fem)
  e0_implied <- c(alpha_to_e0_lt(as.numeric(ans)[1],
                                 lx_std = lx_std,
                                 pr_fem = pr_fem),
                  alpha_to_e0_lt(as.numeric(ans)[2],
                                 lx_std = lx_std,
                                 pr_fem = pr_fem),
                  alpha_to_e0_lt(as.numeric(ans)[3],
                                 lx_std = lx_std,
                                 pr_fem = pr_fem))
  e0_implied <- rvec::rvec(matrix(e0_implied, nr = 1))
  expect_equal(e0, e0_implied, tolerance = 0.001)
})


## 'e0_to_alpha_single' -------------------------------------------------------

test_that("'e0_to_alpha_single' works with numeric", {
  e0 <- 28.3
  lx_std <- lx_west_7_complete$lx
  pr_fem <- 0.49
  ans <- e0_to_alpha_single(e0 = e0,
                            lx_std = lx_std,
                            pr_fem = pr_fem)
  e0_implied <- alpha_to_e0_single(ans,
                                   lx_std = lx_std,
                                   pr_fem = pr_fem)
  expect_equal(e0, e0_implied, tolerance = 0.001)
})

test_that("'e0_to_alpha_single' works with rvec", {
  e0 <- rvec::rvec(matrix(c(28.3, 27.2, 30.1), nr = 1))
  lx_std <- lx_west_7_complete$lx
  pr_fem <- 0.49
  ans <- e0_to_alpha_single(e0 = e0,
                            lx_std = lx_std,
                            pr_fem = pr_fem)
  e0_implied <- c(alpha_to_e0_single(as.numeric(ans)[1],
                                     lx_std = lx_std,
                                     pr_fem = pr_fem),
                  alpha_to_e0_single(as.numeric(ans)[2],
                                     lx_std = lx_std,
                                     pr_fem = pr_fem),
                  alpha_to_e0_single(as.numeric(ans)[3],
                                     lx_std = lx_std,
                                     pr_fem = pr_fem))
  e0_implied <- rvec::rvec(matrix(e0_implied, nr = 1))
  expect_equal(e0, e0_implied, tolerance = 0.001)
})


## 'e0_to_alpha_lt_inner' -----------------------------------------------------

test_that("'alpha_to_alpha_lt_inner' works", {
  e0 <- 28.3
  lx_std <- lx_west_7_abridged$lx
  time_step <- 5
  pr_fem <- 0.49
  ans <- e0_to_alpha_lt_inner(e0 = e0,
                              lx_std = lx_std,
                              pr_fem = pr_fem)
  e0_implied <- alpha_to_e0_lt(ans,
                               lx_std = lx_std,
                               pr_fem = pr_fem)
  expect_equal(e0, e0_implied, tolerance = 0.001)
})


## 'e0_to_alpha_single_inner' -------------------------------------------------

test_that("'alpha_to_alpha_single_inner' works", {
  e0 <- 28.3
  lx_std <- lx_west_7_complete$lx
  time_step <- 5
  pr_fem <- 0.49
  ans <- e0_to_alpha_single_inner(e0 = e0,
                                  lx_std = lx_std,
                                  pr_fem = pr_fem)
  e0_implied <- alpha_to_e0_single(ans,
                                   lx_std = lx_std,
                                   pr_fem = pr_fem)
  expect_equal(e0, e0_implied, tolerance = 0.001)
})


## 'lx_to_Lx_lt' --------------------------------------------------------------

test_that("'lx_to_Lx_lt' works with numeric", {
  lx <- c(1, 0.9, 0.8, 0.75, 0.4,
          1, 0.8, 0.7, 0.5, 0.3)
  ans_obtained <- lx_to_Lx_lt(lx)
  ans_expected <- c(0.9 * 1 + (1 - 0.9) * 0.3141,
                    0.8 * 4 + (0.9 - 0.8) * 2,
                    0.75 * 5 + (0.8 - 0.75) * 2.5,
                    0.4 * 5 + (0.75 - 0.4) * 2.5,
                    0.4 / ((0.75 - 0.4) / (0.4 * 5 + (0.75 - 0.4) * 2.5)),
                    0.8 * 1 + (1 - 0.8) * 0.2991,
                    0.7 * 4 + (0.8 - 0.7) * 2,
                    0.5 * 5 + (0.7 - 0.5) * 2.5,
                    0.3 * 5 + (0.5 - 0.3) * 2.5,
                    0.3 / ((0.5 - 0.3) / (0.3 * 5 + (0.5 - 0.3) * 2.5)))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'lx_to_Lx_lt' works with rvec", {
  lx <- c(1, 0.9, 0.8, 0.75, 0.4,
          1, 0.8, 0.7, 0.5, 0.3)
  lx <- rvec::rvec_dbl(lx)
  ans_obtained <- lx_to_Lx_lt(lx)
  ans_expected <- c(0.9 * 1 + (1 - 0.9) * 0.3141,
                    0.8 * 4 + (0.9 - 0.8) * 2,
                    0.75 * 5 + (0.8 - 0.75) * 2.5,
                    0.4 * 5 + (0.75 - 0.4) * 2.5,
                    0.4 / ((0.75 - 0.4) / (0.4 * 5 + (0.75 - 0.4) * 2.5)),
                    0.8 * 1 + (1 - 0.8) * 0.2991,
                    0.7 * 4 + (0.8 - 0.7) * 2,
                    0.5 * 5 + (0.7 - 0.5) * 2.5,
                    0.3 * 5 + (0.5 - 0.3) * 2.5,
                    0.3 / ((0.5 - 0.3) / (0.3 * 5 + (0.5 - 0.3) * 2.5)))
  ans_expected <- rvec::rvec_dbl(ans_expected)
  expect_equal(ans_obtained, ans_expected)
})


## 'lx_to_Lx_single' ----------------------------------------------------------

test_that("'lx_to_Lx_single' works with numeric", {
  lx <- c(1, 0.9, 0.8, 0.75, 0.4,
          1, 0.8, 0.7, 0.5, 0.3)
  ans_obtained <- lx_to_Lx_single(lx)
  ans_expected <- c(0.9 * 1 + (1 - 0.9) * 0.3141,
                    0.8 * 1 + (0.9 - 0.8) * 0.5,
                    0.75 * 1 + (0.8 - 0.75) * 0.5,
                    0.4 * 1 + (0.75 - 0.4) * 0.5,
                    0.4 / ((0.75 - 0.4) / (0.4 * 1 + (0.75 - 0.4) * 0.5)),
                    0.8 * 1 + (1 - 0.8) * 0.2991,
                    0.7 * 1 + (0.8 - 0.7) * 0.5,
                    0.5 * 1 + (0.7 - 0.5) * 0.5,
                    0.3 * 1 + (0.5 - 0.3) * 0.5,
                    0.3 / ((0.5 - 0.3) / (0.3 * 1 + (0.5 - 0.3) * 0.5)))
  expect_equal(ans_obtained, ans_expected)
})

test_that("'lx_to_Lx_single' works with rvec", {
  lx <- c(1, 0.9, 0.8, 0.75, 0.4,
          1, 0.8, 0.7, 0.5, 0.3)
  lx <- rvec::rvec_dbl(lx)
  ans_obtained <- lx_to_Lx_single(lx)
  ans_expected <- c(0.9 * 1 + (1 - 0.9) * 0.3141,
                    0.8 * 1 + (0.9 - 0.8) * 0.5,
                    0.75 * 1 + (0.8 - 0.75) * 0.5,
                    0.4 * 1 + (0.75 - 0.4) * 0.5,
                    0.4 / ((0.75 - 0.4) / (0.4 * 1 + (0.75 - 0.4) * 0.5)),
                    0.8 * 1 + (1 - 0.8) * 0.2991,
                    0.7 * 1 + (0.8 - 0.7) * 0.5,
                    0.5 * 1 + (0.7 - 0.5) * 0.5,
                    0.3 * 1 + (0.5 - 0.3) * 0.5,
                    0.3 / ((0.5 - 0.3) / (0.3 * 1 + (0.5 - 0.3) * 0.5)))
  ans_expected <- rvec::rvec_dbl(ans_expected)
  expect_equal(ans_obtained, ans_expected)
})


## 'Lx_to_e0' -----------------------------------------------------------------

test_that("'Lx_to_e0' works with valid inputs", {
  Lx <- poputils::west_lifetab |>
    subset(level == 3, select = "Lx") |>
    unlist()
  ans_obtained <- Lx_to_e0(Lx = Lx, pr_fem = 0.49)
  ans_expected <- poputils::west_lifetab |>
    subset(level == 3 & age == 0, select = "ex") |>
    unlist()
  ans_expected <- 0.49 * ans_expected[[1]] + 0.51 * ans_expected[[2]]
  expect_equal(ans_obtained, ans_expected)
})


## 'tfr_to_asfr' --------------------------------------------------------------

test_that("'tfr_to_asfr' works with valid inputs", {
  asfr <- c(1, 0.9, 0.8, 0.75, 0.4)
  tfr <- 5 * sum(asfr)
  asfr_std <- data.frame(age = 11:15,
                         value = 3 * proportions(asfr))
  ans_obtained <- tfr_to_asfr(tfr = tfr,
                              asfr_std = asfr_std,
                              time_step = 5)
  ans_expected <- asfr
  expect_equal(ans_obtained, ans_expected)
})
