

testfun_make_popn_true <- function(age_open = 70) {
  age <- agetime::age_labels_five(lower_last = age_open)
  sex <- c("Female", "Male")
  time <- c(2000, 2005)
  popn <- poputils::west_lifetab |>
    dplyr::filter(level %in% 5:6) |>
    dplyr::select(age, sex, level, Lx) |>
    dplyr::mutate(age = agetime::age_to_five(age, lower_last = age_open)) |>
    dplyr::count(level, sex, age, level, wt = Lx) |>
    dplyr::mutate(popn = 1000 * n,
                  popn = round(popn)) |>
    dplyr::pull(popn)
  expand.grid(age = age,
              sex = sex,
              time = time,
              KEEP.OUT.ATTRS = FALSE,
              stringsAsFactors = FALSE) |>
    dplyr::mutate(popn = popn)
}

testfun_make_popn_obs <- function(popn_true, prob = 0.9) {
  popn_true |>
    dplyr::mutate(age = agetime::age_to_ten(age)) |>
    dplyr::count(age, sex, time, wt = popn, name = "popn") |>
    dplyr::mutate(popn = rbinom(n = dplyr::n(),
                                size = popn,
                                prob = prob))
}
  



  
