#' Make data from parameters
#'
#' @param n A numeric: Total number of people
#' @param p A numeric: Proportion of disease (prevalence)
#' @param se A numeric: Sensitivity of the test
#' @param sp A numeric: Specificity of the test
#'
#' @return A tibble: Number of people in each of the 4 combinations of disease and test (contingency table in a tidy format). `n` is rounded.
#' @export
#'
#' @examples
#' make_data(n = 100, p = 50, se = .8, sp = .7)
make_data <- function(n, p, se, sp){
  dat <- tibble::tribble(
    ~disease, ~test, ~n_e,
    0,0, n*(1-p)*sp,
    0,1, n*(1-p)*(1-sp),
    1,0, n*p*(1-se),
    1,1, n*p*se
  ) %>%
    dplyr::mutate(n = round(n_e))

  #class(dat) <- c(class(dat), "irma_data") #quick and dirty
  dat
}



#' Get Parameters From Data
#' This checks the generated data and compute resulting parameters which are changed from rounding.
#' @param dat A tibble: a tibble with `disease`, `test`, `n`
#'
#' @return A list:
#' - `n`: A numeric: total population
#' - `p`: A numeric: prevalence
#' - `se`: A numeric: sensitivity
#' - `sp`: A numeric: specificity
#' @export
#'
#' @examples
#' make_data(n = 100, p = 50, se = .8, sp = .7) %>% get_param_from_data()
get_param_from_data <- function(dat){
  n_total <- sum(dat$n)
  p <- dat %>%
    count(disease, wt = n) %>%
    mutate(prop = n/sum(n)) %>%
    filter(disease == 1) %>%
    pluck("prop")
  res  <- dat %>%
    group_by(disease) %>%
    mutate(prop = n/sum(n)) %>%
    filter(disease == test) %>%
    pluck("prop")

  #c(sp, se) %<-% res
  sp <- res[1]; se <- res[2]

  list(n = n_total, p = p, se = se, sp = sp)
}
