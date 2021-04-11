#s3

#constructor
new_irma <- function(x = tibble::tibble()) {
  stopifnot(tibble::is_tibble(x))
  structure(
    x,
    class = c("irma", class(x))
  )
}

#validator
validate_irma <- function(x) {
  dat <- unclass(x)

  if (!all(!is.na(dat$n) & dat$n >= 0)) {
    stop(
      "All `n` must be non-missing and greater than zero",
      call. = FALSE
    )
  }

  x
}

# Is irma
#' @export
is_irma <- function(x) inherits(x, "irma")


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
#' irma(n = 100, p = 50, se = .8, sp = .7)
irma <- function(n = numeric(), p = numeric(), se = numeric(), sp = numeric()){
  stopifnot(is.numeric(n) & n > 0)

  dat <- tibble::tribble(
    ~disease, ~test, ~n,
    0,0, n*(1-p)*sp,
    0,1, n*(1-p)*(1-sp),
    1,0, n*p*(1-se),
    1,1, n*p*se
  ) %>%
    dplyr::mutate(n = round(n, 0))

  validate_irma(new_irma(dat))
}

