#generic
#' @export
get_param <- function(x) {
  UseMethod("get_param")
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
#' @importFrom dplyr count mutate filter arrange
#' @examples
#' irma(n = 100, p = 50, se = .8, sp = .7) %>% get_param()
get_param.irma <- function(dat){
  n_total <- sum(dat$n)

  p <- dat %>%
    count(disease, wt = n) %>%
    mutate(prop = n/sum(n)) %>%
    filter(disease == 1) %>%
    pull("prop")

  res  <- dat %>%
    group_by(disease) %>%
    mutate(prop = n/sum(n)) %>%
    filter(disease == test) %>% # sp, se
    arrange(disease) %>% # enforce order
    pull("prop")
  #c(sp, se) %<-% res
  sp <- res[1]; se <- res[2]  # get rid of zeallot dependency ?

  list(n = n_total, p = p, se = se, sp = sp)
}

