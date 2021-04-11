#' Autoplot method for dat
#'
#' @param dat A tibble
#' @param fill_by A quote: The column for fill aesthetics
#' @param size A value: The waffle *grid* size
#' @param pal A character vector: The 2 colours
#' @param title
#' @param subtitle
#' @param ...
#'
#' @return A ggplot()
#' @export
#' @importFrom ggplot2 autoplot
#'
#' @examples
autoplot.irma <- function(dat,
                          fill_by,
                          wrap = T,
                          n_row = 5,
                          size = 5 / n_row, # default trick
                          pal = c("bisque4", "firebrick"),
                          title = "Population",
                          ...) {
  # general coding memo : ggplot can overwrite themes and labs, so maybe keep the ... minimal

  # nice but complexity is dangerous !
  # maybe flip = F and just manually assign both quosures
  if (rlang::enquo(fill_by) %>% rlang::get_expr() == rlang::sym("disease")) {
    wrap_by <- rlang::quo(test)
  } else {
    wrap_by <- rlang::quo(disease)
  }

  p <- dat %>%
    dplyr::arrange(desc({{ fill_by }})) %>%
    ggplot2::ggplot(ggplot2::aes(fill = {{ fill_by }}, values = n)) +
    waffle::geom_waffle(n_rows = n_row, size = size, colour = "white", flip = TRUE)

  if(wrap){
    p <- p +
      ggplot2::facet_wrap(facets = {{ wrap_by }}, nrow = 1, strip.position = "bottom")
  }

  # deal with scales
  p <- p +
    ggplot2::coord_equal() +
    # coord_equal(expand = FALSE) +
    ggplot2::scale_fill_manual(
      name = NULL,
      values = pal
    ) +
    ggplot2::scale_x_discrete() +
    ggplot2::scale_y_continuous( #+/-.5 to trick ticks where they belong
      labels = function(x) (x - .5) * n_row, # * n_rows
      breaks = seq(0, sum(dat$n), by = n_row*5) / n_row + .5 # / n_rows (every 5 rows)
    )

  # and theme
  p <- p +
    ggplot2::theme_minimal(base_family = "Roboto Condensed") +
    ggplot2::theme(
      panel.grid = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_line()
    ) +
    ggplot2::labs(
      title = title,
      subtitle = glue::glue("Total population is {sum(dat$n)}"),
      ...
    )

  p
}


#' @export
#' @importFrom graphics plot
plot.irma <- function(x, ...) {
  print(autoplot(x, ...))
}
