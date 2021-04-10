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
#' @importFrom ggplot2 autoplot ggplot
#'
#' @examples
autoplot.irma_data <- function(dat, fill_by,
                               size = 1.125, pal = c("bisque4", "firebrick"),
                               title = "Population",
                               subtitle = "4 groups",
                               ...) {
  # general coding memo : ggplot can overwrite themes and labs, so maybe keep the ... minimal

  # nice but complexity is dangerous !
  # maybe flip = F and just manually assign both quosures
  if (enquo(fill_by) %>% rlang::get_expr() == sym("disease")) {
    wrap_by <- quo(test)
  } else {
    wrap_by <- quo(disease)
  }

  p <- dat %>%
    arrange(desc({{ fill_by }})) %>%
    ggplot(aes(fill = {{ fill_by }}, values = n)) +
    waffle::geom_waffle(n_rows = 5, size = size, colour = "white", flip = TRUE) +
    facet_wrap(facets = {{ wrap_by }}, nrow = 1, strip.position = "bottom")

  # deal with scales
  p <- p +
    coord_equal() +
    # coord_equal(expand = FALSE) +
    scale_fill_manual(
      name = NULL,
      values = pal
    ) +
    scale_x_discrete() +
    scale_y_continuous( #+/-.5 to trick ticks where they belong
      labels = function(x) (x - .5) * 5, # * n_rows
      breaks = c(0, 25, 50, 75, 100) / 5 + .5 # / n_rows
    )

  # and theme
  p <- p +
    theme_minimal(base_family = "Roboto Condensed") +
    theme(
      panel.grid = element_blank(),
      axis.ticks.y = element_line()
    ) +
    labs(
      title = title,
      subtitle = subtitle,
      y = paste("Out of", sum(dat$n)),
      ...
    )

  p
}


#' @importFrom graphics plot
plot.irma_data <- function(x, ...) {
  print(autoplot(x, ...))
}
