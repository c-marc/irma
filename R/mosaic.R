#' Get coordinates
#'
#' @param data A tibble with a column `n` of counts. Possibly a grouped tibble.
#' @param col A column to aggregate the counts on. As a quosure (tidy eval).
#'  Cumulative statistic will obey the class: character or factor.
#' @param prefix A prefix to add to resulting columns.
#'
#' @return A tibble with the `col` and prefixed `p` (proba), `cmin` and `cmax` (min and max built from the cumulative dist)
#' @export
#'
#' @examples
#' data <- tidyr::expand_grid(x=1:3, y=1:3) %>% dplyr::mutate(n=stats::rpois(n(),10))
#' data %>% get_proba(x)
#'
#' # prefixed
#' data %>% get_proba(x, "x")
#'
#' # conditional on x
#' data %>% group_by(x) %>% get_proba(y, "y")
get_coordinates <- function(data, col, prefix = ""){
  if(prefix != "") prefix <- paste0(prefix,"_")

  data %>%
    dplyr::count({{col}}, wt = n) %>%
    # arrange for cumsum() to be predictable
    dplyr::arrange({{col}}) %>%
    dplyr::mutate(
      p = n/sum(n),
      cmax = cumsum(p),
      cmin = cmax - p
    ) %>%
    # drop n to avoid future collision with original n (we aim at joining with original data)
    dplyr::select({{col}}, p, cmin, cmax) %>%
    dplyr::rename_with(~paste0(prefix,.x), c(p, cmin, cmax))
}


#' Get all required coordinates
#'
#' @param data A tibble with a column `n` of counts. Possibly a grouped tibble.
#' @param x What will be on x
#' @param y What will be on y
#'
#' @return A tibble with all coordinates and prefixes that plot will understand
#' @export
#'
#' @examples
get_all_coordinates <- function(data, x, y){
  data %>%
    left_join(data %>% get_coordinates({{x}}, "x")) %>%
    left_join(data %>% get_coordinates({{y}}, "y")) %>%
    left_join(data %>% group_by({{x}}) %>% get_coordinates({{y}}, "yc")) %>%
    left_join(data %>% group_by({{y}}) %>% get_coordinates({{x}}, "xc"))
}


ggmosaic_v <- function(data) {
  p <- ggplot(data) +
    geom_rect(
      aes(
        xmin = x_cmin,
          ymin = yc_cmin,
          xmax = x_cmax,
          ymax = yc_cmax,
          fill = as.factor(y)
        ),
      colour = "white"
    )

  #second marginal probablities
  p <- p +
    geom_hline(yintercept = unique(res$y_cmin[res$y_cmin > 0]), col = "grey20")

  #annotate
  p <- p +
    geom_text(
      aes(
      x = (x_cmin + x_cmax) / 2,
      y = (yc_cmin + yc_cmax) / 2,
      label = paste0(round(100 * yc_p, 0), "%")
    ),col = "white") +
    geom_text(aes(
      x = x_cmin,
      y = yc_cmax,
      label = paste0(round(100 * x_p * yc_p, 0), "%")
    ), col = "white", size = 3, hjust = -0.2, vjust = 1.2
    )

  # custom scales
  p <- p +
    scale_x_continuous(NULL,
                       breaks = (res$x_cmin + res$x_cmax) / 2,
                       labels = paste0(round(100 * res$x_p, 0), "%")
    ) +
    scale_y_continuous(NULL,
                       breaks = (res$y_cmin + res$y_cmax) / 2,
                       labels = paste0(round(100 * res$y_p, 0), "%")
    )

  #prettify
  p <- p +
    scale_fill_brewer(palette = "Set1") +
    # theme(legend.position = "none")
    coord_equal(expand = F) +
    guides(fill = guide_legend(title = "Y", reverse = TRUE)) +
    theme(axis.ticks = element_blank())

  p
}
