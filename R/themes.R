#' Custom ggplot2 theme
#'
#' Custom these for ggplot, building on [cowplot::theme_minimal_grid()].
#'
#' @param font_family Font family (requires [Cairo::Cairo] package).
#' @param font_size Standard font size
#' @param rel_small Relative size of small font
#' @param rel_tiny Relative size of tiny font
#' @param rel_large Relative size of large font
#' @param grid_major_color Color of major grid lines
#' @param grid_minor_color Color of minor grid lines
#' @param grid_major_linewidth Width of major grid lines
#' @param grid_minor_linewidth Width of minor grid lines
#' @param axis_title_hjust Horizontal adjustment of axis titles
#' @param axis_title_size Font size of axis titles
#' @param axis_text_size Font size of axis test
#'
#' @import cowplot
#' @import Cairo
#' @import ggplot2
#'
#' @return ggplot2 theme
#' @export
#'
my_theme <- function(font_family = "Arial Narrow",
                         font_size = 16,
                         rel_small = 14/16,
                         rel_tiny = 12/16,
                         rel_large = 18/16,
                         grid_major_color = "grey90",
                         grid_minor_color = "grey95",
                         grid_major_linewidth = 0.4,
                         grid_minor_linewidth = 0.3,
                         axis_title_hjust = 0.97,
                         axis_title_size = 18,
                         axis_text_size = 16) {

  cowplot::theme_minimal_grid(font_family = font_family,
                              font_size = font_size,
                              rel_small = rel_small,
                              rel_tiny = rel_tiny,
                              rel_large = rel_large,
                              color = grid_major_color,
                              line_size = grid_major_linewidth) %+replace%
    theme(
      panel.grid.minor = element_line(color = grid_minor_color,
                                      linewidth = grid_minor_linewidth),
      axis.title = element_text(hjust = axis_title_hjust, size = axis_title_size),
      axis.text = element_text(size = axis_text_size)
      )

}


#' Custom ggplot2 map theme
#'
#' Custom ggplot theme for maps.
#'
#' @param map_grid Logical, whether to show grid of longitude and latitude
#' @param map_grid_labels Logical, whether to show axis labels of longitude/latitude
#' @param ... Additional parameters passed on to [my_theme]
#'
#' @import ggplot2
#' @import cowplot
#'
#' @return Custom ggplot2 map theme
#'
#' @export

my_maptheme <- function(map_grid = FALSE,
                        map_grid_labels = FALSE,
                        ...) {

  if (map_grid && map_grid_labels) {
    my_theme(...)
  }

  if (!map_grid && map_grid_labels) {
    my_theme(...) %+replace%
      theme(panel.grid = element_blank())
  }

  if (map_grid && !map_grid_labels) {
    my_theme(...) %+replace%
      theme(axis.text = element_blank())
  }

  if (!map_grid && !map_grid_labels) {
    my_theme(...) %+replace%
      theme(panel.grid = element_blank(),
            axis.text = element_blank())
  }
}


#' Define bounding box
#'
#' Limit map extent in terms of a shape's bounding box.
#'
#' @param shp Shape whose bounding box defines the extent of the map.
#' @param expand Numerical vector of length 2, if the extent of `shp` should be extended.
#'   First element extends the x-axis, while the second element extends the y-axis. The map
#'   will be extended in both directions equally. That is, if `extend` is `c(2,2)`, the y-axis
#'   will be extended 2 units (degrees) from its current minimum and 2 units from its current
#'   maximum.
#' @param expand_x Numerical scalar, specify only the extension of the x-axis.
#' @param expand_y Numerical scalar, specify only the extension of the y-axis.
#'
#' @return [ggplot2::lims()] element
#'
#' @import cli
#' @importFrom ggplot2 lims
#' @importFrom sf st_bbox
#'
#' @export

coord_bbox <- function(shp, expand = c(0,0), expand_x, expand_y) {
  if ((!missing(expand_x) && missing(expand_y)) || (!missing(expand_y) && missing(expand_x))) {
    cli::cli_abort("Both {.arg expand_x} and {.arg expand_y} must be specified.")
  }

  if (!missing(expand_x) && !missing(expand_y)) {
    if (length(expand_x) != 1 | length(expand_y) != 1) {
      cli::cli_abort("{.arg expand_x} and {.arg expand_y} must be numeric scalars.")
    }

    if (!is.numeric(expand_x) | !is.numeric(expand_y)) {
      cli::cli_abort("{.arg expand_x} and {.arg expand_y} must be numeric scalars.")
    }

    ggplot2::lims(x = st_bbox(shp)[c("xmin", "xmax")] + c(-expand_x, expand_x),
                  y = st_bbox(shp)[c("ymin", "ymax")] + c(-expand_y, expand_y))
  } else {

    if (length(expand) != 2) {
      cli::cli_abort("{.arg expand} must be a numeric vector of length 2.")
    }

    if (!is.numeric(expand)) {
      cli::cli_abort("{.arg expand} must be numeric vector of length 2.")
    }

    expand <- expand * c(-1, 1)
    ggplot2::lims(x = st_bbox(shp)[c("xmin", "xmax")] + expand,
                  y = st_bbox(shp)[c("ymin", "ymax")] + expand)
  }
}



