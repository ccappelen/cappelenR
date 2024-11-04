#' Produce contour plots from sensitivity models
#'
#' Plots contour (and related) plots from sensivity analyses conducted
#' using [sensemakr::sensemakr]. Elements of the plot can be styled subsequently
#' using standard [ggplot2::theme] elements.
#'
#' @param mod.sens Object of class `sensemakr`.
#' @param plot_type Type of sensitivity plot. It can be either "estimate" (default)
#'   for contour plots with sensitivity of point estimates; "t-value" for contour plots
#'   with sensitivity of t-values; or "extreme" for extreme scenarios plots.
#' @param n_levels The approximate number of contour lines to draw.
#' @param levels_range Whether the drawing of contour lines should use the full range of values
#'   or only exclude the top- and bottom percentiles. This is mostly a convenience option to
#'   to adjust the number of contour lines shown.
#' @param label_hjust Horizontal adjustment of labels.
#' @param label_vjust Vertical adjustment of labels.
#'
#' @import sensemakr
#' @import dplyr
#' @import stringr
#' @import ggplot2
#' @importFrom metR geom_label_contour
#' @importFrom metR label_placer_random
#' @importFrom reshape2 melt
#' @importFrom stats quantile
#' @importFrom stats setNames
#' @importFrom grDevices dev.off
#' @importFrom grDevices pdf
#'
#' @return ggplot of contour lines.
#' @export
#'
#' @section Examples:
#'
#' ```R
#' library(sensemakr)
#' library(dplyr)
#' library(stringr)
#' library(ggplot2)
#'
#' mod <- lm(
#'   peacefactor ~ directlyharmed  + village +  female +
#'     age + farmer_dar + herder_dar + pastvoted + hhsize_darfur,
#'   data = darfur)
#'
#' mod.sens <- sensemakr(
#'   model = mod,
#'   treatment = "directlyharmed",
#'   benchmark_covariates = "female",
#'   kd = 1:3)
#'
#' # Sensivitity plot of estimates
#' ggsensemakr(mod.sens)
#'
#' # Sensivitity plot of t-values
#' ggsensemakr(mod.sens, plot_type = "t-value")
#'
#' # Sensivitity plot of extreme confounding
#' ggsensemakr(mod.sens, plot_type = "extreme")
#' ```
#'


ggsensemakr <- function(
    mod.sens,
    plot_type = c("estimate", "t-value", "extreme"),
    n_levels = 8,
    levels_range = c("range", "iqr"),
    label_hjust = 0.02,
    label_vjust = 0.00) {

  ## CHECK FOR CLASS 'sensemakr'

  plot_type <- match.arg(plot_type)
  if (missing(plot_type) | !(plot_type %in% c("estimate", "t-value", "extreme"))) {
    stop("Plot type of either (1) 'estimate', 't-value', or 'extreme' is required.")
  }

  levels_range <- match.arg(levels_range)
  if (missing(levels_range) | !(levels_range %in% c("range", "iqr"))) {
    stop("Option levels_range has to be set to either 'range' or 'iqr'.")
  }


  ## Plot type: estimate
  if (plot_type == "estimate") {
    pdf(NULL)
    plot_list <- plot(mod.sens, sensitivity.of = plot_type)
    dev.off()

    bounds <- mod.sens$bounds |>
      dplyr::select(bound_label, x = r2dz.x, y = r2yz.dx, adjusted_estimate) |>
      dplyr::mutate(multiple = str_extract(bound_label, "\\d")) |>
      dplyr::mutate(gglabel = paste0(multiple, " \U00D7", " Benchmark", "\n", "(", round(adjusted_estimate, 3), ")"))

    unadjusted <- data.frame(
      x = 0, y = 0, z = 0,
      estimate = mod.sens$sensitivity_stats$estimate,
      gglabel = paste0("Unadjusted\n", "(", round(mod.sens$sensitivity_stats$estimate, 3), ")")
    )

    threshold <- mod.sens$info$q
    estimate_threshold <- round(mod.sens$sensitivity_stats$estimate * (1 - threshold), 3)
  }


  ## Plot type: t-value
  if (plot_type == "t-value") {
    pdf(NULL)
    plot_list <- plot(mod.sens, sensitivity.of = plot_type)
    dev.off()

    bounds <- mod.sens$bounds |>
      dplyr::select(bound_label, x = r2dz.x, y = r2yz.dx, adjusted_t) |>
      dplyr::mutate(multiple = stringr::str_extract(bound_label, "\\d")) |>
      dplyr::mutate(gglabel = paste0(multiple, " \U00D7", " Benchmark", "\n", "(", round(adjusted_t, 3), ")"))

    unadjusted <- data.frame(
      x = 0, y = 0, z = 0,
      estimate = mod.sens$sensitivity_stats$estimate,
      gglabel = paste0("Unadjusted\n", "(", round(mod.sens$sensitivity_stats$t_statistic, 3), ")")
    )

    estimate_threshold <- 1.96
  }


  ## Plot type: estimate or t-value
  if (plot_type == "estimate" | plot_type == "t-value") {
    plot_df <- reshape2::melt(plot_list$value[])
    plot_df <- transform(
      plot_df,
      x = plot_list$r2dz.x[Var1],
      y = plot_list$r2yz.dx[Var2]
    )

    if (levels_range == "range") {
      default_levels <- pretty(range(plot_list$value), n_levels)
      too_close <- abs(default_levels - estimate_threshold) < min(diff(default_levels)) * 0.25
      default_levels <- default_levels[!too_close]
    }

    if (levels_range == "iqr") {
      default_levels <- pretty(c(quantile(plot_list$value, 1 / 10), quantile(plot_list$value, 9 / 10)), 8)
      too_close <- abs(default_levels - estimate_threshold) < min(diff(default_levels)) * 0.25
      default_levels <- default_levels[!too_close]
    }

    fig <- suppressWarnings(ggplot2::ggplot(data = plot_df, aes(x = x, y = y, z = value)) +
      ggplot2::stat_contour(col = "black", breaks = default_levels, linewidth = 0.4) +
      metR::geom_label_contour(
        label.size = 0, color = "grey70", skip = 0, breaks = default_levels,
        label.placer = metR::label_placer_random()
      ) +
      ggplot2::stat_contour(data = plot_df, col = "black", linetype = "dashed", breaks = estimate_threshold, linewidth = 1) +
      metR::geom_label_contour(label.size = 0, color = "black", skip = 0, breaks = estimate_threshold) +
      ggplot2::geom_point(data = bounds, mapping = aes(x = x, y = y, z = 0), shape = 4, size = 2, stroke = 1.5) +
      ggplot2::geom_text(
        data = bounds, aes(x = x + label_hjust, y = y + label_vjust, z = 0, label = gglabel),
        hjust = "center", vjust = 0
      ) +
      ggplot2::geom_point(data = unadjusted, mapping = aes(x = x, y = y, z = 0), shape = 17, size = 3) +
      ggplot2::geom_text(
        data = unadjusted, aes(x = x + label_hjust, y = y + label_vjust, z = 0, label = gglabel),
        hjust = "center", vjust = 0
      ) +
      ggplot2::labs(
        x = expression(paste("Partial ", R^2, " of confounder(s) with the treatment")),
        y = expression(paste("Partial ", R^2, " of confounder(s) with the outcome"))
      ))
  }


  ## Plot type: extreme
  if (plot_type == "extreme") {
    pdf(NULL)
    plot_list <- plot(mod.sens, type = "extreme")
    dev.off()

    benchmark_labels <- c("100 %" = "solid", "75 %" = "dashed", "50 %" = "dotted")
    benchmark_df <- data.frame(
      x = plot_list$bounds,
      y = 0
    )

    plot_list$bounds <- NULL
    plot_list <- lapply(plot_list, FUN = setNames, nm = c("x", "y", "adjusted_estimate"))
    names(plot_list) <- c("R100", "R75", "R50")

    fig <- suppressWarnings(
      ggplot2::ggplot(aes(x = x, y = adjusted_estimate), data = plot_list$R100) +
        ggplot2::geom_line(data = plot_list$R100, aes(linetype = "100 %")) +
        ggplot2::geom_line(data = plot_list$R75, aes(linetype = "75 %")) +
        ggplot2::geom_line(data = plot_list$R50, aes(linetype = "50 %")) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
        ggplot2::geom_point(data = benchmark_df, aes(x = x, y = y), shape = 4, size = 2, stroke = 2) +
        ggplot2::labs(
          x = expression(paste("Partial ", R^2, " of confounder(s) with the treatment")),
          y = "Adjusted effect estimate",
          linetype = expression(paste("Partial ", R^2, " of confounder(s) with the outcome"))
          ) +
        ggplot2::scale_linetype_manual(values = benchmark_labels, breaks = names(benchmark_labels)[c(1, 2, 3)]) +
        ggplot2::theme(
          legend.position = "inside", legend.position.inside = c(.5, .9),
          legend.direction = "horizontal",
          legend.title.position = "top"
      ))
  }

  fig
  # print(fig)
  # return(invisible(fig))
}
