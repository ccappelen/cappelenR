#' Get fixest coefficients
#'
#' @param model Model of class `fixest`.
#' @param levels Numeric vector of confidence levels. Must be strictly between 0 and 1.
#'  For example `c(0.90, 0.95)`.
#' @param keep Character vector of coefficients to keep
#' @param drop_constant Logical, whether to drop the intercept.
#' @param append Logical, whether to append the output to `append_x`. If `TRUE` `append_x`
#'  must be supplied.
#' @param append_x Dataframe to append the coefficient estimates (must be suppled if
#'  `append` is `TRUE`).
#'
#' @import cli
#' @import dplyr
#' @import fixest
#' @return A dataframe of coefficient estimates and confidence intervals.
#' @export
#'
get_coefs <- function(model,
                      levels = c(0.95, 0.90),
                      keep,
                      drop_constant = TRUE,
                      append = FALSE,
                      append_x) {

  if (length(levels) == 0) {
    cli::cli_abort("{.arg levels} must be a vector of at least one value between 0 and 1.")
  }

  if (!is.numeric(levels)) {
    cli::cli_abort("{.arg levels} must be a numeric vector.")
  }

  if (!all(levels > 0 & levels < 1)) {
    cli::cli_abort("{.arg levels} must be strictly between 0 and 1.")
  }

  if (drop_constant == T) {
    intercept = "Constant"
  } else {
    intercept = ""
  }

  if (length(levels) == 1) {
    coefs <- fixest::coefplot(model, ci_level = levels,
                              keep = keep, drop = intercept)$prms |>
      dplyr::rename_with(.cols = c(ci_high, ci_low),
                         .fn = ~ paste0(.x, levels*100))
  } else {
    coefs <- fixest::coefplot(model, ci_level = levels[1],
                              keep = keep, drop = intercept)$prms |>
      dplyr::rename_with(.cols = c(ci_high, ci_low),
                         .fn = ~ paste0(.x, levels[1]*100))

    for (i in levels[-1]) {
      coefs <- cbind(
        coefs,
        fixest::coefplot(model, ci_level = i,
                         keep = keep, drop = intercept)$prms |>
          dplyr::rename_with(.cols = c(ci_high, ci_low),
                             .fn = ~ paste0(.x, i*100)) |>
          dplyr::select(starts_with("ci_"))
      )
    }
  }

  coefs <- coefs |>
    dplyr::select(estimate_names, estimate_names_raw, estimate,
                  starts_with("ci_low"), starts_with("ci_high"))

  if (append && missing(append_x)) {
    cli::cli_abort("Must specify a data.frame in {.arg append_x} to append to.")
  }

  if (append && !missing(append_x)) {
    if (ncol(coefs) != append_x) {
      cli::cli_abort("{.arg append_x} has different number of columns.")
    } else {
      coefs <- dplyr::bind_rows(
        append_x,
        coefs)
    }
  }

  return(coefs)
}
