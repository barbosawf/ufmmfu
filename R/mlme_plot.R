#' Plots ggplo2-made graphics from `nlme::nlsList()` or `nlme::nlme()`objects.
#'
#' @description
#'
#' This function returns a list and panel of ggplo2-made graphics from the object generated by the functions `nlme::nlsList()` or `nlme::nlme()` using internally the function `ggplot2::stat_function()`.
#'
#' @param df Data frame used to fit the non-linear models by the functions `nlme::nlsList()` or `nlme::nlme()`.
#' @param x.axis Independent variable used to the fit non-linear models. It must be contained in the data frame.
#' @param y.axis Dependent variable used to the fit non-linear models. It must be contained in the data frame.
#' @param grouping.var Variable that defines the data grouping. Example: `y ~ x | grouping.var`.
#' @param fun Modified non-linear function with `.args` prefix (required for internal computation) and suffixes that refer to the parameters of the non-linear model used.
#' Example: if the logistic model was used as `function(x, Asym, xmid, scal) {Asym / (1 + exp(-scal * (x - xmid)))}` in the `nlme::nlsList()` or `nlme::nlme()` functions, the argument `fun` will be `function(x, .args) {.args$Asym / (1 + exp(-.args$scal * (x - .args$xmid)))}`.
#' @param nlme.obj Object returned using the functions `nlme::nlsList()` or `nlme::nlme()`.
#' @param color Sets the color of the plotted estimated non-linear curves.
#' @param linewidth Sets the line width of the plotted estimated non-linear curves.
#' @param x.lab Sets the x-axis label. Default is `NULL` and if `NULL` returns the value of the x.axis argument.
#' @param y.lab Sets the y-axis label. Default is `NULL` and if `NULL` returns the value of the y.axis argument.
#'
#' @returns Return a list and a panel of ggplo2-made graphics from the object generated by the `nlme::nlsList()` function. Non-linear curves are smoothed by using the `ggplot2::stat_function()` function.
#'
#' @export
#'
#' @import rlang
#' @importFrom stats fitted na.omit
#'
#' @examples
#'
#' # Data frame: Orange
#' df <- datasets::Orange
#'
#'
#' # Logistic function
#' logistic <- function(x, Asym, xmid, scal){
#'     Asym / (1 + exp(- scal * (x - xmid)))}
#'
#'
#' # Object from nlsList function
#' nlsList.obj <- nlme::nlsList(circumference ~ logistic(age, Asym, xmid, scal),
#'                              start = list(Asym = 170, xmid = 600, scal = .004),
#'                              data = df)
#'
#'
#' # Arguments for nlme_plot function
#' x.axis <- "age"
#' y.axis <- "circumference"
#' grouping.var <- "Tree"
#' fun <- function(x, .args) {.args$Asym / (1 + exp(-.args$scal * (x - .args$xmid)))}
#' x.lab <- "Age (days since 1968/12/31)"
#' y.lab <- "Trunk circumference (mm)"
#'
#'
#' # Graphics for nlsList object
#' nlme_plot(df = df,
#'           x.axis = x.axis,
#'           y.axis = y.axis,
#'           grouping.var = grouping.var,
#'           fun = fun,
#'           nlme.obj = nlsList.obj,
#'           color = 'blue',
#'           linewidth = 0.5,
#'           x.lab = x.lab,
#'           y.lab = y.lab) -> nlsList_p
#'
#' nlsList_p$panel_p
#'
#'
nlme_plot <-
  function(df,
           x.axis,
           y.axis,
           grouping.var,
           fun,
           nlme.obj,
           color = 'blue',
           linewidth = 0.5,
           x.lab = NULL,
           y.lab = NULL
  ) {


    # grouping.var is turned into symbol to be used in nest_by function
    g.var <- rlang::sym(grouping.var)

    ordem <- sort(levels(df[[grouping.var]]))

    df |>
      as.data.frame() |>
      dplyr::mutate_at(dplyr::vars({{grouping.var}}),
                ~ factor(., levels = ordem)) |>
      dplyr::arrange_at(grouping.var) -> df


    stats::coef(nlme.obj) |>
      as.data.frame() |>
      tibble::rownames_to_column(var = grouping.var) |>
      dplyr::mutate_at(dplyr::vars({{grouping.var}}),
                ~ factor(., levels = ordem)) |>
      dplyr::arrange_at(grouping.var) -> arr.nlme.obj


    arr.nlme.obj |>
      stats::na.omit() |>
      droplevels() |>
      split(arr.nlme.obj[[grouping.var]]) |>
      purrr::map(tibble::as_tibble) -> args_list

    arr.nlme.obj |>
      base::apply(1, anyNA) -> id_with_nas

    arr.nlme.obj |>
      dplyr::select({{grouping.var}}) |> dplyr::pull() -> gv

    id_with_nas <- base::as.vector(base::droplevels(gv[id_with_nas]))


    x_min_max <- c(min = min(df[[x.axis]]), max = max(df[[x.axis]]))

    fit_values <- as.vector(na.omit(fitted(nlme.obj)))
    y_min_max <- c(min = min(c(fit_values, df[[y.axis]])),
                   max = max(c(fit_values, df[[y.axis]])))


    filtered_df <-
      df |> dplyr::filter_at(dplyr::vars(dplyr::all_of(grouping.var)), ~ !. %in% id_with_nas)


    nested_df <- filtered_df |>
      dplyr::nest_by({{g.var}}, .keep = T)

    df_list <- nested_df$data |>
      stats::setNames(nested_df[[1]])

    df_list |>
      purrr::map(~.x |>
                   dplyr::summarise_at(dplyr::vars(dplyr::all_of(x.axis)),
                                       list(min, max))) -> lms

    # x.axis and y.axis are turned into symbol to be used in geom_point function
    x.axis <- rlang::sym(x.axis)
    y.axis <- rlang::sym(y.axis)



    f.x <- function(x.lab) {
      if (is.null(x.lab)) {
        # A waiver is a "flag" object to indicate the calling function
        # should just use the default value.
        ggplot2::xlab(ggplot2::waiver())
      } else {
        ggplot2::xlab(x.lab)
      }
    }

    f.y <- function(y.lab) {
      if (is.null(y.lab)) {
        ggplot2::ylab(ggplot2::waiver())
      } else {
        ggplot2::ylab(y.lab)
      }
    }


    # the pmap function is needed to map objects into three lists simultaneously
    # df_list contain a list of data separated by the grouping.var argument
    # therefore df_list is used in the geom_point function
    # args_list contains the list of parameters estimated by the nlsList function
    # therefore args_list is used in the stat_function function
    # lms is a list with the limits of the x-axis where the non-linear function is applied


    purrr::pmap(list(df_list, args_list, lms), \(.df, .args, .lms) {
      ggplot2::ggplot() +
        ggplot2::geom_point(ggplot2::aes(x = {{x.axis}}, y = {{y.axis}}), data = .df) +
        ggplot2::stat_function(
          fun = \(x) {fun(x, .args)},
          color = color,
          xlim = c(.lms[[1]], .lms[[2]]),
          linewidth = linewidth
        ) +
        ggplot2::scale_x_continuous(limits = x_min_max) +
        ggplot2::scale_y_continuous(limits = y_min_max) +
        ggplot2::labs(title = .args[[grouping.var]]) +
        ggplot2::theme_minimal() +
        ggplot2::theme(
          plot.title = ggplot2::element_text(
            size = 10,
            hjust = 0.5,
            vjust = -0.5
          )
        )  +
        f.x(x.lab) +
        f.y(y.lab)

    }) -> list_p


    ggpubr::ggarrange(
      plotlist = list_p |>
        purrr::map( ~ . + ggpubr::rremove('xlab') +
               ggpubr::rremove('ylab'))
    ) |>
      ggpubr::annotate_figure(
        left = ggpubr::text_grob(
          ifelse(is.null(y.lab), y.axis, y.lab),
          rot = 90,
          vjust = 0.5,
          size = 14
        ),
        bottom = ggpubr::text_grob(ifelse(is.null(x.lab), x.axis, x.lab),
                          size = 14)
      ) -> panel_p

    return(list(list_p = list_p, panel_p = panel_p))


  }
