% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mlme_plot.R
\name{nlme_plot}
\alias{nlme_plot}
\title{Plots ggplo2-made graphics from \code{nlme::nlsList()} or \code{nlme::nlme()}objects.}
\usage{
nlme_plot(
  df,
  x.axis,
  y.axis,
  grouping.var,
  fun,
  nlme.obj,
  color = "blue",
  linewidth = 0.5,
  x.lab = NULL,
  y.lab = NULL
)
}
\arguments{
\item{df}{Data frame used to fit the non-linear models by the functions \code{nlme::nlsList()} or \code{nlme::nlme()}.}

\item{x.axis}{Independent variable used to the fit non-linear models. It must be contained in the data frame.}

\item{y.axis}{Dependent variable used to the fit non-linear models. It must be contained in the data frame.}

\item{grouping.var}{Variable that defines the data grouping. Example: \code{y ~ x | grouping.var}.}

\item{fun}{Modified non-linear function with \code{.args} prefix (required for internal computation) and suffixes that refer to the parameters of the non-linear model used.
Example: if the logistic model was used as \code{function(x, Asym, xmid, scal) {Asym / (1 + exp(-scal * (x - xmid)))}} in the \code{nlme::nlsList()} or \code{nlme::nlme()} functions, the argument \code{fun} will be \code{function(x, .args) {.args$Asym / (1 + exp(-.args$scal * (x - .args$xmid)))}}.}

\item{nlme.obj}{Object returned using the functions \code{nlme::nlsList()} or \code{nlme::nlme()}.}

\item{color}{Sets the color of the plotted estimated non-linear curves.}

\item{linewidth}{Sets the line width of the plotted estimated non-linear curves.}

\item{x.lab}{Sets the x-axis label. Default is \code{NULL} and if \code{NULL} returns the value of the x.axis argument.}

\item{y.lab}{Sets the y-axis label. Default is \code{NULL} and if \code{NULL} returns the value of the y.axis argument.}
}
\value{
Return a list and a panel of ggplo2-made graphics from the object generated by the \code{nlme::nlsList()} function. Non-linear curves are smoothed by using the \code{ggplot2::stat_function()} function.
}
\description{
This function returns a list and panel of ggplo2-made graphics from the object generated by the functions \code{nlme::nlsList()} or \code{nlme::nlme()} using internally the function \code{ggplot2::stat_function()}.
}
\examples{

# Data frame: Orange
df <- datasets::Orange


# Logistic function
logistic <- function(x, Asym, xmid, scal){
    Asym / (1 + exp(- scal * (x - xmid)))}


# Object from nlsList function
nlsList.obj <- nlme::nlsList(circumference ~ logistic(age, Asym, xmid, scal),
                             start = list(Asym = 170, xmid = 600, scal = .004),
                             data = df)


# Arguments for nlme_plot function
x.axis <- "age"
y.axis <- "circumference"
grouping.var <- "Tree"
fun <- function(x, .args) {.args$Asym / (1 + exp(-.args$scal * (x - .args$xmid)))}
x.lab <- "Age (days since 1968/12/31)"
y.lab <- "Trunk circumference (mm)"


# Graphics for nlsList object
nlme_plot(df = df,
          x.axis = x.axis,
          y.axis = y.axis,
          grouping.var = grouping.var,
          fun = fun,
          nlme.obj = nlsList.obj,
          color = 'blue',
          linewidth = 0.5,
          x.lab = x.lab,
          y.lab = y.lab) -> nlsList_p

nlsList_p$panel_p


}
