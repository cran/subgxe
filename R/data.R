#' Synthetic data for subgxe
#'
#' @format A list of 5 \code{data.frames} with 12000 observations
#' (6000 cases, 6000 controls) on 4 variables:
#' \describe{
#'   \item{D}{Disease status. Numeric 0-1}
#'   \item{G}{Genetic variant. Numeric 0-1}
#'   \item{E}{Exposure. Numeric 0-1}
#'   \item{GbyE}{\code{G * E}. Either 1 or 0.}
#' }
"studies"
