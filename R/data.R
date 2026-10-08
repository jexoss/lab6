#' Random objects for the knapsack problem
#'
#' A randomly generated set of 2000 objects, each with a weight and a
#' value, used to test and benchmark the knapsack algorithms in this
#' package. Generated with \code{set.seed(42, kind = "Mersenne-Twister",
#' normal.kind = "Inversion")} after \code{RNGversion("3.5.3")}, so the
#' results are reproducible and match the examples in the lab instructions.
#'
#' @format A data.frame with 2000 rows and 2 variables:
#' \describe{
#'  \item{w}{Weight of the object, an integer between 1 and 4000.}
#'  \item{v}{Value of the object, a number between 0 and 10000.}
#'}
#'
#'@source \url{https://en.wikipedia.org/wiki/Knapsack_problem}
"knapsack_objects"
