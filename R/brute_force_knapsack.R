#' Solve the knapsack problem with brute force
#'
#' finds the exact solution to the 0/1 knapsack problem by exhaustively checking
#' every possible subset of the objects. Each subset is represented as a
#' binary number, generated with \code{intToBits()}, where a 1 in a bit
#' position k means object k is included.
#'
#' @details
#' With n objects there are 2^n possible subsets, so the running time grows
#' exponentially. This is the only approach that is guaranteed to find the
#' exact optima l solution, but it is only practical for small n
#' (rougly n <= 20)
#'
#' @param x A data.frame with columns \code{v} (value) and \code{w}
#'  (weight), both positive.
#' @param W the knapsack's weight capacity, a positive numeric scalar.
#'
#' @return A list with \code{value} (the maximum total value found) and
#'  \code{elements} (the row indices of \code{x} included in that solution).
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Knapsack_problem}
#'
#' @examples
#' brute_force_knapsack(x = knapsack_objects[1:8, ], W = 3500)
#'
#' @export
brute_force_knapsack <- function(x, W) {
  stopifnot(
    is.data.frame(x),
    all(c("v", "w") %in% names(x)),
    all(x$v > 0),
    is.numeric(W), length(W) == 1, W > 0
  )

  n <- nrow(x)
  best_value <- 0
  best_elements <- integer(0)

  for (i in seq_len(2^n -1)){
    bits <- as.integer(intToBits(i))[1:n]
    elements <- which(bits == 1)

    total_w <- sum(x$w[elements])
    if (total_w <= W) {
      total_v <- sum(x$v[elements])
      if (total_v > best_value) {
        best_value <- total_v
        best_elements <- elements
      }
    }
  }
  list(value = best_value, elements = best_elements)
}
