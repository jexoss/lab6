#' Solve the knapsack problem with dynamic programming
#'
#' Finds the exact solution to the 0/1 knapsack problem using dynamic
#' programming. A table of the best achievable value is built up for every
#' combination of "first i objects" and "capacity w", then the chosen
#' objects are found by backtracking through the table.
#'
#' @details
#' Since the weights are integers, the capacity only needs to be checked at
#' integer values, giving a running time of O(W * n) instead of the O(2^n) of
#' brute force search. This makes it practical for far larger n,
#' as long as W is not extremely large.
#'
#' @param x A data.frame with columns \code{v} (value) and \code{w}
#' (weight), both positive.
#' @param W the knapsack's weight capacity, a positive numeric scalar.
#'
#' @return A list with \code{value} (the maximum total value found) and
#' \code{elements} (the row indices of \code{x} included in that solution).
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Knapsack_problem#Dynamic_programming_in-advance_algorithm}
#'
#' @examples
#' knapsack_dynamic(x = knapsack_objects[1:8, ], W = 3500)
#'
#' @export
knapsack_dynamic <- function(x, W) {
  stopifnot(
    is.data.frame(x),
    all(c("v", "w") %in% names(x)),
    all(x$v > 0),
    all(x$w > 0),
    is.numeric(W), length(W) == 1, W > 0
  )

  n <- nrow(x)
  Wcap <- floor(W)

  m <- matrix(0, nrow = n + 1, ncol = Wcap +1)

  for (i in seq_len(n)) {
    wi <- x$w[i]
    vi <- x$v[i]
    prev <- m[i, ]

    if(wi > Wcap) {
      m[i + 1, ] <- prev
    }
    else {
      shifted <- c(rep(-Inf, wi), prev[seq_len(Wcap + 1 - wi)]) + vi
      m[ i + 1, ] <- pmax(prev, shifted)
    }
  }

  best_value <- m[n + 1, Wcap + 1]

  elements <- integer(0)
  w <- Wcap
  for (j in n:1) {
    if (m[j + 1, w + 1] != m[j, w + 1]) {
      elements <- c(j, elements)
      w <- w - x$w[j]
    }
  }

  list(value = best_value, elements = elements)
}
