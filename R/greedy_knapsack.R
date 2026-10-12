#' Solve the knapsack problem with a greedy heuristic
#'
#' Finds an approximate solution to the 0/1 knapsack problem by sorting the
#' objects by value per unit weight, and greedily adding them in that
#' order as long as they fit in the remaining capacity.
#'
#' @details
#' This approach does not guarantee an optimal solution but it can be shown
#' to always return at least 50% of the true maximum value. The running time
#' is O(nlogn), dominated by the sort, which makes it practical for much larger
#' n than brute force or dynamic programming.
#'
#' @param x A data.frame with columns\code{v} (value) and \code{w}
#'  (weight), both positive.
#' @param W The knapsack's weight capacity, a positive numeric scalar.
#'
#' @return A list with \code{value} (the total value of the selected objects)
#' and \code{elements} (the row indices of \code{c} included).
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Knapsack_problem#Greedy_approximation_algorithm}
#'
#' @examples
#' greedy_knapsack(x = knapsack_objects[1:800, ], W = 3500)
#'
#' @export
greedy_knapsack <- function(x, W){
  stopifnot(
    is.data.frame(x),
    all(c("v", "w") %in% names(x)),
    all(x$v > 0),
    all(x$w > 0),
    is.numeric(W), length(W) == 1, W > 0
  )

  ratio <- x$v / x$w
  order_idx <- order(ratio, decreasing = TRUE)

  total_w <- 0
  total_v <- 0
  elements <- integer(0)

  for (i in order_idx) {
    if (total_w + x$w[i] > W) break
      total_w <- total_w + x$w[i]
      total_v <- total_v + x$v[i]
      elements <- c(elements, i)
  }

  list(value = total_v, elements = elements)
}
