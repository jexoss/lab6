# Searches a given range of combination indices and returns the best
# value/elements found within that range. Internal helper used by both
# the sequential and parallel paths of brute_force_knapsack().
.brute_force_range <- function(x, W, idx_range) {
  n <- nrow(x)
  best_value <- 0
  best_elements <- integer(0)

  for (i in idx_range) {
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

#' Solve the knapsack problem with brute force
#'
#' Finds the exact solution to the 0/1 knapsack problem by exhaustively
#' chekcing every possible subset of the objects. Each subset it
#' represented as a binary number, generated with \code{intToBits()}, where
#' a 1 in bit position k means object k is included.
#'
#' @details
#' with n objects there are 2^n possible subsets, so the running time grows
#' exponentially. This is the only approach that is guaranteed to find the
#' exact optimal solution, but it is only practical for small n
#' (roughly n <= 20).
#'
#' When \code{parallel = TRUE}, the search space is split into one chunk per
#' detected CPU core, searched independently in separate processes
#' with \code{parallel::makeCluster()}, and the best result across chunks is
#' is returned. This uses a PSOCK cluster rather than forking, so it works the
#' same way on Windows, macOS and Linux, though the actual speedup
#' achieved is platform- and hardware-dependent.
#'
#' @param x A data.frame with columns \code{v} (value) and \code{w}
#' (weight), both positive.
#' @param W the knapsack's weight capacity, a positive numeric scalar.
#' @param parallel If \code{TRUE}, parallelize the search across the detected
#' CPU cores. Defaults to \code{FALSE}
#'
#' @return A list with \code{value} (the maximum total value found) and
#' \code{elements} (the row indices of \code{x} included in that solution).
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Knapsack_problem}
#'
#' @examples
#' brute_force_knapsack(x = knapsack_objects[1:8, ], W = 3500)
#' \dontrun{
#' brute_force_knapsack(x = knapsack_objects[1:16, ], W = 3500, parallel = TRUE)
#' }
#'
#' @importFrom parallel detectCores makeCluster stopCluster clusterExport parLapply splitIndices
#'
#' @export
brute_force_knapsack <- function (x, W, parallel = FALSE){
  stopifnot(
    is.data.frame(x),
    all(c("v", "w") %in% names(x)),
    all(x$v > 0),
    all(x$w > 0),
    is.numeric(W), length(W) == 1, W > 0,
    is.logical(parallel), length(parallel) == 1
    )
  n <- nrow(x)
  total <- 2^n - 1

  if (!parallel) {
    return(.brute_force_range(x, W, seq_len(total)))
  }

  ncores <- min(parallel::detectCores(), total)

  chk <- Sys.getenv("_R_CHECK_LIMIT_CORES_", "")
  if (nzchar(chk) && chk != "FALSE") {
    ncores <- min(ncores, 2)
  }

  if (is.na(ncores) || ncores < 2){
    return(.brute_force_range(x, W, seq_len(total)))
  }

  chunks <- parallel::splitIndices(total, ncores)

  fn <- .brute_force_range
  cl <- parallel::makeCluster(ncores)
  on.exit(parallel::stopCluster(cl))
  parallel::clusterExport(cl, varlist = "fn", envir = environment())

  results <- parallel::parLapply(cl, chunks, function(idx, data, W){
    fn(data, W, idx)
  }, data = x, W = W)

  values <- vapply(results, function(r) r$value, numeric(1))
  results[[which.max(values)]]
}
