
# lab6

<!-- badges: start -->
[![R-CMD-check](https://github.com/jexoss/lab6/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jexoss/lab6/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of lab6 is to solve the 0/1 knapsack problem using three different 
approaches - brute force, dynamic programming, and a greedy heuristic - and to
compare their running times. The brute force approach can optionally be 
parallelized across multiple CPU cores.

## Installation

You can install the development version of lab6 from
[GitHub](https://github.com/) with:

``` r
devtools::install_github("jexoss/lab6", build_vignettes = TRUE)
```

## Example

This is a basic example which shows you how to solve a knapsack problem with 
each of the three algorithms:

``` r
library(lab6)

data(knapsack_objects)

# Exact solution via brute force
brute_force_knapsack(x = knapsack_objects[1:8, ], W = 3500)

# Exact solution via dynamic programming(faster for larger n)
knapsack_dynamic(x = knapsack_objects[1:500, ], W = 3500)

# Fast approximate solution via greedy heuristic (scales to very large n)
greedy_knapsack(x = knapsack_objects[1:1000000, ], W = 3500)
```

brute force can also be parallelized across CPU cores:

```
brute_force_knapsack(x = knapsack_objects[1:16, ], W = 3500, parallel = TRUE)
```

See `vignette("knapsack")` for a full walkthrough including timing comparisons
between the naive and vectorized dynamic programming implementations.
