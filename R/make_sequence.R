#' make a pseudo-random sequence that avoids runs
#' @export
make_sequence <- function(nchoices, n = 20) {
  set <- sample(nchoices,  3*n,  replace=TRUE)
  runs <- c(FALSE, set[-1] == set[-length(set)])
  twos <- c(FALSE, FALSE, set[-(1:2)] == set[-(length(set) - c(0, 1)) ])

  no_runs <- set[!runs & !twos]
  while  (length(no_runs) < n) {
    no_runs <- c(no_runs, rev(no_runs))
  }

  no_runs[1:n]
}
