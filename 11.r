x <- readLines("11.txt")
blank <- x == ""
blocks <- split(x[!blank], findInterval(seq_along(x)[!blank], which(blank)))
starts <- lapply(
  blocks,
  \(b) strtoi(sub(",", "", strsplit(b[2], " ", fixed = TRUE)[[1]][-(1:4)], fixed = TRUE))
)
ops <- lapply(
  blocks,
  \(b) {
    str <- substring(b[3], nchar("  Operation: new = ") + 1L)
    if (!is.na(strtoi((substr(str, nchar(str), nchar(str))))))
      str <- paste0(str, "L")
    fn <- function(old) NULL
    body(fn) <- parse(text = str)
    fn
  }
)
test_divisors <- vapply(
  blocks,
  \(b) strtoi(substring(b[4], nchar("  Test: divisible by ") + 1L)),
  integer(1)
)
test_succs <- 1L + vapply(
  blocks,
  \(b) strtoi(substring(b[[5]], nchar("    If true: throw to monkey ") + 1L)),
  integer(1)
)
test_fails <- 1L + vapply(
  blocks,
  \(b) strtoi(substring(b[[6]], nchar("    If false: throw to monkey ") + 1L)),
  integer(1)
)

nm <- length(starts)
next_round <- function(items, counts) {
  for (m in seq_along(items)) {
    new_items <- ops[[m]](items[[m]]) %/% 3L
    tests <- new_items %% test_divisors[m]
    counts[m] <- counts[m] + length(new_items)
    items[[m]] <- integer()
    items[[test_succs[m]]] <- c(items[[test_succs[m]]], new_items[tests == 0L])
    items[[test_fails[m]]] <- c(items[[test_fails[m]]], new_items[tests != 0L])
  }
  list(items, counts)
}

after_twenty <- Reduce(
  \(lst, n) next_round(lst[[1]], lst[[2]]),
  seq_len(20L),
  init = list(starts, rep(0L, nm))
)
prod(sort(after_twenty[[2]], partial = nm - 1:0)[nm - 1:0]) # part one: 56350

modulus <- prod(test_divisors) # assume coprime
next_round2 <- function(items, counts) {
  for (m in seq_along(items)) {
    new_items <- ops[[m]](items[[m]]) %% modulus
    tests <- new_items %% test_divisors[m]
    counts[m] <- counts[m] + length(new_items)
    items[[m]] <- integer()
    items[[test_succs[m]]] <- c(items[[test_succs[m]]], new_items[tests == 0L])
    items[[test_fails[m]]] <- c(items[[test_fails[m]]], new_items[tests != 0L])
  }
  list(items, counts)
}
after_many <- Reduce(
  \(lst, n) next_round2(lst[[1]], lst[[2]]),
  seq_len(10000L),
  init = list(starts, rep(0L, nm))
)
prod(sort(after_many[[2]], partial = nm - 1:0)[nm - 1:0]) # part two: 13954061248
