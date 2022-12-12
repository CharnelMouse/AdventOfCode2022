x <- readLines("12.txt")
map <- do.call(rbind, strsplit(x, "", fixed = TRUE))
start <- which(map == "S", arr.ind = TRUE)
end <- which(map == "E", arr.ind = TRUE)
map <- matrix(match(chartr("SE", "az", map), letters), nrow = nrow(map))
dm <- dim(map)
len <- length(map)

# A* time
heuristic <- pmax(
  outer(
    abs(seq_len(dm[1]) - end[1]),
    abs(seq_len(dm[2]) - end[2]),
    "+"
  ),
  max(map) - map
)
flat_start <- start[1] + (start[2] - 1L)*dm[1]
flat_end <- end[1] + (end[2] - 1L)*dm[1]
neighbours <- function(pos) {
  ps <- pos + c(-1L, 1L, -dm[1], dm[1])
  possible <- ps[ps >= 1L & ps <= len]
  possible[map[possible] <= map[pos] + 1L]
}
open <- flat_start
closed <- integer()
costs <- map
costs[] <- NA_integer_
costs[flat_start] <- 0L
current <- open[which.min(costs[open] + heuristic[open])]

while (current != flat_end) {
  current_dist <- costs[current]
  open <- open[open != current]
  closed <- c(current, closed)
  ns <- neighbours(current)
  for (n in ns) {
    cost <- current_dist + 1L
    n_index <- match(n, open)
    if (!is.na(n_index) && (is.na(costs[n]) || cost < costs[n])) {
      open <- open[-n_index]
    }
    n_index2 <- match(n, closed)
    if (!is.na(n_index2) && (is.na(costs[n]) || cost < costs[n])) {
      closed <- closed[-n_index2]
    }
    if (!is.element(n, c(open, closed))) {
      costs[n] <- cost
      open <- c(n, open)
    }
  }
  if (length(open) == 0)
    stop("open is empty")
  current <- open[which.min(costs[open] + heuristic[open])]
}
costs[flat_end] # part one: 361

open <- which(map == 1L)
closed <- integer()
costs <- map
costs[] <- NA_integer_
costs[open] <- 0L
current <- open[which.min(costs[open] + heuristic[open])]

while (current != flat_end) {
  current_dist <- costs[current]
  open <- open[open != current]
  closed <- c(current, closed)
  ns <- neighbours(current)
  for (n in ns) {
    cost <- current_dist + 1L
    n_index <- match(n, open)
    if (!is.na(n_index) && (is.na(costs[n]) || cost < costs[n])) {
      open <- open[-n_index]
    }
    n_index2 <- match(n, closed)
    if (!is.na(n_index2) && (is.na(costs[n]) || cost < costs[n])) {
      closed <- closed[-n_index2]
    }
    if (!is.element(n, c(open, closed))) {
      costs[n] <- cost
      open <- c(n, open)
    }
  }
  if (length(open) == 0)
    stop("open is empty")
  current <- open[which.min(costs[open] + heuristic[open])]
}
costs[flat_end] # part two: 354
