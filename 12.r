x <- readLines("12.txt")
map <- do.call(rbind, strsplit(x, "", fixed = TRUE))
dm <- dim(map)
start <- which(map == "S")
ind_end <- which(map == "E", arr.ind = TRUE)
end <- ind_end[1] + (ind_end[2] - 1L)*dm[1]
map <- matrix(match(chartr("SE", "az", map), letters), nrow = nrow(map))
len <- length(map)

heuristic <- pmax(
  outer(
    abs(seq_len(dm[1]) - ind_end[1]),
    abs(seq_len(dm[2]) - ind_end[2]),
    "+"
  ),
  max(map) - map
)
neighbours <- function(pos) {
  ps <- pos + c(-1L, 1L, -dm[1], dm[1])
  possible <- ps[ps >= 1L & ps <= len]
  possible[map[possible] <= map[pos] + 1L]
}

astar <- function(starts) {
  open <- starts
  closed <- integer()
  costs <- map
  costs[] <- NA_integer_
  costs[starts] <- 0L
  current <- open[which.min(costs[open] + heuristic[open])]
  while (current != end) {
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
    current <- open[which.min(costs[open] + heuristic[open])]
  }
  costs[end]
}

astar(start) # part one: 361
astar(which(map == 1L)) # part two: 354
