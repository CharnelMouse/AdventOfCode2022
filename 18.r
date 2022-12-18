x <- readLines("18.txt") |>
  strsplit(",", fixed = TRUE) |>
  lapply(strtoi, integer(3))
doubled <- lapply(x, "*", 2L)
edges <- c(
  lapply(doubled, \(v) v + c(1L, 0L, 0L)),
  lapply(doubled, \(v) v - c(1L, 0L, 0L)),
  lapply(doubled, \(v) v + c(0L, 1L, 0L)),
  lapply(doubled, \(v) v - c(0L, 1L, 0L)),
  lapply(doubled, \(v) v + c(0L, 0L, 1L)),
  lapply(doubled, \(v) v - c(0L, 0L, 1L))
)
dups <- edges[duplicated(edges)]
nondupped_edges <- setdiff(edges, dups)
length(nondupped_edges) # part one: 4482

mins <- Reduce(pmin, nondupped_edges)
maxs <- Reduce(pmax, nondupped_edges)
lbounds <- mins - 1L
ubounds <- maxs + 1L
size <- prod(1L + (ubounds - lbounds))

# flood fill
queue <- list(lbounds)
resolved <- list()
targets <- nondupped_edges
while (length(queue) > 0 && length(targets) > 0) {
  nxt <- queue[[1]]
  queue <- queue[-1]
  stopifnot(!is.element(list(nxt), resolved))
  neighbour_edges <- lapply(
    list(
      c(1L, 0L, 0L),
      c(0L, 1L, 0L),
      c(0L, 0L, 1L),
      -c(1L, 0L, 0L),
      -c(0L, 1L, 0L),
      -c(0L, 0L, 1L)
    ),
    "+",
    nxt
  )
  neighbour_cells <- lapply(
    list(
      c(2L, 0L, 0L),
      c(0L, 2L, 0L),
      c(0L, 0L, 2L),
      -c(2L, 0L, 0L),
      -c(0L, 2L, 0L),
      -c(0L, 0L, 2L)
    ),
    "+",
    nxt
  )
  valid <- vapply(
    neighbour_cells,
    \(n) all(n >= lbounds) && all(n <= ubounds),
    logical(1)
  )
  valid_neighbours <- neighbour_cells[
    valid &
      !is.element(neighbour_cells, resolved) &
      !is.element(neighbour_edges, targets)
  ]
  queue <- c(queue, setdiff(valid_neighbours, c(queue, resolved)))
  targets <- setdiff(targets, neighbour_edges)
  resolved <- c(resolved, list(nxt))
}
length(nondupped_edges) - length(targets) # part two: 2576
