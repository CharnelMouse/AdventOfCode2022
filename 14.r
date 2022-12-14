x <- strsplit(readLines("14.txt"), " -> ", fixed = TRUE)
parse_line <- function(l) {
  tb <- do.call(rbind, lapply(strsplit(l, ",", fixed = TRUE), strtoi))
  row_parts <- lapply(
    seq_len(nrow(tb) - 1L),
    \(n) {
      steps <- apply(tb[n + 0:1, ], 2, \(z) seq.int(z[1], z[2]))
      do.call(cbind, steps)[-1, , drop = FALSE]
    }
  )
  do.call(rbind, c(list(tb[1, , drop = FALSE]), row_parts))
}
# add 1 to allow for 0 co-ords
coords <- do.call(rbind, lapply(x, parse_line)) + 1L
map_range <- apply(coords, 2, range)
sand_entry <- c(501L, 1L)
initial_sand_path <- cbind(
  sand_entry[1],
  seq.int(sand_entry[2], map_range[1, 2] - 1L)
)
map <- matrix(FALSE, ncol = 1L + diff(map_range[, 1]), nrow = map_range[2, 2])
offset <- 1L - map_range[1, 1]
for (n in seq_len(nrow(coords))) {
  map[coords[n, 2], coords[n, 1] + offset] <- TRUE
}

get_below <- function(pos, map) {
  if (pos[2] == nrow(map))
    return(NULL)
  maprel_x <- pos[1] + offset
  if (maprel_x == 1)
    return(c(FALSE, map[pos[2] + 1L, maprel_x + 0:1]))
  if (maprel_x == ncol(map))
    return(c(map[pos[2] + 1L, maprel_x + (-1):0], FALSE))
  map[pos[2] + 1L, maprel_x + (-1):1]
}

process_grain <- function(initial_path, map) {
  pos <- initial_path[nrow(initial_path), ]
  stopifnot(!map[pos[2], pos[1] + offset])
  out_of_bounds <- pos[2] > nrow(map) ||
    pos[1] + offset < 1L ||
    pos[1] + offset > ncol(map)
  if (out_of_bounds)
    return(list(pos, initial_path, map, FALSE))
  below <- get_below(pos, map)
  if (all(below)) {
    map[pos[2], pos[1] + offset] <- TRUE
    path <- initial_path[-nrow(initial_path), , drop = FALSE]
    continue <- pos[2] != nrow(map)
    return(list(pos, path, map, continue))
  }
  available_shifts <- which(!below) - 2L
  shift <- available_shifts[order(abs(available_shifts), available_shifts)][1]
  return(process_grain(rbind(initial_path, pos + c(shift, 1L)), map))
}

resolve <- function(initial_path, map) {
  path <- initial_path
  curr_map <- map
  continue <- TRUE
  n <- -1L
  while (continue) {
    n <- n + 1L
    nxt <- process_grain(path, curr_map)
    path <- nxt[[2]]
    curr_map <- nxt[[3]]
    continue <- nxt[[4]]
  }
  n
}

resolve(initial_sand_path, map) # part one: 1199

map2 <- cbind(
  matrix(FALSE, nrow = nrow(map) + 1, ncol = -offset),
  rbind(map, FALSE),
  matrix(FALSE, nrow = nrow(map) + 1, ncol = -offset)
)
get_below2 <- function(pos, map) {
  if (pos[2] == nrow(map))
    return(rep(TRUE, 3L))
  if (pos[1] == 0)
    return(c(FALSE, FALSE, map[pos[2] + 1L, pos[1] + 1L]))
  if (pos[1] == -1)
    return(c(FALSE, map[pos[2] + 1L, pos[1] + 0:1]))
  if (pos[1] == ncol(map))
    return(c(map[pos[2] + 1L, pos[1] + (-1):0], FALSE))
  if (pos[1] == ncol(map) + 1L)
    return(c(map[pos[2] + 1L, pos[1] - 1L], FALSE, FALSE))
  if (pos[1] < -1 || pos[1] > ncol(map) + 1)
    return(rep(FALSE, 3L))
  map[pos[2] + 1L, pos[1] + (-1):1]
}

process_grain2 <- function(initial_path, map) {
  pos <- initial_path[nrow(initial_path), ]
  stopifnot(!map[pos[2], pos[1]])
  below <- get_below2(pos, map)
  if (all(below)) {
    map[pos[2], pos[1]] <- TRUE
    path <- initial_path[-nrow(initial_path), , drop = FALSE]
    continue <- any(pos != sand_entry)
    return(list(pos, path, map, continue))
  }
  available_shifts <- which(!below) - 2L
  shift <- available_shifts[order(abs(available_shifts), available_shifts)][1]
  return(process_grain2(rbind(initial_path, pos + c(shift, 1L)), map))
}

resolve2 <- function(initial_path, map) {
  path <- initial_path
  curr_map <- map
  continue <- TRUE
  n <- 0L
  while (continue) {
    n <- n + 1L
    nxt <- process_grain2(path, curr_map)
    path <- nxt[[2]]
    curr_map <- nxt[[3]]
    continue <- nxt[[4]]
  }
  n
}

resolve2(initial_sand_path, map2) # part two: 23925
