# x <- strsplit("#.######
# #>>.<^<#
# #.<..<<#
# #>v.><>#
# #<^v^^>#
# ######.#", "\n")[[1]]
x <- readLines("24.txt")
width <- nchar(x[1]) - 2L
height <- length(x) - 2L
size <- width*height
stopifnot(identical(x[1L], paste(c("#", ".", rep("#", width)), collapse = "")))
stopifnot(identical(x[height + 2L], paste(c(rep("#", width), ".", "#"), collapse = "")))
map <- do.call(rbind, strsplit(x[-c(1L, height + 2L)], "", fixed = TRUE))[
  , -c(1L, width + 2L)
]
stopifnot(identical(c(height, width), dim(map)))

flat_map <- c(".", map, ".") # top to bottom, left to right
rights <- which(flat_map == ">") - 2L
lefts <- which(flat_map == "<") - 2L
ups <- which(flat_map == "^") - 2L
downs <- which(flat_map == "v") - 2L
start <- -1L
goal <- size

safe <- function(positions, time) {
  right_pos <- rights %% height + height*((rights %/% height + time) %% width)
  left_pos <- lefts %% height + height*((lefts %/% height - time) %% width)
  up_pos <- height*(ups %/% height) + ((ups %% height - time) %% height)
  down_pos <- height*(downs %/% height) + ((downs %% height + time) %% height)
  !is.element(positions, c(right_pos, left_pos, up_pos, down_pos))
}

neighbours <- function(current, time) {
  poss <- current
  if (current %% height > 0L)
    poss <- c(poss, current - 1L)
  if (current %% height < height - 1L)
    poss <- c(poss, current + 1L)
  if (current %/% height > 0L)
    poss <- c(poss, current - height)
  if (current %/% height < width - 1L)
    poss <- c(poss, current + height)
  if (current == -1L)
    poss <- c(-1L, 0L)
  if (current == 0L)
    poss <- c(poss, -1L)
  if (current == goal - 1L)
    poss <- c(poss, goal)
  if (current == goal)
    poss <- c(goal, goal - 1L)
  stopifnot(all(poss >= -1L & poss <= size))
  poss[safe(poss, time)]
}

heuristic <- function(pos, flip = FALSE) {
  if (flip)
    pos <- (size - 1L) - pos
  if (pos == start)
    return(height + width)
  if (pos == goal)
    return(0L)
  row <- pos %% height
  col <- pos %/% height
  (height - row) + (width - 1L - col)
}

# from day 12
astar <- function(starts, end, start_time = 0L, flip = FALSE) {
  open <- cbind(starts, start_time %% size)
  closed <- matrix(integer(), ncol = 2L)
  costs <- array(NA_integer_, dim = c(size + 2L, size))
  costs[starts + 2L, start_time %% size + 1L] <- 0L
  current <- which.min(apply(
    open,
    1,
    \(vec) costs[vec[1] + 2L, start_time %% size + 1L] + heuristic(vec[1], flip)
  ))
  stopifnot(length(current) == 1L)
  current_val <- open[current, ]
  while (current_val[1] != end) {
    current_dist <- costs[current_val[1] + 2L, current_val[2] + 1]
    stopifnot((current_dist + start_time) %% size == current_val[2])
    cat(
      length(closed),
      (size + 2L)*size,
      current_dist,
      current_dist + heuristic(current_val[1], flip),
      "\r"
    )
    open <- open[-current, , drop = FALSE]
    closed <- rbind(closed, current_val)
    ns <- lapply(
      neighbours(current_val[1], current_val[2] + 1L),
      c,
      (current_val[2] + 1L) %% size
    )
    for (n in ns) {
      cost <- current_dist + 1L
      pos_eq <- open[, 1] == n[1]
      per_eq <- open[, 2] == n[2]
      n_index <- match(TRUE, pos_eq & per_eq)
      if (n[1] < -1L || n[1] > size || n[2] < 0L || n[2] >= size)
        stop(toString(n), " size ", size)
      cost_entry <- costs[n[1] + 2L, n[2] + 1L]
      if (!is.na(n_index) && (is.na(cost_entry) || cost < cost_entry))
        open <- open[-n_index, , drop = FALSE]
      pos_eq2 <- closed[, 1] == n[1]
      per_eq2 <- closed[, 2] == n[2]
      n_index2 <- match(TRUE, pos_eq2 & per_eq2)
      if (!is.na(n_index2) && (is.na(cost_entry || cost < cost_entry)))
        closed <- closed[-n_index2, , drop = FALSE]
      if (is.na(n_index) && is.na(n_index2)) {
        costs[n[1] + 2L, n[2] + 1L] <- cost
        open <- rbind(open, n)
      }
    }
    current <- which.min(apply(
      open,
      1,
      \(vec) costs[vec[1] + 2L, vec[2] + 1L] + heuristic(vec[1], flip)
    ))
    if (length(current) == 0L)
      stop(
        "empty queue\nvisited: ",
        toString(sort(unique(vapply(closed, "[", integer(1), 1))))
      )
    current_val <- open[current, ]
  }
  list(
    costs[current_val[1] + 2L, current_val[2] + 1L],
    current_val,
    costs,
    open,
    closed
  )
}
res <- astar(start, goal)
res[[1]] # part one: 292

res2 <- astar(goal, start, res[[1]], flip = TRUE)
res3 <- astar(start, goal, res[[1]] + res2[[1]])
res[[1]] + res2[[1]] + res3[[1]] # part two: 816
