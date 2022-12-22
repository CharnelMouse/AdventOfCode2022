x <- readLines("22.txt")
map <- x[seq_len(length(x) - 2L)]
width <- max(nchar(map))
fill <- vapply(
  width - nchar(map),
  \(n) paste(rep(" ", n), collapse = ""),
  character(1)
)
map <- paste0(map, fill)
map <- do.call(rbind, strsplit(map, "", fixed = TRUE))
start <- match(".", map[1, ])
start_state <- c(1L, start, 0L)

moves <- x[length(x)]
mlen <- nchar(moves)
split_moves <- strsplit(moves, "", fixed = TRUE)[[1]]
split_moves_int <- strtoi(split_moves)
change <- cumsum(c(
  FALSE,
  xor(is.na(split_moves_int[-mlen]), is.na(split_moves_int[-1L]))
))
parsed_moves <- tapply(split_moves, change, paste, collapse = "")
num_moves <- strtoi(parsed_moves)

wrap_map <- function(state, map) {
  switch(
    state[3] + 1L,
    c(state[1], Position(\(char) char != " ", map[state[1], ]), state[3]),
    c(Position(\(char) char != " ", map[, state[2]]), state[2], state[3]),
    c(state[1], Position(\(char) char != " ", map[state[1], ], right = TRUE), state[3]),
    c(Position(\(char) char != " ", map[, state[2]], right = TRUE), state[2], state[3])
  )
}

move_internal <- function(state, n, wrapper) {
  if (!all(class(state) == "integer", !anyNA(state), !is.na(n), length(state) == 3L))
    stop("state: ", toString(state))
  if (n == 0L)
    return(state)
  destination <- switch(
    state[3] + 1L,
    c(state[1], state[2] + n),
    c(state[1] + n, state[2]),
    c(state[1], state[2] - n),
    c(state[1] - n, state[2])
  )
  stopifnot(length(destination) == 2L)
  path <- switch(
    state[3] + 1L,
    c(
      map[state[1], state[2] + seq_len(min(n, ncol(map) - state[2]))],
      if (n > ncol(map) - state[2]) " "
    ),
    c(
      map[state[1] + seq_len(min(n, nrow(map) - state[1])), state[2]],
      if (n > nrow(map) - state[1]) " "
    ),
    c(
      map[state[1], state[2] - seq_len(min(n, state[2] - 1L))],
      if (n > state[2] - 1L) " "
    ),
    c(
      map[state[1] - seq_len(min(n, state[1] - 1L)), state[2]],
      if (n > state[1] - 1L) " "
    )
  )
  first_wall <- match("#", path)
  first_void <- match(" ", path)
  if (is.na(first_wall) && is.na(first_void))
    return(c(destination, state[3]))
  if (is.na(first_void) || isTRUE(first_wall < first_void))
    return(c(
      switch(
        state[3] + 1L,
        c(state[1], state[2] + first_wall - 1L),
        c(state[1] + first_wall - 1L, state[2]),
        c(state[1], state[2] - (first_wall - 1L)),
        c(state[1] - (first_wall - 1L), state[2])
      ),
      state[3]
    ))
  # move into void
  state_before_void <- switch(
    state[3] + 1L,
    c(state[1], state[2] + first_void - 1L, state[3]),
    c(state[1] + first_void - 1L, state[2], state[3]),
    c(state[1], state[2] - (first_void - 1L), state[3]),
    c(state[1] - (first_void - 1L), state[2], state[3])
  )
  wrap_to <- wrapper(state_before_void, map)
  stopifnot(length(wrap_to) == 3L)
  if (map[wrap_to[1], wrap_to[2]] == "#") {
    return(state_before_void)
  }
  stopifnot(length(wrap_to) == 3L)
  moves_left <- n - first_void
  Recall(wrap_to, moves_left, wrapper)
}
move <- function(state, n, wrapper) {
  if (!all(class(state) == "integer", length(state) == 3L, !anyNA(state)))
    stop("move error. state: ", toString(state))
  num <- num_moves[n]
  if (is.na(num)) {
    state[3] <- state[3] + ifelse(parsed_moves[n] == "R", 1L, -1L)
    state[3] <- state[3] %% 4L
    return(state)
  }
  res <- move_internal(state, num, wrapper)
  if (!all(class(res) == "integer", length(res) == 3L, !anyNA(res)))
    stop("move_from_interval error. res: ", toString(res))
  res
}

p1 <- Reduce(
  \(s, n) move(s, n, wrap_map),
  seq_along(num_moves),
  init = start_state
)
sum(p1*c(1000L, 4L, 1L)) # part one: 155060

mapdup <- map
cube_size <- 50L
cube_corners <- matrix(integer(), ncol = 4)
while (!all(mapdup == " ")) {
  first_point <- arrayInd(min(match(c("#", "."), mapdup), na.rm = TRUE), dim(map))
  cube_corners <- rbind(cube_corners, c(first_point, first_point + cube_size - 1L))
  cube <- mapdup[
    first_point[1] + seq.int(0L, cube_size - 1L),
    first_point[2] + seq.int(0L, cube_size - 1L)
  ]
  stopifnot(!any(cube == " "))
  mapdup[
    first_point[1] + seq.int(0L, cube_size - 1L),
    first_point[2] + seq.int(0L, cube_size - 1L)
  ] <- " "
}
stopifnot(nrow(cube_corners) == 6L)
plot(cube_corners[, 2:1], xlim = c(1, max(cube_corners)), ylim = c(1, max(cube_corners)))
points(cube_corners[, 4:3], col = "red")
cube_orderings <- apply(cube_corners[, 1:2], 2, \(x) match(x, sort(unique(x))))
cube_edge_pairs <- matrix(integer(), ncol = 4)
cube_edge_from_sides <- matrix(integer(), ncol = 4)
cube_edge_to_sides <- matrix(integer(), ncol = 4)
for (cube in seq_len(6)) {
  given_right <- match(
    TRUE,
    apply(
      cube_orderings[, 1:2],
      1,
      identical,
      cube_orderings[cube, 1:2] + c(0L, 1L)
    )
  )
  given_down <- match(
    TRUE,
    apply(
      cube_orderings[, 1:2],
      1,
      identical,
      cube_orderings[cube, 1:2] + c(1L, 0L)
    )
  )
  given_left <- match(
    TRUE,
    apply(
      cube_orderings[, 1:2],
      1,
      identical,
      cube_orderings[cube, 1:2] - c(0L, 1L)
    )
  )
  given_up <- match(
    TRUE,
    apply(
      cube_orderings[, 1:2],
      1,
      identical,
      cube_orderings[cube, 1:2] - c(1L, 0L)
    )
  )
  cube_edge_pairs <- rbind(
    cube_edge_pairs,
    c(given_right, given_down, given_left, given_up)
  )
  cube_edge_from_sides <- rbind(
    cube_edge_from_sides,
    0:3
  )
  cube_edge_to_sides <- rbind(
    cube_edge_to_sides,
    c(2L, 3L, 0L, 1L)
  )
}
while (anyNA(cube_edge_pairs)) {
  for (dir1 in 1:4) {
    nonmissing1 <- which(!is.na(cube_edge_pairs[, dir1]))
    for (cube1 in nonmissing1) {
      # cube1 in direction dir1 goes to cube_edge_pairs[cube1, dir1]
      # check orthogonal directions, to make L shape
      cube2 <- cube_edge_pairs[cube1, dir1]
      dirinv1 <- cube_edge_to_sides[cube1, dir1] + 1L
      next_dir <- setdiff(
        which(!is.na(cube_edge_pairs[cube2, ])),
        (c(dirinv1 - 1L, dirinv1 + 1L) %% 4L + 1L)
      )
      for (dir2 in next_dir) {
        cube3 <- cube_edge_pairs[cube2, dir2]
        dirinv2 <- cube_edge_to_sides[cube2, dir2] + 1L
        # cube1 cube2 cube3 are in L-shape, so cube1 and cube3 are also neighbours
        spin1 <- dir2 - dirinv1
        cube1_to_cube3 <- (dir1 - spin1 - 1L) %% 4L + 1L
        cube3_to_cube1 <- (dirinv2 + spin1 - 1L) %% 4L + 1L
        if (
          is.na(cube_edge_pairs[cube1, cube1_to_cube3]) ||
          is.na(cube_edge_pairs[cube3, cube3_to_cube1])
        ) {
          cat("setting cubes", cube1, cube3, "\n")
          stopifnot(is.element(
            cube_edge_pairs[cube1, cube1_to_cube3],
            c(NA_integer_, cube3)
          ))
          stopifnot(is.element(
            cube_edge_pairs[cube3, cube3_to_cube1],
            c(NA_integer_, cube1)
          ))
          cube_edge_pairs[cube1, cube1_to_cube3] <- cube3
          cube_edge_to_sides[cube1, cube1_to_cube3] <- cube3_to_cube1 - 1L
          cube_edge_pairs[cube3, cube3_to_cube1] <- cube1
          cube_edge_to_sides[cube3, cube3_to_cube1] <- cube1_to_cube3 - 1L
        }
      }
    }
  }
}

wrap_cube <- function(state, map) {
  cube <- which(
    state[1] >= cube_corners[, 1] &
      state[2] >= cube_corners[, 2] &
      state[1] <= cube_corners[, 3] &
      state[2] <= cube_corners[, 4]
  )
  stopifnot(length(cube) == 1L)
  dir1 <- state[3]
  next_cube <- cube_edge_pairs[cube, dir1 + 1L]
  next_cube_side <- cube_edge_to_sides[cube, dir1 + 1L]
  new_dir <- (next_cube_side + 2L) %% 4L
  carryover <- switch(
    dir1 + 1L,
    state[1] - cube_corners[cube, 1L],
    cube_corners[cube, 4L] - state[2],
    cube_corners[cube, 3L] - state[1],
    state[2] - cube_corners[cube, 2L]
  )
  # if coming in on left, new pos component is left edge position, etc.
  new <- cube_corners[next_cube, switch(next_cube_side + 1L, 4L, 3L, 2L, 1L)]
  # add carryover to starting corner
  carried <- switch(
    new_dir + 1L,
    carryover + cube_corners[next_cube, 1L], # from left
    cube_corners[next_cube, 4L] - carryover,  # from top
    cube_corners[next_cube, 3L] - carryover, # from right
    carryover + cube_corners[next_cube, 2L] # from bottom
  )
  res <- if (new_dir %% 2L == 0L)
    c(carried, new, new_dir) # horizontal entry, carry over is first dim
  else
    c(new, carried, new_dir)
  if (!all(
    res[1] >= cube_corners[next_cube, 1],
    res[1] <= cube_corners[next_cube, 3],
    res[2] >= cube_corners[next_cube, 2],
    res[2] <= cube_corners[next_cube, 4]
  ))
    stop(paste(
      "illegal wrap.\ncube corners:",
      toString(cube_corners[cube, ]),
      "entering side",
      next_cube_side,
      "new dir",
      new_dir,
      "initial state",
      toString(state),
      "new",
      new,
      "carryover",
      carryover,
      "next_cube corners",
      toString(cube_corners[next_cube,]),
      "carried",
      carried,
      "end state",
      toString(res),
      sep = "\n"
    ))
  res
}

p2 <- Reduce(
  \(s, n) move(s, n, wrap_cube),
  seq_along(num_moves),
  init = start_state
)
sum(p2*c(1000L, 4L, 1L)) # part two: 3479
