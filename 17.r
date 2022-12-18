x <- readLines("17.txt")
# x <- ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
moves <- strsplit(x, "", fixed = TRUE)[[1]]
n_moves <- length(moves)
shapes <- list(
  hline = matrix(
    c(
      rep(0L, 4),
      0:3
    ),
    ncol = 2
  ),
  cross = matrix(
    c(
      -c(0L, 1L, 1L, 1L, 2L),
      c(1L, 0L, 1L, 2L, 1L)
    ),
    ncol = 2
  ),
  edge = matrix(
    c(
      -c(0L, 1L, 2L, 2L, 2L),
      c(2L, 2L, 0L, 1L, 2L)
    ),
    ncol = 2
  ),
  vline = matrix(
    c(
      -(0:3),
      rep(0L, 4)
    ),
    ncol = 2
  ),
  square = matrix(
    c(
      -rep(0:1, each = 2L),
      rep(0:1, 2)
    ),
    ncol = 2
  )
)
lefts <- lapply(
  shapes,
  \(s) {
    v <- tapply(s[, 2], s[, 1], min)
    matrix(
      c(
        as.integer(names(v)),
        v
      ),
      ncol = 2
    )
  }
)
rights <- lapply(
  shapes,
  \(s) {
    v <- tapply(s[, 2], s[, 1], max)
    matrix(
      c(
        as.integer(names(v)),
        v
      ),
      ncol = 2
    )
  }
)
downs <- lapply(
  shapes,
  \(s) {
    v <- tapply(s[, 1], s[, 2], min)
    matrix(
      c(
        v,
        as.integer(names(v))
      ),
      ncol = 2
    )
  }
)
shape_dims <- vapply(shapes, apply, integer(2), 2, max)

any_in_df <- function(df1, df2) {
  if (nrow(df1) == 0L || nrow(df2) == 0L)
    return(FALSE)
  s <- split(df1[, 2], df1[, 1])
  any(vapply(
    seq_along(s),
    \(n) {
      v <- as.integer(names(s)[n])
      any(is.element(s[[n]], df2[df2[, 1] == v, 2, drop = FALSE]))
    },
    logical(1)
  ))
}

next_shape <- function(tower, shape_index, move_index, height) {
  highest_settled <- if (nrow(tower) == 0L)
    0L
  else
    max(tower[, 1])
  shape <- shapes[[shape_index + 1L]]
  left <- lefts[[shape_index + 1L]]
  right <- rights[[shape_index + 1L]]
  down <- downs[[shape_index + 1L]]
  lowest_shape <- min(shape[, 1])
  pos <- c(highest_settled - lowest_shape + 4L, 3L)
  finished <- FALSE
  while (!finished) {
    test_area <- tower[
      tower[, 1] <= pos[1] & tower[, 1] >= pos[1] - 5L,
      ,
      drop = FALSE
    ]
    new_pos <- pos
    new_pos[2] <- switch(
      moves[move_index + 1L],
      "<" = new_pos[2] - 1L,
      ">" = new_pos[2] + 1L
    )
    new_check <- switch(
      moves[move_index + 1L],
      "<" = t(apply(left, 1, "+", new_pos)),
      ">" = t(apply(right, 1, "+", new_pos))
    )
    if (
      all(new_check[, 2] >= 1L & new_check[, 2] <= 7L) &&
      !any_in_df(new_check, test_area)
    )
      pos <- new_pos
    move_index <- (move_index + 1L) %% n_moves
    new_pos <- pos
    new_pos[1] <- new_pos[1] - 1L
    new_down <- t(apply(down, 1, "+", new_pos))
    if (
      any(new_down[, 1] <= 0L) ||
      any_in_df(new_down, test_area)
    )
      finished <- TRUE
    else
      pos <- new_pos
  }
  resting_positions <- t(apply(shape, 1, "+", pos))
  if (any_in_df(resting_positions, test_area))
    stop(paste(
      print(resting_positions),
      print(test_area)
    ))
  tower <- rbind(resting_positions, tower)
  row_counts <- table(tower[, 1])
  filled_rows <- which(row_counts == 7)
  if (length(filled_rows) > 0) {
    highest <- as.integer(names(row_counts))[max(filled_rows)]
    height <- height + highest
    tower[, 1] <- tower[, 1] - highest
    tower <- tower[tower[, 1] > 0, , drop = FALSE]
  }
  stopifnot(ncol(tower) == 2)
  list(
    tower = tower,
    shape_index = (shape_index + 1L) %% length(shapes),
    move_index = move_index,
    height = height
  )
}
update <- function(state, n) {
  cat(n, "\r")
  cache <- state$cache
  cache_indices <- state$cache_indices
  cache_heights <- state$cache_heights
  cached_state <- state[1:4]
  cached_settled <- cached_state[[1]]
  cached_settled[] <- as.integer(cached_settled)
  cache_index <- state$shape_index*n_moves + state$move_index + 1L
  cache_match <- match(list(cached_settled), cache[[cache_index]])
  if (!is.na(cache_match)) {
    tower <- cache[[cache_index]][[cache_match]]
    first_index <- cache_indices[[cache_index]][[cache_match]]
    first_height <- cache_heights[[cache_index]][[cache_match]]
    stopifnot(identical(cached_settled, tower))
    return(list(
      tower = cached_settled,
      iters = c(first_index, n),
      heights = c(first_height, state$height),
      cache = cache,
      cache_indices = cache_indices,
      cache_heights = cache_heights
    ))
  }

  new_state <- do.call(next_shape, state[1:4])
  cache[[cache_index]] <- c(
    cache[[cache_index]],
    list(cached_settled)
  )
  cache_indices[[cache_index]] <- c(cache_indices[[cache_index]], n)
  cache_heights[[cache_index]] <- c(cache_heights[[cache_index]], state$height)
  c(
    new_state,
    list(
      cache = cache,
      cache_indices = cache_indices,
      cache_heights = cache_heights
    )
  )
}
iter <- 0L
state <- list(
  tower = matrix(integer(), ncol = 2),
  shape_index = 0L,
  move_index = 0L,
  height = 0L,
  cache = rep(list(list()), n_moves*length(shapes)),
  cache_indices = rep(list(integer()), n_moves*length(shapes)),
  cache_heights = rep(list(integer()), n_moves*length(shapes))
)
while (iter < 2022L) {
  cat(iter, "\r")
  if (length(state) == 6)
    break
  state <- update(state, iter)
  iter <- iter + 1L
}
stopifnot(length(state) == 6) # assuming cycle occurs in part one

resolve_state <- function(state, iter) {
  cycle_length <- diff(state$iters)
  additional_iters <- iter - state$iters[2]
  additional_cycles <- additional_iters %/% cycle_length
  additional_steps <- additional_iters %% cycle_length
  final_cycle_height <- state$heights[2] + additional_cycles*diff(state$heights)
  step_location <- vapply(
    state$cache_indices,
    match,
    integer(1),
    x = state$iters[1] + additional_steps
  )
  step_lookup <- c(which(!is.na(step_location)), na.omit(unlist(step_location)))
  step_height <- state$cache_heights[[step_lookup[1]]][[step_lookup[2]]]
  final_height <- final_cycle_height + step_height - state$heights[1]
  step_tower <- state$cache[[step_lookup[1]]][[step_lookup[2]]]
  final_tower_height <- if (nrow(step_tower) == 0)
    0L
  else
    max(step_tower[, 1])
  final_height + final_tower_height
}
resolve_state(state, 2022L) # part one: 3151
format(resolve_state(state, 1000000000000), scientific = FALSE) # part two: 1560919540245
