x <- scan("09.txt", list(character(1), integer(1)), quiet = TRUE)
moves <- rep(x[[1]], x[[2]])
move_vecs <- list(
  c(1L, 0L),
  c(-1L, 0L),
  c(0L, 1L),
  c(0L, -1L)
)[
  match(moves, c("R", "L", "U", "D"))
]

head_pos <- Reduce("+", move_vecs, accumulate = TRUE)
update_tail <- function(pos, new_head) {
  diff <- new_head - pos
  if (any(abs(diff) > 1L)) {
    pos + as.integer(sign(diff)) # leaving out as.integer causes errors
  }else
    pos
}
tail_pos <- Reduce(update_tail, head_pos, init = c(0L, 0L), accumulate = TRUE)[-1]
length(unique(tail_pos)) # part one: 5735

next_knot <- function(pos_history) {
  Reduce(update_tail, pos_history, init = c(0L, 0L), accumulate = TRUE)[-1]
}
stopifnot(identical(next_knot(head_pos), tail_pos))
pos_history <- tail_pos
for (knot in 2:9) {
  pos_history <- next_knot(pos_history)
}
length(unique(pos_history)) # part two: 2478
