x <- scan("09.txt", list(character(1), integer(1)), quiet = TRUE)
moves <- rep(x[[1]], x[[2]])
move_vecs <- list(
  c(1L, 0L),
  c(-1L, 0L),
  c(0L, 1L),
  c(0L, -1L)
)[match(moves, c("R", "L", "U", "D"))]

update_tail <- function(pos, new_head) {
  diff <- new_head - pos
  if (any(abs(diff) > 1L))
    pos + as.integer(sign(diff)) # leaving out as.integer causes errors
  else
    pos
}
next_knot <- function(pos_history) {
  Reduce(update_tail, pos_history, init = c(0L, 0L), accumulate = TRUE)[-1]
}

head_pos <- Reduce("+", move_vecs, accumulate = TRUE)
knots <- Reduce(\(pos, n) next_knot(pos), 1:9, init = head_pos, accumulate = TRUE)
knot_dfs <- lapply(
  seq_along(knots),
  \(n) {
    base <- setNames(data.frame(do.call(rbind, knots[[n]])), c("x", "y"))
    cbind(
      n = factor(n, 1:10),
      step = seq_len(length(moves)),
      base
    )
  }
)
all_knots <- do.call(rbind, knot_dfs)
all_knots <- all_knots[order(all_knots$step, all_knots$n), ]
max_n <- nlevels(all_knots$n)
segments <- cbind(
  subset(all_knots, n != max_n),
  setNames(subset(all_knots, n != 1, c("x", "y")), c("xend", "yend"))
)

library(ggplot2)
library(gganimate)
library(glue)
max_step <- 100L
gif_duration <- max_step/4L
anim_twoknots <- ggplot(
  subset(segments, step <= max_step & as.integer(n) <= 2),
  aes(x = x, y = y, xend = xend, yend = yend, colour = n)
) +
  geom_segment() +
  transition_states(step) +
  ggtitle('Step {closest_state}') +
  theme(panel.grid.minor = element_blank())
gif_twoknots <- animate(
  anim_twoknots,
  renderer = gifski_renderer(),
  fps = 20,
  nframes = gif_duration*20,
  start_pause = 20
)
anim_allknots <- ggplot(
  subset(segments, step <= 100),
  aes(x = x, y = y, xend = xend, yend = yend, colour = n)
) +
  geom_segment() +
  transition_states(step) +
  ggtitle('Step {closest_state}') +
  theme(panel.grid.minor = element_blank())
gif_allknots <- animate(
  anim_allknots,
  renderer = gifski_renderer(),
  fps = 20,
  nframes = gif_duration*20,
  start_pause = 20
)
anim_save("09p1.gif", gif_twoknots)
anim_save("09p2.gif", gif_allknots)
