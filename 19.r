x <- readLines("19.txt") |>
  sub(pattern = "Blueprint ", replacement = "", fixed = TRUE) |>
  sub(pattern = ": Each ore robot costs ", replacement = ",", fixed = TRUE) |>
  sub(pattern = " ore. Each clay robot costs ", replacement = ",", fixed = TRUE) |>
  sub(pattern = " ore. Each obsidian robot costs ", replacement = ",", fixed = TRUE) |>
  sub(pattern = " ore and ", replacement = ",", fixed = TRUE) |>
  sub(pattern = " clay. Each geode robot costs ", replacement = ",", fixed = TRUE) |>
  sub(pattern = " ore and ", replacement = ",", fixed = TRUE) |>
  sub(pattern = " obsidian.", replacement = "", fixed = TRUE)
dat <- as.matrix(unname(read.table(
  text = x,
  sep = ","
)[, -1]))
blueprints <- apply(
  dat,
  1,
  \(d) matrix(
    c(
      d[1], 0L, 0L,
      d[2], 0L, 0L,
      d[3], d[4], 0L,
      d[5], 0L, d[6]
    ),
    byrow = TRUE,
    ncol = 3L,
    dimnames = list(
      c("ore", "clay", "obsidian", "geode"),
      c("ore", "clay", "obsidian")
    )
  ),
  simplify = FALSE
)

# ore -> ore robot
# ore -> clay robot
# ore + clay -> obsidian robot
# ore + obsidian -> geode robot
# so clay is just for obsidian robots
# example shows this isn't just a bang-bang solution

max_count <- function(
  resources,
  robots,
  blueprint,
  max_costs,
  time,
  geodes,
  actions,
  times,
  best
) {
  if (time == 0L)
    return(geodes)
  if (time == 1L)
    return(geodes + robots[4])
  if (geodes + time*robots[4] + (time*(time - 1L) %/% 2L) <= best)
    return(0L)
  # don't care about resources past maximum amount we could use in time limit
  resources <- pmin(
    resources,
    max_costs*(time - 1L) - robots[-4]*max((time - 2L), 0L)
  )
  # choose next robot to build, not necessarily at this time
  # can only choose a robot we're gathering the resources for
  gathering_for <- apply(
    blueprint,
    1,
    \(b) all(robots[c(b > 0, FALSE)])
  )
  stopifnot(any(gathering_for))
  turns_to_buildable <- apply(
    blueprint[gathering_for, , drop = FALSE],
    1,
    \(b) as.integer(ceiling(max(pmax(b - resources, 0L)[b > 0] / robots[-4][b > 0])))
  )
  available_strategies <- which(gathering_for)[turns_to_buildable < time]
  available_times <- turns_to_buildable[turns_to_buildable < time]
  if (length(available_strategies) == 0) {
    final_geodes <- geodes + time*robots[4]
    return(final_geodes)
  }
  # longest strategies first, since resolve more quickly
  available_strategies <- available_strategies[
    order(available_times, decreasing = TRUE)
  ]
  available_times <- sort(available_times, decreasing = TRUE)
  best <- 0L
  for (n in seq_along(available_strategies)) {
    action <- available_strategies[n]
    timestep <- available_times[n]
    post_robots <- robots
    post_robots[action] <- post_robots[action] + 1L
    post_action_resources <- resources +
      (timestep + 1L)*robots[-4] -
      blueprint[action, ]
    post_action_geodes <- geodes + (timestep + 1L)*robots[4]
    res <- max_count(
      post_action_resources,
      post_robots,
      blueprint,
      max_costs,
      time - (timestep + 1L),
      post_action_geodes,
      c(actions, action),
      c(times, timestep),
      best
    )
    best <- max(best, res)
  }
  best
}
start <- function(blueprint, time) {
  max_costs <- apply(blueprint, 2, max)
  max_count(
    c(0L, 0L, 0L),
    c(1L, 0L, 0L, 0L),
    blueprint,
    max_costs,
    time,
    0L,
    integer(),
    integer(),
    0L
  )
}
res <- vapply(
  blueprints,
  \(b) start(b, 24L)[[1]],
  integer(1)
)
sum(seq_along(blueprints)*res) # part one: 1349
res2 <- vapply(
  blueprints[1:3],
  \(b) start(b, 32L)[[1]],
  integer(1)
)
# prod(res2) # part two: ...
