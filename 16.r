x <- readLines("16.txt") |>
  substring(nchar("Valve ") + 1L) |>
  sub(pattern = " has flow rate=", replacement = ";") |>
  strsplit(";", fixed = TRUE)
valves <- vapply(x, "[", character(1), 1)
n_valves <- length(valves)
rates <- vapply(x, \(sp) strtoi(sp[2]), integer(1))
tos <- lapply(
  x,
  \(sp) {
    txt <- sp[3]
    first_sep <- regexpr(",", txt)
    if (first_sep == -1)
      first_sep <- nchar(txt) + 1L
    strsplit(substring(txt, first_sep - 2L), ", ", fixed = TRUE)[[1]]
  }
)

con <- matrix(
  FALSE,
  nrow = n_valves,
  ncol = n_valves,
  dimnames = rep(list(valves), 2)
)
for (n in seq_len(n_valves)) {
  paths <- match(tos[[n]], valves)
  con[n, paths] <- TRUE
}

relevant <- rates > 0
relevant[match("AA", valves)] <- TRUE
relevant_valves <- valves[relevant]

travel_times <- con[relevant, relevant, drop = FALSE]
travel_times[] <- ifelse(travel_times, 1L, NA_integer_)
t <- 1L
curr <- con
nondiag_complete <- function(mat) {
  diag(mat) <- 0L
  all(!is.na(mat))
}
while (!nondiag_complete(travel_times) && t <= n_valves) {
  t <- t + 1L
  curr <- curr %*% con
  new_times <- t*ifelse(curr[relevant, relevant] > 0, 1L, NA_integer_)
  travel_times[] <- pmin(travel_times, new_times, na.rm = TRUE)
}

reachable <- travel_times[match("AA", relevant_valves), ] > 0
reachable_valves <- relevant_valves[reachable]
reachable_travel_times <- travel_times[reachable, reachable, drop = FALSE]
reachable_rates <- rates[relevant][reachable]
n_reachable <- sum(reachable)

travel_and_open_times <- reachable_travel_times + 1L
time_limit <- 30L

max_route <- function(pos, rem, t) {
  if (t <= 0L || length(rem) == 0L)
    return(0L)
  times <- travel_and_open_times[pos, rem]
  if (all(times >= t - 1L))
    return(0L)
  val <- 0L
  for (target in seq_along(rem)) {
    remtime <- t - times[target]
    if (remtime <= 0L)
      next
    newpos <- rem[target]
    val <- max(
      val,
      remtime*reachable_rates[newpos] + Recall(newpos, rem[-target], remtime)
    )
  }
  val
}
start_point <- match("AA", reachable_valves)
non_start <- seq_len(n_reachable)[-start_point]
max_route(start_point,non_start, 30L) # part one: 1862

elephant <- expand.grid(
  replicate(n_reachable - 1L, c(FALSE, TRUE), simplify = FALSE)
)
combine_max <- function(n) {
  cat(n, "/", nrow(elephant), "\r")
  bools <- unlist(elephant[n, ])
  own <- max_route(start_point, non_start[!bools], 26L)
  el <- max_route(start_point, non_start[bools], 26L)
  own + el
}
scores <- vapply(seq_len(nrow(elephant)), combine_max, integer(1)) # slow
max(scores) # part two: 2422
