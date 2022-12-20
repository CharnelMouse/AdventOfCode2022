x <- scan("20.txt", integer(), quiet = TRUE)
len <- length(x)
zero_pos <- match(0L, x) - 1L

# dancing links
positions <- seq_along(x) - 1L
left <- (positions - 1L) %% len
right <- (positions + 1L) %% len
link <- function(start, n, left, right) {
  while (n >= len)
    n <- n - len
  while (n <= -len)
    n <- n + len
  if (n == 0L)
    return(start)
  links <- if (n < 1) left else right
  pos <- start
  for (m in seq_len(abs(n))) {
    pos <- links[pos + 1L]
  }
  pos
}
link_no_self <- function(start, n, left, right) {
  while (n >= len - 1L)
    n <- n - (len - 1L)
  while (n <= -(len - 1L))
    n <- n + (len - 1L)
  if (n == 0L)
    return(start)
  links <- if (n < 1) left else right
  pos <- start
  for (m in seq_len(abs(n))) {
    pos <- links[pos + 1L]
    if (pos == start)
    pos <- links[start + 1L]
  }
  pos
}
move <- function(state, n, x) {
  stopifnot(n >= 0L && n <= len - 1L)
  left <- state[[1]]
  right <- state[[2]]
  from_val <- x[n + 1L]
  if (from_val %% (len - 1L) == 0L)
    return(list(left, right))
  pred <- link(n, -1L, left, right)
  succ <- link(n, 1L, left, right)
  if (from_val > 0L) {
    new_succ <- link_no_self(n, 1L + from_val, left, right)
    new_pred <- link_no_self(new_succ, -1L, left, right)
  }else{
    new_pred <- link_no_self(n, -1L + from_val, left, right)
    new_succ <- link_no_self(new_pred, 1L, left, right)
  }
  # replacing values n, n, new_succ, new_pred, succ, and pred, respectively
  right[pred + 1L] <- succ
  left[succ + 1L] <- pred
  right[new_pred + 1L] <- n
  left[new_succ + 1L] <- n
  right[n + 1L] <- new_succ
  left[n + 1L] <- new_pred
  if (anyDuplicated(left) || anyDuplicated(right)) {
    stop("duplicated results\nn: ", n, "; from_val: ", from_val, "; mod: ", from_val %% (len - 1L))
  }
  list(left, right)
}
mix <- function(left, right, x) {
  x <- x %% (len - 1L)
  x[x > (len - 1L)/2] <- x[x > (len - 1L)/2] - (len - 1L)
  Reduce(\(s, n) move(s, n, x), seq_len(length(x)) - 1L, init = list(left, right))
}
# reproduce positions for manual checking
reproduce <- function(right) {
  rem <- length(right)
  ind <- 0L
  res <- numeric()
  for (r in seq_len(rem)) {
    res <- c(res, ind)
    ind <- right[ind + 1L]
  }
  res
}
new_state <- mix(left, right, x)
new_left <- new_state[[1]]
new_right <- new_state[[2]]
res <- vapply(
  c(1000L, 2000L, 3000L),
  \(n) link(zero_pos, n, new_left, new_right),
  integer(1)
)
sum(x[1L + res]) # part one: 2203

x2 <- x*811589153
new_state <- Reduce(
  \(state, n) mix(state[[1]], state[[2]], x2),
  seq_len(10L),
  init = list(left, right)
)
new_left <- new_state[[1]]
new_right <- new_state[[2]]
res <- vapply(
  c(1000L, 2000L, 3000L),
  \(n) link(zero_pos, n, new_left, new_right),
  integer(1)
)
format(sum(x2[1L + res]), scientific = FALSE) # part two: 6641234038999
