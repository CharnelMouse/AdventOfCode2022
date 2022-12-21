x <- readLines("21.txt") |>
  strsplit(" ", fixed = TRUE)
stopifnot(all(lengths(x) %in% c(2L, 4L)))

vars <- vapply(x, \(el) substr(el[1], 1L, nchar(el[1]) - 1L), character(1))
resolved <- rep(FALSE, length(x))
vals <- rep(NA_integer_, length(x))
single_in <- lengths(x) == 2L
single_literal <- vapply(x[single_in], \(el) strtoi(el[2]), integer(1))
stopifnot(!anyNA(single_literal))
resolved[single_in] <- TRUE
vals[single_in] <- single_literal
in1 <- vapply(x, "[", character(1), 2L)
stopifnot(all(is.na(strtoi(in1[!single_in]))))
in_match1 <- match(in1, vars)
in2 <- vapply(x, "[", character(1), 4L)
stopifnot(all(is.na(strtoi(in2[!single_in]))))
in_match2 <- match(in2, vars)
ops <- vapply(x, "[", character(1), 3L)

start_resolved <- resolved
start_vals <- vals

root_index <- match("root", vars)
while (!resolved[root_index]) {
  i1 <- match(in1[!resolved], vars)
  present1 <- resolved[i1]
  i2 <- match(in2[!resolved], vars)
  present2 <- resolved[i2]
  able <- present1 & present2
  able_ops <- ops[!resolved][able]
  able_num1 <- vals[i1[able]]
  able_num2 <- vals[i2[able]]
  able_num <- mapply(\(f, n1, n2) get(f)(n1, n2), able_ops, able_num1, able_num2)
  vals[!resolved][able] <- able_num
  resolved[!resolved][able] <- TRUE
}
format(vals[root_index], scientific = FALSE) # part one: 331120084396440

add_polys <- function(p1, p2) {
  length(p1) <- length(p2) <- max(length(p1), length(p2))
  p1[is.na(p1)] <- 0L
  p2[is.na(p2)] <- 0L
  p1 + p2
}
mult_polys <- function(p1, p2) {
  mat <- outer(p1, p2, "*")
  len <- length(p1) + length(p2) - 1L
  res <- integer(len)
  for (n in seq_len(len)) {
    for (x in seq_len(min(length(p1), n))) {
      y <- n + 1L - x
      if (y <= length(p2))
        res[n] <- res[n] + mat[x, y]
    }
  }
  res
}
gcd_int <- function(x, y) {
  if (y == 1L)
    return(1L)
  r <- x %% y
  if (r == 0L)
    return(y)
  Recall(y, r)
}
gcd <- function(x, y) {
  stopifnot(!is.na(x), !is.na(y))
  ax <- abs(x)
  ay <- abs(y)
  u <- max(ax, ay)
  l <- min(ax, ay)
  gcd_int(u, l)
}

self_index <- match("humn", vars)
resolved <- start_resolved
numerators <- as.list(start_vals)
denominators <- rep(list(NA_integer_), length(x))
denominators[start_resolved] <- 1L
ops2 <- ops
ops2[root_index] <- "=="
numerators[[self_index]] <- c(0L, 1L)
denominators[[self_index]] <- 1L
resolved[self_index] <- TRUE
able <- TRUE
while (!resolved[root_index]) {
  i1 <- match(in1[!resolved], vars)
  present1 <- resolved[i1]
  i2 <- match(in2[!resolved], vars)
  present2 <- resolved[i2]
  able <- present1 & present2
  able_ops <- ops2[!resolved][able]
  able_num1 <- numerators[i1[able]]
  able_num2 <- numerators[i2[able]]
  able_den1 <- denominators[i1[able]]
  able_den2 <- denominators[i2[able]]
  able_num <- Map(
    \(f, n1, n2, d1, d2) {
      switch(
        f,
        `+` = add_polys(mult_polys(n1, d2), mult_polys(n2, d1)),
        `-` = add_polys(mult_polys(n1, d2), -mult_polys(n2, d1)),
        `*` = mult_polys(n1, n2),
        `/` = mult_polys(n1, d2),
        `==` = NA_integer_,
        stop("unrecognised op")
      )
    },
    able_ops,
    able_num1,
    able_num2,
    able_den1,
    able_den2
  )
  able_den <- Map(
    \(f, n1, n2, d1, d2) {
      switch(
        f,
        `+` = mult_polys(d1, d2),
        `-` = mult_polys(d1, d2),
        `*` = mult_polys(d1, d2),
        `/` = mult_polys(d1, n2),
        `==` = NA_integer_,
        stop("unrecognised op")
      )
    },
    able_ops,
    able_num1,
    able_num2,
    able_den1,
    able_den2
  )
  non_root <- which(!resolved)[able] != root_index
  reduced <- Map(
    \(n, d) {
      g <- Reduce(gcd, c(n, d))
      r1 <- n %/% g
      if (r1 <= .Machine$integer.max && r1 >= -.Machine$integer.max)
        r1 <- as.integer(r1)
      r2 <- d %/% g
      if (r2 <= .Machine$integer.max && r2 >= -.Machine$integer.max)
        r2 <- as.integer(r2)
      list(r1, r2)
    },
    able_num[non_root],
    able_den[non_root]
  )
  able_num[non_root] <- lapply(reduced, "[[", 1L)
  able_den[non_root] <- lapply(reduced, "[[", 2L)
  stopifnot(length(numerators[!resolved][able]) == length(able_num))
  stopifnot(length(denominators[!resolved][able]) == length(able_den))
  numerators[!resolved][able] <- able_num
  denominators[!resolved][able] <- able_den
  resolved[!resolved][able] <- TRUE
}

num1 <- numerators[[match(in1[root_index], vars)]]
num2 <- numerators[[match(in2[root_index], vars)]]
den1 <- denominators[[match(in1[root_index], vars)]]
den2 <- denominators[[match(in2[root_index], vars)]]
poly1 <- mult_polys(num1, den2)
poly2 <- mult_polys(den1, num2)
stopifnot(length(poly1)*length(poly2) == 2L)
if (length(poly1) == 2) {
  larger <- poly1
  smaller <- poly2
}else{
  larger <- poly2
  smaller <- poly1
}
constant <- smaller[1] - larger[1]
multiplier <- larger[2]
stopifnot(constant %% multiplier == 0L)
format(constant %/% multiplier, scientific = FALSE) # part two: 3378273370680
