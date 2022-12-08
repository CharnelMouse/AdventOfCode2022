x <- lapply(strsplit(readLines("08.txt"), "", fixed = TRUE), strtoi)
mat <- do.call(rbind, x)

visline <- function(l) {
  largest_preceding <- Reduce(max, l[-length(l)], init = -1, accumulate = TRUE)
  l > largest_preceding
}
visrev <- function(l) rev(visline(rev(l)))
left <- t(apply(mat, 1, visline))
right <- t(apply(mat, 1, visrev))
up <- apply(mat, 2, visline)
down <- apply(mat, 2, visrev)
visible <- left | right | up | down
sum(visible) # part one: 1733

scoreline <- function(l) {
  # scoring in view direction
  score <- integer(length(l))
  for (n in seq_along(score)) {
    first_block <- which(l[-seq_len(n)] >= l[n])[1]
    score[n] <- if (is.na(first_block))
      length(l) - n
    else
      first_block
  }
  score
}
scorerev <- function(l) rev(scoreline(rev(l)))

leftscore <- t(apply(mat, 1, scorerev))
rightscore <- t(apply(mat, 1, scoreline))
upscore <- apply(mat, 2, scorerev)
downscore <- apply(mat, 2, scoreline)
max(leftscore * rightscore * upscore * downscore) # part two: 284648
