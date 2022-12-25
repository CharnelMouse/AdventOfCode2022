x <- strsplit(readLines("25.txt"), "", fixed = TRUE)
parsed <- lapply(x, \(d) strtoi(sub("=", "-2", sub("-", "-1", d))))
lazy_sum <- Reduce(
  \(n, m) {
    n_len <- length(n)
    m_len <- length(m)
    pads <- max(n_len, m_len) - c(n_len, m_len)
    n <- c(rep_len(0L, pads[1]), n)
    m <- c(rep_len(0L, pads[2]), m)
    n + m
  },
  parsed
)
for (digit in length(lazy_sum):2) {
  while (lazy_sum[digit] > 2L) {
    lazy_sum[digit] <- lazy_sum[digit] - 5L
    lazy_sum[digit - 1L] <- lazy_sum[digit - 1L] + 1L
  }
  while (lazy_sum[digit] < -2L) {
    lazy_sum[digit] <- lazy_sum[digit] + 5L
    lazy_sum[digit - 1L] <- lazy_sum[digit - 1L] - 1L
  }
}
stopifnot(lazy_sum[1] >= -2L, lazy_sum[1] <= 2L)
paste(sub("-2", "=", sub("-1", "-", lazy_sum)), collapse = "")
# part one: 2----0=--1122=0=0021
