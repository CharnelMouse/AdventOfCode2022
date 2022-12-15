x <- substring(readLines("15.txt"), nchar("Sensor at x=") + 1L) |>
  sub(pattern = "x=", replacement = "", fixed = TRUE) |>
  gsub(pattern = "y=", replacement = "", fixed = TRUE) |>
  sub(pattern = ": closest beacon is at ", replacement = ", ", fixed = TRUE)
tb <- read.table(text = x, col.names = c("sx", "sy", "bx", "by"), sep = ",")

dist <- abs(tb$sx - tb$bx) + abs(tb$sy - tb$by)
p1_row <- 2000000L
row_ranges <- dist - abs(tb$sy - p1_row)
intervals <- mapply(
  \(x, r) if (r < 0) integer() else seq.int(x - r, x + r),
  tb$sx,
  row_ranges
)
covered <- Reduce(c, intervals)
beacons <- unique(tb$bx[tb$by == p1_row])
length(setdiff(covered, beacons)) # part one: 5176944 too high

xmax <- ymax <- 4000000L
y <- -1L
found <- FALSE
while (!found) {
  y <- y + 1L
  cat(y, "\r")
  row_ranges <- dist - abs(tb$sy - y)
  used <- which(row_ranges >= 0L)
  left_ends <- pmax(0L, tb$sx[used] - row_ranges[used])
  right_ends <- pmin(xmax, tb$sx[used] + row_ranges[used])
  poss <- list(c(0L, xmax))
  intervals <- Reduce(
    \(intervals, n) {
      if (length(intervals) == 0)
        return(list())
      l <- left_ends[n]
      r <- right_ends[n]
      legal_with_left <- vapply(intervals, \(i) i[1] < l, logical(1))
      legal_with_right <- vapply(intervals, \(i) i[2] > r, logical(1))
      and_left <- lapply(
        intervals[legal_with_left],
        \(i) c(i[1], min(i[2], l - 1L))
      )
      and_right <- lapply(
        intervals[legal_with_right],
        \(i) c(max(i[1], r + 1L), i[2])
      )
      new <- c(and_left, and_right)
      stopifnot(all(lengths(new) == 2))
      new
    },
    seq_along(used),
    init = poss
  )
  if (length(intervals) > 0)
    found <- TRUE
}
stopifnot(length(intervals) == 1, intervals[[1]][1] == intervals[[1]][2])
format(4000000*intervals[[1]][1] + y, scientific = FALSE) # part two: 13350458933732
