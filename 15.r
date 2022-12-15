x <- substring(readLines("15.txt"), nchar("Sensor at x=") + 1L) |>
  sub(pattern = "x=", replacement = "", fixed = TRUE) |>
  gsub(pattern = "y=", replacement = "", fixed = TRUE) |>
  sub(pattern = ": closest beacon is at ", replacement = ", ", fixed = TRUE)
tb <- read.table(text = x, col.names = c("sx", "sy", "bx", "by"), sep = ",")

dist <- abs(tb$sx - tb$bx) + abs(tb$sy - tb$by)
p1_row <- 2000000L
row_ranges <- dist - abs(tb$sy - p1_row)
relevant <- which(row_ranges >= 0)
left_scanned <- tb$sx[relevant] - row_ranges[relevant]
right_scanned <- tb$sx[relevant] + row_ranges[relevant]
min_scanned <- min(left_scanned)
max_scanned <- max(right_scanned)
scanned <- logical(max_scanned - min_scanned + 1L)
for (n in seq_along(relevant)) {
  scanned[seq.int(left_scanned[n], right_scanned[n]) - min_scanned] <- TRUE
}
sum(scanned) # part one: 5176944

# Each sensor splits the non-scanned region into four sections
# WRT x+y and x-y axes, so work on those, reduce boundaries
# if it doesn't change resulting legal x/y values, and filter for
# valid intervals WRT widths and allowing legal x/y values.
xmax <- ymax <- 4000000L
pmax <- xmax + ymax
mmax <- xmax
mmin <- -ymax
start_interval <- list(c(0L, pmax, mmin, mmax))
# x-y axis consistency:
# a <= x+y <= b, c <= x-y <= d
# => (a+c)/2 <= x <= (b+d)/2, (a-d)/2 <= y <= (b-c)/2.
# remove interval if (a+c)/2 > xmax, (b+d)/2 < 0, etc.
# furthermore, x,y are integers, so [x+y]+[x-y] and [x+y]-[x-y]
# must be even. Therefore, x+y === x-y mod 2.
adjust_for_parity <- function(i) {
  old <- NULL
  while (!identical(old, i) && i[1] <= i[2] && i[3] <= i[4]) {
    old <- i
    if (i[3] == i[4]) {
      i[1:2] <- ifelse(i[1:2] %% 2 == i[3] %% 2, i[1:2], i[1:2] + c(1L, -1L))
    }
    if (i[1] == i[2]) {
      i[3:4] <- ifelse(i[3:4] %% 2 == i[1] %% 2, i[3:4], i[3:4] + c(1L, -1L))
    }
  }
  i
}
is_valid <- function(i) {
  i[1] <= i[2] && i[3] <= i[4] &&
    (i[1] + i[3])/2 <= xmax && (i[2] + i[4])/2 >= 0L &&
    (i[1] - i[4])/2 <= ymax && (i[2] - i[3])/2 >= 0L
}
split_interval <- function(interval, sp, sm, d) {
  splits <- list(
    # > WRT x+y
    c(
      max(interval[1], sp + d + 1L), interval[2],
      interval[3], interval[4]
    ),
    # < WRT x+y
    c(
      interval[1], min(interval[2], sp - d - 1L),
      interval[3], interval[4]
    ),
    # = WRT x+y, > WRT x-y
    c(
      max(interval[1], sp - d), min(interval[2], sp + d),
      max(interval[3], sm + d + 1L), interval[4]
    ),
    # = WRT x+y, < WRT x-y
    c(
      max(interval[1], sp - d), min(interval[2], sp + d),
      interval[3], min(interval[4], sm - d - 1L)
    )
  )
  adjusted <- lapply(splits, adjust_for_parity)
  Filter(is_valid, adjusted)
}
sp <- tb$sx + tb$sy
sm <- tb$sx - tb$sy
poss <- Reduce(
  \(intervals, lst) {
    sp <- lst[[1]]
    sm <- lst[[2]]
    d <- lst[[3]]
    res <- unlist(
      lapply(intervals, split_interval, sp, sm, d),
      recursive = FALSE
    )
    if (length(res) == 0)
      stop(paste(
        "no intervals left. last existing:",
        toString(intervals),
        "lst:",
        toString(lst),
        sep = "\n"
      ))
    res
  },
  Map(c, sp, sm, dist),
  init = start_interval
)
stopifnot(length(poss) == 1)
poss <- poss[[1]]
stopifnot(identical(poss[c(1, 3)], poss[c(2, 4)]))
pos <- as.integer(c(mean(poss[1], poss[3]), mean(poss[1], -poss[4])))
derive_xy <- function(interval) {
  as.integer(c((interval[1] + interval[3])/2, (interval[1] - interval[4])/2))
}
xy_poss <- derive_xy(poss)
format(4000000*xy_poss[1] + xy_poss[2], scientific = FALSE) # part two: 13350458933732
