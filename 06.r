x <- readLines("06.txt")
stopifnot(length(x) == 1)
chars <- strsplit(x, "", fixed = TRUE)[[1]]
uniq <- vapply(
  1:(length(chars) - 3),
  \(n) !anyDuplicated(chars[n:(n + 3)]),
  logical(1)
)
which(uniq)[1] + 3L # part one: 1034
uniq2 <- vapply(
  1:(length(chars) - 13),
  \(n) !anyDuplicated(chars[n:(n + 13)]),
  logical(1)
)
which(uniq2)[1] + 13L # part two: 2472
