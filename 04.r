x <- readLines("04.txt")
split <- lapply(strsplit(x, "[-,]"), strtoi)
mat <- do.call(rbind, split)
left_contained <- mat[, 1] >= mat[, 3] & mat[, 2] <= mat[, 4]
right_contained <- mat[, 1] <= mat[, 3] & mat[, 2] >= mat[, 4]
sum(left_contained | right_contained) # part one: 441
left_on_left <- mat[, 2] < mat[, 3]
left_on_right <- mat[, 1] > mat[, 4]
sum(!left_on_left & !left_on_right) # part two: 861
