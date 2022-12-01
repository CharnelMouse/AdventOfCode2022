x <- readLines("01.txt")
len <- length(x)
empty <- which(x == "")
nonempty <- seq_len(len)[-empty]
cals <- as.integer(tapply(strtoi(x[nonempty]), findInterval(nonempty, empty), sum))
max(cals) # part one: 72511
keep <- length(cals) - 0:2
top <- sort.int(cals, partial = keep)[keep]
sum(top) # part two: 212117
