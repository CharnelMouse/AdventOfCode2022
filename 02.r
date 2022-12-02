lines <- readLines("02.txt")
nums <- chartr("ABCXYZ", "123123", lines)
opp <- strtoi(substr(nums, 1, 1))
self <- strtoi(substr(nums, 3, 3))
cycle <- (self - opp + 1L) %% 3L
sum(self + cycle*3L) # part one: 13005
self2 <- (opp + self) %% 3L + 1L
cycle2 <- self - 1L
sum(self2 + cycle2*3L) # part two: 11373
