x <- readLines("10.txt")

addop <- substr(x, 1, 1) == "a"
cycle <- ifelse(addop, 2L, 1L)
times <- cumsum(cycle)
X <- cumsum(c(1L, strtoi(substring(x[addop], 6))))
X_times <- c(0L, times[addop])
sample_times <- c(20L, 60L, 100L, 140L, 180L, 220L)
sum(sample_times*X[findInterval(sample_times - 1L, X_times)]) # part one: 12840

screen_times <- findInterval(seq_len(240L) - 1L, X_times)
pixel_present <- X[screen_times] >= (seq_len(240L) - 1L) %% 40L - 1L &
  X[screen_times] <= seq_len(240L) %% 40L
image <- t(matrix(ifelse(pixel_present, "#", "."), ncol = 6))
cat(apply(image, 1, paste, collapse = ""), sep = "\n") # part two: ZKJFBJFZ
