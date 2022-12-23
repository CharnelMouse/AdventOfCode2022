x <- do.call(rbind, strsplit(readLines("23.txt"), "", fixed = TRUE))
elves <- which(x == "#", arr.ind = TRUE)
n_elves <- nrow(elves)

decide <- function(n, elves, time) {
  elf <- elves[n, ]
  h <- elves[, 1] %in% (elf[1] + seq.int(-1L, 1L))
  h[n] <- FALSE
  both <- which(elves[h, 2] %in% (elf[2] + seq.int(-1L, 1L)))
  if (!any(both))
    return(elf)
  neighs <- elves[both, , drop = FALSE]
  check_seq <- c("N", "S", "W", "E")[(time + 0:3 - 1L) %% 4L + 1L]
  for (dir in check_seq) {
    if (dir == "N") {
      north_neigh <- neighs[, 1] == (elf[1] - 1L)
      if (!any(north_neigh))
        return(c(elf[1] - 1L, elf[2]))
    }
    if (dir == "S") {
      south_neigh <- neighs[, 1] == (elf[1] + 1L)
      if (!any(south_neigh))
        return(c(elf[1] + 1L, elf[2]))
    }
    if (dir == "W") {
      west_neigh <- neighs[, 2] == (elf[2] - 1L)
      if (!any(west_neigh))
        return(c(elf[1], elf[2] - 1L))
    }
    if (dir == "E") {
      east_neigh <- neighs[, 2] == (elf[2] + 1L)
      if (!any(east_neigh))
        return(c(elf[1], elf[2] + 1L))
    }
  }
  elf
}
step <- function(elves, time) {
  plan <- lapply(seq_len(n_elves), decide, elves, time)
  dupped <- plan[duplicated(plan)]
  dups <- is.element(plan, dupped)
  plan_flat <- do.call(rbind, plan)
  plan_flat[dups, ] <- elves[dups, ]
  plan_flat
}

res <- Reduce(step, 1:10, init = elves)
(diff(range(res[, 1])) + 1L)*(diff(range(res[, 2])) + 1L) - n_elves # part one: 4116

old <- NULL
curr <- elves
time <- 0L
while (!identical(old, curr)) {
  time <- time + 1L
  cat(time, "\r")
  old <- curr
  curr <- step(curr, time)
}
time # part two: 984
