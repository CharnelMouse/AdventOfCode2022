x <- readLines("05.txt")
empty <- which(x == "")[1]

start <- x[seq_len(empty - 1)]
slen <- length(start)
stacknums <- strsplit(start[slen], "[ ]*")[[1]]
stacknums <- stacknums[stacknums != ""]
n_stack <- strtoi(stacknums[length(stacknums)])
start <- start[-slen]
stackcols <- 4L*seq_len(n_stack) - 2L
start_stacks <- lapply(stackcols, \(n) substr(start, n, n))
start_stacks <- lapply(start_stacks, \(s) s[s != " "]) # top crate first

steps <- do.call(rbind, strsplit(x[seq(empty + 1, length(x))], " "))
steps <- apply(steps[, c(2, 4, 6)], 2, strtoi) # n, from, to

stacks <- start_stacks
for (step in seq_len(nrow(steps))) {
  stacks[[steps[step, 3]]] <- c(
    stacks[[steps[step, 2]]][rev(seq_len(steps[step, 1]))],
    stacks[[steps[step, 3]]]
  )
  stacks[[steps[step, 2]]] <- stacks[[steps[step, 2]]][-seq_len(steps[step, 1])]
}
top <- vapply(stacks, "[", character(1), 1)
paste(top, collapse = "") # part one: SBPQRSCDF

stacks2 <- start_stacks
for (step in seq_len(nrow(steps))) {
  stacks2[[steps[step, 3]]] <- c(
    stacks2[[steps[step, 2]]][seq_len(steps[step, 1])],
    stacks2[[steps[step, 3]]]
  )
  stacks2[[steps[step, 2]]] <- stacks2[[steps[step, 2]]][-seq_len(steps[step, 1])]
}
top2 <- vapply(stacks2, "[", character(1), 1)
paste(top2, collapse = "") # part two: RGLVRCQSB
