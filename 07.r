x <- readLines("07.txt")
nav <- which(startsWith(x, "$ cd"))
query <- which(x == "$ ls")
location <- Reduce(
  \(folders, n) {
    target <- substring(n, 6)
    switch(
      target,
      "/" = "/",
      ".." = folders[-length(folders)],
      # use full path, since same folder name can appear in different paths
      c(folders, paste(folders[length(folders)], target, sep = "/"))
    )
  },
  x[nav],
  init = character(),
  accumulate = TRUE
)[-1]
query_locations <- location[findInterval(query, nav)]
# can check for no duplicates here if worried about multiple ls calls on same
# directory

file_responses <- which(substr(x, 1, 1) %in% as.character(0:9))
file_sizes <- vapply(
  strsplit(x[file_responses], " ", fixed = TRUE),
  \(splt) strtoi(splt[1]),
  integer(1)
)
# total file size in queries
query_file_size_totals <- tapply(
  file_sizes,
  factor(findInterval(file_responses, query), seq_along(query)),
  sum,
  default = 0L
)
# total file size in queries, summed across relevant directories
totals <- tapply(
  rep(query_file_size_totals, lengths(query_locations)),
  unlist(query_locations),
  sum
)
sum(totals[totals <= 100000L]) # part one: 1583951

required <- 30000000L
current <- 70000000L - totals["/"]
to_recover <- required - current
min(totals[totals >= to_recover]) # part two: 214171
