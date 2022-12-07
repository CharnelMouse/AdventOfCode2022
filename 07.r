x <- readLines("07.txt")
nav <- which(startsWith(x, "$ cd"))
query <- which(x == "$ ls")
responses <- seq_along(x)[-c(nav, query)]
location <- Reduce(
  \(folders, n) {
    if (substr(n, 1, 4) != "$ cd")
      folders
    else{
      target <- substring(n, 6)
      switch(
        target,
        "/" = "/",
        ".." = folders[-length(folders)],
        c(folders, paste(folders[length(folders)], target, sep = "/"))
      )
    }
  },
  x[nav],
  init = character(),
  accumulate = TRUE
)[-1]
query_locations <- location[findInterval(query, nav)]
# any repeat ls commands in same location?
stopifnot(!anyDuplicated(query_locations))
# any repeated folder names in different locations?
# would expect yes if not adding parent path to folder names
stopifnot(!anyDuplicated(vapply(
  query_locations,
  \(fs) fs[length(fs)],
  character(1)
)))

command <- sort(c(nav, query))
nxt <- vapply(query, \(q) command[command > q][1], integer(1))
nxt[is.na(nxt)] <- length(x) + 1L
stopifnot(all(nxt > query))
query_response_indices <- Map(seq.int, query + 1L, nxt - 1L)
# used all lines?
stopifnot(setequal(c(command, unlist(query_response_indices)), seq_along(x)))

sizes <- integer()
for (q in seq_along(query)) {
  qloc <- query_locations[[q]]
  sizes[setdiff(qloc, names(sizes))] <- 0L
  split_responses <- strsplit(x[query_response_indices[[q]]], " ", fixed = TRUE)
  response_first <- vapply(split_responses, "[", character(1), 1)
  response_size_sum <- sum(strtoi(response_first[response_first != "dir"]))
  sizes[qloc] <- sizes[qloc] + response_size_sum
}
sum(sizes[sizes <= 100000L]) # part one: 1583951

required <- 30000000L
current <- 70000000L - sizes["/"]
to_recover <- required - current
min(sizes[sizes >= to_recover]) # part two: 214171
