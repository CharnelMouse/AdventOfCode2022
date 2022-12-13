x <- readLines("13.txt")
x <- x[seq_along(x) %% 3 != 0]
parse_packets <- function(x) {
  x |>
    gsub(pattern = "[", replacement = "list(", fixed = TRUE) |>
    gsub(pattern = "]", replacement = ")", fixed = TRUE) |>
    lapply(\(s) eval(parse(text = s)))
}
parsed <- parse_packets(x)
pairs <- split(
  parsed,
  rep(seq_len(length(parsed)/2), each = 2)
)
comp <- function(x, y) {
  if (is.numeric(x) && is.numeric(y)) {
    if (x == y)
      NA
    else
      x < y
  }else{
    x <- as.list(x)
    y <- as.list(y)
    len <- min(length(x), length(y))
    els <- mapply(comp, x[seq_len(len)], y[seq_len(len)])
    if (any(!is.na(els)))
      els[!is.na(els)][1]
    else{
      if (length(x) == length(y))
        NA
      else
        length(x) < length(y)
    }
  }
}
res <- vapply(pairs, \(lst) comp(lst[[1]], lst[[2]]), logical(1))
sum(seq_along(res)[res]) # part one: 5623 too low

extra <- parse_packets(c("[[2]]", "[[6]]"))
enhanced <- c(parsed, extra)
sort_packets <- function(packets) {
  if (length(packets) <= 1)
    return(packets)
  hinge <- packets[[1]]
  comps <- vapply(packets[-1], comp, logical(1), hinge)
  c(
    sort_packets(packets[c(FALSE, comps)]),
    list(hinge),
    sort_packets(packets[c(FALSE, !comps)])
  )
}
sorted <- sort_packets(enhanced)
stopifnot(length(sorted) == length(enhanced))
prod(match(extra, sorted)) # part two: 20570
