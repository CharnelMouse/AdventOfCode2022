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
place1 <- 1L + sum(vapply(parsed, comp, logical(1), extra[[1]]))
place2 <- 1L + sum(vapply(parsed, comp, logical(1), extra[[2]]))
place1*place2 + min(place1, place2) # part two: 20570
