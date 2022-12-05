x <- scan(
  text = chartr("-", ",", readLines("04.txt")),
  what = list(integer(), integer(), integer(), integer()),
  sep = ","
)
left_contained <- x[[1]] >= x[[3]] & x[[2]] <= x[[4]]
right_contained <- x[[1]] <= x[[3]] & x[[2]] >= x[[4]]
sum(left_contained | right_contained) # part one: 441
left_on_left <- x[[2]] < x[[3]]
left_on_right <- x[[1]] > x[[4]]
sum(!left_on_left & !left_on_right) # part two: 861
