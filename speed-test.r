times <- microbenchmark::microbenchmark(
  source("01.r"),
  source("02.r"),
  source("03.r"),
  source("04.r"),
  source("05.r"),
  source("06.r"),
  source("07.r"),
  source("08.r"),
  source("09.r"),
  source("10.r"),
  source("11.r"),
  source("12.r"),
  source("13.r"),
  source("14.r"),
  source("21.r"),
  source("22.r"),
  setup = expression(rm(list = ls()))
)

times
print(times, "relative")
