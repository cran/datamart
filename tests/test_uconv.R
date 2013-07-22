library(datamart)

test_uconv <- function() {
  print(uconv(1, "TWh", "PJ", "Energy"))
  print(uconv(13, "km", "m", "Length"))
  print(uconv(-5:5, "t", "g", "Mass"))
}

test_uconv()
