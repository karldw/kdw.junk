context("spatial functions")


test_that("st_union_intersection works as expected", {
  sq = function(pt, sz = 1) {
    sf::st_polygon(list(rbind(c(pt - sz), c(pt[1] + sz, pt[2] - sz), c(pt + sz), c(pt[1] - sz, pt[2] + sz), c(pt - sz))))
  }
  squares <- sf::st_sfc(
    sq(c(4.2, 4.2)), # 1
    sq(c(0, 0)),     # 2
    sq(c(1, -0.8)),  # 3
    sq(c(0.5, 1.7)), # 4
    sq(c(3, 3)),     # 5
    sq(c(-3, -3))    # 6
  )
  res <- st_union_intersection(sf::st_sf(box = 1:6, geometry = squares))
  expected <- c(
    squares[6],
    sf::st_union(squares[c(1, 5)]),
    sf::st_union(squares[c(2, 3, 4)])
  )
  expect_equal(length(expected), length(res))
  expect_true(setequal(expected, res))
})

test_that("st_union_intersection works for empty input", {
  x <- sf::st_sfc()
  expect_equal(st_union_intersection(x), x)
})
