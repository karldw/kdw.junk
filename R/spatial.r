
#' Union intersecting geometries in x
#
#' @param x An simple features object (`sf`, `sfg`, or `sfg`)
#' @return The union of intersecting geometries
#' Specifically, union all of the intersecting groups in x
#' https://gis.stackexchange.com/a/323067
#' @examples
#' library(sf)
#' sq = function(pt, sz = 1) {
#'  st_polygon(list(rbind(c(pt - sz), c(pt[1] + sz, pt[2] - sz), c(pt + sz),
#'    c(pt[1] - sz, pt[2] + sz), c(pt - sz))))
#' }
#' x = st_sf(box = 1:6, st_sfc(sq(c(4.2,4.2)), sq(c(0,0)), sq(c(1, -0.8)),
#'   sq(c(0.5, 1.7)), sq(c(3,3)), sq(c(-3, -3))))
#' st_union_intersection(x)
#' @seealso [sf::st_union()]
#' @export
st_union_intersection <- function(x) {
  if (!requireNamespace("igraph", quietly=TRUE)) {
    stop("st_union_intersection requires the igraph package")
  }
  if (!requireNamespace("sf", quietly=TRUE)) {
    stop("st_union_intersection requires the sf package")
  }
  if (length(x) == 0) {
    return(x)
  }
  # Helper function:
  union_by_index <- function(idx, geom) {
    sf::st_union(geom[idx])
  }
  # doesn't matter if x is already a geometry
  x <- sf::st_geometry(x)
  unioned_list <- sf::st_intersects(x) |>
    igraph::graph_from_adj_list() |>
    igraph::components() |>
    igraph::groups() |>
    lapply(union_by_index, geom=x)
  do.call(c, unioned_list)
}
