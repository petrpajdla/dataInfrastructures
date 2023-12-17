# utility functions

#' Write sf as gpkg
#'
#' Checks whether path exists, append datestamp to the filename.
#'
#' @param x \code{sf} object to write
#' @param dir Path where to write the file
#' @param filename Filename
#'
#' @return
#' @export
#'
#' @examples
write_sf_gpkg <- function(x, dir, filename) {
  path <- paste0(dir, filename, "_", Sys.Date(), ".gpkg")

  if (!dir.exists(dir)) {
    dir.create(dir)
  }

  x |> sf::st_write(path)
}
