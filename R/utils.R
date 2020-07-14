#' @title Download package
#' @description Makes sure that the package is downloadable
#' @param pkg_name string. The name of the package to be loaded.
#' @export
dwnld_pkg <- function(pkg_name) {
  if(!require(pkg_name, character.only = TRUE)) {
    install.packages(pkg_name) 
  }
}