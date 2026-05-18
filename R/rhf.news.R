rhf.news <- function(...) {
  newsfile <- file.path(system.file(package="randomForestRHF"), "NEWS.md")
  file.show(newsfile)
}
