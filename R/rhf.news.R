rhf.news <- function(...) {
  newsfile <- file.path(system.file(package="randomForestRHF"), "NEWS")
  file.show(newsfile)
}
