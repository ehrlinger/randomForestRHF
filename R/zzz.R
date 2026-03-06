.onAttach <- function(libname, pkgname) {
  package.version <- read.dcf(file=system.file("DESCRIPTION", package=pkgname), 
                            fields="Version")
  packageStartupMessage(paste("\n",
                              pkgname,
                              package.version,
                              "\n",
                              "\n",
                              paste("Type ", pkgname, "news() to see new features, changes, and bug fixes.", sep = ""),
                              "\n",
                              "\n"))
}
