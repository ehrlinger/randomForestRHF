rhf.xvar.wt <- function(f, d, scale = 4, parallel = TRUE) {
  o.stump <- rhf(f, d, ntree = 1, treesize = 1)
  xvar.names <- o.stump$xvar.names
  vp  <- varpro(Surv(time, event) ~ .,
                convert.standard.counting(f, d),
                parallel = parallel)
  imp <- get.orgvimp(vp, pretty = FALSE)
  weights <- rep(0, length(xvar.names))
  names(weights) <- xvar.names
  weights[names(imp)] <- imp ^ scale
  weights
}
