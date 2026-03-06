get.data.pass.bits <- function (data.pass) {
  if (!is.null(data.pass)) {
    if (data.pass == TRUE) {
      data.pass <- 2^15
    }
    else if (data.pass == FALSE) {
      data.pass <- 0
    }
    else {
      stop("Invalid choice for 'data.pass' option:  ", data.pass)
    }
  }
  else {
    stop("Invalid choice for 'data.pass' option:  ", data.pass)
  }
  return (data.pass)
}
get.experimental.bits  <- function(experimental.bits, trace) {
  ## New protocol is to use the uniform hazard estimator. This is
  ## UBK's version of the hazard. It avoids the issue of having OOB
  ## unscaled risk inf values, that we experienced due to log(0) where
  ## the column (corresponding to a time interest point) is zero for
  ## the OOB subject.
  if (!is.null(experimental.bits)) {
    if (experimental.bits < 0L) {
      stop("bits must be between 0 and 255.")
    }
    else if (experimental.bits > 255L) {
      stop("bits must be between 0 and 255.")
    }
    else if (experimental.bits == 0) {
        ## This is for RSF, no frills.
        experimental.bits <- 0
    }
    else if (is.bit(experimental.bits, 0) && is.bit(experimental.bits, 1)) {
        cat ("Endpoint Estimation:  ", is.bit(experimental.bits, 0), "\n")
        cat ("Uniform Estimation:   ", is.bit(experimental.bits, 1), "\n")
        stop("only one protocol bit allowed.")
    }
  }
  else {
    ## Default is UBK's rule when null.
    experimental.bits <- 2^1
  }
  if (trace) {
    cat ("Endpoint Estimation:  ", is.bit(experimental.bits, 0), "\n")
    cat ("Uniform Estimation:   ", is.bit(experimental.bits, 1), "\n")
    cat ("Uniform Head:         ", is.bit(experimental.bits, 2), "\n")
    cat ("Uniform Tail:         ", is.bit(experimental.bits, 3), "\n")
  }
  return (experimental.bits)
}
## This little function took just over 20 minutes on ChatGPT Pro.  Lol.
# Returns TRUE if the given 0-7 bit is set in an 8-bit integer, FALSE otherwise.
# - value: integer (vectorized); values outside 0..255 are masked to 8 bits
# - bit: single integer in 0..7 (0 = least significant bit)
is.bit <- function(value, bit) {
  if (length(bit) != 1L || is.na(bit) || bit != as.integer(bit) || bit < 0L || bit > 7L) {
    stop("`bit` must be a single integer in 0..7.")
  }
  v <- bitwAnd(as.integer(value), 255L)         # clamp to 8 bits
  m <- bitwShiftL(1L, as.integer(bit))          # mask for the requested bit
  bitwAnd(v, m) != 0L                           # TRUE if bit set, FALSE otherwise (NA propagates)
}
get.tree.index <- function(get.tree, ntree) {
  ## NULL --> default setting
  if (is.null(get.tree)) {
    rep(1, ntree)
  }
  ## the user has specified a subset of trees
  else {
    pt <- get.tree >=1 & get.tree <= ntree
    if (sum(pt) > 0) {
      get.tree <- get.tree[pt]
      get.tree.temp <- rep(0, ntree)
      get.tree.temp[get.tree] <- 1
      get.tree.temp
    }
    else {
      rep(1, ntree)
    }
  }
}
get.block.size.bits <- function (block.size, ntree) {
    ## Check for user silliness.
    if (!is.null(block.size)) {
        if ((block.size < 1) || (block.size > ntree)) {
            block.size <- ntree
        }
        else {
            block.size <- round(block.size)
        }
    }
    else {
        block.size <- ntree
    }
    return (block.size)
}
get.bootstrap.bits <- function (bootstrap) {
  if (bootstrap == "none") {
    bootstrap <- 0
  }
  else if (bootstrap == "by.root") {
    bootstrap <- 2^19
  }
  else if (bootstrap == "by.user") {
    bootstrap <- 2^20
  }
  else {
    stop("Invalid choice for 'bootstrap' option:  ", bootstrap)
  }
  return (bootstrap)
}
## convert ensemble option into native code parameter.
get.ensemble.bits <- function (ensemble) {
  if (ensemble == "oob") {
    ensemble <- 2^1
  }
  else if (ensemble == "inbag") {
    ensemble <- 2^0
  }
  else if (ensemble == "all") {
    ensemble <- 2^0 + 2^1
  }    
  else {
      ## For testing purposes, we allow the ensemble to be omitted altogether.
      ensemble <- 0
  }
  return (ensemble)
}
get.empirical.risk.bits <-  function (empirical.risk) {
  if (empirical.risk) {
    return (2^18)
  }
  else {
    return (0)
  }
}
get.forest.bits <- function (forest) {
  ## Convert forest option into native code parameter.
  if (!is.null(forest)) {
    if (forest == TRUE) {
      forest <- 2^5
    }
    else if (forest == FALSE) {
      forest <- 0
    }
    else {
      stop("Invalid choice for 'forest' option:  ", forest)
    }
  }
  else {
    stop("Invalid choice for 'forest' option:  ", forest)
  }
  return (forest)
}
get.membership.bits <- function (membership) {
  ## Convert option into native code parameter.
  bits <- 0
  if (!is.null(membership)) {
    if (membership == TRUE) {
      bits <- 2^6
    }
    else if (membership != FALSE) {
      stop("Invalid choice for 'membership' option:  ", membership)
    }
  }
  else {
    stop("Invalid choice for 'membership' option:  ", membership)
  }
  return (bits)
}
get.perf <-  function (perf, family) {
  ## Deal with non-classification
  if (family != "class") {
    if (is.null(perf)) {
      return("default")
    }
    perf <- match.arg(perf, c("none", "default", "standard"))
    if (perf == "standard") {
      perf <- "default"
    }
    return(perf)
  }
  else {
      ## Deal with classification
      if (is.null(perf)) {
          return("default")
      }
      perf <- match.arg(perf, c("none", "default", "standard", "misclass", "brier", "gmean"))
      if (perf == "standard" || perf == "misclass") {
          perf <- "default"
      }
      return(perf)
  }
}
get.perf.bits <- function (perf) {
  if (perf == "default") {
    return (2^2)
  }
  else if (perf == "gmean") {
    return (2^2 + 2^14)
  }
  else if (perf == "brier") {
    return (2^2 + 2^3)
  }
  else {#everything else becomes "none"
    return (0)
  }
}
get.rf.cores <- function () {
    ## PART I:  Two ways for the user to specify cores:
    ## (1) R-option "rf.cores"
    ## (2) Shell-environment-option "RF_CORES"
    if (is.null(getOption("rf.cores", NULL))) {
        if (!is.na(as.numeric(Sys.getenv("RF_CORES")))) {
            options(rf.cores = as.integer(Sys.getenv("RF_CORES")))
        }
    }
    ## If the user has set the cores using either of the two methods, we respect it.
    if (!is.null(getOption("rf.cores", NULL))) {
        return (getOption("rf.cores"))
    }
    ## PART II:  Respect R CMD check limit
    chk <- tolower(Sys.getenv("_R_CHECK_LIMIT_CORES_", ""))
    if (nzchar(chk) && chk != "false") {
        ## under R CMD check --as-cran (CRAN sets this)
        return(2L)
    }
    ## PART III:  Use everything.
    return (-1L)
}
get.weight <- function(weight, n) {
  ## set the default weight
  if (!is.null(weight)) {
    if (any(weight < 0)      ||
      length(weight) != n  ||
      any(is.na(weight))) {
        stop("Invalid weight vector specified.")
    }
  }
  else {
    weight <- rep(1, n)
  }
  return (weight)
}
## Real time predicton option:
is.hidden.rt <-  function(dots) {
  if (is.null(dots$real.time)) {
    FALSE
  }
  else {
    as.logical(as.character(dots$real.time))
  }
}
get.rt.bits  <- function(real.time) {
  if (real.time == TRUE) {
    bits  <- 2^7
  }
  else {
    bits  <- 0
  }
  return (bits)
}
is.hidden.rt.opt  <- function(dots) {
  if (is.null(dots$real.time.options)) {
    list(port = 6666, time.out = 15)      
  }
  else {
    dots$real.time.options
  }
}
## convert samptype option into native code parameter.
get.samptype.bits <- function (samptype) {
  if (samptype == "swr") {
    bits <- 0
  }
  else if (samptype == "swor") {
    bits <- 2^12
  }
  else {
    stop("Invalid choice for 'samptype' option:  ", samptype)
  }
  return (bits)
}
get.seed <- function (seed) {
  if ((is.null(seed)) || (abs(seed) < 1)) {
    seed <- runif(1,1,1e6)
  }
  seed <- -round(abs(seed))
  return (seed)
}
get.trace <- function (do.trace) {
  ## Convert trace into native code parameter.
  if (!is.logical(do.trace)) {
    if (do.trace >= 1) {
      do.trace <- round(do.trace)
    }
    else {
      do.trace <- 0
    }
  }
  else {
    do.trace <- 1 * do.trace
  }
  return (do.trace)
}
