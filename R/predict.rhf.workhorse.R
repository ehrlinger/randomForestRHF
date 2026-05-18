predict.rhf.workhorse <-  function(object,
                                   newdata,
                                   get.tree = NULL,
                                   block.size = NULL,
                                   seed = NULL,
                                   membership = FALSE,
                                   do.trace = FALSE,
                                   ...)
{
  ## confirm this is a rhf object
  if (!inherits(object, "rhf")) {
    stop("this function only works for objects of class 'rhf'")
  }
  ## hidden options (used later)
  dots <- list(...)
  ## initialize the seed
  seed <- get.seed(seed)
  ## uniform versus endpoint estimation hazard
  experimental.bits <- get.experimental.bits(dots$experimental.bits, FALSE)
  ## hard coded options (some are legacy)
  perf.type <- NULL
  ## set restore.mode and the ensemble option
  if (missing(newdata)) {##restore: no data
    restore.mode <- TRUE
    ensemble <- "all"
  }
  else {##not restore: test data present
    restore.mode <- FALSE
    ensemble <- "inbag"
  }
  ## check if this is an anonymous object and process accordingly
  if (inherits(object, "anonymous")) {
    anonymize.bits <- 2^26
    if (restore.mode) {
      stop("in order to predict with anonymous forests please provide a test data set")
    }
  }
  else {
    anonymize.bits <- 0
  }
  ## set the family
  family <- object$family
  subj  <- object$id
  subj.unique.count <- length(unique(subj))
  case.wt  <- get.weight(NULL, subj.unique.count)
  event.info <- object$event.info
  ## get time-map information
  max.time <- object$max.time 
  time.map <- object$time.map
  if (is.null(time.map)) {
    time.map <- list(method = "legacy",
                     max.time = as.double(max.time),
                     tau = as.double(max.time))
  }
  ## obtain y-outcome information from the grow object; scale y to [0,1]
  yvar <- .iscale.yvar(object$yvar, time.map)
  yvar.names <- object$yvar.names
  yvar.types <- object$yvar.types
  yvar.nlevels  <- object$yvar.factor$nlevels
  yvar.nlevels  <- NULL
  ## obtain x information from the grow object 
  xvar <- object$xvar
  xvar.names <- object$xvar.names
  xvar.types <- object$xvar.types
  xvar.nlevels  <- rep(0, length(xvar.types))
  xvar.nlevels  <- NULL
  ## get the subject name
  subj.names  <- object$subj.names  
  ## hard coded options
  xvar.augment.newdata <- augmentXlist <- hcutCnt <- NULL
  hcut  <- 1
  ## recover the split information
  splitinfo <- object$splitinfo
  splitrule <- object$splitinfo$splitrule
  ## get the subjects -- this might be the test subjects eventually TBD
  id = object$id  
  ## initialize the seed
  seed <- get.seed(seed)
  ## REASSIGN OBJECT: hereafter we only need the forest 
  object <- object$forest
  ## confirm version coherence
  if (is.null(object$version)) {
      stop(
          paste(
              "This function only works with objects created with the following minimum version of the package:",
                     "  Minimum version:  0.0.0.0",
              paste0("  Your version:     unknown"),
              sep = "\n"
          ),
          call. = FALSE
      )
  }
  else {
    object.version <- as.integer(unlist(strsplit(object$version, "[.]")))
    installed.version <- as.integer(unlist(strsplit("1.0.1", "[.]")))
    minimum.version <- as.integer(unlist(strsplit("0.0.0.0", "[.]")))
    object.version.adj <- object.version[1] + (object.version[2]/10) + (object.version[3]/100)
    installed.version.adj <- installed.version[1] + (installed.version[2]/10) + (installed.version[3]/100)
    minimum.version.adj <- minimum.version[1] + (minimum.version[2]/10) + (minimum.version[3]/100)
    ## Minimum object version must be satisfied for us to proceed.  This is the only way
    ## terminal node restoration is guaranteed, due to RNG coherency.
    if (object.version.adj >= minimum.version.adj) {
      ## We are okay
    }
    else {
      stop(
          paste(
              "This function only works with objects created with the following minimum version of the package:",
              "  Minimum version:  0.0.0.0",
              paste0("  Your version:     ", object$version),
              sep = "\n"
          ),
          call. = FALSE
      )
    }
  }
  ##--------------------------------------------------------
  ##
  ## process x and y: test data is present
  ##
  ##--------------------------------------------------------
  if (!restore.mode) {
    ## clean up test data (handle missingness, scale time, scale y)
    nd <- cleanup.counting.newdata(newdata    = newdata,
                                   xvar.names = xvar.names,
                                   yvar.names = yvar.names,
                                   subj.names = subj.names,
                                   time.map   = time.map,
                                   max.time   = max.time)
    ## overwrite with cleaned version
    newdata   <- nd$newdata
    n.newdata <- nrow(newdata)
    ## get the test subjects
    subj.newdata  <- nd$subj
    subj.newdata.unique.count <- length(unique(subj.newdata))
    ## restrict xvar to the training xvar.names
    xvar.newdata <- nd$xvar
    ## hard coded 
    xvar.augment.newdata <- NULL
    ## extract test yvar (if present); already scaled to [0,1]
    yvar.newdata <- nd$yvar
    yvar.present <- nd$yvar.present
    if (yvar.present) {
      perf.type <- get.perf(perf.type, family)
    } else {
      ## keep subj.newdata.unique.count for dimensionality,
      ## but drop subj/y for performance calculations
      subj.newdata <- yvar.newdata <- NULL
      perf.type <- "none"
    }
  }
  ##--------------------------------------------------------
  ##
  ##  process x and y: no test data is present
  ##
  ##--------------------------------------------------------
  else {
    ## There cannot be test data in restore mode
    ## The native code switches based on n.newdata being zero (0).  Be careful.
    n.newdata <- 0
    xvar.newdata <- yvar.newdata <-  subj.newdata  <- NULL
    ## perf type
    perf.type <- get.perf(perf.type, family)
  }
  ## ------------------------------------------------------------
  ##
  ## restore/non-restore x/y processing completed
  ## finalize and make C call
  ##
  ## ------------------------------------------------------------
  ## set the performance bits
  perf.bits <-  get.perf.bits(perf.type)
  ## initialize the number of trees in the forest
  ntree <- object$ntree
  ## process the get.tree vector that specifies which trees we want
  ## to extract from the forest.  This is only relevant to restore mode.
  ## The parameter is ignored in predict mode.
  get.tree <- get.tree.index(get.tree, ntree)
  bootstrap.bits <- get.bootstrap.bits(object$parms$bootstrap)
  ## initialize the low bits
  ensemble.bits <- get.ensemble.bits(ensemble)
  ## sample related
  samptype <- object$parms$samptype
  sampsize <- object$parms$sampsize
  samp <- object$parms$samp
  ## Initalize the high bits
  samptype.bits <- get.samptype.bits(samptype)
  membership.bits <-  get.membership.bits(membership)
  ##  jitt.bits <- get.jitt.bits(jitt)
  ## We over-ride block-size in the case that get.tree is user specified
  block.size <- min(get.block.size.bits(block.size, ntree), sum(get.tree))
  ## Default target. We don't support more than one, and this is a bit of a legacy issue.
  m.target.idx <- 1
  ## do.trace
  do.trace <- get.trace(do.trace)
  ## WARNING: Note that the maximum number of slots in the following
  ## foreign function call is 64.  Ensure that this limit is not
  ## exceeded.  Otherwise, the program will error on the call.
  ## Real time prediction option:
  real.time  <- is.hidden.rt(dots)    
  real.time.bits  <- get.rt.bits(real.time)
  if (real.time) {
    real.time.options  <- is.hidden.rt.opt(dots)
  }
  else {
    real.time.options  <- NULL
  }
  ## Start the C external timer.
  ctime.external.start  <- proc.time()
  nativeOutput <- tryCatch({.Call("entryPred",
                                  as.integer(do.trace),
                                  as.integer(seed),
                                  as.integer(bootstrap.bits +   
                                             perf.bits +
                                             ensemble.bits +
                                             anonymize.bits),   ## low option word
                                  as.integer(membership.bits +  ## high option word
                                             2^19 + 2^18 +      ## TERM_INCG and TERM_OUTG
                                             samptype.bits),
                                  as.integer(experimental.bits),  ## rhf (local and experimental) option word
                                  as.integer(ntree),
                                  as.integer(object$n),
                                  list(as.integer(subj.unique.count),
                                       if (is.null(case.wt)) NULL else as.double(case.wt),
                                       as.integer(sampsize),
                                       if (is.null(samp)) NULL else as.integer(samp)),
                                  as.integer(hcut),
                                  as.integer(splitinfo$index),
                                  list(if (is.null(m.target.idx)) as.integer(0) else as.integer(length(m.target.idx)),
                                       if (is.null(m.target.idx)) NULL else as.integer(m.target.idx)),
                                  list(as.integer(length(yvar.types)),
                                       if (is.null(yvar.types)) NULL else as.character(yvar.types),
                                       if (is.null(yvar.nlevels)) NULL else as.integer(yvar.nlevels),
                                       if (is.null(yvar.nlevels)) NULL else sapply(1:length(yvar.nlevels), function(nn) {as.integer(length(yvar.nlevels[[nn]]))}),
                                       if (is.null(subj)) NULL else as.integer(subj),
                                       if (is.null(event.info)) as.integer(0) else as.integer(length(event.info$event.type)),
                                       if (is.null(event.info)) NULL else as.integer(event.info$event.type)),
                                  if (is.null(yvar.nlevels)) {
                                    NULL
                                  }
                                  else {
                                    lapply(1:length(yvar.nlevels),
                                           function(nn) {as.integer(yvar.nlevels[[nn]])})
                                  },
                                  if (is.null(yvar.types)) NULL else as.double(as.vector(data.matrix(yvar))),
                                  list(as.integer(ncol(xvar)),
                                       if (is.null(xvar.types)) NULL else as.character(xvar.types),
                                       if (is.null(xvar.nlevels)) NULL else as.integer(xvar.nlevels),
                                       if (is.null(xvar.nlevels)) NULL else sapply(1:length(xvar.nlevels), function(nn) {as.integer(length(xvar.nlevels[[nn]]))}),
                                       NULL,
                                       NULL),
                                  if (is.null(xvar.nlevels)) {
                                    NULL
                                  }
                                  else {
                                    lapply(1:length(xvar.nlevels),
                                           function(nn) {as.integer(xvar.nlevels[[nn]])})
                                  },
                                  as.double(as.vector(data.matrix(xvar))),
                                  ## assignment of augmented variables (list of two values: dimension, data)
                                  augmentXlist,
                                  as.integer(n.newdata),
                                  if (is.null(subj.newdata)) NULL else as.integer(subj.newdata),
                                  if (is.null(yvar.newdata)) NULL else as.double(as.vector(data.matrix(yvar.newdata))),
                                  if (is.null(xvar.newdata)) NULL else as.double(as.vector(data.matrix(xvar.newdata))),
                                  as.integer(object$totalNodeCount),
                                  as.integer(object$leafCount),
                                  as.integer(object$seed),
                                  as.integer((object$nativeArray)$treeID),
                                  as.integer((object$nativeArray)$nodeID),
                                  as.integer((object$nativeArray)$nodeSZ),
                                  as.integer((object$nativeArray)$brnodeID),
                                  ## This is hc_zero.  It is never NULL.
                                  list(as.integer((object$nativeArray)$parmID),
                                  as.double((object$nativeArray)$contPT),
                                  as.integer((object$nativeArray)$mwcpSZ),
                                  as.integer((object$nativeArray)$fsrecID),
                                  if (is.null((object$nativeFactorArray)$mwcpPT)) NULL else as.integer((object$nativeFactorArray)$mwcpPT)),
                                  as.integer(object$trmbrCaseCt),
                                  as.integer(object$timbrCaseCt),
                                  as.integer(object$tombrCaseCt),
                                  as.integer(object$trmbrCaseId),
                                  as.integer(object$timbrCaseId),
                                  as.integer(object$tombrCaseId),
                                  as.integer(get.tree),
                                  as.integer(block.size),
                                  if (real.time) list(as.integer(real.time.options$port), as.integer(real.time.options$time.out)) else NULL,
                                  as.integer(get.rf.cores()))}, error = function(e){NULL})
  ## Stop the C external timer.
  ctime.external.stop <- proc.time()
  ## check for error return condition in the native code
  if (is.null(nativeOutput)) {
    if (real.time == TRUE) {
      ## This is acceptable, for now.  Real time mode returns null,
      ## but we can revist this as the code evolves.
      return (NULL)  
    }
    else {
      stop("An error has occurred in prediction.  Please turn trace on for further analysis.")
    }
  }
  ## sample size used for the return predict object
  n.observed = ifelse(restore.mode, nrow(xvar), n.newdata)
  if (!restore.mode) {
      hazard.ibg <- NULL
      hazard.oob <- NULL
      chf.ibg <- NULL
      chf.oob <- NULL
      risk.ibg     <- NULL
      risk.oob     <- NULL
      int.haz.ibg <- NULL
      int.haz.oob <- NULL
      inbag.out <- NULL
      pseudo.membership <- NULL
  } 
  if (restore.mode) {
    if (membership) {
      pseudo.membership <- matrix(nativeOutput$nodeMembership, c(n.observed, ntree))
      inbag.out <- matrix(nativeOutput$bootstrapCount, c(subj.unique.count, ntree))
      nativeOutput$nodeMembership <- NULL
      nativeOutput$bootstrapCount <- NULL
    } 
    else {
      pseudo.membership <- NULL
      inbag.out  <- NULL
    }
    if (!is.null(nativeOutput$ensembleID)) {
      ensemble.id <- nativeOutput$ensembleID
    }
    else {
      ensemble.id <- NULL
    }
    if (!is.null(nativeOutput$ibgEnsbHazard)) {
      hazard.ibg  <- array(nativeOutput$ibgEnsbHazard, c(subj.unique.count, length(event.info$time.interest)))
    } else {
      hazard.ibg <- NULL
    }
    if (!is.null(nativeOutput$oobEnsbHazard)) {
      hazard.oob  <- array(nativeOutput$oobEnsbHazard, c(subj.unique.count, length(event.info$time.interest)))
    } else {
      hazard.oob <- NULL
    }
    if (!is.null(nativeOutput$ibgEnsbNlsnAaln)) {
      chf.ibg  <- array(nativeOutput$ibgEnsbNlsnAaln, c(subj.unique.count, length(event.info$time.interest)))
    } else {
      chf.ibg <- NULL
    }
    if (!is.null(nativeOutput$oobEnsbNlsnAaln)) {
      chf.oob  <- array(nativeOutput$oobEnsbNlsnAaln, c(subj.unique.count, length(event.info$time.interest)))
    } else {
      chf.oob <- NULL
    }
    if (!is.null(nativeOutput$ibgRisk)) {
      risk.ibg <- nativeOutput$ibgRisk
    } else {
      risk.ibg <- NULL
    }
    if (!is.null(nativeOutput$oobRisk)) {
      risk.oob <- nativeOutput$oobRisk
    } else {
      risk.oob <- NULL
    }
    if (!is.null(nativeOutput$ibgWCase)) {
        int.haz.ibg <- nativeOutput$ibgWCase
    } else {
        int.haz.ibg <- NULL
    }
    if (!is.null(nativeOutput$oobWCase)) {
        int.haz.oob <- nativeOutput$oobWCase
    } else {
        int.haz.oob <- NULL
    }
    if (!is.null(nativeOutput$absWCaseTimeLeft)) {
        int.haz.left <- .inverse.time(nativeOutput$absWCaseTimeLeft, time.map)
    } else {
        int.haz.left <- NULL
    }
    if (!is.null(nativeOutput$absWCaseTimeRight)) {
        int.haz.right <- .inverse.time(nativeOutput$absWCaseTimeRight, time.map)
    } else {
        int.haz.right <- NULL
    }
    hazard.tst  <- NULL
    chf.tst  <- NULL
    unscaled.risk.tst  <- NULL
    risk.tst     <- NULL
    int.haz.tst <- NULL
    ttmbrCaseCt <- NULL
    ttmbrCaseId <- NULL
  } else {
    if (!is.null(nativeOutput$ensembleID)) {
      ensemble.id <- nativeOutput$ensembleID
    }
    else {
      ensemble.id <- NULL
    }
    if (!is.null(nativeOutput$ibgEnsbHazard)) {
      hazard.tst  <- array(nativeOutput$ibgEnsbHazard, c(subj.newdata.unique.count, length(event.info$time.interest)))
    } else {
      hazard.tst <- NULL
    }
    if (!is.null(nativeOutput$ibgEnsbNlsnAaln)) {
      chf.tst  <- array(nativeOutput$ibgEnsbNlsnAaln, c(subj.newdata.unique.count, length(event.info$time.interest)))
    } else {
      chf.tst <- NULL
    }
    if (!is.null(nativeOutput$ibgRisk)) {
      risk.tst <- nativeOutput$ibgRisk
    } else {
      risk.tst <- NULL
    }
    if (!is.null(nativeOutput$ibgWCase)) {
        int.haz.tst <- nativeOutput$ibgWCase
    } else {
        int.haz.tst <- NULL
    }
    if (!is.null(nativeOutput$absWCaseTimeLeft)) {
      int.haz.left <- .inverse.time(nativeOutput$absWCaseTimeLeft, time.map)
    } else {
      int.haz.left <- NULL
    }
    if (!is.null(nativeOutput$absWCaseTimeRight)) {
      int.haz.right <- .inverse.time(nativeOutput$absWCaseTimeRight, time.map)
    } else {
      int.haz.right <- NULL
    }
    ## We use the existing oob data structure to output the test case counts and ids.
    ttmbrCaseCt = nativeOutput$tombrCaseCt
    ttmbrCaseId = nativeOutput$tombrCaseId
  }
  if (!is.null(nativeOutput$tNelsonAalen)) {
      t.chf <- vector("list", ntree)
      offset = 0
      for (i in 1:ntree) {
        t.chf[[i]] <- matrix(nativeOutput$tNelsonAalen[(offset+1):(offset + (nativeOutput$leafCount[i] * length(event.info$time.interest)))],
                                    c(nativeOutput$leafCount[i], length(event.info$time.interest)), byrow=TRUE)
          offset = offset + (nativeOutput$leafCount[i] * length(event.info$time.interest))
      }
  } else {
      t.chf <- NULL
  }
  if (!is.null(nativeOutput$tHazard)) {
      t.hazard <- vector("list", ntree)
      offset = 0
      for (i in 1:ntree) {
        t.hazard[[i]] <- matrix(nativeOutput$tHazard[(offset+1):(offset + (nativeOutput$leafCount[i] * length(event.info$time.interest)))],
                                    c(nativeOutput$leafCount[i], length(event.info$time.interest)), byrow=TRUE)
          offset = offset + (nativeOutput$leafCount[i] * length(event.info$time.interest))
      }
  } else {
      t.hazard <- NULL
  }
  if (!is.null(nativeOutput$tHazardTimeCnt)) {
      t.haz.time.cnt  <- nativeOutput$tHazardTimeCnt
  }
  else {
      t.haz.time.cnt  <- NULL
  }
  if (!is.null(nativeOutput$tHazardTimeIdx)) {
      t.haz.time.idx <- vector("list", n.observed)
      offset = 0
      for (i in 1:n.observed) {
          if (t.haz.time.cnt[i] != 0) {
              t.haz.time.idx[[i]]  <- nativeOutput$tHazardTimeIdx[(offset+1):(offset + t.haz.time.cnt[i])]
              offset = offset + t.haz.time.cnt[i]
          }
          else {
              t.haz.time.idx[[i]] <- NA_integer_
          }
      }
  }
  else {
      t.haz.time.idx  <- NULL
  }
  ## set this to NULL 
  err.rate <- NULL
  ## scale values back to original time scale
  hz.scale <- .hazard.scale(event.info$time.interest, time.map)
  if (!is.null(hazard.ibg)) hazard.ibg <- sweep(hazard.ibg, 2L, hz.scale, "*")
  if (!is.null(hazard.oob)) hazard.oob <- sweep(hazard.oob, 2L, hz.scale, "*")
  if (!is.null(hazard.tst)) hazard.tst <- sweep(hazard.tst, 2L, hz.scale, "*")
  if (!is.null(t.hazard)) {
    t.hazard <- lapply(t.hazard, function(h) sweep(h, 2L, hz.scale, "*"))
  }
  ## DO NOT rescale chf or t.chf
  ## make the output object
  rhfOutput <- list(
    forest = object,
    family = family,
    n = n.observed,
    ntree = ntree,
    yvar = if (restore.mode) as.data.frame(.scale.yvar(yvar, time.map)) else as.data.frame(.scale.yvar(yvar.newdata, time.map)),
    xvar = if (restore.mode) xvar else xvar.newdata,
    xvar.time = object$xvar.time, 
    hcut = hcut,
    max.time = max.time,
    time.map = time.map,
    time.interest = .inverse.time(event.info$time.interest, time.map),
    event.info = event.info,
    ensemble.id = ensemble.id,
    hazard.inbag = hazard.ibg,
    hazard.oob = hazard.oob,
    hazard.test = hazard.tst,
    chf.inbag = chf.ibg,
    chf.oob   = chf.oob,
    chf.test = chf.tst,
    risk.inbag = risk.ibg,
    risk.oob   = risk.oob,
    risk.test  = risk.tst,
    int.haz.inbag = int.haz.ibg,
    int.haz.oob   = int.haz.oob,
    int.haz.test  = int.haz.tst,
    int.haz.left = int.haz.left,
    int.haz.right = int.haz.right,
    t.chf = t.chf,
    t.hazard = t.hazard,
    t.haz.time.cnt = t.haz.time.cnt,
    t.haz.time.idx = t.haz.time.idx,
    ttmbrCaseCt = ttmbrCaseCt,
    ttmbrCaseId = ttmbrCaseId,
    id = if (restore.mode) id else subj.newdata,
    pseudo.membership = pseudo.membership,
    inbag = inbag.out,
    err.rate = err.rate,
    ctime.internal = nativeOutput$cTimeInternal,
    ctime.external = ctime.external.stop - ctime.external.start
  )
  ## memory management
  nativeOutput$leafCount <- NULL
  remove(object)
  class(rhfOutput) <- c("rhf", "predict",   family)
  return(rhfOutput)
}
