###################################################################
### Hazard simulation machinery 
###################################################################
####################################################################
## basic theory
## given for t, but applies for x-variables by conditioning on x
##
## relationship between F and CHF
## F(t) = 1 - exp(-H(t))
##
## probability integral transform
## Y           ~  F
## F(Y)        ~  U[0,1]
## 1 - F(Y)    ~  U[0,1]
## exp(-H(T))  ~  U[0,1]
## H(T)        ~  -log(U[0,1])
## T           ~  H^{-1}(-log{U[0,1]})
##
## illustration for Cox PH model
## h(t|x) = h_0(t) exp(b x)
## H(t|x) = H_0(t) exp(b x)
## T|x ~ H_0^{-1}(-log{U[0,1]} * exp(-b x))
##
## time scaling convention (used below)
## Let t* = s * t be scaled time with scale factor s.
## Then h*(t*) = h(t* / s) / s and H*(t*) = H(t* / s).
## Our code applies this consistently when scale != 1.
####################################################################
hazard.simulation <- function(type = 1,
                              n = 500, p = 10, nrecords = 7,
                              scale = FALSE, ngrid = 1e5, ...) {
  ## Allow numeric or character type specification
  hazard.types <- c("hazard.1", "hazard.2", "hazard.3")
  if (is.numeric(type)) {
    if (type < 1 || type > length(hazard.types)) {
      stop("Invalid numeric type. Must be 1, 2, or 3.")
    }
    type <- hazard.types[type]
  } else {
    type <- match.arg(type, choices = hazard.types)
  }
  ## Dispatch to selected hazard function
  sim_data <- switch(type,
    hazard.1 = hazard.1(n = n, p = p, nrecords = nrecords, scale = scale, ngrid = ngrid, ...),
    hazard.2 = hazard.2(n = n, p = p, nrecords = nrecords, scale = scale, ...),
    hazard.3 = hazard.3(n = n, p = p, nrecords = nrecords, scale = scale, ...)
  )
  sim_data
}
##----------------------------------------------------------
## SIMULATION 1
##
## h(t|x) = (1 + z_2 * t) * exp(z_1 + z_2 * t)
## where z_1 = 1.5 * x_1,  z_2 = 1.5 * (x_4 + x_5)
##
## integration by parts gives
## H(t|x) = t * exp(z_1 + z_2 * t)
##
## write z2(t) = z_2 * t = 1.5 * x_4(t) + 1.5 * x_5(t)
## where x_4(t) = x_4 * t, x_5(t) = x_5 * t
##
## p = dimension
## nrecords = number of records (nr) per case: 1 + Bin(nr - 1, 0.7)
## scale:
##   FALSE -> no scaling
##   TRUE  -> scale times to [0,1] by dividing by max(stop)
##   numeric s -> multiply times by s (e.g., s=365 to map to days)
## ngrid kept for backward compatibility (not used by new inversion)
##----------------------------------------------------------
hazard.1 <- function(n = 500, p = 10, nrecords = 7, scale = FALSE, ngrid = 1e5) {
  ## static covariates
  p <- max(5, p)
  x <- matrix(runif(n * p), n)
  ## numerical inversion for CHF using uniroot with automatic bracketing
  ## Solve t * exp(Z1 + Z2 * t) = -log(U)
  invert.H <- function(Z1, Z2, U, upper = 100) {
    rhs <- -log(U)
    if (Z2 == 0) return(rhs * exp(-Z1))          # closed form when Z2=0
    f   <- function(t) t * exp(Z1 + Z2 * t) - rhs
    ub  <- upper
    ## expand upper bound until f(ub) >= 0 (monotone increasing)
    while (f(ub) < 0) ub <- ub * 2
    stats::uniroot(f, c(0, ub), tol = 1e-10)$root
  }
  ## simulate number of records per case
  if (nrecords > 1) {
    id.nrecords <- 1 + rbinom(n, size = (nrecords - 1), prob = .7)
  } else {
    id.nrecords <- rep(1, n)
  }
  ## build up the data, one case at a time
  dta <- do.call(rbind, lapply(seq_len(n), function(i) {
    ## simulate the event and censoring time 
    x4 <- x[i, 4]
    x5 <- x[i, 5]
    z1 <- 1.5 * x[i, 1]
    z2 <- 1.5 * (x4 + x5)
    Tm <- invert.H(z1, z2, runif(1))
    Ce <- -1.5 * log(runif(1))
    ## observed time 
    observed.time <- pmin(Tm, Ce)
    ## time dependent covariates
    ## sample random points between 0 and the observed time
    tseq <- c(sort(runif(max(1, id.nrecords[i] - 1), 0, observed.time)), observed.time)
    xtd  <- (x4 + x5) * tseq
    ## start and stop times
    start <- c(0, head(tseq, -1))
    stop  <- tseq
    ## event occurrence flags per interval
    event <- 1L * ((Tm <= Ce) & Tm <= stop)
    ## assemble the data for the case
    dta.i <- data.frame(id = i,
                        start = start,
                        stop  = stop,
                        event = event,
                        xtd   = xtd)
    data.frame(dta.i, x = do.call(cbind, lapply(x[i, ], function(xx) rep(xx, id.nrecords[i]))))
  }))
  ## compute scale factor s:
  ## FALSE -> 1, TRUE -> 1 / max(stop), numeric -> that numeric
  s <- if (isTRUE(scale)) {
    1 / max(dta$stop)
  } else if (isFALSE(scale)) {
    1
  } else if (is.numeric(scale) && length(scale) == 1L) {
    as.numeric(scale)
  } else {
    1
  }
  dta$start <- s * dta$start
  dta$stop  <- s * dta$stop
  ## true hazard for data (ID-aligned; accounts for time scaling)
  haz <- function(time, x) {
    ids  <- x[, "id"]
    ## original-time evaluation point for theory-based hazard
    time <- sort(time)
    t0   <- time / s                         # map scaled time back to original
    X    <- x[match(unique(ids), ids), grepl("^x\\.", names(x)), drop = FALSE]
    out  <- do.call(rbind, lapply(seq_len(nrow(X)), function(i) {
      x4 <- X[i, 4]; x5 <- X[i, 5]
      z1 <- 1.5 * X[i, 1]
      z2 <- 1.5 * (x4 + x5)
      h0 <- (1 + z2 * t0) * exp(z1 + z2 * t0)  # hazard in original time
      h0 / s                                   # transform to scaled-time hazard
    }))
    rownames(out) <- as.character(unique(ids))
    out
  }
  list(dta = dta, haz = haz, scale = s)
}
##----------------------------------------------------------
## SIMULATION 2 
##
## if x_1 <= .5 & x_2<=.5:
## h(t|x) = 0 if 0.5 <= t <= 2.5 else h(t|x) = exp(.2 * x_2)
## i.e. H(t|x)= H_0(t) * g1(x), where g_1(x) = exp(.2 * x_2)
##
## H_0(t) = t                      for 0<t<0.5
## H_0(t) = .5                     for 0.5<=t<=2.5
## H_0(t) = t - 2                  for 2.5<t
##
## otherwise if x_1 and x_2 are not as above: 
## h(t|x) = 0 if 2.5 <= t <= 4.5 else h(t|x) = exp(.2 * x_3 + x_4(t))
## where x_4(t) is the time dependent covariate
##
## x_4(t) = x_4 * log(t), x_4 is a 0/1 binary variable
##
## i.e. if  t < 2.5 or  t > 4.5
## h(t|x) = t ^ (x_4) * exp(.2 * x_3)
##
## So H(t|x) = H_0(t) * g2(x), g2(x) = exp(.2 * x_3)
##
## thus if x_4 = 0:
## H_0(t) = t                      for 0<t<2.5
## H_0(t) = 2.5                    for 2.5<=t<=4.5
## H_0(t) = t - 2                  for 4.5<t
## 
## thus if x_4 = 1:
## H_0(t) = 0.5 * t^2                                for 0<t<2.5
## H_0(t) = 3.125                                    for 2.5<=t<=4.5
## H_0(t) = 0.5 t^2 - 7                               for 4.5<t
##----------------------------------------------------------
H2.case1.inverse <- function(x) {
  ifelse(x < 0.5, x,
         ifelse(x == 0.5, 0.5, x + 2))
}
H2.case20.inverse <- function(x) {
  ifelse(x < 2.5, x,
         ifelse(x == 2.5, 2.5, x + 2))
}
H2.case21.inverse <- function(x) {
  ifelse(x < 3.125, sqrt(2 * x),
         ifelse(x == 3.125, 2.5, sqrt(2 * x + 14)))
}
hazard.2 <- function(n = 500, p = 10, nrecords = 7, scale = FALSE) {
  ## static covariates
  p <- max(4, p)
  x <- matrix(runif(n * p), n)
  x[, 4] <- rbinom(n, 1, .8)
  ## simulate number of records per case
  if (nrecords > 1) {
    id.nrecords <- 1 + rbinom(n, size = (nrecords - 1), prob = .7)
  } else {
    id.nrecords <- rep(1, n)
  }
  ## build up the data, one case at a time
  dta <- do.call(rbind, lapply(seq_len(n), function(i) {
    ## simulate the censoring time and event time
    x4  <- x[i, 4]
    x.pt <- x[i, 1] <= 0.5 & x[i, 2] <= 0.5
    ## case 1
    if (x.pt) {
      z  <- -log(runif(1)) * exp(-.2 * x[i, 2])
      Tm <- H2.case1.inverse(z)
    }
    ## case 2
    else {
      z  <- -log(runif(1)) * exp(-.2 * x[i, 3])
      Tm <- if (x4 == 0) H2.case20.inverse(z) else H2.case21.inverse(z)
    }
    ## censoring 
    Ce <- -5.5 * log(runif(1))
    ## observed time
    observed.time <- pmin(Tm, Ce)
    ## time dependent covariates
    ## sample random points between 0 and the observed time
    tseq <- c(sort(runif(max(1, id.nrecords[i] - 1), 0, observed.time)), observed.time)
    eps  <- 1e-8
    xtd  <- x4 * log(pmax(tseq, eps))
    ## start and stop times
    start <- c(0, head(tseq, -1))
    stop  <- tseq
    ## event occurrence flags per interval
    event <- 1L * ((Tm <= Ce) & Tm <= stop)
    ## assemble the data for the case
    dta.i <- data.frame(id    = i,
                        start = start,
                        stop  = stop,
                        event = event,
                        xtd   = xtd)
    data.frame(dta.i, x = do.call(cbind, lapply(x[i, ], function(xx) rep(xx, id.nrecords[i]))))
  }))
  ## compute scale factor s
  s <- if (isTRUE(scale)) {
    1 / max(dta$stop)
  } else if (isFALSE(scale)) {
    1
  } else if (is.numeric(scale) && length(scale) == 1L) {
    as.numeric(scale)
  } else {
    1
  }
  dta$start <- s * dta$start
  dta$stop  <- s * dta$stop
  ## true hazard for data (ID-aligned; accounts for time scaling)
  haz <- function(time, x) {
    ids  <- x[, "id"]
    time <- sort(time)
    t0   <- time / s
    X    <- x[match(unique(ids), ids), grepl("^x\\.", names(x)), drop = FALSE]
    out  <- do.call(rbind, lapply(seq_len(nrow(X)), function(i) {
      x.pt <- X[i, 1] <= 0.5 & X[i, 2] <= 0.5
      sapply(t0, function(tm) {
        if (x.pt) {
          if (0.5 <= tm && tm <= 2.5) 0 else exp(.2 * X[i, 2])
        } else {
          if (2.5 <= tm && tm <= 4.5) 0 else (tm ^ X[i, 4]) * exp(.2 * X[i, 3])
        }
      })
    }))
    ## transform to scaled-time hazard
    out <- out / s
    rownames(out) <- as.character(unique(ids))
    out
  }
  list(dta = dta, haz = haz, scale = s)
}
##----------------------------------------------------------
## SIMULATION 3
##
## Time-varying covariates with stochastic trajectories:
##   X4_i(t) = X4_i + A4_i * t
##   X5_i(t) = X5_i + A5_i * t
## where (A4_i, A5_i) are subject-specific random slopes.
##
## We draw
##   A4_i ~ Normal(a4, a4_sd^2),   A5_i ~ Normal(a5, a5_sd^2),
## independently across subjects. Setting a4_sd = a5_sd = 0 recovers the
## original deterministic-trajectory version.
##
## Hazard:
##   h3(t | X_i(t)) = (1 + t) * exp( beta1 * X1 + beta2 * X2
##                                   + beta3 * X4_i(t) + beta4 * X5_i(t)
##                                   + beta5 * X1 * X4_i(t)
##                                   + beta6 * X2^2 )
##
## Linear predictor:
##   eta_i(t) = theta0_i + lambda_i * t
## where
##   theta0_i = beta1*X1 + beta2*X2 + beta3*X4 + beta4*X5
##              + beta5*X1*X4 + beta6*X2^2
##   lambda_i = beta3*A4_i + beta4*A5_i + beta5*A4_i*X1
##
## CHF:
##   H_i(t) = exp(theta0_i) * J(t; lambda_i),
##   with closed-form J(t; lambda) as below.
##
## Event times obtained by inversion H_i(T_i) = -log U.
##----------------------------------------------------------
hazard.3 <- function(n = 500, p = 10, nrecords = 7, scale = FALSE,
                     a4 = 2.5, a5 = -1.5,
                     a4_sd = 1.25, a5_sd = 1.25) {
  if (a4_sd < 0 || a5_sd < 0) stop("hazard.3: 'a4_sd' and 'a5_sd' must be nonnegative.")
  ## static covariates
  p <- max(5, p)
  x <- matrix(runif(n * p), n)
  ## number of records per subject
  if (nrecords > 1) {
    id.nrecords <- 1 + rbinom(n, size = (nrecords - 1), prob = 0.7)
  } else {
    id.nrecords <- rep(1, n)
  }
  ## coefficients
  beta1 <- 0.5
  beta2 <- -0.5
  beta3 <- 0.8
  beta4 <- 0.5
  beta5 <- 0.7
  beta6 <- 0.5
  ## subject-specific slopes (stochastic trajectories)
  a4_i <- stats::rnorm(n, mean = a4, sd = a4_sd)
  a5_i <- stats::rnorm(n, mean = a5, sd = a5_sd)
  ## optional truncation (helps avoid extreme slopes with small n)
  if (a4_sd > 0) a4_i <- pmin(pmax(a4_i, a4 - 4 * a4_sd), a4 + 4 * a4_sd)
  if (a5_sd > 0) a5_i <- pmin(pmax(a5_i, a5 - 4 * a5_sd), a5 + 4 * a5_sd)
  ## name slopes by id for safe lookup inside haz()
  names(a4_i) <- as.character(seq_len(n))
  names(a5_i) <- as.character(seq_len(n))
  ## subject-specific CHF in original time (unscaled)
  H_fun_i <- function(t, xi, a4i, a5i) {
    if (t <= 0) return(0)
    x1 <- xi[1]
    x2 <- xi[2]
    x4 <- xi[4]
    x5 <- xi[5]
    theta0 <- beta1 * x1 + beta2 * x2 +
              beta3 * x4 + beta4 * x5 +
              beta5 * x1 * x4 + beta6 * x2^2
    lambda_i <- beta3 * a4i + beta4 * a5i + beta5 * a4i * x1
    ## J(t; lambda) = Integral_0^t (1 + u) exp(lambda u) du
    if (abs(lambda_i) < 1e-8) {
      ## limit lambda -> 0
      J <- t + 0.5 * t^2
    } else {
      J <- ((lambda_i * t + lambda_i - 1) * exp(lambda_i * t) - (lambda_i - 1)) / (lambda_i^2)
    }
    exp(theta0) * J
  }
  ## build counting-process data
  dta <- do.call(rbind, lapply(seq_len(n), function(i) {
    xi  <- x[i, ]
    a4i <- a4_i[i]
    a5i <- a5_i[i]
    ## simulate event time via inversion
    Ui     <- runif(1)
    target <- -log(Ui)
    t_upper <- 1
    H_upper <- H_fun_i(t_upper, xi, a4i, a5i)
    while (H_upper < target && t_upper < 1e3) {
      t_upper <- 2 * t_upper
      H_upper <- H_fun_i(t_upper, xi, a4i, a5i)
    }
    if (H_upper < target) {
      Tm <- Inf
    } else {
      Tm <- stats::uniroot(function(t) H_fun_i(t, xi, a4i, a5i) - target,
                           interval = c(0, t_upper),
                           tol = 1e-8)$root
    }
    ## independent censoring
    Ce <- -4 * log(runif(1))
    observed.time <- min(Tm, Ce)
    ## sampling record times
    nrec <- id.nrecords[i]
    if (nrec > 1) {
      tseq <- sort(runif(nrec - 1, 0, observed.time))
      tseq <- c(tseq, observed.time)
    } else {
      tseq <- observed.time
    }
    start <- c(0, head(tseq, -1))
    stop  <- tseq
    event <- as.integer((Tm <= Ce) & (Tm <= stop))
    ## time-varying covariates (original time scale)
    xtd1 <- xi[4] + a4i * tseq
    xtd2 <- xi[5] + a5i * tseq
    dta.i <- data.frame(id    = i,
                        start = start,
                        stop  = stop,
                        event = event,
                        xtd1  = xtd1,
                        xtd2  = xtd2)
    ## replicate static covariates across records
    data.frame(dta.i,
               x = do.call(cbind, lapply(x[i, ], function(xx) rep(xx, nrec))))
  }))
  ## scale factor
  s <- if (isTRUE(scale)) {
    1 / max(dta$stop)
  } else if (isFALSE(scale)) {
    1
  } else if (is.numeric(scale) && length(scale) == 1L) {
    as.numeric(scale)
  } else {
    1
  }
  dta$start <- s * dta$start
  dta$stop  <- s * dta$stop
  ## ------------------------------------------------------
  ## true hazard (scaled time) as a subject x time matrix
  ## ------------------------------------------------------
  haz <- function(time, x) {
    ## time: vector on the scaled time axis
    time <- as.numeric(time)
    if (!length(time)) stop("hazard.3$haz: 'time' must be non-empty.")
    ## original (unscaled) time points
    t0 <- time / s
    ## subject IDs and unique subjects
    ids      <- as.character(x[, "id"])
    id_unq   <- unique(ids)
    n_subj   <- length(id_unq)
    K        <- length(t0)
    ## slopes aligned with id_unq
    a4_vec <- a4_i[id_unq]
    a5_vec <- a5_i[id_unq]
    if (anyNA(a4_vec) || anyNA(a5_vec)) stop("hazard.3$haz: slope lookup failed (id mismatch).")
    ## pull one static covariate row per subject
    Xmat <- as.matrix(x[match(unique(ids), ids), grepl("^x\\.", names(x)), drop = FALSE])
    out <- matrix(NA_real_, nrow = n_subj, ncol = K)
    rownames(out) <- id_unq
    colnames(out) <- NULL   ## keep times implicit; columns aligned with 'time'
    for (i in seq_len(n_subj)) {
      xrow <- Xmat[i, ]
      x1   <- xrow[1]
      x2   <- xrow[2]
      x4   <- xrow[4]
      x5   <- xrow[5]
      theta0 <- as.numeric(beta1 * x1 + beta2 * x2 +
                           beta3 * x4 + beta4 * x5 +
                           beta5 * x1 * x4 + beta6 * x2^2)
      lambda_i <- as.numeric(beta3 * a4_vec[i] + beta4 * a5_vec[i] + beta5 * a4_vec[i] * x1)
      ## hazard in original time
      h0 <- (1 + t0) * exp(theta0 + lambda_i * t0)
      ## transform to scaled-time hazard: h*(t*) = h(t*/s) / s
      out[i, ] <- h0 / s
    }
    out
  }
  list(dta = dta, haz = haz, scale = s)
}
