#' Atkinson index
#'
#' Estimate the Atkinson index, an inequality measure
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param epsilon a parameter that determines the sensivity towards inequality in the bottom of the distribution. Defaults to \code{epsilon = 1}.
#' @param na.rm Should cases with missing values be dropped?
#' @param deff Return the design effect (see \code{survey::svymean})
#' @param linearized Should a matrix of linearized variables be returned
#' @param influence Should a matrix of (weighted) influence functions be returned? (for compatibility with \code{\link[survey]{svyby}})
#' @param return.replicates Return the replicate estimates?
#' @param ... future expansion
#'
#' @details you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
#'
#' @return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of the statistic.
#'
#' @author Guilherme Jacob, Djalma Pessoa and Anthony Damico
#'
#' @seealso \code{\link{svygei}}
#'
#' @references Matti Langel (2012). Measuring inequality in finite population sampling.
#' PhD thesis: Universite de Neuchatel,
#' URL \url{https://doc.rero.ch/record/29204/files/00002252.pdf}.
#'
#' Martin Biewen and Stephen Jenkins (2002). Estimation of Generalized Entropy
#' and Atkinson Inequality Indices from Complex Survey Data. \emph{DIW Discussion Papers},
#' No.345,
#' URL \url{https://www.diw.de/documents/publikationen/73/diw_01.c.40394.de/dp345.pdf}.
#'
#' @keywords survey
#'
#' @examples
#' library(survey)
#' library(laeken)
#' data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )
#'
#' # linearized design
#' des_eusilc <- svydesign( ids = ~rb030 , strata = ~db040 ,  weights = ~rb050 , data = eusilc )
#' des_eusilc <- convey_prep(des_eusilc)
#'
#' # replicate-weighted design
#' des_eusilc_rep <- as.svrepdesign( des_eusilc , type = "bootstrap" )
#' des_eusilc_rep <- convey_prep(des_eusilc_rep)
#'
#'
#' # subset all designs to positive income and non-missing records only
#' des_eusilc_pos_inc <- subset( des_eusilc , eqincome > 0 )
#' des_eusilc_rep_pos_inc <- subset( des_eusilc_rep , eqincome > 0 )
#'
#'
#' # linearized design
#' svyatk( ~eqincome , des_eusilc_pos_inc, epsilon = .5 )
#' svyatk( ~eqincome , des_eusilc_pos_inc )
#' svyatk( ~eqincome , des_eusilc_pos_inc, epsilon = 2 )
#'
#' # replicate-weighted design
#' svyatk( ~eqincome , des_eusilc_rep_pos_inc, epsilon = .5 )
#' svyatk( ~eqincome , des_eusilc_rep_pos_inc )
#' svyatk( ~eqincome , des_eusilc_rep_pos_inc, epsilon = 2 )
#'
#'
#' # subsetting
#' svyatk( ~eqincome , subset(des_eusilc_pos_inc, db040 == "Styria"), epsilon = .5 )
#' svyatk( ~eqincome , subset(des_eusilc_pos_inc, db040 == "Styria") )
#' svyatk( ~eqincome , subset(des_eusilc_pos_inc, db040 == "Styria"), epsilon = 2 )
#'
#' svyatk( ~eqincome , subset(des_eusilc_rep_pos_inc, db040 == "Styria"), epsilon = .5 )
#' svyatk( ~eqincome , subset(des_eusilc_rep_pos_inc, db040 == "Styria") )
#' svyatk( ~eqincome , subset(des_eusilc_rep_pos_inc, db040 == "Styria"), epsilon = 2 )
#'
#' \dontrun{
#'
#' # linearized design using a variable with missings (but subsetted to remove negatives)
#' svyatk( ~py010n , subset(des_eusilc, py010n > 0 | is.na(py010n)), epsilon = .5 )
#' svyatk( ~py010n , subset(des_eusilc, py010n > 0 | is.na(py010n)), epsilon = .5 , na.rm=TRUE )
#'
#' # replicate-weighted design using a variable with missings (but subsetted to remove negatives)
#' svyatk( ~py010n , subset(des_eusilc_rep, py010n > 0 | is.na(py010n)), epsilon = .5 )
#' svyatk( ~py010n , subset(des_eusilc_rep, py010n > 0 | is.na(py010n)), epsilon = .5 , na.rm=TRUE )
#'
#'
#' # database-backed design
#' library(RSQLite)
#' library(DBI)
#' dbfile <- tempfile()
#' conn <- dbConnect( RSQLite::SQLite() , dbfile )
#' dbWriteTable( conn , 'eusilc' , eusilc )
#'
#' dbd_eusilc <-
#' 	svydesign(
#' 		ids = ~rb030 ,
#' 		strata = ~db040 ,
#' 		weights = ~rb050 ,
#' 		data="eusilc",
#' 		dbname=dbfile,
#' 		dbtype="SQLite"
#' 	)
#'
#' dbd_eusilc <- convey_prep( dbd_eusilc )
#'
#'
#' # subset all designs to positive income and non-missing records only
#' dbd_eusilc_pos_inc <- subset( dbd_eusilc , eqincome > 0 )
#'
#'
#' # database-backed linearized design
#' svyatk( ~eqincome , dbd_eusilc_pos_inc, epsilon = .5 )
#' svyatk( ~eqincome , dbd_eusilc_pos_inc )
#' svyatk( ~eqincome , dbd_eusilc_pos_inc, epsilon = 2 )
#'
#' svyatk( ~eqincome , subset(dbd_eusilc_pos_inc, db040 == "Styria"), epsilon = .5 )
#' svyatk( ~eqincome , subset(dbd_eusilc_pos_inc, db040 == "Styria") )
#' svyatk( ~eqincome , subset(dbd_eusilc_pos_inc, db040 == "Styria"), epsilon = 2 )
#'
#' # database-backed linearized design using a variable with missings
#' # (but subsetted to remove negatives)
#' svyatk( ~py010n , subset(dbd_eusilc, py010n > 0 | is.na(py010n)), epsilon = .5 )
#' svyatk( ~py010n , subset(dbd_eusilc, py010n > 0 | is.na(py010n)), epsilon = .5 , na.rm=TRUE )
#'
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' dbDisconnect( conn , shutdown = TRUE )
#'
#' }
#'
#' @export
svyatk <-
  function(formula, design, ...) {
    if (length(attr(terms.formula(formula) , "term.labels")) > 1)
      stop(
        "convey package functions currently only support one variable in the `formula=` argument"
      )

    if ('epsilon' %in% names(list(...)) &&
        list(...)[["epsilon"]] <= 0)
      stop("epsilon= must be positive.")

    UseMethod("svyatk", design)

  }


#' @rdname svyatk
#' @export
svyatk.survey.design <-
  function(formula ,
           design ,
           epsilon = 1 ,
           na.rm = FALSE ,
           deff = FALSE ,
           linearized = FALSE ,
           influence = FALSE ,
           ...) {
    # collect income data
    incvar <-
      model.frame(formula, design$variables, na.action = na.pass)[[1]]

    # treat missing values
    if (na.rm) {
      nas <- is.na(incvar)
      design$prob <- ifelse(nas , Inf , design$prob)
    }

    # collect sampling weights
    w <- 1 / design$prob

    # treat non-positive incomes
    if (any(incvar[w != 0] <= 0))
      stop(
        "The Atkinson Index is defined for strictly positive variables only. Negative and zero values not allowed."
      )

    # compute point estimate
    estimate <- CalcAtkinson(incvar , w , epsilon)

    # compute linearized function
    lin <- CalcAtkinson_IF(incvar , w , epsilon)

    # compute variance
    variance <-
      survey::svyrecvar(
        lin / design$prob,
        design$cluster,
        design$strata,
        design$fpc,
        postStrata = design$postStrata
      )
    variance[which(is.nan(variance))] <- NA
    colnames(variance) <-
      rownames(variance) <-
      strsplit(as.character(formula)[[2]] , ' \\+ ')[[1]]

    # compute deff
    if (is.character(deff) || deff) {
      nobs <- sum(weights(design) != 0)
      npop <- sum(weights(design))
      if (deff == "replace")
        vsrs <-
        survey::svyvar(lin , design, na.rm = na.rm) * npop ^ 2 / nobs
      else
        vsrs <-
        survey::svyvar(lin , design , na.rm = na.rm) * npop ^ 2 * (npop - nobs) /
        (npop * nobs)
      deff.estimate <- variance / vsrs
    }

    # coerce to matrix
    lin <-
      matrix(lin ,
             nrow = length(lin) ,
             dimnames = list(names(lin) , strsplit(as.character(formula)[[2]] , ' \\+ ')[[1]]))

    # build result object
    rval <- estimate
    names(rval) <-
      strsplit(as.character(formula)[[2]] , ' \\+ ')[[1]]
    class(rval) <- c("cvystat" , "svystat")
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "atkinson"
    attr(rval, "epsilon") <- epsilon
    if (linearized)
      attr(rval, "linearized") <- lin
    if (influence)
      attr(rval , "influence")  <-
      sweep(lin , 1 , design$prob , "/")
    if (linearized |
        influence)
      attr(rval , "index") <- as.numeric(rownames(lin))
    if (is.character(deff) ||
        deff)
      attr(rval , "deff") <- deff.estimate
    rval

  }

#' @rdname svyatk
#' @export
svyatk.svyrep.design <-
  function(formula ,
           design ,
           epsilon = 1 ,
           na.rm = FALSE ,
           deff = FALSE ,
           linearized = FALSE ,
           return.replicates = FALSE ,
           ...) {
    # collect data
    incvar <-
      model.frame(formula, design$variables, na.action = na.pass)[[1]]

    # treat missing values
    if (na.rm) {
      nas <- is.na(incvar)
      design <- design[!nas, ]
      incvar <- incvar[!nas]
    }

    # colelct sampling weights
    ws <- weights(design, "sampling")

    # treat non-positive incomes
    if (any(incvar[ws != 0] <= 0))
      stop(
        "The Atkinson Index is defined for strictly positive variables only. Negative and zero values not allowed."
      )

    # compute point estimate
    estimate <- CalcAtkinson(incvar , ws , epsilon)

    # collect analysis weights
    ww <- weights(design, "analysis")

    # compute replicates
    qq <- apply(ww, 2, function(wi)
      CalcAtkinson(incvar , wi , epsilon))

    # compute variance
    if (any(is.na(qq)))
      variance <- as.matrix(NA)
    else {
      variance <-
        survey::svrVar(qq ,
                       design$scale ,
                       design$rscales ,
                       mse = design$mse ,
                       coef = estimate)
      this.mean <- attr(variance , "means")
      variance <- as.matrix(variance)
      attr(variance , "means") <- this.mean
    }
    colnames(variance) <-
      rownames(variance) <-
      strsplit(as.character(formula)[[2]] , ' \\+ ')[[1]]

    # compute deff
    if (is.character(deff) || deff || linearized) {
      # compute linearized function
      lin <- CalcAtkinson_IF(incvar , ws , epsilon)

      # compute deff
      nobs <- length(design$pweights)
      npop <- sum(design$pweights)
      vsrs <-
        unclass(
          survey::svyvar(
            lin ,
            design,
            na.rm = na.rm,
            return.replicates = FALSE,
            estimate.only = TRUE
          )
        ) * npop ^ 2 / nobs
      if (deff != "replace")
        vsrs <- vsrs * (npop - nobs) / npop
      deff.estimate <- variance / vsrs

      # filter observation
      names(lin) <- rownames(design$variables)

      # coerce to matrix
      lin <-
        matrix(lin ,
               nrow = length(lin) ,
               dimnames = list(names(lin) , strsplit(as.character(formula)[[2]] , ' \\+ ')[[1]]))

    }

    # build result object
    rval <- estimate
    names(rval) <-
      strsplit(as.character(formula)[[2]] , ' \\+ ')[[1]]
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "gini"
    if (linearized)
      attr(rval , "linearized") <- lin
    if (linearized)
      attr(rval , "index") <- as.numeric(rownames(lin))

    # keep replicates
    if (return.replicates) {
      attr(qq , "scale") <- design$scale
      attr(qq , "rscales") <- design$rscales
      attr(qq , "mse") <- design$mse
      rval <- list(mean = rval , replicates = qq)
    }

    # add design effect estimate
    if (is.character(deff) ||
        deff)
      attr(rval , "deff") <- deff.estimate

    # return object
    class(rval) <- c("cvystat" , "svrepstat")
    rval

  }


#' @rdname svyatk
#' @export
svyatk.DBIsvydesign <-
  function (formula, design, ...) {
    design$variables <-
      getvars(
        formula,
        design$db$connection,
        design$db$tablename,
        updates = design$updates,
        subset = design$subset
      )

    NextMethod("svyatk", design)
  }



CalcAtkinson <-
  function(yk , wk , epsilon) {
    # filter observations
    yk <- yk[wk != 0]
    wk <- wk[wk != 0]

    # compute intermediate statistics
    U0 <- sum(wk)
    U1 <- sum(wk * yk)
    T0 <- sum(wk * log(yk))
    T1 <- sum(wk * yk * log(yk))
    # compute values
    if (epsilon == 1) {
      res <- 1 - (U0 / U1) * exp(T0 / U0)
    } else {
      U.neps <- sum(wk * yk ^ (1 - epsilon))
      afac <- epsilon / (1 - epsilon)
      bfac <- 1 / (1 - epsilon)
      res <- 1 - (U.neps ^ bfac) / (U0 ^ afac * U1)
    }
    res

  }

CalcAtkinson_IF <-
  function(yk , wk , epsilon) {
    U0 <- sum(wk)
    U1 <- sum(wk * yk)
    T0 <- sum(wk * ifelse(wk != 0 , log(yk) , 0))
    T1 <- sum(wk * ifelse(wk != 0 , yk * log(yk) , 0))
    if (epsilon == 1) {
      A1 <- CalcAtkinson(yk , wk , 1)
      L1 <- (A1 - 1) * (1 - T0 / U0) / U0
      L2 <- yk * (1 - A1) / U1
      L3 <- ifelse(wk != 0 , log(yk) * (A1 - 1) / U0 , 0)
      lin <- rowSums(cbind(L1 , L2 , L3))
    } else {
      U.neps <- sum(wk * ifelse(wk != 0 , yk ^ (1 - epsilon) , 0))
      afac <- epsilon / (1 - epsilon)
      bfac <- 1 / (1 - epsilon)
      L1 <- afac * (U.neps ^ bfac) / (U1 * U0 ^ bfac)
      L2 <- yk * (U.neps ^ bfac) / (U0 ^ afac * U1 ^ 2)
      L3 <-
        -ifelse(wk != 0 , bfac * (yk ^ (1 - epsilon)) * (U.neps ^ afac) / (U0 ^
                                                                             afac * U1) , 0)
      lin <- rowSums(cbind(L1 , L2 , L3))
    }
    lin <- ifelse(wk != 0 , lin , 0)
    lin
  }
