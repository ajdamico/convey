#' Zenga index
#'
#' Estimate the Zenga index, a measure of inequality
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
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
#' @author Djalma Pessoa, Guilherme Jacob, and Anthony Damico
#'
#' @seealso \code{\link{svygini}}
#'
#' @references Lucio Barabesi, Giancarlo Diana and Pier Francesco Perri (2016). Linearization of inequality indices in the design-based framework. Statistics, 50(5), 1161-1172.
#' URL \url{https://www.tandfonline.com/doi/pdf/10.1080/02331888.2015.1135924}.
#'
#' Matti Langel and Yves Tille (2012). Inference by linearization for Zenga's new inequality index: a comparison with the Gini index.
#' Metrika, 75, 1093-1110. URL \url{https://doi.org/10.1007/s00184-011-0369-1}.
#'
#' Matti Langel (2012). Measuring inequality in finite population sampling.
#' PhD thesis: Universite de Neuchatel,
#' URL \url{https://doc.rero.ch/record/29204/files/00002252.pdf}.
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
#' svyzenga( ~eqincome , design = des_eusilc )
#'
#' # replicate-weighted design
#' des_eusilc_rep <- as.svrepdesign( des_eusilc , type = "bootstrap" )
#' des_eusilc_rep <- convey_prep(des_eusilc_rep)
#'
#' svyzenga( ~eqincome , design = des_eusilc_rep )
#'
#' \dontrun{
#'
#' # linearized design using a variable with missings
#' svyzenga( ~ py010n , design = des_eusilc )
#' svyzenga( ~ py010n , design = des_eusilc , na.rm = TRUE )
#' # replicate-weighted design using a variable with missings
#' svyzenga( ~ py010n , design = des_eusilc_rep )
#' svyzenga( ~ py010n , design = des_eusilc_rep , na.rm = TRUE )
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
#' svyzenga( ~ eqincome , design = dbd_eusilc )
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' dbDisconnect( conn , shutdown = TRUE )
#'
#' }
#'
#' @export
svyzenga <-
  function(formula, design, ...) {
    if (length(attr(terms.formula(formula) , "term.labels")) > 1)
      stop(
        "convey package functions currently only support one variable in the `formula=` argument"
      )

    UseMethod("svyzenga", design)

  }


#' @rdname svyzenga
#' @export
svyzenga.survey.design <-
  function(formula ,
           design ,
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

    # check for negative incomes
    if (any(incvar[w != 0] < 0, na.rm = TRUE))
      stop("The Zenga index is defined for non-negative numeric variables only.")

    # compute point estimate
    estimate <- CalcZenga(incvar , w)

    # compute linearized function
    lin <- CalcZenga_IF(incvar , w)

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
             nrow = length(w) ,
             dimnames = list(names(w) , strsplit(as.character(formula)[[2]] , ' \\+ ')[[1]]))

    # build result object
    rval <- estimate
    names(rval) <-
      strsplit(as.character(formula)[[2]] , ' \\+ ')[[1]]
    class(rval) <- c("cvystat" , "svystat")
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "zenga"
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

#' @rdname svyzenga
#' @export
svyzenga.svyrep.design <-
  function(formula ,
           design ,
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

    # collect sampling weights
    ws <- weights(design, "sampling")

    # check for negative incomes
    if (any(incvar[ws != 0] < 0, na.rm = TRUE))
      stop("The Zenga index is defined for non-negative numeric variables only.")

    # compute point estimate
    estimate <- CalcZenga(incvar, ws)

    # collect analysis weights
    ww <- weights(design, "analysis")

    # compute replicates
    qq <- apply(ww, 2, function(wi)
      CalcZenga(incvar, wi))

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
      lin <- CalcZenga_IF(incvar , ws)

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
               nrow = length(ws) ,
               dimnames = list(names(ws) , strsplit(as.character(formula)[[2]] , ' \\+ ')[[1]]))

    }

    # build result object
    rval <- estimate
    names(rval) <-
      strsplit(as.character(formula)[[2]] , ' \\+ ')[[1]]
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "zenga"
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


#' @rdname svyzenga
#' @export
svyzenga.DBIsvydesign <-
  function (formula, design, ...) {
    design$variables <-
      getvars(
        formula,
        design$db$connection,
        design$db$tablename,
        updates = design$updates,
        subset = design$subset
      )

    NextMethod("svyzenga", design)
  }

# BDP2016 estimator
CalcZenga <- function(y.k , w.k) {
  # filter observation
  y.k <- y.k[w.k != 0]
  w.k <- w.k[w.k != 0]

  # reorder observations
  ordy <- order(y.k)
  y.k <- y.k[ordy]
  w.k <- w.k[ordy]

  # compute intermediate statistical functionals
  N <- sum(w.k)
  Tot <- sum(w.k * y.k)
  Hy <- cumsum(w.k)
  Ky <- (Tot - cumsum(w.k * y.k)) + w.k * y.k

  # compute zenga
  1 - sum(w.k * (N - Hy) * (Tot - Ky) / (N * Hy * Ky))

}

# BDP2016 estimator
CalcZenga_IF <- function(y.k , w.k) {
  # treat null weights
  y.k <- ifelse(w.k == 0 , 0 , y.k)

  # reorder observations
  ordy <- order(y.k)
  y.k <- y.k[ordy]
  w.k <- w.k[ordy]

  # filter observations
  wf.k <- w.k
  y.k <- y.k[wf.k != 0]
  w.k <- wf.k[wf.k != 0]

  # compute intermediate statistical functionals
  N <- sum(w.k)
  Tot <- sum(w.k * y.k)
  Hy <- cumsum(w.k)
  Ky <- (Tot - cumsum(w.k * y.k)) + w.k * y.k

  # compute linearized variable
  T1 <- -(N - Hy) * (Tot - Ky) / (N * Hy * Ky)
  T2 <- -sum(w.k * (Tot - Ky) / Ky) / N ^ 2
  T3 <- -(y.k / N) * sum(w.k * (N - Hy) / (Hy * Ky))
  T4 <-
    sum(w.k * (Tot - Ky) / (Hy ^ 2 * Ky)) - cumsum(w.k * (Tot - Ky) / (Hy ^
                                                                         2 * Ky)) + (w.k * (Tot - Ky) / (Hy ^ 2 * Ky))
  T5 <- (Tot * y.k / N) * cumsum(w.k * (N - Hy) / (Hy * Ky ^ 2))
  lin.domain <- rowSums(cbind(T1 , T2 , T3 , T4 , T5))

  # return linearized variable
  lin <- rep(0 , length(wf.k))
  lin[wf.k != 0] <- lin.domain
  lin <- lin[order(ordy)]
  lin

}
