#' At-risk-of-poverty rate
#'
#' Estimate the proportion of persons with income below the at-risk-of-poverty threshold.
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param quantiles income quantile, usually .50 (median)
#' @param percent fraction of the quantile, usually .60
#' @param na.rm Should cases with missing values be dropped?
#' @param ... arguments passed on to `svyarpt`
#'
#' @details you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
#'
#' @return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of the statistic.
#'
#' @author Djalma Pessoa and Anthony Damico
#'
#' @seealso \code{\link{svyarpt}}
#'
#' @references Guillaume Osier (2009). Variance estimation for complex indicators
#' of poverty and inequality. \emph{Journal of the European Survey Research
#' Association}, Vol.3, No.3, pp. 167-195,
#' ISSN 1864-3361, URL \url{https://ojs.ub.uni-konstanz.de/srm/article/view/369}.
#'
#' Jean-Claude Deville (1999). Variance estimation for complex statistics and estimators:
#' linearization and residual techniques. Survey Methodology, 25, 193-203,
#' URL \url{https://www150.statcan.gc.ca/n1/en/catalogue/12-001-X19990024882}.
#'
#' @keywords survey
#'
#' @examples
#'
#' library(survey)
#' library(laeken)
#' data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )
#'
#' # linearized design
#' des_eusilc <- svydesign( ids = ~rb030 , strata = ~db040 ,  weights = ~rb050 , data = eusilc )
#' des_eusilc <- convey_prep( des_eusilc )
#'
#' svyarpr( ~eqincome , design = des_eusilc )
#'
#' # replicate-weighted design
#' des_eusilc_rep <- as.svrepdesign( des_eusilc , type = "bootstrap" )
#' des_eusilc_rep <- convey_prep( des_eusilc_rep )
#'
#' svyarpr( ~eqincome , design = des_eusilc_rep )
#'
#' \dontrun{
#'
#' # linearized design using a variable with missings
#' svyarpr( ~ py010n , design = des_eusilc )
#' svyarpr( ~ py010n , design = des_eusilc , na.rm = TRUE )
#' # replicate-weighted design using a variable with missings
#' svyarpr( ~ py010n , design = des_eusilc_rep )
#' svyarpr( ~ py010n , design = des_eusilc_rep , na.rm = TRUE )
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
#' svyarpr( ~ eqincome , design = dbd_eusilc )
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' dbDisconnect( conn , shutdown = TRUE )
#'
#' }
#'
#' @export
svyarpr <- function(formula, design, ...) {
  if (length(attr(terms.formula(formula) , "term.labels")) > 1)
    stop(
      "convey package functions currently only support one variable in the `formula=` argument"
    )

  UseMethod("svyarpr", design)

}

#' @rdname svyarpr
#' @export
svyarpr.survey.design <-
  function(formula,
           design,
           quantiles = 0.5,
           percent = 0.6,
           na.rm = FALSE,
           ...) {
    if (is.null(attr(design, "full_design")))
      stop(
        "you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svydesign() function."
      )


    # if the class of the full_design attribute is just a TRUE, then the design is
    # already the full design.  otherwise, pull the full_design from that attribute.
    if ("logical" %in% class(attr(design, "full_design")))
      full_design <-
        design
    else
      full_design <- attr(design, "full_design")

    # domain
    incvar <-
      model.frame(formula, design$variables, na.action = na.pass)[[1]]

    if (na.rm) {
      nas <- is.na(incvar)
      design <- design[!nas,]
      if (length(nas) > length(design$prob))
        incvar <- incvar[!nas]
      else
        incvar[nas] <- 0
    }

    if (is.null(names(design$prob)))
      ind <-
      as.character(seq(length(design$prob)))
    else
      ind <- names(design$prob)

    w <- 1 / design$prob
    N <- sum(w)

    # if the class of the full_design attribute is just a TRUE, then the design is
    # already the full design.  otherwise, pull the full_design from that attribute.
    if ("logical" %in% class(attr(design, "full_design")))
      full_design <-
      design
    else
      full_design <- attr(design, "full_design")
    incvec <-
      model.frame(formula, full_design$variables, na.action = na.pass)[[1]]

    if (na.rm) {
      nas <- is.na(incvec)
      full_design <- full_design[!nas,]
      if (length(nas) > length(full_design$prob))
        incvec <- incvec[!nas]
      else
        incvec[nas] <- 0
    }


    if (is.null(names(full_design$prob)))
      ncom <-
      as.character(seq(length(full_design$prob)))
    else
      ncom <- names(full_design$prob)


    wf <- 1 / full_design$prob

    # VARDPOOR replication would not use this:
    htot <- h_fun(incvar, w)

    ARPT <-
      svyarpt(
        formula = formula,
        design = full_design,
        quantiles = quantiles,
        percent = percent,
        na.rm = na.rm,
        ...
      )
    arptv <- coef(ARPT)
    arptlin <- attr(ARPT, "lin")

    # value of arpr and first term of lin
    poor <- incvar <= arptv
    rval <- sum(poor * w) / N
    if (sum(1 / design$prob == 0) > 0)
      ID <- 1 * (1 / design$prob != 0)
    else
      ID <- 1 * (ncom %in% ind)
    arpr1lin <- (1 / N) * ID * ((incvec <= arptv) - rval)

    # VARDPOOR replication instead uses h for the domain sample:
    # htot <- h_fun( incvar , w )

    Fprime <-
      densfun(
        formula = formula,
        design = design ,


        arptv,
        # VARDPOOR replication divides by the percent here:
        # arptv/percent , # on the quantile, not the threshold (differs from Osier 2009)

        h = htot,
        FUN = "F",
        na.rm = na.rm
      )

    # combine linearization terms
    arprlin <- arpr1lin + Fprime * arptlin

    # To understand why, notice that arpr1lin is the *domain* poverty rate
    # assuming we know the poverty threshold, defined over the entire population.
    # Now, Fprime works like a derivative of the *domain* poverty rate wrt the
    # *full sample* estimated threshold.
    # It "propagates" the uncertainty expressed in arptlin over to the arprlin.
    #
    # This highlights three issues:
    #   1. The domain indicator should be used only for the domain poverty rate term,
    #      as observations outside the domain still influence the ARPT estimation.
    #   2. Fprime should be computed on the domain sample, since we are
    #      interested in the impact on the domain poverty rate;
    #   3. Fprime is the derivative wrt to the estimated threshold, not the
    #      estimated quantile.
    #
    # The issue on (3) is the reason why convey differs from vardpoor here.

    variance <-
      survey::svyrecvar(
        arprlin / full_design$prob,
        full_design$cluster,
        full_design$strata,
        full_design$fpc,
        postStrata = full_design$postStrata
      )

    colnames(variance) <-
      rownames(variance) <-
      names(rval) <-
      strsplit(as.character(formula)[[2]] , ' \\+ ')[[1]]
    class(rval) <- c("cvystat" , "svystat")
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "arpr"
    attr(rval, "lin") <- arprlin
    rval
  }



#' @rdname svyarpr
#' @export
svyarpr.svyrep.design <-
  function(formula,
           design,
           quantiles = 0.5,
           percent = 0.6,
           na.rm = FALSE,
           ...) {
    if (is.null(attr(design, "full_design")))
      stop(
        "you must run the ?convey_prep function on your replicate-weighted survey design object immediately after creating it with the svrepdesign() function."
      )


    # if the class of the full_design attribute is just a TRUE, then the design is
    # already the full design.  otherwise, pull the full_design from that attribute.
    if ("logical" %in% class(attr(design, "full_design")))
      full_design <-
        design
    else
      full_design <- attr(design, "full_design")

    df <- model.frame(design)
    incvar <-
      model.frame(formula, design$variables, na.action = na.pass)[[1]]

    if (na.rm) {
      nas <- is.na(incvar)
      design <- design[!nas,]
      df <- model.frame(design)
      incvar <- incvar[!nas]
    }

    ws <- weights(design, "sampling")

    df_full <- model.frame(full_design)
    incvec <-
      model.frame(formula, full_design$variables, na.action = na.pass)[[1]]

    if (na.rm) {
      nas <- is.na(incvec)
      full_design <- full_design[!nas,]
      df_full <- model.frame(full_design)
      incvec <- incvec[!nas]
    }

    wsf <- weights(full_design, "sampling")
    names(incvec) <- names(wsf) <- row.names(df_full)
    ind <- row.names(df)

    ComputeArpr <-
      function(xf, wf, ind, quantiles, percent) {
        thresh <- percent * computeQuantiles(xf, wf, p = quantiles)
        sum((xf[ind] <= thresh) * wf[ind]) / sum(wf[ind])
      }

    rval <-
      ComputeArpr(
        xf = incvec,
        wf = wsf,
        ind = ind,
        quantiles = quantiles,
        percent = percent
      )
    wwf <- weights(full_design, "analysis")

    qq <-
      apply(wwf, 2, function(wi) {
        names(wi) <- row.names(df_full)
        ComputeArpr(incvec,
                    wi,
                    ind = ind,
                    quantiles = quantiles,
                    percent = percent)
      })
    if (anyNA(qq))
      variance <- NA
    else
      variance <-
      survey::svrVar(qq,
                     design$scale,
                     design$rscales,
                     mse = design$mse,
                     coef = rval)

    variance <- as.matrix(variance)

    colnames(variance) <-
      rownames(variance) <-
      names(rval) <-
      strsplit(as.character(formula)[[2]] , ' \\+ ')[[1]]
    class(rval) <- c("cvystat" , "svrepstat")
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "arpr"
    rval
  }

#' @rdname svyarpr
#' @export
svyarpr.DBIsvydesign <-
  function (formula, design, ...) {
    if (!("logical" %in% class(attr(design, "full_design")))) {
      full_design <- attr(design , "full_design")

      full_design$variables <-
        getvars(
          formula,
          attr(design , "full_design")$db$connection,
          attr(design , "full_design")$db$tablename,
          updates = attr(design , "full_design")$updates,
          subset = attr(design , "full_design")$subset
        )

      attr(design , "full_design") <- full_design

      rm(full_design)

    }

    design$variables <-
      getvars(
        formula,
        design$db$connection,
        design$db$tablename,
        updates = design$updates,
        subset = design$subset
      )

    NextMethod("svyarpr", design)
  }
