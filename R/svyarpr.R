#' At-risk-of-poverty rate
#'
#' Estimate the proportion of persons with income below the
#' at-risk-of-poverty threshold.
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design}
#' of the library survey
#' @param order  income quantile order, usually .5
#' @param percent fraction of the quantile, usually .60
#' @param comp logical variable \code{TRUE} if the linearized variable for domains
#' should be completed with zeros
#'
#' @return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of the statistic.
#'
#' @author Djalma Pessoa and Anthony Damico
#' @seealso \code{\link{arpt}}
#'
#' @references Guillaume Osier (2009). Variance estimation for complex indicators
#'of poverty and inequality. \emph{Journal of the European Survey Research
#' Association}, Vol.3, No.3, pp. 167-195,
#' ISSN 1864-3361, URL \url{http://ojs.ub.uni-konstanz.de/srm/article/view/369}.

#'Jean-Claude Deville (1999). Variance estimation for complex statistics and estimators:
#'linearization and residual techniques. Survey Methodology, 25, 193-203,
#' URL \url{http://www5.statcan.gc.ca/bsolc/olc-cel/olc-cel?lang=eng&catno=12-001-X19990024882}.
#'
#' @keywords survey
#'
#' @examples
#' library(survey)
#' library(vardpoor)
#' data(eusilc)
#'
#' # linearized design
#' des_eusilc <- svydesign( ids = ~rb030 , strata = ~db040 ,  weights = ~rb050 , data = eusilc )
#' des_eusilc <- convey_prep( des_eusilc )
#'
#' svyarpr( ~eqIncome , design = des_eusilc )
#'
#' # replicate-weighted design
#' des_eusilc_rep <- as.svrepdesign( des_eusilc , type = "bootstrap" )
#' des_eusilc_rep <- convey_prep( des_eusilc_rep )
#' svyarpr( ~eqIncome , design = des_eusilc_rep )
#' @export
#'
svyarpr <- function(formula, design, ...) {

    UseMethod("svyarpr", design)

}

#' @rdname svyarpr
#' @export
svyarpr.survey.design <- function(formula, design, order = 0.5, percent = 0.6, comp = TRUE,
  ...) {

  if (is.null(attr(design, "full_design")))
    stop("you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svydesign() function.")

  if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )


  # if the class of the full_design attribute is just a TRUE, then the design is
  # already the full design.  otherwise, pull the full_design from that attribute.
  if ("logical" %in% class(attr(design, "full_design")))
    full_design <- design else full_design <- attr(design, "full_design")

    # domain
    inc <- terms.formula(formula)[[2]]
    df <- model.frame(design)
    incvar <- df[[as.character(inc)]]
    w <- weights(design)
    N <- sum(w)
    ind <- row.names(df)

    # if the class of the full_design attribute is just a TRUE, then the design is
    # already the full design.  otherwise, pull the full_design from that attribute.
    if ("logical" %in% class(attr(design, "full_design")))
      full_design <- design else full_design <- attr(design, "full_design")


    df_full <- model.frame(full_design)
    incvec <- df_full[[as.character(inc)]]
    wf <- weights(full_design)
    ncom <- row.names(df_full)
    htot <- h_fun(incvec, wf)
    ARPT <- svyarpt(formula = formula, full_design, order = 0.5, percent = 0.6)
    arptv <- ARPT[1]
    arptlin <- attr(ARPT, "lin")
    # value of arpr and first term of lin
    poor<- incvar<=arptv
    rval <- sum(poor*w)/N
    arpr1lin <- (1/N)*((incvar<=arptv)-rval)
    names(arpr1lin)<- ind
    arpr1lin<- complete(arpr1lin,ncom )
    # use h for the whole sample
    Fprime <- densfun(formula = formula, design = design, arptv, h=htot, fun = "F")
    arprlin <- arpr1lin + Fprime * arptlin
    variance <- (SE_lin2(arprlin, full_design))^2
    colnames( variance ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]

    rownames( variance ) <- names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- "cvystat"
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "arpr"
    attr(rval, "lin") <- arprlin
    rval
}



#' @rdname svyarpr
#' @export
svyarpr.svyrep.design <- function(formula, design, order = 0.5, percent = 0.6, ...) {

	convey_prep_needs_to_be_run <- ( "svyrep.design" %in% class( design ) & "survey.design" %in% class( attr( design , "full_design" ) ) ) | is.null(attr(design, "full_design"))

  if (convey_prep_needs_to_be_run)
    stop("you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svrepdesign() or as.svrepdesign() functions.")

  if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )


  # if the class of the full_design attribute is just a TRUE, then the design is
  # already the full design.  otherwise, pull the full_design from that attribute.
  if ("logical" %in% class(attr(design, "full_design")))
    full_design <- design else full_design <- attr(design, "full_design")

    inc <- terms.formula(formula)[[2]]
    df <- model.frame(design)
    incvar <- df[[as.character(inc)]]
    ws <- weights(design, "sampling")
    ComputeArpr <- function(x, w, order, percent) {
        tresh <- percent * computeQuantiles(x, w, p = order)
        sum((incvar <= tresh) * w)/sum(w)
    }
    rval <- ComputeArpr(x = incvar, w = ws, order = order, percent = percent)
    ww <- weights(design, "analysis")
    qq <- apply(ww, 2, function(wi) 0.6 * ComputeArpr(incvar, wi, order = order,
        percent = percent))
    variance <- svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)

	variance <- as.matrix( variance )

	rownames( variance ) <- names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- "cvystat"
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "arpr"
    rval
}


#' @rdname svyarpr
#' @export
svyarpr.DBIsvydesign <- function(x, design, ...) {
    design$variables <- survey:::getvars(x, design$db$connection, design$db$tablename,
        updates = design$updates, subset = design$subset)
    NextMethod("svyarpr", design)
}

