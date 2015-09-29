#' At-risk-of-poverty threshold
#'
#' The standard definition is to use  60\% of the median income.
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design}
#' of the library survey
#' @param order  income quantile order, usually .50 (median)
#' @param percent fraction of the quantile, usually .60
#' @param comp logical variable \code{TRUE} if the inearized variable for domains
#' should be completed with zeros
#'
#' @return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute #'giving the variance and a "\code{statistic}" attribute giving the name of
#'the statistic.
#'
#' @author Djalma Pessoa and Anthony Damico
#'
#' @seealso \code{\link{arpr}}
#'
#' @references Guillaume Osier (2009). Variance estimation for complex indicators
#'of poverty and inequality. \emph{Journal of the European Survey Research
#' Association}, Vol.3, No.3, pp. 167-195,
#' ISSN 1864-3361, URL \url{http://ojs.ub.uni-konstanz.de/srm/article/view/369}.

#'Jean-Claude Deville (1999). Variance estimation for complex statistics and estimators:
#' linearization and residual techniques. Survey Methodology, 25, 193-203,
#' URL \url{http://www5.statcan.gc.ca/bsolc/olc-cel/olc-cel?lang=eng&catno=12-001-X19990024882}.
#'
#' @keywords survey
#'
#' @examples
#' library(vardpoor)
#' data(eusilc)
#' library(survey)
#' des_eusilc <- svydesign(ids=~db040, weights=~rb050, data=eusilc)
#' des_eusilc <- convey_prep( des_eusilc )
#' arpt_eqIncome <-svyarpt(~eqIncome, design=des_eusilc, .5, .6,comp=TRUE)
#' arpt_eqIncome
#' @export
svyarpt <- function(formula, design, ...) {

    UseMethod("svyarpt", design)

}

#' @rdname svyarpt
#' @export
svyarpt.survey.design <- function(formula, design, order = 0.5, percent = 0.6, comp = TRUE,
    ...) {
    if (is.null(attr(design, "full_design")))
        stop("you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svydesign() function.")

	if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )


		# if the class of the full_design attribute is just a TRUE, then the design is
  # already the full design.  otherwise, pull the full_design from that attribute.
  if ("logical" %in% class(attr(design, "full_design")))
    full_design <- design else full_design <- attr(design, "full_design")
    df_full <- model.frame(full_design)
    inc <- terms.formula(formula)[[2]]
    incvec <- df_full[[as.character(inc)]]
    wf <- weights(full_design)
    htot <- h_fun(incvec, wf)
    w <- weights(design)
    ind <- names(w)
    linqalpha <- iqalpha(formula = formula, design = design, alpha = order, h=htot,
      comp = TRUE,
        compinc = FALSE)
    rval <- percent * linqalpha[1]
    lin <- percent * attr(linqalpha, "lin")
    ncom <- names(weights(full_design))
    # names(lin) <- ind if (comp) lin <- complete(lin, ncom)
    variance <- (SE_lin2(lin, full_design))^2
	colnames( variance ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]

	names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- "cvystat"
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "arpt"
    attr(rval, "lin") <- lin
    rval
}

#' @rdname svyarpt
#' @export
svyarpt.svyrep.design <- function(formula, design, order = 0.5, percent = 0.6, ...) {

	if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

    inc <- terms.formula(formula)[[2]]
    df <- model.frame(design)
    incvar <- df[[as.character(inc)]]
    w <- weights(design, "sampling")
    quant_val <- computeQuantiles(incvar, w, p = order)
    quant_val <- as.vector(quant_val)
    rval <- percent * quant_val
    ww <- weights(design, "analysis")
    qq <- apply(ww, 2, function(wi) 0.6 * computeQuantiles(incvar, wi, p = order))
    variance <- svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)

	names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- "cvystat"
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "arpt"
    rval
}


#' @rdname svyarpt
#' @export
svyarpt.DBIsvydesign <- function(x, design, ...) {
    design$variables <- survey:::getvars(x, design$db$connection, design$db$tablename,
        updates = design$updates, subset = design$subset)
    NextMethod("svyarpt", design)
}


