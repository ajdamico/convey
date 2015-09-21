#' At-risk-of-poverty threshold
#'
#' The standard definition is to use  60\% of the median income.
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design}
#' of the library survey
#' @param order  income quantile order, usually .50 (median)
#' @param percent fraction of the quantile, usually .60
#' @param h bandwidth to estimate the derivative of the cdf function
#' @param ncom length of the income vector for the whole sample
#' @param comp logical variable \code{TRUE} if the inearized variable for domains
#' should be completed with zeros
#' @return a list with two components: the indicator estimate \code{value}
#' and the linearized variable \code{lin}.
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
#' htot <- h_fun(eusilc$eqIncome, eusilc$rb050)
#' des_eusilc <- svydesign(ids=~db040, weights=~rb050, data=eusilc)
#' arpt_eqIncome <-svyarpt(~eqIncome, design=des_eusilc, .5, .6, h = htot,
#' ncom=rownames(eusilc), comp=TRUE)
#'
#' @export
svyarpt <- function(formula, design, ...) {

    UseMethod("svyarpt", design)

}

#' @rdname svyarpt
#' @export
svyarpt.survey.design <- function(formula, design, order = 0.5, percent = 0.6, h,
    ncom, comp, ...) {
    w <- weights(design)
    ind <- names(w)
    quant_val <- survey::svyquantile(x = formula, design = design, quantiles = order, method = "constant")
    quant_val <- as.vector(quant_val)
    ARPT <- percent * quant_val
    lin_ARPT <- percent * iqalpha(formula = formula, design = design, alpha = order,
        h = h, ncom = ncom, comp = FALSE, incvec = NULL)$lin
    names(lin_ARPT) <- ind
    lin_ARPT_comp <- complete(lin_ARPT, ncom)
    if (comp)
        lin <- lin_ARPT_comp else lin <- lin_ARPT
    # attr(ARPT, 'statistic')<- 'arpt' attr(ARPT,
    # 'var')<-survey::svyCprod(lin/design$prob,design$strata, design$cluster[[1]],
    # design$fpc, design$nPSU,design$certainty,design$postStrata)

	
   
	rval <- ARPT

   	# if the 7th function up in the stack was `svyby`..
	if( as.character( sys.call( -7 ) )[ 1 ] == "svyby" ){
		# ..then pull the full function from that design.
		full_design <- eval( quote( design ) , envir = parent.frame() )
	# otherwise use the design passed into the function
	} else full_design <- design

	variance <- ( SE_lin2( lin , full_design ) )^2
 	class(rval) <- "cvystat"
	attr( rval , "var" ) <- variance
	attr( rval , "statistic" ) <- "arpt"
	rval
}

#' @rdname svyarpt
#' @export
svyarpt.svyrep.design <- function(formula, design, order = 0.5, percent = 0.6, ...) {
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
    
	class(rval)<- "cvystat"
	attr( rval , "var" ) <- variance
	attr( rval , "statistic" ) <- "arpt"
	rval
}


#' @rdname svyarpt
#' @export
svyarpt.DBIsvydesign <-
	function (x, design, ...)
	{
		design$variables <- survey:::getvars(x, design$db$connection, design$db$tablename,
			updates = design$updates, subset = design$subset)
		NextMethod("svyarpt", design)
	}


