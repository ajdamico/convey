#' Gini coefficient
#'
#' Estimate the Gini coefficient which is a measure of inequalty
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design}
#' of the library survey
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
#' gini_eqIncome <- svygini(~eqIncome, design=des_eusilc, ncom=nrow(eusilc), comp=TRUE)
#'
#' @export

svygini <- function(formula, design, ...) {
    
    UseMethod("svygini", design)
    
}



#' @rdname svygini
#' @export
svygini.survey.design <- function(formula, design, ncom, comp = TRUE, ...) {
    inc <- terms.formula(formula)[[2]]
    w <- survey::weights(design)
    ind <- names(w)
    df <- model.frame(design)
    incvar <- df[[as.character(inc)]]
    w <- w[order(incvar)]
    incvar <- incvar[order(incvar)]
    # population size
    N <- sum(w)
    # sample size
    n <- length(incvar)
    # total income
    T <- sum(incvar * w)
    # cumulative weight
    r <- cumsum(w)
    Num <- sum((2 * r - 1) * incvar * w)
    Den <- N * T
    # Gini coeficient
    Gini <- (Num/Den) - 1
    # cumulative distribution function
    F <- cumsum(w)/N
    
    # partial weighted function
    G <- cumsum(incvar * w)
    
    # Gini coefficient linearized variable
    lin_gini <- (2 * (T - G + incvar * w + N * (incvar * F)) - incvar - (Gini + 1) * 
        (T + N * incvar))/(N * T)
    # original order lin_gini<- lin_gini[ind] complete 0's
    names(lin_gini) <- names(w)
    lin_gini_comp <- complete(lin_gini, ncom)
    if (comp) 
        res <- lin_gini_comp else res <- lin_gini
    list(gini_coef = Gini, lin = res)
}

#' @rdname svygini
#' @export
svygini.svyrep.design <- function(formula, design, ...) {
    inc <- terms.formula(formula)[[2]]
    df <- model.frame(design)
    incvar <- df[[as.character(inc)]]
    ComputeGini <- function(x, w) {
        w <- w[order(x)]
        x <- x[order(x)]
        N <- sum(w)
        n <- length(x)
        T <- sum(x * w)
        r <- cumsum(w)
        Num <- sum((2 * r - 1) * x * w)
        Den <- N * T
        (Num/Den) - 1
    }
    ws <- survey::weights(design, "sampling")
    rval <- ComputeGini(incvar, ws)
    ww <- survey::weights(design, "analysis")
    qq <- apply(ww, 2, function(wi) ComputeGini(incvar, wi))
    variance <- svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)
    list(value = rval, se = sqrt(variance))
} 




#' @rdname svygini
#' @export
svygini.DBIsvydesign <-
	function (x, design, ...) 
	{
		design$variables <- survey::getvars(x, design$db$connection, design$db$tablename, 
			updates = design$updates, subset = design$subset)
		NextMethod("svygini", design)
	}

 