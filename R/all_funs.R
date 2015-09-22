#'Complete the linearezed values with 0's
#'
#' Complete the vector of the linearized value of an indicator for a domain with
#' 0 values coresponding to the other domains
#'
#' @param x vetor with linearized values for the domain
#' @param n the length of the vector forthe whole sample
#'
#' @return vector with 0 in the elements outside the domain
#' @author Djalma Pessoa and Anthony Damico
#' @keywords survey
#' @export


complete <- function(x, ind.all) {
  x.comp <- rep(0, length(ind.all))
  names(x.comp) <- ind.all
  x.comp[names(x)] <- x
  x.comp
}

#'Computes the bandwidth needed to compute the derivative of the cdf function
#'
#'Using the whole sample, computes the bandwith used to get the linearized variable
#'
#' @param inc_var income variable used in the estimation of the indicators
#' @param w vector of design weights
#' @return value of the bandwidth
#' @author Djalma Pessoa and Anthony Damico
#' @keywords survey
#' @export


h_fun <- function(inc_var, w) {
    N <- sum(w)
    sd_inc <- sqrt((sum(w * inc_var * inc_var) - sum(w * inc_var) * sum(w * inc_var)/N)/N)
    h <- sd_inc/exp(0.2 * log(sum(w)))
    h
}

#'Estimate the derivative of the cdf function using kernel estimator
#'
#'computes the derivative of a function in a point using kernel estimation
#'
#'@param formula a formula specifying the income variable
#'@param design a design object of class \code{survey.design} of the library survey
#'@param x the point where the derivative is calculated
#'@param htot value of the bandwidth based on the whole sample
#'@param if \code{F} estimates the derivative of the cdf function; if \code{S} estimates
#'the derivative of total in the tails of the distribution
#'@return the value of the derivative at \code{x}
#'@author Djalma Pessoa and Anthony Damico
#'@keywords survey
#'@export

densfun <- function(formula, design, x, htot = NULL, fun = c("F", "S"), ...) {
    inc <- terms.formula(formula)[[2]]
    w <- weights(design)
    N <- sum(w)
    df <- model.frame(design)
    inc_var <- df[[as.character(inc)]]
    sd_inc <- sqrt((sum(w * inc_var * inc_var) - sum(w * inc_var) * sum(w * inc_var)/N)/N)
    h <- sd_inc/exp(0.2 * log(sum(w)))
    if (!is.null(htot))
        h <- htot
    u <- (x - inc_var)/h
    vectf <- exp(-(u^2)/2)/sqrt(2 * pi)
    if (fun == "F")
        res <- sum(vectf * w)/(N * h) else {
        v <- w * inc_var
        res <- sum(vectf * v)/h
    }
    res
}

#' Linearization of the cdf function of a variable
#'
#' Computes the linearized variable of the cdf function in a point.
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design}
#' of the library survey
#' @param ncom length of the income vector for the whole sample
#' @param comp logical variable \code{TRUE} if the linearized variable for domains
#' should be completed with zeros
#' @return a list with two components: the cdf estimate \code{value}
#' and the linearized cdf variable \code{lin}.
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
#' icdf_eqIncome <-icdf(~eqIncome, design=des_eusilc, 20000, ncom=rownames(eusilc), comp=TRUE)
#'icdf_eqIncome$value
#' @export

icdf <- function(formula, design, x, ncom, comp, ...) {
    inc <- terms.formula(formula)[[2]]
    df <- model.frame(design)
    incvar <- df[[as.character(inc)]]
    w <- weights(design)
    ind <- names(w)
    N <- sum(w)
    poor <- (incvar <= x) * 1
    design <- update(design, poor = poor)
    # rate of poor
    cdf_fun <- coef( survey::svymean( poor , design ) )
    inf_fun <- (1/N) * ((incvar <= x) - cdf_fun)
    names(inf_fun) <- ind
    inf_fun_comp <- complete(inf_fun, ncom)
    if (comp)
        lin <- inf_fun_comp else lin <- inf_fun
    list(value = cdf_fun, lin = lin)
}



#' Linearization of a variable quantile
#'
#' Computes the linearized variable of a quantile of variable.
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design}
#' of the library survey
#' @param alpha the order of the quantile
#' @param ncom length of the income vector for the whole sample
#' @param comp logical variable \code{TRUE} if the linearized variable for domains
#' should be completed with zeros
#' @param invec vector of sample values of the variable whose quantile is to be linearized
#' @return a list with two components: the quantile estimate \code{value}
#' and the linearized quantile variable \code{lin}.
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
#' iqalpha_eqIncome <-iqalpha(~eqIncome, design=des_eusilc, .50, ncom=rownnames(eusilc),
#' comp=TRUE, eusilc$eqIncome )
#'iqalpha_eqIncome$value
#' @export

iqalpha <- function(formula, design, alpha, h = NULL, ncom, comp, incvec = NULL,
    ...) {
    inc <- terms.formula(formula)[[2]]
    df <- model.frame(design)
    incvar <- df[[as.character(inc)]]
    w <- weights(design)
    ind <- names(w)
    q_alpha <- survey::svyquantile(x = formula, design = design, quantiles = alpha, method = "constant")
    q_alpha <- as.vector(q_alpha)
    N <- sum(w)
    Fprime <- densfun(formula = formula, design = design, q_alpha, htot = h, fun = "F")
    iq <- -(1/(N * Fprime)) * ((incvar <= q_alpha) - alpha)
    if (!is.null(incvec)) {
        iq <- -(1/(N * Fprime)) * ((incvec <= q_alpha) - alpha)
        comp <- FALSE
    }
    if (comp) {
        names(iq) <- ind
        iq_comp <- complete(iq, ncom)
        res = iq_comp
    } else res <- iq
    list(value = q_alpha, lin = res)
}

#' Linearization of the total above a quantile or below a quantile
#'
#' Computes the linearized variable of the total in the lower or upper tail of
#' the distribution of a variable.
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design}
#' of the library survey
#' @param alpha the order of the quantile
#' @param type  \code{inf} for the lower tail and \code{sup} for the upper tail
#' @param h  bandwidth to estimate the derivative of the total in the tails
#' @param ncom length of the income vector for the whole sample
#' @param invec vector of sample values of the variable whose quantile is to be linearized
#' @return a list with two components: the tail total estimate \code{value}
#' and the linearized of the tail total \code{lin}.
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
#' isq_eqIncome <-isq(~eqIncome, design=des_eusilc, .80, 'sup', htot,
#' ncom=rownames(eusilc),  eusilc$eqIncome)
#'isq_eqIncome$value
#' @export


isq <- function(formula, design, alpha, type = c("inf", "sup"), h = NULL, ncom, incvec,
    ...) {
    inc <- terms.formula(formula)[[2]]
    df <- model.frame(design)
    incvar <- df[[as.character(inc)]]
    w <- weights(design)
    ind <- names(w)
    q_alpha <- survey::svyquantile(x = formula, design = design, quantiles = alpha, method = "constant")
    q_alpha <- as.vector(q_alpha)
    if (type == "inf") {
        inc_inf <- (incvar <= q_alpha) * incvar
        tot <- sum(inc_inf * w)
    } else {
        inc_sup <- (incvar > q_alpha) * incvar
        tot <- sum(inc_sup * w)
    }
    Fprime <- densfun(formula = formula, design = design, q_alpha, htot = h, fun = "S")
    iq <- iqalpha(formula = formula, design = design, alpha, h = h, ncom = ncom,
        comp = TRUE, incvec = incvec)$lin
    isqalpha <- incvec * ((incvec <= q_alpha)) + Fprime * iq
    if (type == "inf")
        ires <- isqalpha else ires <- incvec - isqalpha
    list(value = tot, lin = ires)
}




computeQuantiles <- function(xx, w, p = quantiles) {
    if (any(is.na(xx)))
        return(NA * p)
    oo <- order(xx)
    cum.w <- cumsum(w[oo])/sum(w)
    cdf <- approxfun(cum.w, xx[oo], method = "constant", f = 1, yleft = min(xx),
        yright = max(xx), ties = min)
    cdf(p)
}


#' Extracts the se estimate
#'
#' Computes the se from the linearized variable.
#'
#' @param object output of a linearizing indicator function
#' @param design a survey design object of the library survey
#' @return the estime of the indicator se
#'
#' @author Djalma Pessoa and Anthony Damico
#'
#' @keywords survey
#'
#' @examples
#' library(vardpoor)
#' data(eusilc)
#' library(survey)
#' htot <- h_fun(eusilc$eqIncome, eusilc$rb050)
#' des_eusilc <- svydesign(ids=~db040, weights=~rb050, data=eusilc)
#' qsr_eqIncome <- svyqsr(~eqIncome, design=des_eusilc, alpha= .20, ncom = rownames(eusilc),
#' comp=TRUE, incvec = eusilc$eqIncome)
#' # se estimate of isq_eqIncome for the whole sample
#' SE_lin(qsr_eqIncome, des_eusilc)
#' # se estimates for domains
#' isq_eqIncome_dom <-  svyby(~eqIncome, by= ~db040, design=des_eusilc,
#' FUN=svyqsr, alpha=.20,  h= htot, ncom=rownames(eusilc), comp=TRUE,
#' incvec= eusilc$eqIncome, deff=FALSE, keep.var=FALSE)
#' SE_lin(isq_eqIncome_dom, des_eusilc)
#' @export

SE_lin <- function(object, design) {

    if (length(object) == 2) {
        t <- object$lin
        x <- update(design, t = t)
        res <- survey::SE( survey::svytotal( ~t , x ) )
    } else {
        lincomp <- object$statistic.lin
        list_se <- lapply(lincomp, function(t) {
            x <- update(design, t = t)
            survey::SE( survey::svytotal( ~t , x ) )
        })
        names(list_se) <- object[[1]]
        res <- list_se
    }

    res
}


# infuence function of a total: formula (34)
itot <- function(formula, design) {
    inc <- terms.formula(formula)[[2]]
    df <- model.frame(design)
    incvar <- df[[as.character(inc)]]
    value <- coef( survey::svytotal(x = formula, design = design))
    lin <- incvar
    list(value = value, lin = lin)
}

## derivation rules for influence functions of functionals linear combination of
## functionals: formula (29) a, b - scalars T, S - lists with two components:
## value and lin IF - list with with two components Fprime - real function

cl_inf <- function(a, b, T, S) {
    lin <- a * T$lin + b * S$lin
    value <- a * T$value + b * S$value
    list(value = value, lin = lin)
}

# product of o two functionals: formula (30)
prod_inf <- function(T, S) {

    value <- T$value * S$value
    lin <- T$value * S$lin + S$value * T$lin
    list(value = value, lin = lin)
}

# ratio of functionals: formula (31)

ratio_inf <- function(T, S) {
    value <- T$value/S$value
    lin <- (S$value * T$lin - T$value * S$lin)/((S$value)^2)
    list(value = value, lin = lin)
}










#' Extracts the se estimate
#'
#' Computes the se from the linearized variable.
#'
#' @param object output of a linearizing indicator function
#' @param design a survey design object of the library survey
#' @return the estime of the indicator se
#'
#' @author Djalma Pessoa and Anthony Damico
#'
#' @keywords survey
#'
#' @examples
#' library(vardpoor)
#' data(eusilc)
#' library(survey)
#' htot <- h_fun(eusilc$eqIncome, eusilc$rb050)
#' des_eusilc <- svydesign(ids=~db040, weights=~rb050, data=eusilc)
#' qsr_eqIncome <- svyqsr(~eqIncome, design=des_eusilc, alpha= .20, ncom = rownames(eusilc),
#' comp=TRUE, incvec = eusilc$eqIncome)
#' # se estimate of isq_eqIncome for the whole sample
#' SE_lin2(qsr_eqIncome, des_eusilc)
#' # se estimates for domains
#' isq_eqIncome_dom <-  svyby(~eqIncome, by= ~db040, design=des_eusilc,
#' FUN=svyqsr, alpha=.20,  h= htot, ncom=rownames(eusilc), comp=TRUE,
#' incvec= eusilc$eqIncome, deff=FALSE, keep.var=FALSE)
#' SE_lin2(isq_eqIncome_dom, des_eusilc)
#' @export

SE_lin2 <- function(object, design) {
	x <- update(design, t = object)
	res <- survey::SE( survey::svytotal( ~t , x ) )
    res
}





# cvystat print method
#' @export
print.cvystat <- function( x , ... ) {
    
	vv <- attr( x , "var" )
	
	if( is.matrix( vv ) ){
		m <- cbind( x , sqrt( diag( vv ) ) ) 
	} else {
	
		m <- cbind( x , sqrt( vv ) )
		
	}
	
	colnames( m ) <- c( attr( x , "statistic" ) , "SE" )
	
	printCoefmat( m )
		
}



#' prepare linearized svydesign objects for convey package
#'
#' stores the full survey design (needed for linearized convey functions) within the design
#'
#' @param design a survey design object of the library survey
#' @return design a survey design object of the library survey
#'
#' @author Djalma Pessoa and Anthony Damico
#'
#' @keywords survey
#'
#' @examples
#' library(vardpoor)
#' data(eusilc)
#' des_eusilc <- svydesign(ids=~db040, weights=~rb050, data=eusilc)
#' des_eusilc <- convey_prep( des_eusilc )
#' @export
convey_prep <-
	function( design ){
		

		if( !( 'survey.design' %in% class( design ) ) ) stop( "convey_prep only needs to be run on linearized designs" )
		
		cat( "preparing your full survey design to work with R convey package functions\n\rnote that this function must be run on the full survey design object immediately after the svydesign() call.\n\r" )
		
		# store the full design within one of the attributes of the design
		attr( design , "full_design" ) <- design
		
		design
	}
