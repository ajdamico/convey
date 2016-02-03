#'Complete the linearized values with 0's
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
#' @param incvar income variable used in the estimation of the indicators
#' @param w vector of design weights
#' @return value of the bandwidth
#' @author Djalma Pessoa and Anthony Damico
#' @keywords survey
#' @export


h_fun <- function(incvar, w) {
    N <- sum(w)
    sd_inc <- sqrt((sum(w * incvar * incvar) - sum(w * incvar) * sum(w * incvar)/N)/N)
    h <- sd_inc/exp(0.2 * log(sum(w)))
    h
}

#'Estimate the derivative of the cdf function using kernel estimator
#'
#'computes the derivative of a function in a point using kernel estimation
#'
#'@param formula a formula specifying the income variable
#'@param design a design object of class \code{survey.design} of the library survey.  database-backed designs not supported
#'@param x the point where the derivative is calculated
#'@param h value of the bandwidth based on the whole sample
#'@param if \code{F} estimates the derivative of the cdf function; if \code{S} estimates
#'the derivative of total in the tails of the distribution
#'@return the value of the derivative at \code{x}
#'@author Djalma Pessoa and Anthony Damico
#'@keywords survey
#' @examples
#' library(vardpoor)
#' data(eusilc)
#' library(survey)
#' des_eusilc <- svydesign(ids = ~rb030, strata =~db040,  weights = ~rb050, data = eusilc)
#' des_eusilc <- convey_prep( des_eusilc )
#' densfun (~eqIncome, design=des_eusilc, 10000, fun="F" )
#' # linearized design using a variable with missings
#' densfun ( ~ py010n , design = des_eusilc, 10000, fun="F" )
#' densfun ( ~ py010n , design = des_eusilc , 10000,fun="F", na.rm = TRUE )
#'@export

densfun <- function(formula, design, x, h = NULL, fun = c("F", "S"), na.rm=FALSE, ...) {

  incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]
  if(na.rm){
    nas<-is.na(incvar)
    design<-design[!nas,]
    if (length(nas) > length(design$prob))
      incvar <- incvar[!nas]
    else incvar[nas] <- 0
  }
  w <- 1/design$prob
  N <- sum(w)
  if(is.null(h)) h <- h_fun(incvar,w)
  u <- (x - incvar)/h
  vectf <- exp(-(u^2)/2)/sqrt(2 * pi)
  if (fun == "F")
    res <- sum(vectf * w)/(N * h) else {
      v <- w * incvar
      res <- sum(vectf * v)/h
    }
  res
}

#' Linearization of the cdf function of a variable
#'
#' Computes the linearized variable of the cdf function in a point.
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design}  of the library survey.  database-backed designs not supported
#'
#'@return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of the statistic.
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
#' @examples
#' library(vardpoor)
#' data(eusilc)
#' library(survey)
#' des_eusilc <- svydesign(ids = ~rb030, strata =~db040,  weights = ~rb050, data = eusilc)
#' des_eusilc <- convey_prep( des_eusilc )
#' icdf(~eqIncome, design=des_eusilc, 10000 )
#' # linearized design using a variable with missings
#' icdf( ~ py010n , design = des_eusilc, 10000 )
#' icdf( ~ py010n , design = des_eusilc , 10000, na.rm = TRUE )
#' @export

icdf <- function(formula, design, x, na.rm = FALSE, ...) {

  incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]
  ncom<- names(design$prob)
  if(na.rm){
    nas<-is.na(incvar)
    design<-design[!nas,]
    if (length(nas) > length(design$prob))
      incvar <- incvar[!nas]
    else incvar[nas] <- 0
  }
  w <- 1/design$prob
  #ind<- names(w)
  N <- sum(w)
  poor <- (incvar <= x) * 1
  value<- sum(poor*w)/N
  lin<-((incvar<=x)-value)/N
  rval <- value
  variance <- (SE_lin2(lin, design))^2
  class(rval) <- "cvystat"
  attr(rval, "lin") <- lin
  attr(rval, "var") <- variance
  attr(rval, "statistic") <- "cdf"
  rval
}


computeQuantiles <- function(xx, w, p = quantiles) {
    if (any(is.na(xx)))
        return(NA * p)

	if( sum( w ) == 0 ) return( NA )

    oo <- order(xx)
    cum.w <- cumsum(w[oo])/sum(w)
    cdf <- approxfun(cum.w, xx[oo], method = "constant", f = 1, yleft = min(xx),
        yright = max(xx), ties = min)
    cdf(p)
}



# infuence function of a total: formula (34)
itot <- function(formula, design) {
    incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]
    value <- coef(survey::svytotal(x = formula, design = design))
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
#' @param design a survey design object of the library survey.  database-backed designs not supported
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
#' des_eusilc <- svydesign(ids = ~rb030, strata =~db040,  weights = ~rb050, data = eusilc)
#' qsr_eqIncome <- svyqsr(~eqIncome, design=des_eusilc, alpha= .20)
#' # se estimate of isq_eqIncome for the whole sample
#' SE_lin2(attr(qsr_eqIncome,"lin"), des_eusilc)
#' @export

SE_lin2 <- function(object, design) {
    x <- update(design, t = object)
    res <- survey::SE(survey::svytotal(~t, x))
    res
}



# cvystat print method
#' @export
print.cvystat <- function(x, ...) {

    vv <- attr(x, "var")

    if (is.matrix(vv)) {
        m <- cbind(x, sqrt(diag(vv)))
    } else {

        m <- cbind(x, sqrt(vv))

    }

    colnames(m) <- c(attr(x, "statistic"), "SE")

    printCoefmat(m)

}


# cvystat vcov method
#' @export
vcov.cvystat <- function (object, ...)
{
    as.matrix(attr(object, "var"))
}


# cvystat coef method
#' @export
coef.cvystat <- function(object, ...) {
    attr(object, "statistic") <- NULL
    attr(object, "deff") <- NULL
    attr(object, "var") <- NULL
	attr(object, "lin") <- NULL
    unclass(object)
}


#' prepare svydesign and svyrep.design objects for the convey package
#'
#' stores the full survey design (needed for convey functions) within the design
#'
#' @param design a survey design object of the library survey.  database-backed designs not supported
#' @return design the same survey object, preparred for all convey package functions.
#'
#' @author Djalma Pessoa and Anthony Damico
#'
#' @keywords survey
#'
#' @examples
#'
#' library(survey)
#' library(vardpoor)
#' data(eusilc)
#'
#' # linearized design: convey_prep must be run as soon as the linearized design has been created
#' des_eusilc <- svydesign( ids = ~rb030 , strata = ~db040 ,  weights = ~rb050 , data = eusilc )
#' des_eusilc <- convey_prep( des_eusilc )
#' # now this linearized design object is ready for analysis!
#'
#' # # # CORRECT usage example # # #
#' des_eusilc <- svydesign( ids = ~rb030 , strata = ~db040 ,  weights = ~rb050 , data = eusilc )
#' des_eusilc <- convey_prep( des_eusilc )
#' sub_eusilc <- subset( des_eusilc , age > 20 )
#' # since convey_prep() was run immediately after creating the design
#' # this will calculate the variance accurately
#' SE( svyarpt( ~ eqIncome , sub_eusilc ) )
#' # # # end of CORRECT usage example # # #
#'
#' # # # INCORRECT usage example # # #
#' des_eusilc <- svydesign( ids = ~rb030 , strata = ~db040 ,  weights = ~rb050 , data = eusilc )
#' sub_eusilc <- subset( des_eusilc , age > 20 )
#' sub_eusilc <- convey_prep( sub_eusilc )
#' # since convey_prep() was not run immediately after creating the design
#' # this will make the variance wrong
#' SE( svyarpt( ~ eqIncome , sub_eusilc ) )
#' # # # end of INCORRECT usage example # # #
#'
#' @export
convey_prep <- function(design) {

	if( ( "DBIsvydesign" %in% class( design ) ) | ( "DBIrepdesign" %in% class( design ) ) ) stop( "the `convey` package does not support database-backed survey designs.\r\nyou must create your design within memory" )

    if (!is.null(attr(design, "full_design")))stop("convey_prep has already been run on this design")

    cat("preparing your full survey design to work with R convey package functions\n\rnote that this function must be run on the full survey design object immediately after the svydesign() or svrepdesign() call.\n\r")

    # store the full design within one of the attributes of the design
    attr(design, "full_design") <- design

    # store the full_design's full_design attribute as TRUE
    attr(attr(design, "full_design"), "full_design") <- TRUE

    design
}
