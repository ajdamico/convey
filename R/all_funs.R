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
#'@param h value of the bandwidth based on the whole sample
#'@param if \code{F} estimates the derivative of the cdf function; if \code{S} estimates
#'the derivative of total in the tails of the distribution
#'@return the value of the derivative at \code{x}
#'@author Djalma Pessoa and Anthony Damico
#'@keywords survey
#'@export

densfun <- function(formula, design, x, h = NULL, fun = c("F", "S"), ...) {
    inc <- terms.formula(formula)[[2]]
    w <- weights(design)
    N <- sum(w)
    df <- model.frame(design)
    inc_var <- df[[as.character(inc)]]
    if(is.null(h)) h <- h_fun(inc_var,w)
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
#' @param design a design object of class \code{survey.design} or class
#' \code{svyrep.design}  of the library survey
#'
#'@return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of
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
#'
#' @export

icdf <- function(formula, design, x, ...) {

    if (is.null(attr(design, "full_design")))
        stop("you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svydesign() function.")

    inc <- terms.formula(formula)[[2]]
    df <- model.frame(design)
    incvar <- df[[as.character(inc)]]
    w <- weights(design)
    ind <- row.names(df)
    N<- sum(w)
    poor <- (incvar <= x) * 1
    names(poor) <- ind

    # if the class of the full_design attribute is just a TRUE, then the design is
    # already the full design.  otherwise, pull the full_design from that attribute.
    if ("logical" %in% class(attr(design, "full_design")))
        full_design <- design else full_design <- attr(design, "full_design")
    df_full <- model.frame(full_design)
    ncom <- row.names(df_full)

    incvec <- df_full[[as.character(inc)]]
    wf<- weights(full_design)
    Nf<- sum(wf)
    value<- sum(poor*w)/N
    lin<-(1/N)*((incvar<=x)-value)
    names(lin)<- ind
    lin<-complete(lin,ncom)
    rval<- value
    variance <- (SE_lin2(lin, full_design))^2
    class(rval) <- "cvystat"
    attr(rval, "lin") <- lin
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "cdf"
    rval
}


#' Linearization of a variable quantile
#'
#' Computes the linearized variable of a quantile of variable.
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design}
#' of the library survey
#' @param alpha the order of the quantile
#' @param comp logical variable \code{TRUE} if the linearized variable for domains
#' should be completed with zeros
#' @param compinc logical variable \code{TRUE} if should use the vector of sample #'values of the variable whose quantile is to be linearized
#' @return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of
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
#' iqalpha_eqIncome <-iqalpha(~eqIncome, design=des_eusilc, .50 )
#'
#' @export

iqalpha <- function(formula, design, alpha, h=NULL, comp = TRUE, compinc = FALSE, ...) {

    if (is.null(attr(design, "full_design")))
        stop("you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svydesign() function.")
    inc <- terms.formula(formula)[[2]]
    df <- model.frame(design)
    incvar <- df[[as.character(inc)]]
    w <- weights(design)
    N <- sum(w)
    ind <- row.names(df)
    q_alpha <- survey::svyquantile(x = formula, design = design, quantiles = alpha,
        method = "constant")
    q_alpha <- as.vector(q_alpha)

    # if the class of the full_design attribute is just a TRUE, then the design is
    # already the full design.  otherwise, pull the full_design from that attribute.
    if ("logical" %in% class(attr(design, "full_design")))
        full_design <- design else full_design <- attr(design, "full_design")
    df_full <- model.frame(full_design)
    ncom <- row.names(df_full)
    incvec <- df_full[[as.character(inc)]]
    htot <- h_fun(incvec, weights(full_design))
    Fprime <- densfun(formula = formula, design = design, q_alpha, h=h, fun = "F")
    iq <- -(1/(N * Fprime)) * ((incvar <= q_alpha) - alpha)
    names(iq) <- ind
    if (comp)
      iq <- complete(iq, ncom)
        rval <- q_alpha
    if (compinc) {
        iq <- -(1/(N * Fprime)) * ((incvec <= q_alpha) - alpha)
    }
    variance <- (SE_lin2(iq, full_design))^2
    class(rval) <- "cvystat"
    attr(rval, "lin") <- iq
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "quantile"
    rval
}

#' Linearization of the total above a quantile or below a quantile
#'
#' Computes the linearized variable of the total in the lower tail of
#' the distribution of a variable.
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design}
#' of the library survey
#' @param alpha the order of the quantile
#' @param compinc logical variable \code{TRUE} if should use the vector of sample #'values of the variable whose quantile is to be linearized
#' @return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of
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
#' isq_eqIncome <-isq(~eqIncome, design=des_eusilc, .20)
#'
#' @export


isq <- function(formula, design, alpha, comp = TRUE, compinc,...) {
    if (is.null(attr(design, "full_design")))
        stop("you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svydesign() function.")
    inc <- terms.formula(formula)[[2]]
    df <- model.frame(design)
    incvar <- df[[as.character(inc)]]
    w <- weights(design)
    ind <- row.names(df)

    # if the class of the full_design attribute is just a TRUE, then the design is
    # already the full design.  otherwise, pull the full_design from that attribute.
    if ("logical" %in% class(attr(design, "full_design")))
        full_design <- design else full_design <- attr(design, "full_design")

    df_full <- model.frame(full_design)
    ncom <- row.names(df_full)
    incvec <- df_full[[as.character(inc)]]
    h <- h_fun(incvec, weights(full_design))
    QALPHA <- iqalpha(formula = formula, design = design, alpha,comp = TRUE,
      compinc = compinc)
    q_alpha <- QALPHA[1]
    iq <- attr(QALPHA, "lin")
    inc_inf <- (incvar <= q_alpha) * incvar
    tot <- sum(inc_inf * w)
    Fprime <- densfun(formula = formula, design = design, q_alpha, fun = "S")
    isqalpha <- incvec * ((incvec <= q_alpha)) + Fprime * iq
    rval <- tot
    variance <- (SE_lin2(isqalpha, full_design))^2
    class(rval) <- "cvystat"
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "isq"
    attr(rval, "lin") <- isqalpha
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
    inc <- terms.formula(formula)[[2]]
    df <- model.frame(design)
    incvar <- df[[as.character(inc)]]
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
#' des_eusilc <- svydesign(ids=~db040, weights=~rb050, data=eusilc)
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
convey_prep <- function(design) {


    if (!("survey.design" %in% class(design)))
        stop("convey_prep only needs to be run on linearized designs")

    if (!is.null(attr(design, "full_design")))
        stop("convey_prep has already been run on this design")

    cat("preparing your full survey design to work with R convey package functions\n\rnote that this function must be run on the full survey design object immediately after the svydesign() call.\n\r")

    # store the full design within one of the attributes of the design
    attr(design, "full_design") <- design

    # store the full_design's full_design attribute as TRUE
    attr(attr(design, "full_design"), "full_design") <- TRUE

    design
}
