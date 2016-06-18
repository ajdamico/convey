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
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} from the \code{survey} library.
#' @param x the point where the derivative is calculated
#' @param h value of the bandwidth based on the whole sample
#' @param FUN if \code{F} estimates the derivative of the cdf function; if \code{big_s} estimates the derivative of total in the tails of the distribution
#' @param na.rm Should cases with missing values be dropped?
#' @param ... future expansion
#'
#' @return the value of the derivative at \code{x}
#'
#' @author Djalma Pessoa and Anthony Damico
#'
#' @keywords survey
#' @examples
#' library(vardpoor)
#' data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )
#' library(survey)
#' des_eusilc <- svydesign(ids = ~rb030, strata =~db040,  weights = ~rb050, data = eusilc)
#' des_eusilc <- convey_prep( des_eusilc )
#' densfun (~eqincome, design=des_eusilc, 10000, FUN="F" )
#' # linearized design using a variable with missings
#' densfun ( ~ py010n , design = des_eusilc, 10000, FUN="F" )
#' densfun ( ~ py010n , design = des_eusilc , 10000,FUN="F", na.rm = TRUE )
#'
#' @export

densfun <- function(formula, design, x, h = NULL, FUN = "F" , na.rm=FALSE, ...) {

	if( !( FUN %in% c( "F" , "big_s" ) ) ) stop( "valid choices for `FUN=` are 'F' and 'big_s'" )

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
  if (FUN == "F")
    res <- sum(vectf * w)/(N * h) else {
      v <- w * incvar
      res <- sum(vectf * v)/h
    }
  res
}

#' Linearization of the cumulative distribution function (cdf) of a variable
#'
#' Computes the linearized variable of the cdf function in a point.
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param x the point where the cdf is calculated
#' @param na.rm Should cases with missing values be dropped?
#' @param ... future expansion
#'
#' @return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of the statistic.
#'
#' @author Djalma Pessoa and Anthony Damico
#'
#' @seealso \code{\link{svyarpr}}
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
#' data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )
#' library(survey)
#' des_eusilc <- svydesign(ids = ~rb030, strata =~db040,  weights = ~rb050, data = eusilc)
#' des_eusilc <- convey_prep( des_eusilc )
#' icdf(~eqincome, design=des_eusilc, 10000 )
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
  variance <- survey::svyrecvar(lin/design$prob, design$cluster,
    design$strata, design$fpc, postStrata = design$postStrata)
  class(rval) <- "cvystat"
  attr(rval, "lin") <- lin
  attr(rval, "var") <- variance
  attr(rval, "statistic") <- "cdf"
  rval
}

# computeQuantiles from the survey library v3.30-3
# written by Thomas Lumley and copied here under the same GPL-3 license
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

# checkConnection from the survey library v3.30-3
# written by Thomas Lumley and copied here under the same GPL-3 license
checkConnection <- 
	function (dbconnection, error = TRUE) {
		
		if (is(dbconnection, "DBIConnection")) {
			if (!dbIsValid(dbconnection)) 
				if (error) 
					stop("Database connection is closed")
				else return(FALSE)
		} else {
		
		}
		invisible(TRUE)
	}

# getvars from the survey library v3.30-3
# written by Thomas Lumley and copied here under the same GPL-3 license
getvars <- function (formula, dbconnection, tables, db.only = TRUE, updates = NULL, 
    subset = NULL) {
    checkConnection(dbconnection)
    if (is.null(formula)) 
        return(NULL)
    if (inherits(formula, "formula")) {
        var0 <- all.vars(formula)
    }
    else if (is.character(formula)) {
        var0 <- formula
    }
    else {
        return(formula)
    }
    infilter <- updatesInfilter(var0, updates)
    if (db.only) {
        in.db <- infilter$varlist
    }
    else {
        query <- sub("@tab@", tables, "select * from @tab@ limit 1")
        if (is(dbconnection, "DBIConnection")) 
            oneline <- dbGetQuery(dbconnection, query)
        else oneline <- sqlQuery(dbconnection, query)
        in.db <- infilter$varlist[infilter$varlist %in% names(oneline)]
    }
    query <- paste("select", paste(in.db, collapse = ", "), "from", 
        tables)
    if (is(dbconnection, "DBIConnection")) 
        df <- dbGetQuery(dbconnection, query)
    else df <- sqlQuery(dbconnection, query)
    if (!is.null(subset)) 
        df <- df[subset, , drop = FALSE]
    df <- updatesOutfilter(df, var0, infilter$history, updates)
    is.string <- sapply(df, is.character)
    if (any(is.string)) {
        for (i in which(is.string)) df[[i]] <- as.factor(df[[i]])
    }
    df
}



# Funções U e big_t de Jenkins & Biewen:
U_fn <-
	function( x, weights, gamma ) {
		x <- x[weights != 0]

		weights <- weights[weights != 0]

		sum( weights * x^gamma )
	}

T_fn <-
	function( x, weights, gamma ) {
		x <- x[weights != 0]

		weights <- weights[weights != 0]

		sum( weights * x^gamma * log( x ) )
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
    nattr <- length(names(attributes(x)))
    if (nattr>5) {
      for(i in 6:nattr)
      {m <- cbind(m, attr(x, names(attributes(x)[i])))}
      colnames(m) <- c(attr(x, "statistic"), "SE", names(attributes(x))[6:nattr])
    }
    else {
      colnames(m) <- c(attr(x, "statistic"), "SE")
    }

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
#' stores the full survey design (needed for convey functions) within the design.  this function must be run immediately after the full design object creation with \code{svydesign} or \code{svrepdesign}
#'
#' @param design a survey design object of the library survey.
#' 
#' @return the same survey object with a \code{full_design} attribute as the storage space for the unsubsetted survey design
#'
#' @author Djalma Pessoa and Anthony Damico
#'
#' @details some of the functions in the convey package require the complete (pre-subsetted) design in order to calculate variances correctly.  this function stores the full design object as a separate attribute so that functions from the \code{survey} package such as \code{subset} and \code{svyby} do not disrupt the calculation of error terms.
#' 
#' @keywords survey
#'
#' @examples
#'
#' library(survey)
#' library(vardpoor)
#' data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )
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
#' SE( svyarpt( ~ eqincome , sub_eusilc ) )
#' # # # end of CORRECT usage example # # #
#'
#' # # # INCORRECT usage example # # #
#' des_eusilc <- svydesign( ids = ~rb030 , strata = ~db040 ,  weights = ~rb050 , data = eusilc )
#' sub_eusilc <- subset( des_eusilc , age > 20 )
#' sub_eusilc <- convey_prep( sub_eusilc )
#' # since convey_prep() was not run immediately after creating the design
#' # this will make the variance wrong
#' SE( svyarpt( ~ eqincome , sub_eusilc ) )
#' # # # end of INCORRECT usage example # # #
#'
#' @export
convey_prep <- function(design) {

    if (!is.null(attr(design, "full_design")))stop("convey_prep has already been run on this design")

    cat("preparing your full survey design to work with R convey package functions\n\rnote that this function must be run on the full survey design object immediately after the svydesign() or svrepdesign() call.\n\r")

    # store the full design within one of the attributes of the design
    attr(design, "full_design") <- design

    # store the full_design's full_design attribute as TRUE
    attr(attr(design, "full_design"), "full_design") <- TRUE

    design
}
