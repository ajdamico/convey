#' Amato index (EXPERIMENTAL)
#'
#' Estimate the Amato index, a measure of inequality.
#'
#' @param formula a formula specifying the income variable.
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param standardized If \code{standardized = TRUE}, returns the standardized Amato index, i.e., a linear tranformation of the amato index.
#' @param na.rm Should cases with missing values be dropped?
#' @param ... future expansion
#'
#' @details you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
#'
#' The Amato index is the length of the Lorenz curve.
#'
#' @return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of the statistic.
#'
#' @author Guilherme Jacob, Djalma Pessoa and Anthony Damico
#'
#' @note This function is experimental and is subject to change in later versions.
#'
#' @seealso \code{\link{svygini}}
#'
#' @references Lucio Barabesi, Giancarlo Diana and Pier Francesco Perri (2016). Linearization of inequality indexes in the design-based framework.
#' Statistics. URL \url{http://www.tandfonline.com/doi/pdf/10.1080/02331888.2015.1135924}.
#'
#' Barry C. Arnold (2012). On the Amato inequality index.
#' Statistics & Probability Letters, v. 82, n. 8, August 2012, pp. 1504-1506, ISSN 0167-7152.
#' URL \url{http://dx.doi.org/10.1016/j.spl.2012.04.020}.
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
#' # replicate-weighted design
#' des_eusilc_rep <- as.svrepdesign( des_eusilc , type = "bootstrap" )
#' des_eusilc_rep <- convey_prep(des_eusilc_rep)
#'
#'
#' # variable without missing values
#' svyamato(~eqincome, des_eusilc)
#' svyamato(~eqincome, des_eusilc_rep)
#'
#' # subsetting:
#' svyamato(~eqincome, subset( des_eusilc, db040 == "Styria"))
#' svyamato(~eqincome, subset( des_eusilc_rep, db040 == "Styria"))
#'
#' \dontrun{
#'
#' # variable with with missings
#' svyamato(~py010n, des_eusilc )
#' svyamato(~py010n, des_eusilc_rep )
#'
#' svyamato(~py010n, des_eusilc, na.rm = TRUE )
#' svyamato(~py010n, des_eusilc_rep, na.rm = TRUE )
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
#'
#' # variable without missing values
#' svyamato(~eqincome, dbd_eusilc)
#'
#' # subsetting:
#' svyamato(~eqincome, subset( dbd_eusilc, db040 == "Styria"))
#'
#' # variable with with missings
#' svyamato(~py010n, dbd_eusilc )
#'
#' svyamato(~py010n, dbd_eusilc, na.rm = TRUE )
#'
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' dbDisconnect( conn , shutdown = TRUE )
#'
#' }
#'
#' @export
svyamato <- function(formula, design, ...) {

  if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

  warning("The svyamato function is experimental and is subject to changes in later versions.")

  UseMethod("svyamato", design)

}

#' @rdname svyamato
#' @export
svyamato.survey.design <- function( formula, design, standardized = FALSE , na.rm = FALSE, ... ) {

  incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

  w <- 1/design$prob

  if ( any( incvar[w != 0] < 0, na.rm = TRUE ) )  stop( "The Amato Index is defined for non-negative numeric variables only." )

  if (na.rm) {
    nas <- is.na(incvar)
    design <- design[nas == 0, ]
    if (length(nas) > length(design$prob))
      incvar <- incvar[nas == 0]
    else incvar[nas > 0] <- 0
  }

  w <- 1/design$prob

  incvar <- incvar[ w != 0 ]
  w <- w[ w != 0 ]

  if ( any( is.na(incvar) ) ) {
    rval <- as.numeric(NA)
    variance <- as.matrix(NA)
    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- c( "cvystat" , "svystat" )
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "amato"
    return(rval)

  }

  N <- sum(w)
  Tot <- sum(w * incvar)

  rval <- sum( w * ( 1 / N^2 + incvar^2 / Tot^2 )^.5 )

  z <- 1/design$prob
  z[ z > 0 ]  <- sqrt( ( 1 / N^2 + incvar / Tot^2 ) ) +
    ( -1 / N^3 ) * sum( w * ( 1 / N^2 + incvar^2 / Tot^2 )^-.5 ) +
    ( - incvar / Tot^3 ) * sum( w * incvar^2 * ( 1 / N^2 + incvar^2 / Tot^2 )^-.5 )

  variance <- survey::svyrecvar( z / design$prob, design$cluster, design$strata, design$fpc, postStrata = design$postStrata )

  if ( standardized ) {

    rval <- ( rval - sqrt(2) ) / ( 2 - sqrt(2) )
    variance <- variance / ( 2 - sqrt(2) )^2

    }

  colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
  class(rval) <- c( "cvystat" , "svystat" )
  attr(rval, "var") <- variance

  if ( standardized ) {
    attr(rval, "statistic") <- "standardized amato index"
  } else {
    attr(rval, "statistic") <- "amato index"
    }

  return( rval )

}

#' @rdname svyamato
#' @export
svyamato.svyrep.design <- function(formula, design, standardized = FALSE, na.rm=FALSE, ...) {
  incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]


  if(na.rm){
    nas<-is.na(incvar)
    design<-design[!nas,]
    df <- model.frame(design)
    incvar <- incvar[!nas]
  }

  if ( any(incvar < 0, na.rm = TRUE) ) stop( "The Amato Index is defined for non-negative numeric variables only." )

  calc.amato <- function( x, weights ) {

    x <- x[weights != 0 ]
    weights <- weights[weights != 0 ]

    N <- sum(weights)
    Tot <- sum(weights * x)

    sum( weights * ( 1 / N^2 + x^2 / Tot^2 )^.5 )
  }

  ws <- weights(design, "sampling")
  rval <- calc.amato( x = incvar, weights = ws )
  ww <- weights(design, "analysis")
  qq <- apply(ww, 2, function(wi) calc.amato(incvar, wi))
  if ( any(is.na(qq))) {
    variance <- as.matrix(NA)
    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- c( "cvystat" , "svrepstat" )
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "amato"
    return(rval)

  } else {
    variance <- survey::svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)

    variance <- as.matrix( variance )
  }

  if ( standardized ) {

    rval <- ( rval - sqrt(2) ) / ( 2 - sqrt(2) )
    variance <- variance / ( 2 - sqrt(2) )^2

  }

  colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
  class(rval) <- c( "cvystat" , "svrepstat" )
  attr(rval, "var") <- variance

  if ( standardized ) {
    attr(rval, "statistic") <- "standardized amato index"
  } else {
    attr(rval, "statistic") <- "amato index"
  }

  return(rval)

}

#' @rdname svyamato
#' @export
svyamato.DBIsvydesign <-
  function (formula, design, ...) {

    design$variables <- getvars(formula, design$db$connection, design$db$tablename,
                                updates = design$updates, subset = design$subset)

    NextMethod("svyamato", design)
  }
