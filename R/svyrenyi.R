#' Renyi divergence measure (EXPERIMENTAL)
#'
#' Estimate the Renyi divergence measure, a measure of inequality
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param epsilon a parameter that determines the sensivity towards inequality on the top of the distribution. Defaults to epsilon = 1.
#' @param na.rm Should cases with missing values be dropped?
#' @param ... future expansion
#'
#' @details you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
#'
#' If \code{epsilon == 1}, the result matches \code{svygei} with \code{epsilon == 1}. As in the generalized entropy index, when \code{epsilon == 1}, the logarithm in the function only allows for strictly positive variables.
#'
#' @return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of the statistic.
#'
#' @author Guilherme Jacob, Djalma Pessoa and Anthony Damico
#'
#' @note This function is experimental and is subject to change in later versions.
#'
#' @seealso \code{\link{svygei}}
#'
#' @references Matti Langel (2012). Measuring inequality in finite population sampling.
#' PhD thesis: Universite de Neuchatel,
#' URL \url{https://doc.rero.ch/record/29204/files/00002252.pdf}.
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
#' svyrenyi( ~eqincome , design = des_eusilc, epsilon = .5 )
#'
#' # replicate-weighted design
#' des_eusilc_rep <- as.svrepdesign( des_eusilc , type = "bootstrap" )
#' des_eusilc_rep <- convey_prep(des_eusilc_rep)
#'
#' svyrenyi( ~eqincome , design = des_eusilc_rep, epsilon = .5 )
#'
#' \dontrun{
#'
#' # linearized design using a variable with missings
#' svyrenyi( ~py010n , design = des_eusilc, epsilon = .5 )
#' svyrenyi( ~py010n , design = des_eusilc, epsilon = .5, na.rm = TRUE )
#' # replicate-weighted design using a variable with missings
#' svyrenyi( ~py010n , design = des_eusilc_rep, epsilon = .5 )
#' svyrenyi( ~py010n , design = des_eusilc_rep, epsilon = .5, na.rm = TRUE )
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
#' svyrenyi( ~eqincome , design = dbd_eusilc, epsilon = .5 )
#'
#' # Testing if Renyi and GEI match when epsilon == 1:
#' svyrenyi( ~eqincome , design = subset(dbd_eusilc, eqincome > 0 ), epsilon = 1 )
#' svygei( ~eqincome , design = subset(dbd_eusilc, eqincome > 0 ), epsilon = 1 )
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' dbDisconnect( conn , shutdown = TRUE )
#'
#' }
#'
#' @export
svyrenyi <- function(formula, design, ...) {

	if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

	if( 'epsilon' %in% names( list(...) ) && list(...)[["epsilon"]] < 0 ) stop( "epsilon= cannot be negative." )

  warning("The svyrenyi function is experimental and is subject to changes in later versions.")

	UseMethod("svyrenyi", design)

}

#' @rdname svyrenyi
#' @export
svyrenyi.survey.design <- function ( formula, design, epsilon = 1, na.rm = FALSE, ... ) {

  incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

  if (na.rm) {
    nas <- is.na(incvar)
    design <- design[nas == 0, ]
    if (length(nas) > length(design$prob))
      incvar <- incvar[nas == 0]
    else incvar[nas > 0] <- 0
  }

  # Jenkins & Biewen's U and big_t functions:
  U_fn <- function( x, weights, gamma ) {
    x <- x[ weights != 0 ]
    weights <- weights[ weights != 0 ]
    return( sum( weights * x^gamma ) )
  }
  T_fn <- function( x, weights, gamma ) {
    x <- x[ weights != 0 ]
    weights <- weights[ weights != 0 ]
    return( sum( weights * x^gamma * log( x ) ) )
  }

  calc.renyi <- function( x, weights, epsilon ) {
    x <- x[weights != 0 ]
    weights <- weights[weights != 0 ]

    N <- sum(weights)
    X <- sum( weights * x )
    x_bar <- X/N
    if ( epsilon == 1 ) {
      #b <- x * log(x)
      #B <- T_fn(x,weights,1)
      result <- (T_fn(x,weights,1)/U_fn( x, weights, 1)) - log( U_fn( x, weights, 1)/U_fn( x, weights, 0) ) }
    else {
      #b <- x^epsilon
      #B <- U_fn(x, weights, gamma = epsilon )
      result <- ( (epsilon - 1)*log(sum(weights)) - epsilon*log(U_fn(x,weights,1)) + log(U_fn(x, weights, gamma = epsilon )) )/(epsilon-1)
    }
    return( result )
  }

  w <- 1/design$prob
  if ( epsilon == 1 & any( incvar[ w != 0 ] == 0, na.rm = TRUE) ) { stop( paste("the RDM is undefined for zero incomes if epsilon ==", epsilon) ) }

  rval <- calc.renyi( x = incvar, weights = w, epsilon = epsilon )

  if ( is.na(rval) ){
    variance <- as.matrix(NA)
    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- c( "cvystat" , "svystat" )
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "renyi divergence measure"
    attr(rval,"epsilon")<- epsilon

    return(rval)

  }

  if ( epsilon == 1 ) {
    s <- ( incvar/U_fn(incvar,w,1) ) * ( log(incvar) - T_fn(incvar,w,1)/U_fn(incvar,w,1) - 1) + 1/U_fn(incvar,w,0)
    s[ w == 0 ] <- 0
    variance <- survey::svyrecvar(s/design$prob, design$cluster,
                                  design$strata, design$fpc, postStrata = design$postStrata)
  } else {
    s <- ( (epsilon - 1)/U_fn(incvar,w,0) - epsilon*incvar/U_fn(incvar,w,1) + (incvar^epsilon)/U_fn(incvar, w, gamma = epsilon ) )/(epsilon-1)
    s[ w == 0 ] <- 0
    variance <- survey::svyrecvar(s/design$prob, design$cluster,
                                  design$strata, design$fpc, postStrata = design$postStrata)
  }

  colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
  class(rval) <- c( "cvystat" , "svystat" )
  attr(rval, "var") <- variance
  attr(rval, "statistic") <- "renyi divergence measure"
  attr(rval,"epsilon")<- epsilon

  return( rval )

}

#' @rdname svyrenyi
#' @export
svyrenyi.svyrep.design <- function ( formula, design, epsilon = 1, na.rm = FALSE, ... ) {

  incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

  if(na.rm){
    nas<-is.na(incvar)
    design<-design[!nas,]
    df <- model.frame(design)
    incvar <- incvar[!nas]
  }

  # Jenkins & Biewen's U and big_t functions:
  U_fn <- function( x, weights, gamma ) {
    return( sum( weights * x^gamma ) )
  }
  T_fn <- function( x, weights, gamma ) {
    return( sum( weights * x^gamma * log( x ) ) )
  }

  calc.renyi <- function( x, weights, epsilon ) {
    x <- x[weights != 0 ]
    weights <- weights[weights != 0 ]

    N <- sum(weights)
    X <- sum( weights * x )
    x_bar <- X/N
    if ( epsilon == 1 ) {
      #b <- x * log(x)
      #B <- T_fn(x,weights,1)
      result <- (T_fn(x,weights,1)/U_fn( x, weights, 1)) - log( U_fn( x, weights, 1)/U_fn( x, weights, 0) ) }
    else {
      #b <- x^epsilon
      #B <- U_fn(x, weights, gamma = epsilon )
      result <- ( (epsilon - 1)*log(sum(weights)) - epsilon*log(U_fn(x,weights,1)) + log(U_fn(x, weights, gamma = epsilon )) )/(epsilon-1)
    }
    return( result )
  }

  ws <- weights(design, "sampling")
  if ( epsilon == 1 & any(incvar [ ws != 0 ] == 0, na.rm = TRUE ) ) { stop( paste("the RDM is undefined for zero incomes if epsilon ==", epsilon) ) }
  rval <- calc.renyi( x = incvar, weights = ws, epsilon = epsilon )

  ww <- weights(design, "analysis")
  qq <- apply(ww, 2, function(wi) calc.renyi(incvar, wi, epsilon = epsilon))
  if ( any(is.na(qq))) {
    variance <- as.matrix(NA)
    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- c( "cvystat" , "svrepstat" )
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "renyi divergence measure"

    return(rval)

  } else {
    variance <- survey::svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)

    variance <- as.matrix( variance )
  }
  colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
  class(rval) <- c( "cvystat" , "svrepstat" )
  attr(rval, "var") <- variance
  attr(rval, "statistic") <- "renyi divergence measure"
  attr(rval,"epsilon")<- epsilon

  return( rval )

}

#' @rdname svyrenyi
#' @export
svyrenyi.DBIsvydesign <-
  function (formula, design, ...) {

    design$variables <- getvars(formula, design$db$connection, design$db$tablename, updates = design$updates, subset = design$subset)

    NextMethod("svyrenyi", design)
  }
