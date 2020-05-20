#' Zenga index (EXPERIMENTAL)
#'
#' Estimate the Zenga index, a measure of inequality
#'
#' @param formula a formula specifying the income variable.
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param na.rm Should cases with missing values be dropped?
#' @param ... future expansion
#'
#' @details you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
#'
#' @return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of the statistic.
#'
#' @author Guilherme Jacob, Djalma Pessoa and Anthony Damico
#'
#' @note This function is experimental and is subject to changes in later versions.
#'
#' @seealso \code{\link{svygini}}
#'
#' @references Lucio Barabesi, Giancarlo Diana and Pier Francesco Perri (2016). Linearization of inequality indexes in the design-based framework.
#' Statistics. URL \url{http://www.tandfonline.com/doi/pdf/10.1080/02331888.2015.1135924}.
#'
#' Matti Langel (2012). Measuring inequality in finite population sampling.
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
#' # replicate-weighted design
#' des_eusilc_rep <- as.svrepdesign( des_eusilc , type = "bootstrap" )
#' des_eusilc_rep <- convey_prep(des_eusilc_rep)
#'
#'
#' # variable without missing values
#' svyzenga(~eqincome, des_eusilc)
#' svyzenga(~eqincome, des_eusilc_rep)
#'
#' # subsetting:
#' svyzenga(~eqincome, subset( des_eusilc, db040 == "Styria"))
#' svyzenga(~eqincome, subset( des_eusilc_rep, db040 == "Styria"))
#'
#' \dontrun{
#'
#' # variable with with missings
#' svyzenga(~py010n, des_eusilc )
#' svyzenga(~py010n, des_eusilc_rep )
#'
#' svyzenga(~py010n, des_eusilc, na.rm = TRUE )
#' svyzenga(~py010n, des_eusilc_rep, na.rm = TRUE )
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
#' svyzenga(~eqincome, dbd_eusilc)
#'
#' # subsetting:
#' svyzenga(~eqincome, subset( dbd_eusilc, db040 == "Styria"))
#'
#' # variable with with missings
#' svyzenga(~py010n, dbd_eusilc )
#'
#' svyzenga(~py010n, dbd_eusilc, na.rm = TRUE )
#'
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' dbDisconnect( conn , shutdown = TRUE )
#'
#' }
#'
#' @export
svyzenga <- function(formula, design, ...) {

  if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

  warning("The svyzenga function is experimental and is subject to changes in later versions.")

  UseMethod("svyzenga", design)

}

#' @rdname svyzenga
#' @export
svyzenga.survey.design <- function( formula, design, na.rm = FALSE, ... ) {

  incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

  w <- 1/design$prob

  if ( any( incvar[w != 0] < 0, na.rm = TRUE ) ) stop( "The Zenga index is defined for non-negative numeric variables only.")


  if (na.rm) {
    nas <- is.na(incvar)
    design <- design[nas == 0, ]
    if (length(nas) > length(design$prob))
      incvar <- incvar[nas == 0]
    else incvar[nas > 0] <- 0
  }

  w <- 1/design$prob

  ordincvar <- order(incvar)
  w <- w[ ordincvar ]
  incvar <- incvar[ ordincvar ]

  incvar <- incvar[ w != 0 ]
  w <- w[ w != 0 ]

  if ( any( is.na(incvar) ) ) {
    rval <- as.numeric(NA)
    variance <- as.matrix(NA)
    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- c( "cvystat" , "svystat" )
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "zenga"
    return(rval)

  }

  N <- sum(w)
  Tot <- sum(w * incvar)
  H_y <- cumsum(w)
  K_y <- Tot - ( cumsum(w * incvar) - w*incvar )
  # K_y <- Tot - cumsum(w * incvar)


  rval <- 1 - sum(  w * ( N - H_y )*( Tot - K_y ) / ( N * H_y * K_y ) )
  # sum( w/N -  w * ( N - H_y )*( Tot - K_y ) / ( N * H_y * K_y ) )



	zenga_df <-
		data.frame(
			this_incvar = incvar ,
			this_N = N ,
			this_H_y = H_y ,
			this_K_y = K_y ,
			this_Tot = Tot ,
			this_w = w
		)

	zenga_df[ , 'line1' ] <-
		- ( zenga_df[ , 'this_N' ] - zenga_df[ , 'this_H_y' ] )*( zenga_df[ , 'this_Tot' ] - zenga_df[ , 'this_K_y' ] ) / ( zenga_df[ , 'this_N' ] * zenga_df[ , 'this_H_y' ] * zenga_df[ , 'this_K_y' ] )

	zenga_df[ , 'line2' ] <-
		- ( 1 / zenga_df[ , 'this_N' ]^2 ) * sum( zenga_df[ , 'this_w' ] * ( ( zenga_df[ , 'this_Tot' ] - zenga_df[ , 'this_K_y' ] ) / zenga_df[ , 'this_K_y' ] ) )

	zenga_df[ , 'line3' ] <-
		- ( zenga_df[ , 'this_incvar' ] / zenga_df[ , 'this_N' ] ) * sum( zenga_df[ , 'this_w' ] * ( zenga_df[ , 'this_N' ] - zenga_df[ , 'this_H_y' ] ) / ( zenga_df[ , 'this_H_y' ] * zenga_df[ , 'this_K_y' ] ) )

	zenga_df[ , 'line4' ] <-
		rev( cumsum( rev( zenga_df[ , 'this_w' ] * ( ( zenga_df[ , 'this_Tot' ] - zenga_df[ , 'this_K_y' ] ) / ( zenga_df[ , 'this_H_y' ]^2 * zenga_df[ , 'this_K_y' ] ) ) ) ) )

	zenga_df[ , 'line5' ] <-
		( zenga_df[ , 'this_Tot' ] * zenga_df[ , 'this_incvar' ] / zenga_df[ , 'this_N' ] ) * cumsum( zenga_df[ , 'this_w' ] * ( ( zenga_df[ , 'this_N' ] - zenga_df[ , 'this_H_y' ] ) / ( zenga_df[ , 'this_H_y' ] * zenga_df[ , 'this_K_y' ]^2 ) ) )

	my_outvec <- rowSums( zenga_df[ , paste0( 'line' , 1:5 ) ] )



  z_if <- 1/design$prob
  z_if <- z_if[ordincvar]
  z_if[ z_if != 0 ] <- as.numeric( my_outvec )
  z_if <- z_if[ order(ordincvar) ]

  variance <- survey::svyrecvar( z_if/design$prob, design$cluster, design$strata, design$fpc, postStrata = design$postStrata)

  colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
  class(rval) <- c( "cvystat" , "svystat" )
  attr(rval, "var") <- variance
  attr(rval, "statistic") <- "zenga"
  attr(rval, "lin") <- z_if

  return( rval )

}

#' @rdname svyzenga
#' @export
svyzenga.svyrep.design <- function(formula, design, na.rm=FALSE, ...) {

  incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

  if(na.rm){
    nas<-is.na(incvar)
    design<-design[!nas,]
    df <- model.frame(design)
    incvar <- incvar[!nas]
  }

  if ( any(incvar < 0, na.rm = TRUE) ) stop( "The Zenga index is defined for non-negative numeric variables only." )

  calc.zenga <- function( x, weights ) {

    x <- x[weights != 0 ]
    weights <- weights[weights != 0 ]
    ordx <- order(x)

    x <- x[ ordx ]
    weights <- weights[ ordx ]

    N <- sum(weights)
    Tot <- sum(weights * x)
    H_y <- cumsum(weights)
    K_y <- Tot - ( cumsum(weights * x) - weights*x )
    # K_y <- Tot - cumsum(weights * incvar)


    1 - sum(  weights * ( N - H_y )*( Tot - K_y ) / ( N * H_y * K_y ) )

  }

  ws <- weights(design, "sampling")
  rval <- calc.zenga( x = incvar, weights = ws )
  ww <- weights(design, "analysis")
  qq <- apply(ww, 2, function(wi) calc.zenga(incvar, wi))
  if ( any(is.na(qq))) {
    variance <- as.matrix(NA)
    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- c( "cvystat" , "svrepstat" )
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "zenga"
    return(rval)

  } else {
    variance <- survey::svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)

    variance <- as.matrix( variance )
  }

  colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
  class(rval) <- c( "cvystat" , "svrepstat" )
  attr(rval, "var") <- variance
  attr(rval, "statistic") <- "zenga"
  return(rval)

}

#' @rdname svyzenga
#' @export
svyzenga.DBIsvydesign <-
  function (formula, design, ...) {

  design$variables <- getvars(formula, design$db$connection, design$db$tablename, updates = design$updates, subset = design$subset)

    NextMethod("svyzenga", design)
  }
