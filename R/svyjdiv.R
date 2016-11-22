#' J-divergence measure
#'
#' Estimate the j-divergence measure, an entropy-based measure of inequality
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param na.rm Should cases with missing values be dropped?
#' @param ... future expansion
#'
#' @details you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
#'
#' @return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of the statistic.
#'
#' @author Guilherme Jacob
#'
#' @seealso \code{\link{svygei}}
#'
#' @references Nicholas Rohde (2016). J-divergence measurements of economic inequality.
#' J. R. Statist. Soc. A, v. 179, Part 3 (2016), pp. 847-870.
#' URL \url{http://onlinelibrary.wiley.com/doi/10.1111/rssa.12153/abstract}.
#'
#' Martin Biewen and Stephen Jenkins (2002). Estimation of Generalized Entropy
#' and Atkinson Inequality Indices from Complex Survey Data. \emph{DIW Discussion Papers},
#' No.345,
#' URL \url{https://www.diw.de/documents/publikationen/73/diw_01.c.40394.de/dp345.pdf}.
#'
#' @keywords survey
#'
#' @examples
#' library(survey)
#' library(vardpoor)
#' data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )
#'
#' # linearized design
#' des_eusilc <- svydesign( ids = ~rb030 , strata = ~db040 ,  weights = ~rb050 , data = eusilc )
#' des_eusilc <- convey_prep(des_eusilc)
#'
#' svyjdiv( ~eqincome , design = des_eusilc )
#'
#' # replicate-weighted design
#' des_eusilc_rep <- as.svrepdesign( des_eusilc , type = "bootstrap" )
#' des_eusilc_rep <- convey_prep(des_eusilc_rep)
#'
#' svyjdiv( ~eqincome , design = des_eusilc_rep )
#'
#' # linearized design using a variable with missings
#' svyjdiv( ~py010n , design = des_eusilc )
#' svyjdiv( ~py010n , design = des_eusilc, na.rm = TRUE )
#' # replicate-weighted design using a variable with missings
#' svyjdiv( ~py010n , design = des_eusilc_rep )
#' svyjdiv( ~py010n , design = des_eusilc_rep, na.rm = TRUE )
#'
#' # library(MonetDBLite) is only available on 64-bit machines,
#' # so do not run this block of code in 32-bit R
#' \dontrun{
#'
#' # database-backed design
#' library(MonetDBLite)
#' library(DBI)
#' dbfolder <- tempdir()
#' conn <- dbConnect( MonetDBLite::MonetDBLite() , dbfolder )
#' dbWriteTable( conn , 'eusilc' , eusilc )
#'
#' dbd_eusilc <-
#' 	svydesign(
#' 		ids = ~rb030 ,
#' 		strata = ~db040 ,
#' 		weights = ~rb050 ,
#' 		data="eusilc",
#' 		dbname=dbfolder,
#' 		dbtype="MonetDBLite"
#' 	)
#'
#' dbd_eusilc <- convey_prep( dbd_eusilc )
#'
#' svyjdiv( ~eqincome , design = dbd_eusilc )
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' dbDisconnect( conn , shutdown = TRUE )
#'
#' }
#'
#' @export
svyjdiv <- function(formula, design, ...) {

  if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

  UseMethod("svyjdiv", design)

}

#' @rdname svyjdiv
#' @export
svyjdiv.survey.design <- function ( formula, design, na.rm = FALSE, ... ) {

  if (is.null(attr(design, "full_design"))) stop("you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svydesign() function.")

  incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

  if (na.rm) {
    nas <- is.na(incvar)
    design <- design[nas == 0, ]
    if (length(nas) > length(design$prob)) incvar <- incvar[nas == 0] else incvar[nas > 0] <- 0
  }

  w <- 1/design$prob

  if ( any(incvar[w != 0] <= 0) ) {
    warning("The function is defined for strictly positive incomes only.  Discarding observations with zero or negative incomes.")
    nps <- incvar <= 0
    design <- design[!nps]
    if (length(nps) > length(design$prob)) incvar <- incvar[!nps] else incvar[ nps ] <- 0
  }

  w <- 1/design$prob

  rval <- NULL

  U_0 <- list( value = sum( w ), lin = rep( 1, length( incvar ) ) )
  U_1 <- list( value = sum( w * incvar ), lin = incvar )
  T_0 <- list( value = sum( w * log( incvar ) ), lin = log( incvar ) )
  T_1 <- list( value = sum( w * incvar * log( incvar ) ), lin = incvar * log( incvar ) )
  Y_AVG <- contrastinf( quote( U_1 / U_0 ), list(  U_0 = U_0, U_1 = U_1 ) )

  list_all <- list(  U_0 = U_0, U_1 = U_1, T_0 = T_0, T_1 = T_1, Y_AVG = Y_AVG )
  estimate <- contrastinf( quote( ( 1 / U_1 ) * ( T_1 - ( Y_AVG * T_0 ) - ( log( Y_AVG ) * U_1 - Y_AVG * log( Y_AVG ) * U_0 ) ) ) , list_all )

  rval <- estimate$value

  if ( is.na(rval) ) {
    variance <- as.matrix(NA)
    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- c( "cvystat" , "svystat" )
    attr(rval, "statistic") <- "j-divergence"
    attr(rval, "var") <- variance
    return(rval)
  }

  variance <- survey::svyrecvar( estimate$lin/design$prob, design$cluster, design$strata, design$fpc, postStrata = design$postStrata)

  colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
  class(rval) <- c( "cvystat" , "svystat" )
  attr(rval, "statistic") <- "j-divergence"
  attr(rval, "var") <- variance

  rval

}


#' @rdname svyjdiv
#' @export
svyjdiv.svyrep.design <- function ( formula, design, na.rm = FALSE, ... ) {

  if (is.null(attr(design, "full_design"))) stop("you must run the ?convey_prep function on your replicate-weighted survey design object immediately after creating it with the svrepdesign() function.")

  incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

  if(na.rm){
    nas<-is.na(incvar)
    design<-design[!nas,]
    df <- model.frame(design)
    incvar <- incvar[!nas]
  }

  ws <- weights(design, "sampling")

  if ( any( incvar[ws != 0] <= 0, na.rm = TRUE ) ) {
    warning("The function is defined for strictly positive incomes only.  Discarding observations with zero or negative incomes.")
    nps <- incvar <= 0
    nps[ is.na(nps) ] <- FALSE
    design <- design[!nps]
    if (length(nps) > length(design$prob)) incvar <- incvar[!nps] else incvar[ nps ] <- 0
    ws <- weights(design, "sampling")
  }

  rval <- calc.jdiv( x = incvar, weights = ws )
  if ( is.na(rval) ) {
    variance <- as.matrix(NA)
    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- c( "cvystat" , "svrepstat" )
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "j-divergence"

    return(rval)
  }

  ww <- weights(design, "analysis")
  qq <- apply(ww, 2, function(wi) calc.jdiv(incvar, wi ) )
  if ( any(is.na(qq))) {
    variance <- as.matrix(NA)
    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- c( "cvystat" , "svrepstat" )
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "j-divergence"

    return(rval)

  } else {
    variance <- survey::svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)

    variance <- as.matrix( variance )
  }
  colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
  class(rval) <- c( "cvystat" , "svrepstat" )
  attr(rval, "var") <- variance
  attr(rval, "statistic") <- "j-divergence"

  return( rval )

}

#' @rdname svyjdiv
#' @export
svyjdiv.DBIsvydesign <-
  function (formula, design, ...) {

    if (!( "logical" %in% class(attr(design, "full_design"))) ){

      full_design <- attr( design , "full_design" )

      full_design$variables <- getvars(formula, attr( design , "full_design" )$db$connection, attr( design , "full_design" )$db$tablename,
                                       updates = attr( design , "full_design" )$updates, subset = attr( design , "full_design" )$subset)

      attr( design , "full_design" ) <- full_design

      rm( full_design )

    }

    design$variables <- getvars(formula, design$db$connection, design$db$tablename,
                                updates = design$updates, subset = design$subset)

    NextMethod("svyjdiv", design)
  }


# J-divergence measure:
calc.jdiv <-  function( x, weights ) {

  x <- x[ weights > 0]
  weights <- weights[ weights > 0]

  avg <- sum( x * weights ) / sum( weights )
  jdiv = ( ( x - avg ) / avg ) * log( x / avg )

  return( sum( jdiv * weights ) / sum( weights ) )

}
