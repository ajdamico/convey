#' Zenga index
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
#' \dontrun{
#' library(survey)
#' library(vardpoor)
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
#' # subset all designs to positive income and non-missing records only
#' des_eusilc_pos_inc <- subset( des_eusilc , eqincome > 0 )
#' des_eusilc_rep_pos_inc <- subset( des_eusilc_rep , eqincome > 0 )
#'
#'
#' # variable without missing values
#' svyzenga(~eqincome, des_eusilc_pos_inc)
#' svyzenga(~eqincome, des_eusilc_rep_pos_inc)
#'
#' # subsetting:
#' svyzenga(~eqincome, subset( des_eusilc_pos_inc, db040 == "Styria"))
#' svyzenga(~eqincome, subset( des_eusilc_rep_pos_inc, db040 == "Styria"))
#'
#' # variable with with missings (but subsetted to remove negatives)
#' svyzenga(~py010n, subset( des_eusilc, py010n > 0 | is.na(py010n)) )
#' svyzenga(~py010n, subset( des_eusilc_rep, py010n > 0 | is.na(py010n)) )
#'
#' svyzenga(~py010n, subset( des_eusilc, py010n > 0 | is.na(py010n)), na.rm = TRUE)
#' svyzenga(~py010n, subset( des_eusilc_rep, py010n > 0 | is.na(py010n)), na.rm = TRUE)
#'
#'
#' # library(MonetDBLite) is only available on 64-bit machines,
#' # so do not run this block of code in 32-bit R
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
#'
#' # subset all designs to positive income and non-missing records only
#' dbd_eusilc_pos_inc <- subset( dbd_eusilc , eqincome > 0 )
#'
#' # variable without missing values
#' svyzenga(~eqincome, dbd_eusilc_pos_inc)
#'
#' # subsetting:
#' svyzenga(~eqincome, subset( dbd_eusilc_pos_inc, db040 == "Styria"))
#'
#' # variable with with missings (but subsetted to remove negatives)
#' svyzenga(~py010n, subset( dbd_eusilc, py010n > 0 | is.na(py010n)) )
#'
#' # svyzenga(~py010n, subset( dbd_eusilc, py010n > 0 | is.na(py010n)), na.rm = TRUE)
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

  UseMethod("svyzenga", design)

}

#' @rdname svyzenga
#' @export
svyzenga.survey.design <- function( formula, design, na.rm = FALSE, ... ) {

  if (is.null(attr(design, "full_design"))) stop("you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svydesign() function.")

  incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

  w <- 1/design$prob

  if ( any( incvar[w != 0] <= 0, na.rm = TRUE ) ) { warning( "The function is defined for strictly positive incomes only.")
    nps <- incvar <= 0
    design <- design[nps == 0 ]
    if (length(nps) > length(design$prob))
      incvar <- incvar[nps == 0]
    else incvar[nps > 0] <- 0
  }

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
    class(rval) <- "cvystat"
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

  lin_zenga <- function(i) {

    u <- incvar[i]

    - ( N - H_y[i] )*( Tot - K_y[i] ) / ( N * H_y[i] * K_y[i] ) -
      (1/N^2) * sum( w * ( Tot - K_y ) / K_y ) -
      (u/N) * sum( w * (N - H_y) / (H_y * K_y) ) +
      sum( w * ( ( Tot - K_y ) / ( H_y^2 * K_y ) ) * ( incvar >= u ) ) +
      (Tot*u/N) * sum( w * ( ( N - H_y ) / ( H_y * K_y^2 ) ) * ( u >= incvar ) )

  }

  z_if <- 1/design$prob
  z_if <- z_if[ordincvar]
  z_if[ z_if != 0 ] <- as.numeric( lapply(1:length(incvar), function(x) lin_zenga(i = x ) ) )
  z_if <- z_if[ order(ordincvar) ]

  variance <- survey::svyrecvar( z_if/design$prob, design$cluster,
                                 design$strata, design$fpc, postStrata = design$postStrata)

  colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
  class(rval) <- "cvystat"
  attr(rval, "var") <- variance
  attr(rval, "statistic") <- "zenga"

  return( rval )

}

#' @rdname svyzenga
#' @export
svyzenga.svyrep.design <- function(formula, design, na.rm=FALSE, ...) {
  incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

  if (is.null(attr(design, "full_design"))) stop("you must run the ?convey_prep function on your replicate-weighted survey design object immediately after creating it with the svrepdesign() function.")


  if(na.rm){
    nas<-is.na(incvar)
    design<-design[!nas,]
    df <- model.frame(design)
    incvar <- incvar[!nas]
  }

  if ( any(incvar <= 0, na.rm = TRUE) ) { warning( "The function is defined for strictly positive incomes only.")
    nps <- incvar <= 0
    nps[ is.na(nps) ] <- 0
    design <- design[ nps == 0 ]
    if (length(nps) > length(design$prob)) {
      incvar <- incvar[nps == 0]
    } else { incvar[nps > 0] <- 0 }

  }

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
    class(rval) <- "cvystat"
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "zenga"
    return(rval)

  } else {
    variance <- survey::svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)

    variance <- as.matrix( variance )
  }

  colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
  class(rval) <- "cvystat"
  attr(rval, "var") <- variance
  attr(rval, "statistic") <- "zenga"
  return(rval)

}

#' @rdname svyzenga
#' @export
svyzenga.DBIsvydesign <-
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

    NextMethod("svyzenga", design)
  }
