#' Coefficient of variation
#'
#' Estimate the coefficient of variation, a measure of inequality
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param squared a logical variable defining if the result is the squared or unsquared coefficient of variation. Defaults to \code{TRUE}.
#' @param na.rm Should cases with missing values be dropped?
#'
#' @details you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
#'
#' @return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of the statistic.
#'
#' @author Djalma Pessoa, Anthony Damico and Guilherme Jacob
#'
#' @seealso \code{\link{svygei}}
#'
#' @references Milorad Kovacevic and David Binder (1997). Variance Estimation for Measures of Income
#' Inequality and Polarization - The Estimating Equations Approach. \emph{Journal of Official Statistics},
#' Vol.13, No.1, 1997. pp. 41 58. URL \url{http://www.jos.nu/Articles/abstract.asp?article=13141}.
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
#' library(convey)
#' des_eusilc <- convey_prep(des_eusilc)
#'
#' # replicate-weighted design
#' des_eusilc_rep <- survey:::as.svrepdesign( des_eusilc , type = "bootstrap" )
#'
#' # database-backed design
#' require(RSQLite)
#' tfile <- tempfile()
#' conn <- dbConnect( SQLite() , tfile )
#' dbWriteTable( conn , 'eusilc' , eusilc )
#' dbd_eusilc <- svydesign(ids = ~rb030 , strata = ~db040 ,  weights = ~rb050 , data="eusilc", dbname=tfile, dbtype="SQLite")
#' dbd_eusilc <- convey_prep( dbd_eusilc )
#'
#' # variable no missing
#' svycv(~eqincome, des_eusilc, squared = TRUE, na.rm = FALSE )
#' svycv(~eqincome, dbd_eusilc, squared = TRUE, na.rm = FALSE )
#' svycv(~eqincome, des_eusilc_rep, squared = TRUE, na.rm = FALSE )
#'
#' svycv(~eqincome, des_eusilc, squared = FALSE, na.rm = FALSE )
#' svycv(~eqincome, dbd_eusilc, squared = FALSE, na.rm = FALSE )
#' svycv(~eqincome, des_eusilc_rep, squared = FALSE, na.rm = FALSE )
#'
#' # subset:
#' svycv(~eqincome, subset(des_eusilc, db040 == "Styria" ), squared = TRUE, na.rm = FALSE )
#' svycv(~eqincome, subset(dbd_eusilc, db040 == "Styria"), squared = TRUE, na.rm = FALSE )
#' svycv(~eqincome, subset(des_eusilc_rep, db040 == "Styria"), squared = TRUE, na.rm = FALSE )
#'
#' svycv(~py010n, subset(des_eusilc, db040 == "Styria"), squared = FALSE, na.rm = FALSE )
#' svycv(~py010n, subset(dbd_eusilc, db040 == "Styria"), squared = FALSE, na.rm = FALSE )
#' svycv(~py010n, subset(des_eusilc_rep, db040 == "Styria"), squared = FALSE, na.rm = FALSE )
#'
#' svycv(~py010n, subset(des_eusilc, db040 == "Styria"), squared = FALSE, na.rm = TRUE )
#' svycv(~py010n, subset(dbd_eusilc, db040 == "Styria"), squared = FALSE, na.rm = TRUE )
#' svycv(~py010n, subset(des_eusilc_rep, db040 == "Styria"), squared = FALSE, na.rm = TRUE )
#'
#' @export
#'

svycv <- function(formula, design, ...) {

  UseMethod("svycv", design)

}

#' @rdname svycv
#' @export
svycv.survey.design <- function ( formula, design, squared = TRUE, na.rm = FALSE, ... ) {
  incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

  if (na.rm) {
    nas <- is.na(incvar)
    design <- design[nas == 0, ]
    if (length(nas) > length(design$prob))
      incvar <- incvar[nas == 0]
    else incvar[nas > 0] <- 0
  }

  w <- 1/design$prob
  average <- sum( w * incvar ) / sum( w )

  rval <- sum( w * ( ( incvar/average ) - 1 )^2 ) / sum( w )

  if ( squared ) {

    u_i <- ( (incvar/average - 1 )^2 - (2*incvar/average - 1)*rval )/sum(w)
    variance <- survey::svyrecvar(u_i/design$prob, design$cluster,
                                  design$strata, design$fpc, postStrata = design$postStrata)
    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- "cvystat"
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "squared cv"

    return(rval)

  } else {
    rval <- sqrt(rval)
    u_i <-  (1/2) * ( ( (incvar/average - 1)^2)/rval - (2*incvar/average - 1)*rval )/sum(w)
    variance <- survey::svyrecvar(u_i/design$prob, design$cluster,
                                  design$strata, design$fpc, postStrata = design$postStrata)
    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- "cvystat"
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "cv"

    return(rval)

  }

}

#' @rdname svycv
#' @export
svycv.svyrep.design <- function(formula, design, squared = TRUE, na.rm=FALSE, ...) {

  incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

  if(na.rm){
    nas<-is.na(incvar)
    design<-design[!nas,]
    df <- model.frame(design)
    incvar <- incvar[!nas]
  }

  calc.cv <- function(x, weights){
    x <- x[weights != 0]
    weights <- weights[weights != 0]

    average <- sum( weights * x ) / sum( weights )

    return( sum( weights * ( (x/average) - 1 )^2 ) / sum( weights ) )

  }

  ws <- weights(design, "sampling")
  rval <- calc.cv( x = incvar, weights = ws )
  if (!squared) { rval <- sqrt(rval) }
  ww <- weights(design, "analysis")
  qq <- apply(ww, 2, function(wi) calc.cv(x = incvar, weights = wi) )
  if (!squared) { qq <- sqrt(qq) }

  if ( any(is.na(qq)) ) {
    variance <- as.matrix(NA)
  } else {
    variance <- survey:::svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)
    variance <- as.matrix( variance )
  }

  colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
  class(rval) <- "cvystat"
  attr(rval, "var") <- variance
  attr(rval, "statistic") <- "squared cv"
  if (!squared) { attr(rval, "statistic") <- "cv" }
  return(rval)

}

#' @rdname svycv
#' @export
svycv.DBIsvydesign <-
  function (formula, design, ...) {

    if (!( "logical" %in% class(attr(design, "full_design"))) ){

      full_design <- attr( design , "full_design" )

      full_design$variables <- survey:::getvars(formula, attr( design , "full_design" )$db$connection, attr( design , "full_design" )$db$tablename,
                                                updates = attr( design , "full_design" )$updates, subset = attr( design , "full_design" )$subset)

      attr( design , "full_design" ) <- full_design

      rm( full_design )

    }

    design$variables <- survey:::getvars(formula, design$db$connection, design$db$tablename,
                                         updates = design$updates, subset = design$subset)

    NextMethod("svycv", design)
  }
