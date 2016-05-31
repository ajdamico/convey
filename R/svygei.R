#' Generalized entropy index
#'
#' Estimate the generalized entropy index, a measure of inequalty
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param epsilon a parameter that determines the sensivity towards inequality in the top of the distribution. Defaults to epsilon = 1.
#' @param na.rm Should cases with missing values be dropped?
#'
#' @details you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
#'
#' If \code{epsilon == 0} or \code{epsilon == 1}, the logarithm in the function only allows for strictly positive variables.
#'
#' @return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of the statistic.
#'
#' @author Djalma Pessoa, Anthony Damico and Guilherme Jacob
#'
#' @seealso \code{\link{svyatk}}
#'
#' @references Matti Langel (2012). Measuring inequality in finite population sampling.
#'PhD thesis: Université de Neuchâtel,
#' URL \url{https://doc.rero.ch/record/29204/files/00002252.pdf}.
#'
#'Martin Biewen and Stephen Jenkins (2002). Estimation of Generalized Entropy
#'and Atkinson Inequality Indices from Complex Survey Data. \emph{DIW Discussion Papers},
#' No.345,
#' URL \url{https://www.diw.de/documents/publikationen/73/diw_01.c.40394.de/dp345.pdf}.
#' @keywords survey
#'
#' @examples
#' library(survey)
#' library(vardpoor)
#' data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )
#'
#' # linearized design
#' des_eusilc <- svydesign( ids = ~rb030 , strata = ~db040 ,  weights = ~rb050 , data = eusilc )
#'
#' library(convey)
#' des_eusilc <- convey_prep(des_eusilc)
#' svygei( ~eqincome , design = des_eusilc )
#'
#' # replicate-weighted design
#' des_eusilc_rep <- survey:::as.svrepdesign( des_eusilc , type = "bootstrap" )
#' svygei( ~eqincome , design = des_eusilc_rep, epsilon = .5 )
#'
#' # linearized design using a variable with missings
#' svygei( ~py010n , design = des_eusilc, epsilon = .5 )
#' svygei( ~py010n , design = des_eusilc, epsilon = .5, na.rm = TRUE )
#' # replicate-weighted design using a variable with missings
#' svygei( ~py010n , design = des_eusilc_rep, epsilon = .5 )
#' svygei( ~py010n , design = des_eusilc_rep, epsilon = .5, na.rm = TRUE )
#'
#'
#' # database-backed design
#' require(RSQLite)
#' tfile <- tempfile()
#' conn <- dbConnect( SQLite() , tfile )
#' dbWriteTable( conn , 'eusilc' , eusilc )
#'
#' dbd_eusilc <- svydesign(ids = ~rb030 , strata = ~db040 ,  weights = ~rb050 , data="eusilc", dbname=tfile, dbtype="SQLite")
#' dbd_eusilc <- convey_prep( dbd_eusilc )
#'
#' svygei( ~eqincome , design = dbd_eusilc, epsilon = .5 )
#'
#' @export
#'

svygei <- function(formula, design, ...) {

  UseMethod("svygei", design)

}


#' @rdname svygei
#' @export
svygei.survey.design <- function ( formula, design, epsilon = 1, na.rm = FALSE, ... ) {
  if (epsilon < 0 ) { stop( "epsilon has to be [0,+Inf)") }
  incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

  if (na.rm) {
    nas <- is.na(incvar)
    design <- design[nas == 0, ]
    if (length(nas) > length(design$prob))
      incvar <- incvar[nas == 0]
    else incvar[nas > 0] <- 0
  }

  if ( epsilon %in% c(0,1) & any(incvar == 0) ) { stop( paste("the GEI is undefined for zero incomes if epsilon ==", epsilon) ) }

  w <- 1/design$prob
  N <- sum( w )

  # Jenkins & Biewen's U and T functions:
  U_fn <- function( x, weights, gamma ) {
    return( sum( weights * x^gamma ) )
  }
  T_fn <- function( x, weights, gamma ) {
    return( sum( weights * x^gamma * log( x ) ) )
  }

  calc.gei <- function( x, weights, epsilon ) {
    if ( epsilon == 0 ) {
      result.est <- -T_fn(x,w,0)/U_fn(x,w,0) + log(U_fn(x,w,1)/U_fn(x,w,0))
    } else if ( epsilon == 1 ) {
      result.est <- (T_fn(x,w,1)/U_fn( x, w, 1)) - log( U_fn( x, w, 1)/U_fn( x, w, 0) )
    } else {
      result.est <- ((epsilon)*((epsilon)-1))^(-1)*(U_fn( x, w, 0)^((epsilon)-1)*U_fn( x, w, 1)^(-(epsilon))*U_fn( x, w, epsilon)-1)
    }

    result <- NULL
    result$gei.result <- result.est

    return( result )
  }

  result <- NULL
  result <- calc.gei( x = incvar, w = w, epsilon = epsilon )

  rval <- result$gei.result

  if ( epsilon == 0 ) {
    v <- -U_fn(incvar,w,0)^(-1)*log(incvar) + U_fn(incvar,w,1)^(-1)*incvar + U_fn(incvar,w,0)^(-1)*(T_fn(incvar,w,0)*U_fn(incvar,w,0)^(-1) - 1 )
    v[w == 0] <- NA
    variance <- survey::svyrecvar(v/design$prob, design$cluster,
                                  design$strata, design$fpc, postStrata = design$postStrata)
  } else if ( epsilon == 1) {
    v <- U_fn(incvar,w,1)^(-1)*incvar*log(incvar) - U_fn(incvar,w,1)^(-1)*(T_fn(incvar,w,1)*U_fn(incvar,w,1)^(-1)+1)*incvar + U_fn(incvar,w,0)^(-1)
    v[w == 0] <- NA
    variance <- survey::svyrecvar(v/design$prob, design$cluster,
                                  design$strata, design$fpc, postStrata = design$postStrata)
  } else {
    v <- (epsilon)^(-1)*U_fn(incvar,w,epsilon)*U_fn(incvar,w,1)^(-(epsilon))*U_fn(incvar,w,0)^((epsilon)-2) -
      ((epsilon)-1)^(-1)*U_fn(incvar,w,epsilon)*U_fn(incvar,w,1)^(-(epsilon)-1)*U_fn(incvar,w,0)^((epsilon)-1)*incvar +
      ((epsilon)^2-(epsilon))^(-1)*U_fn(incvar,w,0)^((epsilon)-1)*U_fn(incvar,w,1)^(-(epsilon))*incvar^(epsilon)
    v[w == 0] <- NA
    variance <- survey::svyrecvar(v/design$prob, design$cluster,
                                  design$strata, design$fpc, postStrata = design$postStrata)
  }

  colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
  class(rval) <- "cvystat"
  attr(rval, "var") <- variance
  attr(rval, "statistic") <- "generalized entropy index"
  attr(rval,"epsilon")<- epsilon
  rval

}


#' @rdname svygei
#' @export
svygei.svyrep.design <- function(formula, design, epsilon = 1,na.rm=FALSE, ...) {
  if (epsilon < 0 ) { stop( "epsilon has to be [0,+Inf) ") }
  df <- model.frame(design)
  incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

  if ( epsilon %in% c(0,1) & any(incvar == 0) ) { stop( paste("the GEI is undefined for zero incomes if epsilon ==", epsilon) ) }

  if(na.rm){
    nas<-is.na(incvar)
    design<-design[!nas,]
    df <- model.frame(design)
    incvar <- incvar[!nas]
  }

  # Jenkins & Biewen's U and T functions:
  U_fn <- function( x, weights, gamma ) {
    return( sum( weights * x^gamma ) )
  }
  T_fn <- function( x, weights, gamma ) {
    return( sum( weights * x^gamma * log( x ) ) )
  }

  calc.gei <- function( x, w, epsilon ) {
    x <- x[ w != 0 ]
    w <- w[ w != 0 ]

    if ( epsilon == 0 ) {
      result.est <- -T_fn(x,w,0)/U_fn(x,w,0) + log(U_fn(x,w,1)/U_fn(x,w,0))
    } else if ( epsilon == 1 ) {
      result.est <- (T_fn(x,w,1)/U_fn( x, w, 1)) - log( U_fn( x, w, 1)/U_fn( x, w, 0) )
    } else {
      result.est <- ((epsilon)*((epsilon)-1))^(-1)*(U_fn( x, w, 0)^((epsilon)-1)*U_fn( x, w, 1)^(-(epsilon))*U_fn( x, w, epsilon)-1)
    }

    return( result.est )

  }

  ws <- weights(design, "sampling")
  rval <- calc.gei( x = incvar, w = ws, epsilon = epsilon)
  ww <- weights(design, "analysis")
  qq <- apply(ww, 2, function(wi) calc.gei(incvar, wi, epsilon = epsilon))
  if ( any(is.na(qq))) {
    variance <- as.matrix(NA)
    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- "cvystat"
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "generalized entropy index"

    return(rval)

  } else {
    variance <- survey:::svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)

    variance <- as.matrix( variance )
  }

  colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
  class(rval) <- "cvystat"
  attr(rval, "var") <- variance
  attr(rval, "statistic") <- "generalized entropy index"
  return(rval)
}


#' @rdname svygei
#' @export
svygei.DBIsvydesign <-
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

    NextMethod("svygei", design)
  }
