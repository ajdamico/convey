#' Atkinson index
#'
#' Estimate the Atkinson index, a measure of inequalty
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param epsilon a parameter that determines the sensivity towards inequality in the bottom of the distribution. Defaults to epsilon = 1.
#' @param na.rm Should cases with missing values be dropped?
#'
#'@details you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
#'
#' @return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of the statistic.
#'
#' @author Djalma Pessoa, Anthony Damico and Guilherme Jacob
#'
#' @seealso \code{\link{arpr}}
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
#' svyatk( ~eqincome , design = des_eusilc, epsilon = .5 )
#'
#' # replicate-weighted design
#' des_eusilc_rep <- survey:::as.svrepdesign( des_eusilc , type = "bootstrap" )
#' svyatk( ~eqincome , design = des_eusilc_rep, epsilon = .5 )
#'
#' # linearized design using a variable with missings
#' svyatk( ~py010n , design = des_eusilc, epsilon = .5 )
#' svyatk( ~py010n , design = des_eusilc, epsilon = .5, na.rm = TRUE )
#' # replicate-weighted design using a variable with missings
#' svyatk( ~py010n , design = des_eusilc_rep, epsilon = .5 )
#' svyatk( ~py010n , design = des_eusilc_rep, epsilon = .5, na.rm = TRUE )
#'
#'
#' # database-backed design
#' require(RSQLite)
#' tfile <- tempfile()
#' conn <- dbConnect( SQLite() , tfile )
#' dbWriteTable( conn , 'eusilc' , eusilc )
#'
#' dbd_eusilc <- svydesign(ids = ~rb030 , strata = ~db040 ,  weights = ~rb050 , data="eusilc", dbname=tfile, dbtype="SQLite")
#'
#' dbd_eusilc <- convey_prep( dbd_eusilc )
#' svyatk( ~eqincome , design = dbd_eusilc, epsilon = .5 )
#'
#' @export
#'

svyatk <- function(formula, design, ...) {

  UseMethod("svyatk", design)

}


#' @rdname svyatk
#' @export
svyatk.survey.design <- function ( formula, design, epsilon = 1, na.rm = FALSE, ... ) {
  if (epsilon <= 0 ) { stop( "epsilon has to be (0,+Inf) ") }
  incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

  if ( any(incvar <= 0) ) { warning( "The function is defined for strictly positive incomes only.")
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

  ordincvar<-order(incvar)
  w <- w[ordincvar]
  incvar <- incvar[ordincvar]

  # population size
  N <- sum(w)
  # total income
  Y <- sum(incvar * w)
  # cumulative weight
  r <- cumsum(w)
  # partial weighted function
  G <- cumsum(incvar * w)

  wtd.generalized.mean <- function( y, weights, gamma ) {
    if (gamma == 0) {
      result <- exp( sum( weights * log(x) ) / sum( weights ) )
    } else {
      result <- ( ( sum( weights * x^(gamma) ) / sum( weights ) ) )^(1/gamma)
    }
    return( result )
  }

  calc.atkinson <- function( x, w, epsilon ) {
    N <- sum( w )
    x_bar <- sum( x * w ) / N
    if ( epsilon != 1 ) {
      b <- x[w != 0]^( 1 - epsilon )
      B <- sum( w[w != 0] * b )
      atk.result <- 1 - ( N^( epsilon/(epsilon-1) ) ) * ( B^(1/(1-epsilon) ) ) / sum( w[w != 0] * x[w != 0] )
    } else {
      b <- log( x[w != 0], base = exp(1) )
      B <- sum( w[w != 0] * b )
      atk.result <- 1 - ( N / sum( x[w != 0] * w[w != 0] ) ) * exp( B/N )
    }

    result <- NULL
    result$atk.result <- atk.result
    result$b <- b
    result$B <- B

    return( result )

  }

  result <- NULL
  result <- calc.atkinson( x = incvar, w = w, epsilon = epsilon )

  rval <- result$atk.result

  if ( epsilon != 1 ) {
    v <- (1 - result$atk.result) * ( (incvar[w != 0]/sum(w[w != 0]*incvar[w != 0])) - ( result$b/((1-epsilon)*result$B) ) - epsilon/( (epsilon-1)*N ) )
    #v[w == 0] <- 0
    v[w == 0] <- NA
    variance <- svyrecvar(v/design$prob, design$cluster,
                          design$strata, design$fpc, postStrata = design$postStrata)
  } else {
    v <- ( (1 - result$atk.result) / N ) * ( (N*incvar[w != 0]/sum(w[w != 0]*incvar[w != 0])) - result$b - 1 + (result$B/N) )
    #v[w == 0] <- 0
    v[w == 0] <- NA
    variance <- svyrecvar(v/design$prob, design$cluster,
                          design$strata, design$fpc, postStrata = design$postStrata)
  }

  colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
  class(rval) <- "cvystat"
  attr(rval, "var") <- variance
  attr(rval, "statistic") <- "atkinson"
  attr(rval,"epsilon")<- epsilon
  rval

}


#' @rdname svyatk
#' @export
svyatk.svyrep.design <- function(formula, design, epsilon = 1,na.rm=FALSE, ...) {
  df <- model.frame(design)
  incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

  if(na.rm){
    nas<-is.na(incvar)
    design<-design[!nas,]
    df <- model.frame(design)
    incvar <- incvar[!nas]
  }

  if ( any(incvar <= 0) ) { warning( "The function is defined for strictly positive incomes only.")
    nps <- incvar <= 0
    design <- design[nps == 0 ]
    if (length(nps) > length(design$prob))
      incvar <- incvar[nps == 0]
    else incvar[nps > 0] <- 0
  }

  calc.atkinson <- function( x, w, epsilon ) {
    N <- sum( w )
    x_bar <- sum( x * w ) / N
    if ( epsilon != 1 ) {
      b <- x[w != 0]^( 1 - epsilon )
      B <- sum( w[w != 0] * b )
      atk.result <- 1 - ( N^( epsilon/(epsilon-1) ) ) * ( B^(1/(1-epsilon) ) ) / sum( w[w != 0] * x[w != 0] )
    } else {
      b <- log( x[w != 0], base = exp(1) )
      B <- sum( w[w != 0] * b )
      atk.result <- 1 - ( N / sum( x[w != 0] * w[w != 0] ) ) * exp( B/N )
    }

    return( atk.result )

  }
  ws <- weights(design, "sampling")
  rval <- calc.atkinson( x = incvar, w = ws, epsilon = epsilon)
  ww <- weights(design, "analysis")
  qq <- apply(ww, 2, function(wi) calc.atkinson(incvar, wi, epsilon = epsilon))
  if ( any(is.na(qq))) {
    variance <- as.matrix(NA)
    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- "cvystat"
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "atkinson"

    return(rval)

  } else {
    variance <- survey:::svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)

    variance <- as.matrix( variance )
  }

  colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
  class(rval) <- "cvystat"
  attr(rval, "var") <- variance
  attr(rval, "statistic") <- "atkinson"
  return(rval)
}


#' @rdname svyatk
#' @export
svyatk.DBIsvydesign <-
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

    NextMethod("svyatk", design)
  }
