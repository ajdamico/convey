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
#' @references Matti Langel (2012). Measuring inequality in finite population sampling.
#' PhD thesis: Universite de Neuchatel,
#' URL \url{https://doc.rero.ch/record/29204/files/00002252.pdf}.
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
#' # replicate-weighted design
#' des_eusilc_rep <- as.svrepdesign( des_eusilc , type = "bootstrap" )
#' des_eusilc_rep <- convey_prep(des_eusilc_rep)
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
#' # Variable without missing values
#' svyzenga(~eqincome, des_eusilc, na.rm = FALSE)
#' svyzenga(~eqincome, dbd_eusilc, na.rm = FALSE)
#' svyzenga(~eqincome, des_eusilc_rep, na.rm = FALSE)
#'
#' # Subsetting:
#' svyzenga(~eqincome, subset( des_eusilc, db040 == "Styria"), na.rm = FALSE)
#' svyzenga(~eqincome, subset( dbd_eusilc, db040 == "Styria"), na.rm = FALSE)
#' svyzenga(~eqincome, subset( des_eusilc_rep, db040 == "Styria"), na.rm = FALSE)
#'
#' # Variable with missing values and subsetting:
#' svyzenga(~py010n, subset( des_eusilc, db040 == "Styria"), na.rm = FALSE)
#' svyzenga(~py010n, subset( dbd_eusilc, db040 == "Styria"), na.rm = FALSE)
#' svyzenga(~py010n, subset( des_eusilc_rep, db040 == "Styria"), na.rm = FALSE)
#'
#' svyzenga(~py010n, subset( des_eusilc, db040 == "Styria"), na.rm = TRUE)
#' svyzenga(~py010n, subset( dbd_eusilc, db040 == "Styria"), na.rm = TRUE)
#' svyzenga(~py010n, subset( des_eusilc_rep, db040 == "Styria"), na.rm = TRUE)
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' @export
svyzenga <- function(formula, design, ...) {

	if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

	UseMethod("svyzenga", design)

}

#' @rdname svyzenga
#' @export
svyzenga.survey.design <- function( formula, design, na.rm = FALSE, ... ) {
  y <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

  d <- 1/design$prob

  if ( any(y[d != 0] <= 0) ) { warning( "The function is defined for strictly positive incomes only.")
    nps <- y <= 0
    design <- design[nps == 0 ]
    if (length(nps) > length(design$prob))
      y <- y[nps == 0]
    else y[nps > 0] <- 0
  }

  if (na.rm) {
    nas <- is.na(y)
    design <- design[nas == 0, ]
    if (length(nas) > length(design$prob))
      y <- y[nas == 0]
    else y[nas > 0] <- 0
  }

  d <- 1/design$prob

  y <- y[ d != 0 ]
  d <- d[ d != 0 ]

  if ( any( is.na(y) ) ) {
    rval <- as.numeric(NA)
    variance <- as.matrix(NA)
    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- "cvystat"
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "zenga"
    return(rval)

  }
  ordy <- order(y)
  d <- d[ ordy ]
  y <- y[ ordy ]

  D = sum(d)
  D_k = cumsum(d)
  D_k_1 = c(0, D_k[ -length(D_k) ] )
  alpha_k = D_k/D
  alpha_k_1 = c(0, alpha_k[ -length(alpha_k) ] )

  Y = sum( d * y )
  Y_k = cumsum( d * y )
  Y_k_1 = c(0, Y_k[ -length(Y_k) ] )
  k <- which( (alpha_k_1 < alpha_k ) & ( alpha_k <= alpha_k) )
  n <- max(k)
  Y_alpha = Y_k_1 + y * ( alpha_k*D - D_k_1)

  A_k <- NULL
  A_k[ k == 0 ] <- 0
  A_k[ k == 1 ] <- 0
  A_k[ k >= 2 ] <- (D_k_1*y - Y_k_1)[ k >= 2 ]

  Z_k <- NULL
  Z_k[ k == 1 ] <- ( Y / ( D * y[ k==1 ] ) - 1 ) * log( Y / (Y - Y_k[ k == 1] ) )
  Z_k[ k > 1 & k < n ] <- ( ( A_k/(Y + A_k) ) * log( D_k/D_k_1 ) + ( Y/(D*y) - Y/( Y + A_k ) ) * log( ( Y - Y_k_1 ) / (Y - Y_k ) ) ) [ k > 1 & k < n ]
  Z_k[ k == n ] <- ( 1 - Y/( D * y[ k==n ] ) ) * log( D_k[k==n]/D_k[k==(n-1)] )

  rval <- sum( Z_k )


  v_l <- NULL

  for ( l in 1:length(y) ) {

    u_l <- NULL
    u_l[ k == 1 ] = ( (D*y[l] - Y)/(D^2)*y[1])*log( Y / ( Y - Y_k[1] ) ) + y[l]*( Y/(D*y[1]) - 1 ) * ( 1/Y - 1 * (l > 1)/ ( Y - Y_k [1] ) )
    u_l[ k == n ] = ( ( Y - D*y[l] )/( y[k==n]*D^2 ) ) * log ( D / D_k[ k == (n-1) ] ) + ( 1 - Y/(D*y[k==n]) ) * ( 1/D - 1*(l < n)/D_k[k == (n-1) ] )
    u_l[ k > 1 & k < n ] = ( ( ( Y * ( y - y[l] ) * ( l < k ) - A_k * y[l] )/(Y + A_k)^2 ) * log( D_k*( Y - Y_k_1 )/( D_k_1 * (Y - Y_k) ) ) +
                               (A_k/(Y + A_k))*( (l <= k)/D_k - (l < k)/D_k_1 ) + ( ( D*y[l] - Y )/(y*D^2) )*log( (Y - Y_k_1)/(Y - Y_k) ) +
                               ( Y*y[l]/(Y - Y_k_1 ) )*( 1*( l == k ) - ( (y*d)*(l > k)/(Y - Y_k) ) ) * ( 1/(D*y) - 1/(Y + A_k) ) ) [ k > 1 & k < n ]

    v_l[l] <- sum( u_l )

  }

  v_l <- v_l[ order(ordy) ]

  v <- 1/design$prob
  v[ v!= 0 ] <- v_l

  variance <- survey::svyrecvar(v/design$prob, design$cluster,
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

  if(na.rm){
    nas<-is.na(incvar)
    design<-design[!nas,]
    df <- model.frame(design)
    incvar <- incvar[!nas]
  }

  if ( any(incvar <= 0) ) { warning( "The function is defined for strictly positive incomes only.")
    nps <- incvar <= 0
    nps[ is.na(nps) ] <- 0
    design <- design[ nps == 0 ]
    if (length(nps) > length(design$prob)) {
      incvar <- incvar[nps == 0]
    } else { incvar[nps > 0] <- 0 }

  }

  # Auxiliary functions:
  Y_fn <- function( x, weights, desl = 0, ordered = FALSE ) {
    x <- x[weights != 0 ]
    weights <- weights[weights != 0 ]
    ordx <- order(x)

    x <- x[ ordx ]
    weights <- weights[ ordx ]

    result <- cumsum(weights * x)

    if (desl != 0) {
      result <- c( rep(0,desl), result[ seq( 1, length(result) - desl, 1 ) ] )
    }

    if (ordered) {
      return( result )
    }

    result[order(ordx)]

  }

  D_fn <- function( x, weights, desl = 0, ordered = FALSE ) {
    x <- x[weights != 0 ]
    weights <- weights[weights != 0 ]
    ordx <- order(x)

    x <- x[ ordx ]
    weights <- weights[ ordx ]

    result <- cumsum(weights)

    if (desl != 0) {
      result <- c( rep(0,desl), result[ seq( 1, length(result) - desl, 1 ) ] )
    }

    if (ordered) {
      return( result )
    }

    result[order(ordx)]

  }


  A_fn <- function( x, weights, ordered = FALSE ) {

    x <- x[weights != 0 ]
    weights <- weights[weights != 0 ]
    ordx <- order(x)

    x <- x[ ordx ]
    weights <- weights[ ordx ]

    result <- D_fn(x,weights,1) * x - Y_fn(x,weights,1)

    if (ordered) {
      return(result)
    }

    result[ order(ordx) ]

  }

  calc.zenga <- function( x, weights ) {
    x <- x[weights != 0 ]
    weights <- weights[weights != 0 ]
    ordx <- order(x)

    x <- x[ ordx ]
    weights <- weights[ ordx ]

    z_1 <- ( sum(weights*x)/(sum(weights) * x[1] ) - 1) * log( sum(weights*x)/(sum(weights*x) - Y_fn(x,weights,0,TRUE)[1]) )
    z_n <- (1 - sum(weights*x)/(sum(weights)*x[length(x)])) * log(D_fn(x,weights,0,TRUE)[length(x)]/D_fn(x,weights,1,TRUE)[length(x)])
    z <- (A_fn(x,weights)/(sum(weights*x)+A_fn(x,weights))) * log(D_fn(x,weights)/D_fn(x,weights,1)) +
      ( sum(weights * x)/(sum(weights) * x) - sum(weights*x)/(sum(weights*x)+A_fn(x,weights) ) ) *
      log( (sum(weights * x) - Y_fn(x,weights,1))/(sum(weights * x) - Y_fn(x,weights,0)) )
    z[1] <- z_1
    z[length(z)] <- z_n
    return(sum(z))

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

      full_design$variables <- survey:::getvars(formula, attr( design , "full_design" )$db$connection, attr( design , "full_design" )$db$tablename,
                                                updates = attr( design , "full_design" )$updates, subset = attr( design , "full_design" )$subset)

      attr( design , "full_design" ) <- full_design

      rm( full_design )

    }

    design$variables <- survey:::getvars(formula, design$db$connection, design$db$tablename,
                                         updates = design$updates, subset = design$subset)

    NextMethod("svyzenga", design)
  }
