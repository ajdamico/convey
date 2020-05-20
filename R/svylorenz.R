#' Lorenz curve
#'
#' Estimate the Lorenz curve, an inequality graph
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param quantiles a sequence of probabilities that defines the quantiles sum to be calculated
#' @param ci Should the confidence interval be plotted? Defaults to \code{TRUE}.
#' @param alpha a number that especifies de confidence level for the graph.
#' @param empirical Should an empirical Lorenz curve be estimated as well? Defaults to \code{FALSE}.
#' @param plot Should the Lorenz curve be plotted? Defaults to \code{TRUE}.
#' @param add Should a new curve be plotted on the current graph?
#' @param curve.col a string defining the color of the curve.
#' @param na.rm Should cases with missing values be dropped? Defaults to \code{FALSE}.
#' @param ... additional arguments passed to \code{plot} methods
#'
#' @details you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
#'
#' Notice that the 'empirical' curve is observation-based and is the one actually used to calculate the Gini index.
#' On the other hand, the quantile-based curve is used to estimate the shares, SEs and confidence intervals.
#'
#' This way, as the number of quantiles of the quantile-based function increases, the quantile-based curve approacches the observation-based curve.
#'
#' @return Object of class "\code{svyquantile}", which are vectors with a "\code{quantiles}" attribute giving the proportion of income below that quantile,
#' and a "\code{SE}" attribute giving the standard errors of the estimates.
#'
#' @author Guilherme Jacob, Djalma Pessoa and Anthony Damico
#'
#' @seealso \code{\link{svyquantile}}
#'
#' @references Milorad Kovacevic and David Binder (1997). Variance Estimation for Measures of Income
#' Inequality and Polarization - The Estimating Equations Approach. \emph{Journal of Official Statistics},
#' Vol.13, No.1, 1997. pp. 41 58. URL \url{https://www.scb.se/contentassets/ca21efb41fee47d293bbee5bf7be7fb3/variance-estimation-for-measures-of-income-inequality-and-polarization---the-estimating-equations-approach.pdf}.
#'
#' Shlomo Yitzhaki and Robert Lerman (1989). Improving the accuracy of estimates of Gini coefficients.
#' \emph{Journal of Econometrics}, Vol.42(1), pp. 43-47, September.
#'
#' Matti Langel (2012). \emph{Measuring inequality in finite population sampling}. PhD thesis. URL \url{http://doc.rero.ch/record/29204}.
#'
#' @keywords survey
#'
#' @examples
#'
#' library(survey)
#' library(laeken)
#' data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )
#'
#' # linearized design
#' des_eusilc <- svydesign( ids = ~rb030 , strata = ~db040 ,  weights = ~rb050 , data = eusilc )
#' des_eusilc <- convey_prep( des_eusilc )
#' svylorenz( ~eqincome , des_eusilc, seq(0,1,.05), alpha = .01 )
#'
#' # replicate-weighted design
#' des_eusilc_rep <- as.svrepdesign( des_eusilc , type = "bootstrap" )
#' des_eusilc_rep <- convey_prep( des_eusilc_rep )
#'
#' svylorenz( ~eqincome , des_eusilc_rep, seq(0,1,.05), alpha = .01 )
#'
#' \dontrun{
#'
#' # linearized design using a variable with missings
#' svylorenz( ~py010n , des_eusilc, seq(0,1,.05), alpha = .01 )
#' svylorenz( ~py010n , des_eusilc, seq(0,1,.05), alpha = .01, na.rm = TRUE )
#' # demonstration of `curve.col=` and `add=` parameters
#' svylorenz( ~eqincome , des_eusilc, seq(0,1,.05), alpha = .05 , add = TRUE , curve.col = 'green' )
#' # replicate-weighted design using a variable with missings
#' svylorenz( ~py010n , des_eusilc_rep, seq(0,1,.05), alpha = .01 )
#' svylorenz( ~py010n , des_eusilc_rep, seq(0,1,.05), alpha = .01, na.rm = TRUE )
#'
#'
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
#' svylorenz( ~eqincome , dbd_eusilc, seq(0,1,.05), alpha = .01 )
#'
#' # highlithing the difference between the quantile-based curve and the empirical version:
#' svylorenz( ~eqincome , dbd_eusilc, seq(0,1,.5), empirical = TRUE, ci = FALSE, curve.col = "green" )
#' svylorenz( ~eqincome , dbd_eusilc, seq(0,1,.5), alpha = .01, add = TRUE )
#' legend( "topleft", c("Quantile-based", "Empirical"), lwd = c(1,1), col = c("red", "green"))
#' # as the number of quantiles increases, the difference between the curves gets smaller
#' svylorenz( ~eqincome , dbd_eusilc, seq(0,1,.01), empirical = TRUE, ci = FALSE, curve.col = "green" )
#' svylorenz( ~eqincome , dbd_eusilc, seq(0,1,.01), alpha = .01, add = TRUE )
#' legend( "topleft", c("Quantile-based", "Empirical"), lwd = c(1,1), col = c("red", "green"))
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' dbDisconnect( conn , shutdown = TRUE )
#'
#' }
#'
#' @importFrom grDevices adjustcolor
#' @importFrom graphics abline lines plot points polygon
#' @importFrom utils tail
#'
#' @export
svylorenz <- function(formula, design, ...) {

  if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

  UseMethod("svylorenz", design)

}


# thanks to alex for these wrapper functions
# http://stackoverflow.com/a/37518103/1759499
svylorenzplot_wrap <-
  function( cex = 0.1 , xlab = "Cumulative Population Share" , ylab = "Total Income Share" , ... ){
    plot(
      NULL ,
      NULL ,
      xlim = c( 0 , 1 ) ,
      ylim = c( 0 , 1 ) ,
      cex = cex ,
      xlab = xlab ,
      ylab = ylab ,
      ...
    )
  }

svylorenzlines_wrap <-
  function( x = x , y = y , pch = 16 , cex = 0.1 , lwd = 1 , col = col , ... ){
    lines( x , y , xlim = c( 0 , 1 ) , ylim = c( 0 , 1 ) , pch = pch , cex = cex , lwd = lwd , col = col , ... )
  }

svylorenzpoints_wrap <-
  function( x = x , y = y , pch = 16 , cex = 0.1 , lwd = 1 , col = col , ... ){

    points( x, y , xlim = c( 0 , 1 ) , ylim = c( 0 , 1 ) , pch = pch , cex = cex * 4 , lwd = lwd , col = col , ... )

  }

svylorenzpolygon_wrap <-
  function( x = x , y = y , col = col , border = NA , ... ){
    polygon( x , y , col = col , border = border , ... )
  }


#' @rdname svylorenz
#' @export
svylorenz.survey.design <- function ( formula , design, quantiles = seq(0,1,.1), empirical = FALSE, plot = TRUE, add = FALSE, curve.col = "red", ci = TRUE, alpha = .05, na.rm = FALSE , ... ) {

  # quantile function:
  wtd.qtl <- function (x, q = .5, weights = NULL ) {

    indices <- weights != 0
    x <- x[indices]
    weights <- weights[indices]

    x_1 <- c(0,x[-length(x)])
    N <- sum(weights)
    wsum <- cumsum(weights)
    wsum_1 <- c(0,wsum[-length(wsum)])
    alpha_k <- wsum / N

    k <- which( (wsum_1 < (q * N) ) & ( (q * N) <= wsum) )

    return( x_1[ k ] + ( x[k] - x_1[k] ) * ( (q * N) - wsum_1[k] ) / weights[k] )

  }

  # partial sum (1st definition):
  wtd.psum <- function (x, q = .5, weights = NULL ) {
    indices <- weights != 0
    x <- x[indices]
    weights <- weights[indices]

    x_thres <- wtd.qtl(x = x, q = q, weights = weights )

    return( sum( weights * x * 1 * (x <= x_thres) ) )

  }

  # partial sum (2nd definition)
  wtd.psum <- function (x, q = .5, weights = NULL ) {

    indices <- weights != 0
    x <- x[indices]
    weights <- weights[indices]

    x_1 <- c(0,x[-length(x)])
    N <- sum(weights)
    wsum <- cumsum(weights)
    wsum_1 <- c(0,wsum[-length(wsum)])
    alpha_k <- wsum / N

    k <- which( (wsum_1 < (q * N) ) & ( (q * N) <= wsum) )

    t_k <- ( (q * N) - wsum_1 ) / weights

    H_fn <- function(x) {
      y <- NULL
      y[ x < 0 ] <- 0
      y[ (0 <= x) & (x < 1) ] <- x[ (0 <= x) & (x < 1) ]
      y[ x >= 1 ] <- 1

      return(y)
    }

    return( sum( weights * x * H_fn(t_k) ) )

  }

  incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

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

  incvar <- incvar[w != 0]
  w <- w[w != 0]

  average <- sum( w * incvar ) / sum( w )
  if ( is.na(average) ) {
    variance <- as.matrix(NA)
    cis <- array( rbind(rep(NA, length(quantiles)),rep(NA, length(quantiles))), dim = c(2, length(quantiles)), dimnames = list( c( "(lower", "upper)" ), as.character(quantiles) ) )
    rval <- t( matrix( data = rep(NA, length(quantiles)), nrow = length(quantiles), dimnames = list( as.character( quantiles ), as.character(formula)[2] ) ) )
    rval <- list(quantiles = rval, CIs = cis)
    attr(rval, "SE") <- rep(NA, length(quantiles))
    class(rval) <- c( "cvyquantile" , "svyquantile" )

    return(rval)
  }

  p <- NULL
  L.p <- NULL

  L.p <- as.numeric( lapply( quantiles, function(x) wtd.psum(x = incvar, weights = w, q = x ) ) ) / sum( w * incvar )

  GL.p <- L.p * average

  if (empirical) {
    E_p <- ( 2*cumsum(w[w != 0]) - w[w != 0] ) / ( 2*sum(w[w != 0]) )
    E_L.p <- cumsum(w[w != 0]*incvar[w != 0])/sum(w[w != 0]*incvar[w != 0])
    E_GL.p <- E_L.p[w != 0] * average
  }

  N <- sum( w )
  var <- NULL
  lin_mat <- matrix( NA, nrow = length( ordincvar ), ncol = length(quantiles) )
  for ( pc in quantiles ) {
    i <- match( pc, quantiles )
    pc

    if ( pc > 0 & pc < 1 ) {

      quant <- wtd.qtl( x = incvar, q = pc, weights = w )
      s.quant <- L.p[i]

      u_i <- 1/design$prob
      u_i[ u_i > 0 ] <- ( 1 / ( N * average ) ) * ( ( ( incvar - quant ) * ( incvar <= quant ) ) + ( pc * quant ) - ( incvar * s.quant ) )
      u_i <- u_i[ sort(ordincvar) ]

      lin_mat[ , i ] <- u_i

      rm(quant, s.quant , u_i )

    } else if ( pc == 0 ) {

      L.p[i] <- 0

      lin_mat[ , i ] <- 0

    } else if ( pc == 1 ) {

      L.p[i] <- 1

      lin_mat[ , i] <- 0
    }

    rm( i, pc )

  }
  var <- survey::svyrecvar( lin_mat/design$prob, design$cluster, design$strata, design$fpc, postStrata = design$postStrata )

  se <- sqrt(diag(var))


  CI.L <- L.p - se * qnorm( alpha, mean = 0, sd = 1, lower.tail = FALSE )
  CI.U <- L.p + se * qnorm( alpha, mean = 0, sd = 1, lower.tail = FALSE )

  cis <- structure( rbind( CI.L,CI.U ), .Dim = c(2L, length(quantiles), 1L), .Dimnames = list(c("(lower", "upper)"), as.character(quantiles),  as.character(formula)[2]))

  rval <- t( matrix( data = L.p, nrow = length(quantiles), dimnames = list( as.character( quantiles ), as.character(formula)[2] ) ) )
  rval <- list(quantiles = rval, CIs = cis)
  attr(rval, "var") <- var
  attr(rval, "SE") <- se
  class(rval) <- c( "cvyquantile" , "svyquantile" )

  if ( plot ) {

    plot_dots <- list( ... )

    # remove `deff` argument sent by svyby
    if( 'deff' %in% names( plot_dots ) ) plot_dots$deff <- NULL

    if ( !add ) do.call( svylorenzplot_wrap , plot_dots )

    if( any( c( 'xlim' , 'ylim' , 'col' ) %in% names( list( ... ) ) ) ) stop( "xlim=, ylim=, and col= parameters are fixed within `svylorenz`.  use curve.col= to change the line color" )
    abline( 0 , 1 , ylim = c( 0 , 1 ) , plot_dots )

    if( empirical ) {
      lines_dots <- plot_dots
      lines_dots$x <- E_p
      lines_dots$y <- E_L.p
      lines_dots$col = curve.col
      do.call( svylorenzlines_wrap , lines_dots )
    }

    points_dots <- plot_dots
    points_dots$x <- quantiles
    points_dots$y <- L.p
    points_dots$col <- curve.col

    do.call( svylorenzpoints_wrap , points_dots )

    if (ci) {
      X.Vec <- as.numeric( c(quantiles, tail(quantiles, 1), rev(quantiles), quantiles[1]) )
      Y.Vec <- as.numeric( c( CI.L, tail(CI.U, 1), rev(CI.U), CI.L[1] ) )

      polygon_dots <- plot_dots
      polygon_dots$x <- X.Vec
      polygon_dots$y <- Y.Vec
      polygon_dots$col <- adjustcolor( curve.col, alpha.f = .2)
      polygon_dots$border <- NA

      do.call( svylorenzpolygon_wrap , polygon_dots )

    }

  }

  return(rval)

}


#' @rdname svylorenz
#' @export
svylorenz.svyrep.design <- function(formula , design, quantiles = seq(0,1,.1), empirical = FALSE, plot = TRUE, add = FALSE, curve.col = "red", ci = TRUE, alpha = .05, na.rm = FALSE , ...) {

  # quantile function:
  wtd.qtl <- function (x, q = .5, weights = NULL ) {

    indices <- weights != 0
    x <- x[indices]
    weights <- weights[indices]

    x_1 <- c(0,x[-length(x)])
    N <- sum(weights)
    wsum <- cumsum(weights)
    wsum_1 <- c(0,wsum[-length(wsum)])
    alpha_k <- wsum / N

    k <- which( (wsum_1 < (q * N) ) & ( (q * N) <= wsum) )

    return( x_1[ k ] + ( x[k] - x_1[k] ) * ( (q * N) - wsum_1[k] ) / weights[k] )

  }

  # partial sum (1st definition):
  wtd.psum <- function (x, q = .5, weights = NULL ) {
    indices <- weights != 0
    x <- x[indices]
    weights <- weights[indices]

    x_thres <- wtd.qtl(x = x, q = q, weights = weights )

    return( sum( weights * x * 1 * (x <= x_thres) ) )

  }

  # partial sum (2nd definition)
  wtd.psum <- function (x, q = .5, weights = NULL ) {

    indices <- weights != 0
    x <- x[indices]
    weights <- weights[indices]

    x_1 <- c(0,x[-length(x)])
    N <- sum(weights)
    wsum <- cumsum(weights)
    wsum_1 <- c(0,wsum[-length(wsum)])
    alpha_k <- wsum / N

    k <- which( (wsum_1 < (q * N) ) & ( (q * N) <= wsum) )

    t_k <- ( (q * N) - wsum_1 ) / weights

    H_fn <- function(x) {
      y <- NULL
      y[ x < 0 ] <- 0
      y[ (0 <= x) & (x < 1) ] <- x[ (0 <= x) & (x < 1) ]
      y[ x >= 1 ] <- 1

      return(y)
    }

    return( sum( weights * x * H_fn(t_k) ) )

  }

  # wtd.psum for multiple quantiles:
  lapply_wtd.psum <- function (x, qs = seq(0,1,.2), weights = NULL ) {
    res <- lapply( qs, function (q) { wtd.psum( x = x, weights = weights, q = q ) / sum( weights[weights != 0] * x[weights != 0] ) } )
    as.numeric(res)
  }

  incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

  if(na.rm){
    nas<-is.na(incvar)
    design<-design[!nas,]
  }

  incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]
  ws <- weights(design, "sampling")
  ww <- weights(design, "analysis")

  ordincvar <- order( incvar )

  ws <- ws[ ordincvar ]
  ww <- ww[ ordincvar, ]
  incvar <- incvar[ ordincvar ]

  if ( any( is.na( incvar [ ws > 0 ] ) ) ) {
    variance <- as.matrix(NA)
    cis <- array( rbind(rep(NA, length(quantiles)),rep(NA, length(quantiles))), dim = c(2, length(quantiles)), dimnames = list( c( "(lower", "upper)" ), as.character(quantiles) ) )
    rval <- t( matrix( data = rep(NA, length(quantiles)), nrow = length(quantiles), dimnames = list( as.character( quantiles ), as.character(formula)[2] ) ) )
    rval <- list(quantiles = rval, CIs = cis)
    attr(rval, "SE") <- rep(NA, length(quantiles))
    class(rval) <- c( "cvyquantile" , "svyquantile" )

    return(rval)
  }

  L.p <- t( as.matrix( lapply_wtd.psum( x = incvar, qs = quantiles, weights = ws ) ) )
  rval <- t( matrix( data = L.p, dimnames = list( as.character( quantiles ) ) ) )
  qq <- apply(ww, 2, function(wi) lapply_wtd.psum(x = incvar, qs = quantiles, weights = wi ) )

  if ( any(is.na(qq))) {
    variance <- as.matrix(NA)
    cis <- array( rbind(rep(NA, length(quantiles)),rep(NA, length(quantiles))), dim = c(2, length(quantiles)), dimnames = list( c( "(lower", "upper)" ), as.character(quantiles) ) )
    rval <- t( matrix( data = rep(NA, length(quantiles)), nrow = length(quantiles), dimnames = list( as.character( quantiles ), as.character(formula)[2] ) ) )
    rval <- list(quantiles = rval, CIs = cis)
    attr(rval, "SE") <- rep(NA, length(quantiles))
    class(rval) <- c( "cvyquantile" , "svyquantile" )

    return(rval)
  }

  variance <- survey::svrVar( t(qq), design$scale, design$rscales, mse = design$mse, coef = rval )
  se <- sqrt(diag(variance))
  se[c(1, length(quantiles))] <- 0

  if (empirical) {
    ordincvar <- order(incvar)
    incvar <- incvar[ordincvar]
    ws <- ws[ordincvar]
    E_p <- ( 2*cumsum(ws[ws != 0]) - ws[ws != 0] ) / ( 2*sum(ws[ws != 0]) )
    E_L.p <- cumsum(ws[ws != 0]*incvar[ws != 0])/sum(ws[ws != 0]*incvar[ws != 0])
  }

  CI.L <- as.numeric( L.p - se * qnorm( alpha, mean = 0, sd = 1, lower.tail = FALSE ) )
  CI.U <- as.numeric( L.p + se * qnorm( alpha, mean = 0, sd = 1, lower.tail = FALSE ) )

  cis <- structure(rbind(CI.L,CI.U), .Dim = c(2L, length(quantiles), 1L), .Dimnames = list(c("(lower", "upper)"), as.character(quantiles),  as.character(formula)[2]))
  rval <- t( matrix( data = L.p, nrow = length(quantiles), dimnames = list( as.character( quantiles ), as.character(formula)[2] ) ) )
  rval <- list(quantiles = rval, CIs = cis)
  attr(rval, "var") <- variance
  attr(rval, "SE") <- se
  class(rval) <- c( "cvyquantile" , "svyquantile" )


  if ( plot ) {

    plot_dots <- list( ... )

    # remove `deff` argument sent by svyby
    if( 'deff' %in% names( plot_dots ) ) plot_dots$deff <- NULL

    if ( !add ) do.call( svylorenzplot_wrap , plot_dots )

    if( any( c( 'xlim' , 'ylim' , 'col' ) %in% names( list( ... ) ) ) ) stop( "xlim=, ylim=, and col= parameters are fixed within `svylorenz`.  use curve.col= to change the line color" )
    abline( 0 , 1 , ylim = c( 0 , 1 ) , plot_dots )

    if( empirical ) {
      lines_dots <- plot_dots
      lines_dots$x <- E_p
      lines_dots$y <- E_L.p
      lines_dots$col = curve.col
      do.call( svylorenzlines_wrap , lines_dots )
    }

    points_dots <- plot_dots
    points_dots$x <- quantiles
    points_dots$y <- L.p
    points_dots$col <- curve.col

    do.call( svylorenzpoints_wrap , points_dots )

    if (ci) {
      X.Vec <- as.numeric( c(quantiles, tail(quantiles, 1), rev(quantiles), quantiles[1]) )
      Y.Vec <- as.numeric( c( CI.L, tail(CI.U, 1), rev(CI.U), CI.L[1] ) )

      polygon_dots <- plot_dots
      polygon_dots$x <- X.Vec
      polygon_dots$y <- Y.Vec
      polygon_dots$col <- adjustcolor( curve.col, alpha.f = .2)
      polygon_dots$border <- NA

      do.call( svylorenzpolygon_wrap , polygon_dots )

    }

  }

  return(rval)

}

#' @rdname svylorenz
#' @export
svylorenz.DBIsvydesign <- function (formula, design, ...) {

  design$variables <- getvars(formula, design$db$connection, design$db$tablename, updates = design$updates, subset = design$subset)

  NextMethod("svylorenz", design)

}
