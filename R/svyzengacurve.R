#' Zenga inequality curve (EXPERIMENTAL)
#'
#' Estimate the Zenga curve, an inequality graph
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param quantiles a sequence of probabilities that defines the quantiles sum to be calculated
#' @param ci Should the confidence interval be plotted? Defaults to \code{TRUE}.
#' @param alpha a number that especifies de confidence level for the graph.
#' @param empirical Should an empirical Zenga curve be estimated as well? Defaults to \code{FALSE}.
#' @param plot Should the Zenga curve be plotted? Defaults to \code{TRUE}.
#' @param add Should a new curve be plotted on the current graph?
#' @param curve.col a string defining the color of the curve.
#' @param na.rm Should cases with missing values be dropped? Defaults to \code{FALSE}.
#' @param ... additional arguments passed to \code{plot} methods
#'
#' @details WARNING: this is an experimental version. Use with caution.
#'
#' You must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
#'
#' Notice that the 'empirical' curve is observation-based and is based on the Lorenz curve formula actually used to calculate the Gini index.
#' On the other hand, the quantile-based curve is used to estimate the complement of the ratio between the means, SEs and confidence intervals.
#'
#' This way, as the number of quantiles of the quantile-based function increases, the quantile-based curve approaches the observation-based curve.
#'
#' @return Object of class "\code{svyquantile}", which are vectors with a "\code{quantiles}" attribute giving the complement of the ratio between the lower and upper means,
#' and a "\code{SE}" attribute giving the standard errors of the estimates.
#'
#' @author Guilherme Jacob, Djalma Pessoa and Anthony Damico
#'
#' @note This function is experimental and is subject to changes in later versions.
#'
#' @seealso \code{\link{svyquantile}}
#'
#' @references Marcella Polisicchio and Francesco Porro (2011). A Comparison Between Lorenz L(P) Curve
#' and Zenga I(P) Curve. Statistica Applicata, v. 21, n. 3-4, 289-301.
#'
#' Matti Langel (2012). \emph{Measuring inequality in finite population sampling}. PhD thesis. URL \url{http://doc.rero.ch/record/29204}.
#'
#' Jean-Claude Deville (1999). Variance estimation for complex statistics and estimators:
#' linearization and residual techniques. Survey Methodology, 25, 193-203,
#' URL \url{http://www5.statcan.gc.ca/bsolc/olc-cel/olc-cel?lang=eng&catno=12-001-X19990024882}.
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
#' svyzengacurve( ~eqincome , des_eusilc,  alpha = .01 )
#'
#' # replicate-weighted design
#' des_eusilc_rep <- as.svrepdesign( des_eusilc , type = "bootstrap" )
#' des_eusilc_rep <- convey_prep( des_eusilc_rep )
#'
#' svyzengacurve( ~eqincome , des_eusilc_rep,  alpha = .01 )
#'
#' \dontrun{
#'
#' # linearized design using a variable with missings
#' svyzengacurve( ~py010n , des_eusilc, alpha = .01 )
#' svyzengacurve( ~py010n , des_eusilc, alpha = .01, na.rm = TRUE )
#' # demonstration of `curve.col=` and `add=` parameters
#' svyzengacurve( ~eqincome , des_eusilc,  alpha = .05 , add = TRUE , curve.col = 'green' )
#' # replicate-weighted design using a variable with missings
#' svyzengacurve( ~py010n , des_eusilc_rep, alpha = .01 )
#' svyzengacurve( ~py010n , des_eusilc_rep, alpha = .01, na.rm = TRUE )
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
#' svyzengacurve( ~eqincome , dbd_eusilc, alpha = .01 )
#'
#' # highlighting the difference between the quantile-based curve and the empirical version:
#' svyzengacurve( ~eqincome , dbd_eusilc, seq(0,1,.1), empirical = TRUE, curve.col = "green" )
#' svyzengacurve( ~eqincome , dbd_eusilc, seq(0,1,.1), alpha = .01, add = TRUE )
#' legend( "bottomleft", c("Quantile-based", "Empirical"), lwd = c(1,1), col = c("red", "green"))
#'
#' # as the number of quantiles increases, the difference between the curves gets smaller
#' svyzengacurve( ~eqincome , dbd_eusilc, seq(0,1,.01), empirical = TRUE, curve.col = "green" )
#' svyzengacurve( ~eqincome , dbd_eusilc, seq(0,1,.01), alpha = .01, add = TRUE )
#' legend( "bottomleft", c("Quantile-based", "Empirical"), lwd = c(1,1), col = c("red", "green"))
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
svyzengacurve <- function(formula, design, ...) {

  if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

  warning("The svyzengacurve function is experimental and is subject to changes in later versions.")

  UseMethod("svyzengacurve", design)

}


# thanks to alex for these wrapper functions
# http://stackoverflow.com/a/37518103/1759499
svyzengacurveplot_wrap <-
  function( cex = 0.1 , xlab = "Cumulative Population Share" , ylab = "Complement of the Lower-Upper Means Ratio" , ... ){
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

svyzengacurvelines_wrap <-
  function( x = x , y = y , pch = 16 , cex = 0.1 , lwd = 1 , col = col , ... ){
    lines( x , y , xlim = c( 0 , 1 ) , ylim = c( 0 , 1 ) , pch = pch , cex = cex , lwd = lwd , col = col , ... )
  }

svyzengacurvepoints_wrap <-
  function( x = x , y = y , pch = 16 , cex = 0.1 , lwd = 1 , col = col , ... ){

    points( x, y , xlim = c( 0 , 1 ) , ylim = c( 0 , 1 ) , pch = pch , cex = cex * 4 , lwd = lwd , col = col , ... )

  }

svyzengacurvepolygon_wrap <-
  function( x = x , y = y , col = col , border = NA , ... ){
    polygon( x , y , col = col , border = border , ... )
  }


#' @rdname svyzengacurve
#' @export
svyzengacurve.survey.design <- function ( formula , design, quantiles = seq(0,1,.1), empirical = FALSE, plot = TRUE, add = FALSE, curve.col = "red", ci = TRUE, alpha = .05, na.rm = FALSE , ... ) {

  quantiles <- quantiles[ !(quantiles %in% 0:1) ]

  # quantile function:
  wtd.qtl <- function (x, q = .5, weights = NULL ) {

    indices <- weights != 0
    x <- x[indices]
    weights <- weights[indices]

    ordx <- order(x)
    x <- x[ordx]
    weights <- weights[ordx]

    N <- sum(weights)
    wsum <- cumsum(weights)
    wsum_1 <- c(0,wsum[-length(wsum)])

    k <- which( ( wsum_1 < (q * N) ) & ( (q * N) <= wsum) )

    return( x[k] )

  }

  # partial sum (2nd definition)
  H_fn <- function(x) {
    y <- NULL
    y[ x < 0 ] <- 0
    y[ (0 <= x) & (x < 1) ] <- x[ (0 <= x) & (x < 1) ]
    y[ x >= 1 ] <- 1

    return(y)
  }

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

    return( sum( weights * x * H_fn(t_k) ) )

  }

  z.p_calc <- function( x, q = .5, weights ) {
    X <- sum( x * weights )
    Xp_tilde <- wtd.psum( x = x, q = q, weights = weights )

    return( ( q * X - Xp_tilde ) / ( q * ( X - Xp_tilde ) )  )

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

  if ( any( incvar[w != 0] < 0, na.rm = TRUE ) ) stop( "The Zenga index is defined for non-negative numeric variables only." )

  domain <- w != 0

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

  p <- quantiles
  p.quant <- as.numeric( lapply( p, function(i) { wtd.qtl(x = incvar, q = i, weights = w) } ) )
  Y <- sum( w * incvar )
  Yp_tilde <- as.numeric( lapply( p, function(i) { wtd.psum( x = incvar, q = i, weights = w ) } ) )

  Z.p <- (p*Y - Yp_tilde) / (p*Y - p*Yp_tilde)

  if (empirical) {
    E_p <- ( 2*cumsum(w[w != 0]) - w[w != 0] ) / ( 2*sum(w[w != 0]) )
    E_L.p <- cumsum(w[w != 0]*incvar[w != 0])/sum(w[w != 0]*incvar[w != 0])
    E_Z.p <- (E_p - E_L.p) / (E_p - E_p*E_L.p)
    E_Z.p <- E_Z.p[ E_p >= .05 & E_p <= .95 ]
    E_p <- E_p[ E_p >= .05 & E_p <= .95 ]
  }

  var <- NULL
  for ( i in seq_along(p) ) {

    lin_wtd.psum <- incvar * H_fn( ( p[i] * sum(w) - cumsum( w ) - w ) / w ) + ( p[i] - 1*( incvar < p.quant[i] ) ) * p.quant[i]
    epsilon_p <- p[i]*(Y - Yp_tilde[i])
    lambda_p <- p[i]*Y - Yp_tilde[i]


    v_k <- 1*domain
    v_k <- v_k[ ordincvar ]
    v_k[ v_k != 0 ] <- ( epsilon_p * ( p[i] * incvar - lin_wtd.psum ) - lambda_p * p[i] * ( incvar -lin_wtd.psum ) ) / epsilon_p^2
    v_k <- v_k[ order(ordincvar) ]

    var[i] <- survey::svyrecvar( v_k/design$prob, design$cluster, design$strata, design$fpc, postStrata = design$postStrata )
    rm(v_k, lin_wtd.psum)

  }

  se <- sqrt(var)

  CI.L <- Z.p - se * qnorm( alpha, mean = 0, sd = 1, lower.tail = FALSE )
  CI.U <- Z.p + se * qnorm( alpha, mean = 0, sd = 1, lower.tail = FALSE )

  cis <- structure( rbind( CI.L,CI.U ), .Dim = c(2L, length(quantiles), 1L), .Dimnames = list(c("(lower", "upper)"), as.character(quantiles),  as.character(formula)[2]))

  rval <- t( matrix( data = Z.p, nrow = length(quantiles), dimnames = list( as.character( quantiles ), as.character(formula)[2] ) ) )
  rval <- list(quantiles = rval, CIs = cis)
  attr(rval, "SE") <- se
  class(rval) <- c( "cvyquantile" , "svyquantile" )

  if ( plot ) {

    plot_dots <- list( ... )

    # remove `deff` argument sent by svyby
    if( 'deff' %in% names( plot_dots ) ) plot_dots$deff <- NULL

    if ( !add ) do.call( svyzengacurveplot_wrap , plot_dots )

    if( any( c( 'xlim' , 'ylim' , 'col' ) %in% names( list( ... ) ) ) ) stop( "xlim=, ylim=, and col= parameters are fixed within `svyzengacurve`.  use curve.col= to change the line color" )
    #abline( 0 , 1 , ylim = c( 0 , 1 ) , plot_dots )

    if( empirical ) {
      lines_dots <- plot_dots
      lines_dots$x <- E_p
      lines_dots$y <- E_Z.p
      lines_dots$col = curve.col
      do.call( svyzengacurvelines_wrap , lines_dots )
    }

    points_dots <- plot_dots
    points_dots$x <- quantiles
    points_dots$y <- Z.p
    points_dots$col <- curve.col

    do.call( svyzengacurvepoints_wrap , points_dots )

    if (ci) {
      X.Vec <- as.numeric( c(quantiles, tail(quantiles, 1), rev(quantiles), quantiles[1]) )
      Y.Vec <- as.numeric( c( CI.L, tail(CI.U, 1), rev(CI.U), CI.L[1] ) )

      polygon_dots <- plot_dots
      polygon_dots$x <- X.Vec
      polygon_dots$y <- Y.Vec
      polygon_dots$col <- adjustcolor( curve.col, alpha.f = .2)
      polygon_dots$border <- NA

      do.call( svyzengacurvepolygon_wrap , polygon_dots )

    }

  }

  return(rval)

}


#' @rdname svyzengacurve
#' @export
svyzengacurve.svyrep.design <- function(formula , design, quantiles = seq(0,1,.1), empirical = FALSE, plot = TRUE, add = FALSE, curve.col = "red", ci = TRUE, alpha = .05, na.rm = FALSE , ...) {

  quantiles <- quantiles[ !(quantiles %in% 0:1) ]

  # quantile function:
  wtd.qtl <- function (x, q = .5, weights = NULL ) {

    indices <- weights != 0
    x <- x[indices]
    weights <- weights[indices]

    ordx <- order(x)
    x <- x[ordx]
    weights <- weights[ordx]

    N <- sum(weights)
    wsum <- cumsum(weights)

    k <- which( ( (wsum - weights) < (q * N) ) & ( (q * N) <= wsum) )

    return( x[k] )

  }

  # partial sum (2nd definition)
  H_fn <- function(x) {
    y <- NULL
    y[ x < 0 ] <- 0
    y[ (0 <= x) & (x < 1) ] <- x[ (0 <= x) & (x < 1) ]
    y[ x >= 1 ] <- 1

    return(y)
  }

  wtd.psum <- function (x, q = .5, weights = NULL ) {

    x_1 <- c(0,x[-length(x)])
    N <- sum(weights)
    wsum <- cumsum(weights)
    alpha_k <- wsum / N

    k <- which( ( (wsum - weights) < (q * N) ) & ( ( q * N ) <= wsum ) )

    t_k <- ( (q * N) - (wsum - weights) ) / weights

    return( sum( weights * x * H_fn(t_k) ) )

  }

  z.p_calc <- function( x, q = .5, weights ) {

    x <- x[weights != 0 ]
    weights <- weights[weights != 0 ]

    ordx <- order(x)
    x <- x[ordx]
    weights <- weights[ordx]

    X <- sum( x * weights )
    Xp_tilde <- wtd.psum( x = x, q = q, weights = weights )

    return( ( q * X - Xp_tilde ) / ( q * ( X - Xp_tilde ) )  )

  }

  # wtd.psum for multiple quantiles:
  lapply_z.p_calc <- function (x, qs = seq(0,1,.2), weights = NULL ) {
    res <- lapply( qs, function (i) { z.p_calc( x = x, weights = weights, q = i ) } )
    as.numeric(res)
  }

  incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

  if(na.rm){
    nas<-is.na(incvar)
    design<-design[!nas,]
    #df <- model.frame(design)
    incvar <- incvar[!nas]
  }

  if ( any(incvar < 0, na.rm = TRUE) ) stop( "The Zenga index is defined for non-negative numeric variables only." )


  ws <- weights(design, "sampling")
  Z.p <- t( as.matrix( lapply_z.p_calc(x = incvar, q = quantiles, weights = ws ) ) )
  rval <- t( matrix( data = Z.p, dimnames = list( as.character( quantiles ) ) ) )
  ww <- weights(design, "analysis")
  qq <- apply(ww, 2, function(wi) lapply_z.p_calc(x = incvar, qs = quantiles, weights = wi ) )

  variance <- apply( qq, 1, function(x) survey::svrVar(x, design$scale, design$rscales, mse = design$mse, coef = rval) )
  se <- sqrt(variance)

  if (empirical) {
    ordincvar <- order(incvar)
    incvar <- incvar[ordincvar]
    ws <- ws[ordincvar]
    E_p <- ( 2*cumsum(ws[ws != 0]) - ws[ws != 0] ) / ( 2*sum(ws[ws != 0]) )
    E_L.p <- cumsum(ws[ws != 0]*incvar[ws != 0])/sum(ws[ws != 0]*incvar[ws != 0])
    E_Z.p <- (E_p - E_L.p) / (E_p - E_p*E_L.p)
    E_Z.p <- E_Z.p[ E_p >= .05 & E_p <= .95 ]
    E_p <- E_p[ E_p >= .05 & E_p <= .95 ]
  }

  CI.L <- as.numeric( Z.p - se * qnorm( alpha, mean = 0, sd = 1, lower.tail = FALSE ) )
  CI.U <- as.numeric( Z.p + se * qnorm( alpha, mean = 0, sd = 1, lower.tail = FALSE ) )

  cis <- structure(rbind(CI.L,CI.U), .Dim = c(2L, length(quantiles), 1L), .Dimnames = list(c("(lower", "upper)"), as.character(quantiles),  as.character(formula)[2]))
  rval <- t( matrix( data = Z.p, nrow = length(quantiles), dimnames = list( as.character( quantiles ), as.character(formula)[2] ) ) )
  rval <- list(quantiles = rval, CIs = cis)
  attr(rval, "SE") <- se
  class(rval) <- c( "cvyquantile" , "svyquantile" )


  if ( plot ) {

    plot_dots <- list( ... )

    # remove `deff` argument sent by svyby
    if( 'deff' %in% names( plot_dots ) ) plot_dots$deff <- NULL

    if ( !add ) do.call( svyzengacurveplot_wrap , plot_dots )

    if( any( c( 'xlim' , 'ylim' , 'col' ) %in% names( list( ... ) ) ) ) stop( "xlim=, ylim=, and col= parameters are fixed within `svyzengacurve`.  use curve.col= to change the line color" )
    #abline( 0 , 1 , ylim = c( 0 , 1 ) , plot_dots )

    if( empirical ) {
      lines_dots <- plot_dots
      lines_dots$x <- E_p
      lines_dots$y <- E_Z.p
      lines_dots$col = curve.col
      do.call( svyzengacurvelines_wrap , lines_dots )
    }

    points_dots <- plot_dots
    points_dots$x <- quantiles
    points_dots$y <- Z.p
    points_dots$col <- curve.col

    do.call( svyzengacurvepoints_wrap , points_dots )

    if (ci) {
      X.Vec <- as.numeric( c(quantiles, tail(quantiles, 1), rev(quantiles), quantiles[1]) )
      Y.Vec <- as.numeric( c( CI.L, tail(CI.U, 1), rev(CI.U), CI.L[1] ) )

      polygon_dots <- plot_dots
      polygon_dots$x <- X.Vec
      polygon_dots$y <- Y.Vec
      polygon_dots$col <- adjustcolor( curve.col, alpha.f = .2)
      polygon_dots$border <- NA

      do.call( svyzengacurvepolygon_wrap , polygon_dots )

    }

  }

  return(rval)

}

#' @rdname svyzengacurve
#' @export
svyzengacurve.DBIsvydesign <- function (formula, design, ...) {

  design$variables <- getvars(formula, design$db$connection, design$db$tablename, updates = design$updates, subset = design$subset)

  NextMethod("svyzengacurve", design)

}
