#'
#' Bourguignon (1999) multidimensional inequality indices (EXPERIMENTAL)
#'
#' Estimate indices from the Bourguignon (1999) class, a class of multidimensional inequality measures.
#'
#' @param formula a formula specifying the variables. Variables can be numeric or ordered factors.
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param alpha a scalar defining the exponent of the indicator.
#' @param beta a scalar defining the exponent of the indicator.
#' @param dimw a vector defining the weight of each dimension in the multidimensional deprivation sum.
#' @param na.rm Should cases with missing values be dropped?
#' @param ... future expansion
#'
#' @details you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
#'
#' @return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of the statistic.
#'
#' @author Guilherme Jacob, Djalma Pessoa and Anthony Damico
#'
#' @note This function is experimental and is subject to change in later versions.
#'
#' @seealso \code{\link{svyfgt}}
#'
#' @references Francois Bourguignon (1999). Comment to 'Multidimensioned Approaches to Welfare
#' Analysis' by Maasoumi, E. In: Handbook of income inequality measurement., ed. J.
#' Silber, Boston, Dordrecht and London: Kluwer Academic, p. 477-484.
#'
#' Maria Ana Lugo (2007). Comparing multidimensional indices of inequality: Methods and application.
#' In: Research on Economic Inequality. Emerald, p. 213-236.
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
#' des_eusilc <- update(des_eusilc, pb220a = ordered( pb220a ) )
#'
#' # replicate-weighted design
#' des_eusilc_rep <- as.svrepdesign( des_eusilc , type = "bootstrap", replicates = 50 )
#' des_eusilc_rep <- convey_prep(des_eusilc_rep)
#'
#' # linearized
#' svybmi(~eqincome+hy050n, design = des_eusilc, alpha = .5, beta = .5, na.rm = FALSE )
#' svybmi(~eqincome+hy050n, design = des_eusilc, alpha = .5, beta = 0, na.rm = FALSE )
#' svybmi(~eqincome+hy050n, design = des_eusilc, alpha = .5, beta = -.5, na.rm = FALSE )
#' svybmi(~eqincome+hy050n, design = des_eusilc, alpha = .5, beta = -1, na.rm = FALSE )
#' svybmi(~eqincome+hy050n, design = des_eusilc, alpha = .5, beta = -2, na.rm = FALSE )
#'
#' # replicate
#' svybmi(~eqincome+hy050n, design = des_eusilc_rep, alpha = .5, beta = .5, na.rm = FALSE )
#' svybmi(~eqincome+hy050n, design = des_eusilc_rep, alpha = .5, beta = 0, na.rm = FALSE )
#' svybmi(~eqincome+hy050n, design = des_eusilc_rep, alpha = .5, beta = -.5, na.rm = FALSE )
#' svybmi(~eqincome+hy050n, design = des_eusilc_rep, alpha = .5, beta = -1, na.rm = FALSE )
#' svybmi(~eqincome+hy050n, design = des_eusilc_rep, alpha = .5, beta = -2, na.rm = FALSE )
#'
#' \dontrun{
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
#' # linearized
#' svybmi(~eqincome+hy050n, design = dbd_eusilc, alpha = .5, beta = .5, na.rm = FALSE )
#' svybmi(~eqincome+hy050n, design = dbd_eusilc, alpha = .5, beta = 0, na.rm = FALSE )
#' svybmi(~eqincome+hy050n, design = dbd_eusilc, alpha = .5, beta = -.5, na.rm = FALSE )
#' svybmi(~eqincome+hy050n, design = dbd_eusilc, alpha = .5, beta = -1, na.rm = FALSE )
#' svybmi(~eqincome+hy050n, design = dbd_eusilc, alpha = .5, beta = -2, na.rm = FALSE )
#'
#' # subsetting:
#' sub_dbd_eusilc <- subset( dbd_eusilc, db040 == "Styria")
#' svybmi(~eqincome+hy050n, design = sub_dbd_eusilc, alpha = .5, beta = .5, na.rm = FALSE )
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' dbDisconnect( conn , shutdown = TRUE )
#'
#' }
#'
#' @export
svybmi <- function( formula, design, ...) {

  warning("The svybmi function is experimental and is subject to changes in later versions.")

  UseMethod("svybmi", design)

}

#' @rdname svybmi
#' @export
svybmi.survey.design <- function( formula, design, alpha = .5, beta = -2, dimw = NULL, na.rm = FALSE, ...) {

  if ( alpha <= 0 | alpha > 1 ) stop( "This function is only defined for alpha in (0,1]." )
  if ( beta >= 1 ) stop( "This function is only defined for beta < 1." )

  nac.matrix <- model.frame(formula, design$variables, na.action = na.pass)[,]

  if ( !is.null( dimw ) ) {
    if ( any( is.na( dimw ) ) ) { stop( "Invalid value in dimension weights vector." ) }
    if ( sum( dimw ) > 1 ) { stop( "The sum of dimension weigths have to be equal to one." ) }
    if ( any( dimw > 1 | dimw < 0 ) ) { stop( "Dim. weights have to be within interval [0,1]." ) }
    if ( length(dimw) != ncol(nac.matrix) ) { stop( "Dimension weights' length differs from number of dimensions in formula" ) }
  }

  var.class <- lapply( nac.matrix, function(x) class(x)[1] )
  var.class <- matrix(var.class, nrow = 1, ncol = ncol(nac.matrix),
                      dimnames = list( c("var.class"), colnames( nac.matrix ) ) )

  if ( any( !( var.class %in% c( "numeric", "integer" ) ) ) ) {
    stop( "This function is only applicable to variables of type 'numeric'." )
  }
  if ( any( ( var.class == "integer" ) ) ) {
    stop( "At least one of the variables is an integer.\nCoerce your column to numeric with as.numeric if you are sure it's what you want." )
  }

  w <- 1/design$prob

  if ( any( nac.matrix[ w != 0, var.class == "numeric" ] < 0, na.rm = TRUE ) ) stop( "The Bourguignon multidimensional inequality index is defined for non-negative numeric variables only." )


  if (na.rm) {
    nas <- apply( nac.matrix, 1, function(x) any( is.na(x) ) )
    design <- design[nas == 0, ]
    w <- 1/design$prob
  }

  nac.matrix <- model.frame(formula, design$variables, na.action = na.pass) [ w > 0, ]
  w <- w [ w > 0 ]

  # Normalized Achievement Matrix
  if ( any( ( nac.matrix < 0 | nac.matrix > 1 )[ w > 0 ], na.rm = T ) ) {

    for ( i in seq_along(var.class) ) {

      top <- max( nac.matrix[ , i], na.rm = TRUE )
      bottom <- min( nac.matrix[ , i], na.rm = TRUE )
      if (top != bottom) {
        nac.matrix[ , i ] <- ( nac.matrix[ , i ] - bottom ) / ( top - bottom )
      } else {
        warning( paste0( "component '" , colnames(nac.matrix)[ i ] , "' does not vary across this sample.\n Assuming normalized component = 1." ) )
        nac.matrix[ , i ] <- 1
      }

    }

  }

  # Cross Achievement Aggregation:
  if ( is.null( dimw ) ) {
    dimw = rep( 1 / ncol( nac.matrix ), ncol( nac.matrix ) )
  }
  if ( sum( dimw ) != 1 ) {
    warning( paste( "sum(dimw) != 1. Normalizing dimension weights to" , paste( as.character( dimw / sum( dimw ) ) , collapse = " ; " ) ) )
    dimw = dimw / sum( dimw )
  }

  indiv.welfare <- aggregator( mat = nac.matrix, dimw = dimw, alpha = alpha, beta = beta )

  if ( any( is.na( indiv.welfare ) ) ) {

    rval <- as.numeric(NA)
    variance <- as.numeric(NA)
    class(rval) <- c( "cvystat" , "svystat" )
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "bourguignon"
    attr(rval, "dimensions") <- matrix( strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]], nrow = 1, ncol = ncol(var.class), dimnames = list( "variables", paste("dimension", 1:ncol(var.class) ) ) )
    attr(rval, "parameters") <- matrix( c( alpha, beta ), nrow = 1, ncol = 2, dimnames = list( "parameters", c( "alpha=", "beta=" ) ) )
    return(rval)

  }

  U_x <- list( value = sum( w * indiv.welfare ) , lin = indiv.welfare )
  aggr.pop <- list( value = sum( w ), lin = rep( 1 , length( nac.matrix[ , 1 ] ) ) )

  U_x <- contrastinf( quote( U_x / aggr.pop ) , list( U_x = U_x, aggr.pop = aggr.pop ) )

  aggr.beta <- list( value = beta , lin = rep( 0 , length( nac.matrix[ , 1 ] ) ) )
  aggr.alpha <- list( value = alpha , lin = rep( 0 , length( nac.matrix[ , 1 ] ) ) )

  for ( i in seq_along( var.class ) ) {

    if ( i == 1 ) {

      if ( beta != 0 ) {

        aggr.nac <- list( value = sum( w * nac.matrix[ , i ] ), lin = nac.matrix[ , i ] )
        aggr.dimw <- list( value = dimw[ i ] , lin = rep( 0 , length( nac.matrix[ , i ] ) ) )

        list_all <- list( aggr.nac = aggr.nac , aggr.pop = aggr.pop , aggr.dimw = aggr.dimw , aggr.alpha = aggr.alpha, aggr.beta = aggr.beta )
        aggr.mu <- contrastinf( quote( aggr.dimw * ( aggr.nac / aggr.pop )^aggr.beta ), list_all )

      } else {

        aggr.nac <- list( value = sum( nac.matrix[ , i ] * w ), lin = nac.matrix[ , i ] )
        aggr.dimw <- list( value = dimw[ i ] , lin = rep( 0 , length( nac.matrix[ , i ] ) ) )

        list_all <- list( aggr.nac = aggr.nac , aggr.pop = aggr.pop , aggr.dimw = aggr.dimw , aggr.alpha = aggr.alpha, aggr.beta = aggr.beta )
        aggr.mu <- contrastinf( quote( ( aggr.nac / aggr.pop ) ^ aggr.dimw  ), list_all )

      }

    } else {

      if ( beta != 0 ) {

        aggr.nac <- list( value = sum( w * nac.matrix[ , i ] ), lin = nac.matrix[ , i ] )
        aggr.dimw <- list( value = dimw[ i ] , lin = rep( 0 , length( nac.matrix[ , i ] ) ) )

        list_all <- list( aggr.nac = aggr.nac , aggr.pop = aggr.pop , aggr.dimw = aggr.dimw , aggr.alpha = aggr.alpha, aggr.beta = aggr.beta )
        aggr.mu_iter <- contrastinf( quote( aggr.dimw * ( aggr.nac / aggr.pop )^aggr.beta ), list_all )

        aggr.mu <- contrastinf( quote( aggr.mu + aggr.mu_iter ), list( aggr.mu = aggr.mu, aggr.mu_iter = aggr.mu_iter ) )

      } else {

        aggr.nac <- list( value = sum( nac.matrix[ , i ] * w ), lin = nac.matrix[ , i ] )
        aggr.dimw <- list( value = dimw[ i ] , lin = rep( 0 , length( nac.matrix[ , i ] ) ) )

        list_all <- list( aggr.nac = aggr.nac , aggr.pop = aggr.pop , aggr.dimw = aggr.dimw , aggr.alpha = aggr.alpha, aggr.beta = aggr.beta )
        aggr.mu_iter <- contrastinf( quote( ( aggr.nac / aggr.pop ) ^ aggr.dimw  ), list_all )

        aggr.mu <- contrastinf( quote( aggr.mu * aggr.mu_iter ), list( aggr.mu = aggr.mu, aggr.mu_iter = aggr.mu_iter ) )

      }

    }

  }

  if ( beta != 0 ) {
    aggr.mu <- contrastinf( quote( ( aggr.mu )^( aggr.alpha / aggr.beta ) ), list( aggr.mu = aggr.mu, aggr.alpha = aggr.alpha, aggr.beta = aggr.beta ) )
  } else {
    aggr.mu <- contrastinf( quote( ( aggr.mu )^( aggr.alpha ) ), list( aggr.mu = aggr.mu, aggr.alpha = aggr.alpha, aggr.beta = aggr.beta ) )
  }

  estimate <- contrastinf( quote( 1 - ( U_x / aggr.mu ) ), list( U_x = U_x, aggr.mu = aggr.mu ) )
  if ( length(estimate$lin) < length(design$prob) ) {
    lin <- 1 *( (1/design$prob) > 0 )
    lin[ lin == 1 ] <- estimate$lin
    estimate$lin <- lin
    rm(lin)
  }

  variance <- survey::svyrecvar( estimate$lin / design$prob, design$cluster,design$strata, design$fpc, postStrata = design$postStrata )

  rval <- estimate$value
  names( rval )[1] <- attr(rval, "statistic") <- "bourguignon"
  attr(rval, "var") <- variance
  attr(rval, "dimensions") <- matrix( strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]], nrow = 1, ncol = ncol(var.class), dimnames = list( "variables", paste("dimension", 1:ncol(var.class) ) ) )
  attr(rval, "parameters") <- matrix( c( alpha, beta ), nrow = 1, ncol = 2, dimnames = list( "parameters", c( "alpha=", "beta=" ) ) )
  class(rval) <- c( "cvystat" , "svystat" )


  rval

}

#' @rdname svybmi
#' @export
svybmi.svyrep.design <- function( formula, design, alpha = .5, beta = -2, dimw = NULL, na.rm = FALSE , ...) {

  if ( alpha <= 0 | alpha > 1 ) stop( "This function is only defined for alpha in (0,1]." )
  if ( beta >= 1 ) stop( "This function is only defined for beta < 1." )

  nac.matrix <- model.frame(formula, design$variables, na.action = na.pass)[,]

  if ( !is.null( dimw ) ) {
    if ( any( is.na( dimw ) ) ) { stop( "Invalid value in dimension weights vector." ) }
    if ( sum( dimw ) > 1 ) { stop( "The sum of dimension weigths have to be equal to one." ) }
    if ( any( dimw > 1 | dimw < 0 ) ) { stop( "Dim. weights have to be within interval [0,1]." ) }
    if ( length(dimw) != ncol(nac.matrix) ) { stop( "Dimension weights' length differs from number of dimensions in formula" ) }
  }

  var.class <- lapply( nac.matrix, function(x) class(x)[1] )
  var.class <- matrix(var.class, nrow = 1, ncol = ncol(nac.matrix), dimnames = list( c("var.class"), colnames( nac.matrix ) ) )

  if ( any( !( var.class %in% c( "numeric", "integer" ) ) ) ) {
    stop( "This function is only applicable to variables of type 'numeric'." )
  }
  if ( any( ( var.class == "integer" ) ) ) {
    stop( "At least one of the variables is an integer.\nCoerce your column to numeric with as.numeric if you are sure it's what you want." )
  }

  w <- weights( design, "sampling" )

  if ( any( nac.matrix[ w != 0, var.class == "numeric" ] < 0, na.rm = TRUE ) ) stop( "The Bourguignon multidimensional inequality index is defined for non-negative numeric variables only." )


  if (na.rm) {
    nas <- apply( nac.matrix, 1, function(x) any( is.na(x) ) )
    design <- design[nas == 0, ]
    w <- weights( design, "sampling" )
  }

  nac.matrix <- model.frame(formula, design$variables, na.action = na.pass)[ , ]

  # Normalized Achievement Matrix
  if ( any( ( nac.matrix < 0 | nac.matrix > 1 )[ w > 0 ], na.rm = T ) ) {

    for ( i in seq_along(var.class) ) {

      top <- max( nac.matrix[ w > 0 , i ], na.rm = TRUE )
      bottom <- min( nac.matrix[ w > 0 , i ], na.rm = TRUE )

      if (top != bottom) {
        nac.matrix[ , i ] <- ( nac.matrix[ , i ] - bottom ) / ( top - bottom )
      } else {
        warning( paste0( "component '" , colnames(nac.matrix)[ i ] , "' does not vary across this sample.\n Assuming normalized component = 1." ) )
        nac.matrix[ , i ] <- 1
      }

    }

  }

  # Cross Achievement Aggregation:
  if ( is.null( dimw ) ) {
    dimw = rep( 1 / ncol( nac.matrix ), ncol( nac.matrix ) )
  }
  if ( sum( dimw ) != 1 ) {
    warning( paste( "sum(dimw) != 1. Normalizing dimension weights to" , paste( as.character( dimw / sum( dimw ) ) , collapse = " ; " ) ) )
    dimw = dimw / sum( dimw )
  }
  indiv.welfare <- aggregator( mat = nac.matrix, dimw = dimw, alpha = alpha, beta = beta )


  if ( any( is.na( indiv.welfare )[ w > 0 ] ) ) {

    rval <- as.numeric(NA)
    variance <- as.numeric(NA)
    class(rval) <- c( "cvystat" , "svrepstat" )
    attr(rval, "var") <- variance
    names( rval )[1] <- attr(rval, "statistic") <- "bourguignon"
    attr(rval, "dimensions") <- matrix( strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]], nrow = 1, ncol = ncol(var.class), dimnames = list( "variables", paste("dimension", 1:ncol(var.class) ) ) )
    attr(rval, "parameters") <- matrix( c( alpha, beta ), nrow = 1, ncol = 2, dimnames = list( "parameters", c( "alpha=", "beta=" ) ) )
    return(rval)

  }


  if ( beta != 0 ) {
    rval <- 1 - ( sum( w [ w > 0 ] * indiv.welfare [ w > 0 ] ) / sum( w [ w > 0 ] ) ) /
      sum( ( dimw * ( colSums( w [ w > 0 ] * nac.matrix [ w > 0, ] ) / sum( w [ w > 0 ] ) )^( beta ) ) )^( alpha / beta )

    ww <- weights( design, "analysis" )

    qq <- apply( ww, 2, function(wi) {
      1 - ( sum( wi [ wi > 0 ] * indiv.welfare [ wi > 0 ] ) / sum( wi [ wi > 0 ] ) ) /
        sum( ( dimw * ( colSums( wi [ wi > 0 ] * nac.matrix [ wi > 0, ] ) / sum( wi [ wi > 0 ] ) )^( beta ) )^( alpha / beta ) )
    } )

  } else {

    rval <- 1 - ( sum( w [ w > 0 ] * indiv.welfare [ w > 0 ] ) / sum( w [ w > 0 ] ) ) /
      prod( ( colSums( w [ w > 0 ] * nac.matrix [ w > 0, ] ) / sum( w [ w > 0 ] ) )^( dimw ) )^( alpha )

    ww <- weights( design, "analysis" )

    qq <- apply( ww, 2, function(wi) {
      1 - ( sum( wi [ wi > 0 ] * indiv.welfare [ wi > 0 ] ) / sum( wi [ wi > 0 ] ) ) /
        prod( ( colSums( wi [ wi > 0 ] * nac.matrix [ wi > 0, ] ) / sum( wi [ wi > 0 ] ) )^( dimw ) )^( alpha )
    } )

  }

  variance <- survey::svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)
  variance <- as.matrix( variance )

  class(rval) <- c( "cvystat" , "svrepstat" )
  attr(rval, "var") <- variance
  names( rval )[1] <- attr(rval, "statistic") <- "bourguignon"
  attr(rval, "dimensions") <- matrix( strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]], nrow = 1, ncol = ncol(var.class), dimnames = list( "variables", paste("dimension", 1:ncol(var.class) ) ) )
  attr(rval, "parameters") <- matrix( c( alpha, beta ), nrow = 1, ncol = 2, dimnames = list( "parameters", c( "alpha=", "beta=" ) ) )

  rval

}

#' @rdname svybmi
#' @export
svybmi.DBIsvydesign <-
  function (formula, design, ...) {

    design$variables <- getvars(formula, design$db$connection, design$db$tablename, updates = design$updates, subset = design$subset)

    NextMethod("svybmi", design)
  }


aggregator <- function( mat, dimw, alpha, beta ) {


  if ( beta != 0 ) {

    return( ( rowSums( sweep( mat^beta, MARGIN=2 , dimw,`*`) )^( alpha / beta ) ) )

  } else {

    return( apply( sweep( mat, MARGIN=2 , dimw,`^`), 1, prod )^alpha ) }

}
