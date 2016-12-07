#' Bourguignon (1999) multimensional inequality indices
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
#' @seealso \code{\link{svyfgt}}
#'
#' @references François Bourguignon (1999). Comment to 'Multidimensioned Approaches to Welfare
#' Analysis' by Maasoumi, E. In: Handbook of income inequality measurement., ed. J.
#' Silber, Boston, Dordrecht and London: Kluwer Academic, p. 477-484.
#'
#' María Ana Lugo (2007). Comparing multidimensional indices of inequality: Methods and application.
#' In: Research on Economic Inequality. Emerald, p. 213–236.
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
svybmi <- function( formula, design, alpha = .5, beta = -2, cutoffs = NULL, dimw = NULL, na.rm = FALSE, ...) {

  if ( alpha <= 0 | alpha >= 1 ) { stop( "This function is only defined for alpha in (0,1]." ) }
  if ( alpha <= 0 | alpha >= 1 ) { stop( "This function is only defined for beta < 1." ) }

  UseMethod("svybmi", design)

}

#' @rdname svybmi
#' @export
svybmi.survey.design <- function( formula, design, alpha = .5, beta = -2, cutoffs = NULL, dimw = NULL, na.rm = FALSE, ...) {

  if (is.null(attr(design, "full_design"))) stop("you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svydesign() function.")

  nac.matrix <- model.frame(formula, design$variables, na.action = na.pass)[,]

  var.class <- lapply( nac.matrix, function(x) class(x)[1] )
  var.class <- matrix(var.class, nrow = 1, ncol = ncol(nac.matrix),
                      dimnames = list( c("var.class"), colnames( nac.matrix ) ) )

  if ( any( !( var.class %in% c( "numeric" ) ) ) ) {
    stop( "This function is only applicable to variables of type 'numeric'." )
  }

  w <- 1/design$prob

  if ( any( nac.matrix[ w != 0, var.class == "numeric" ] < 0, na.rm = TRUE ) ) {
    warning( "The function is defined for non-negative numeric variables only.")
    nps <- apply( nac.matrix[ , var.class == "numeric" ] < 0, 1, function( x ) any( x, na.rm = FALSE ) )
    nps[ is.na(nps) ] <- FALSE
    design <- design[nps == 0 ]
    w <- 1/design$prob
  }

  if (na.rm) {
    nas <- apply( nac.matrix, 1, function(x) any( is.na(x) ) )
    design <- design[nas == 0, ]
    w <- 1/design$prob
  }

  nac.matrix <- model.frame(formula, design$variables, na.action = na.pass)[,]

  # Normalized Achievement Matrix
  if ( any( ( nac.matrix < 0 | nac.matrix > 1 )[ w > 0 ], na.rm = T ) ) {

    for ( i in seq_along(var.class) ) {
      top <- max( nac.matrix[ , i], na.rm = TRUE )
      bottom <- min( nac.matrix[ , i], na.rm = TRUE )
      nac.matrix[ , i ] <- ( nac.matrix[ , i ] - bottom ) / ( top - bottom )
    }

  }

  # Cross Achievement Aggregation:
  if ( is.null( dimw ) ) {
    dimw = rep( 1 / ncol( nac.matrix ), ncol( nac.matrix ) )
  }
  if ( sum( dimw ) != 1 ) {
    warning( "sum(dimw) != 1. Normalizing dimension weights.")
    dimw = dimw / sum( dimw )
  }
  indiv.welfare <- apply( nac.matrix, 1, function(x) aggregator( mat = x, dimw = dimw, alpha = alpha, beta = beta ) )

  if ( any( is.na( indiv.welfare )[ w > 0 ] ) ) {

    rval <- as.numeric(NA)
    variance <- as.numeric(NA)
    class(rval) <- c( "cvystat" , "svystat" )
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "bourguignon"
    attr(rval, "dimensions") <- matrix( strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]], nrow = 1, ncol = ncol(var.class), dimnames = list( "variables", paste("dimension", 1:ncol(var.class) ) ) )
    attr(rval, "parameters") <- matrix( c( alpha, beta ), nrow = 1, ncol = 2, dimnames = list( "parameters", c( "alpha=", "beta=" ) ) )
    return(rval)

  }

  U_x <- list( value = sum( w[ w > 0 ] * indiv.welfare[ w > 0 ] ) , lin = indiv.welfare )
  aggr.pop <- list( value = sum( w[ w > 0 ] ), lin = rep( 1 , length( nac.matrix[ , 1 ] ) ) )

  U_x <- contrastinf( quote( U_x / aggr.pop ) , list( U_x = U_x, aggr.pop = aggr.pop ) )

  aggr.beta <- list( value = beta , lin = rep( 0 , length( nac.matrix[ , 1 ] ) ) )
  aggr.alpha <- list( value = alpha , lin = rep( 0 , length( nac.matrix[ , 1 ] ) ) )

  for ( i in seq_along( var.class ) ) {

    if ( i == 1 ) {

      if ( beta != 0 ) {

        aggr.nac <- list( value = sum( w[ w > 0 ] * nac.matrix[ w > 0 , i ] ), lin = nac.matrix[ , i ] )
        aggr.dimw <- list( value = dimw[ i ] , lin = rep( 0 , length( nac.matrix[ , i ] ) ) )

        list_all <- list( aggr.nac = aggr.nac , aggr.pop = aggr.pop , aggr.dimw = aggr.dimw , aggr.alpha = aggr.alpha, aggr.beta = aggr.beta )
        aggr.mu <- contrastinf( quote( aggr.dimw * ( aggr.nac / aggr.pop )^aggr.beta ), list_all )

      } else {

        aggr.nac <- list( value = sum( nac.matrix[ w > 0 , i ] * w[ w > 0 ] ), lin = nac.matrix[ , i ] )
        aggr.dimw <- list( value = dimw[ i ] , lin = rep( 0 , length( nac.matrix[ , i ] ) ) )

        list_all <- list( aggr.nac = aggr.nac , aggr.pop = aggr.pop , aggr.dimw = aggr.dimw , aggr.alpha = aggr.alpha, aggr.beta = aggr.beta )
        aggr.mu <- contrastinf( quote( ( aggr.nac / aggr.pop ) ^ aggr.dimw  ), list_all )

      }

    } else {

      if ( beta != 0 ) {

        aggr.nac <- list( value = sum( w[ w > 0 ] * nac.matrix[ w > 0 , i ] ), lin = nac.matrix[ , i ] )
        aggr.dimw <- list( value = dimw[ i ] , lin = rep( 0 , length( nac.matrix[ , i ] ) ) )

        list_all <- list( aggr.nac = aggr.nac , aggr.pop = aggr.pop , aggr.dimw = aggr.dimw , aggr.alpha = aggr.alpha, aggr.beta = aggr.beta )
        aggr.mu_iter <- contrastinf( quote( aggr.dimw * ( aggr.nac / aggr.pop )^aggr.beta ), list_all )

        aggr.mu <- contrastinf( quote( aggr.mu + aggr.mu_iter ), list( aggr.mu = aggr.mu, aggr.mu_iter = aggr.mu_iter ) )

      } else {

        aggr.nac <- list( value = sum( nac.matrix[ w > 0 , i ] * w[ w > 0 ] ), lin = nac.matrix[ , i ] )
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
  variance <- survey::svyrecvar( estimate$lin / design$prob, design$cluster,design$strata, design$fpc, postStrata = design$postStrata )

  rval <- estimate$value
  class(rval) <- c( "cvystat" , "svystat" )
  attr(rval, "var") <- variance
  attr(rval, "statistic") <- "bourguignon"
  attr(rval, "dimensions") <- matrix( strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]], nrow = 1, ncol = ncol(var.class), dimnames = list( "variables", paste("dimension", 1:ncol(var.class) ) ) )
  attr(rval, "parameters") <- matrix( c( alpha, beta ), nrow = 1, ncol = 2, dimnames = list( "parameters", c( "alpha=", "beta=" ) ) )

  rval

}

#' @rdname svybmi
#' @export
svybmi.svyrep.design <- function( formula, design, alpha = .5, beta = -2, cutoffs = NULL, dimw = NULL, na.rm = FALSE , ...) {
  if (is.null(attr(design, "full_design"))) stop("you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svydesign() function.")

  nac.matrix <- model.frame(formula, design$variables, na.action = na.pass)[,]

  var.class <- lapply( nac.matrix, function(x) class(x)[1] )
  var.class <- matrix(var.class, nrow = 1, ncol = ncol(nac.matrix),
                      dimnames = list( c("var.class"), colnames( nac.matrix ) ) )

  if ( any( !( var.class %in% c( "numeric" ) ) ) ) {
    stop( "This function is only applicable to variables of type 'numeric'." )
  }

  w <- weights( design, "sampling" )

  if ( any( nac.matrix[ w != 0, var.class == "numeric" ] < 0, na.rm = TRUE ) ) {
    warning( "The function is defined for non-negative numeric variables only.")
    nps <- apply( nac.matrix[ , var.class == "numeric" ] < 0, 1, function( x ) any( x, na.rm = FALSE ) )
    nps[ is.na(nps) ] <- FALSE
    design <- design[nps == 0 ]
    w <- weights( design, "sampling" )
  }

  if (na.rm) {
    nas <- apply( nac.matrix, 1, function(x) any( is.na(x) ) )
    design <- design[nas == 0, ]
    w <- weights( design, "sampling" )
  }

  nac.matrix <- model.frame(formula, design$variables, na.action = na.pass)[,]

  # Normalized Achievement Matrix
  if ( any( ( nac.matrix < 0 | nac.matrix > 1 )[ w > 0 ], na.rm = T ) ) {

    for ( i in seq_along(var.class) ) {
      top <- max( nac.matrix[ , i], na.rm = TRUE )
      bottom <- min( nac.matrix[ , i], na.rm = TRUE )
      nac.matrix[ , i ] <- ( nac.matrix[ , i ] - bottom ) / ( top - bottom )
    }

  }

  # Cross Achievement Aggregation:
  if ( is.null( dimw ) ) {
    dimw = rep( 1 / ncol( nac.matrix ), ncol( nac.matrix ) )
  }
  if ( sum( dimw ) != 1 ) {
    warning( "sum(dimw) != 1. Normalizing dimension weights.")
    dimw = dimw / sum( dimw )
  }
  indiv.welfare <- apply( nac.matrix, 1, function(x) aggregator( mat = x, dimw = dimw, alpha = alpha, beta = beta ) )


  if ( any( is.na( indiv.welfare )[ w > 0 ] ) ) {

    rval <- as.numeric(NA)
    variance <- as.numeric(NA)
    class(rval) <- c( "cvystat" , "svystat" )
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "bourguignon"
    attr(rval, "dimensions") <- matrix( strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]], nrow = 1, ncol = ncol(var.class), dimnames = list( "variables", paste("dimension", 1:ncol(var.class) ) ) )
    attr(rval, "parameters") <- matrix( c( alpha, beta ), nrow = 1, ncol = 2, dimnames = list( "parameters", c( "alpha=", "beta=" ) ) )
    return(rval)

  }


  if ( beta != 0 ) {
    rval <- 1 - ( sum( w [ w > 0 ] * indiv.welfare [ w > 0 ] ) / sum( w [ w > 0 ] ) ) /
      sum( ( dimw * ( colSums( w [ w > 0 ] * nac.matrix [ w > 0, ] ) / sum( w [ w > 0 ] ) )^( beta ) ) )^( alpha / beta )

    ww <- weights( design, "analysis" )

    qq <- apply( ww, 2, function(wi) {
      1 - ( sum( wi [ w > 0 ] * indiv.welfare [ w > 0 ] ) / sum( wi [ w > 0 ] ) ) /
        sum( ( dimw * ( colSums( wi [ w > 0 ] * nac.matrix [ w > 0, ] ) / sum( wi [ w > 0 ] ) )^( 1 / beta ) )^( alpha / beta ) )
    } )

  } else {

    rval <- 1 - ( sum( w [ w > 0 ] * indiv.welfare [ w > 0 ] ) / sum( w [ w > 0 ] ) ) /
      prod( ( colSums( w [ w > 0 ] * nac.matrix [ w > 0, ] ) / sum( w [ w > 0 ] ) )^( dimw ) )^( alpha )

    ww <- weights( design, "analysis" )

    qq <- apply( ww, 2, function(wi) {
      1 - ( sum( wi [ w > 0 ] * indiv.welfare [ w > 0 ] ) / sum( wi [ w > 0 ] ) ) /
        prod( ( colSums( wi [ w > 0 ] * nac.matrix [ w > 0, ] ) / sum( wi [ w > 0 ] ) )^( dimw ) )^( alpha )
      } )

  }

  variance <- survey::svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)
  variance <- as.matrix( variance )

  class(rval) <- c( "cvystat" , "svystat" )
  attr(rval, "var") <- variance
  attr(rval, "statistic") <- "bourguignon"
  attr(rval, "dimensions") <- matrix( strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]], nrow = 1, ncol = ncol(var.class), dimnames = list( "variables", paste("dimension", 1:ncol(var.class) ) ) )
  attr(rval, "parameters") <- matrix( c( alpha, beta ), nrow = 1, ncol = 2, dimnames = list( "parameters", c( "alpha=", "beta=" ) ) )

  rval

}

#' @rdname svybmi
#' @export
svybmi.DBIsvydesign <-
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

    NextMethod("svybmi", design)
  }


aggregator <- function( mat, dimw, alpha, beta ) {


  if ( beta != 0 ) {

    return( ( sum( ( dimw * mat^beta ) ) )^( alpha / beta ) )

  } else {

    return( prod( mat^dimw )^alpha )

  }

}
