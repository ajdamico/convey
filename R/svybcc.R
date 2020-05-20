#' Bourguignon-Chakravarty multidimensional poverty class (EXPERIMENTAL)
#'
#' Estimate indices from the Bourguignon-Chakravarty class, a class of poverty measures.
#'
#' @param formula a formula specifying the variables. Variables can be numeric or ordered factors.
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param theta a scalar defining the elasticity of substitution between the normalized gaps of the attributes.
#' @param alpha a scalar that can be interpreted as the society's aversion to poverty.
#' @param cutoffs a list defining each variable's deprivation limit.
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
#' @note This function is experimental and is subject to changes in later versions.
#'
#' @seealso \code{\link{svyafc}}
#'
#' @references Francois Bourguignon and Satya R. Chakravarty (2003). The measurement of multidimensional poverty.
#' Journal of Economic Inequality, v. 1, n. 1, April 2003, pp. 1-25.
#' URL \url{http://dx.doi.org/10.1023/A:1023913831342}.
#'
#' Maria Casilda Lasso de la Vega, Ana Urrutia and Henar Diez (2009). The Bourguignon and Chakravarty multidimensional poverty family: a characterization.
#' Working Papers 109, ECINEQ, Society for the Study of Economic Inequality.
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
#' des_eusilc_rep <- as.svrepdesign( des_eusilc , type = "bootstrap" )
#' des_eusilc_rep <- convey_prep(des_eusilc_rep)
#'
#' # cutoffs
#' cos <- list( 10000, 5000 )
#'
#' # variables without missing values
#' svybcc( ~ eqincome + hy050n , design = des_eusilc , cutoffs = cos )
#' svybcc( ~ eqincome + hy050n , design = des_eusilc_rep , cutoffs = cos )
#'
#' # subsetting:
#' sub_des_eusilc <- subset( des_eusilc, db040 == "Styria")
#' sub_des_eusilc_rep <- subset( des_eusilc_rep, db040 == "Styria")
#'
#' svybcc( ~ eqincome + hy050n , design = sub_des_eusilc , cutoffs = cos )
#' svybcc( ~ eqincome + hy050n , design = sub_des_eusilc_rep , cutoffs = cos )
#'
#' \dontrun{
#'
#' # including factor variable with missings
#' cos <- list( 10000, 5000, "EU" )
#' svybcc(~eqincome+hy050n+pb220a, des_eusilc, cutoffs = cos, na.rm = FALSE )
#' svybcc(~eqincome+hy050n+pb220a, des_eusilc, cutoffs = cos, na.rm = TRUE )
#' svybcc(~eqincome+hy050n+pb220a, des_eusilc_rep, cutoffs = cos, na.rm = FALSE )
#' svybcc(~eqincome+hy050n+pb220a, des_eusilc_rep, cutoffs = cos, na.rm = TRUE )
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
#' dbd_eusilc <- update( dbd_eusilc, pb220a = ordered( pb220a ) )
#'
#' # cutoffs
#' cos <- list( 10000 , 5000 )
#'
#' # variables without missing values
#' svybcc(~eqincome+hy050n, design = dbd_eusilc, cutoffs = cos )
#'
#' # subsetting:
#' sub_dbd_eusilc <- subset( dbd_eusilc, db040 == "Styria")
#' svybcc(~eqincome+hy050n, design = sub_dbd_eusilc, cutoffs = cos )
#'
#' # cutoffs
#' cos <- list( 10000, 5000, "EU" )
#'
#' # including factor variable with missings
#' svybcc(~eqincome+hy050n+pb220a, dbd_eusilc, cutoffs = cos, na.rm = FALSE )
#' svybcc(~eqincome+hy050n+pb220a, dbd_eusilc, cutoffs = cos, na.rm = TRUE )
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' dbDisconnect( conn , shutdown = TRUE )
#'
#' }
#'
#' @export
svybcc <- function(formula, design, ...) {

  warning("The svybcc function is experimental and is subject to changes in later versions.")

  UseMethod("svybcc", design)

}

#' @rdname svybcc
#' @export
svybcc.survey.design <- function( formula, design, theta = .5 , alpha = .5 , cutoffs , dimw = NULL, na.rm = FALSE, ... ) {

  if ( theta <= 0 | alpha <= 0 ) stop( "This function is undefined for theta <= 0 or alpha <= 0." )
  if ( !is.list( cutoffs ) ) stop( "The parameter 'cutoffs' has to be a list." )

  ach.matrix <- model.frame(formula, design$variables, na.action = na.pass)[,]

  if ( !is.null( dimw ) ) {
    if ( any( is.na( dimw ) ) ) { stop( "Invalid value in dimension weights vector." ) }
    if ( sum( dimw ) > 1 ) { stop( "The sum of dimension weigths have to be equal to one." ) }
    if ( any( dimw > 1 | dimw < 0 ) ) { stop( "Dim. weights have to be within interval [0,1]." ) }
    if ( length(dimw) != ncol(ach.matrix) ) { stop( "Dimension weights' length differs from number of dimensions in formula" ) }
  }

  var.class <- lapply( ach.matrix, function(x) class(x)[1] )
  var.class <- matrix(var.class, nrow = 1, ncol = ncol(ach.matrix),
                      dimnames = list( c("var.class"), colnames( ach.matrix ) ) )

  if ( any( !( var.class %in% c( "numeric", "integer", "ordered" ) ) ) ) {
    stop( "This function is only applicable to variables of types 'numeric' or 'ordered factor'." )
  }
  if ( any( ( var.class == "integer" ) ) ) {
    stop( "At least one of the variables is an integer.\nCoerce your column to numeric with as.numeric if you are sure it's what you want." )
  }

  w <- 1/design$prob

  if ( any( ach.matrix[ w != 0, var.class == "numeric" ] < 0, na.rm = TRUE ) ) stop( "The Bourguignon-Chakravarty multidimensional poverty class is defined for non-negative numeric variables only.")

  if (na.rm) {
    nas <- apply( ach.matrix, 1, function(x) any( is.na(x) ) )
    design <- design[nas == 0, ]
    w <- 1/design$prob
  }

  ach.matrix <- model.frame(formula, design$variables, na.action = na.pass)[,]
  ach.matrix <- ach.matrix [ w > 0, ]
  w <- w [ w > 0 ]

  if ( any( is.na(ach.matrix) ) ) {

    if ( is.null(dimw) ) {
      dimw = rep( 1 / ncol(var.class), length(var.class) )
    }

    rval <- as.numeric(NA)
    variance <- as.numeric(NA)
    class(rval) <- c( "cvystat" , "svystat" )
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "bourguignon-chakravarty"
    dimtable <- as.data.frame( matrix( c( strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]], dimw ), nrow = ncol(var.class), ncol = 2, dimnames = list( paste("dimension", 1:ncol(var.class) ), c( "variables", "weight" ) ) ), stringsAsFactors = FALSE )
    dimtable[,2] <- as.numeric( dimtable[,2] )
    attr(rval, "dimensions") <- dimtable
    attr(rval, "parameters") <- matrix( c( theta, alpha ), nrow = 1, ncol = 2, dimnames = list( "parameters", c( "theta=", "alpha=" ) ) )

    return(rval)

  }

  # Deprivation Matrix
  dep.matrix <- sapply( seq_along(cutoffs), FUN = function( i ){
    if ( var.class[ i ] == "numeric" ) {
      ( 1 - ach.matrix[ , i ] / cutoffs[[i]] ) * ( cutoffs[[i]] > ach.matrix[ , i ] )
    } else {
      ( cutoffs[[i]] > ach.matrix[ , i ] )
    }
  } )
  colnames(dep.matrix) <- colnames( var.class )

  # calculate multidimensional deprivation scores
  if ( is.null(dimw) ) {
    dimw = rep( 1 / ncol(dep.matrix), ncol(dep.matrix) )
  }
  # Weighted sum of deprivations:
  depr.scores <- ( rowSums( sweep( dep.matrix, MARGIN=2 , dimw,`*`) )^( 1 / theta ) )^( alpha )

  # calculate the poverty measure
  w <- 1/design$prob
  w[ w > 0 ] <- depr.scores
  depr.scores <- w
  rm(w)

  estimate <- survey::svymean( depr.scores , design )

  rval <- estimate
  variance <- attr( estimate, "var" )
  class(rval) <- c( "cvystat" , "svystat" )
  attr(rval, "var") <- variance
  names( rval )[1] <- attr(rval, "statistic") <- "bourguignon-chakravarty"
  dimtable <- as.data.frame(matrix( c( strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]], dimw ), nrow = ncol(var.class), ncol = 2, dimnames = list( paste("dimension", 1:ncol(var.class) ), c( "variables", "weight" ) ) ), stringsAsFactors = FALSE )
  dimtable[,2] <- as.numeric( dimtable[,2] )
  attr(rval, "dimensions") <- dimtable
  attr(rval, "parameters") <- matrix( c( theta, alpha ), nrow = 1, ncol = 2, dimnames = list( "parameters", c( "theta=", "alpha=" ) ) )

  return( rval )

}

#' @rdname svybcc
#' @export
svybcc.svyrep.design <- function( formula, design, theta = .5 , alpha = .5 , cutoffs , dimw = NULL, na.rm = FALSE, ... ) {

  if ( theta <= 0 | alpha <= 0 ) stop( "This function is undefined for theta <= 0 or alpha <= 0." )
  if ( !is.list( cutoffs ) ) stop( "The parameter 'cutoffs' has to be a list." )

  ach.matrix <- model.frame(formula, design$variables, na.action = na.pass)[,]

  if ( !is.null( dimw ) ) {
    if ( any( is.na( dimw ) ) ) { stop( "Invalid value in dimension weights vector." ) }
    if ( sum( dimw ) > 1 ) { stop( "The sum of dimension weigths have to be equal to one." ) }
    if ( any( dimw > 1 | dimw < 0 ) ) { stop( "Dim. weights have to be within interval [0,1]." ) }
    if ( length(dimw) != ncol(ach.matrix) ) { stop( "Dimension weights' length differs from number of dimensions in formula" ) }
  }

  var.class <- lapply( ach.matrix, function(x) class(x)[1] )
  var.class <- matrix(var.class, nrow = 1, ncol = ncol(ach.matrix),
                      dimnames = list( c("var.class"), colnames( ach.matrix ) ) )

  if ( any( !( var.class %in% c( "numeric", "integer", "ordered" ) ) ) ) {
    stop( "This function is only applicable to variables of types 'numeric' or 'ordered factor'." )
  }
  if ( any( ( var.class == "integer" ) ) ) {
    stop( "At least one of the variables is an integer.\nCoerce your column to numeric with as.numeric if you are sure it's what you want." )
  }

  w <- weights(design, "sampling" )

  if ( any( ach.matrix[ w != 0, var.class == "numeric" ] < 0, na.rm = TRUE ) ) stop( "The Bourguignon-Chakravarty multidimensional poverty class is defined for non-negative numeric variables only.")

  if (na.rm) {
    nas <- apply( ach.matrix, 1, function(x) any( is.na(x) ) )
    design <- design[nas == 0, ]
    w <- weights(design, "sampling" )
  }

  ach.matrix <- model.frame(formula, design$variables, na.action = na.pass)[,]
  ach.matrix <- ach.matrix [ w > 0, ]
  w <- w [ w > 0 ]

  if ( any( is.na(ach.matrix) ) ) {

    if ( is.null(dimw) ) {
      dimw = rep( 1 / ncol(var.class), length(var.class) )
    }

    rval <- as.numeric(NA)
    variance <- as.numeric(NA)
    class(rval) <- c( "cvystat" , "svystat" )
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "bourguignon-chakravarty"
    dimtable <- as.data.frame(matrix( c( strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]], dimw ), nrow = ncol(var.class), ncol = 2, dimnames = list( paste("dimension", 1:ncol(var.class) ), c( "variables", "weight" ) ) ), stringsAsFactors = FALSE )
    dimtable[,2] <- as.numeric( dimtable[,2] )
    attr(rval, "dimensions") <- dimtable
    attr(rval, "parameters") <- matrix( c( theta, alpha ), nrow = 1, ncol = 2, dimnames = list( "parameters", c( "theta=", "alpha=" ) ) )

    return(rval)

  }

  # Deprivation Matrix
  dep.matrix <- sapply( seq_along(cutoffs), FUN = function( i ){
    if ( var.class[ i ] == "numeric" ) {
      ( 1 - ach.matrix[ , i ] / cutoffs[[i]] ) * ( cutoffs[[i]] > ach.matrix[ , i ] )
    } else {
      ( cutoffs[[i]] > ach.matrix[ , i ] )
    }
  } )
  colnames(dep.matrix) <- colnames( var.class )

  # calculate multidimensional deprivation scores
  if ( is.null(dimw) ) {
    dimw = rep( 1 / ncol(dep.matrix), ncol(dep.matrix) )
  }
  depr.scores <- ( rowSums( sweep( dep.matrix, MARGIN=2 , dimw,`*`) )^( 1 / theta ) )^( alpha )

  # calculate the poverty measure
  w <- weights(design, "sampling" )
  w[ w > 0 ] <- depr.scores
  depr.scores <- w
  rm(w)

  estimate <- survey::svymean( depr.scores , design )

  rval <- estimate
  variance <- attr( estimate, "var" )
  class(rval) <- c( "cvystat" , "svrepstat" )
  attr(rval, "var") <- variance
  names( rval )[1] <- attr(rval, "statistic") <-  "bourguignon-chakravarty"
  dimtable <- as.data.frame( matrix( c( strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]], dimw ), nrow = ncol(var.class), ncol = 2, dimnames = list( paste("dimension", 1:ncol(var.class) ), c( "variables", "weight" ) ) ), stringsAsFactors = FALSE )
  dimtable[,2] <- as.numeric( dimtable[,2] )
  attr(rval, "dimensions") <- dimtable
  attr(rval, "parameters") <- matrix( c( theta, alpha ), nrow = 1, ncol = 2, dimnames = list( "parameters", c( "theta=", "alpha=" ) ) )

  return( rval )

}

#' @rdname svybcc
#' @export
svybcc.DBIsvydesign <-
  function (formula, design, ...) {

    design$variables <- getvars(formula, design$db$connection, design$db$tablename, updates = design$updates, subset = design$subset)

    NextMethod("svybcc", design)
  }
