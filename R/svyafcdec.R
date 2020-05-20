#' Alkire-Foster multidimensional poverty decompostition
#'
#' Decomposition of indices from the Alkire-Foster class
#'
#' @param formula a formula specifying the variables. Variables can be numeric or ordered factors.
#' @param subgroup a formula defining the group variable for decomposition.
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param g a scalar defining the exponent of the indicator.
#' @param cutoffs a list defining each variable's deprivation limit.
#' @param k a scalar defining the multidimensional cutoff.
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
#' @references Sabina Alkire and James Foster (2011). Counting and multidimensional poverty measurement.
#' Journal of Public Economics, v. 95, n. 7-8, August 2011, pp. 476-487, ISSN 0047-2727.
#' URL \url{http://dx.doi.org/10.1016/j.jpubeco.2010.11.006}.
#'
#' Alkire et al. (2015). Multidimensional Poverty Measurement and Analysis. Oxford University Press, 2015.
#'
#' Daniele Pacifico and Felix Poege (2016). MPI: Stata module to compute the Alkire-Foster multidimensional poverty measures and their decomposition groups deprivation indicators and population sub-groups.
#' URL \url{http://EconPapers.repec.org/RePEc:boc:bocode:s458120}.
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
#'
#' # variables without missing values
#' svyafcdec( ~ eqincome + hy050n ,  ~1 , des_eusilc , k = .5 , g = 0, cutoffs = cos )
#' svyafcdec( ~ eqincome + hy050n ,  ~1 , des_eusilc_rep , k = .5 , g = 0, cutoffs = cos )
#'
#' svyafcdec( ~ eqincome + hy050n ,  ~rb090 , des_eusilc , k = .5 , g = 0, cutoffs = cos )
#' svyafcdec( ~ eqincome + hy050n ,  ~rb090 , des_eusilc_rep , k = .5 , g = 0, cutoffs = cos )
#'
#' # subsetting:
#' sub_des_eusilc <- subset( des_eusilc, db040 == "Styria")
#' sub_des_eusilc_rep <- subset( des_eusilc_rep, db040 == "Styria")
#'
#' svyafcdec( ~ eqincome + hy050n ,  ~1 , sub_des_eusilc , k = .5 , g = 0, cutoffs = cos )
#' svyafcdec( ~ eqincome + hy050n ,  ~1 , sub_des_eusilc_rep , k = .5 , g = 0, cutoffs = cos )
#'
#' svyafcdec( ~ eqincome + hy050n ,  ~rb090 , sub_des_eusilc ,
#'	k = .5 , g = 0, cutoffs = cos )
#' svyafcdec( ~ eqincome + hy050n ,  ~rb090 , sub_des_eusilc_rep ,
#'	k = .5 , g = 0, cutoffs = cos )
#'
#' \dontrun{
#'
#' # including factor variable with missings
#' cos <- list( 10000, 5000, "EU" )
#' svyafcdec(~eqincome+hy050n+pb220a,  ~1 , des_eusilc,
#' 		k = .5, g = 0, cutoffs = cos , na.rm = FALSE )
#' svyafcdec(~eqincome+hy050n+pb220a,  ~1 , des_eusilc,
#' 		k = .5, g = 0, cutoffs = cos , na.rm = TRUE )
#' svyafcdec(~eqincome+hy050n+pb220a,  ~1 , des_eusilc_rep,
#' 		k = .5, g = 0, cutoffs = cos , na.rm = FALSE )
#' svyafcdec(~eqincome+hy050n+pb220a,  ~1 , des_eusilc_rep,
#' 		k = .5, g = 0, cutoffs = cos , na.rm = TRUE )
#'
#' svyafcdec(~eqincome+hy050n+pb220a,  ~rb090 , des_eusilc,
#' 		k = .5, g = 0, cutoffs = cos , na.rm = FALSE )
#' svyafcdec(~eqincome+hy050n+pb220a,  ~rb090 , des_eusilc,
#' 		k = .5, g = 0, cutoffs = cos , na.rm = TRUE )
#' svyafcdec(~eqincome+hy050n+pb220a,  ~rb090 , des_eusilc_rep,
#' 		k = .5, g = 0, cutoffs = cos , na.rm = FALSE )
#' svyafcdec(~eqincome+hy050n+pb220a,  ~rb090 , des_eusilc_rep,
#' 		k = .5, g = 0, cutoffs = cos , na.rm = TRUE )
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
#' svyafcdec( ~eqincome+hy050n ,  ~1 , des_eusilc ,
#'	k = .5 , g = 0, cutoffs = cos )
#' svyafcdec( ~eqincome+hy050n ,  ~rb090 , des_eusilc ,
#'	k = .5 , g = 0, cutoffs = cos )
#'
#' # subsetting:
#' sub_des_eusilc <- subset( des_eusilc, db040 == "Styria")
#'
#' svyafcdec( ~ eqincome + hy050n ,  ~1 , sub_des_eusilc ,
#' 		k = .5 , g = 0, cutoffs = cos )
#'
#' svyafcdec( ~ eqincome + hy050n ,  ~rb090 , sub_des_eusilc ,
#' 		k = .5 , g = 0, cutoffs = cos )
#'
#' # including factor variable with missings
#' cos <- list( 10000, 5000, "EU" )
#'
#' svyafcdec(~eqincome+hy050n+pb220a,  ~1 , dbd_eusilc,
#' 		k = .5, g = 0, cutoffs = cos , na.rm = FALSE )
#' svyafcdec(~eqincome+hy050n+pb220a,  ~1 , dbd_eusilc,
#' 		k = .5, g = 0, cutoffs = cos , na.rm = TRUE )
#'
#' svyafcdec(~eqincome+hy050n+pb220a,  ~rb090 , dbd_eusilc,
#' 		k = .5, g = 0, cutoffs = cos , na.rm = FALSE )
#' svyafcdec(~eqincome+hy050n+pb220a,  ~rb090 , dbd_eusilc,
#' 		k = .5, g = 0, cutoffs = cos , na.rm = TRUE )
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' dbDisconnect( conn , shutdown = TRUE )
#'
#' }
#'
#' @export
svyafcdec <- function(formula, subgroup , design, ...) {

  UseMethod("svyafcdec", design)

}

#' @rdname svyafcdec
#' @export
svyafcdec.survey.design <- function( formula, subgroup = ~1 , design, g , cutoffs , k , dimw = NULL, na.rm = FALSE, ... ) {

  if ( k <= 0 | k > 1 ) stop( "This functions is only defined for k in (0,1]." )
  if ( g < 0 ) stop( "This function is undefined for g < 0." )
  if ( !is.list( cutoffs ) ) stop( "The parameter 'cutoffs' has to be a list." )
  if( length( cutoffs ) != length( all.vars( formula ) ) ) stop( "number of variables in formula must exactly match cutoffs" )
  if ( !is.null( dimw ) ) {
    if ( any( is.na( dimw ) ) ) { stop( "Invalid value in dimension weights vector." ) }
    if ( sum( dimw ) > 1 ) { stop( "The sum of dimension weigths have to be equal to one." ) }
    if ( any( dimw > 1 | dimw < 0 ) ) { stop( "Dim. weights have to be within interval [0,1]." ) }
  }

  ach.matrix <- model.frame(formula, design$variables, na.action = na.pass)[,]

  if ( !is.null( dimw ) ) {
    if ( any( is.na( dimw ) ) ) { stop( "Invalid value in dimension weights vector." ) }
    if ( sum( dimw ) > 1 ) { stop( "The sum of dimension weigths have to be equal to one." ) }
    if ( any( dimw > 1 | dimw < 0 ) ) { stop( "Dim. weights have to be within interval [0,1]." ) }
    if ( length(dimw) != ncol(ach.matrix) ) { stop( "Dimension weights' length differs from number of dimensions in formula" ) }
  }

  grpvar <- model.frame(subgroup, design$variables, na.action = na.pass)[,]

  var.class <- lapply( ach.matrix, function(x) class(x)[1] )
  var.class <- matrix(var.class, nrow = 1, ncol = ncol(ach.matrix), dimnames = list( c("var.class"), colnames( ach.matrix ) ) )

  if ( any( !( var.class %in% c( "numeric", "ordered" ) ) ) ) {
    stop( "This function is only applicable to variables of types 'numeric' or 'ordered factor'." )
  }

  w <- 1/design$prob

  if ( any( ach.matrix[ w != 0, var.class == "numeric" ] < 0, na.rm = TRUE ) ) stop( "The Alkire-Foster multidimensional poverty decompostition is defined for non-negative numeric variables only." )

  ach.matrix <- model.frame(formula, design$variables, na.action = na.pass)[,]
  grpvar <- model.frame(subgroup, design$variables, na.action = na.pass)[,]

  if ( class(grpvar) == "labelled" ) {
    stop( "This function does not support 'labelled' variables. Try factor().")
  }

  if (na.rm) {
    nas <- apply( cbind( ach.matrix, grpvar ), 1, function(x) any( is.na(x) ) )
    design <- design[ nas == 0, ]
  }

  w <- 1/design$prob
  ach.matrix <- model.frame(formula, design$variables, na.action = na.pass)[,]
  grpvar <- model.frame(subgroup, design$variables, na.action = na.pass)[,]

  # Deprivation Matrix
  dep.matrix <- sapply( seq_along(var.class), FUN = function( i ){ 1*( cutoffs[[i]] > ach.matrix[ , i ] ) } )
  colnames(dep.matrix) <- colnames( var.class )

  # Unweighted count of deprivations:
  # depr.count <- rowSums( dep.matrix )

  # deprivation k cut
  if ( is.null(dimw) ) {
    dimw = rep( 1 / ncol(dep.matrix), ncol(dep.matrix) )
  }

  # Weighted sum of deprivations:
  depr.sums <- rowSums( sweep( dep.matrix, MARGIN=2 , dimw,`*`) )

  # k multidimensional cutoff:
  multi.cut <- depr.sums*( depr.sums >= k )
  #rm(dep.matrix)

  # Censored Deprivation Matrix
  cen.dep.matrix <- sapply( seq_along(cutoffs) , FUN = function( x ) {
    if ( var.class[ x ] == "numeric" ) {
      1*( cutoffs[[x]] > ach.matrix[ , x ] ) * ( ( cutoffs[[x]] - ach.matrix[ , x ] ) / cutoffs[[x]] )^g
    } else {
      1*( cutoffs[[x]] > ach.matrix[ , x ] )
    }
  } )
  colnames(cen.dep.matrix) <- colnames( var.class )

  cen.dep.matrix[ multi.cut == 0 & !is.na( multi.cut ), ] <- 0

  # Sum of censored deprivations:
  cen.depr.sums <- rowSums( sweep( cen.dep.matrix, MARGIN=2 , dimw,`*`) )

  if ( any( is.na( cen.depr.sums )[ w > 0 ] ) ) {

    # overall result:
    overall.result <- matrix( c( NA, NA ), nrow = 1, ncol = 2, dimnames = list( "overall", c( "alkire-foster", "SE")) )

    # group breakdown:
    if ( !is.null( levels( grpvar ) ) ) {

      grp.estim <- matrix( rep( NA, 2*length( levels( grpvar ) ) ), nrow = length( levels( grpvar ) ), ncol = 2, dimnames = list( levels( grpvar ), c( "alkire-foster", "SE" ) ) )
      grp.contr <- matrix( rep( NA, 2*length( levels( grpvar ) ) ), nrow = length( levels( grpvar ) ), ncol = 2, dimnames = list( levels( grpvar ), c( "contribution", "SE" ) ) )

    }

    # dimensional breakdown:
    dim.raw.hc <- matrix( rep( NA, 2*ncol( cen.dep.matrix ) ), nrow = ncol( cen.dep.matrix ), ncol = 2, dimnames = list( colnames( cen.dep.matrix ), c( "raw headcount", "SE" ) ) )
    dim.cen.hc <- matrix( rep( NA, 2*ncol( cen.dep.matrix ) ), nrow = ncol( cen.dep.matrix ), ncol = 2, dimnames = list( colnames( cen.dep.matrix ), c( "cens. headcount", "SE" ) ) )
    dim.contri <- matrix( rep( NA, 2*ncol( cen.dep.matrix ) ), nrow = ncol( cen.dep.matrix ), ncol = 2, dimnames = list( colnames( cen.dep.matrix ), c( "contribution", "SE" ) ) )

    # set up result object:
    if ( is.null( levels( grpvar ) ) ) {
      rval <- list( overall.result, dim.raw.hc, dim.cen.hc, dim.contri )
    } else {
      rval <- list( overall.result, dim.raw.hc, dim.cen.hc, dim.contri, grp.estim, grp.contr )
    }

    return(rval)

  }

  raw.hc.ratio <- survey::svymean( dep.matrix[,] , design, na.rm = TRUE )
  attr( raw.hc.ratio, "statistic" ) <- "raw headcount"
  cen.hc.ratio <- survey::svymean( cen.dep.matrix[,] , design, na.rm = TRUE )
  attr( cen.hc.ratio, "statistic" ) <- "cens. headcount"

  U_0 <- list( value = sum( w[ w > 0 ] ), lin = rep( 1, length( w ) ) )
  U_1 <- list( value = sum( w[ w > 0 ] * cen.depr.sums[ w > 0 ] ), lin = cen.depr.sums )

  # overall alkire-foster index:
  overall <- survey::svymean( cen.depr.sums, design, na.rm = TRUE )
  names( overall )[1] <- attr( overall, "statistic" ) <- "alkire-foster"

  # group decomposition
  if ( !is.null( levels( grpvar ) ) ) {
    grp.pctg.estm <- NULL
    grp.pctg.estm_var <- NULL
    grp.pctg.cont <- NULL
    grp.pctg.cont_lin <- matrix( data = rep( w, length( levels( grpvar ) ) ), nrow = length( w ) )
    for (i in seq_along( levels( grpvar ) ) ) {

      w_i <- w * ( grpvar == levels( grpvar )[ i ] )
      U_1_i <- list( value = sum( cen.depr.sums[ w_i > 0 ] * w_i[ w_i > 0 ] ), lin = cen.depr.sums * ( grpvar == levels( grpvar )[ i ] ) )
      U_0_i <- list( value = sum( w_i[ w_i > 0 ] ), lin = 1 * ( grpvar == levels( grpvar )[ i ] ) )

      list_all <- list( U_0 = U_0, U_1 = U_1, U_0_i = U_0_i, U_1_i = U_1_i )

      estimate <- contrastinf( quote( ( U_1_i / U_0_i ) ), list_all )
      grp.pctg.estm[ i ] <- estimate$value
      estimate$lin[ is.na( estimate$lin ) ] <- 0
      grp.pctg.estm_var[ i ] <- survey::svyrecvar( estimate$lin*w_i, design$cluster,design$strata, design$fpc, postStrata = design$postStrata)

      estimate <- contrastinf( quote( ( U_0_i /U_0 ) * ( U_1_i / U_0_i ) / ( U_1 / U_0 ) ), list_all )
      grp.pctg.cont[ i ] <- estimate$value
      estimate$lin[ is.na( estimate$lin ) ] <- 0
      grp.pctg.cont_lin[ , i ] <- estimate$lin

    }

    grp.pctg.cont_var <- survey::svyrecvar( grp.pctg.cont_lin*w, design$cluster,design$strata, design$fpc, postStrata = design$postStrata)

    grp.contr.estimate <- matrix( grp.pctg.estm, ncol = 1, dimnames = list( levels( grpvar ), "alkire-foster" ) )
    attr( grp.contr.estimate, "names" ) <- levels( grpvar )
    attr( grp.contr.estimate, "var") <- grp.pctg.estm_var
    attr( grp.contr.estimate, "statistic") <- "alkire-foster"
    class( grp.contr.estimate ) <- c( "svystat" )

    grp.contr.pct <- matrix( grp.pctg.cont, ncol = 1, dimnames = list( levels( grpvar ), "grp. % contribution" ) )
    attr( grp.contr.pct, "names" ) <- levels( grpvar )
    attr( grp.contr.pct, "var") <- grp.pctg.cont_var
    attr( grp.contr.pct, "statistic") <- "grp. % contribution"
    class( grp.contr.pct ) <- c( "svystat" )

    rm( grp.pctg.estm, grp.pctg.estm_var, grp.pctg.cont, grp.pctg.cont_lin, grp.pctg.cont_var )

  }

  # dimensional decomposition:
  dim.contr <- NULL
  dim.contr_lin <- matrix( data = rep( w, ncol(cen.dep.matrix) ), nrow = length( w ) )
  for ( i in 1:ncol(cen.dep.matrix) ) {

    wj <- list( value = dimw[i], lin = rep( 0, length( cen.depr.sums ) ) )

    H_0 <- list( value = sum( cen.dep.matrix[ w > 0 , i ] * w[ w > 0 ] ), lin = cen.dep.matrix[ , i ] * ( w > 0 ) )
    list_all <- list( U_0 = U_0, U_1 = U_1, H_0 = H_0, wj = wj )
    estimate <- contrastinf( quote( wj * ( H_0 / U_0 ) / ( U_1 / U_0 ) ), list_all )

    dim.contr[ i ] <- estimate$value
    estimate$lin[ is.na( estimate$lin ) ] <- 0
    dim.contr_lin[ , i ] <- estimate$lin

  }

  dim.contr_var <- survey::svyrecvar( dim.contr_lin*w, design$cluster,design$strata, design$fpc, postStrata = design$postStrata )

  dim.result <- dim.contr
  attr( dim.result, "names" ) <- colnames( ach.matrix )
  attr( dim.result, "var") <- dim.contr_var
  attr( dim.result, "statistic") <- "dim. % contribution"
  class( dim.result ) <- c( "svystat" )

  rm( dim.contr, dim.contr_lin, dim.contr_var )

  # set up result object:
  if ( is.null( levels( grpvar ) ) ) {

    rval <- list( overall, raw.hc.ratio, cen.hc.ratio, dim.result )
    names( rval ) <- list( "overall", "raw headcount ratio", "censored headcount ratio", "percentual contribution per dimension" )

  } else {

    rval <- list( overall, raw.hc.ratio, cen.hc.ratio, dim.result, grp.contr.estimate, grp.contr.pct )
    names( rval ) <- list( "overall", "raw headcount ratio", "censored headcount ratio", "percentual contribution per dimension", "subgroup alkire-foster estimates", "percentual contribution per subgroup" )

  }

  return( rval )

}

#' @rdname svyafcdec
#' @export
svyafcdec.svyrep.design <- function( formula, subgroup = ~1 , design, g , cutoffs , k , dimw = NULL, na.rm=FALSE, ...) {

  if ( k <= 0 | k > 1 ) stop( "This functions is only defined for k in (0,1]." )
  if ( g < 0 ) stop( "This function is undefined for g < 0." )
  if ( !is.list( cutoffs ) ) stop( "The parameter 'cutoffs' has to be a list." )
  if( length( cutoffs ) != length( all.vars( formula ) ) ) stop( "number of variables in formula must exactly match cutoffs" )
  if ( !is.null( dimw ) ) {
    if ( any( is.na( dimw ) ) ) { stop( "Invalid value in dimension weights vector." ) }
    if ( sum( dimw ) > 1 ) { stop( "The sum of dimension weigths have to be equal to one." ) }
    if ( any( dimw > 1 | dimw < 0 ) ) { stop( "Dim. weights have to be within interval [0,1]." ) }
  }

  ach.matrix <- model.frame(formula, design$variables, na.action = na.pass)[,]

  var.class <- lapply( ach.matrix, function(x) class(x)[1] )
  var.class <- matrix( var.class, nrow = 1, ncol = ncol(ach.matrix), dimnames = list( c("var.class"), colnames( ach.matrix ) ) )

  if ( any( !( var.class %in% c( "numeric", "ordered" ) ) ) ) {
    stop( "This function is only applicable to variables of types 'numeric' or 'ordered factor'." )
  }

  ws <- weights(design, "sampling")

  if ( any( ach.matrix[ ws != 0, var.class == "numeric" ] < 0, na.rm = TRUE ) ) stop( "The Alkire-Foster multidimensional poverty decompostition is defined for non-negative numeric variables only." )

  ach.matrix <- model.frame(formula, design$variables, na.action = na.pass)[,]
  grpvar <- model.frame(subgroup, design$variables, na.action = na.pass)[,]

  if ( class(grpvar) == "labelled" ) {
    stop( "This function does not support 'labelled' variables. Try factor().")
  }

  if (na.rm) {
    nas <- apply( cbind( ach.matrix, grpvar ), 1, function(x) any( is.na(x) ) )
    design <- design[ nas == 0, ]
  }

  ws <- weights(design, "sampling")
  ach.matrix <- model.frame(formula, design$variables, na.action = na.pass)[,]
  grpvar <- model.frame(subgroup, design$variables, na.action = na.pass)[,]

  # Deprivation Matrix
  dep.matrix <- sapply( seq_along(var.class), FUN = function( i ){ 1*( cutoffs[[i]] > ach.matrix[ , i ] ) } )
  colnames(dep.matrix) <- colnames( var.class )

  # Unweighted count of deprivations:
  # depr.count <- rowSums( dep.matrix )

  # deprivation k cut
  if ( is.null(dimw) ) {
    dimw = rep( 1 / ncol( dep.matrix ), ncol( dep.matrix ) )
  }

  # Weighted sum of deprivations:
  depr.sums <- rowSums( sweep( dep.matrix, MARGIN=2 , dimw,`*`) )

  # k multidimensional cutoff:
  multi.cut <- depr.sums*( depr.sums >= k )
  #rm(dep.matrix)

  # Censored Deprivation Matrix
  cen.dep.matrix <- sapply( seq_along(cutoffs) , FUN = function( x ) {
    if ( var.class[ x ] == "numeric" ) {
      1*( cutoffs[[x]] > ach.matrix[ , x ] ) * ( ( cutoffs[[x]] - ach.matrix[ , x ] ) / cutoffs[[x]] )^g
    } else {
      1*( cutoffs[[x]] > ach.matrix[ , x ] )
    }
  } )
  colnames(cen.dep.matrix) <- colnames( var.class )
  cen.dep.matrix[ multi.cut == 0 & !is.na( multi.cut ), ] <- 0

  ww <- weights( design, "analysis")

  # Sum of censored deprivations:
  cen.depr.sums <- rowSums( sweep( cen.dep.matrix, MARGIN=2 , dimw,`*`) )

  if ( any( is.na( cen.depr.sums )[ ws > 0 ] ) ) {

    # overall result:
    overall.result <- matrix( c( NA, NA ), nrow = 1, ncol = 2, dimnames = list( "overall", c( "alkire-foster", "SE")) )

    # group breakdown:
    if ( !is.null( levels( grpvar ) ) ) {

      grp.estim <- matrix( rep( NA, 2*length( levels( grpvar ) ) ), nrow = length( levels( grpvar ) ), ncol = 2, dimnames = list( levels( grpvar ), c( "alkire-foster", "SE" ) ) )
      grp.contr <- matrix( rep( NA, 2*length( levels( grpvar ) ) ), nrow = length( levels( grpvar ) ), ncol = 2, dimnames = list( levels( grpvar ), c( "contribution", "SE" ) ) )

    }

    # dimensional breakdown:
    dim.raw.hc <- matrix( rep( NA, 2*ncol( cen.dep.matrix ) ), nrow = ncol( cen.dep.matrix ), ncol = 2, dimnames = list( colnames( cen.dep.matrix ), c( "raw headcount", "SE" ) ) )
    dim.cen.hc <- matrix( rep( NA, 2*ncol( cen.dep.matrix ) ), nrow = ncol( cen.dep.matrix ), ncol = 2, dimnames = list( colnames( cen.dep.matrix ), c( "cens. headcount", "SE" ) ) )
    dim.contri <- matrix( rep( NA, 2*ncol( cen.dep.matrix ) ), nrow = ncol( cen.dep.matrix ), ncol = 2, dimnames = list( colnames( cen.dep.matrix ), c( "contribution", "SE" ) ) )

    # set up result object:
    if ( is.null( levels( grpvar ) ) ) {
      rval <- list( overall.result, dim.raw.hc, dim.cen.hc, dim.contri )
    } else {
      rval <- list( overall.result, dim.raw.hc, dim.cen.hc, dim.contri, grp.estim, grp.contr )
    }

    return(rval)

  }

  raw.hc.ratio <- survey::svymean( dep.matrix[,] , design, na.rm = TRUE )
  attr( raw.hc.ratio, "statistic" ) <- "raw headcount"
  cen.hc.ratio <- survey::svymean( cen.dep.matrix[,] , design, na.rm = TRUE )
  attr( cen.hc.ratio, "statistic" ) <- "cens. headcount"

  # overall alkire-foster index:
  overall <- survey::svymean( as.matrix(cen.depr.sums) , design, na.rm = TRUE )
  names( overall )[1] <- attr( overall, "statistic" ) <- "alkire-foster"

  # group decomposition
  if ( !is.null( levels( grpvar ) ) ) {
    grp.pctg.estm <- NULL
    grp.pctg.estm_var <- NULL
    qq.estm <- matrix( NA, ncol = length(levels( grpvar ) ), nrow = ncol( ww ) )
    grp.pctg.cont <- NULL
    grp.pctg.cont_var <- NULL
    qq.pctg <- matrix( NA, ncol = length(levels( grpvar ) ), nrow = ncol( ww ) )

    for ( i in seq_along( levels( grpvar ) ) ) {

      grp.pctg.estm[ i ] <- sum( ws * ( grpvar == levels( grpvar )[ i ] ) * cen.depr.sums ) / sum( ws * ( grpvar == levels( grpvar )[ i ] ) )

      qq.estm[ , i ] <- apply( ww, 2, function(wi) {
        sum( wi * ( grpvar == levels( grpvar )[ i ] ) * cen.depr.sums ) / sum( wi * ( grpvar == levels( grpvar )[ i ] ) )
      } )


      grp.pctg.cont[ i ] <- ( sum( ws * ( grpvar == levels( grpvar )[ i ] ) ) / sum( ws ) ) *
        ( ( sum( ws * ( grpvar == levels( grpvar )[ i ] ) * cen.depr.sums ) /
              sum( ws * ( grpvar == levels( grpvar )[ i ] ) ) ) /
            ( sum( ws * cen.depr.sums ) /
                sum( ws ) ) )

      qq.pctg[ , i ] <- apply( ww, 2, function(wi) {
        ( sum( wi * ( grpvar == levels( grpvar )[ i ] ) ) / sum( wi ) ) *
          ( ( sum( wi * ( grpvar == levels( grpvar )[ i ] ) * cen.depr.sums ) /
                sum( wi * ( grpvar == levels( grpvar )[ i ] ) ) ) /
              ( sum( wi * cen.depr.sums ) /
                  sum( wi ) ) )
      } )

    }

    grp.pctg.estm_var <- survey::svrVar( qq.estm, design$scale, design$rscales, mse = design$mse, coef = grp.pctg.estm )
    grp.pctg.cont_var <- survey::svrVar( qq.pctg, design$scale, design$rscales, mse = design$mse, coef = grp.pctg.cont )

    grp.contr.estimate <- matrix( grp.pctg.estm, ncol = 1, dimnames = list( levels( grpvar ), "alkire-foster" ) )
    attr( grp.contr.estimate, "names" ) <- levels( grpvar )
    attr( grp.contr.estimate, "var") <- grp.pctg.estm_var
    attr( grp.contr.estimate, "statistic") <- "alkire-foster"
    class( grp.contr.estimate ) <- c( "svrepstat" )

    grp.contr.pct <- matrix( grp.pctg.cont, ncol = 1, dimnames = list( levels( grpvar ), "grp. % contribution" ) )
    attr( grp.contr.pct, "names" ) <- levels( grpvar )
    attr( grp.contr.pct, "var") <- grp.pctg.cont_var
    attr( grp.contr.pct, "statistic") <- "grp. % contribution"
    class( grp.contr.pct ) <- c( "svrepstat" )


  }

  # dimensional decomposition:
  dim.contr <- NULL
  qq.dim.contr <- matrix( NA, ncol = ncol(cen.dep.matrix), nrow = ncol( ww ) )
  dim.contr_var <- NULL
  for ( i in 1:ncol(cen.dep.matrix) ) {

    dim.contr[ i ] <- dimw[i] * ( sum( ws * cen.dep.matrix[ , i ] ) / sum( ws * cen.depr.sums ) )

    qq.dim.contr[ , i ] <- apply( ww, 2, function(wi) {
      dimw[i] * ( sum( ws * cen.dep.matrix[ , i ] ) / sum( ws * cen.depr.sums ) )
    } )

  }
  dim.contr_var <- survey::svrVar( qq.dim.contr, design$scale, design$rscales, mse = design$mse, coef = dim.contr )

  dim.result <- dim.contr
  attr( dim.result, "names" ) <- colnames( ach.matrix )
  attr( dim.result, "var") <- dim.contr_var
  attr( dim.result, "statistic") <- "dim. % contribution"
  class( dim.result ) <- c( "svrepstat" )

  # build result matrices:

  # set up result object:
  if ( is.null( levels( grpvar ) ) ) {

    rval <- list( overall, raw.hc.ratio, cen.hc.ratio, dim.result )
    names( rval ) <- list( "overall", "raw headcount ratio", "censored headcount ratio", "percentual contribution per dimension" )

  } else {

    rval <- list( overall, raw.hc.ratio, cen.hc.ratio, dim.result, grp.contr.estimate, grp.contr.pct )
    names( rval ) <- list( "overall", "raw headcount ratio", "censored headcount ratio", "percentual contribution per dimension", "subgroup alkire-foster estimates", "percentual contribution per subgroup" )

  }

  return( rval )

}

#' @rdname svyafcdec
#' @export
svyafcdec.DBIsvydesign <-
  function (formula, subgroup = ~1 , design, ...) {

    design$variables <-
      cbind(
        getvars(formula, design$db$connection,design$db$tablename, updates = design$updates, subset = design$subset),
        getvars(subgroup, design$db$connection, design$db$tablename,updates = design$updates, subset = design$subset)
      )

    NextMethod("svyafcdec", design)
  }

