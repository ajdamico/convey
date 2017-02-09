#'
#' Allison-Foster (2004) S-curve
#'
#' Plot the S-curve, a graph for inequality analysis of ordinal variables.
#'
#' @param formula a formula specifying the variable. Variable has to be ordered factor.
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param curve should the S-curve be plotted? Defaults to \code{TRUE}.
#' @param na.rm Should cases with missing values be dropped?
#' @param ... future expansion
#'
#' @details you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
#'
#' @return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of the statistic.
#'
#' @author Guilherme Jacob
#'
#' @seealso \code{\link{svyfgt}}
#'
#' @references R. Andrew Allison and James E. Foster (2007). Measuring health inequality using qualitative data.
#' Journal of Health Economics, v. 23, n. 3, May 2004, pp. 505-524.
#'
#' @keywords survey
#'
#' @examples
#'
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
#' svyScurve(~eqincome+hy050n, design = dbd_eusilc, alpha = .5, beta = .5, na.rm = FALSE )
#' svyScurve(~eqincome+hy050n, design = dbd_eusilc, alpha = .5, beta = 0, na.rm = FALSE )
#' svyScurve(~eqincome+hy050n, design = dbd_eusilc, alpha = .5, beta = -.5, na.rm = FALSE )
#' svyScurve(~eqincome+hy050n, design = dbd_eusilc, alpha = .5, beta = -1, na.rm = FALSE )
#' svyScurve(~eqincome+hy050n, design = dbd_eusilc, alpha = .5, beta = -2, na.rm = FALSE )
#'
#' # subsetting:
#' sub_dbd_eusilc <- subset( dbd_eusilc, db040 == "Styria")
#' svyScurve(~eqincome+hy050n, design = sub_dbd_eusilc, alpha = .5, beta = .5, na.rm = FALSE )
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' dbDisconnect( conn , shutdown = TRUE )
#'
#' }
#'
#' @export
svyScurve <- function( formula, design, ...) {

  UseMethod("svyScurve", design)

}

#' @rdname svyScurve
#' @export
svyScurve.survey.design <- function( formula, design, add = FALSE, line.color = "black" , alpha = .05 , na.rm = TRUE , ...) {

  if (is.null(attr(design, "full_design"))) stop("you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svydesign() function.")

  mvar <- model.frame(formula, design$variables, na.action = na.pass)[,]

  if ( !( c( "ordered" ) %in% class( mvar ) ) ) {
    stop( "This function is only applicable to variables of type 'ordered'." )
  }

  w <- 1/design$prob

  if ( na.rm ) {
    nas <- is.na( mvar )
    design <- design[nas == 0, ]
    w <- 1/design$prob
  }

  # get original order
  ordvar <- order( mvar )

  # reorder vectors
  mvar <- mvar [ ordvar ]
  w <- w [ ordvar ]

  # remove observations with zero weight
  mvar <- mvar [ w > 0 ]
  w <- w [ w > 0 ]

  # Cumulative distribution function per category
  cdf <- cumsum( w ) / sum( w )

  # matrix of categories
  cat.ind <- matrix( 0 , nrow = length(mvar) , ncol = length( levels( mvar ) ) )
  for ( category in seq_along( levels(mvar) ) ) {
     cat.ind[ , category ] <- 1*( mvar <= levels(mvar)[ category ] )
  }

  # median category
  median.category <- as.character( mvar[ min( which.min( abs( cdf - .5 ) ) ) ] )
  cat_i <- which.max( levels( mvar ) == median.category )

  # population total
  pop_t <- list( value = sum( w[ w > 0 ] ) , lin = 1*( w > 0 )[ w > 0 ] )

  # calculate S-curve
  y.coord <- NULL
  y.coord_lin <- matrix( NA , nrow = length(mvar) , ncol = length( levels( mvar ) ) )
  for ( category in seq_along( levels(mvar) ) ) {

    p_cat <- list( value = sum( w[ w > 0 ] * cat.ind[ w > 0 , category ] ) , lin = cat.ind[ w > 0 , category ] )
    lin.var <- contrastinf( quote( p_cat / pop_t ) , list( p_cat = p_cat, pop_t = pop_t  ) )
    y.coord[ category ] <- lin.var[[1]]
    y.coord_lin[ , category ] <- lin.var[[2]]
    rm( p_cat , lin.var )

  }

  # calculate variance
  variance <- NULL
  u_i <- matrix( 1 / design$prob , nrow = length( design$prob ) , ncol = length( levels( mvar ) ) )
  u_i[ 1/design$prob > 0 , ] <- y.coord_lin
  y.coord_lin <- u_i ; rm ( u_i )
  variance <- survey::svyrecvar( y.coord_lin / design$prob , design$cluster, design$strata, design$fpc, postStrata = design$postStrata )

  CI.L <- y.coord - sqrt(diag(variance)) * qnorm( alpha, mean = 0, sd = 1, lower.tail = FALSE )
  CI.U <- y.coord + sqrt(diag(variance)) * qnorm( alpha, mean = 0, sd = 1, lower.tail = FALSE )

  # S-curve

  if (!add) {

    plot( x = y.coord ,
          y = ifelse( y.coord < .5 , sum( y.coord < .5 ) - seq_along( y.coord ) + (cat_i - 1) , seq_along( y.coord ) - (cat_i - 1) ) ,
          type = "s" , xaxt = "n" , yaxt = "n" , xlim = c( 0 , 1 ) , xlab = "CDF" , ylab = "Categories" , col = line.color )
    axis( 1, at = seq( 0 , 1 , .1) , labels= paste0( 100*seq( 0 , 1 , .1) , "%" ) )
    axis( 2, at = seq_along( y.coord [ y.coord < .5 ] ) + 1, labels= rev( levels( mvar ) [ y.coord < .5 ] ) )
    axis( 4, at = seq_along( y.coord [ y.coord >= .5 ] ), labels= levels( mvar ) [ y.coord >= .5 ] )

  } else {
    lines( x = y.coord ,
          y = ifelse( y.coord < .5 , sum( y.coord < .5 ) - seq_along( y.coord ) + (cat_i - 1) , seq_along( y.coord ) - (cat_i - 1) ) ,
          type = "s" , col = line.color )
  }

return( median.category)

}

#' @rdname svyScurve
#' @export
svyScurve.svyrep.design <- function( formula, design, add = FALSE, line.color = "black" , alpha = .05 , na.rm = TRUE , ... ) {


}

#' @rdname svyScurve
#' @export
svyScurve.DBIsvydesign <-
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

    NextMethod("svyScurve", design)
  }
