#' FGT indices decomposition (EXPERIMENTAL)
#'
#' Estimate the Foster et al. (1984) poverty class and its components
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param g If g=1 estimates the average normalised poverty gap, and if g=2 estimates the average squared normalised poverty gap.
#' @param abs_thresh poverty threshold value if type_thresh is "abs"
#' @param thresh return the poverty threshold value
#' @param na.rm Should cases with missing values be dropped?
#' @param ... passed to \code{svyarpr} and \code{svyarpt}
#'
#' @details you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
#'
#' @return Object of class "\code{cvydstat}", with estimates for the FGT(2), FGT(0), FGT(1), income gap ratio and GEI(income gaps; epsilon = g) with a "\code{var}" attribute giving the variance-covariance matrix.
#' A "\code{statistic}" attribute giving the name of the statistic.
#'
#' @author Guilherme Jacob, Djalma Pessoa and Anthony Damico
#'
#' @note This function is experimental and is subject to change in later versions.
#'
#' @seealso \code{\link{svyfgt},\link{svyfgt},\link{svyfgt}}
#'
#' @references Oihana Aristondo, Cassilda Lasso De La vega and Ana Urrutia (2010).
#' A new multiplicative decomposition for the Foster-Greer-Thorbecke poverty indices.
#' \emph{Bulletin of Economic Research}, Vol.62, No.3, pp. 259-267.
#' University of Wisconsin. URL \url{https://www.irp.wisc.edu/publications/dps/pdfs/dp568.pdf}.
#'
#' James Foster, Joel Greer and Erik Thorbecke (1984). A class of decomposable poverty measures.
#' \emph{Econometrica}, Vol.52, No.3, pp. 761-766.
#'
#' Guillaume Osier (2009). Variance estimation for complex indicators
#' of poverty and inequality. \emph{Journal of the European Survey Research
#' Association}, Vol.3, No.3, pp. 167-195,
#' ISSN 1864-3361, URL \url{http://ojs.ub.uni-konstanz.de/srm/article/view/369}.
#'
#' Jean-Claude Deville (1999). Variance estimation for complex statistics and estimators:
#' linearization and residual techniques. Survey Methodology, 25, 193-203,
#' URL \url{http://www5.statcan.gc.ca/bsolc/olc-cel/olc-cel?lang=eng&catno=12-001-X19990024882}.
#'
#' @keywords survey
#'
#' @examples
#' library(survey)
#' library(vardpoor)
#' data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )
#'
#' # linearized design
#'
#' des_eusilc <- svydesign( ids = ~rb030 , strata = ~db040 ,  weights = ~rb050 , data = eusilc )
#' des_eusilc <- convey_prep( des_eusilc )
#'
#' # replicate-weighted design
#' des_eusilc_rep <- as.svrepdesign( des_eusilc , type = "bootstrap" )
#' des_eusilc_rep <- convey_prep( des_eusilc_rep )
#'
#' # absolute poverty threshold
#' svyfgtdec(~eqincome, des_eusilc, g = 1, abs_thresh=10000)
#'
#' #  using svrep.design:
#' # absolute poverty threshold
#' svyfgtdec(~eqincome, des_eusilc_rep, g = 1, abs_thresh=10000)
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
#'
#' dbd_eusilc <- convey_prep( dbd_eusilc )
#'
#' # absolute poverty threshold
#' svyfgtdec(~eqincome, dbd_eusilc , g = 1, abs_thresh=10000)
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' dbDisconnect( conn , shutdown = TRUE )
#'
#' }
#'
#' @export
svyfgtdec <-
  function(formula, design, ...) {

    warning("The svyfgtdec function is experimental and is subject to changes in later versions.")

    # if( 'type_thresh' %in% names( list( ... ) ) && !( list(...)[["type_thresh"]] %in% c( 'abs' ) ) ) stop( 'type_thresh= must be "abs". See ?svyfgtdec for more detail.' )
    if( !( 'abs_thresh' %in% names( list(...) ) ) ) stop( "abs_thresh= parameter must be specified." )

    if( !( 'g' %in% names(list(...)) ) ) stop( "g= parameter must be specified" )
    if( !is.na( list(...)[["g"]] ) && !( ( list(...)[["g"]] == 0 ) | ( list(...)[["g"]] >= 1 ) ) ) stop( "tis decomposition is defined for g >= 1." )

    if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

    UseMethod("svyfgtdec", design)

  }

#' @rdname svyfgtdec
#' @export
svyfgtdec.survey.design <-
  function(formula, design, g=NULL, abs_thresh=NULL, na.rm = FALSE, thresh = FALSE, ...){

    # domain
    incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

    if(na.rm){
      nas<-is.na(incvar)
      design<-design[!nas,]
      if (length(nas) > length(design$prob))incvar <- incvar[!nas] else incvar[nas] <- 0
    } else {
      nas <- rep(0 , nrow( design ) )
    }

    w <- 1/design$prob

    N <- sum(w)

    # linearization
    th <- abs_thresh

    fgt0 <- svyfgt( formula , design , g = 0 , type_thresh = "abs" , abs_thresh = abs_thresh )
    fgt1 <- svyfgt( formula , design , g = 1 , type_thresh = "abs" , abs_thresh = abs_thresh )
    fgtg <- svyfgt( formula , design , g = g , type_thresh = "abs" , abs_thresh = abs_thresh )

    # income gap ratio
    fgt0 <- list( value = fgt0[[1]], lin = attr( fgt0 , "lin" ) )
    fgt1 <- list( value = fgt1[[1]], lin = attr( fgt1 , "lin" ) )

    list_all <- list( fgt0 = fgt0 , fgt1 = fgt1 )
    igr <- convey::contrastinf( quote( fgt1 / fgt0 ) , list_all )
    rm(list_all)

    # generalized entropy index of incomes among the poor
    stopifnot( length( incvar ) == length( design$prob ) )
    design$variables$income_gaps <- ifelse( incvar < th , 1 - incvar / th  , 0 )
    design_poor <- design[ incvar <= th , ]
    class( design_poor ) <- class( design_poor )[ !( class(design_poor ) %in% "DBIsvydesign" ) ]
    ID <- as.numeric( rownames( design_poor ) )
    gei_poor <- svygei( ~income_gaps , design_poor , epsilon = g )
    if ( length(fgt0$lin) > length(  attr(gei_poor , "lin" ) ) ) {
      lin <- rep( 0 , length(fgt0$lin) )
      lin[ ID ] <- attr(gei_poor , "lin" )
      attr(gei_poor , "lin" ) <- lin ; rm(lin , design_poor )
    }
    #L_poor <- list( value = L_poor[[1]] , lin = ifelse( incvec <= th$value , attr(L_poor , "lin" ) , 0 ) )
    gei_poor <- list( value = gei_poor[[1]] , lin = attr( gei_poor , "lin" ) )


    test.estimate <- convey::contrastinf( quote( fgt0 * igr^g * ( 1 + ( g^2 - g ) * gei_poor ) ) , list( fgt0 = fgt0 , igr = igr , gei_poor = gei_poor , g  = list( value = g , lin = rep( 0 , length(igr$lin) ) ) ) )

    lin.matrix <- cbind(test.estimate$lin, fgt0$lin, fgt1$lin , igr$lin , gei_poor$lin)
    lin.matrix <- as.matrix( lin.matrix )
    colnames(lin.matrix) <- c( paste0("fgt",g), "fgt0", "fgt1" , "igr" , paste0( "gei(poor;epsilon=",g,")" ) )

    if ( length(design$prob) > nrow( lin.matrix ) ) {
      lin.matrix <- apply( lin.matrix , 2 , function(x) { y = 1/design$prob ; y[ y > 0 ] <- x ; return( y )  } )
      lin.matrix <- as.matrix( lin.matrix )
    }
    if ( length(design$prob) < nrow( lin.matrix ) ) {
      lin.matrix <- lin.matrix [ as.numeric(rownames(design) ) , ]
    }

    # stopifnot( abs(sqrt(survey::svyrecvar( test.estimate$lin/design$prob, design$cluster, design$strata, design$fpc, postStrata = design$postStrata)) - sqrt(survey::svyrecvar( attr(watts , "lin") /design$prob, design$cluster, design$strata, design$fpc, postStrata = design$postStrata))) < 10^-10 )

    estimates <- matrix( c( test.estimate$value, fgt0$value, fgt1$value , igr$value , gei_poor$value ), dimnames = list( c( paste0("fgt",g), "fgt0", "fgt1" , "igr" , paste0( "gei(poor;epsilon=",g,")" ) ) ) )[,]
    variance <- survey::svyrecvar( lin.matrix/design$prob , design$cluster, design$strata, design$fpc, postStrata = design$postStrata )

    rval <- list( estimate = estimates )
    names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    attr(rval, "SE") <- sqrt(diag(variance[1:5,1:5]))
    attr(rval, "var") <- variance[1:5,1:5]
    attr(rval, "statistic") <- paste0("fgt", g , " decomposition")
    class(rval) <- c( "cvydstat" , "cvystat" , "svystat" , "svrepstat" )

    rval


  }


#' @rdname svyfgtdec
#' @export
svyfgtdec.svyrep.design <-
  function(formula, design, g=NULL, abs_thresh=NULL, na.rm = FALSE, thresh = FALSE, ...){

    # svyrep design ComputeIndex functions
    ComputeFGT <-
      function( y , w , g , thresh ) {
        y <- y[ w > 0 ]
        w <- w[ w > 0 ]
        N <- sum(w)
        h <- function(y,thresh,g) ( ( ( thresh - y ) / thresh )^g ) * ( y <= thresh )
        sum( w * h( y , thresh , g ) ) / N
      }
    ComputeGEI <-
      function( y , w , epsilon ) {

        y <- y[ w > 0 ]
        w <- w[ w > 0 ]

        if ( epsilon == 0 ) {
          result.est <- -T_fn( y , w , 0 ) / U_fn( y , w , 0 ) + log( U_fn( y , w , 1 ) / U_fn( y , w , 0 ) )
        } else if ( epsilon == 1 ) {
          result.est <- ( T_fn( y , w , 1 ) / U_fn( y , w , 1 ) ) - log( U_fn( y , w , 1 ) / U_fn( y , w , 0 ) )
        } else {
          result.est <- ( epsilon * ( epsilon - 1 ) )^( -1 ) * ( U_fn( y , w , 0 )^( epsilon - 1 ) * U_fn( y , w , 1 )^( -epsilon ) * U_fn( y , w , epsilon ) - 1 )
        }

        result.est

      }

    # domain
    incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

    if(na.rm){
      nas<-is.na(incvar)
      design<-design[!nas,]
      incvar <- incvar[!nas]
    }

    ws <- weights(design, "sampling")
    # poverty threshold
    th <- abs_thresh

    # estimates
    fgt0 <- ComputeFGT(incvar, ws, g = 0 , thresh = th )
    fgt1 <- ComputeFGT(incvar, ws, g = 1 , thresh = th )
    fgtg <- ComputeFGT(incvar, ws, g = g , thresh = th )
    igr <- fgt1/fgt0
    gei_poor <- ComputeGEI( ifelse( incvar < th , 1 - incvar / th , 0 ) , ifelse( incvar < th , ws , 0 ) , epsilon = g )

    ww <- weights(design, "analysis" )

    # get replicates
    qq.fgt0 <- apply( ww, 2, function(wi){ ComputeFGT( incvar, wi, g = 0 , thresh = th ) } )
    qq.fgt1 <- apply( ww, 2, function(wi){ ComputeFGT( incvar, wi, g = 1 , thresh = th ) } )
    qq.fgtg <- apply( ww, 2, function(wi){ ComputeFGT( incvar, wi, g = g , thresh = th ) } )
    qq.igr  <- apply( ww, 2, function(wi){ ComputeFGT( incvar, wi, g = 1 , thresh = th ) / ComputeFGT( incvar, wi, g = 0 , thresh = th ) } )
    qq.gei_poor <- apply( ww, 2, function(wi){ ComputeGEI( ifelse( incvar < th , 1 - incvar / th , 0 ) , ifelse( incvar < th , wi , 0 ) , epsilon = g ) } )

    qq <- cbind( qq.fgtg , qq.fgt0 , qq.fgt1 , qq.igr , qq.gei_poor )
    colnames(qq) <- c( paste0("fgt",g), "fgt0", "fgt1" , "igr" , paste0( "gei(poor;epsilon=",g,")" ) )

    # test.estimate <- fgt0 * ( log( th / mip ) + L_poor )
    # qq.test.estimate <- qq.fgt0 * ( log( th / qq.mip ) + qq.L_poor )

    if (anyNA(qq)) variance <- matrix( NA , ncol = 5 , nrow = 5 , dimnames = list( c( paste0("fgt",g), "fgt0", "fgt1" , "igr" , paste0( "gei(poor;epsilon=",g,")" ) ) , c( paste0("fgt",g), "fgt0", "fgt1" , "igr" , paste0( "gei(poor;epsilon=",g,")" ) ) ) ) else variance <- survey::svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)

    variance <- as.matrix( variance )

    estimates <- matrix( c( fgtg, fgt0, fgt1, igr, gei_poor ), dimnames = list( c( paste0("fgt",g), "fgt0", "fgt1" , "igr" , paste0( "gei(poor;epsilon=",g,")" ) ) ) )[,]

    rval <- list( estimate = estimates )
    names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    attr(rval, "SE") <- sqrt(diag(variance[1:5,1:5]))
    attr(rval, "var") <- variance[1:5,1:5]
    attr(rval, "statistic") <- paste0("fgt", g , " decomposition")
    class(rval) <- c( "cvydstat" , "cvystat" , "svrepstat" , "svystat" )

    rval

  }


#' @rdname svyfgtdec
#' @export
svyfgtdec.DBIsvydesign <-
  function (formula, design, ...) {

    design$variables <- getvars( formula, design$db$connection, design$db$tablename, updates = design$updates, subset = design$subset )

    NextMethod("svyfgtdec", design)
  }

