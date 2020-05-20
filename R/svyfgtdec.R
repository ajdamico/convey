#' FGT indices decomposition (EXPERIMENTAL)
#'
#' Estimate the Foster et al. (1984) poverty class and its components
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param type_thresh type of poverty threshold. If "abs" the threshold is fixed and given the value
#' of abs_thresh; if "relq" it is given by percent times the quantile; if "relm" it is percent times the mean.
#' @param abs_thresh poverty threshold value if type_thresh is "abs"
#' @param g If g=2 estimates the average squared normalised poverty gap. This function is defined for g >= 2 only,
#' @param percent the multiple of the the quantile or mean used in the poverty threshold definition
#' @param quantiles the quantile used used in the poverty threshold definition
#' @param thresh return the poverty threshold value
#' @param na.rm Should cases with missing values be dropped?
#' @param ... additional arguments. Currently not used.
#'
#'
#' @details you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
#'
#' @return Object of class "\code{cvydstat}", with estimates for the FGT(g), FGT(0), FGT(1), income gap ratio and GEI(income gaps; epsilon = g) with a "\code{var}" attribute giving the variance-covariance matrix.
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
#' University of Wisconsin. URL \url{http://dx.doi.org/10.1111/j.1467-8586.2009.00320.x}.
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
#' library(laeken)
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
#' svyfgtdec(~eqincome, des_eusilc, g=2, abs_thresh=10000)
#' # poverty threshold equal to arpt
#' svywattsdec(~eqincome, des_eusilc, g=2, type_thresh= "relq" , thresh = TRUE)
#' # poverty threshold equal to 0.6 times the mean
#' svywattsdec(~eqincome, des_eusilc, g=2, type_thresh= "relm" , thresh = TRUE)
#'
#' # using svrep.design:
#' # absolute poverty threshold
#' svyfgtdec(~eqincome, des_eusilc_rep, g=2, abs_thresh=10000)
#' # poverty threshold equal to arpt
#' svywattsdec(~eqincome, des_eusilc_rep, g=2, type_thresh= "relq" , thresh = TRUE)
#' # poverty threshold equal to 0.6 times the mean
#' svywattsdec(~eqincome, des_eusilc_rep, g=2, type_thresh= "relm" , thresh = TRUE)
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
#'
#' dbd_eusilc <- convey_prep( dbd_eusilc )
#'
#'
#' # absolute poverty threshold
#' svyfgtdec(~eqincome, dbd_eusilc, g=2, abs_thresh=10000)
#' # poverty threshold equal to arpt
#' svywattsdec(~eqincome, dbd_eusilc, g=2, type_thresh= "relq" , thresh = TRUE)
#' # poverty threshold equal to 0.6 times the mean
#' svywattsdec(~eqincome, dbd_eusilc, g=2, type_thresh= "relm" , thresh = TRUE)
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
    if( !is.na( list(...)[["g"]] ) && !( list(...)[["g"]] >= 2 ) ) stop( "this decomposition is defined for g >= 2 only." )

    if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

    UseMethod("svyfgtdec", design)

  }

#' @rdname svyfgtdec
#' @export
svyfgtdec.survey.design <-
  function(formula, design, g, type_thresh="abs",  abs_thresh=NULL, percent = .60, quantiles = .50, na.rm = FALSE, thresh = FALSE, ...){

    if (is.null(attr(design, "full_design"))) stop("you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svydesign() function.")

    if( type_thresh == "abs" & is.null( abs_thresh ) ) stop( "abs_thresh= must be specified when type_thresh='abs'" )

    fgt0 <- svyfgt( formula = formula, design=design, g=0, type_thresh=type_thresh, percent=percent, quantiles=quantiles , abs_thresh=abs_thresh , na.rm=na.rm , thresh = thresh )
    fgt1 <- svyfgt( formula = formula, design=design, g=1, type_thresh=type_thresh, percent=percent, quantiles=quantiles , abs_thresh=abs_thresh , na.rm=na.rm , thresh = thresh )
    fgtg <- svyfgt( formula = formula, design=design, g=g, type_thresh=type_thresh, percent=percent, quantiles=quantiles , abs_thresh=abs_thresh , na.rm=na.rm , thresh = thresh )

    if ( thresh ) thresh.value <- attr( fgt0 , "thresh" )

    # income gap ratio
    fgt0 <- list( value = fgt0[[1]], lin = attr( fgt0 , "lin" ) )
    fgt1 <- list( value = fgt1[[1]], lin = attr( fgt1 , "lin" ) )

    igr <- contrastinf( quote( fgt1 / fgt0 ) , list( fgt0 = fgt0 , fgt1 = fgt1) )

    # generalized entropy index of poverty gaps
    # by residual
    fgtg <- list( value = fgtg[[1]], lin = attr( fgtg , "lin" ) )
    gei_poor <- contrastinf( quote( ( fgtg / ( fgt0 * igr^g ) - 1 ) / ( g^2 - g ) ) , list( fgtg =fgtg , fgt0 = fgt0 , fgt1 = fgt1 , igr = igr , g = list( value = g , lin = rep( 0 , length( igr$lin ) ) ) ) )


    lin.matrix <- cbind( fgtg$lin, fgt0$lin, fgt1$lin , igr$lin , gei_poor$lin)
    lin.matrix <- as.matrix( lin.matrix )
    colnames(lin.matrix) <- c( paste0("fgt",g), "fgt0", "fgt1" , "igr" , paste0( "gei(poor;epsilon=",g,")" ) )

    if ( length(design$prob) > nrow( lin.matrix ) ) {
      lin.matrix <- apply( lin.matrix , 2 , function(x) { y = 1/design$prob ; y[ y > 0 ] <- x ; return( y )  } )
      lin.matrix <- as.matrix( lin.matrix )
    }
    if ( length(design$prob) < nrow( lin.matrix ) ) {
      lin.matrix <- lin.matrix [ as.numeric(rownames(design) ) , ]
    }


    estimates <- matrix( c( fgtg$value, fgt0$value, fgt1$value , igr$value , gei_poor$value ), dimnames = list( c( paste0("fgt",g), "fgt0", "fgt1" , "igr" , paste0( "gei(poor;epsilon=",g,")" ) ) ) )[,]
    variance <- survey::svyrecvar( lin.matrix/design$prob , design$cluster, design$strata, design$fpc, postStrata = design$postStrata )

    rval <- list( estimate = estimates )
    names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    attr(rval, "SE") <- sqrt(diag(variance[1:5,1:5]))
    attr(rval, "var") <- variance[1:5,1:5]
    attr(rval, "statistic") <- paste0("fgt", g , " decomposition")
    if (thresh) attr(rval, "thresh") <- thresh.value
    class(rval) <- c( "cvydstat" , "cvystat" , "svystat" , "svrepstat" )

    rval


  }


#' @rdname svyfgtdec
#' @export
svyfgtdec.svyrep.design <-
  function(formula, design, g, type_thresh="abs",  abs_thresh=NULL, percent = .60, quantiles = .50, na.rm = FALSE, thresh = FALSE, ...){

    if (is.null(attr(design, "full_design"))) stop("you must run the ?convey_prep function on your replicate-weighted survey design object immediately after creating it with the svrepdesign() function.")

    if( type_thresh == "abs" & is.null( abs_thresh ) ) stop( "abs_thresh= must be specified when type_thresh='abs'" )

    # if the class of the full_design attribute is just a TRUE, then the design is
    # already the full design.  otherwise, pull the full_design from that attribute.
    if ("logical" %in% class(attr(design, "full_design"))) full_design <- design else full_design <- attr(design, "full_design")

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

    df <- model.frame(design)
    incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

    if(na.rm){
      nas<-is.na(incvar)
      design<-design[!nas,]
      df <- model.frame(design)
      incvar <- incvar[!nas]
    }

    ws <- weights(design, "sampling")

    df_full<- model.frame(full_design)
    incvec <- model.frame(formula, full_design$variables, na.action = na.pass)[[1]]

    if(na.rm){
      nas<-is.na(incvec)
      full_design<-full_design[!nas,]
      df_full <- model.frame(full_design)
      incvec <- incvec[!nas]
    }

    wsf <- weights(full_design,"sampling")
    names(incvec) <- names(wsf) <- row.names(df_full)
    ind<- row.names(df)

    # poverty threshold
    if(type_thresh=='relq') th <- percent * computeQuantiles( incvec, wsf, p = quantiles)
    if(type_thresh=='relm') th <- percent*sum(incvec*wsf)/sum(wsf)
    if(type_thresh=='abs') th <- abs_thresh


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
    if (thresh) attr(rval, "thresh") <- th
    class(rval) <- c( "cvydstat" , "cvystat" , "svrepstat" , "svystat" )

    rval

  }


#' @rdname svyfgtdec
#' @export
svyfgtdec.DBIsvydesign <-
  function (formula, design, ...) {

    if (!( "logical" %in% class(attr(design, "full_design"))) ){

      full_design <- attr( design , "full_design" )

      full_design$variables <-
        getvars(
          formula,
          attr( design , "full_design" )$db$connection,
          attr( design , "full_design" )$db$tablename,
          updates = attr( design , "full_design" )$updates,
          subset = attr( design , "full_design" )$subset
        )

      attr( design , "full_design" ) <- full_design

      rm( full_design )

    }

    design$variables <-
      getvars(
        formula,
        design$db$connection,
        design$db$tablename,
        updates = design$updates,
        subset = design$subset
      )

    NextMethod("svyfgtdec", design)
  }

