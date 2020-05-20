#' FGT measure of poverty
#'
#' Estimate the FGT measure for the cases: \code{alpha=0} headcount ratio and \code{alpha=1} poverty gap index.
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param type_thresh type of poverty threshold. If "abs" the threshold is fixed and given the value
#' of abs_thresh; if "relq" it is given by percent times the quantile; if "relm" it is percent times the mean.
#' @param abs_thresh poverty threshold value if type_thresh is "abs"
#' @param g If g=0 estimates the headcount ratio; If g=1 estimates the average normalised poverty gap, and if g=2 estimates the average squared normalised poverty gap
#' @param percent the multiple of the the quantile or mean used in the poverty threshold definition
#' @param quantiles the quantile used used in the poverty threshold definition
#' @param thresh return the poverty threshold value
#' @param na.rm Should cases with missing values be dropped?
#' @param ... passed to \code{svyarpr} and \code{svyarpt}
#'
#' @details you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
#'
#' @return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of the statistic.
#'
#' @author Djalma Pessoa and Anthony Damico
#'
#' @seealso \code{\link{svyarpt}}
#'
#' @references James Foster, Joel Greer and Erik Thorbecke (1984). A class of decomposable poverty measures.
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
#' # headcount ratio, poverty threshold fixed
#' svyfgt(~eqincome, des_eusilc, g=0,  abs_thresh=10000)
#' # poverty gap index, poverty threshold fixed
#' svyfgt(~eqincome, des_eusilc, g=1,  abs_thresh=10000)
#' # headcount ratio, poverty threshold equal to arpt
#' svyfgt(~eqincome, des_eusilc, g=0, type_thresh= "relq" , thresh = TRUE)
#' # poverty gap index, poverty threshold equal to arpt
#' svyfgt(~eqincome, des_eusilc, g=1, type_thresh= "relq", thresh = TRUE)
#' # headcount ratio, poverty threshold equal to .6 times the mean
#' svyfgt(~eqincome, des_eusilc, g=0, type_thresh= "relm", thresh = TRUE)
#' # poverty gap index, poverty threshold equal to 0.6 times the mean
#' svyfgt(~eqincome, des_eusilc, g=1, type_thresh= "relm" , thresh = TRUE)
#'
#' #  using svrep.design:
#' # headcount ratio, poverty threshold fixed
#' svyfgt(~eqincome, des_eusilc_rep, g=0,  abs_thresh=10000)
#' # poverty gap index, poverty threshold fixed
#' svyfgt(~eqincome, des_eusilc, g=1,  abs_thresh=10000)
#' # headcount ratio, poverty threshold equal to arpt
#' svyfgt(~eqincome, des_eusilc_rep, g=0, type_thresh= "relq" , thresh = TRUE)
#' # poverty gap index, poverty threshold equal to arpt
#' svyfgt(~eqincome, des_eusilc, g=1, type_thresh= "relq", thresh = TRUE)
#' # headcount ratio, poverty threshold equal to .6 times the mean
#' svyfgt(~eqincome, des_eusilc_rep, g=0, type_thresh= "relm" , thresh = TRUE)
#' # poverty gap index, poverty threshold equal to 0.6 times the mean
#' svyfgt(~eqincome, des_eusilc_rep, g=1, type_thresh= "relm", thresh = TRUE)
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
#' # headcount ratio, poverty threshold fixed
#' svyfgt(~eqincome, dbd_eusilc, g=0, abs_thresh=10000)
#' # poverty gap index, poverty threshold fixed
#' svyfgt(~eqincome, dbd_eusilc, g=1, abs_thresh=10000)
#' # headcount ratio, poverty threshold equal to arpt
#' svyfgt(~eqincome, dbd_eusilc, g=0, type_thresh= "relq", thresh = TRUE)
#' # poverty gap index, poverty threshold equal to arpt
#' svyfgt(~eqincome, dbd_eusilc, g=1, type_thresh= "relq")
#' # headcount ratio, poverty threshold equal to .6 times the mean
#' svyfgt(~eqincome, dbd_eusilc, g=0, type_thresh= "relm")
#' # poverty gap index, poverty threshold equal to 0.6 times the mean
#' svyfgt(~eqincome, dbd_eusilc, g=1, type_thresh= "relm")
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' dbDisconnect( conn , shutdown = TRUE )
#'
#' }
#'
#' @export
svyfgt <-
  function(formula, design,  ...) {

    if( !( 'g' %in% names(list(...)) ) ) stop( "g= parameter must be specified" )

    if( !is.na( list(...)[["g"]] ) && !( ( list(...)[["g"]] == 0 ) | ( list(...)[["g"]] >= 1 ) ) ) stop( "g= must be 0 to estimate the headcount ratio or >=1 to estimate the poverty index" )

    if( 'type_thresh' %in% names( list( ... ) ) && !( list(...)[["type_thresh"]] %in% c( 'relq' , 'abs' , 'relm' ) ) ) stop( 'type_thresh= must be "relq" "relm" or "abs".  see ?svyfgt for more detail.' )

    if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

    UseMethod("svyfgt", design)

  }

#' @rdname svyfgt
#' @export
svyfgt.survey.design <-
  function(formula, design, g, type_thresh="abs",  abs_thresh=NULL, percent = .60, quantiles = .50, na.rm = FALSE, thresh = FALSE, ...){

    if (is.null(attr(design, "full_design"))) stop("you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svydesign() function.")

    if( type_thresh == "abs" & is.null( abs_thresh ) ) stop( "abs_thresh= must be specified when type_thresh='abs'" )

    # if the class of the full_design attribute is just a TRUE, then the design is
    # already the full design.  otherwise, pull the full_design from that attribute.
    if ("logical" %in% class(attr(design, "full_design"))) full_design <- design else full_design <- attr(design, "full_design")


    #  survey design h function
    h <- function( y , thresh , g ) ( ( ( thresh - y ) / thresh )^g ) * ( y <= thresh )

    # ht function
    ht <- function( y , thresh , g ) ( g * ( ( ( thresh - y ) / thresh )^( g - 1 ) ) * ( y / ( thresh^2 ) ) ) * ( y <= thresh )

    # domain
    incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

    if(na.rm){
      nas<-is.na(incvar)
      design<-design[!nas,]
      if (length(nas) > length(design$prob))incvar <- incvar[!nas] else incvar[nas] <- 0
    }

    w <- 1/design$prob

    if( is.null( names( design$prob ) ) ) ind <- as.character( seq( length( design$prob ) ) ) else ind <- names(design$prob)

    N <- sum(w)

    # if the class of the full_design attribute is just a TRUE, then the design is
    # already the full design.  otherwise, pull the full_design from that attribute.
    if ("logical" %in% class(attr(design, "full_design"))) full_design <- design else full_design <- attr(design, "full_design")

    incvec <- model.frame(formula, full_design$variables, na.action = na.pass)[[1]]

    if(na.rm){
      nas<-is.na(incvec)
      full_design<-full_design[!nas,]
      if (length(nas) > length(full_design$prob)) incvec <- incvec[!nas] else incvec[nas] <- 0
    }

    wf <- 1/full_design$prob

    if( is.null( names( full_design$prob ) ) ) ncom <- as.character( seq( length( full_design$prob ) ) ) else ncom <- names(full_design$prob)

    htot <- h_fun(incvec, wf)
    if (sum(1/design$prob==0) > 0) ID <- 1*(1/design$prob!=0) else ID <- 1 * ( ncom %in% ind )


    # linearization

    if( type_thresh == 'relq' ){

      ARPT <- svyarpt(formula = formula, full_design, quantiles=quantiles, percent=percent,  na.rm=na.rm, ...)
      th <- coef(ARPT)
      arptlin <- attr(ARPT, "lin")
      rval <- sum(w*h(incvar,th,g))/N
      ahat <- sum(w*ht(incvar,th,g))/N

      if( g == 0 ){

        ARPR <- svyarpr(formula = formula, design, quantiles=quantiles, percent=percent,  na.rm=na.rm, ...)
        fgtlin <- attr(ARPR,"lin")

      } else fgtlin <-ID*( h( incvec , th , g ) - rval ) / N + ( ahat * arptlin )

    }

    if( type_thresh == 'relm'){

      # thresh for the whole population
      th <- percent*sum(incvec*wf)/sum(wf)
      rval <- sum(w*h(incvar,th,g))/N
      ahat <- sum(w*ht(incvar,th,g))/N

      if( g == 0 ){

        Fprime <- densfun(formula=formula, design = design, x= th, FUN = "F", na.rm = na.rm )
        fgtlin<- ID*(h(incvec,th,g)-rval + Fprime*(incvec-th))/N

      } else fgtlin <-ID*( h( incvec , th , g ) - rval + ( ( percent * incvec ) - th ) * ahat ) / N

    }

    if( type_thresh == 'abs' ){

      th <- abs_thresh

      rval <- sum( w*h( incvar , th , g ) ) / N

      fgtlin <- ID*( h( incvec , th , g ) - rval ) / N

    }

    variance <- survey::svyrecvar(fgtlin/full_design$prob, full_design$cluster, full_design$strata, full_design$fpc, postStrata = full_design$postStrata)



    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- c( "cvystat" , "svystat" )
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- paste0("fgt",g)
    attr(rval, "lin") <- fgtlin
    if(thresh) attr(rval, "thresh") <- th
    rval

  }



#' @rdname svyfgt
#' @export
svyfgt.svyrep.design <-
  function(formula, design, g, type_thresh="abs", abs_thresh=NULL, percent = .60, quantiles = .50, na.rm = FALSE, thresh = FALSE,...) {

    if (is.null(attr(design, "full_design"))) stop("you must run the ?convey_prep function on your replicate-weighted survey design object immediately after creating it with the svrepdesign() function.")

    if( type_thresh == "abs" & is.null( abs_thresh ) ) stop( "abs_thresh= must be specified when type_thresh='abs'" )

    # if the class of the full_design attribute is just a TRUE, then the design is
    # already the full design.  otherwise, pull the full_design from that attribute.
    if ("logical" %in% class(attr(design, "full_design"))) full_design <- design else full_design <- attr(design, "full_design")

    # svyrep design h function
    h <- function(y,thresh,g) ( ( ( thresh - y ) / thresh )^g ) * ( y <= thresh )

    # svyrep design ComputeFGT function
    ComputeFGT <-
      function(y, w, thresh, g){
        N <- sum(w)
        sum( w * h( y , thresh , g ) ) / N
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


    rval <- ComputeFGT(incvar, ws, g = g, th)

    wwf <- weights(full_design, "analysis")

    qq <-
      apply(wwf, 2, function(wi){
        names(wi)<- row.names(df_full)
        wd<-wi[ind]
        incd <- incvec[ind]
        ComputeFGT(incd, wd, g = g, th)}
      )
    if (anyNA(qq))variance <- NA else variance <- survey::svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)

    variance <- as.matrix( variance )

    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- c( "cvystat" , "svrepstat" )
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- paste0("fgt",g)
    attr(rval, "lin") <- NA
    if(thresh) attr(rval, "thresh") <- th
    rval
  }

#' @rdname svyfgt
#' @export
svyfgt.DBIsvydesign <-
  function (formula, design, ...){

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

    NextMethod("svyfgt", design)
  }

