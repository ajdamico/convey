#' Sen (1976) poverty index
#'
#' Estimate the Sen (1976) poverty measure.
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param type_thresh type of poverty threshold. If "abs" the threshold is fixed and given the value
#' of abs_thresh; if "relq" it is given by percent times the quantile; if "relm" it is percent times the mean.
#' @param abs_thresh poverty threshold value if type_thresh is "abs"
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
#' @author Guilherme Jacob, Djalma Pessoa and Anthony Damico
#'
#' @seealso \code{\link{svyarpt}}
#'
#' @references Amartya Sen (2009). Poverty: An Ordinal Approach to Measurement.
#' \emph{Econometric}, v. 44, n. 3, pp. 219-231.
#' URL \url{http://www.jstor.org/stable/1912718}.
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
#' # headcount ratio, poverty threshold fixed
#' svysen(~eqincome, des_eusilc, g=0,  abs_thresh=10000)
#' # poverty gap index, poverty threshold fixed
#' svysen(~eqincome, des_eusilc, g=1,  abs_thresh=10000)
#' # headcount ratio, poverty threshold equal to arpt
#' svysen(~eqincome, des_eusilc, g=0, type_thresh= "relq" , thresh = TRUE)
#' # poverty gap index, poverty threshold equal to arpt
#' svysen(~eqincome, des_eusilc, g=1, type_thresh= "relq", thresh = TRUE)
#' # headcount ratio, poverty threshold equal to .6 times the mean
#' svysen(~eqincome, des_eusilc, g=0, type_thresh= "relm", thresh = TRUE)
#' # poverty gap index, poverty threshold equal to 0.6 times the mean
#' svysen(~eqincome, des_eusilc, g=1, type_thresh= "relm" , thresh = TRUE)
#'
#' #  using svrep.design:
#' # headcount ratio, poverty threshold fixed
#' svysen(~eqincome, des_eusilc_rep, g=0,  abs_thresh=10000)
#' # poverty gap index, poverty threshold fixed
#' svysen(~eqincome, des_eusilc, g=1,  abs_thresh=10000)
#' # headcount ratio, poverty threshold equal to arpt
#' svysen(~eqincome, des_eusilc_rep, g=0, type_thresh= "relq" , thresh = TRUE)
#' # poverty gap index, poverty threshold equal to arpt
#' svysen(~eqincome, des_eusilc, g=1, type_thresh= "relq", thresh = TRUE)
#' # headcount ratio, poverty threshold equal to .6 times the mean
#' svysen(~eqincome, des_eusilc_rep, g=0, type_thresh= "relm" , thresh = TRUE)
#' # poverty gap index, poverty threshold equal to 0.6 times the mean
#' svysen(~eqincome, des_eusilc_rep, g=1, type_thresh= "relm", thresh = TRUE)
#'
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
#' # headcount ratio, poverty threshold fixed
#' svysen(~eqincome, dbd_eusilc, g=0, abs_thresh=10000)
#' # poverty gap index, poverty threshold fixed
#' svysen(~eqincome, dbd_eusilc, g=1, abs_thresh=10000)
#' # headcount ratio, poverty threshold equal to arpt
#' svysen(~eqincome, dbd_eusilc, g=0, type_thresh= "relq", thresh = TRUE)
#' # poverty gap index, poverty threshold equal to arpt
#' svysen(~eqincome, dbd_eusilc, g=1, type_thresh= "relq")
#' # headcount ratio, poverty threshold equal to .6 times the mean
#' svysen(~eqincome, dbd_eusilc, g=0, type_thresh= "relm")
#' # poverty gap index, poverty threshold equal to 0.6 times the mean
#' svysen(~eqincome, dbd_eusilc, g=1, type_thresh= "relm")
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' dbDisconnect( conn , shutdown = TRUE )
#'
#' }
#'
#' @export
svysen <-
  function(formula, design,  ...) {

    if( !( 'g' %in% names(list(...)) ) ) stop( "g= parameter must be specified" )

    if( !is.na( list(...)[["g"]] ) && !( ( list(...)[["g"]] == 0 ) | ( list(...)[["g"]] >= 1 ) ) ) stop( "g= must be 0 to estimate the headcount ratio or >=1 to estimate the poverty index" )

    if( 'type_thresh' %in% names( list( ... ) ) && !( list(...)[["type_thresh"]] %in% c( 'relq' , 'abs' , 'relm' ) ) ) stop( 'type_thresh= must be "relq" "relm" or "abs".  see ?svysen for more detail.' )

    if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

    UseMethod("svysen", design)

  }

#' @rdname svysen
#' @export
svysen.survey.design <-
  function(formula, design, type_thresh="abs",  abs_thresh=NULL, percent = .60, quantiles = .50, na.rm = FALSE, thresh = FALSE, ...){

    if (is.null(attr(design, "full_design"))) stop("you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svydesign() function.")

    if( type_thresh == "abs" & is.null( abs_thresh ) ) stop( "abs_thresh= must be specified when type_thresh='abs'" )

    # if the class of the full_design attribute is just a TRUE, then the design is
    # already the full design.  otherwise, pull the full_design from that attribute.
    if ("logical" %in% class(attr(design, "full_design"))) full_design <- design else full_design <- attr(design, "full_design")

    # FGT(0)
    rval.fgt0 <- convey::svyfgt( formula = formula, design, type_thresh=type_thresh,  abs_thresh=abs_thresh, g = 0, percent = percent, quantiles = quantiles, na.rm = na.rm, thresh = thresh )
    fgt0 <- NULL
    fgt0$value <- coef( rval.fgt0 )[[1]]
    fgt0$lin <- attr( rval.fgt0, "lin" )
    rm( rval.fgt0 )

    # FGT(1)
    rval.fgt1 <- convey::svyfgt( formula = formula, design, type_thresh=type_thresh,  abs_thresh=abs_thresh, g = 1, percent = percent, quantiles = quantiles, na.rm = na.rm, thresh = thresh )
    fgt1 <- NULL
    fgt1$value <- coef( rval.fgt1 )[[1]]
    fgt1$lin <- attr( rval.fgt1, "lin" )
    rm( rval.fgt1 )

    # Gini index of poor incomes

    if( type_thresh == 'relq' ){

      ARPT <- svyarpt(formula = formula, full_design, quantiles=quantiles, percent=percent,  na.rm=na.rm )
      th <- coef(ARPT)[[1]]

      incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]
      poor.design <- design[ incvar <= th , ]

      rval.gini <- svygini( formula = formula, design = poor.design, na.rm = na.rm )

      gini <- NULL
      gini$value <- coef(rval.gini)[[1]]
      gini$lin <- 1 / design$prob
      gini$lin[ incvar <= th ] <- attr( rval.gini, "lin" )
      gini$lin[ incvar > th ] = 0
      rm( rval.gini, poor.design )

      sen <- contrastinf( quote( fgt0 * gini + fgt1 * ( 1 - gini) ), list( fgt0 = fgt0, fgt1 = fgt1, gini = gini ) )

    }

    if( type_thresh == 'relm'){

      # thresh for the whole population
      th <- percent*svymean( x = formula, design = full_design, na.rm = na.rm )[[1]]
      incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]
      poor.design <- design[ incvar <= th , ]

      rval.gini <- svygini( formula = formula, design = poor.design, na.rm = na.rm )

      gini <- NULL
      gini$value <- coef(rval.gini)[[1]]
      gini$lin <- 1 / design$prob
      gini$lin[ incvar <= th ] <- attr( rval.gini, "lin" )
      gini$lin[ incvar > th ] = 0
      rm( rval.gini, poor.design )

      sen <- contrastinf( quote( fgt0 * gini + fgt1 * ( 1 - gini) ), list( fgt0 = fgt0, fgt1 = fgt1, gini = gini ) )

    }

    if( type_thresh == 'abs' ){

      th <- abs_thresh
      incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]
      poor.design <- design[ incvar <= th , ]

      rval.gini <- svygini( formula = formula, design = poor.design, na.rm = na.rm )

      gini <- NULL
      gini$value <- coef(rval.gini)[[1]]
      gini$lin <- 1 / design$prob
      gini$lin[ incvar <= th ] <- attr( rval.gini, "lin" )
      gini$lin[ incvar > th ] = 0
      rm( rval.gini, poor.design )

      sen <- contrastinf( quote( fgt0 * gini + fgt1 * ( 1 - gini) ), list( fgt0 = fgt0, fgt1 = fgt1, gini = gini ) )

    }

    variance <- survey::svyrecvar(sen$lin/full_design$prob, full_design$cluster, full_design$strata, full_design$fpc, postStrata = full_design$postStrata)

    rval <- sen$value
    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- c( "cvystat" , "svystat" )
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "sen poverty index"
    if(thresh) attr(rval, "thresh") <- th

    rval

  }



#' @rdname svysen
#' @export
svysen.svyrep.design <-
  function(formula, design, g, type_thresh="abs", abs_thresh=NULL, percent = .60, quantiles = .50, na.rm = FALSE, thresh = FALSE,...) {

    if (is.null(attr(design, "full_design"))) stop("you must run the ?convey_prep function on your replicate-weighted survey design object immediately after creating it with the svrepdesign() function.")

    if( type_thresh == "abs" & is.null( abs_thresh ) ) stop( "abs_thresh= must be specified when type_thresh='abs'" )

    # if the class of the full_design attribute is just a TRUE, then the design is
    # already the full design.  otherwise, pull the full_design from that attribute.
    if ("logical" %in% class(attr(design, "full_design")))
      full_design <- design else full_design <- attr(design, "full_design")

      # svyrep design h function
      h <- function(y,thresh,g) ( ( ( thresh - y ) / thresh )^g ) * ( y <= thresh )

      # svyrep design ComputeFGT function
      ComputeFGT <-
        function(y, w, thresh, g){
          N <- sum(w)
          sum( w * h( y , thresh , g ) ) / N
        }

      # svyrep design ComputeGini function
      ComputeGini <-
        function(x, w) {
          w <- w[order(x)]
          x <- x[order(x)]
          N <- sum(w)
          n <- length(x)
          big_t <- sum(x * w)
          r <- cumsum(w)
          Num <- sum((2 * r - 1) * x * w)
          Den <- N * big_t
          (Num/Den) - 1
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


      fgt0 <- ComputeFGT(incvar, ws, g = 0, th)
      fgt1 <- ComputeFGT(incvar, ws, g = 1, th)
      gini <- ComputeGini( incvar, ws * ( incvar <= th ) )

      sen <- (fgt0 * gini) + fgt1 * ( 1- gini )

      wwf <- weights(full_design, "analysis")

      qq.fgt0 <-
        apply(wwf, 2, function(wi){
          names(wi)<- row.names(df_full)
          wd<-wi[ind]
          incd <- incvec[ind]

          if(type_thresh=='relq') qq.th <- percent * computeQuantiles( incvec, wi, p = quantiles )
          if(type_thresh=='relm') qq.th <- percent*sum(incvec*wi)/sum(wi)
          if(type_thresh=='abs') qq.th <- abs_thresh

          ComputeFGT(incd, wd, g = 0, th)}
        )

      qq.fgt1 <-
        apply(wwf, 2, function(wi){
          names(wi)<- row.names(df_full)
          wd<-wi[ind]
          incd <- incvec[ind]

          if(type_thresh=='relq') qq.th <- percent * computeQuantiles( incvec, wi, p = quantiles )
          if(type_thresh=='relm') qq.th <- percent*sum(incvec*wi)/sum(wi)
          if(type_thresh=='abs') qq.th <- abs_thresh

          ComputeFGT(incd, wd, g = 1, th)}
        )

      qq.gini <-
        apply(wwf, 2, function(wi){
          names(wi)<- row.names(df_full)
          wd<-wi[ind]
          incd <- incvec[ind]

          if(type_thresh=='relq') qq.th <- percent * computeQuantiles( incvec, wi, p = quantiles )
          if(type_thresh=='relm') qq.th <- percent*sum(incvec*wi)/sum(wi)
          if(type_thresh=='abs') qq.th <- abs_thresh

          wd<- wd *( incd <= qq.th )
          ComputeGini(incd, wd )}
        )

      qq <- ( qq.fgt0 * qq.gini ) + fgt1 * ( 1 - gini )

      if(anyNA(qq)) {
        variance <- NA
      } else {
        variance <- survey::svrVar( qq, design$scale, design$rscales, mse = design$mse, coef = sen )
        }

      rval <- sen
      names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
      class(rval) <- c( "cvystat" , "svrepstat" )
      attr(rval, "var") <- variance
      attr(rval, "statistic") <- "sen poverty index"
      if(thresh) attr(rval, "thresh") <- th

      return( rval )

  }

#' @rdname svysen
#' @export
svysen.DBIsvydesign <-
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

    NextMethod("svysen", design)
  }

