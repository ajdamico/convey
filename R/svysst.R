#' Sen-Shorrocks-Thon poverty index (EXPERIMENTAL)
#'
#' Estimate the Sen-Shorrocks-Thon poverty measure.
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param abs_thresh poverty threshold value
#' @param na.rm Should cases with missing values be dropped?
#' @param components Keep estimates of FGT(0), FGT(1), Gini index of poverty gap ratios.
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
#' @seealso \code{\link{svysen}}, \code{\link{svyfgt}}, \code{\link{svygini}}.
#'
#' @references Anthony F. Shorrocks (1995). Revisiting the Sen Poverty Index.
#' \emph{Econometrica}, v. 63, n. 5, pp. 1225-230.
#' URL \url{http://www.jstor.org/stable/2171728}.
#'
#' Dominique Thon (1979). On measuring poverty.
#' \emph{Review of Income and Wealth}, v. 25, n. 4, pp. 429-439.
#' URL \url{http://dx.doi.org/10.1111/j.1475-4991.1979.tb00117.x}.
#'
#' Amartya K. Sen (1976). Poverty: An Ordinal Approach to Measurement.
#' \emph{Econometrica}, v. 44, n. 3, pp. 219-231.
#' URL \url{http://www.jstor.org/stable/1912718}.
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
#' # using linearized design
#' svysst( ~eqincome, des_eusilc, abs_thresh=10000 )
#'
#' # using replicate design:
#' svysst( ~eqincome, des_eusilc_rep, abs_thresh = 10000 )
#'
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
#' # linearized SE:
#' svysst(~eqincome, dbd_eusilc, abs_thresh=10000)
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' dbDisconnect( conn , shutdown = TRUE )
#'
#' }
#'
#' @export
svysst <-
  function(formula, design,  ...) {

    if( 'type_thresh' %in% names( list( ... ) ) && !( list(...)[["type_thresh"]] %in% c( 'relq' , 'abs' , 'relm' ) ) ) stop( 'type_thresh= must be "relq" "relm" or "abs".  see ?svysst for more detail.' )

    if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

    warning("The svysst function is experimental and is subject to changes in later versions.")

    UseMethod("svysst", design)

  }

#' @rdname svysst
#' @export
svysst.survey.design <-
  function( formula, design, abs_thresh=NULL, na.rm = FALSE, components = FALSE, ...){

    if (is.null(attr(design, "full_design"))) stop("you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svydesign() function.")

    # if the class of the full_design attribute is just a TRUE, then the design is
    # already the full design.  otherwise, pull the full_design from that attribute.
    if ("logical" %in% class(attr(design, "full_design"))) full_design <- design else full_design <- attr(design, "full_design")

    if( is.null( names( design$prob ) ) ) ind <- as.character( seq( length( design$prob ) ) ) else ind <- names(design$prob)
    if( is.null( names( full_design$prob ) ) ) ncom <- as.character( seq( length( full_design$prob ) ) ) else ncom <- names(full_design$prob)
    if (sum(1/design$prob==0) > 0) { ID <- 1*(1/design$prob!=0) } else { ID <- 1 * ( ncom %in% ind ) }


    # FGT(0)
    rval.fgt0 <- svyfgt( formula = formula, design = design, abs_thresh=abs_thresh, g = 0, na.rm = na.rm )
    fgt0 <- NULL
    fgt0$value <- coef( rval.fgt0 )[[1]]
    fgt0$lin <- attr( rval.fgt0, "lin" )
    rm( rval.fgt0 )

    # FGT(1)
    th <- abs_thresh
    incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]
    rval.fgt1 <- svyfgt( formula = formula, design = design[ incvar <= abs_thresh ], abs_thresh=abs_thresh, g = 1, na.rm = na.rm )
    fgt1 <- NULL
    fgt1$value <- coef( rval.fgt1 )[[1]]
    fgt1$lin <- attr( rval.fgt1, "lin" )
    rm( rval.fgt1 )

    # Gini index
    #design$variables$pgratio <- ( ( th - incvar ) / th ) * ( th >= incvar )
    design$variables$pgratio <- ( th - incvar ) * ( th >= incvar )
    #design$variables$pgratio <- (th - incvar) / th

    gini.design <- design
    class( gini.design ) <- class( design )[ !( class(design ) %in% "DBIsvydesign" ) ]
    rval.gini <- svygini( formula = ~pgratio, design = gini.design, na.rm = na.rm )

    gini <- NULL
    gini$value <- coef(rval.gini)[[1]]
    if ( length( attr( rval.gini, "lin" ) ) == length( fgt0$lin ) ) {
      gini$lin <- attr( rval.gini, "lin" )
    } else {
      gini$lin <- 1 * ( fgt0$lin != 0 )
      gini$lin[ gini$lin == 1 ] <- attr( rval.gini, "lin" )
    }
    rm( rval.gini, gini.design )

    if ( any( is.na( c( fgt0$value, fgt1$value, gini$value ) ) ) ) {

      sst <- NULL
      variance <- NA

      rval <- sst$value
      colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
      class(rval) <- c( "cvystat" , "svystat" )
      attr(rval, "var") <- variance
      attr(rval, "statistic") <- names( rval ) <- "sen-shorrocks-thon"

      if ( components ) {
        component.var <- matrix( rep(NA,16), ncol = 4, dimnames = list( c( "FGT(0)", "FGT(1)", "Gini(PGR)", "SST" ), c( "FGT(0)", "FGT(1)", "Gini(PGR)", "SST" ) ) )[,]
        component.est <- matrix( data = rep( NA, 8 ),
                                 nrow = 4, dimnames = list( c( "FGT(0)", "FGT(1)", "Gini(PGR)", "SST" ), c( "Estimate", "SE" ) ) )
        attr(rval, "components") <- list( `Components` = component.est, `C-VCOV` = component.var )
      }

    }

    sst <- contrastinf( quote( fgt0 * fgt1 * ( 1 + gini) ), list( fgt0 = fgt0, fgt1 = fgt1, gini = gini ) )

    variance <- survey::svyrecvar( sst$lin/full_design$prob, full_design$cluster, full_design$strata, full_design$fpc, postStrata = full_design$postStrata)

    rval <- sst$value
    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- c( "cvystat" , "svystat" )
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- names( rval ) <- "sen-shorrocks-thon"

    if ( components ) {
      lin.matrix <-  matrix( data = c(fgt0$lin, fgt1$lin, gini$lin, sst$lin), ncol = 4, dimnames = list( NULL, c( "FGT(0)", "FGT(1)", "Gini(PGR)", "SST" ) ) )
      component.var <- survey::svyrecvar( lin.matrix/full_design$prob, full_design$cluster, full_design$strata, full_design$fpc, postStrata = full_design$postStrata)
      component.est <- matrix( data = c( fgt0$value, fgt1$value, gini$value, sst$value, sqrt( diag( component.var ) ) ),
                               nrow = 4, dimnames = list( c( "FGT(0)", "FGT(1)", "Gini(PGR)", "SST" ), c( "Estimate", "SE" ) ) )
      attr(rval, "components") <- list( `Components` = component.est, `C-VCOV` = component.var )
    }

    return(rval)

  }



#' @rdname svysst
#' @export
svysst.svyrep.design <-
  function( formula, design, abs_thresh = NULL, na.rm = FALSE, components = FALSE, ... ) {

    if (is.null(attr(design, "full_design"))) stop("you must run the ?convey_prep function on your replicate-weighted survey design object immediately after creating it with the svrepdesign() function.")

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
      th <- abs_thresh


      fgt0 <- ComputeFGT( incvar, ws, g = 0, th)
      # fgt1 <- ComputeFGT(incvar, ws, g = 1, th)
      fgt1 <- ComputeFGT( incvar, ws * ( incvar <= th ), g = 1, th)
      #gini <- ComputeGini( ( ( th - incvar ) / th ) * ( incvar <= th ), ws )
      gini <- ComputeGini( ( th - incvar ) * ( incvar <= th ) , ws )

      sst <- fgt0 * fgt1 * ( 1 + gini )

      wwf <- weights(full_design, "analysis")

      qq.fgt0 <-
        apply(wwf, 2, function(wi){
          names(wi)<- row.names(df_full)
          wd<-wi[ind]
          incd <- incvec[ind]
          ComputeFGT(incd, wd, g = 0, th)}
        )

      qq.fgt1 <-
        apply(wwf, 2, function(wi){
          names(wi)<- row.names(df_full)
          wd<-wi[ind]
          incd <- incvec[ind]
          ComputeFGT(incd, wd, g = 1, th)}
        )

      qq.gini <-
        apply(wwf, 2, function(wi){
          names(wi)<- row.names(df_full)
          wd<-wi[ind]
          incd <- incvec[ind]
          #wd<- wd *( incd <= th )
          ComputeGini( ( ( th - incd ) / th ) * ( incd <= th ) , wd )}
        )

      qq <- qq.fgt0 *  qq.fgt1 * ( 1 + qq.gini )

      if(anyNA(qq)) {
        if ( any( is.na( c( fgt0$value, fgt1$value, gini$value ) ) ) ) {

          variance <- NA
          rval <- NA
          colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
          class(rval) <- c( "cvystat" , "svrepstat" )
          attr(rval, "var") <- variance
          attr(rval, "statistic") <- names( rval ) <- "sen-shorrocks-thon"

          if ( components ) {
            component.var <- matrix( rep(NA,16), ncol = 4, dimnames = list( c( "FGT(0)", "FGT(1)", "Gini(PGR)", "SST" ), c( "FGT(0)", "FGT(1)", "Gini(PGR)", "SST" ) ) )[,]
            component.est <- matrix( data = rep( NA, 8 ),
                                     nrow = 4, dimnames = list( c( "FGT(0)", "FGT(1)", "Gini(PGR)", "SST" ), c( "Estimate", "SE" ) ) )
            attr(rval, "components") <- list( `Components` = component.est, `C-VCOV` = component.var )
          }

        }
      } else {
        variance <- survey::svrVar( qq, design$scale, design$rscales, mse = design$mse, coef = sst )
      }

      rval <- sst
      names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
      class(rval) <- c( "cvystat" , "svrepstat" )
      attr(rval, "var") <- variance
      attr(rval, "statistic") <- names( rval ) <- "sen-shorrocks-thon"

      if ( components ) {
        qq.matrix <-  matrix( data = c( qq.fgt0, qq.fgt1, qq.gini, qq ), ncol = 4, dimnames = list( NULL, c( "FGT(0)", "FGT(1)", "Gini(PGR)", "SST" ) ) )
        component.var <- survey::svrVar( qq.matrix , design$scale, design$rscales, mse = design$mse, coef = c( fgt0, fgt1, gini, sst ) )
        component.est <- matrix( data = c( fgt0, fgt1, gini, sst, sqrt( diag( component.var[1:4,1:4] ) ) ),
                                 nrow = 4, dimnames = list( c( "FGT(0)", "FGT(1)", "Gini(PGR)", "SST" ), c( "Estimate", "SE" ) ) )
        attr(rval, "components") <- list( `Components` = component.est, `C-VCOV` = component.var )
      }

      return( rval )

  }

#' @rdname svysst
#' @export
svysst.DBIsvydesign <-
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
      full_design$variables$pgratio <- full_design$variables[,1]
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
    design$variables$pgratio <- design$variables[,1]


    NextMethod("svysst", design)
  }

