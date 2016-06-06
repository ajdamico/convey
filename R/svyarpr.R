#' At-risk-of-poverty rate
#'
#' Estimate the proportion of persons with income below the at-risk-of-poverty threshold.
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param order income quantile order, usually .5
#' @param percent fraction of the quantile, usually .60
#' @param na.rm Should cases with missing values be dropped?
#'
#'@details you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
#'
#' @return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of the statistic.
#'
#' @author Djalma Pessoa and Anthony Damico
#'
#' @seealso \code{\link{arpt}}
#'
#' @references Guillaume Osier (2009). Variance estimation for complex indicators
#'of poverty and inequality. \emph{Journal of the European Survey Research
#' Association}, Vol.3, No.3, pp. 167-195,
#' ISSN 1864-3361, URL \url{http://ojs.ub.uni-konstanz.de/srm/article/view/369}.

#'Jean-Claude Deville (1999). Variance estimation for complex statistics and estimators:
#'linearization and residual techniques. Survey Methodology, 25, 193-203,
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
#' des_eusilc <- svydesign( ids = ~rb030 , strata = ~db040 ,  weights = ~rb050 , data = eusilc )
#' des_eusilc <- convey_prep( des_eusilc )
#'
#' svyarpr( ~eqincome , design = des_eusilc )
#'
#' # replicate-weighted design
#' des_eusilc_rep <- survey:::as.svrepdesign( des_eusilc , type = "bootstrap" )
#' des_eusilc_rep <- convey_prep( des_eusilc_rep )
#'
#' svyarpr( ~eqincome , design = des_eusilc_rep )
#'
#' # linearized design using a variable with missings
#' svyarpr( ~ py010n , design = des_eusilc )
#' svyarpr( ~ py010n , design = des_eusilc , na.rm = TRUE )
#' # replicate-weighted design using a variable with missings
#' svyarpr( ~ py010n , design = des_eusilc_rep )
#' svyarpr( ~ py010n , design = des_eusilc_rep , na.rm = TRUE )
#'
#' # database-backed design
#' require(RSQLite)
#' tfile <- tempfile()
#' conn <- dbConnect( SQLite() , tfile )
#' dbWriteTable( conn , 'eusilc' , eusilc )
#'
#' dbd_eusilc <- svydesign(ids = ~rb030 , strata = ~db040 ,  weights = ~rb050 , data="eusilc", dbname=tfile, dbtype="SQLite")
#' dbd_eusilc <- convey_prep( dbd_eusilc )
#'
#' svyarpr( ~ eqincome , design = dbd_eusilc )
#'
#' @export
#'
svyarpr <- function(formula, design, ...) {

    UseMethod("svyarpr", design)

}

#' @rdname svyarpr
#' @export
svyarpr.survey.design <- function(formula, design, order = 0.5, percent = 0.6, na.rm=FALSE,...) {

  if (is.null(attr(design, "full_design")))
    stop("you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svydesign() function.")

  if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )


  # if the class of the full_design attribute is just a TRUE, then the design is
  # already the full design.  otherwise, pull the full_design from that attribute.
  if ("logical" %in% class(attr(design, "full_design")))
    full_design <- design else full_design <- attr(design, "full_design")

    # domain
    incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

    if(na.rm){
      nas<-is.na(incvar)
      design<-design[!nas,]
      if (length(nas) > length(design$prob))
        incvar <- incvar[!nas]
      else incvar[nas] <- 0
    }
    ind<- names(design$prob)
    w <- 1/design$prob
    N <- sum(w)
    # if the class of the full_design attribute is just a TRUE, then the design is
    # already the full design.  otherwise, pull the full_design from that attribute.
    if ("logical" %in% class(attr(design, "full_design")))
      full_design <- design else full_design <- attr(design, "full_design")
    incvec <- model.frame(formula, full_design$variables, na.action = na.pass)[[1]]

    if(na.rm){
      nas<-is.na(incvec)
      full_design<-full_design[!nas,]
      if (length(nas) > length(full_design$prob))
        incvec <- incvec[!nas]
      else incvec[nas] <- 0
    }
    ncom<- names(full_design$prob)
    wf <- 1/full_design$prob
    htot <- h_fun(incvec, wf)
    ARPT <- svyarpt (formula = formula, design=full_design, order = 0.5, percent = 0.6, na.rm = na.rm)
    arptv <- coef(ARPT)
    arptlin <- attr(ARPT, "lin")
    # value of arpr and first term of lin
    poor<- incvar<=arptv
    rval <- sum(poor*w)/N
    ID <- rep(1, length(incvec))* (ncom %in% ind)
    arpr1lin <- (1/N)*ID*((incvec<=arptv)-rval)

    # use h for the whole sample
    Fprime <- densfun(formula = formula, design = design, arptv, h=htot, fun = "F", na.rm=na.rm)

    arprlin <- arpr1lin + Fprime * arptlin
    variance <- survey::svyrecvar(arprlin/full_design$prob, full_design$cluster,
      full_design$strata, full_design$fpc,
      postStrata = full_design$postStrata)

    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- "cvystat"
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "arpr"
    attr(rval, "lin") <- arprlin
    rval
}



#' @rdname svyarpr
#' @export
svyarpr.svyrep.design <- function(formula, design, order = 0.5, percent = 0.6,na.rm=FALSE, ...) {

  if (is.null(attr(design, "full_design")))
    stop("you must run the ?convey_prep function on your replicate-weighted survey design object immediately after creating it with the svrepdesign() function.")

  if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )


  # if the class of the full_design attribute is just a TRUE, then the design is
  # already the full design.  otherwise, pull the full_design from that attribute.
  if ("logical" %in% class(attr(design, "full_design")))
    full_design <- design else full_design <- attr(design, "full_design")

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
    wsf<- weights(full_design,"sampling")
    names(incvec)<-names(wsf)<- row.names(df_full)
    ind<- row.names(df)
    ComputeArpr <- function(xf, wf, ind, order, percent) {
      tresh <- percent * convey:::computeQuantiles(xf, wf, p = order)
      sum((xf[ind] <= tresh) * wf[ind])/sum(wf[ind])
    }
    rval <- ComputeArpr(xf = incvec, wf=wsf, ind= ind, order = order, percent = percent)
    wwf <- weights(full_design, "analysis")
    qq <- apply(wwf, 2, function(wi){
      names(wi)<- row.names(df_full)
      ComputeArpr(incvec, wi, ind=ind, order = order,percent = percent)}
    )
    if(sum(is.na(qq))==length(qq))variance <- NA else
    variance <- survey:::svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)

    variance <- as.matrix( variance )

    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- "cvystat"
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "arpr"
    rval
}

#' @rdname svyarpr
#' @export
svyarpr.DBIsvydesign <-
  function (formula, design, ...)
  {

    if (!( "logical" %in% class(attr(design, "full_design"))) ){

      full_design <- attr( design , "full_design" )

      full_design$variables <- survey:::getvars(formula, attr( design , "full_design" )$db$connection, attr( design , "full_design" )$db$tablename,
        updates = attr( design , "full_design" )$updates, subset = attr( design , "full_design" )$subset)

      attr( design , "full_design" ) <- full_design

      rm( full_design )

    }

    design$variables <- survey:::getvars(formula, design$db$connection, design$db$tablename,
      updates = design$updates, subset = design$subset)

    NextMethod("svyarpr", design)
  }
