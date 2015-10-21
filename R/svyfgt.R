#' FGT measure of poverty
#'
#' Estimate the FGT measure for the cases: \code{alpha=0} headcount ratio and \code{alpha=1} poverty gap index.
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} of the library survey
#' @param t poverty threshold. If NULL uses the \code{arpt}
#' @param alpha If 0 estimates the headcount ratio and if 1 the poverty gap index
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
#' data(eusilc)
#'
#' # linearized design
#' des_eusilc <- svydesign( ids = ~rb030 , strata = ~db040 ,  weights = ~rb050 , data = eusilc )
#' des_eusilc <- convey_prep( des_eusilc )
#'
#' svyarpr( ~eqIncome , design = des_eusilc )
#'
#' # replicate-weighted design
#' des_eusilc_rep <- survey:::as.svrepdesign( des_eusilc , type = "bootstrap" )
#' des_eusilc_rep <- convey_prep( des_eusilc_rep )
#'
#' # headcount ratio, poverty threshold fixed
#' svyfgt(~eqIncome, des_eusilc, t=10000, alpha=0)
#' # poverty gap index, poverty threshold fixed
#' svyfgt(~eqIncome, des_eusilc, t=10000, alpha=1)
#' # headcount ratio, poverty threshold equal to arpt
#' svyfgt(~eqIncome, des_eusilc,  t=NULL, alpha=0)
#' # poverty gap index, poverty threshold equal to arpt
#' svyfgt(~eqIncome, des_eusilc,  t=NULL, alpha=1 )
#'
#' @export
#'
svyfgt <- function(formula, design, ...) {

    UseMethod("svyfgt", design)

}

#' @rdname svyfgt
#' @export
svyfgt.survey.design <-  function(formula, design, t=NULL, alpha,na.rm=FALSE, ...) {

  if (is.null(attr(design, "full_design")))
    stop("you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svydesign() function.")

  if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )


  # if the class of the full_design attribute is just a TRUE, then the design is
  # already the full design.  otherwise, pull the full_design from that attribute.
  if ("logical" %in% class(attr(design, "full_design")))
    full_design <- design else full_design <- attr(design, "full_design")

    # domain
    inc <- terms.formula(formula)[[2]]
    df <- model.frame(design)
    incvar <- df[[as.character(inc)]]
    if(na.rm){
      nas<-is.na(incvar)
      design<-design[!nas,]
      df <- model.frame(design)
      incvar <- incvar[!nas]
    }
    w <- weights(design)
    N <- sum(w)
    ind <- row.names(df)

    # if the class of the full_design attribute is just a TRUE, then the design is
    # already the full design.  otherwise, pull the full_design from that attribute.
    if ("logical" %in% class(attr(design, "full_design")))
      full_design <- design else full_design <- attr(design, "full_design")


    df_full <- model.frame(full_design)
    incvec <- df_full[[as.character(inc)]]
    if(na.rm){
      nas<-is.na(incvec)
      full_design<-full_design[!nas,]
      df_full <- model.frame(full_design)
      incvec <- incvec[!nas]
    }

    wf <- weights(full_design)
    ncom <- row.names(df_full)
    htot <- h_fun(incvec, wf)

    if(!is.null(t)){
      if(alpha==0){
        FGT <-icdf(formula=formula, design=design, t)
        rval <- coef(FGT)
        fgtlin <-attr(FGT,"lin")
      }
      else{
        poor <- (incvar<=t)
        T1 <-  list(value = t*sum(poor*w), lin= t*poor)
        T2 <- list(value= sum(incvar*poor*w), lin= incvar*poor)
        T3<- list(value= t*sum(w) , lin=rep(t, length(incvar)))
        list_all <- list(T1 = T1, T2 = T2,T3 = T3)
        FGT<- contrastinf(quote((T1-T2)/T3), list_all)
        rval <- FGT$value
        fgtlin<- FGT$lin
        names(fgtlin)<- ind
        fgtlin <- complete(fgtlin, ncom)
      }
    }
    else{
      ## threshold is arpt
      if(alpha==0) {
        FGT <- svyarpr(formula = formula, design=design)
        rval<- coef(FGT)
        fgtlin <- attr(FGT, "lin")
      }
      else{
        ARPT <- svyarpt(formula = formula, full_design)
        arpt <-coef(ARPT)
        arptlin<- attr(ARPT, "lin")
        ARPR <- svyarpr(formula = formula, design)
        arprlin <-attr(ARPR, "lin")
        T1<- list(value=coef(ARPR), lin=arprlin)
        # total below arpt
        Fprime <- densfun(formula = formula, design = design, arpt, h= htot, fun = "S")
        isqalpha <- incvec * ((incvec <= arpt)) + Fprime * arptlin
        T2 = list(value= sum(incvar*(incvar<=arpt)*w), lin= isqalpha)
        T3= list(value= arpt, lin= arptlin)
        T4 = list(value=N, lin=rep(1,length(incvec) ) )
        list_all=list(T1=T1, T2=T2, T3=T3, T4=T4)
        FGT<- contrastinf(quote(T1-T2/(T3*T4)), list_all)
        rval<- FGT$value
        fgtlin<- FGT$lin
      }
    }

    variance <- (SE_lin2(fgtlin, full_design))^2
    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- "cvystat"
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "fgt"
    attr(rval, "lin") <- fgtlin
    rval
}



#' @rdname svyfgt
#' @export
svyfgt.svyrep.design <- function(formula, design, t = NULL, alpha, na.rm=FALSE, ...) {

  if (is.null(attr(design, "full_design")))
    stop("you must run the ?convey_prep function on your replicate-weighted survey design object immediately after creating it with the svrepdesign() function.")

  if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )


  # if the class of the full_design attribute is just a TRUE, then the design is
  # already the full design.  otherwise, pull the full_design from that attribute.
  if ("logical" %in% class(attr(design, "full_design")))
    full_design <- design else full_design <- attr(design, "full_design")

    inc <- terms.formula(formula)[[2]]
    df <- model.frame(design)
    incvar <- df[[as.character(inc)]]
    if(na.rm){
      nas<-is.na(incvar)
      design<-design[!nas,]
      df <- model.frame(design)
      incvar <- incvar[!nas]
    }
    ws <- weights(design, "sampling")

    df_full<- model.frame(full_design)
    incvec <-  df_full[[as.character(inc)]]
    if(na.rm){
      nas<-is.na(incvec)
      full_design<-full_design[!nas,]
      df_full <- model.frame(full_design)
      incvec <- incvec[!nas]
    }

    wsf<- weights(full_design,"sampling")
    names(incvec)<-names(wsf)<- row.names(df_full)
    ind<- row.names(df)

    ComputeFGT<- function(y, w, t, alpha){
      N <-sum(w)
      poor <- (y <= t)
      fac1 <-((t-y)/t)^alpha
      sum(fac1* poor*w)/N
    }
    # threshold arpt
    if(is.null(t)) t <- .60 * convey:::computeQuantiles(incvec, wsf, p = .50)
    rval <- ComputeFGT(incvar, ws, t=t, alpha=alpha)
    wwf <- weights(full_design, "analysis")
    qq <- apply(wwf, 2, function(wi){
      if(is.null(t)) t<- .60 * convey:::computeQuantiles(incvec, wi, p = .50)
      names(wi)<- row.names(df_full)
      wd<-wi[ind]
      incd <- incvec[ind]
      ComputeFGT(incd,wd, t=t, alpha = alpha)}
    )
    variance <- survey:::svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)

    variance <- as.matrix( variance )

    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- "cvystat"
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "fgt"
    rval
}


#' @rdname svyfgt
#' @export
svyfgt.DBIsvydesign <- function(formula, design, ...) {
    design$variables <- survey:::getvars(formula, design$db$connection, design$db$tablename,
        updates = design$updates, subset = design$subset)
    NextMethod("svyfgt", design)
}

