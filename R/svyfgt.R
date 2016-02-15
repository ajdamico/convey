#' FGT measure of poverty
#'
#' Estimate the FGT measure for the cases: \code{alpha=0} headcount ratio and \code{alpha=1} poverty gap index.
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param type_thresh type of poverty threshold. If "abs" the threshold is fixed and given the value
#' of abs_thresh; if "relq" it is given by percent times the order quantile; if "relm" it is percent times the mean.
#' @param g If 0 estimates the headcount ratio and if 1 the poverty gap index
#' @param percent the multiple of the the quantile or mean used in the poverty threshold definition
#' @param order the quantile order used used in the poverty threshold definition
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
#'
#' des_eusilc <- svydesign( ids = ~rb030 , strata = ~db040 ,  weights = ~rb050 , data = eusilc )
#' des_eusilc <- convey_prep( des_eusilc )
#'
#' # replicate-weighted design
#' des_eusilc_rep <- survey:::as.svrepdesign( des_eusilc , type = "bootstrap" )
#' des_eusilc_rep <- convey_prep( des_eusilc_rep )
#'
#' # headcount ratio, poverty threshold fixed
#' svyfgt(~eqIncome, des_eusilc, g=0, type_thresh= "abs", abs_thresh=10000)
#' # poverty gap index, poverty threshold fixed
#' svyfgt(~eqIncome, des_eusilc, g=1, type_thresh= "abs", abs_thresh=10000)
#' # headcount ratio, poverty threshold equal to arpt
#' svyfgt(~eqIncome, des_eusilc, g=0, type_thresh= "relq")
#' # poverty gap index, poverty threshold equal to arpt
#' svyfgt(~eqIncome, des_eusilc, g=1, type_thresh= "relq")
#' # headcount ratio, poverty threshold equal to .6 times the mean
#' svyfgt(~eqIncome, des_eusilc, g=0, type_thresh= "relm")
#' # poverty gap index, poverty threshold equal to 0.6 times the mean
#' svyfgt(~eqIncome, des_eusilc, g=1, type_thresh= "relm")
#'
#' #  using svrep.design:
#' # headcount ratio, poverty threshold fixed
#' svyfgt(~eqIncome, des_eusilc_rep, g=0, type_thresh= "abs", abs_thresh=10000)
#' # poverty gap index, poverty threshold fixed
#' svyfgt(~eqIncome, des_eusilc, g=1, type_thresh= "abs", abs_thresh=10000)
#' # headcount ratio, poverty threshold equal to arpt
#' svyfgt(~eqIncome, des_eusilc_rep, g=0, type_thresh= "relq")
#' # poverty gap index, poverty threshold equal to arpt
#' svyfgt(~eqIncome, des_eusilc, g=1, type_thresh= "relq")
#' # headcount ratio, poverty threshold equal to .6 times the mean
#' svyfgt(~eqIncome, des_eusilc_rep, g=0, type_thresh= "relm")
#' # poverty gap index, poverty threshold equal to 0.6 times the mean
#' svyfgt(~eqIncome, des_eusilc_rep, g=1, type_thresh= "relm")
#'
#'
#'
#' @export
#'
svyfgt <- function(formula, design, ...) {

    UseMethod("svyfgt", design)

}

#' @rdname svyfgt
#' @export
svyfgt.survey.design <-   function(formula, design, g, type_thresh, abs_thresh,
  percent= .60, order =.50, na.rm=FALSE,...){

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
    w <- 1/design$prob
    ind<- names(design$prob)
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
    wf <- 1/full_design$prob
    ncom<- names(full_design$prob)
    htot <- h_fun(incvec, wf)
    #  h function
    h <- function(y,t,g){
      (((t-y)/t)^g)*(y<=t)
    }

    # ht function
    ht <- function(y,t,g){
      (g*(((t-y)/t)^(g-1))*(y/(t^2)))*(y<=t)
    }

    # linearization
    N <- sum(w)
    if(type_thresh=='relq'){
      ARPT <- svyarpt(formula = formula, full_design, order=order, percent=percent,  na.rm=na.rm)
      arpt <-coef(ARPT)
      arptlin<- attr(ARPT, "lin")
      rval <- sum(w*h(incvar,arpt,g))/N
      ahat <- sum(w*ht(incvar,arpt,g))/N
      if(g==0){
        ARPR <- svyarpr(formula = formula, design, order=order, percent=percent,  na.rm=na.rm)
        fgtlin <- attr(ARPR,"lin")
      } else
        fgtlin <-(h(incvar,arpt,g)-rval)/N+(ahat*arptlin)
    }
    if(type_thresh=='relm'){
      # thresh for the whole population
      t <- percent*sum(incvec*wf)/sum(wf)
      rval <- sum(w*h(incvar,t,g))/N
      ahat <- sum(w*ht(incvar,t,g))/N
      if(g==0){
        Fprime<- densfun(formula=formula, design = design, x= t, fun = "F", na.rm = na.rm )
        fgtlin<- (h(incvar,t,g)-rval + Fprime*(incvar-t))/N
      }else
        fgtlin <-(h(incvar,t,g)-rval+((percent*incvar)-t)*ahat)/N
    }
    if(type_thresh=='abs'){
      t<-abs_thresh
      rval <- sum(w*h(incvar,t,g))/N
      fgtlin <- (h(incvar,t,g)-rval)/N
      variance <- (SE_lin2(fgtlin, design))^2
    } else{
      if(nrow(full_design$variables)>length(fgtlin)){
        names(fgtlin)<- ind
        fgtlin <- complete(fgtlin, ncom)
      }
      variance <- (SE_lin2(fgtlin, full_design))^2
    }
    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- "cvystat"
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- paste0("fgt",g)
    attr(rval, "lin") <- fgtlin
    rval
}



#' @rdname svyfgt
#' @export
svyfgt.svyrep.design <-  function(formula, design, g, type_thresh, abs_thresh,
  percent= .60, order =.50, na.rm=FALSE,...) {

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
    # poverty threshold
    if(type_thresh=='relq') t <- percent* convey:::computeQuantiles(incvec, wsf, p = order)
    if(type_thresh=='relm') t <- percent*sum(incvec*wsf)/sum(wsf) else
    if(type_thresh=='abs') t <- abs_thresh
    ComputeFGT<- function(y, w, t, g){
      #  h function
      h <- function(y,t,g){
        (((t-y)/t)^g)*(y<=t)
      }
      N <-sum(w)
      sum(w*h(incvar,t,g))/N
    }

    rval <- ComputeFGT(incvar, ws, g = g, t)
    wwf <- weights(full_design, "analysis")
    qq <- apply(wwf, 2, function(wi){
      names(wi)<- row.names(df_full)
      wd<-wi[ind]
      incd <- incvec[ind]
      ComputeFGT(incd, wd, g = g, t)}
    )
    variance <- survey:::svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)

    variance <- as.matrix( variance )

    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- "cvystat"
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- paste0("fgt",g)
    rval
}
