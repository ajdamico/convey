#' Relative median poverty gap
#'
#' Estimate the difference between the at-risk-of-poverty threshold (\code{arpt}) and the median of incomes less than the \code{arpt} relative to the \code{arpt}.
#'
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design}
#' of the library survey
#' @param order income quantile order, usually .5
#' @param percent fraction of the quantile, usually .60
#' @param comp logical variable \code{TRUE} if the inearized variable for domains should be completed with zeros
#' @param na.rm Should cases with missing values be dropped?
#'
#'@details you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
#'
#'@return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of the statistic.
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
#' svyrmpg( ~eqIncome , design = des_eusilc )
#'
#' # replicate-weighted design
#' des_eusilc_rep <- survey:::as.svrepdesign( des_eusilc , type = "bootstrap" )
#' des_eusilc_rep <- convey_prep( des_eusilc_rep )
#' svyrmpg( ~eqIncome , design = des_eusilc_rep )
#'
#'
#' # linearized design using a variable with missings
#' svyrmpg( ~ py010n , design = des_eusilc )
#' svyrmpg( ~ py010n , design = des_eusilc , na.rm = TRUE )
#' # replicate-weighted design using a variable with missings
#' svyrmpg( ~ py010n , design = des_eusilc_rep )
#' svyrmpg( ~ py010n , design = des_eusilc_rep , na.rm = TRUE )
#'
#'
#' @export

svyrmpg <- function(formula, design, ...) {

    UseMethod("svyrmpg", design)

}

#' @rdname svyrmpg
#' @export
svyrmpg.survey.design <- function(formula, design, order = 0.5, percent = 0.6, comp, na.rm=FALSE, ...) {
  if (is.null(attr(design, "full_design")))
    stop("you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svydesign() function.")

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

    df_full <- model.frame(full_design)
    incvec <- df_full[[as.character(inc)]]
    if(na.rm){
      nas<-is.na(incvec)
      full_design<-full_design[!nas,]
      df_full <- model.frame(full_design)
      incvec <- incvec[!nas]
    }

    ARPT <- svyarpt(formula = formula, full_design, order = 0.5, percent = 0.6, na.rm = na.rm )
    arpt <- coef(ARPT)
    linarpt <- attr(ARPT, "lin")
    POORMED <- svypoormed(formula = formula, design = design, order = order, percent = percent, na.rm = na.rm)
    medp <- coef(POORMED)
    linmedp <- attr(POORMED, "lin")
    MEDP<- list(value=medp,lin=linmedp)
    ARPT<- list(value = arpt, lin= linarpt)
    list_all<- list(ARPT=ARPT, MEDP=MEDP)
    # linearize RMPG
    RMPG<- contrastinf(quote((ARPT-MEDP)/ARPT), list_all)
    rval <- RMPG$value
    infun <- unlist( RMPG$lin)
    variance <- ( SE_lin2( infun , full_design ) )^2
    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- "cvystat"
    attr( rval , "var" ) <- variance
    attr(rval, "lin") <- infun
    attr( rval , "statistic" ) <- "rmpg"
    rval
}
#' @rdname svyrmpg
#' @export
svyrmpg.svyrep.design <- function(formula, design, order = 0.5, percent = 0.6,na.rm=FALSE, ...) {

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
    ComputeRmpg <- function(xf, wf, ind, order, percent) {
      tresh <- percent * convey:::computeQuantiles(xf, wf, p = order)
      x<-xf[ind]
      w<- wf[ind]
      indpoor <- (x <= tresh)
      medp <- convey:::computeQuantiles(x[indpoor], w[indpoor], p = 0.5)
      1 - (medp/tresh)
    }
    ws <- weights(design, "sampling")
    rval <- ComputeRmpg(xf = incvec, wf=wsf, ind= ind, order = order, percent = percent)
    wwf <- weights(full_design, "analysis")
    qq <- apply(wwf, 2, function(wi){
      names(wi)<- row.names(df_full)
      ComputeRmpg(incvec, wi, ind=ind, order = order,percent = percent)
    })
    variance <- survey:::svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)

    variance <- as.matrix( variance )

    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- "cvystat"
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "rmpg"
    rval
}



#' @rdname svyrmpg
#' @export
svyrmpg.DBIsvydesign <- function(x, design, ...) {
    design$variables <- survey:::getvars(x, design$db$connection, design$db$tablename,
        updates = design$updates, subset = design$subset)
    NextMethod("svyrmpg", design)
}


