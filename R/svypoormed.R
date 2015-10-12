#' Relative median poverty gap
#'
#' Estimate the median of incomes less than the at-risk-of-poverty threshold (\code{arpt}).
#'
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} of the library survey
#' @param order income quantile order, usually .5
#' @param percent fraction of the quantile, usually .60
#' @param comp logical variable \code{TRUE} if the inearized variable for domains should be completed with zeros
#' @param na.rm Should cases with missing values be dropped?
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
#' svypoormed( ~eqIncome , design = des_eusilc )
#'
#' # replicate-weighted design
#' des_eusilc_rep <- as.svrepdesign( des_eusilc , type = "bootstrap" )
#' des_eusilc_rep <- convey_prep( des_eusilc_rep )
#' svypoormed( ~eqIncome , design = des_eusilc_rep )
#'
#' # linearized design using a variable with missings
#' svypoormed( ~ py010n , design = des_eusilc )
#' svypoormed( ~ py010n , design = des_eusilc , na.rm = TRUE )
#' # replicate-weighted design using a variable with missings
#' svypoormed( ~ py010n , design = des_eusilc_rep )
#' svypoormed( ~ py010n , design = des_eusilc_rep , na.rm = TRUE )
#'
#' @export
#'

svypoormed <- function(formula, design, ...) {

    UseMethod("svypoormed", design)

}

#' @rdname svypoormed
#' @export
svypoormed.survey.design <- function(formula, design, order = 0.5, percent = 0.6, comp=TRUE,na.rm=FALSE, ...) {
  if (is.null(attr(design, "full_design")))
    stop("you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svydesign() function.")

  if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

  # if the class of the full_design attribute is just a TRUE, then the design is
  # already the full design.  otherwise, pull the full_design from that attribute.
  if ("logical" %in% class(attr(design, "full_design")))
    full_design <- design else full_design <- attr(design, "full_design")
    df_full <- model.frame(full_design)
    ncom = row.names(df_full)
    w <- weights(design)
    N <- sum(w)
    inc <- terms.formula(formula)[[2]]
    df <- model.frame(design)
    ind <- row.names(df)
    incvar <- df[[as.character(inc)]]
    df_full<- model.frame(full_design)
    incvec <- df_full[[as.character(inc)]]
    wf<- weights(full_design)
    ARPT <- svyarpt(formula = formula, full_design, order = 0.5, percent = 0.6)
    arpt <- ARPT[1]
    linarpt <- attr(ARPT, "lin")
    dsub <- subset(design, subset = (incvar <= arpt))
    medp <- survey::svyquantile(x = formula, dsub, 0.5, method = "constant")
    medp <- as.vector(medp)
    htot <- h_fun(incvec,wf)
    ARPR <- svyarpr(formula=formula, design= design, order, percent)
    Fprimemedp <- densfun(formula = formula, design = design, medp,
      h = htot, fun = "F")
    arpr<-ARPR[1]
    ifarpr<-attr(ARPR, "lin")
    # linearize cdf of medp
    ifmedp <- (1/N) * ((incvar <= medp) - 0.5 * arpr)
    names(ifmedp) <- ind
    ifmedp <- complete(ifmedp, ncom)
    # linearize median of poor
    linmedp <- (0.5 * ifarpr - ifmedp)/Fprimemedp
    rval <-medp
    variance <- ( SE_lin2( linmedp , full_design ) )^2
    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- "cvystat"
    attr( rval , "var" ) <- variance
    attr(rval, "lin") <- linmedp
    attr( rval , "statistic" ) <- "poormed"
    rval
}

#' @rdname svypoormed
#' @export
svypoormed.svyrep.design <- function(formula, design, order = 0.5, percent = 0.6,na.rm=FALSE, ...) {

  if (is.null(attr(design, "full_design")))
    stop("you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svrepdesign() function.")

  if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

  # if the class of the full_design attribute is just a TRUE, then the design is
  # already the full design.  otherwise, pull the full_design from that attribute.
  if ("logical" %in% class(attr(design, "full_design")))
    full_design <- design else full_design <- attr(design, "full_design")

    inc <- terms.formula(formula)[[2]]
    df <- model.frame(design)
    incvar <- df[[as.character(inc)]]
    ws <- weights(design, "sampling")
    wsf<- weights(full_design,"sampling")
    df_full<- model.frame(full_design)
    incvec <-  df_full[[as.character(inc)]]
    names(incvec)<-names(wsf)<- row.names(df_full)
    ind<- row.names(df)
    ComputePoormed <- function(xf, wf, ind, order, percent) {
      tresh <- percent * convey:::computeQuantiles(xf, wf, p = order)
      x<-xf[ind]
      w<- wf[ind]
      indpoor <- (x <= tresh)
      medp <- convey:::computeQuantiles(x[indpoor], w[indpoor], p = 0.5)
      medp
    }
    ws <- weights(design, "sampling")
    rval <- ComputePoormed(xf = incvec, wf=wsf, ind= ind, order = order, percent = percent)
    wwf <- weights(full_design, "analysis")
    qq <- apply(wwf, 2, function(wi){
      names(wi)<- row.names(df_full)
      ComputePoormed(incvec, wi, ind=ind, order = order,percent = percent)
    })

	variance <- as.matrix( variance )

	colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- "cvystat"
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "poormed"
    rval
}



#' @rdname svypoormed
#' @export
svypoormed.DBIsvydesign <- function(x, design, ...) {
    design$variables <- survey:::getvars(x, design$db$connection, design$db$tablename,
        updates = design$updates, subset = design$subset)
    NextMethod("svypoormed", design)
}


