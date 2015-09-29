#' Relative median poverty gap
#'
#' Estimate the difference between the poverty threshold \code{arpt} and the median of  incomes less than the \code{arpt} relative to the \code{arpt}.
#'
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design}
#' of the library survey
#' @param order  income quantile order, usually .5
#' @param percent fraction of the quantile, usually .60
#' @param comp logical variable \code{TRUE} if the inearized variable for domains
#' should be completed with zeros
#'
#'@return Object of class "\code{cvystat}", which are vectors with a "var" attribute #'giving the variance and a "\code{statistic}" attribute giving the name of
#'the statistic.
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
#' library(vardpoor)
#' data(eusilc)
#' library(survey)
#' des_eusilc <- svydesign(ids=~db040, weights=~rb050, data=eusilc)
#' des_eusilc <- convey_prep( des_eusilc )
#' rmpg_eqIncome <- svyrmpg(~eqIncome, design=des_eusilc, order =.50,
#' percent = .60)
#'
#' @export

svyrmpg <- function(formula, design, ...) {

    UseMethod("svyrmpg", design)

}

#' @rdname svyrmpg
#' @export
svyrmpg.survey.design <- function(formula, design, order = 0.5, percent = 0.6, comp, ...) {
  if (is.null(attr(design, "full_design")))
    stop("you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svydesign() function.")

  if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

  # if the class of the full_design attribute is just a TRUE, then the design is
  # already the full design.  otherwise, pull the full_design from that attribute.
  if ("logical" %in% class(attr(design, "full_design")))
    full_design <- design else full_design <- attr(design, "full_design")
    ncom = names(weights(full_design))
    w <- weights(design)
    ind <- names(w)
    N <- sum(w)
    inc <- terms.formula(formula)[[2]]
    df <- model.frame(design)
    incvar <- df[[as.character(inc)]]
    df_full<- model.frame(full_design)
    incvec <- df_full[[as.character(inc)]]
    wf<- weights(full_design)
    ncom <- names(wf)
    ARPT <- svyarpt(formula = formula, full_design, order = 0.5, percent = 0.6)
    arpt <- ARPT[1]
    linarpt <- attr(ARPT, "lin")
    dsub <- subset(design, subset = (incvar <= arpt))
    medp <- survey::svyquantile(x = formula, dsub, 0.5, method = "constant")
    medp <- as.vector(medp)
    #RMPG <- 1 - (medp/arpt)
    htot <- h_fun(incvec,wf)
    ARPR <- svyarpr(formula=formula, design= design, order, percent)
    Fprimemedp <- densfun(formula = formula, design = design, medp,
      h = htot, fun = "F")
    arpr<-ARPR[1]
    ifarpr<-attr(ARPR, "lin")
    # linearize cdf of medp
    ifmedp <- (1/N) * ((incvar <= medp) - 0.5 * arpr)
    names(ifmedp) <- names(w)
    ifmedp <- complete(ifmedp, ncom)
    # linearize median of poor
    linmedp <- (0.5 * ifarpr - ifmedp)/Fprimemedp
    MEDP<- list(value=medp,lin=linmedp)
    ARPT<- list(value = arpt, lin= linarpt)
    list_all<- list(ARPT=ARPT, MEDP=MEDP)
    # linearize RMPG
    RMPG<- contrastinf(quote((ARPT-MEDP)/ARPT), list_all)
    rval <- RMPG$value
    infun <- unlist( RMPG$lin)
    variance <- ( SE_lin2( infun , full_design ) )^2
    colnames( variance ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]

    names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- "cvystat"
    attr( rval , "var" ) <- variance
    attr(rval, "lin") <- infun
    attr( rval , "statistic" ) <- "rmpg"
    rval
}

#' @rdname svyrmpg
#' @export
svyrmpg.svyrep.design <- function(formula, design, order = 0.5, percent = 0.6, ...) {

	if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

    inc <- terms.formula(formula)[[2]]
    df <- model.frame(design)
    incvar <- df[[as.character(inc)]]
    ComputeRmpg <- function(x, w, order, percent) {
        tresh <- percent * computeQuantiles(incvar, w, p = order)
        arpr <- sum((incvar <= tresh) * w)/sum(w)
        indpoor <- (x <= tresh)
        medp <- computeQuantiles(x[indpoor], w[indpoor], p = 0.5)
        1 - (medp/tresh)
    }
    ws <- weights(design, "sampling")
    rval <- ComputeRmpg(incvar, ws, order = order, percent = percent)
    ww <- weights(design, "analysis")
    qq <- apply(ww, 2, function(wi) ComputeRmpg(incvar, wi, order = order, percent = percent))
    variance <- svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)

	names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
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


