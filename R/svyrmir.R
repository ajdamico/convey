#' Relative median income ratio
#'
#' Estimates the ratio between the median income of people with age above 65 and the
#' median income of people with age below 65.
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design}
#' of the library survey
#' @param age formula defining the variable age
#' @param agelim the age cutpoint, the default is 65
#' @param order  income quantile order, usually .5
#'
#' @return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of the statistic.
#'
#' @author Djalma Pessoa and Anthony Damico
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
#' svyrmir( ~eqIncome , design = des_eusilc , age = ~age , agelim = 65 )
#'
#' # replicate-weighted design
#' des_eusilc_rep <- as.svrepdesign( des_eusilc , type = "bootstrap" )
#' des_eusilc_rep <- convey_prep( des_eusilc_rep )
#' svyrmir( ~eqIncome , design = des_eusilc_rep )
#' @export
#'



svyrmir <- function(formula, design, ...) {

  UseMethod("svyrmir", design)

}

#' @rdname svyrmir
#' @export


svyrmir.survey.design  <- function(formula, design, age, agelim, order=0.5){
  if (is.null(attr(design, "full_design")))
    stop("you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svydesign() function.")

  if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

  # if the class of the full_design attribute is just a TRUE, then the design is
  # already the full design.  otherwise, pull the full_design from that attribute.
  if ("logical" %in% class(attr(design, "full_design")))
    full_design <- design else full_design <- attr(design, "full_design")
    inc <- terms.formula(formula)[[2]]
    df <- model.frame(design)
    ind <- row.names(df)
    incvar <- df[[as.character(inc)]]
    df_full<- model.frame(full_design)
    incvec <- df_full[[as.character(inc)]]
    wf<- weights(full_design)
    htot<- h_fun(incvec,wf)
    age <-terms.formula(age)[[2]]
    agevar <- df[[as.character(inc)]]
    dsub1 <- subset(design, age < agelim )
    iquant1<- iqalpha(formula = formula, design = dsub1, order, h=htot )
    dsub2 <- subset(design, age >= agelim )
    iquant2<- iqalpha(formula = formula, design = dsub2, order, h=htot )
    # linearize ratio of medians

    MED1 <- list(value =coef(iquant1) , lin=attr(iquant1, "lin") )
    MED2 <- list(value = coef(iquant2), lin=attr(iquant2, "lin") )
    list_all<- list(MED1=MED1, MED2=MED2)
    RMED <- contrastinf(quote(MED2/MED1),list_all)
    rval <- as.vector(RMED$value)
    lin <- RMED$lin
    variance <- ( SE_lin2( lin , full_design ) )^2
    colnames( variance ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    rownames( variance ) <- names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- "cvystat"
    attr( rval , "var" ) <- variance
    attr(rval, "lin") <- lin
    attr( rval , "statistic" ) <- "rmir"
    rval
}

#' @rdname svyrmir
#' @export
#'
svyrmir.svyrep.design <- function(formula, design, order = 0.5, age, agelim, ...) {

	convey_prep_needs_to_be_run <- ( "svyrep.design" %in% class( design ) & "survey.design" %in% class( attr( design , "full_design" ) ) ) | is.null(attr(design, "full_design"))

  if (convey_prep_needs_to_be_run)
    stop("you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svrepdesign() or as.svrepdesign() functions.")

  if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

  # if the class of the full_design attribute is just a TRUE, then the design is
  # already the full design.  otherwise, pull the full_design from that attribute.
  if ("logical" %in% class(attr(design, "full_design")))
    full_design <- design else full_design <- attr(design, "full_design")

inc <- terms.formula(formula)[[2]]
df <- model.frame(design)
incvar <- df[[as.character(inc)]]
age <- terms.formula(age)[[2]]
agevar<- df[[as.character(age)]]
ws <- weights(design, "sampling")
ComputeRmir <- function(x, w, order, age, agelim) {
  indb <- age < agelim
  quant_below <- computeQuantiles(x[indb], w[indb], p = order)
  inda <-  age >= agelim
  quant_above <- computeQuantiles(x[inda], w[inda], p = order)
  quant_above/quant_below
}
rval <- ComputeRmir(x = incvar, w = ws, order = order, age= agevar, agelim = agelim)
ww <- weights(design, "analysis")
qq <- apply(ww, 2, function(wi) 0.6 * ComputeRmir(incvar, wi, order = order,
  age= agevar, agelim = agelim))
variance <- svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)

	variance <- as.matrix( variance )

	rownames( variance ) <- names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
class(rval) <- "cvystat"
attr(rval, "var") <- variance
attr(rval, "statistic") <- "rmir"
rval
}


#' @rdname svyrmir
#' @export
svyrmir.DBIsvydesign <- function(x, design, ...) {
  design$variables <- survey:::getvars(x, design$db$connection, design$db$tablename,
    updates = design$updates, subset = design$subset)
  NextMethod("svyrmir", design)
}
