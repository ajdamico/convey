#' Quintile Share Ratio
#'
#' Estimate ration of the total income received by the top 20%  to the total income received by bottom 20%.
#'
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design}
#' of the library survey
#' @param alpha order of the quintile ratio
#' @param comp logical variable \code{TRUE} if the linearized variable for domains
#' should be completed with zeros
#'
#'@return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of the statistic.
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
#' svyqsr( ~eqIncome , design = des_eusilc )
#'
#' # replicate-weighted design
#' des_eusilc_rep <- as.svrepdesign( des_eusilc )
#' des_eusilc_rep <- convey_prep( des_eusilc_rep )
#' svyqsr( ~eqIncome , design = des_eusilc_rep )
#' @export
#'

svyqsr <- function(formula, design, ...) {

    UseMethod("svyqsr", design)

}

#' @rdname svyqsr
#' @export
svyqsr.survey.design <- function(formula, design, alpha = 0.2, comp=TRUE,...) {
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
    df_full<-model.frame(full_design)
    ncom<- row.names(df_full)
    incvec<-df_full[[as.character(inc)]]
    w <- weights(design)
    ind <- row.names(df)
    alpha1 <- alpha
    alpha2 <- 1 - alpha
    # Linearization of S20
    S20 <- isq(formula = formula, design = design, alpha1,compinc=TRUE)
    S20 <- list(value= S20[1], lin=attr(S20,"lin"))
    # Linearization of S80
    S80 <- isq(formula = formula, design = design, alpha2,compinc = TRUE)
    S80 <- list(value= S80[1], lin=attr(S80,"lin"))
    names(incvar)<-ind
    TOT<- list(value=sum(incvar*w), lin=incvec)
    # LINEARIZED VARIABLE OF THE SHARE RATIO

    list_all<- list(TOT=TOT, S20 = S20, S80 = S80)
    QSR <- contrastinf( quote((TOT-S80)/S20), list_all)
    rval <- QSR$value
    lin<- as.vector(QSR$lin)
    variance <- (SE_lin2(lin, full_design))^2
	colnames( variance ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]

	rownames( variance ) <- names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- "cvystat"
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "qsr"
    rval
}

#' @rdname svyqsr
#' @export
svyqsr.svyrep.design <- function(formula, design, alpha = 0.2, ...) {

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
    ComputeQsr <- function(x, w, alpha) {
        alpha1 <- alpha
        alpha2 <- 1 - alpha
        quant_inf <- computeQuantiles(x, w, p = alpha1)
        quant_sup <- computeQuantiles(x, w, p = alpha2)
        rich <- (x > quant_sup) * x
        S80 <- sum(rich * w)
        poor <- (x <= quant_inf) * x
        S20 <- sum(poor * w)
        S80/S20
    }
    ws <- weights(design, "sampling")
    rval <- ComputeQsr(incvar, ws, alpha = alpha)
    ww <- weights(design, "analysis")
    qq <- apply(ww, 2, function(wi) ComputeQsr(incvar, w = wi, alpha = alpha))
    variance <- svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)

	variance <- as.matrix( variance )

	rownames( variance ) <- names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- "cvystat"
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "qsr"
    rval
}




#' @rdname svyqsr
#' @export
svyqsr.DBIsvydesign <- function(x, design, ...) {
    design$variables <- survey:::getvars(x, design$db$connection, design$db$tablename,
        updates = design$updates, subset = design$subset)
    NextMethod("svyqsr", design)
}

