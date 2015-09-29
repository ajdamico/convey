#' Gini coefficient
#'
#' Estimate the Gini coefficient which is a measure of inequalty
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class
#' \code{svyrep.design}
#' of the library survey
#' @param comp logical variable \code{TRUE} if the linearized variable for domains
#' should be completed with zeros
#'
#' @return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of
#'the statistic.
#'
#' @author Djalma Pessoa and Anthony Damico
#'
#' @seealso \code{\link{arpr}}
#'
#' @references Guillaume Osier (2009). Variance estimation for complex indicators
#'of poverty and inequality. \emph{Journal of the European Survey Research
#' Association}, Vol.3, No.3, pp. 167-195,
#' ISSN 1864-3361, URL \url{http://ojs.ub.uni-konstanz.de/srm/article/view/369}.

#'Jean-Claude Deville (1999). Variance estimation for complex statistics and estimators:
#' linearization and residual techniques. Survey Methodology, 25, 193-203,
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
#' gini_eqIncome <- svygini(~eqIncome, design=des_eusilc)
#'
#' @export

svygini <- function(formula, design, ...) {

    UseMethod("svygini", design)

}



#' @rdname svygini
#' @export
svygini.survey.design <- function(formula, design, ncom, comp = TRUE, ...) {
  if (is.null(attr(design, "full_design")))
    stop("you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svydesign() function.")

  	if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )


  # if the class of the full_design attribute is just a TRUE, then the design is
  # already the full design.  otherwise, pull the full_design from that attribute.
  if ("logical" %in% class(attr(design, "full_design")))
    full_design <- design else full_design <- attr(design, "full_design")
    ncom <- names(weights(full_design))
    inc <- terms.formula(formula)[[2]]
    w <- weights(design)
    ind <- names(w)
    df <- model.frame(design)
    incvar <- df[[as.character(inc)]]
    w <- w[order(incvar)]
    incvar <- incvar[order(incvar)]
    # population size
    N <- sum(w)
    # total income
    Y <- sum(incvar * w)
    # cumulative weight
    r <- cumsum(w)
    # partial weighted function
    G <- cumsum(incvar * w)
    T2<- list(value=sum(incvar*w), lin=incvar)
    T3<- list(value= sum(w), lin=rep(1, length(incvar)))
    # get T1
    T1val<- sum(r*incvar*w)
    T1lin<-  Y - G + incvar * w + r* incvar
    T1<- list(value=T1val, lin=T1lin)
    list_all<- list(T1 = T1, T2 = T2, T3 = T3)
    GINI<- contrastinf(quote((2*T1-T2)/(T2*T3)-1), list_all)
    lingini <- as.vector(GINI$lin)
    # complete with 0
    names(lingini) <- names(w)
    if (comp) lingini<-complete(lingini, ncom)

    rval <- GINI$value

    variance <- (SE_lin2(lingini, full_design))^2
	colnames( variance ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]

	names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
	class(rval) <- "cvystat"
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "gini"
    attr(rval,"lin")<- lingini
    rval
}

#' @rdname svygini
#' @export
svygini.svyrep.design <- function(formula, design, ...) {

	if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

    inc <- terms.formula(formula)[[2]]
    df <- model.frame(design)
    incvar <- df[[as.character(inc)]]
    ComputeGini <- function(x, w) {
        w <- w[order(x)]
        x <- x[order(x)]
        N <- sum(w)
        n <- length(x)
        T <- sum(x * w)
        r <- cumsum(w)
        Num <- sum((2 * r - 1) * x * w)
        Den <- N * T
        (Num/Den) - 1
    }
    ws <- weights(design, "sampling")
    rval <- ComputeGini(incvar, ws)
    ww <- weights(design, "analysis")
    qq <- apply(ww, 2, function(wi) ComputeGini(incvar, wi))
    variance <- svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)

	names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- "cvystat"
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "gini"
    rval
}




#' @rdname svygini
#' @export
svygini.DBIsvydesign <- function(x, design, ...) {
    design$variables <- survey:::getvars(x, design$db$connection, design$db$tablename,
        updates = design$updates, subset = design$subset)
    NextMethod("svygini", design)
}

