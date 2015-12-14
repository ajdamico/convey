#' Linearization of the total below a quantile
#'
#' Computes the linearized variable of the total in the lower tail of
#' the distribution of a variable.
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design}
#' of the library survey.  database-backed designs not supported
#' @param alpha the order of the quantile
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
#' des_eusilc <- svydesign(ids = ~rb030, strata =~db040,  weights = ~rb050, data = eusilc)
#' svyisq(~eqIncome, design=des_eusilc,.20)
#' # replicate-weighted design
#' des_eusilc_rep <- survey:::as.svrepdesign( des_eusilc , type = "bootstrap" )
#' svyisq( ~eqIncome , design = des_eusilc_rep, .20 )
#'# linearized design using a variable with missings
#' svyisq( ~ py010n , design = des_eusilc, .20 )
#' svyisq( ~ py010n , design = des_eusilc , .20, na.rm = TRUE )
#' # replicate-weighted design using a variable with missings
#' svyisq( ~ py010n , design = des_eusilc_rep, .20 )
#' svyisq( ~ py010n , design = des_eusilc_rep , .20,  na.rm = TRUE )
#'
#' @export

svyisq <- function(formula, design, ...) {

  UseMethod("svyisq", design)

}

#' @rdname svyisq
#' @export

svyisq.survey.design <- function(formula, design, alpha, na.rm = FALSE,...) {
  incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

  if(na.rm){
    nas<-is.na(incvar)
    design<-design[!nas,]
    if (length(nas) > length(design$prob))
      incvar <- incvar[!nas]
    else incvar[nas] <- 0
  }
  ind <- names(design$prob)
  w <- 1/design$prob
  N<- sum(w)
  h <- h_fun(incvar, w)
  q_alpha <- survey::svyquantile(x = formula, design = design, quantiles = alpha,
    method = "constant", na.rm = na.rm)
  q_alpha <- as.vector(q_alpha)
  Fprime <- densfun(formula = formula, design = design, q_alpha, h=h, fun = "F", na.rm=na.rm)
  iq <- -(1/(N * Fprime)) * ((incvar <= q_alpha) - alpha)
  rval <- sum((incvar<=q_alpha)*incvar * w)
  Fprime <- densfun(formula = formula, design = design, q_alpha, fun = "S", na.rm = na.rm)
  isqalpha1<- incvar * ((incvar <= q_alpha))
  isqalpha <- isqalpha1 + Fprime * iq
  variance <- SE_lin2(isqalpha, design)^2
  class(rval) <- "cvystat"
  attr(rval, "var") <- variance
  attr(rval, "statistic") <- "isq"
  attr(rval, "lin") <- isqalpha
  rval
}

#' @rdname svyisq
#' @export
svyisq.svyrep.design <- function(formula, design, alpha, na.rm = FALSE,...){
    incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]
    if(na.rm){
      nas<-is.na(incvar)
      design<-design[!nas,]
      if (length(nas) > length(design$prob))
        incvar <- incvar[!nas]
      else incvar[nas] <- 0
    }
    compute_isq<- function(x, w, alpha){
      q_alpha<-computeQuantiles(x=x, w=w, p = alpha)
      sum(x*(x<=q_alpha)*w)
    }
    rval<-compute_isq(incvar, alpha = alpha, w = weights(design, "sampling"))
    ww <- weights(design, "analysis")
    qq <- apply(ww, 2, function(wi) compute_isq(incvar, wi, alpha = alpha))
    variance <- survey:::svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)
    variance <- as.matrix( variance )
    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- "cvystat"
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "isq"
    rval
  }



