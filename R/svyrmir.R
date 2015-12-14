#' Relative median income ratio
#'
#' Estimate the ratio between the median income of people with age above 65 and the median income of people with age below 65.
#'
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} of the library survey.  database-backed designs not supported
#' @param age formula defining the variable age
#' @param agelim the age cutpoint, the default is 65
#' @param order income quantile order, usually .5
#' @param na.rm Should cases with missing values be dropped?
#'
#'@details you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
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
#'
#' svyrmir( ~eqIncome , design = des_eusilc , age = ~age , agelim = 65 )
#'
#' # replicate-weighted design
#' des_eusilc_rep <- survey:::as.svrepdesign( des_eusilc , type = "bootstrap" )
#' svyrmir( ~eqIncome , design = des_eusilc_rep, age= ~age, agelim = 65)
#'
#' # linearized design using a variable with missings
#' svyrmir( ~ py010n , design = des_eusilc,age= ~age, agelim = 65)
#' svyrmir( ~ py010n , design = des_eusilc , age= ~age, agelim = 65, na.rm = TRUE )
#' # replicate-weighted design using a variable with missings
#' svyrmir( ~ py010n , design = des_eusilc_rep,age= ~age, agelim = 65 )
#' svyrmir( ~ py010n , design = des_eusilc_rep ,age= ~age, agelim = 65, na.rm = TRUE )
#'
#' @export
#'



svyrmir <- function(formula, design, ...) {

  UseMethod("svyrmir", design)

}

#' @rdname svyrmir
#' @export


svyrmir.survey.design  <- function(formula, design, age, agelim, order=0.5, na.rm=FALSE,...){

    incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]
    agevar <- model.frame(age, design$variables, na.action = na.pass)[[1]]
    x <- cbind(incvar,agevar)
    w <- 1/design$prob
    if(na.rm){
      nas<-rowSums(is.na(x))
      design<-design[nas==0,]
      incvar <- incvar[nas==0]
      agevar <- agevar[nas==0]
      w <- w[nas==0]
    }
    N<- sum(w)
    h<- h_fun(incvar,w)
    dsub1 <- subset(design, age < agelim )
    q_alpha1 <- survey::svyquantile(x = formula, design = dsub1, quantiles = order,
      method = "constant", na.rm = na.rm)
    q_alpha1 <- as.vector(q_alpha1)
    Fprime <- densfun(formula = formula, design = design, q_alpha1, h=h, fun = "F", na.rm=na.rm)

    linquant1<- -(1/(N * Fprime)) * ((incvar <= q_alpha1) - order)
    dsub2 <- subset(design, age >= agelim )
    q_alpha2 <- survey::svyquantile(x = formula, design = dsub2, quantiles = order,
          method = "constant", na.rm = na.rm)
    q_alpha2 <- as.vector(q_alpha2)
    Fprime <- densfun(formula = formula, design = design, q_alpha2, h=h, fun = "F", na.rm=na.rm)

    linquant2<- -(1/(N * Fprime)) * ((incvar <= q_alpha2) - order)
      # linearize ratio of medians

    MED1 <- list(value =q_alpha1 , lin=linquant1 )
    MED2 <- list(value = q_alpha2, lin=linquant2 )
    list_all<- list(MED1=MED1, MED2=MED2)
    RMED <- contrastinf(quote(MED2/MED1),list_all)
    rval <- as.vector(RMED$value)
    lin <- RMED$lin
    variance <- (SE_lin2( lin , design ) )^2
    colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    class(rval) <- "cvystat"
    attr( rval , "var" ) <- variance
    attr(rval, "lin") <- lin
    attr( rval , "statistic" ) <- "rmir"
    rval
}

#' @rdname svyrmir
#' @export
#'
svyrmir.svyrep.design <- function(formula, design, order = 0.5, age, agelim,na.rm=FALSE,...) {

df <- model.frame(design)
incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]
agevar <- model.frame(age, design$variables, na.action = na.pass)[[1]]
x <- cbind(incvar,agevar)
if(na.rm){
  nas<-rowSums(is.na(x))
  design<-design[nas==0,]
  df <- model.frame(design)
  incvar <- incvar[nas==0]
  agevar<- agevar[nas==0]
}

ComputeRmir <- function(x, w, order, age, agelim) {
  indb <- age < agelim
  quant_below <- computeQuantiles(x[indb], w[indb], p = order)
  inda <-  age >= agelim
  quant_above <- computeQuantiles(x[inda], w[inda], p = order)
  quant_above/quant_below
}
ws <- weights(design, "sampling")
rval <- ComputeRmir(x = incvar, w = ws, order = order, age= agevar, agelim = agelim)
ww <- weights(design, "analysis")
qq <- apply(ww, 2, function(wi) ComputeRmir(incvar, wi, order = order,
  age= agevar, agelim = agelim))
variance <- survey:::svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)

	variance <- as.matrix( variance )

	colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
class(rval) <- "cvystat"
attr(rval, "var") <- variance
attr(rval, "statistic") <- "rmir"
rval
}
