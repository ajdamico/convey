#' Linearization of the gender pay (wage) gap
#'
#' Estimate the difference between the average gross hourly earnings of men and women expressed as a percentage of the average gross hourly earnings of men.
#'
#'
#' @param formula a formula specifying the gross hourly earnings variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} of the library survey.  database-backed designs not supported
#' @param sex formula with a factor with labels 'male' and 'female'
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
#'library(vardpoor)
#'library(survey)
#'data(ses)
#'des_ses<- svydesign(id=~1, weights=~weights, data=ses,
#'variables=~weights+sex+earningsHour+education)
#'# linearized design
#'svygpg(~earningsHour, des_ses, ~sex)
#'# replicate-weighted design
#'des_ses_rep <-  survey:::as.svrepdesign( des_ses , type = "bootstrap" )
#'svygpg(~earningsHour, des_ses_rep, ~sex)

#' @export


svygpg <- function(formula, design, ...) {

    UseMethod("svygpg", design)

}

#' @rdname svygpg
#' @export
svygpg.survey.design <- function(formula, design, sex,  na.rm=FALSE,...) {

    wagevar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]
    if(na.rm){
      nas<-is.na(wagevar)
      design<-design[!nas,]
      if (length(nas) > length(design$prob))
        wagevar <- wagevar[!nas]
      else wagevar[nas] <- 0
    }
    w <- 1/design$prob
    ind<- names(design$prob)

  # sex factor
  mf <- model.frame(sex, design$variables, na.action = na.pass)
  xx <- lapply(attr(terms(sex), "variables")[-1], function(tt) model.matrix(eval(bquote(~0 +
      .(tt))), mf))
  cols <- sapply(xx, NCOL)
  sex <- matrix(nrow = NROW(xx[[1]]), ncol = sum(cols))
  scols <- c(0, cumsum(cols))
  for (i in 1:length(xx)) {
    sex[, scols[i] + 1:cols[i]] <- xx[[i]]
  }
  colnames(sex) <- do.call("c", lapply(xx, colnames))
  sex <- as.matrix(sex)
  if(na.rm)sex<-sex[!nas,]
  col_female <- grep("female", colnames(sex))
  col_male <- setdiff(1:2, col_female)
  # create linearization objects of totals
  INDM <-list(value = sum(sex[, col_male]*w), lin=sex[, col_male])
  INDF <- list(value = sum(sex[, col_female]*w), lin=sex[, col_female])
  TM<- list(value = sum(wagevar*sex[, col_male]*w), lin=wagevar*sex[, col_male])
  TF<- list(value = sum(wagevar*sex[, col_female]*w), lin=wagevar*sex[, col_female])
  list_all_tot <-list(INDM=INDM,INDF=INDF,TM=TM,TF=TF)
  IGPG<-contrastinf(quote((TM/INDM-TF/INDF)/(TM/INDM)),list_all_tot)
  infun<-IGPG$lin
    rval <- IGPG$value

  variance <- SE_lin2( infun , design )^2
  colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
	class(rval) <- "cvystat"
  attr( rval , "var" ) <- variance
  attr(rval, "lin") <- infun
  attr( rval , "statistic" ) <- "gpg"
  rval
}


#' @rdname svygpg
#' @export
svygpg.svyrep.design <- function(formula, design, sex,na.rm=FALSE, ...) {

    wage <- terms.formula(formula)[[2]]
    df <- model.frame(design)
    wage <- df[[as.character(wage)]]

    if(na.rm){
      nas<-is.na(wage)
      design<-design[!nas,]
      df <- model.frame(design)
      wage <- wage[!nas]
    }
    ws <- weights(design, "sampling")
    design <- update(design, one = rep(1, length(wage)))
    # sex factor
    mf <- model.frame(sex, design$variables, na.action = na.pass)
    xx <- lapply(attr(terms(sex), "variables")[-1], function(tt) model.matrix(eval(bquote(~0 +
        .(tt))), mf))
    cols <- sapply(xx, NCOL)
    sex <- matrix(nrow = NROW(xx[[1]]), ncol = sum(cols))
    scols <- c(0, cumsum(cols))
    for (i in 1:length(xx)) {
        sex[, scols[i] + 1:cols[i]] <- xx[[i]]
    }
    colnames(sex) <- do.call("c", lapply(xx, colnames))
    sex <- as.matrix(sex)
    ComputeGpg <- function(earn_hour, w, sex) {
        col_female <- grep("female", colnames(sex))
        col_male <- setdiff(1:2, col_female)
        ind_men <- sex[, col_male]
        ind_fem <- sex[, col_female]
        med_men <- sum(ind_men * earn_hour * w)/sum(ind_men * w)
        med_fem <- sum(ind_fem * earn_hour * w)/sum(ind_fem * w)
        gpg <- (med_men - med_fem)/med_men
        gpg
    }
    rval <- ComputeGpg(earn_hour = wage, w = ws, sex = sex)
    ww <- weights(design, "analysis")
    qq <- apply(ww, 2, function(wi) ComputeGpg(wage, wi, sex = sex))
    variance <- survey:::svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)

	variance <- as.matrix( variance )

   	colnames( variance ) <- rownames( variance ) <-  names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
	class(rval) <- "cvystat"
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "gqg"
    rval
}
