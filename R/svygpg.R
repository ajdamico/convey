#' Linearization of the gender pay (wage) gap
#'
#' Estimate the difference between the average gross hourly earnings of men and women expressed as a percentage of the average gross hourly earnings of men.
#'
#'
#' @param formula a formula specifying the gross hourly earnings variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design}
#' of the library survey
#'@param sex formula with a factor with labels 'male' and 'female'
#'
#'@return a list with two components: the indicator estimate \code{value}
#' and the linearized variable \code{lin}.
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
#'
#'set.seed(1)
#'y<- c(rchisq(10,10),rchisq(10,20))
#'H<- rep(c('str1','str2'),c(10,10))
#'PSU<- rep(rep(c(1,2),c(5,5)),2)
#'weights <- rep(2,20)
#'
#' # create data frame
#'testset<- data.frame(y=y,H=H,psu=PSU, w=weights)
#'testset$sex<- rep(c(1,2),c(10,10))
#'testset<- transform(testset, sex= factor(sex,labels=c('female','male')))
#'testset$im<- 1*(testset$sex=='male')
#'testset$ifm<- 1*(testset$sex=='female')
#'testset$ym<- testset$y*(testset$sex=='male')
#'testset$yfm<- testset$y*(testset$sex=='female')
#'des<- survey::svydesign(id=~psu, strata =~H, weights =~w, data=testset, nest = TRUE )
#'des <- convey_prep( des )
#'a <-survey::svytotal(~ym+yfm+im+ifm, des)
#'# using the function svycontrast from the library survey
#'survey::svycontrast(a, quote((ym/im-yfm/im)/(ym/im)))
#' # using svygpg:
#'lin_gpg<- svygpg(~y, des, ~sex)
#'
#' @export


svygpg <- function(formula, design, ...) {

    UseMethod("svygpg", design)

}

#' @rdname svygpg
#' @export
svygpg.survey.design <- function(x, design, sex, comp=TRUE,...) {

  if( is.null( attr( design , "full_design" ) ) ) stop( "you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svydesign() function." )
  wage <- terms.formula(x)[[2]]
  df <- model.frame(design)
  wage <- df[[as.character(wage)]]
  w<- weights(design)
  ind<-names(w)
  full_design <- attr( design , "full_design" )
  ncom<-names(weights(full_design))

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
  col_female <- grep("female", colnames(sex))
  col_male <- setdiff(1:2, col_female)
  # create linearization objects of totals
  INDM <-list(value = sum(sex[, col_male]*w), lin=sex[, col_male])
  INDF <- list(value = sum(sex[, col_female]*w), lin=sex[, col_female])
  TM<- list(value = sum(wage*sex[, col_male]*w), lin=wage*sex[, col_male])
  TF<- list(value = sum(wage*sex[, col_female]*w), lin=wage*sex[, col_female])
  list_all_tot <-list(INDM=INDM,INDF=INDF,TM=TM,TF=TF)
  list_all_totc<-lapply(list_all_tot, function(t){
    names(t$lin)<-ind
    list(value=t$value,lin=complete(t$lin,ncom))
  })
  IGPG<-contrastinf(quote((TM/INDM-TF/INDF)/(TM/INDM)),list_all_totc)
  infun<-IGPG$lin
  #names(infun) <- ind
  #infuncomp <- complete(infun, ncom)
  # if (comp) lin <- infuncomp else lin <- infun
  rval <- IGPG$value
  # if the class of the full_design attribute is just a TRUE, then the design is
  # already the full design.  otherwise, pull the full_design from that attribute.
  if ("logical" %in% class(attr(design, "full_design")))
    full_design <- design else full_design <- attr(design, "full_design")
  variance <- ( SE_lin2( infun , full_design ) )^2
  colnames( variance ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]

	names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
	class(rval) <- "cvystat"
  attr( rval , "var" ) <- variance
  attr(rval, "lin") <- infun
  attr( rval , "statistic" ) <- "gpg"
  rval
}


#' @rdname svygpg
#' @export
svygpg.svyrep.design <- function(x, design, sex, ...) {
    ws <- weights(design, "sampling")
    wage <- terms.formula(x)[[2]]
    df <- model.frame(design)
    wage <- df[[as.character(wage)]]
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
        gpg <- 100 * (med_men - med_fem)/med_men
        gpg
    }
    rval <- ComputeGpg(earn_hour = wage, w = ws, sex = sex)
    ww <- weights(design, "analysis")
    qq <- apply(ww, 2, function(wi) ComputeGpg(wage, wi, sex = sex))
    variance <- svrVar(qq, design$scale, design$rscales, mse = design$mse, coef = rval)

   	names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
	class(rval) <- "cvystat"
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "gqg"
    rval
}





#' @rdname svygpg
#' @export
svygpg.DBIsvydesign <- function(x, design, ...) {
    design$variables <- survey:::getvars(x, design$db$connection, design$db$tablename,
        updates = design$updates, subset = design$subset)
    NextMethod("svygpg", design)
}

