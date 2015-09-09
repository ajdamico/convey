#' Linearization of the gender pay (wage) gap
#'
#' Estimate the difference between the average gross hourly earnings of men and women expressed as a percentage of the average gross hourly earnings of men.
#'
#'
#' @param formula a formula specifying the gross hourly earnings variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design}
#' of the library survey
#'@param sex formula with a factor with labels "male" and "female"
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
#' @export


svygpg <-function( formula , design , ... ){

  UseMethod( "svygpg" , design )

}
#' @export


svygpg.survey.design<- function(x, design, sex){
  wage <- terms.formula(x)[[2]]
  df <- model.frame(design)
  wage<-df[[as.character(wage)]]
  design<- update(design, one =rep(1,length(wage)))
  # sex factor
  mf<-model.frame(sex,design$variables,na.action=na.pass)
  xx<-lapply(attr(terms(sex),"variables")[-1],
    function(tt) model.matrix(eval(bquote(~0+.(tt))),mf))
  cols<-sapply(xx,NCOL)
  sex<-matrix(nrow=NROW(xx[[1]]),ncol=sum(cols))
  scols<-c(0,cumsum(cols))
  for(i in 1:length(xx)){
    sex[,scols[i]+1:cols[i]]<-xx[[i]]
  }
  colnames(sex)<-do.call("c",lapply(xx,colnames))
  sex<-as.matrix(sex)
  col_female<- grep("female", colnames(sex))
  col_male<-setdiff(1:2,col_female)
  earnhem<- wage*sex[,col_male]
  earnhf<- wage*sex[,col_female]
  indm<- sex[,col_male]
  indf<- 1*sex[,col_female]
  design<- update(design, earnhem=earnhem, earnhf=earnhf, indm=indm,indf=indf)
  imean1<- ratio_inf(itot(~earnhem,design=design),itot(~indm,design=design))
  imean2<- ratio_inf(itot(~earnhf,design=design),itot(~indf,design=design))
  NUM<- cl_inf(1,-1, imean1,imean2)
  DEN<- imean1
  igpg<- ratio_inf(NUM,DEN)
  list(value=igpg$value,lin=igpg$lin )
}

#' @export
svygpg.svyrep.design<- function(x, design, sex){
  ws <- weights(design, "sampling")
  wage <- terms.formula(x)[[2]]
  df <- model.frame(design)
  wage<-df[[as.character(wage)]]
  design<- update(design, one =rep(1,length(wage)))
  # sex factor
  mf<-model.frame(sex,design$variables,na.action=na.pass)
  xx<-lapply(attr(terms(sex),"variables")[-1],
    function(tt) model.matrix(eval(bquote(~0+.(tt))),mf))
  cols<-sapply(xx,NCOL)
  sex<-matrix(nrow=NROW(xx[[1]]),ncol=sum(cols))
  scols<-c(0,cumsum(cols))
  for(i in 1:length(xx)){
    sex[,scols[i]+1:cols[i]]<-xx[[i]]
  }
  colnames(sex)<-do.call("c",lapply(xx,colnames))
  sex<-as.matrix(sex)
  ComputeGpg<- function(earn_hour,w, sex){
    col_female<- grep("female", colnames(sex))
    col_male<-setdiff(1:2,col_female)
    ind_men<- sex[,col_male]
    ind_fem<- sex[,col_female]
    med_men<-sum(ind_men*earn_hour*w)/sum(ind_men*w)
    med_fem<-sum(ind_fem*earn_hour*w)/sum(ind_fem*w)
    gpg<- 100*(med_men-med_fem)/med_men
    gpg
  }
  rval<- ComputeGpg(earn_hour=wage, w=ws, sex=sex)
  ww <- weights(design, "analysis")
  qq <- apply(ww, 2, function(wi) ComputeGpg(wage, wi, sex=sex))
  variance <- svrVar(qq,design$scale,design$rscales, mse = design$mse, coef = rval)
  list(value = rval, se = sqrt(variance))
}

