context("Atk output")

skip_on_cran()


# function from the IC2 library 1.0-1
# removed from CRAN but available at
# https://cran.r-project.org/src/contrib/Archive/IC2/
calcAtkinson<-function(x, w=NULL, epsilon=1)
{
  if (epsilon<0) return(NULL)
  if (!is.numeric(x)) return(NULL)
  xNA<-sum(as.numeric(is.na(x)))
  weighted<-FALSE
  wNA<-NULL
  if (is.null(w)) w<-rep(1, length(x))
  else 
  {
    if (!is.numeric(w)) return(NULL)
    weighted<-TRUE
    wNA<-sum(as.numeric(is.na(w)))
  }
  df<-cbind("x"=x,"w"=w)
  df<-df[complete.cases(df),, drop=FALSE]
  if(nrow(df)==0) return (NULL)
  if(any(df[,"x"]<0) || sum(df[,"x"])==0) return(NULL)
  if (any(df[,"x"]==0) && epsilon==1) return(NULL)
  index<-0
  names(index)<-"Atk"
  names(epsilon)<-"epsilon"
  Atk<-list(ineq=   list(index=index,
                        parameter=epsilon),
            nas=    list(xNA=xNA, wNA=wNA, totalNA=length(x)-nrow(df)))
  class(Atk)<-"ICI"
  if(nrow(df)==1) return(Atk)
  if (epsilon==1)
  {
    w<-df[,"w"]/sum(df[,"w"])
    xMean<-weighted.mean(df[,"x"],w)
    index<-1-(prod(exp(w*log(df[,"x"])))/xMean)
  }
  else
  {
    xMean<-weighted.mean(df[,"x"], df[,"w"])
    x1<-df[,"x"]/xMean
    w<-df[,"w"]/sum(df[,"w"])
    param<-1-epsilon
    index<-1-(weighted.mean((x1^param), w)^(1/param))
  }
  names(index)<-"Atk"
  Atk[["ineq"]][["index"]]<-index
  return(Atk)
}








library(survey)
library(laeken)
data(eusilc)
dati = data.frame(IDd = seq( 10000 , 10000 + nrow( eusilc ) - 1 ) , eusilc)
dati_nz <- subset(dati, eqIncome > 0)

des_eusilc <- svydesign(ids = ~rb030, strata =~db040,  weights = ~rb050, data = eusilc)

des_eusilc <- convey_prep(des_eusilc)
convey_atk <- svyatk(~eqIncome, subset(des_eusilc, eqIncome > 0) )

IC2_atk <- calcAtkinson( x = dati_nz$eqIncome, w = dati_nz$rb050 )$ineq$index

# point estiamte
vardest <- as.numeric( IC2_atk )
convest <- as.numeric(coef(convey_atk)[1])
#domain
# IC2 point estimates
vardestd <- sapply( split(dati_nz, dati_nz$hsize), function(x){ calcAtkinson( x = x$eqIncome, w = x$rb050 )$ineq$index[[1]] } )
vardestd <- as.numeric( vardestd )

# convey point estimates
convestd <- as.numeric( coef( svyby(~eqIncome, ~factor(hsize), subset(des_eusilc, eqIncome > 0), svyatk) ) )

test_that("compare results convey vs vardpoor",{
  expect_equal(vardest,convest)
  expect_equal(vardestd, convestd)
})
