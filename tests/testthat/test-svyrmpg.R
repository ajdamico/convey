context("Rmpg output")
library(laeken)
library(survey)
library(vardpoor)
data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )
dati = data.frame(IDd = seq( 10000 , 10000 + nrow( eusilc ) - 1 ) , eusilc)

SE_lin2 <- function(t,des){
  variance<-survey::svyrecvar(t/des$prob, des$cluster,des$strata, des$fpc,postStrata = des$postStrata)
  sqrt(variance)
}
des_eusilc <- svydesign(ids = ~rb030, strata =~db040,  weights = ~rb050, data = eusilc)

des_eusilc <- convey_prep(des_eusilc)

vardpoor_linrmpgw <- linrmpg(Y="eqincome", id="IDd", weight = "rb050", Dom=NULL,
  dataset=dati, percentage=60, order_quant=50L)
vardest<- vardpoor_linrmpgw$value
attributes(vardest)<- NULL
vardest<- unlist(vardest)
varse<- SE_lin2(vardpoor_linrmpgw$lin$lin_rmpg, des_eusilc)
attributes(varse)<- NULL
fun_svyrmpgw <- svyrmpg(~eqincome, design = des_eusilc, 0.5, 0.6)
convest<-coef(fun_svyrmpgw)
attributes(convest)<-NULL
convse<- SE(fun_svyrmpgw)
attributes(convse)<-NULL
#domain
vardpoor_rmpgd <- linrmpg(Y = "eqincome", id = "IDd", weight = "rb050", Dom = c("hsize"),    dataset = dati)
#  point estimates
vardestd<-unlist(vardpoor_rmpgd$value$rmpg)
#  se estimates
varsed<-sapply(data.frame(vardpoor_rmpgd$lin)[,2:10],function(t) SE_lin2(t,des_eusilc))
attributes (varsed) <- NULL
# library convey

fun_rmpgd <- svyby(~eqincome, by = ~hsize, design = subset(des_eusilc,hsize<8),
FUN = svyrmpg, deff = FALSE)

convestd<- coef(fun_rmpgd)
attributes(convestd) <- NULL
convsed<- SE(fun_rmpgd)



test_that("compare results convey vs vardpoor",{
  expect_equal(vardest,100*convest)
  expect_equal(varse, 100*convse)
  expect_equal(vardestd[1:7], 100*convestd)
  expect_equal(vardestd[8:9], as.numeric(c(NA,NA)))
  expect_equal(varsed[8:9], c(0,0) )
  skip('different arpr se')
  expect_equal(varsed[1:7], 100*convsed )
})
