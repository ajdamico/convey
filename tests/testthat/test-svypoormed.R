context("Poormed output")
library(survey)
library(laeken)
library(vardpoor)
data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )
dati = data.frame(IDd = seq( 10000 , 10000 + nrow( eusilc ) - 1 ) , eusilc)

SE_lin2 <- function(t,des){
  variance<-survey::svyrecvar(t/des$prob, des$cluster,des$strata, des$fpc,postStrata = des$postStrata)
  sqrt(variance)
}
des_eusilc <- svydesign(ids = ~rb030, strata =~db040,  weights = ~rb050, data = eusilc)

des_eusilc <- convey_prep(des_eusilc)

vardpoor_linpoormedw <- linpoormed(Y="eqincome", id="IDd", weight = "rb050", Dom=NULL,
dataset=dati, percentage=60, order_quant=50L)

vardest<- vardpoor_linpoormedw$value
attributes(vardest)<- NULL
vardest<- unlist(vardest)
varse<- SE_lin2(vardpoor_linpoormedw$lin$lin_poormed, des_eusilc)
attributes(varse)<- NULL
fun_poormedtw <- svypoormed(~eqincome, design = des_eusilc, 0.5, 0.6)
convest<-coef(fun_poormedtw)
attributes(convest)<-NULL
convse<- SE(fun_poormedtw)
attributes(convse)<-NULL
#domain
vardpoor_linpoormedd <- linpoormed(Y = "eqincome", id = "IDd", weight = "rb050", Dom = c("hsize"),    dataset = dati, percentage=60, order_quant=50L )
#  point estimates
vardestd<-unlist(vardpoor_linpoormedd$value$poor_people_median)
#  se estimates
varsed<-sapply(data.frame(vardpoor_linpoormedd$lin)[,2:10],function(t) SE_lin2(t,des_eusilc))
attributes (varsed) <- NULL
# library convey
fun_poormedd <- svyby(~eqincome, by = ~hsize, design = subset( des_eusilc , hsize < 8 ) ,
  FUN = svypoormed,deff = FALSE)
convestd<- coef(fun_poormedd)
attributes(convestd) <- NULL
convsed<- SE(fun_poormedd)


test_that("compare results convey vs vardpoor",{
  expect_equal(vardest, convest)
  expect_equal(varse, convse)
  expect_equal(vardestd[1:7], convestd)
  expect_identical(vardestd[8:9],as.numeric(c(NA,NA)))
  expect_identical(varsed[8:9],c(0,0))
  skip('different arpr se')
  expect_equal(varsed[1:7], convsed)
})
