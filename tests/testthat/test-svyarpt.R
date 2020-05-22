context("Arpt output")
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

vardpoor_arptw <- linarpt(Y = "eqincome", id = "IDd", weight = "rb050", Dom = NULL, dataset = dati, percentage = 60, order_quant = 50L)

vardest<- vardpoor_arptw$value
attributes(vardest)<- NULL
vardest<- unlist(vardest)
varse<- SE_lin2(vardpoor_arptw$lin$lin_arpt, des_eusilc)
attributes(varse)<- NULL
fun_arptw <- svyarpt(~eqincome, design = des_eusilc, 0.5, 0.6)
convest<-coef(fun_arptw)
attributes(convest)<-NULL
convse<- SE(fun_arptw)
attributes(convse)<-NULL

#domain
vardpoor_arptd <- linarpt(Y = "eqincome", id = "IDd", weight = "rb050", Dom = "hsize",
  dataset = dati, percentage = 60, order_quant = 50L)
#  point estimates
vardestd<-unlist(vardpoor_arptd$value$threshold)
#  se estimates
varsed<-sapply(data.frame(vardpoor_arptd$lin)[,2:10],function(t) SE_lin2(t,des_eusilc))
attributes (varsed) <- NULL
# library convey
fun_arptd <- svyby(~eqincome, by = ~hsize, design = des_eusilc, FUN = svyarpt, quantiles = 0.5,percent = 0.6,deff = FALSE)
convestd<- coef(fun_arptd)
attributes(convestd) <- NULL
convsed<- SE(fun_arptd)

test_that("compare results convey vs vardpoor",{
  expect_equal(vardest, convest)
  expect_equal(varse, convse)
  expect_equal(vardestd, convestd)
  expect_equal(varsed, convsed)
})
