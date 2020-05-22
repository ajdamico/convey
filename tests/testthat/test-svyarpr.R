
context("Arpr output")
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
des_eusilc_rep <- as.svrepdesign(des_eusilc, type= "bootstrap")
des_eusilc_rep <- convey_prep(des_eusilc_rep)

vardpoor_arprw <- linarpr(Y = "eqincome", id = "IDd", weight = "rb050", Dom = NULL, dataset = dati, percentage = 60, order_quant = 50L)
vardest<- vardpoor_arprw$value
attributes(vardest)<- NULL
vardest<- unlist(vardest)
varse <- SE_lin2 (vardpoor_arprw$lin$lin_arpr,des_eusilc )
attributes(varse)<- NULL
fun_arprw <- svyarpr(~eqincome, design = des_eusilc, 0.5, 0.6)
fun_arprw_rep<- svyarpr(~eqincome, design = des_eusilc_rep, 0.5, 0.6)
convest<-coef(fun_arprw)
attributes(convest)<-NULL
convse<- SE(fun_arprw)
attributes(convse)<-NULL
#domain
vardpoor_arprd <- linarpr(Y = "eqincome", id = "IDd", weight = "rb050", Dom = "hsize",
  dataset = dati, percentage = 60, order_quant = 50L)
#  point estimates
vardestd<-unlist(vardpoor_arprd$value$arpr)
#  se estimates
varsed<-sapply(data.frame(vardpoor_arprd$lin)[,2:10],function(t) SE_lin2(t,des_eusilc))
attributes (varsed) <- NULL
# library convey
fun_arprd <- svyby(~eqincome, by = ~hsize, design = des_eusilc, FUN = svyarpr, quantiles = 0.5, percent = 0.6,deff = FALSE)
convestd<- coef(fun_arprd)
attributes(convestd) <- NULL
convsed<- SE(fun_arprd)

test_that("compare results convey vs vardpoor",{
  expect_equal(vardest,100*convest)
  expect_equal(vardestd, 100*convestd)
  expect_lte(confint(fun_arprw)[1], coef(fun_arprw))
  expect_gte(confint(fun_arprw)[2],coef(fun_arprw))
  expect_equal(coef(fun_arprw), coef(fun_arprw_rep))
  skip('density should be computed at the arpt')
  expect_equal(varse, 100*convse)
  expect_equal(varsed, 100*convsed)

})
