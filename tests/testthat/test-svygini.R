context("Gini output")
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

vardpoor_giniw <- lingini(Y = "eqincome", id = "IDd", weight = "rb050",
  dataset = dati)

vardest<- vardpoor_giniw$value
attributes(vardest)<- NULL
vardest<- unlist(vardest)
varse<- SE_lin2(vardpoor_giniw$lin$lin_gini, des_eusilc)
attributes(varse)<- NULL
fun_giniw <- svygini(~eqincome, design = des_eusilc)
convest<-coef(fun_giniw)
attributes(convest)<-NULL
convse<- SE(fun_giniw)
attributes(convse)<-NULL
#domain
vardpoor_ginid <- lingini(Y = "eqincome", id = "IDd", weight = "rb050", Dom = c("hsize"),    dataset = dati)
#  point estimates
vardestd<-unlist(vardpoor_ginid$value$Gini)
#  se estimates
varsed<-sapply(data.frame(vardpoor_ginid$lin)[,2:10],function(t) SE_lin2(t,des_eusilc))
attributes (varsed) <- NULL
# library convey
fun_ginid <- svyby(~eqincome, by = ~hsize, design = des_eusilc,
  FUN = svygini,deff = FALSE)
convestd<- coef(fun_ginid)
attributes(convestd) <- NULL
convsed<- SE(fun_ginid)

test_that("compare results convey vs vardpoor",{
  expect_equal(vardest[1],100*convest)
  expect_equal(varse, 100*convse)
  expect_equal(vardestd, 100*convestd)
  expect_equal(varsed, 100*convsed )

})
