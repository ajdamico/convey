context("Qsr output")
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

vardpoor_qsrw <- linqsr(Y = "eqincome", id = "IDd", weight = "rb050",
  dataset = dati)
vardest<- vardpoor_qsrw$value$QSR
attributes(vardest)<- NULL
vardest<- unlist(vardest)
varse<- SE_lin2(vardpoor_qsrw$lin$lin_qsr, des_eusilc)
attributes(varse)<- NULL
fun_qsrw <-svyqsr(~eqincome, design=des_eusilc)
convest<-coef(fun_qsrw)
attributes(convest)<-NULL
convse<- SE(fun_qsrw)
attributes(convse)<-NULL
#domain
vardpoor_qsrd <- linqsr(Y = "eqincome", id = "IDd", weight = "rb050", Dom = c("hsize"),    dataset = dati)
#  point estimates
vardestd<-unlist(vardpoor_qsrd$value$QSR)
#  se estimates
varsed<-sapply(data.frame(vardpoor_qsrd$lin)[,2:10],function(t) SE_lin2(t,des_eusilc))
attributes (varsed) <- NULL
# library convey
fun_qsrd <- svyby(~eqincome, by = ~hsize, design = des_eusilc,
  FUN = svyqsr,deff = FALSE)
convestd<- coef(fun_qsrd)
attributes(convestd) <- NULL
convsed<- SE(fun_qsrd)

test_that("compare results convey vs vardpoor",{
  expect_equal(vardest,convest)
  expect_equal(varse, convse)
  expect_equal(vardestd, convestd)

  expect_equal(varsed, convsed )
})
