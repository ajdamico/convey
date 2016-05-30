
context("Rmir output")
library(vardpoor)
library(survey)
data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )
dati = data.frame(1:nrow(eusilc), eusilc)
colnames(dati)[1] <- "IDd"
SE_lin2 <- function(t,des){
variance<-survey::svyrecvar(t/des$prob, des$cluster,des$strata, des$fpc,postStrata = des$postStrata)
sqrt(variance)
 }

des_eusilc <- survey:::svydesign(ids = ~rb030, strata =~db040,  weights = ~rb050, data = eusilc)
des_eusilc <- convey_prep(des_eusilc)
des_eusilc_rep <- survey:::as.svrepdesign(des_eusilc, type= "bootstrap")
des_eusilc_rep <- convey_prep(des_eusilc_rep)
dati <- data.frame(IDd = 1:nrow(eusilc), eusilc)
vardpoor_rmirw <- linrmir(Y = "eqincome", id = "IDd", age = "age", weight = "rb050", dataset = dati)
vardest<- vardpoor_rmirw$value
attributes(vardest)<- NULL
vardest<- unlist(vardest)
varse <- SE_lin2 (vardpoor_rmirw$lin$lin_rmir,des_eusilc )
attributes(varse)<- NULL
fun_rmirw <- svyrmir( ~eqincome , design = des_eusilc , age = ~age , agelim = 65 )
fun_rmirw_rep<- svyrmir( ~eqincome , design = des_eusilc_rep , age = ~age , agelim = 65 )
convest<-coef(fun_rmirw)
attributes(convest)<-NULL
convse<- survey:::SE(fun_rmirw)
attributes(convse)<-NULL
#domain
vardpoor_rmird <- linrmir(Y = "eqincome", id = "IDd", age = "age", weight = "rb050", Dom = "db040",   dataset = dati, order_quant = 50)
#  point estimates
vardestd<-unlist(vardpoor_rmird$value$rmir)
#  se estimates
varsed<-sapply(data.frame(vardpoor_rmird$lin)[,2:10],function(t) SE_lin2(t,des_eusilc))
attributes (varsed) <- NULL
# library convey
fun_rmird <- survey:::svyby(~eqincome, by = ~db040, design = des_eusilc, FUN = svyrmir,
  age = ~age , agelim = 65 ,deff = FALSE)
convestd<- coef(fun_rmird)
attributes(convestd) <- NULL
convsed<- survey:::SE(fun_rmird)

test_that("compare results convey vs vardpoor",{
  expect_equal(vardest, convest)
  expect_equal(vardestd, convestd)
  expect_lte(confint(fun_rmirw)[1], coef(fun_rmirw))
  expect_gte(confint(fun_rmirw)[2],coef(fun_rmirw))
  expect_equal(coef(fun_rmirw), coef(fun_rmirw_rep))
})
