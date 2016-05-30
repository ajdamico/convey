context("poormed output")
library(vardpoor)
data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )
dati = data.frame(1:nrow(eusilc), eusilc)
colnames(dati)[1] <- "IDd"
SE_lin2 <- function(t,des){
  variance<-survey::svyrecvar(t/des$prob, des$cluster,des$strata, des$fpc,postStrata = des$postStrata)
  sqrt(variance)
}
des_eusilc <- survey:::svydesign(ids = ~rb030, strata =~db040,  weights = ~rb050, data = eusilc)

des_eusilc <- convey_prep(des_eusilc)
dati <- data.frame(IDd = 1:nrow(eusilc), eusilc)
vardpoor_linpoormedw <- linpoormed(Y="eqincome", id="IDd", weight = "rb050", Dom=NULL,
dataset=dati, percentage=60, order_quant=50)

vardest<- vardpoor_linpoormedw$value
attributes(vardest)<- NULL
vardest<- unlist(vardest)
varse<- SE_lin2(vardpoor_linpoormedw$lin$lin_poormed, des_eusilc)
attributes(varse)<- NULL
fun_poormedtw <- svypoormed(~eqincome, design = des_eusilc, 0.5, 0.6)
convest<-coef(fun_poormedtw)
attributes(convest)<-NULL
convse<- survey:::SE(fun_poormedtw)
attributes(convse)<-NULL

test_that("compare results convey vs vardpoor",{
  expect_equal(vardest, convest)
  expect_equal(varse, convse)
})
