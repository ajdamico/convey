context("arpr output")
library(vardpoor)
data(eusilc)
dati = data.frame(1:nrow(eusilc), eusilc)
colnames(dati)[1] <- "IDd"

des_eusilc <- survey:::svydesign(ids = ~db040, weights = ~rb050, data = eusilc)

des_eusilc <- convey_prep(des_eusilc)
dati <- data.frame(IDd = 1:nrow(eusilc), eusilc)
vardpoor_arprw <- linarpr(Y = "eqIncome", id = "IDd", weight = "rb050", Dom = NULL, dataset = dati, percentage = 60, order_quant = 50)

vardest<- vardpoor_arprw$value
attributes(vardest)<- NULL
vardest<- unlist(vardest)
varse<- SE_lin2(vardpoor_arprw$lin$lin_arpr, des_eusilc)
attributes(varse)<- NULL
fun_arprw <- svyarpr(~eqIncome, design = des_eusilc, 0.5, 0.6)
convest<-coef(fun_arprw)
attributes(convest)<-NULL
convse<- survey:::SE(fun_arprw)
attributes(convse)<-NULL
#domain
vardpoor_arprd <- linarpr(Y = "eqIncome", id = "IDd", weight = "rb050", Dom = "db040",
  dataset = dati, percentage = 60, order_quant = 50)
#  point estimates
vardestd<-unlist(vardpoor_arprd$value$arpr)
#  se estimates
varsed<-sapply(data.frame(vardpoor_arprd$lin)[,2:10],function(t) SE_lin2(t,des_eusilc))
attributes (varsed) <- NULL
# library convey
fun_arprd <- svyby(~eqIncome, by = ~db040, design = des_eusilc, FUN = svyarpr, order = 0.5, percent = 0.6,deff = FALSE)
convestd<- coef(fun_arprd)
attributes(convestd) <- NULL
convsed<- SE(fun_arprd)
test_that("compare results convey vs vardpoor",{
  expect_equal(vardest,100*convest)
  expect_equal(varse, 100*convse)
  expect_equal(vardestd, 100*convestd)
  expect_equal(varsed, 100*convsed)
})