context("gini output")
library(vardpoor)
data(eusilc)
dati = data.frame(1:nrow(eusilc), eusilc)
colnames(dati)[1] <- "IDd"

des_eusilc <- survey:::svydesign(ids = ~db040, weights = ~rb050, data = eusilc)

des_eusilc <- convey_prep(des_eusilc)
dati <- data.frame(IDd = 1:nrow(eusilc), eusilc)
vardpoor_giniw <- lingini(Y = "eqIncome", id = "IDd", weight = "rb050",
  dataset = dati)

vardest<- vardpoor_giniw$value
attributes(vardest)<- NULL
vardest<- unlist(vardest)
varse<- SE_lin2(vardpoor_giniw$lin$lin_gini, des_eusilc)
attributes(varse)<- NULL
fun_giniw <- svygini(~eqIncome, design = des_eusilc)
convest<-coef(fun_giniw)
attributes(convest)<-NULL
convse<- survey:::SE(fun_giniw)
attributes(convse)<-NULL
#domain
vardpoor_ginid <- lingini(Y = "eqIncome", id = "IDd", weight = "rb050", Dom = c("db040"),    dataset = dati)
#  point estimates
vardestd<-unlist(vardpoor_ginid$value$Gini)
#  se estimates
varsed<-sapply(data.frame(vardpoor_ginid$lin)[,2:10],function(t) SE_lin2(t,des_eusilc))
attributes (varsed) <- NULL
# library convey
fun_ginid <- survey:::svyby(~eqIncome, by = ~db040, design = des_eusilc,
  FUN = svygini,deff = FALSE)
convestd<- coef(fun_ginid)
attributes(convestd) <- NULL
convsed<- survey:::SE(fun_ginid)

test_that("compare results convey vs vardpoor",{
  expect_equal(vardest[1],100*convest)
  expect_equal(varse, 100*convse)
  expect_equal(vardestd, 100*convestd)
  expect_equal(varsed, 100*convsed )
})
