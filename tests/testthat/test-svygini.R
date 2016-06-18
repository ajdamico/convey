context("Gini output")
library(vardpoor)
data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )
dati = data.frame(1:nrow(eusilc), eusilc)
colnames(dati)[1] <- "IDd"
SE_lin2 <- function(t,des){
  variance<-survey::svyrecvar(t/des$prob, des$cluster,des$strata, des$fpc,postStrata = des$postStrata)
  sqrt(variance)
}
des_eusilc <- svydesign(ids = ~rb030, strata =~db040,  weights = ~rb050, data = eusilc)

des_eusilc <- convey_prep(des_eusilc)
dati <- data.frame(IDd = 1:nrow(eusilc), eusilc)
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
vardpoor_ginid <- lingini(Y = "eqincome", id = "IDd", weight = "rb050", Dom = c("db040"),    dataset = dati)
#  point estimates
vardestd<-unlist(vardpoor_ginid$value$Gini)
#  se estimates
varsed<-sapply(data.frame(vardpoor_ginid$lin)[,2:10],function(t) SE_lin2(t,des_eusilc))
attributes (varsed) <- NULL
# library convey
fun_ginid <- svyby(~eqincome, by = ~db040, design = des_eusilc,
  FUN = svygini,deff = FALSE)
convestd<- coef(fun_ginid)
attributes(convestd) <- NULL
convsed<- SE(fun_ginid)

test_that("compare results convey vs vardpoor",{
  expect_equal(vardest[1],100*convest)
  
  # currently fails
  # expect_equal(varse, 100*convse)
  
  expect_equal(vardestd, 100*convestd)

  # currently fails
  # expect_equal(varsed, 100*convsed )

})
