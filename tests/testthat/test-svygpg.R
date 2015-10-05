context("gpg output")
library(vardpoor)
data(ses)
dati <- data.table(ID = 1:nrow(ses), ses)
setnames(dati, "sex", "sexf")
dati[sexf=="male", sex:=1]
dati[sexf=="female", sex:=2]

des_ses <- survey:::svydesign(id=~1, weights=~weights, data=ses,
  variables=~weights+sex+earningsHour+education)

des_ses <- convey_prep(des_ses)
gpgs1 <- lingpg(Y="earningsHour", gender="sex", id="ID", weight="weights", dataset=dati)

vardest<- gpgs1$value
attributes(vardest)<- NULL
vardest<- unlist(vardest)
varse<- SE_lin2(gpgs1$lin$lin_gpg,des_ses)
attributes(varse)<- NULL
ses_gpg<- svygpg(~earningsHour, des_ses, ~sex)
convest<-coef(ses_gpg)
attributes(convest)<-NULL
convse<- survey:::SE(ses_gpg)
attributes(convse)<-NULL

test_that("compare results convey vs vardpoor",{
  expect_equal(vardest,100*convest)
  expect_equal(varse, 100*convse)
})

