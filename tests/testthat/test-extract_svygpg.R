context("Gpg output survey.design and svyrep.design")
library(vardpoor)
library(survey)
data(ses)
des_ses <- svydesign(id=~1, weights=~weights, data=ses,
  variables=~weights+sex+earningsHour+education)
des_ses <- convey_prep(des_ses)
des_ses_rep <- as.svrepdesign(des_ses, type = "bootstrap")
des_ses_rep <- convey_prep(des_ses_rep)

# database-backed design
library(MonetDBLite)
library(DBI)
dbfolder <- tempdir()
conn <- dbConnect( MonetDBLite::MonetDBLite() , dbfolder )
dbWriteTable( conn , 'ses' , ses )
dbd_ses <- svydesign(id=~1, weights=~weights, data="ses", dbname=dbfolder, dbtype="MonetDBLite")
dbd_ses <- convey_prep( dbd_ses )


a1 <- svygpg(~earningsHour, des_ses, ~sex)

a2 <- svyby(~earningsHour, by = ~education, design = des_ses, FUN = svygpg, sex=~sex, deff = FALSE)

b1 <- svygpg(~earningsHour, design = des_ses_rep, ~sex)

b2 <- svyby(~earningsHour, by = ~education, design = des_ses_rep,
  FUN = svygpg, sex=~sex, deff = FALSE)

c1 <-  svygpg(formula=~earningshour, design=dbd_ses, sex= ~sex)
c2 <- svyby(~earningsHour, by = ~education, design = dbd_ses, FUN = svygpg, sex=~sex, deff = FALSE)

dbRemoveTable( conn , 'ses' )

rel_error1 <- abs(SE(a1)-SE(b1))/SE(a1)
rel_error2 <- max(abs(SE(a2)-SE(b2))/SE(a2))

test_that("output svygpg",{
  expect_is(coef(a1),"numeric")
  expect_is(coef(a2), "numeric")
  expect_is(coef(b1),"numeric")
  expect_is(coef(b2),"numeric")
  expect_equal(coef(a1), coef(b1))
  expect_equal(coef(a2), coef(b2))
  expect_lte(rel_error1,.1)
  expect_lte(rel_error1,.2)
  expect_equal(coef(a1), coef(c1))
  expect_equal(coef(a2), coef(c2))
  expect_equal(SE(a1), SE(c1))
  expect_equal(SE(a2), SE(c2))
  expect_is(SE(a1),"numeric")
  expect_is(SE(a2), "numeric")
  expect_is(SE(b1),"numeric")
  expect_is(SE(b2),"numeric")
  expect_lte(confint(a1)[1], coef(a1))
  expect_gte(confint(a1)[2],coef(a1))
  expect_lte(confint(b1)[,1], coef(b1))
  expect_gte(confint(b1)[2], coef(b1))
  expect_equal(sum(confint(a2)[,1]<= coef(a2)),length(coef(a2)))
  expect_equal(sum(confint(a2)[,2]>= coef(a2)),length(coef(a2)))
  expect_equal(sum(confint(b2)[,1]<= coef(b2)),length(coef(b2)))
  expect_equal(sum(confint(b2)[,2]>= coef(b2)),length(coef(b2)))
})
