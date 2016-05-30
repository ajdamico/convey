context("gpg output survey.design and svyrep.design")
library(vardpoor)
library(survey)
data(ses)
des_ses <- survey:::svydesign(id=~1, weights=~weights, data=ses,
  variables=~weights+sex+earningsHour+education)
des_ses <- convey_prep(des_ses)
des_ses_rep <- survey:::as.svrepdesign(des_ses, type = "bootstrap")
des_ses_rep <- convey_prep(des_ses_rep)

a1 <- svygpg(~earningsHour, des_ses, ~sex)

a2 <- survey:::svyby(~earningsHour, by = ~education, design = des_ses, FUN = svygpg, sex=~sex, deff = FALSE)

b1 <- svygpg(~earningsHour, design = des_ses_rep, ~sex)

b2 <- survey:::svyby(~earningsHour, by = ~education, design = des_ses_rep,
  FUN = svygpg, sex=~sex, deff = FALSE)


test_that("output svygpg",{
  expect_is(coef(a1),"numeric")
  expect_is(coef(a2), "numeric")
  expect_is(coef(b1),"numeric")
  expect_is(coef(b2),"numeric")
  expect_equal(coef(a1), coef(b1))
  expect_equal(coef(a2), coef(b2))
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
