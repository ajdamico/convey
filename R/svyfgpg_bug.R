library(vardpoor)
data(ses)

des_ses<- svydesign(id=~1, weights=~weights, data=ses,
  variables=~weights+sex+earningsHour+education+location)
des_ses <- convey_prep( des_ses )

des_ses_rep <-as.svrepdesign( des_ses , type = "bootstrap" )
des_ses_rep<- convey_prep(des_ses_rep)

# whole population
svygpg(~earningsHour, des_ses, ~sex)

svygpg(~earningsHour, des_ses_rep, ~sex)

# by education

svyby(~earningsHour, des_ses, by=~education, FUN = svyrmpg, sex=~sex, deff = FALSE)

svyby(~earningsHour, des_ses_rep, by=~education, FUN = svyrmpg, sex=~sex, deff = FALSE)

table(ses$education)

svygpg(~earningsHour, subset(des_ses_rep, education=="ISCED 0 and 1"), ~sex)


