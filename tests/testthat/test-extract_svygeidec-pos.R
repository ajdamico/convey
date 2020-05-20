
library(laeken)
library(survey)

data(api)
dstrat1<-convey_prep(svydesign(id=~1,data=apistrat))
test_that("svygeidec works on unweighted designs",{
  svygei(~api00, design=dstrat1)
})


data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )

des_eusilc <- svydesign(ids = ~rb030, strata =~db040,  weights = ~rb050, data = eusilc)
des_eusilc <- convey_prep(des_eusilc)
des_eusilc_rep_save <- des_eusilc_rep <-as.svrepdesign(des_eusilc, type= "bootstrap")
des_eusilc_rep <- convey_prep(des_eusilc_rep)

des_eusilc <- subset( des_eusilc , eqincome > 0 )
des_eusilc_rep <- subset( des_eusilc_rep , eqincome > 0 )



a1 <- svygeidec(~eqincome, subgroup = ~db040, design = des_eusilc )
a2 <- svyby(formula = ~eqincome, by =~db040 , design = des_eusilc , subgroup = ~rb090, svygeidec )
b1 <- svygeidec(~eqincome, subgroup = ~db040, design = des_eusilc_rep )
b2 <- svyby(formula = ~eqincome, by =~db040 , design = des_eusilc_rep , subgroup = ~rb090, svygeidec )


se_dif1 <- abs(SE(a1)-SE(b1))
se_dif1 <- max(abs(SE(a2)-SE(b2)))

test_that("output svygeidec",{
  expect_is(coef(a1),"numeric")
  expect_is(coef(a2), "numeric")
  expect_is(coef(b1),"numeric")
  expect_is(coef(b2),"numeric")
  expect_equal(coef(a1), coef(b1))
  expect_equal(coef(a2), coef(b2))
  expect_is(SE(a1),"numeric")
  expect_is(SE(a2), "svyby" )
  expect_is(SE(b1),"numeric")
  expect_is(SE(b2),"svyby")
  expect_lte(confint(a1)[1,1], coef(a1)[1])
  expect_gte(confint(a1)[1,2], coef(a1)[1])
  expect_lte(confint(a1)[2,1], coef(a1)[2])
  expect_gte(confint(a1)[2,2], coef(a1)[2])
  expect_lte(confint(a1)[3,1], coef(a1)[3])
  expect_gte(confint(a1)[3,2], coef(a1)[3])
  expect_lte( max( confint(a2[1,])[,1] - coef(a2[1,]) ) , 0)
  expect_gte( min( confint(a2[1,])[,2] - coef(a2[1,]) ) , 0)
  expect_lte( max( confint(a2[2,])[,1] - coef(a2[2,]) ) , 0)
  expect_gte( min( confint(a2[2,])[,2] - coef(a2[2,]) ) , 0)
  expect_lte( max( confint(a2[3,])[,1] - coef(a2[3,]) ) , 0)
  expect_gte( min( confint(a2[3,])[,2] - coef(a2[3,]) ) , 0)
  expect_lte( max( confint(a2[4,])[,1] - coef(a2[4,]) ) , 0)
  expect_gte( min( confint(a2[4,])[,2] - coef(a2[4,]) ) , 0)
  expect_lte( max( confint(a2[5,])[,1] - coef(a2[5,]) ) , 0)
  expect_gte( min( confint(a2[5,])[,2] - coef(a2[5,]) ) , 0)
  expect_lte( max( confint(a2[6,])[,1] - coef(a2[6,]) ) , 0)
  expect_gte( min( confint(a2[6,])[,2] - coef(a2[6,]) ) , 0)
  expect_lte( max( confint(a2[7,])[,1] - coef(a2[7,]) ) , 0)
  expect_gte( min( confint(a2[7,])[,2] - coef(a2[7,]) ) , 0)
  expect_lte( max( confint(b2[1,])[,1] - coef(b2[1,]) ) , 0)
  expect_gte( min( confint(b2[1,])[,2] - coef(b2[1,]) ) , 0)
  expect_lte( max( confint(b2[2,])[,1] - coef(b2[2,]) ) , 0)
  expect_gte( min( confint(b2[2,])[,2] - coef(b2[2,]) ) , 0)
  expect_lte( max( confint(b2[3,])[,1] - coef(b2[3,]) ) , 0)
  expect_gte( min( confint(b2[3,])[,2] - coef(b2[3,]) ) , 0)
  expect_lte( max( confint(b2[4,])[,1] - coef(b2[4,]) ) , 0)
  expect_gte( min( confint(b2[4,])[,2] - coef(b2[4,]) ) , 0)
  expect_lte( max( confint(b2[5,])[,1] - coef(b2[5,]) ) , 0)
  expect_gte( min( confint(b2[5,])[,2] - coef(b2[5,]) ) , 0)
  expect_lte( max( confint(b2[6,])[,1] - coef(b2[6,]) ) , 0)
  expect_gte( min( confint(b2[6,])[,2] - coef(b2[6,]) ) , 0)
  expect_lte( max( confint(b2[7,])[,1] - coef(b2[7,]) ) , 0)
  expect_gte( min( confint(b2[7,])[,2] - coef(b2[7,]) ) , 0)
})


# compare subsetted objects to svyby objects
sub_des <- svygeidec( ~eqincome , subgroup = ~db040 , design = subset( des_eusilc , rb090 == "male" ) )
sby_des <- svyby( ~eqincome, by = ~rb090, design = des_eusilc, FUN = svygeidec , subgroup = ~db040 )
sub_rep <- svygeidec( ~eqincome , subgroup = ~db040 , design = subset( des_eusilc_rep , rb090 == "male" ) )
sby_rep <- svyby( ~eqincome, by = ~rb090, design = des_eusilc_rep , FUN = svygeidec , subgroup = ~db040 )

test_that("subsets equal svyby",{
  expect_equal(as.numeric(coef(sub_des)), as.numeric(coef(sby_des[ 1, ])))
  expect_equal(as.numeric(coef(sub_rep)), as.numeric(coef(sby_rep[ 1, ])))
  expect_equal(as.numeric(SE(sub_des)), as.numeric(SE(sby_des[1,])))
  expect_equal(as.numeric(SE(sub_rep)), as.numeric(SE(sby_rep[1,])))

  # coefficients should match across svydesign & svrepdesign
  expect_equal(coef(sub_des), coef(sub_rep))
  expect_equal(coef(sby_des), coef(sby_rep))

  # coefficients of variation should be within five percent
  cv_dif <- max(unlist(abs(cv(sby_des)-cv(sby_rep))))
  expect_lte( cv_dif, 5 ) # add warning for unreliable cv
})



# database-backed design
library(RSQLite)
library(DBI)
dbfile <- tempfile()
conn <- dbConnect( RSQLite::SQLite() , dbfile )
dbWriteTable( conn , 'eusilc' , eusilc )

dbd_eusilc <-
  svydesign(
    ids = ~rb030 ,
    strata = ~db040 ,
    weights = ~rb050 ,
    data="eusilc",
    dbname=dbfile,
    dbtype="SQLite"
  )
dbd_eusilc <- convey_prep( dbd_eusilc )

dbd_eusilc <- subset( dbd_eusilc , eqincome > 0 )

# create a hacky database-backed svrepdesign object
# mirroring des_eusilc_rep_save
dbd_eusilc_rep <-
  svrepdesign(
    weights = ~ rb050,
    repweights = des_eusilc_rep_save$repweights ,
    scale = des_eusilc_rep_save$scale ,
    rscales = des_eusilc_rep_save$rscales ,
    type = "bootstrap" ,
    data = "eusilc" ,
    dbtype="SQLite" ,
    dbname = dbfile ,
    combined.weights = FALSE
  )

dbd_eusilc_rep <- convey_prep( dbd_eusilc_rep )

dbd_eusilc_rep <- subset( dbd_eusilc_rep , eqincome > 0 )

c1 <- svygeidec( ~ eqincome , subgroup = ~db040 , design = dbd_eusilc )
c2 <- svyby(formula = ~eqincome, by =~db040 , design = dbd_eusilc , subgroup = ~rb090, svygeidec )
d1 <- svygeidec( ~ eqincome , subgroup = ~db040 , design = dbd_eusilc_rep )
d2 <- svyby(formula = ~eqincome, by =~db040 , design = dbd_eusilc_rep , subgroup = ~rb090, svygeidec )

test_that("output database svygeidec",{
  expect_is(coef(c1),"numeric")
  expect_is(coef(c2), "numeric")
  expect_is(coef(d1),"numeric")
  expect_is(coef(d2),"numeric")
  expect_equal(coef(c1), coef(d1))
  expect_equal(coef(c2), coef(d2))
  expect_is(SE(c1),"numeric")
  expect_is(SE(c2), "svyby" )
  expect_is(SE(d1),"numeric")
  expect_is(SE(d2),"svyby")
  expect_lte(confint(c1)[1,1], coef(c1)[1])
  expect_gte(confint(c1)[1,2], coef(c1)[1])
  expect_lte(confint(c1)[2,1], coef(c1)[2])
  expect_gte(confint(c1)[2,2], coef(c1)[2])
  expect_lte(confint(c1)[3,1], coef(c1)[3])
  expect_gte(confint(c1)[3,2], coef(c1)[3])
  expect_lte( max( confint(c2[1,])[,1] - coef(c2[1,]) ) , 0)
  expect_gte( min( confint(c2[1,])[,2] - coef(c2[1,]) ) , 0)
  expect_lte( max( confint(c2[2,])[,1] - coef(c2[2,]) ) , 0)
  expect_gte( min( confint(c2[2,])[,2] - coef(c2[2,]) ) , 0)
  expect_lte( max( confint(c2[3,])[,1] - coef(c2[3,]) ) , 0)
  expect_gte( min( confint(c2[3,])[,2] - coef(c2[3,]) ) , 0)
  expect_lte( max( confint(c2[4,])[,1] - coef(c2[4,]) ) , 0)
  expect_gte( min( confint(c2[4,])[,2] - coef(c2[4,]) ) , 0)
  expect_lte( max( confint(c2[5,])[,1] - coef(c2[5,]) ) , 0)
  expect_gte( min( confint(c2[5,])[,2] - coef(c2[5,]) ) , 0)
  expect_lte( max( confint(c2[6,])[,1] - coef(c2[6,]) ) , 0)
  expect_gte( min( confint(c2[6,])[,2] - coef(c2[6,]) ) , 0)
  expect_lte( max( confint(c2[7,])[,1] - coef(c2[7,]) ) , 0)
  expect_gte( min( confint(c2[7,])[,2] - coef(c2[7,]) ) , 0)
  expect_lte( max( confint(d2[1,])[,1] - coef(d2[1,]) ) , 0)
  expect_gte( min( confint(d2[1,])[,2] - coef(d2[1,]) ) , 0)
  expect_lte( max( confint(d2[2,])[,1] - coef(d2[2,]) ) , 0)
  expect_gte( min( confint(d2[2,])[,2] - coef(d2[2,]) ) , 0)
  expect_lte( max( confint(d2[3,])[,1] - coef(d2[3,]) ) , 0)
  expect_gte( min( confint(d2[3,])[,2] - coef(d2[3,]) ) , 0)
  expect_lte( max( confint(d2[4,])[,1] - coef(d2[4,]) ) , 0)
  expect_gte( min( confint(d2[4,])[,2] - coef(d2[4,]) ) , 0)
  expect_lte( max( confint(d2[5,])[,1] - coef(d2[5,]) ) , 0)
  expect_gte( min( confint(d2[5,])[,2] - coef(d2[5,]) ) , 0)
  expect_lte( max( confint(d2[6,])[,1] - coef(d2[6,]) ) , 0)
  expect_gte( min( confint(d2[6,])[,2] - coef(d2[6,]) ) , 0)
  expect_lte( max( confint(d2[7,])[,1] - coef(d2[7,]) ) , 0)
  expect_gte( min( confint(d2[7,])[,2] - coef(d2[7,]) ) , 0)
})

test_that("check dbi and non-dbi svygeidec output",{
  expect_equal(coef(c1), coef(a1))
  expect_equal(coef(c2), coef(a2))
  expect_equal(coef(d1), coef(b1))
  expect_equal(coef(d2), coef(b2))
  expect_equal(SE(c1), SE(a1))
  expect_equal(SE(c2), SE(a2))
  expect_equal(SE(d1), SE(b1))
  expect_equal(SE(d2), SE(b2))
  expect_equal(cv(c1), cv(a1))
  expect_equal(cv(c2), cv(a2))
  expect_equal(cv(d1), cv(b1))
  expect_equal(cv(d2), cv(b2))
  expect_equal(confint(c1), confint(a1))
  expect_equal(confint(c2), confint(a2))
  expect_equal(confint(d1), confint(d1))
  expect_equal(confint(d2), confint(d2))
})

sub_dbd <- svygeidec( ~eqincome , subgroup = ~db040, design = subset( dbd_eusilc , rb090 == "male" ) )
sub_dbr <- svygeidec( ~eqincome , subgroup = ~db040, design = subset( dbd_eusilc_rep , rb090 == "male" ) )
sby_dbd <- svyby(formula = ~eqincome, by =~rb090 , design = dbd_eusilc , subgroup = ~db040, svygeidec )
sby_dbr <- svyby(formula = ~eqincome, by =~rb090 , design = dbd_eusilc_rep , subgroup = ~db040, svygeidec )

# compare database-backed designs to non-database-backed designs
test_that("dbi subsets equal non-dbi subsets",{
  expect_equal(coef(sub_des), coef(sub_dbd))
  expect_equal(coef(sub_rep), coef(sub_dbr))
  expect_equal(SE(sub_des), SE(sub_dbd))
  expect_equal(SE(sub_rep), SE(sub_dbr))
  expect_equal(confint(sub_des), confint(sub_dbd))
  expect_equal(confint(sub_rep), confint(sub_dbr))
})

# compare database-backed subsetted objects to database-backed svyby objects
test_that("dbi subsets equal dbi svyby",{
  expect_equal(as.numeric(coef(sub_dbd)), as.numeric(coef(sby_dbd[2,])) ) # inverted results!
  expect_equal(as.numeric(coef(sub_dbr)), as.numeric(coef(sby_dbr[2,])) ) # inverted results!
  expect_equal(as.numeric(SE(sub_dbd)), as.numeric(SE(sby_dbd[2,])) ) # inverted results!
  expect_equal(as.numeric(SE(sub_dbr)), as.numeric(SE(sby_dbr[2,])) ) # inverted results!
})


dbRemoveTable( conn , 'eusilc' )
		dbDisconnect( conn )

