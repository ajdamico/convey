
library(vardpoor)
library(survey)

data(api)
dstrat1<-convey_prep(svydesign(id=~1,data=apistrat))
test_that("svyjdivdec works on unweighted designs",{
  svyjdiv(~api00, design=dstrat1)
})


data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )

des_eusilc <- svydesign(ids = ~rb030, strata =~db040,  weights = ~rb050, data = eusilc)
des_eusilc <- convey_prep(des_eusilc)
des_eusilc_rep_save <- des_eusilc_rep <-as.svrepdesign(des_eusilc, type= "bootstrap")
des_eusilc_rep <- convey_prep(des_eusilc_rep)

des_eusilc <- subset( des_eusilc , eqincome > 0 )
des_eusilc_rep <- subset( des_eusilc_rep , eqincome > 0 )



a1 <- svyjdiv(~eqincome, design = des_eusilc )
b1 <- svyjdivdec(~eqincome, subgroup = ~db040, design = des_eusilc )

se_dif1 <- abs(SE(a1)-SE(b1)[1])

test_that("output svyjdiv",{
  expect_is(coef(a1),"numeric")
  expect_is(coef(b1),"numeric")
  expect_equal(coef(a1)[[1]], coef(b1)[[1]])
  expect_equal(coef(b1)[[1]], coef(b1)[[2]] + coef(b1)[[3]])
  expect_equal(coef(a1)[[1]], coef(b1)[[2]] + coef(b1)[[3]])
  expect_equal(SE(a1)[[1]], SE(b1)[[1]])
})


# database-backed design
library(MonetDBLite)
library(DBI)
dbfolder <- tempdir()
conn <- dbConnect( MonetDBLite::MonetDBLite() , dbfolder )
dbWriteTable( conn , 'eusilc' , eusilc )

dbd_eusilc <-
  svydesign(
    ids = ~rb030 ,
    strata = ~db040 ,
    weights = ~rb050 ,
    data="eusilc",
    dbname=dbfolder,
    dbtype="MonetDBLite"
  )
dbd_eusilc <- convey_prep( dbd_eusilc )

dbd_eusilc <- subset( dbd_eusilc , eqincome > 0 )

c1 <- svyjdivdec( ~ eqincome , subgroup = ~db040 , design = dbd_eusilc )

dbRemoveTable( conn , 'eusilc' )

test_that("database svyjdiv",{
  expect_is(coef(c1),"numeric")
  expect_equal(coef(c1), coef(b1))
  expect_equal(SE(c1), SE(b1))
})

# compare subsetted objects to svyby objects
sub_des <- svyjdivdec( ~eqincome , subgroup = ~db040 , design = subset( des_eusilc , rb090 == "male" ) )
sby_des <- svyby( ~eqincome, by = ~rb090, design = des_eusilc, FUN = svyjdivdec , subgroup = ~db040 )
sub_rep <- svyjdivdec( ~eqincome , subgroup = ~db040 , design = subset( des_eusilc_rep , rb090 == "male" ) )
sby_rep <- svyby( ~eqincome, by = ~rb090, design = des_eusilc_rep , FUN = svyjdivdec , subgroup = ~db040 )

test_that("subsets equal svyby",{
  expect_equal(as.numeric(coef(sub_des)), as.numeric(coef(sby_des))[c(1,3,5)])
  expect_equal(as.numeric(coef(sub_rep)), as.numeric(coef(sby_rep))[c(1,3,5)])
  expect_equal(as.numeric(SE(sub_des)), as.numeric(SE(sby_des)[1,])[1:3])
  expect_equal(as.numeric(SE(sub_rep)), as.numeric(SE(sby_rep)[1,])[1:3])

  # coefficients should match across svydesign & svrepdesign
  expect_equal(as.numeric(coef(sub_des)), as.numeric(coef(sby_rep))[c(1,3,5)])

  # coefficients of variation should be within five percent
  cv_dif <- abs(cv(sub_des)-cv(sby_rep)[1,])
  expect_lte(cv_dif[[1]],5)
})




# second run of database-backed designs #

# database-backed design
library(MonetDBLite)
library(DBI)
dbfolder <- tempdir()
conn <- dbConnect( MonetDBLite::MonetDBLite() , dbfolder )
dbWriteTable( conn , 'eusilc' , eusilc )

dbd_eusilc <-
  svydesign(
    ids = ~rb030 ,
    strata = ~db040 ,
    weights = ~rb050 ,
    data="eusilc",
    dbname=dbfolder,
    dbtype="MonetDBLite"
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
    dbtype = "MonetDBLite" ,
    dbname = dbfolder ,
    combined.weights = FALSE
  )

dbd_eusilc_rep <- convey_prep( dbd_eusilc_rep )

dbd_eusilc_rep <- subset( dbd_eusilc_rep , eqincome > 0 )

sub_dbd <- svyjdivdec( ~eqincome , subgroup = ~db040, design = subset( dbd_eusilc , rb090 == "male" ) )
sub_dbr <- svyjdivdec( ~eqincome , subgroup = ~db040, design = subset( dbd_eusilc_rep , rb090 == "male" ) )

dbRemoveTable( conn , 'eusilc' )


# compare database-backed designs to non-database-backed designs
test_that("dbi subsets equal non-dbi subsets",{
  expect_equal(coef(sub_des), coef(sub_dbd))
  expect_equal(coef(sub_rep), coef(sub_dbr))
  expect_equal(SE(sub_des), SE(sub_dbd))
  expect_equal(SE(sub_rep), SE(sub_dbr))
})

