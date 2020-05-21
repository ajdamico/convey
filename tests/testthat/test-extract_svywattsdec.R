context("Watts decomposition output survey.design and svyrep.design")
library(laeken)
library(survey)

data(api)
apistrat[ , sapply( apistrat, is.integer ) ] <- apply( apistrat[ , sapply( apistrat, is.integer ) ], 2, as.numeric )
dstrat1<-convey_prep(svydesign(id=~1,data=apistrat))
for ( this_thresh in c( "abs" , "relm" , "relq" ) ){
  test_that("svywattsdec works on unweighted designs",{
    svywattsdec(~api00, design=dstrat1, type_thresh= this_thresh, percent = .6, abs_thresh=600 , na.rm  = FALSE )
  })
}


test_that( "output svywattsdec" ,{
	skip_on_cran()
	
data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )
eusilc[ , sapply( eusilc, is.integer ) ] <- apply( eusilc[ , sapply( eusilc, is.integer ) ], 2, as.numeric )

# data.frame-backed design
des_eusilc <- svydesign(ids = ~rb030, strata =~db040,  weights = ~rb050, data = eusilc)
des_eusilc <- convey_prep(des_eusilc)
# data.frame-backed replicate design
des_eusilc_rep <-as.svrepdesign(des_eusilc, type= "bootstrap")
des_eusilc_rep <- convey_prep(des_eusilc_rep)

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

# create a hacky database-backed svrepdesign object
# mirroring des_eusilc_rep
dbd_eusilc_rep <-
  svrepdesign(
    weights = ~ rb050,
    repweights = des_eusilc_rep$repweights ,
    scale = des_eusilc_rep$scale ,
    rscales = des_eusilc_rep$rscales ,
    type = "bootstrap" ,
    data = "eusilc" ,
    dbtype="SQLite" ,
    dbname = dbfile ,
    combined.weights = FALSE
  )

dbd_eusilc_rep <- convey_prep( dbd_eusilc_rep )



for ( this_thresh in c( "abs" , "relm" , "relq" ) ) {

  # data.frame-backed design
  a1 <- svywattsdec( ~eqincome, design=des_eusilc, type_thresh= this_thresh, percent = .6, abs_thresh=15000 , na.rm  = FALSE )
  a2 <- svyby( ~eqincome, by = ~rb090, design = des_eusilc, FUN = svywattsdec, type_thresh= this_thresh, percent = .6, abs_thresh=15000 , na.rm  = FALSE , deff = FALSE)
  # data.frame-backed replicate design
  b1 <- svywattsdec( ~eqincome, design=des_eusilc_rep, type_thresh= this_thresh, percent = .6, abs_thresh=15000 , na.rm  = FALSE )
  b2 <- svyby( ~eqincome, by = ~rb090, design = des_eusilc_rep , FUN = svywattsdec, type_thresh= this_thresh, percent = .6, abs_thresh=15000 , na.rm  = FALSE , deff = FALSE)

  se_dif1 <- max(abs(SE(a1)-SE(b1)))
  se_diff2 <- max(abs(SE(a2)-SE(b2)))

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
    expect_lte(confint(a1)[4,1], coef(a1)[4])
    expect_gte(confint(a1)[4,2], coef(a1)[4])
    expect_lte(confint(a1)[1,1], coef(a1)[1])
    expect_gte(confint(b1)[1,2], coef(b1)[1])
    expect_lte(confint(b1)[2,1], coef(b1)[2])
    expect_gte(confint(b1)[2,2], coef(b1)[2])
    expect_lte(confint(b1)[3,1], coef(b1)[3])
    expect_gte(confint(b1)[3,2], coef(b1)[3])
    expect_lte(confint(b1)[4,1], coef(b1)[4])
    expect_gte(confint(b1)[4,2], coef(b1)[4])
    expect_equal(sum(confint(a2)[,1]<= coef(a2)),length(coef(a2)))
    expect_equal(sum(confint(a2)[,2]>= coef(a2)),length(coef(a2)))
    expect_equal(sum(confint(b2)[,1]<= coef(b2)),length(coef(b2)))
    expect_equal(sum(confint(b2)[,2]>= coef(b2)),length(coef(b2)))
  

  # compare subsetted objects to svyby objects
  sub_des <- svywattsdec( ~eqincome, design=subset(des_eusilc , rb090 == "male"), type_thresh= this_thresh, percent = .6, abs_thresh=15000 , na.rm  = FALSE )
  sby_des <- svyby( ~eqincome, by = ~rb090, design = des_eusilc, FUN = svywattsdec, type_thresh= this_thresh, percent = .6, abs_thresh=15000 , na.rm  = FALSE , deff = FALSE)
  sub_rep <- svywattsdec( ~eqincome, design=subset(des_eusilc_rep , rb090 == "male"), type_thresh= this_thresh, percent = .6, abs_thresh=15000 , na.rm  = FALSE )
  sby_rep <- svyby( ~eqincome, by = ~rb090, design = des_eusilc_rep, FUN = svywattsdec, type_thresh= this_thresh, percent = .6, abs_thresh=15000 , na.rm  = FALSE , deff = FALSE)

  # subsets equal svyby
    expect_equal(as.numeric(coef(sub_des)), as.numeric(coef(sby_des)[ grepl( "^male", names(coef(sby_des)) ) ]))
    expect_equal(as.numeric(coef(sub_rep)), as.numeric(coef(sby_rep)[ grepl( "^male", names(coef(sby_rep)) ) ]))
    expect_equal(as.numeric(SE(sub_des)), as.numeric(SE(sby_des)[1,]))
    expect_equal(as.numeric(SE(sub_rep)), as.numeric(SE(sby_rep)[1,]))

    # coefficients should match across svydesign & svrepdesign
    expect_equal(coef(sby_des), coef(sby_rep))

    # coefficients of variation should be within five percent
    cv_dif <- abs(cv(sby_des)-cv(sby_rep))
    expect_lte( max( unlist(cv_dif) ) , .05 )

  


  # database-backed design
  c1 <- svywattsdec( ~eqincome, design=dbd_eusilc, type_thresh= this_thresh, percent = .6, abs_thresh=15000 , na.rm  = FALSE )
  c2 <- svyby( ~eqincome, by = ~rb090, design=dbd_eusilc, FUN = svywattsdec, type_thresh= this_thresh, percent = .6, abs_thresh=15000 , na.rm  = FALSE , deff = FALSE)
  # database-backed svrepdesign object
  d1 <- svywattsdec( ~eqincome, design=dbd_eusilc_rep, type_thresh= this_thresh, percent = .6, abs_thresh=15000 , na.rm  = FALSE )
  d2 <- svyby( ~eqincome, by = ~rb090, design=dbd_eusilc_rep, FUN = svywattsdec, type_thresh= this_thresh, percent = .6, abs_thresh=15000 , na.rm  = FALSE , deff = FALSE)

  # database svywattsdec
    expect_equal(coef(a1), coef(c1))
    # expect_equal(rev(coef(a2)), coef(c2)) # inverted results
    expect_equal(SE(a1), SE(c1))
    # expect_equal(rev(SE(a2)), SE(c2)) # inverted results
  

  # db-svrep svywattsdec
    expect_equal(coef(b1), coef(d1))
    # expect_equal(rev(coef(b2)), coef(d2)) # inverted results
    expect_equal(SE(b1), SE(d1))
    # expect_equal(rev(SE(a2)), SE(c2)) # inverted results
  

  # compare database-backed designs to non-database-backed designs
  sub_dbd <- svywattsdec( ~eqincome, design=subset( dbd_eusilc, rb090 == "male"), type_thresh= this_thresh, percent = .6, abs_thresh=15000 , na.rm  = FALSE )
  sby_dbd <- svyby( ~eqincome, by = ~rb090, design = dbd_eusilc, FUN = svywattsdec, type_thresh= this_thresh, percent = .6, abs_thresh=15000 , na.rm  = FALSE , deff = FALSE)
  sub_dbr <- svywattsdec( ~eqincome, design=subset( dbd_eusilc_rep, rb090 == "male"), type_thresh= this_thresh, percent = .6, abs_thresh=15000 , na.rm  = FALSE )
  sby_dbr <- svyby( ~eqincome, by = ~rb090, design = dbd_eusilc_rep, FUN = svywattsdec, type_thresh= this_thresh, percent = .6, abs_thresh=15000 , na.rm  = FALSE , deff = FALSE)

  # compare database-backed designs to non-database-backed designs
  # dbi subsets equal non-dbi subsets
    expect_equal(coef(sub_des), coef(sub_dbd))
    expect_equal(coef(sub_rep), coef(sub_dbr))
    expect_equal(SE(sub_des), SE(sub_dbd))
    expect_equal(SE(sub_rep), SE(sub_dbr))
  

  # compare database-backed subsetted objects to database-backed svyby objects
  # dbi subsets equal dbi svyby
    expect_equal(as.numeric(coef(sub_dbd)), as.numeric(coef(sby_dbd[2,])) ) # inverted results!
    expect_equal(as.numeric(coef(sub_dbr)), as.numeric(coef(sby_dbr[2,])) ) # inverted results!
    expect_equal(as.numeric(SE(sub_dbd)), as.numeric(SE(sby_dbd[2,])) ) # inverted results!
    expect_equal(as.numeric(SE(sub_dbr)), as.numeric(SE(sby_dbr[2,])) ) # inverted results!
  

}

dbRemoveTable( conn , 'eusilc' )
		dbDisconnect( conn )
dbDisconnect( conn , shutdown = TRUE )

})
