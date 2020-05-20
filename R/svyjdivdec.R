#' J-Divergence Decomposition (EXPERIMENTAL)
#'
#' Estimates the group decomposition of the generalized entropy index
#'
#' @param formula a formula specifying the income variable
#' @param subgroup a formula specifying the group variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param na.rm Should cases with missing values be dropped? Observations containing missing values in income or group variables will be dropped.
#' @param ... future expansion
#'
#' @details you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
#'
#' This measure only allows for strictly positive variables.
#'
#' @return Object of class "\code{cvydstat}", which are vectors with a "\code{var}" attribute giving the variance-covariance matrix and a "\code{statistic}" attribute giving the name of the statistic.
#'
#' @author Guilherme Jacob, Djalma Pessoa and Anthony Damico
#'
#' @note This function is experimental and is subject to change in later versions.
#'
#' @seealso \code{\link{svyjdiv}}
#'
#' @references Anthony F. Shorrocks (1984). Inequality decomposition
#' by population subgroups. \emph{Econometrica}, v. 52, n. 6, 1984, pp. 1369-1385.
#' URL \url{http://www.jstor.org/stable/1913511}.
#'
#' Nicholas Rohde (2016). J-divergence measurements of economic inequality.
#' J. R. Statist. Soc. A, v. 179, Part 3 (2016), pp. 847-870.
#' URL \url{http://onlinelibrary.wiley.com/doi/10.1111/rssa.12153/abstract}.
#'
#' Martin Biewen and Stephen Jenkins (2002). Estimation of Generalized Entropy
#' and Atkinson Inequality Indices from Complex Survey Data. \emph{DIW Discussion Papers},
#' No.345,
#' URL \url{https://www.diw.de/documents/publikationen/73/diw_01.c.40394.de/dp345.pdf}.
#'
#' @keywords survey
#'
#' @examples
#' library(survey)
#' library(laeken)
#' data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )
#'
#' # linearized design
#' des_eusilc <- svydesign( ids = ~rb030 , strata = ~db040 ,  weights = ~rb050 , data = eusilc )
#' des_eusilc <- convey_prep(des_eusilc)
#'
#' # replicate-weighted design
#' des_eusilc_rep <- as.svrepdesign( des_eusilc , type = "bootstrap" )
#' des_eusilc_rep <- convey_prep(des_eusilc_rep)
#'
#' # linearized design
#' svyjdivdec( ~eqincome , ~rb090 , subset(des_eusilc, eqincome > 0) )
#'
#' # replicate-weighted design
#' svyjdivdec( ~eqincome , ~rb090 , subset(des_eusilc_rep, eqincome > 0) )
#'
#' \dontrun{
#'
#' # linearized design using a variable with missings
#' sub_des_eusilc <- subset(des_eusilc, py010n > 0 | is.na(py010n) )
#' svyjdivdec( ~py010n , ~rb090 , sub_des_eusilc )
#' svyjdivdec( ~py010n , ~rb090 , sub_des_eusilc , na.rm = TRUE )
#'
#' # replicate-weighted design using a variable with missings
#' sub_des_eusilc_rep <- subset(des_eusilc_rep, py010n > 0 | is.na(py010n) )
#' svyjdivdec( ~py010n , ~rb090 , sub_des_eusilc_rep )
#' svyjdivdec( ~py010n , ~rb090 , sub_des_eusilc_rep , na.rm = TRUE )
#'
#' # database-backed design
#' library(RSQLite)
#' library(DBI)
#' dbfile <- tempfile()
#' conn <- dbConnect( RSQLite::SQLite() , dbfile )
#' dbWriteTable( conn , 'eusilc' , eusilc )
#'
#' dbd_eusilc <-
#' 	svydesign(
#' 		ids = ~rb030 ,
#' 		strata = ~db040 ,
#' 		weights = ~rb050 ,
#' 		data="eusilc",
#' 		dbname=dbfile,
#' 		dbtype="SQLite"
#' 	)
#'
#' dbd_eusilc <- convey_prep( dbd_eusilc )
#'
#' # database-backed linearized design
#' svyjdivdec( ~eqincome , ~rb090 , subset(dbd_eusilc, eqincome > 0) )
#'
#' # database-backed linearized design using a variable with missings
#' sub_dbd_eusilc <- subset(dbd_eusilc, py010n > 0 | is.na(py010n) )
#' svyjdivdec( ~py010n , ~rb090 , sub_dbd_eusilc )
#' svyjdivdec( ~py010n , ~rb090 , sub_dbd_eusilc , na.rm = TRUE )
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' dbDisconnect( conn , shutdown = TRUE )
#'
#' }
#'
#' @export
svyjdivdec <-
  function( formula, subgroup, design, ...) {

    if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

    if( length( attr( terms.formula( subgroup ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `subgroup=` argument" )

    warning("The svyjdivdec function is experimental and is subject to changes in later versions.")

    UseMethod("svyjdivdec", design)

  }

#' @rdname svyjdivdec
#' @export
svyjdivdec.survey.design <-
  function ( formula, subgroup, design, na.rm = FALSE, ... ) {

    w <- 1/design$prob
    incvar <- model.frame(formula, design$variables, na.action = na.pass)[,]
    grpvar <- model.frame( subgroup, design$variables, na.action = na.pass)[,]

    if ( class(grpvar) == "labelled" ) {
      stop( "This function does not support 'labelled' variables. Try factor().")
    }

    if (na.rm) {
      nas <- ( is.na(incvar) | is.na(grpvar ) )
      design <- design[nas == 0, ]
      w <- 1/design$prob
      incvar <- model.frame(formula, design$variables, na.action = na.pass)[,]
      grpvar <- model.frame( subgroup, design$variables, na.action = na.pass)[,]
    }


    if ( any( incvar[ w != 0 ] <= 0, na.rm = TRUE) ) stop( "The J-divergence index is defined for strictly positive incomes only." )

    incvar <- incvar[ w > 0 ]
    grpvar <- grpvar[ w > 0 ]
    w <- w[ w > 0 ]

    if ( any( any( is.na(incvar) | is.na(grpvar ) ) & (w > 0) ) ) {

      rval <- list( estimate = matrix( c( NA, NA, NA ), dimnames = list( c( "total", "within", "between" ) ) )[,] )
      names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
      attr(rval, "var") <- matrix( rep(NA,9), ncol = 3, dimnames = list( c( "total", "within", "between" ), c( "total", "within", "between" ) ) )[,]
      attr(rval, "statistic") <- "j-divergence decomposition"
      attr(rval,"group")<- as.character( subgroup )[[2]]
      class(rval) <- c( "cvydstat" , "cvystat" , "svystat" )

      return(rval)

    }

    grpvar <- interaction( grpvar )

    # total
    U_0 <- list( value = sum( w ), lin = rep( 1, length( incvar ) ) )
    U_1 <- list( value = sum( w * incvar ), lin = incvar )
    T_0 <- list( value = sum( w * log( incvar ) ), lin = log( incvar ) )
    T_1 <- list( value = sum( w * incvar * log( incvar ) ), lin = incvar * log( incvar ) )

    list_all <- list(  U_0 = U_0, U_1 = U_1, T_0 = T_0, T_1 = T_1 )
    estimate <- contrastinf( quote( ( T_1 / U_1 ) - ( T_0 / U_0 ) ) , list_all )

    ttl.jdiv <- estimate$value
    ttl.jdiv.lin <- 1/design$prob
    ttl.jdiv.lin[ ttl.jdiv.lin > 0 ] <- estimate$lin

    # within:

    # Theil T index:
    grp.theilt <- NULL
    grp.theilt.lin <- matrix( NA, nrow = length(incvar), ncol = length( levels( grpvar ) ) )
    grp.theilt.wtd <- NULL
    grp.theilt.wtd.lin <- matrix( NA, nrow = length(incvar), ncol = length( levels( grpvar ) ) )

    for ( i in seq_along( levels(grpvar) ) ) {
      w_i <- w
      w_i[ grpvar != levels(grpvar)[i] ] <- 0

      U_0_i <- list( value = sum( w_i ), lin = rep( 1, length( incvar ) ) )
      U_1_i <- list( value = sum( w_i * incvar ), lin = incvar )
      T_0_i <- list( value = sum( w_i * log( incvar ) ), lin = log( incvar ) )
      T_1_i <- list( value = sum( w_i * incvar * log( incvar ) ), lin = incvar * log( incvar ) )
      Y_AVG_i <- contrastinf( quote( U_1_i / U_0_i ), list( U_0_i = U_0_i , U_1_i = U_1_i ) )

      list_all <- list(  U_0 = U_0, U_1 = U_1, T_0 = T_0, T_1 = T_1, Y_AVG_i = Y_AVG_i, U_1_i = U_1_i, T_1_i = T_1_i )
      estimate <- contrastinf( quote( ( 1 / U_1_i ) * ( T_1_i - log( Y_AVG_i ) * U_1_i ) ) , list_all )

      grp.theilt[i] <- estimate$value
      grp.theilt.lin[,i] <- estimate$lin * ( w_i != 0 )

      estimate <- contrastinf( quote( grp_theilt * U_1_i / U_1 ) , list( grp_theilt = estimate, U_1_i = U_1_i, U_1 = U_1 ) )
      grp.theilt.wtd[i] <- estimate$value
      grp.theilt.wtd.lin[,i] <- estimate$lin * ( w_i != 0 )

      rm(i, w_i, estimate)

    }

    wtn.theilt <- sum( grp.theilt.wtd )
    w_teste <- 1/design$prob
    w_teste[ w_teste > 0 ] <- apply( grp.theilt.wtd.lin, 1, sum )
    wtn.theilt.lin <- w_teste ; rm( w_teste )


    # Theil L index:
    grp.theill <- NULL
    grp.theill.lin <- matrix( NA, nrow = length(incvar), ncol = length( levels( grpvar ) ) )
    grp.theill.wtd <- NULL
    grp.theill.wtd.lin <- matrix( NA, nrow = length(incvar), ncol = length( levels( grpvar ) ) )

    for ( i in seq_along( levels(grpvar) ) ) {
      w_i <- w
      w_i[ grpvar != levels(grpvar)[i] ] <- 0

      U_0_i <- list( value = sum( w_i ), lin = rep( 1, length( incvar ) ) )
      U_1_i <- list( value = sum( w_i * incvar ), lin = incvar )
      T_0_i <- list( value = sum( w_i * log( incvar ) ), lin = log( incvar ) )
      T_1_i <- list( value = sum( w_i * incvar * log( incvar ) ), lin = incvar * log( incvar ) )
      Y_AVG_i <- contrastinf( quote( U_1_i / U_0_i ), list(  U_0_i = U_0_i, U_1_i = U_1_i ) )

      list_all <- list(  U_0 = U_0, U_1 = U_1, T_0 = T_0, T_1 = T_1, Y_AVG_i = Y_AVG_i, U_0_i = U_0_i, U_1_i = U_1_i, T_0_i = T_0_i, T_1_i = T_1_i )
      estimate <- contrastinf( quote( ( 1 / U_0_i ) * ( log( Y_AVG_i ) * U_0_i - T_0_i ) ) , list_all )

      grp.theill[i] <- estimate$value
      grp.theill.lin[,i] <- estimate$lin * ( w_i != 0 )

      estimate <- contrastinf( quote( grp_theill * U_0_i / U_0 ) , list( grp_theill = estimate, U_0_i = U_0_i, U_0 = U_0 ) )
      grp.theill.wtd[i] <- estimate$value
      grp.theill.wtd.lin[,i] <- estimate$lin * ( w_i != 0 )

      rm(i, w_i, estimate)

    }

    wtn.theill <- sum( grp.theill.wtd )
    w_teste <- 1/design$prob
    w_teste[ w_teste > 0 ] <- apply( grp.theill.wtd.lin, 1, sum )
    wtn.theill.lin <- w_teste ; rm( w_teste )

    # Within component:
    within.jdiv <- wtn.theilt + wtn.theill
    within.jdiv.lin <- wtn.theilt.lin + wtn.theill.lin

    # between:
    between.jdiv <- ttl.jdiv - within.jdiv
    between.jdiv.lin <- ttl.jdiv.lin - within.jdiv.lin

    estimates <- matrix( c( ttl.jdiv, within.jdiv, between.jdiv ), dimnames = list( c( "total", "within", "between" ) ) )[,]

    lin.matrix <- matrix( data = c(ttl.jdiv.lin, within.jdiv.lin, between.jdiv.lin), ncol = 3, dimnames = list( NULL, c( "total", "within", "between" ) ) )
    variance <- survey::svyrecvar( lin.matrix/design$prob , design$cluster, design$strata, design$fpc, postStrata = design$postStrata)

    rval <- list( estimate = estimates )
    names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    attr(rval, "var") <- variance[1:3,1:3]
    attr(rval, "statistic") <- "j-divergence decomposition"
    attr(rval,"group")<- as.character( subgroup )[[2]]
    class(rval) <- c( "cvydstat" , "cvystat" , "svystat" )

    rval

  }


#' @rdname svyjdivdec
#' @export
svyjdivdec.svyrep.design <-
  function( formula, subgroup, design, na.rm=FALSE, ...) {

    # J-divergence measure:
    calc.jdiv <-  function( x, weights ) {

      x <- x[ weights > 0]
      weights <- weights[ weights > 0]

      avg <- sum( x * weights ) / sum( weights )
      jdiv = ( ( x - avg ) / avg ) * log( x / avg )

      return( sum( jdiv * weights ) / sum( weights ) )

    }

    incvar <- model.frame(formula, design$variables, na.action = na.pass)[,]
    grpvar <- model.frame( subgroup, design$variables, na.action = na.pass)[,]

    if(na.rm){
      nas<-is.na(incvar) | is.na(grpvar)
      design<-design[!nas,]
      df <- model.frame(design)
      incvar <- incvar[!nas]
      grpvar <- grpvar[!nas]
    }

    ws <- weights(design, "sampling")

    if ( any( incvar[ ws != 0 ] <= 0, na.rm = TRUE) ) stop( "The J-divergence index is defined for strictly positive incomes." )

    if ( any( is.na(incvar) | is.na(grpvar) ) ) {

      rval <- list( estimate = matrix( c( NA, NA, NA ), dimnames = list( c( "total", "within", "between" ) ) )[,] )
      names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
      attr(rval, "var") <- matrix( rep(NA,9), ncol = 3, dimnames = list( c( "total", "within", "between" ), c( "total", "within", "between" ) ) )[,]
      attr(rval, "statistic") <- "j-divergence decomposition"
      attr(rval,"group")<- as.character( subgroup )[[2]]
      class(rval) <- c( "cvydstat" , "cvystat" , "svystat" , "svrepstat" )

      return(rval)

    }

    grpvar <- interaction( grpvar )

    ww <- weights(design, "analysis")

    # Total
    ttl.jdiv <- calc.jdiv( x = incvar, weights = ws )
    qq.ttl.jdiv <- apply(ww, 2, function(wi) calc.jdiv(incvar, wi ) )

    # within

    # Theil T index:
    grp.wtd.theilt <- NULL
    qq.grp.wtd.theilt <- matrix( NA, nrow = length(qq.ttl.jdiv), ncol = length( levels( grpvar ) ) )
    for ( i in seq_along( levels(grpvar) ) ) {
      ind <- 1*( grpvar == levels(grpvar)[i] )

      grp.theilt <- calc.gei( x = incvar, weights = ws * ind, epsilon = 1 )
      qq.grp.theilt <- apply(ww, 2, function(wi) calc.gei( incvar, wi * ind, epsilon = 1 ) )

      grp.incshr <- sum( incvar * ind * ws ) / sum( incvar * ws )
      qq.grp.incshr <- apply( ww, 2, function(wi) { sum( incvar * ind * wi ) / sum( incvar * wi ) } )

      grp.wtd.theilt[i] <- grp.theilt * grp.incshr
      qq.grp.wtd.theilt[,i] <- qq.grp.theilt * qq.grp.incshr

    }

    # Theil L index:
    grp.wtd.theill <- NULL
    qq.grp.wtd.theill <- matrix( NA, nrow = length(qq.ttl.jdiv), ncol = length( levels( grpvar ) ) )
    for ( i in seq_along( levels(grpvar) ) ) {

      ind <- 1*( grpvar == levels(grpvar)[i] )

      grp.theill <- calc.gei( x = incvar, weights = ws * ind, epsilon = 0 )
      qq.grp.theill <- apply(ww, 2, function(wi) calc.gei( incvar, wi * ind, epsilon = 0 ) )

      grp.popshr <- sum( ind * ws ) / sum( ws )
      qq.grp.popshr <- apply( ww, 2, function(wi) { sum( ind * wi ) / sum( wi ) } )

      grp.wtd.theill[i] <- grp.theill * grp.popshr
      qq.grp.wtd.theill[,i] <- qq.grp.theill * qq.grp.popshr

    }

    # Within component:
    within.jdiv <- sum( grp.wtd.theilt + grp.wtd.theill )
    qq.within.jdiv <- apply( qq.grp.wtd.theilt + qq.grp.wtd.theill, 1, sum )

    # Between:
    between.jdiv <- ttl.jdiv - within.jdiv
    qq.between.jdiv <- qq.ttl.jdiv - qq.within.jdiv



    qq.matrix <- matrix( c( qq.ttl.jdiv, qq.within.jdiv, qq.between.jdiv ), ncol = 3, dimnames = list( NULL, c( "total", "within", "between" ) ) )
    variance <- survey::svrVar( qq.matrix, design$scale, design$rscales, mse = design$mse, coef = matrix( ttl.jdiv, within.jdiv, between.jdiv ) )

    rval <- list( estimate = matrix( c( ttl.jdiv, within.jdiv, between.jdiv ), dimnames = list( c( "total", "within", "between" ) ) )[,] )
    names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    #attr(rval, "var") <- variance[1:3, 1:3]
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "j-divergence decomposition"
    attr(rval,"group")<- as.character( subgroup )[[2]]
    class(rval) <- c( "cvydstat" , "cvystat" , "svrepstat" , "svystat" )
    rval

  }


#' @rdname svyjdivdec
#' @export
svyjdivdec.DBIsvydesign <-
  function (formula, subgroup, design, ...) {

    design$variables <-
      cbind(
        getvars(formula, design$db$connection,design$db$tablename, updates = design$updates, subset = design$subset),
        getvars(subgroup, design$db$connection, design$db$tablename,updates = design$updates, subset = design$subset)
      )

    NextMethod("svyjdivdec", design)

  }

