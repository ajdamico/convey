#' Generalized entropy index Decomposition
#'
#' Estimates the group decomposition of the generalized entropy index
#'
#' @param formula a formula specifying the income variable
#' @param subgroup a formula specifying the group variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param epsilon a parameter that determines the sensivity towards inequality in the top of the distribution. Defaults to epsilon = 1.
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
#' @seealso \code{\link{svygei}}
#'
#' @references Anthony F. Shorrocks (1984). Inequality decomposition
#' groups population subgroups. \emph{Econometrica}, v. 52, n. 6, 1984, pp. 1369-1385.
#' URL \url{http://www.jstor.org/stable/1913511}.
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
#' svygeidec( ~eqincome , ~rb090 , subset( des_eusilc, eqincome > 0 ) , epsilon = 0 )
#' svygeidec( ~eqincome , ~rb090 , subset( des_eusilc, eqincome > 0 ) , epsilon = .5 )
#' svygeidec( ~eqincome , ~rb090 , subset( des_eusilc, eqincome > 0 ) , epsilon = 1 )
#' svygeidec( ~eqincome , ~rb090 , subset( des_eusilc, eqincome > 0 ) , epsilon = 2 )
#'
#' # replicate-weighted design
#' svygeidec( ~eqincome , ~rb090 , subset( des_eusilc_rep, eqincome > 0 ) , epsilon = 0 )
#' svygeidec( ~eqincome , ~rb090 , subset( des_eusilc_rep, eqincome > 0 ) , epsilon = .5 )
#' svygeidec( ~eqincome , ~rb090 , subset( des_eusilc_rep, eqincome > 0 ) , epsilon = 1 )
#' svygeidec( ~eqincome , ~rb090 , subset( des_eusilc_rep, eqincome > 0 ) , epsilon = 2 )
#'
#' \dontrun{
#'
#' # linearized design using a variable with missings
#' sub_des_eusilc <- subset(des_eusilc, py010n > 0 | is.na(py010n) )
#' svygeidec( ~py010n , ~rb090 , sub_des_eusilc , epsilon = 0 )
#' svygeidec( ~py010n , ~rb090 , sub_des_eusilc , epsilon = 0, na.rm = TRUE )
#' svygeidec( ~py010n , ~rb090 , sub_des_eusilc , epsilon = 1 )
#' svygeidec( ~py010n , ~rb090 , sub_des_eusilc , epsilon = 1, na.rm = TRUE )
#'
#' # replicate-weighted design using a variable with missings
#' sub_des_eusilc_rep <- subset(des_eusilc_rep, py010n > 0 | is.na(py010n) )
#' svygeidec( ~py010n , ~rb090 , sub_des_eusilc_rep , epsilon = 0 )
#' svygeidec( ~py010n , ~rb090 , sub_des_eusilc_rep , epsilon = 0, na.rm = TRUE )
#' svygeidec( ~py010n , ~rb090 , sub_des_eusilc_rep , epsilon = 1 )
#' svygeidec( ~py010n , ~rb090 , sub_des_eusilc_rep , epsilon = 1, na.rm = TRUE )
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
#' svygeidec( ~eqincome , ~rb090 , subset(dbd_eusilc, eqincome > 0) , epsilon = 0 )
#' svygeidec( ~eqincome , ~rb090 , subset(dbd_eusilc, eqincome > 0) , epsilon = .5 )
#' svygeidec( ~eqincome , ~rb090 , subset(dbd_eusilc, eqincome > 0) , epsilon = 1 )
#' svygeidec( ~eqincome , ~rb090 , subset(dbd_eusilc, eqincome > 0) , epsilon = 2 )
#'
#' # database-backed linearized design using a variable with missings
#' sub_dbd_eusilc <- subset(dbd_eusilc, py010n > 0 | is.na(py010n) )
#' svygeidec( ~py010n , ~rb090 , sub_dbd_eusilc , epsilon = 0 )
#' svygeidec( ~py010n , ~rb090 , sub_dbd_eusilc , epsilon = 0, na.rm = TRUE )
#' svygeidec( ~py010n , ~rb090 , sub_dbd_eusilc , epsilon = .5 )
#' svygeidec( ~py010n , ~rb090 , sub_dbd_eusilc , epsilon = .5, na.rm = TRUE )
#' svygeidec( ~py010n , ~rb090 , sub_dbd_eusilc , epsilon = 1 )
#' svygeidec( ~py010n , ~rb090 , sub_dbd_eusilc , epsilon = 1, na.rm = TRUE )
#' svygeidec( ~py010n , ~rb090 , sub_dbd_eusilc , epsilon = 2 )
#' svygeidec( ~py010n , ~rb090 , sub_dbd_eusilc , epsilon = 2, na.rm = TRUE )
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' dbDisconnect( conn , shutdown = TRUE )
#'
#' }
#'
#' @export
svygeidec <-
  function( formula, subgroup, design,  ...) {

    if( length( attr( terms.formula( formula ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `formula=` argument" )

    if( length( attr( terms.formula( subgroup ) , "term.labels" ) ) > 1 ) stop( "convey package functions currently only support one variable in the `subgroup=` argument" )

    #if( 'epsilon' %in% names( list(...) ) && list(...)[["epsilon"]] < 0 ) stop( "epsilon= cannot be negative." )

    UseMethod("svygeidec", design)

  }

#' @rdname svygeidec
#' @export
svygeidec.survey.design <-
  function ( formula, subgroup, design, epsilon = 1, na.rm = FALSE, ... ) {

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
    }


    if ( any( incvar[ w > 0 ] <= 0, na.rm = TRUE) ) stop( paste("the GEI is undefined for incomes <= 0 if epsilon ==", epsilon) )

    if ( any( ( is.na(incvar) | is.na(grpvar ) ) & w > 0 ) ) {

      rval <- list( estimate = matrix( c( NA, NA, NA ), dimnames = list( c( "total", "within", "between" ) ) )[,] )
      names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
      attr(rval, "var") <- matrix( rep(NA,9), ncol = 3, dimnames = list( c( "total", "within", "between" ), c( "total", "within", "between" ) ) )[,]
      attr(rval, "statistic") <- "gei decomposition"
      attr(rval,"epsilon")<- epsilon
      attr(rval,"group")<- as.character( subgroup )[[2]]
      class(rval) <- c( "cvydstat" , "cvystat" , "svystat" )

      return(rval)

    }

    grpvar <- interaction( grpvar )

    # total
    ttl.gei <- calc.gei( x = incvar, weights = w, epsilon = epsilon )

    if ( epsilon == 0 ){
      ttl.lin <-
        -U_fn( incvar , w , 0 )^( -1 ) *
        log( incvar ) +
        U_fn( incvar , w ,  1 )^( -1 ) *
        incvar +
        U_fn( incvar , w , 0 )^( -1 ) *
        (
          T_fn( incvar , w , 0 ) *
            U_fn( incvar , w , 0 )^( -1 ) - 1
        )

    } else if ( epsilon == 1) {

      ttl.lin <-
        U_fn( incvar , w , 1 )^( -1 ) * incvar * log( incvar ) -
        U_fn( incvar , w , 1 )^( -1 ) * ( T_fn( incvar , w , 1 ) * U_fn( incvar , w, 1 )^( -1 ) + 1 ) * incvar +
        U_fn( incvar , w , 0 )^( -1 )

    } else {

      ttl.lin <-
        ( epsilon )^( -1 ) *
        U_fn( incvar , w , epsilon ) *
        U_fn( incvar , w , 1 )^( -epsilon ) *
        U_fn( incvar , w , 0 )^( epsilon - 2 ) -

        ( epsilon - 1 )^( -1 ) *
        U_fn( incvar , w , epsilon ) *
        U_fn( incvar , w , 1 )^( -epsilon -1 ) *
        U_fn( incvar , w , 0 )^( epsilon - 1 ) * incvar +

        ( epsilon^2 - epsilon )^( -1 ) *
        U_fn( incvar , w , 0 )^( epsilon - 1 ) *
        U_fn( incvar , w , 1 )^( -epsilon ) *
        incvar^(epsilon)

    }

    ttl.lin [ w == 0 ] <- 0

    ttl.variance <- survey::svyrecvar(ttl.lin/design$prob, design$cluster,design$strata, design$fpc, postStrata = design$postStrata)

    # within:
    grp.gei <- NULL
    grp.lin <- matrix( NA, nrow = length(incvar), ncol = length( levels( grpvar ) ) )
    grp.U_0 <- NULL
    grp.U_0.lin <- matrix( NA, nrow = length(incvar), ncol = length( levels( grpvar ) ) )
    grp.U_1 <- NULL
    grp.U_1.lin <- matrix( NA, nrow = length(incvar), ncol = length( levels( grpvar ) ) )
    grp.p <- NULL
    grp.p.lin <- matrix( NA, nrow = length(incvar), ncol = length( levels( grpvar ) ) )
    grp.g <- NULL
    grp.g.lin <- matrix( NA, nrow = length(incvar), ncol = length( levels( grpvar ) ) )


    for ( i in seq_along( levels(grpvar) ) ) {
      w_i <- w
      w_i[ grpvar != levels(grpvar)[i] ] <- 0

      grp.gei[i] <- calc.gei( x = incvar, weights = w_i, epsilon = epsilon )

      grp.U_0[i] <- sum( w_i )
      grp.U_0.lin[ , i] <- ifelse( w_i > 0 , 1 , 0 )

      grp.U_1[i] <- sum( incvar[ w_i > 0 ] * w_i[ w_i > 0 ] )
      grp.U_1.lin[,i] <- ifelse( w_i > 0 , incvar , 0)

      U_0_i <- list( value = grp.U_0[i] , lin = grp.U_0.lin[,i] )
      U_0 <- list( value = sum( w ) , lin = rep( 1 , length(incvar) ) )
      list_all <- list( U_0_i = U_0_i, U_0 = U_0 )
      grp.est <- contrastinf( quote( U_0_i / U_0 ) , list_all )
      grp.p[i] <- grp.est$value
      grp.p.lin[ , i ] <- grp.est$lin
      rm(grp.est)


      U_1_i <- list( value = grp.U_1[i] , lin = grp.U_1.lin[,i] )
      U_1 <- list( value = sum( incvar[ w > 0 ] * w[ w > 0 ] ) , lin = incvar )
      list_all <- list( U_0_i = U_0_i, U_0 = U_0, U_1_i = U_1_i, U_1 = U_1 )
      grp.est <- contrastinf( quote( (U_0_i / U_0) * ( U_1_i / U_0_i ) / ( U_1 / U_0 ) ) , list_all )
      grp.g[i] <- grp.est$value
      grp.g.lin[ , i ] <- ifelse( w_i > 0 , grp.est$lin , 0 )
      grp.g.lin[ , i ] <- grp.est$lin
      rm(grp.est)


      if ( epsilon == 0 ){
        grp.lin [ , i ] <-
          ifelse( w_i > 0 ,
                  -U_fn( incvar , w_i , 0 )^( -1 ) * log( incvar ) +
                    U_fn( incvar , w_i ,  1 )^( -1 ) * incvar +
                    U_fn( incvar , w_i , 0 )^( -1 ) * ( T_fn( incvar , w_i , 0 ) * U_fn( incvar , w_i , 0 )^( -1 ) - 1 ) ,
                  0 )

      } else if ( epsilon == 1) {

        grp.lin[ , i ] <-
          ifelse( w_i > 0 ,
                  U_fn( incvar , w_i , 1 )^( -1 ) * incvar * log( incvar ) -
                    U_fn( incvar , w_i , 1 )^( -1 ) * ( T_fn( incvar , w_i , 1 ) * U_fn( incvar , w_i, 1 )^( -1 ) + 1 ) * incvar +
                    U_fn( incvar , w_i , 0 )^( -1 ) ,
                  0 )

      } else {

        grp.lin[ , i ] <-
          ifelse( w_i > 0 ,
                  ( epsilon )^( -1 ) *
                    U_fn( incvar , w_i , epsilon ) *
                    U_fn( incvar , w_i , 1 )^( -epsilon ) *
                    U_fn( incvar , w_i , 0 )^( epsilon - 2 ) -

                    ( epsilon - 1 )^( -1 ) *
                    U_fn( incvar , w_i , epsilon ) *
                    U_fn( incvar , w_i , 1 )^( -epsilon -1 ) *
                    U_fn( incvar , w_i , 0 )^( epsilon - 1 ) * incvar +

                    ( epsilon^2 - epsilon )^( -1 ) *
                    U_fn( incvar , w_i , 0 )^( epsilon - 1 ) *
                    U_fn( incvar , w_i , 1 )^( -epsilon ) *
                    incvar^(epsilon) ,
                  0 )

      }
      rm(i, w_i)
    }

    # finish within
    wtd.gei <- NULL
    wtd.gei.lin <- matrix( NA, nrow = length(incvar), ncol = length( levels( grpvar ) ) )
    for ( i in seq_along( levels(grpvar) ) ) {
      w_i <- w
      w_i[ grpvar != levels(grpvar)[i] ] <- 0

      g_i <- list( value = grp.g[i] , lin = grp.g.lin[,i] )
      p_i <- list( value = grp.p[i] , lin = grp.p.lin[,i] )
      gei_i <- list( value = grp.gei[i] , lin = grp.lin[,i] )
      list_all <- list( g_i = g_i, p_i = p_i, gei_i = gei_i, epsilon = list( value = epsilon , lin = rep( 0, length( incvar ) ) ) )

      if ( epsilon == 0 ) {

        grp.est <- contrastinf( quote( p_i * gei_i ) , list_all )
        wtd.gei[i] <- grp.est$value
        wtd.gei.lin[ , i ] <- ifelse( w_i > 0 , grp.est$lin , 0 )
        rm(grp.est)

      } else if ( epsilon == 1 ) {

        grp.est <- contrastinf( quote( g_i * gei_i ) , list_all )
        wtd.gei[i] <- grp.est$value
        wtd.gei.lin[ , i ] <- ifelse( w_i > 0 , grp.est$lin , 0 )
        rm(grp.est)

      } else {

        grp.est <- contrastinf( quote( g_i^epsilon * p_i^(1-epsilon) * gei_i ) , list_all )
        wtd.gei[i] <- grp.est$value
        wtd.gei.lin[ , i ] <- ifelse( w_i > 0 , grp.est$lin , 0 )
        rm(grp.est)

      }


    }

    wtn.gei <- sum(wtd.gei)
    within.lin <- apply(wtd.gei.lin,1,sum)

    # between:
    btw.gei <- ttl.gei - wtn.gei
    between.lin <- ttl.lin - within.lin


    estimates <- matrix( c( ttl.gei, wtn.gei, btw.gei ), dimnames = list( c( "total", "within", "between" ) ) )[,]

    lin.matrix <- matrix( data = c(ttl.lin, within.lin, between.lin), ncol = 3, dimnames = list( NULL, c( "total", "within", "between" ) ) )
    variance <- survey::svyrecvar( lin.matrix/design$prob , design$cluster, design$strata, design$fpc, postStrata = design$postStrata )

    rval <- list( estimate = estimates )
    names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "gei decomposition"
    attr(rval,"epsilon")<- epsilon
    attr(rval,"group")<- as.character( subgroup )[[2]]
    class(rval) <- c( "cvydstat" , "cvystat" , "svystat" )

    rval

  }


#' @rdname svygeidec
#' @export
svygeidec.svyrep.design <-
  function( formula, subgroup, design, epsilon = 1, na.rm=FALSE, ...) {

    incvar <- model.frame(formula, design$variables, na.action = na.pass)[,]
    grpvar <- model.frame( subgroup, design$variables, na.action = na.pass)[,]

    if ( class(grpvar) == "labelled" ) {
      stop( "This function does not support 'labelled' variables. Try factor().")
    }

    if(na.rm){
      nas<-is.na(incvar) | is.na(grpvar)
      design<-design[!nas,]
      df <- model.frame(design)
      incvar <- incvar[!nas]
      grpvar <- grpvar[!nas]
    }

    ws <- weights(design, "sampling")

    if ( any( incvar[ ws != 0 ] <= 0, na.rm = TRUE) ) stop( paste("the GEI is undefined for zero incomes if epsilon ==", epsilon) )

    if ( any( is.na(incvar) | is.na(grpvar) ) ) {

      rval <- list( estimate = matrix( c( NA, NA, NA ), dimnames = list( c( "total", "within", "between" ) ) )[,] )
      names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
      attr(rval, "var") <- matrix( rep(NA,9), ncol = 3, dimnames = list( c( "total", "within", "between" ), c( "total", "within", "between" ) ) )[,]
      attr(rval, "statistic") <- "gei decomposition"
      attr(rval,"epsilon")<- epsilon
      attr(rval,"group")<- as.character( subgroup )[[2]]
      class(rval) <- c( "cvydstat" , "cvystat" , "svrepstat" )

      return(rval)

    }

    grpvar <- interaction( grpvar )

    ww <- weights(design, "analysis")

    # overall estimates
    mu <- sum( incvar * ws ) / sum(ws)

    # total
    ttl.gei <- calc.gei( x = incvar, weights = ws, epsilon = epsilon)
    qq.ttl.gei <- apply(ww, 2, function(wi) calc.gei(incvar, wi, epsilon = epsilon) )

    # within
    within.gei.fun <- function(f.data) {

      tt.mu <- sum( f.data[,1] * f.data[,3] ) / sum(f.data[,3])
      tt.bw <- sum( f.data[,3] )

      grp.wtd.gei <- by( data = f.data, INDICES = f.data[,2],

                         function(data = f.data) {
                           grp.mu <- sum(data[,1] * data[,3]) / sum( data[,3] )
                           grp.p <- sum(data[,3])/tt.bw
                           grp.g <- grp.p * grp.mu / tt.mu
                           grp.gei <- calc.gei( x = data[,1], weights = data[,3], epsilon = epsilon)

                           if (epsilon == 0) {
                             return( grp.p * grp.gei )
                           } else if (epsilon == 1) {
                             return( grp.g * grp.p^(1-epsilon) * grp.gei )
                           } else {
                             return( grp.g^epsilon * grp.p^(1-epsilon) * grp.gei )
                           }

                         },
                         simplify = FALSE )

      return( sum( as.numeric(grp.wtd.gei) ) )

    }

    wtn.gei <- within.gei.fun( f.data = data.frame(incvar, grpvar, ws = ws) )
    qq.wtn.gei <- apply( ww, 2, function(wi) within.gei.fun( f.data = data.frame(incvar, grpvar, ws =  wi ) ) )

    # between:
    btw.gei <- ttl.gei - wtn.gei
    qq.btw.gei <- qq.ttl.gei - qq.wtn.gei

    if ( any(is.na( c( qq.ttl.gei, qq.wtn.gei, qq.btw.gei ) ) ) ) {

      rval <- list( estimate = matrix( c( NA, NA, NA ), dimnames = list( c( "total", "within", "between" ) ) )[,] )
      names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
      attr(rval, "var") <- matrix( rep(NA,9), ncol = 3, dimnames = list( c( "total", "within", "between" ), c( "total", "within", "between" ) ) )[,]
      attr(rval, "statistic") <- "gei decomposition"
      attr(rval,"epsilon")<- epsilon
      attr(rval,"group")<- as.character( subgroup )[[2]]
      class(rval) <- c( "cvydstat" , "cvystat" , "svrepstat" )

      return(rval)

    }

    qq.matrix <- matrix( c( qq.ttl.gei, qq.wtn.gei, qq.btw.gei ), ncol = 3, dimnames = list( NULL, c( "total", "within", "between" ) ) )
    variance <- survey::svrVar( qq.matrix, design$scale, design$rscales, mse = design$mse, coef = matrix( ttl.gei, wtn.gei, btw.gei ) )

    rval <- list( estimate = matrix( c( ttl.gei, wtn.gei, btw.gei ), dimnames = list( c( "total", "within", "between" ) ) )[,] )
    names( rval ) <- strsplit( as.character( formula )[[2]] , ' \\+ ' )[[1]]
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "gei decomposition"
    attr(rval,"epsilon")<- epsilon
    attr(rval,"group")<- as.character( subgroup )[[2]]
    class(rval) <- "cvydstat"

    rval

  }


#' @rdname svygeidec
#' @export
svygeidec.DBIsvydesign <-
  function (formula, subgroup, design, ...) {

    design$variables <-
      cbind(
        getvars(formula, design$db$connection,design$db$tablename, updates = design$updates, subset = design$subset),
        getvars(subgroup, design$db$connection, design$db$tablename,updates = design$updates, subset = design$subset)
      )

    NextMethod("svygeidec", design)

  }

