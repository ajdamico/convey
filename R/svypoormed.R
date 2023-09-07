#' Relative median poverty gap
#'
#' Estimate the median of incomes less than the at-risk-of-poverty threshold (\code{arpt}).
#'
#'
#' @param formula a formula specifying the income variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param quantiles income quantile, usually .5 (median)
#' @param percent fraction of the quantile, usually .60
#' @param na.rm Should cases with missing values be dropped?
#' @param ... arguments passed on to `survey::oldsvyquantile`
#'
#' @details you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
#'
#' @return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of the statistic.
#'
#' @author Djalma Pessoa and Anthony Damico
#'
#' @seealso \code{\link{svyarpt}}
#'
#' @references Guillaume Osier (2009). Variance estimation for complex indicators
#' of poverty and inequality. \emph{Journal of the European Survey Research
#' Association}, Vol.3, No.3, pp. 167-195,
#' ISSN 1864-3361, URL \url{https://ojs.ub.uni-konstanz.de/srm/article/view/369}.
#'
#' Jean-Claude Deville (1999). Variance estimation for complex statistics and estimators:
#' linearization and residual techniques. Survey Methodology, 25, 193-203,
#' URL \url{https://www150.statcan.gc.ca/n1/en/catalogue/12-001-X19990024882}.
#'
#' @keywords survey
#'
#' @examples
#'
#' library(survey)
#' library(laeken)
#' data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )
#'
#' # linearized design
#' des_eusilc <- svydesign( ids = ~rb030 , strata = ~db040 ,  weights = ~rb050 , data = eusilc )
#' des_eusilc <- convey_prep( des_eusilc )
#'
#' svypoormed( ~eqincome , design = des_eusilc )
#'
#' # replicate-weighted design
#' des_eusilc_rep <- as.svrepdesign( des_eusilc , type = "bootstrap" )
#' des_eusilc_rep <- convey_prep( des_eusilc_rep )
#'
#' svypoormed( ~eqincome , design = des_eusilc_rep )
#'
#' \dontrun{
#'
#' # linearized design using a variable with missings
#' svypoormed( ~ py010n , design = des_eusilc )
#' svypoormed( ~ py010n , design = des_eusilc , na.rm = TRUE )
#' # replicate-weighted design using a variable with missings
#' svypoormed( ~ py010n , design = des_eusilc_rep )
#' svypoormed( ~ py010n , design = des_eusilc_rep , na.rm = TRUE )
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
#' svypoormed( ~ eqincome , design = dbd_eusilc )
#'
#' dbRemoveTable( conn , 'eusilc' )
#'
#' dbDisconnect( conn , shutdown = TRUE )
#'
#' }
#'
#' @export
svypoormed <-
  function(formula, design, ...) {
    if (length(attr(terms.formula(formula) , "term.labels")) > 1)
      stop(
        "convey package functions currently only support one variable in the `formula=` argument"
      )

    UseMethod("svypoormed", design)

  }

#' @rdname svypoormed
#' @export
svypoormed.survey.design <-
  function(formula,
           design,
           quantiles = 0.5,
           percent = 0.6,
           na.rm = FALSE,
           ...) {

    # collect full sample
    if (is.null(attr(design, "full_design")))
      stop(
        "you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svydesign() function."
      )

    # if the class of the full_design attribute is just a TRUE, then the design is
    # already the full design.  otherwise, pull the full_design from that attribute.
    if ("logical" %in% class(attr(design, "full_design")))
      full_design <-
        design
    else
      full_design <- attr(design, "full_design")

    # collect full sample income data
    incvec <-
      model.frame(formula, full_design$variables, na.action = na.pass)[[1]]

    # treat missing
    if ( na.rm ) {
      nas <- is.na( incvec )
      full_design$prob <- ifelse( nas , Inf , full_design$prob )
    }

    # collect domain income data
    incvar <- model.frame(formula, design$variables, na.action = na.pass)[[1]]

    # treat missing
    if ( na.rm ) {
      nas <- is.na( incvar )
      design$prob <- ifelse( nas , Inf , design$prob )
    }

    # collect full sample weights
    wf <- 1 / full_design$prob

    # collect domain weights
    w <- 1 / design$prob

    # create domain indicator
    ind <- rownames( design$variables ) [ is.finite( design$prob ) ]
    ind <- rownames( full_design$variables ) %in% ind

    # compute domain population size
    N <- sum( w )

    # compute h
    htot <- h_fun( incvar[ w > 0 ] , w[ w > 0 ] )

    # compute arpt
    ARPT <-
      svyarpt(
        formula = formula,
        full_design,
        quantiles = quantiles,
        percent = percent,
        na.rm = na.rm
      )
    arpt <- coef( ARPT )
    linarpt <- attr( ARPT , "lin" )

    # if arpt is NA
    if ( is.na( arpt ) ) {

      rval <- as.numeric( NA )
      linmedp <- rep( NA , length = length( wf ) )
      varest <- matrix( NA )

    } else {

      # compute subset
      nome <- terms.formula(formula)[[2]]
      dsub <- eval( substitute( subset( design, subset = ( incvar <= arpt ) ) , list(incvar = nome, arpt = arpt)))

      # if the dataset is empty or has all zero weights or has all zero income values, return a missing rval
      if ( nrow( dsub ) == 0 || !( any( weights( dsub ) > 0 ) ) || all( dsub$variables[ , as.character(formula)[[2]] ] == 0 , na.rm = TRUE ) ) {

        warning( paste( "zero records in the set of poor people.  determine the poverty threshold by running svyarpt on ~", nome ) )
        rval <- as.vector( NA , mode = "numeric" )
        linmedp <- rep( NA , length = length( wf ) )
        varest <- matrix( NA )

      } else {

        rval <-
          survey::oldsvyquantile(x = formula ,
                                 dsub ,
                                 0.5 ,
                                 method = "constant" ,
                                 na.rm = na.rm ,
                                 ... )
        rval <- as.numeric( rval )

        # F prime
        Fprimemedp <-
          densfun(
            formula = formula,
            design = design,
            rval ,
            h = htot,
            FUN = "F",
            na.rm = na.rm
          )

        # at risk of poverty rate
        ARPR <-
          svyarpr(
            formula = formula,
            design = design,
            quantiles,
            percent,
            na.rm = na.rm
          )
        arpr <- coef( ARPR )
        ifarpr <- attr( ARPR, "lin" )

        # create domain binary
        ID <- as.numeric( ind )

        # linearize cdf of medp
        ifmedp <- ( 1 / N ) * ID * ( ( incvec <= rval ) - 0.5 * arpr )
        ifmedp <- ifmedp[ wf >0 ]

        # linearize median of poor
        v1 <- ( 0.5 * ifarpr - ifmedp ) / Fprimemedp
        linmedp <- ifelse( wf > 0 , 1 , 0 )
        linmedp[ wf > 0 ] <- v1

        # estimate variance
        varest <-
          survey::svyrecvar(
            linmedp / full_design$prob ,
            full_design$cluster ,
            full_design$strata ,
            full_design$fpc ,
            postStrata = full_design$postStrata )

      }

    }

    # create estimate object
    rval <- as.numeric( rval )
    varest <- as.matrix( varest )
    colnames( varest ) <-
      rownames( varest ) <-
      names( rval ) <-
      strsplit( as.character( formula )[[2]] , ' \\+ ')[[1]]
    class( rval ) <- c( "cvystat" , "svystat" )
    attr( rval , "var" ) <- varest
    attr( rval , "lin" ) <- linmedp
    attr( rval , "statistic" ) <- "poormed"
    rval

  }


#' @rdname svypoormed
#' @export
svypoormed.svyrep.design <-
  function(formula,
           design,
           quantiles = 0.5,
           percent = 0.6,
           na.rm = FALSE,
           ...) {
    if (is.null(attr(design, "full_design")))
      stop(
        "you must run the ?convey_prep function on your replicate-weighted survey design object immediately after creating it with the svrepdesign() function."
      )

    # if the class of the full_design attribute is just a TRUE, then the design is
    # already the full design.  otherwise, pull the full_design from that attribute.
    if ("logical" %in% class(attr(design, "full_design")))
      full_design <-
        design
    else
      full_design <- attr( design , "full_design" )

    # collect income data for full sample
    incvec <-
      model.frame(formula, full_design$variables, na.action = na.pass)[[1]]

    # treat missing
    if (na.rm) {
      nas <- is.na(incvec)
      full_design <- full_design[!nas, ]
      incvec <- incvec[!nas]
    }

    # collect full sample sampling weights
    wsf <- weights( full_design , "sampling" )

    # collect domain income data
    incvar <-
      model.frame( formula , design$variables , na.action = na.pass )[[1]]

    # treat missing
    if (na.rm) {
      nas <- is.na(incvar)
      design <- design[!nas,]
      incvar <- incvar[!nas]
    }

    # collect domain sampling weights
    ws <- weights(design, "sampling")

    # create domain indicators
    names( incvec ) <- names( wsf ) <- rownames( full_design$variables )
    names( incvar ) <- names( ws ) <- rownames( design$variables )
    ind <- names( wsf ) %in% names( ws )

    # compute value
    varname <- terms.formula( formula )[[2]]
    rval <-
      ComputePoormed( xf = incvec ,
                      wf = wsf ,
                      ind = ind ,
                      quantiles = quantiles ,
                      percent = percent ,
                      varname = varname )

    # collect replicate weights
    wwf <- weights( full_design , "analysis" )

    # compute replicates
    qq <-
      apply( wwf , 2 , function( wi ) {
        suppressWarnings(
          ComputePoormed( incvec ,
                          wi ,
                          ind = ind ,
                          quantiles = quantiles ,
                          percent = percent ,
                          varname = NULL ) )
      } )

    # compute variance
    if ( anyNA( qq ) ) {
      varest <- matrix( NA )
    } else varest <- survey::svrVar( qq ,
                                     design$scale ,
                                     design$rscales ,
                                     mse = design$mse ,
                                     coef = rval )

    # format results
    rval <- as.numeric( rval )
    varest <- as.matrix( varest )
    colnames( varest ) <-
      rownames( varest ) <-
      names(rval) <-
      strsplit( as.character ( formula )[[2]] , ' \\+ ')[[1]]
    class( rval ) <- c("cvystat" , "svrepstat")
    attr(rval, "var") <- varest
    attr(rval, "statistic") <- "poormed"
    rval

  }

#' @rdname svypoormed
#' @export
svypoormed.DBIsvydesign <-
  function (formula, design, ...) {
    if (!("logical" %in% class(attr(design, "full_design")))) {
      full_design <- attr(design , "full_design")

      full_design$variables <-
        getvars(
          formula,
          attr(design , "full_design")$db$connection,
          attr(design , "full_design")$db$tablename,
          updates = attr(design , "full_design")$updates,
          subset = attr(design , "full_design")$subset
        )

      attr(design , "full_design") <- full_design

      rm(full_design)

    }

    design$variables <-
      getvars(
        formula,
        design$db$connection,
        design$db$tablename,
        updates = design$updates,
        subset = design$subset
      )

    NextMethod("svypoormed", design)
  }


# estimation function
ComputePoormed <-
  function(xf , wf , ind , quantiles , percent , varname = NULL ) {
    thresh <- percent * computeQuantiles( xf , wf , p = quantiles )
    x <- xf[ind]
    w <- wf[ind]
    if ( is.na( thresh ) ) return( NA )
    indpoor <- ( x <= thresh )
    if ( !any( indpoor ) ) {
      if ( !is.null( varname ) ) warning( paste( "zero records in the set of poor people.  determine the poverty threshold by running svyarpt on ~", varname ) )
      return( NA )
    }
    medp <- computeQuantiles( x[indpoor] , w[indpoor] , p = 0.5)
    medp
  }

