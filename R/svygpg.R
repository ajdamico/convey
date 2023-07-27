#' Linearization of the gender pay (wage) gap
#'
#' Estimate the difference between the average gross hourly earnings of men and women expressed as a percentage of the average gross hourly earnings of men.
#'
#'
#' @param formula a formula specifying the gross hourly earnings variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param sex formula with a factor with labels 'male' and 'female'
#' @param na.rm Should cases with missing values be dropped?
#' @param ... future expansion
#'
#' @details you must run the \code{convey_prep} function on your survey design object immediately after creating it with the \code{svydesign} or \code{svrepdesign} function.
#'
#'@return Object of class "\code{cvystat}", which are vectors with a "\code{var}" attribute giving the variance and a "\code{statistic}" attribute giving the name of the statistic.
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
#' library(laeken)
#' library(survey)
#' data(ses)
#' names( ses ) <- gsub( "size" , "size_" , tolower( names( ses ) ) )
#' des_ses <- svydesign(id=~1, weights=~weights, data=ses)
#' des_ses <- convey_prep(des_ses)
#'
#' # linearized design
#' svygpg(~earningshour, des_ses, ~sex)
#' # replicate-weighted design
#' des_ses_rep <-  as.svrepdesign( des_ses , type = "bootstrap" )
#' des_ses_rep <- convey_prep(des_ses_rep)
#'
#' svygpg(~earningshour, des_ses_rep, ~sex)
#'
#' \dontrun{
#'
#' # database-backed design
#' library(RSQLite)
#' library(DBI)
#' dbfile <- tempfile()
#' conn <- dbConnect( RSQLite::SQLite() , dbfile )
#' dbWriteTable( conn , 'ses' , ses )
#'
#' dbd_ses <- svydesign(id=~1, weights=~weights, data="ses", dbname=dbfile, dbtype="SQLite")
#' dbd_ses <- convey_prep( dbd_ses )
#'
#' svygpg(formula=~earningshour, design=dbd_ses, sex= ~sex)
#'
#' dbRemoveTable( conn , 'ses' )
#'
#' }
#'
#' @export
svygpg <-
  function(formula, design, ...) {
    if (length(attr(terms.formula(formula) , "term.labels")) > 1)
      stop(
        "convey package functions currently only support one variable in the `formula=` argument"
      )

    UseMethod("svygpg", design)

  }

#' @rdname svygpg
#' @export
svygpg.survey.design <-
  function(formula, design, sex,  na.rm = FALSE, ...) {
    if (is.null(attr(design, "full_design")))
      stop(
        "you must run the ?convey_prep function on your linearized survey design object immediately after creating it with the svydesign() function."
      )

    # collect wage data
    wagevar <-
      model.frame(formula, design$variables, na.action = na.pass)[[1]]

    # sex indicator matrix
    mf <- model.frame(sex, design$variables, na.action = na.pass)
    xx <- model.matrix.lm( update( sex , ~0 + . ), design$variables, na.action = na.pass)

    # filter columns
    xx <- xx[ , 1:2 , drop = FALSE ]
    x <- cbind( xx , wagevar )

    # treat missing values
    if ( na.rm ) {
      nas <- rowSums( is.na( x ) )
      design <- design[nas == 0, ]
      if (length(nas) > length(design$prob)) {
        wagevar <- wagevar[nas == 0 ]
        xx <- xx[nas == 0, , drop = FALSE ]
      } else {
        design$prob[ nas == 1 ] <- Inf
        wagevar[nas != 0 ] <- 0
        xx[nas != 0, ] <- 0
      }
    }

    # collect weights
    w <- 1 / design$prob

    # compute means
    popsize.est <- w %*% xx
    means.est <- colSums( sweep( xx , 1 , w * wagevar , "*" ) ) / popsize.est

    # create linearization objects of totals
    linmat <- sweep( xx , 1 , wagevar , "*" ) - sweep( xx , 2 , means.est , "*" )
    linmat <- sweep( linmat , 2 , popsize.est , "/" )
    R1 <- list( value = means.est[1], lin = linmat[,1] )
    R2 <- list( value = means.est[2], lin = linmat[,2] )
    list_all_tot <- list( R1 = R1 , R2 = R2 )
    IGPG <- contrastinf( quote( 1 - R1/R2 ) , list_all_tot )
    infun <- IGPG$lin

    # build result object
    rval <- IGPG$value
    variance <-
      survey::svyrecvar(
        infun / design$prob ,
        design$cluster,
        design$strata,
        design$fpc,
        postStrata = design$postStrata
      )
    if ( is.nan( variance ) ) variance[,] <- NA
    colnames(variance) <-
      rownames(variance) <-
      names(rval) <-
      strsplit(as.character(formula)[[2]] , ' \\+ ')[[1]]
    class(rval) <- c("cvystat" , "svystat")
    attr( rval , "var" ) <- variance
    attr( rval, "lin" ) <- infun
    attr( rval , "statistic" ) <- "gpg"

    # return final object
    rval

  }


#' @rdname svygpg
#' @export
svygpg.svyrep.design <-
  function(formula, design, sex, na.rm = FALSE, ...) {
    if (is.null(attr(design, "full_design")))
      stop(
        "you must run the ?convey_prep function on your replicate-weighted survey design object immediately after creating it with the svrepdesign() function."
      )

    # collect wage data
    wagevar <-
      model.frame(formula, design$variables, na.action = na.pass)[[1]]

    # sex indicator matrix
    mf <- model.frame(sex, design$variables, na.action = na.pass )
    xx <- model.matrix.lm( update( sex , ~0 + . ) , design$variables , na.action = na.pass )

    # filter columns
    xx <- xx[ , 1:2 , drop = FALSE ]
    x <- cbind( xx , wagevar )

    # treat missing values
    if ( na.rm ) {
      nas <- rowSums( is.na( x ) )
      design <- design[nas == 0, ]
      if (length(nas) > length(design$pweights)) {
        wagevar <- wagevar[nas == 0 ]
        xx <- xx[nas == 0, , drop = FALSE ]
      } else {
        design$prob[ nas == 1 ] <- Inf
        wagevar[nas != 0 ] <- 0
        xx[nas != 0, ] <- 0
      }
    }

    # collect sampling weights
    ws <- weights( design , "sampling" )

    # compute means
    popsize.est <- ws %*% xx
    means.est <- colSums( sweep( xx , 1 , ws * wagevar , "*" ) ) / popsize.est

    # create linearization objects of totals
    linmat <- sweep( xx , 1 , wagevar , "*" ) - sweep( xx , 2 , means.est , "*" )
    linmat <- sweep( linmat , 2 , popsize.est , "/" )
    R1 <- list( value = means.est[1], lin = linmat[,1] )
    R2 <- list( value = means.est[2], lin = linmat[,2] )
    list_all_tot <- list( R1 = R1 , R2 = R2 )
    IGPG <- contrastinf( quote( 1 - R1/R2 ) , list_all_tot )
    infun <- IGPG$lin
    rval <- IGPG$value

    # collect replicate weights
    ww <- weights( design , "analysis" )

    # compute replicates
    qq <- apply( ww , 2 , function( tw ) {
      popsize.rep <- tw %*% xx
      means.rep <- colSums( sweep( xx , 1 , tw * wagevar , "*" ) ) / popsize.rep
      gpg.rep <- 1 - means.rep[1] / means.rep[2]
      gpg.rep
    } )

    # compute variance
    if ( sum( !is.na( qq ) ) <= 1 ) variance <- NA else {
      variance <-
        survey::svrVar( qq,
                        design$scale,
                        design$rscales,
                        mse = design$mse,
                        coef = rval )
    }
    variance <- as.matrix( variance )
    colnames(variance) <-
      rownames(variance) <-
      names(rval) <-
      strsplit(as.character(formula)[[2]] , ' \\+ ')[[1]]
    class(rval) <- c("cvystat" , "svrepstat")
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "gpg"

    # return final object
    rval
  }

#' @rdname svygpg
#' @export
svygpg.DBIsvydesign <-
  function (formula, design, sex, ...) {
    if (!("logical" %in% class(attr(design, "full_design")))) {
      full_design <- attr(design , "full_design")

      full_design$variables <-
        cbind(
          getvars(
            formula,
            attr(design , "full_design")$db$connection,
            attr(design , "full_design")$db$tablename,
            updates = attr(design , "full_design")$updates,
            subset = attr(design , "full_design")$subset
          ),

          getvars(
            sex,
            attr(design , "full_design")$db$connection,
            attr(design , "full_design")$db$tablename,
            updates = attr(design , "full_design")$updates,
            subset = attr(design , "full_design")$subset
          )
        )

      attr(design , "full_design") <- full_design

      rm(full_design)

    }

    design$variables <-
      cbind(
        getvars(
          formula,
          design$db$connection,
          design$db$tablename,
          updates = design$updates,
          subset = design$subset
        ),

        getvars(
          sex,
          design$db$connection,
          design$db$tablename,
          updates = design$updates,
          subset = design$subset
        )
      )

    NextMethod("svygpg", design)
  }
