#' J-Divergence Decomposition
#'
#' Estimates the group decomposition of the generalized entropy index
#'
#' @param formula a formula specifying the income variable
#' @param subgroup a formula specifying the group variable
#' @param design a design object of class \code{survey.design} or class \code{svyrep.design} from the \code{survey} library.
#' @param na.rm Should cases with missing values be dropped? Observations containing missing values in income or group variables will be dropped.
#' @param deff Return the design effect (see \code{survey::svymean})
#' @param linearized Should a matrix of linearized variables be returned
#' @param influence Should a matrix of (weighted) influence functions be returned? (for compatibility with \code{\link[survey]{svyby}})
#' @param return.replicates Return the replicate estimates?
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
#' URL \url{https://www.jstor.org/stable/1913511}.
#'
#' Nicholas Rohde (2016). J-divergence measurements of economic inequality.
#' J. R. Statist. Soc. A, v. 179, Part 3 (2016), pp. 847-870.
#' URL \url{https://rss.onlinelibrary.wiley.com/doi/abs/10.1111/rssa.12153}.
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
  function(formula, subgroup, design, ...) {
    if (length(attr(terms.formula(formula) , "term.labels")) > 1)
      stop(
        "convey package functions currently only support one variable in the `formula=` argument"
      )

    if (length(attr(terms.formula(subgroup) , "term.labels")) > 1)
      stop(
        "convey package functions currently only support one variable in the `subgroup=` argument"
      )

    UseMethod("svyjdivdec", design)

  }

#' @rdname svyjdivdec
#' @export
svyjdivdec.survey.design <-
  function (formula,
            subgroup,
            design,
            na.rm = FALSE,
            deff = FALSE ,
            linearized = FALSE ,
            influence = FALSE ,
            ...) {
    # collect information
    incvar <-
      model.frame(formula, design$variables, na.action = na.pass)[, ]
    grpvar <-
      model.frame(subgroup, design$variables, na.action = na.pass)[, ]

    if (inherits(grpvar, "labelled")) {
      stop("This function does not support 'labelled' variables. Try factor().")
    }

    # treat missing
    if (na.rm) {
      nas <- (is.na(incvar) | is.na(grpvar))
      design$prob <- ifelse(nas , Inf , design$prob)
    }

    # collect weights
    w <- 1 / design$prob


    # treat non-positive
    if (any(incvar[w != 0] <= 0 , na.rm = TRUE))
      stop("The J-divergence index is defined for strictly positive incomes only.")


    # if (any(any(is.na(incvar) | is.na(grpvar)) & (w > 0))) {
    #   rval <-
    #     list(estimate = matrix(c(NA, NA, NA), dimnames = list(c(
    #       "total", "within", "between"
    #     )))[, ])
    #   names(rval) <-
    #     strsplit(as.character(formula)[[2]] , ' \\+ ')[[1]]
    #   attr(rval, "var") <-
    #     matrix(rep(NA, 9), ncol = 3, dimnames = list(
    #       c("total", "within", "between"),
    #       c("total", "within", "between")
    #     ))[, ]
    #   attr(rval, "statistic") <- "j-divergence decomposition"
    #   attr(rval, "group") <- as.character(subgroup)[[2]]
    #   class(rval) <- c("cvydstat" , "cvystat" , "svystat")
    #
    #   return(rval)
    #
    # }

    # create interactions
    grpvar <- interaction(grpvar)

    # total
    total.jdiv <- CalcJDiv(incvar , w)

    # compute linearized function
    total.lin <- CalcJDiv_IF(incvar , w)


    # within:
    # create matrix of group-specific weights
    ind <-
      sapply(levels(grpvar) , function(z)
        ifelse(grpvar == z , 1 , 0))
    wg <- sweep(ind , 1 , w , "*")

    # calc gei components
    gei0.group <-
      apply(wg , 2 , function(tw)
        CalcGEI(incvar , tw , epsilon = 0))
    gei1.group <-
      apply(wg , 2 , function(tw)
        CalcGEI(incvar , tw , epsilon = 1))
    gei0.group.lin <-
      apply(wg , 2 , function(tw)
        CalcGEI_IF(incvar , tw , epsilon = 0))
    gei1.group.lin <-
      apply(wg , 2 , function(tw)
        CalcGEI_IF(incvar , tw , epsilon = 1))

    # calc share components
    pshare.group <- colSums (wg) / sum(w)
    sshare.group <-
      (ifelse(w > 0 , incvar , 0) %*% wg) / sum(ifelse(w > 0 , w * incvar , 0))
    pshare.group.lin <-
      sweep(ind , 2 , pshare.group , "-") / sum(w)
    sshare.group.lin <-
      (ind * incvar - incvar %*% sshare.group) / sum(w * incvar)

    # create within estimates
    within.jdiv <-
      pshare.group * gei0.group + sshare.group * gei1.group
    mat.gei0 <-
      sweep(gei0.group.lin , 2 , pshare.group , "*") + sweep(pshare.group.lin , 2 , gei0.group , "*")
    mat.gei1 <-
      sweep(gei1.group.lin , 2 , sshare.group , "*") + sweep(sshare.group.lin , 2 , gei1.group , "*")
    within.jdiv <- sum(within.jdiv)
    within.lin <- rowSums(mat.gei0 + mat.gei1)
    within.lin[w == 0] <- 0

    # between
    between.jdiv <- total.jdiv - within.jdiv
    between.lin <- total.lin - within.lin
    between.lin[w == 0] <- 0

    # create matrix
    lin.matrix <-
      matrix(
        data = c(total.lin, within.lin, between.lin),
        ncol = 3,
        dimnames = list(names(w) , c("total", "within", "between"))
      )

    # compute variance
    variance <-
      survey::svyrecvar(
        sweep(lin.matrix , 1 , w , "*") ,
        design$cluster,
        design$strata,
        design$fpc,
        postStrata = design$postStrata
      )
    variance[which(is.nan(variance))] <- NA

    # compute deff
    if (is.character(deff) || deff) {
      nobs <- sum(weights(design) != 0)
      npop <- sum(weights(design))
      if (deff == "replace")
        vsrs <-
        survey::svyvar(lin.matrix , design, na.rm = na.rm) * npop ^ 2 / nobs
      else
        vsrs <-
        survey::svyvar(lin.matrix , design , na.rm = na.rm) * npop ^ 2 * (npop - nobs) /
        (npop * nobs)
      deff.estimate <- variance / vsrs
    }

    # build result object
    estimates <- c(total.jdiv , within.jdiv , between.jdiv)
    names(estimates) <- colnames(variance)
    rval <- c(estimates)
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "jdiv decomposition"
    attr(rval, "group") <- as.character(subgroup)[[2]]
    if (linearized)
      attr(rval, "linearized") <- lin.matrix
    if (influence)
      attr(rval , "influence")  <- sweep(lin.matrix , 1 , w , "*")
    if (linearized |
        influence)
      attr(rval , "index") <- as.numeric(rownames(lin.matrix))
    if (is.character(deff) ||
        deff)
      attr(rval , "deff") <- deff.estimate
    class(rval) <- c("cvystat" , "svystat")
    rval

  }


#' @rdname svyjdivdec
#' @export
svyjdivdec.svyrep.design <-
  function(formula,
           subgroup,
           design,
           na.rm = FALSE,
           deff = FALSE ,
           linearized = FALSE ,
           return.replicates = FALSE ,
           ...) {
    # collect income and group variable
    incvar <-
      model.frame(formula, design$variables, na.action = na.pass)[, ]
    grpvar <-
      model.frame(subgroup, design$variables, na.action = na.pass)[, ]

    # treat missing
    if (na.rm) {
      nas <- is.na(incvar) | is.na(grpvar)
      design <- design[!nas, ]
      incvar <- incvar[!nas]
      grpvar <- grpvar[!nas]
    }

    # collect sampling weights
    ws <- weights(design, "sampling")

    # treat non-positive incomes
    if (any(incvar[ws != 0] <= 0, na.rm = TRUE))
      stop("The J-divergence index is defined for strictly positive incomes.")

    # do interactions
    grpvar <- interaction(grpvar)

    # collect replication weights
    ww <- weights(design, "analysis")

    # Total
    total.jdiv <- CalcJDiv(incvar , ws)
    qq.total.jdiv <-
      apply(ww, 2 , function(wi)
        CalcJDiv(incvar, wi))

    # create matrix of group-specific weights
    ind <-
      sapply(levels(grpvar) , function(z)
        ifelse(grpvar == z , 1 , 0))
    wg <- sweep(ind , 1 , ws , "*")

    # calculate between component
    Ybar <- weighted.mean(incvar , ws)
    Ybar.group <- colSums(wg * incvar) / colSums(wg)
    pshare.group <- colSums(wg) / sum(wg)
    between.jdiv <-
      sum(pshare.group * (Ybar.group / Ybar - 1) * log(Ybar.group / Ybar))
    qq.Ybar <-
      apply(ww , 2 , function(wi)
        weighted.mean(incvar , wi))
    qq.Ybar.group <-
      apply(ww , 2 , function(wi)
        colSums((wi * incvar) * ind) / colSums(wi * ind))
    qq.pshare.group <-
      apply(ww , 2 , function(wi)
        colSums(wi * ind) / sum(wi))
    qq.between.jdiv <-
      colSums(qq.pshare.group * (sweep(qq.Ybar.group , 2 , qq.Ybar , "/") - 1) * log(sweep(qq.Ybar.group , 2 , qq.Ybar , "/")))

    # calculate within component
    within.jdiv <- total.jdiv - between.jdiv
    qq.within.jdiv <- qq.total.jdiv - qq.between.jdiv

    # replicate matrix
    qq.matrix <-
      matrix(
        c(qq.total.jdiv, qq.within.jdiv, qq.between.jdiv),
        ncol = 3,
        dimnames = list(NULL , c("total", "within", "between"))
      )

    # collect estimates
    estimates <- c(total.jdiv , within.jdiv , between.jdiv)
    names(estimates) <- colnames(qq.matrix)

    # variance estimation
    variance <-
      survey::svrVar(qq.matrix,
                     design$scale,
                     design$rscales,
                     mse = design$mse,
                     coef = estimates)

    # compute deff
    if (is.character(deff) || deff || linearized) {
      # compute linearized function of the total jdiv
      total.lin <- CalcJDiv_IF(incvar , ws)

      # calculate intermediate statistics
      gei0.group <-
        apply(wg , 2 , function(wi)
          CalcGEI(incvar , wi , 0))
      gei1.group <-
        apply(wg , 2 , function(wi)
          CalcGEI(incvar , wi , 1))
      sshare.group <- t(wg) %*% incvar
      sshare.group <- c(t(sshare.group) / sum(sshare.group))

      # compute linearized function of the within jdiv
      gei0.group.lin <-
        apply(wg , 2 , function(wi)
          CalcGEI_IF(incvar , wi , 0))
      gei1.group.lin <-
        apply(wg , 2 , function(wi)
          CalcGEI_IF(incvar , wi , 1))
      pshare.group.lin <-
        sweep(ind , 2 , pshare.group , "-") / sum(ws)
      sshare.group.lin <-
        (ind * incvar - incvar %*% t(sshare.group)) / sum(ws * incvar)
      gei0.component.lin <-
        sweep(gei0.group.lin , 2 , pshare.group , "*") + sweep(pshare.group.lin , 2 , gei0.group , "*")
      gei1.component.lin <-
        sweep(gei1.group.lin , 2 , sshare.group , "*") + sweep(sshare.group.lin , 2 , gei1.group , "*")
      within.lin <-
        rowSums(gei0.component.lin + gei1.component.lin)
      within.lin[ws == 0] <- 0

      # between (residual)
      between.lin <- total.lin - within.lin
      between.lin[ws == 0] <- 0

      # create linearized matrix
      lin.matrix <-
        matrix(
          data = c(total.lin , within.lin, between.lin),
          ncol = 3,
          dimnames = list(names(ws) , c("total", "within", "between"))
        )

      ### compute deff
      nobs <- length(design$pweights)
      npop <- sum(design$pweights)
      vsrs <-
        unclass(
          survey::svyvar(
            lin.matrix ,
            design,
            na.rm = na.rm,
            return.replicates = FALSE,
            estimate.only = TRUE
          )
        ) * npop ^ 2 / nobs
      if (deff != "replace")
        vsrs <- vsrs * (npop - nobs) / npop
      deff.estimate <- variance / vsrs

    }

    # build result object
    rval <- estimates
    names(rval) <- c("total", "within", "between")
    attr(rval, "var") <- variance
    attr(rval, "statistic") <- "jdiv decomposition"
    attr(rval, "group") <- as.character(subgroup)[[2]]
    class(rval) <- c("cvystat" , "svrepstat" , "svystat")
    if (linearized)
      attr(rval, "linearized") <- lin.matrix
    if (linearized)
      attr(rval , "index") <- as.numeric(rownames(lin.matrix))

    # keep replicates
    if (return.replicates) {
      attr(qq.matrix , "scale") <- design$scale
      attr(qq.matrix , "rscales") <- design$rscales
      attr(qq.matrix , "mse") <- design$mse
      rval <- list(mean = rval , replicates = qq.matrix)
    }

    # add design effect estimate
    if (is.character(deff) || deff)
      attr(rval , "deff") <- deff.estimate

    # retorna objeto
    class(rval) <- c("cvystat" , "svrepstat" , "svystat")
    rval

  }


#' @rdname svyjdivdec
#' @export
svyjdivdec.DBIsvydesign <-
  function (formula, subgroup, design, ...) {
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
          subgroup,
          design$db$connection,
          design$db$tablename,
          updates = design$updates,
          subset = design$subset
        )
      )

    NextMethod("svyjdivdec", design)

  }
