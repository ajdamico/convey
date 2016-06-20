#' @importFrom stats approxfun coef deriv model.frame model.matrix na.pass printCoefmat qnorm terms terms.formula update weights
#' @importFrom methods is

# each of the functions below are from the survey library v3.30-3
# written by Thomas Lumley and copied here under the same GPL-3 license
# these functions were copied following the direction in this thread:
# https://stat.ethz.ch/pipermail/r-devel/2013-August/thread.html#67180

computeQuantiles <- function(xx, w, p) {
    if (any(is.na(xx)))
        return(NA * p)

	if( sum( w ) == 0 ) return( NA )

    oo <- order(xx)
    cum.w <- cumsum(w[oo])/sum(w)
    cdf <- approxfun(cum.w, xx[oo], method = "constant", f = 1, yleft = min(xx),
        yright = max(xx), ties = min)
    cdf(p)
}

checkConnection <- 
	function (dbconnection, error = TRUE) {
		
		if (is(dbconnection, "DBIConnection")) {
			if (!DBI::dbIsValid(dbconnection)) 
				if (error) 
					stop("Database connection is closed")
				else return(FALSE)
		} else {
		
		}
		invisible(TRUE)
	}

getvars <- function (formula, dbconnection, tables, db.only = TRUE, updates = NULL, 
    subset = NULL) {
    checkConnection(dbconnection)
    if (is.null(formula)) 
        return(NULL)
    if (inherits(formula, "formula")) {
        var0 <- all.vars(formula)
    }
    else if (is.character(formula)) {
        var0 <- formula
    }
    else {
        return(formula)
    }
    infilter <- updatesInfilter(var0, updates)
    if (db.only) {
        in.db <- infilter$varlist
    }
    else {
        query <- sub("@tab@", tables, "select * from @tab@ limit 1")
        if (is(dbconnection, "DBIConnection")) 
            oneline <- DBI::dbGetQuery(dbconnection, query)
        else oneline <- RODBC::sqlQuery(dbconnection, query)
        in.db <- infilter$varlist[infilter$varlist %in% names(oneline)]
    }
    query <- paste("select", paste(in.db, collapse = ", "), "from", 
        tables)
    if (is(dbconnection, "DBIConnection")) 
        df <- DBI::dbGetQuery(dbconnection, query)
    else df <- RODBC::sqlQuery(dbconnection, query)
    if (!is.null(subset)) 
        df <- df[subset, , drop = FALSE]
    df <- updatesOutfilter(df, var0, infilter$history, updates)
    is.string <- sapply(df, is.character)
    if (any(is.string)) {
        for (i in which(is.string)) df[[i]] <- as.factor(df[[i]])
    }
    df
}

updatesInfilter <-
	function (varlist, updates) {
		if (is.null(updates)) 
			return(list(varlist = varlist))
		n <- length(updates)
		v <- vector("list", n)
		for (i in n:1) {
			if (any(idx <- (varlist %in% names(updates[[i]])))) {
				v[[i]] <- varlist[idx]
				ups <- match(v[[i]], names(updates[[i]]))
				varlist <- unique(c(varlist[!idx], do.call(c, lapply(updates[[i]][ups], 
					"[[", "inputs"))))
			}
		}
		list(varlist = varlist, history = v)
	}

updatesOutfilter <-
	function (df, varlist, history, updates) {
		if (is.null(updates)) 
			return(df)
		if (all(sapply(history, length) == 0)) 
			return(df)
		n <- length(updates)
		for (i in 1:n) {
			if (mi <- length(history[[i]])) {
				outputs <- vector("list", mi)
				for (j in 1:mi) {
					idx.j <- match(history[[i]][j], names(updates[[i]]))
					outputs[[j]] <- eval(updates[[i]][[idx.j]]$expression, 
					  df)
				}
				names(outputs) <- history[[i]]
				if (any(mod <- names(df) %in% names(outputs))) {
					df <- df[, !mod, drop = FALSE]
				}
				df <- cbind(df, outputs)
			}
		}
		df[, names(df) %in% varlist, drop = FALSE]
	}

	
model.frame.survey.design<-function(formula,...,drop=TRUE){
  formula$variables
}
model.frame.svyrep.design<-function(formula,...){
  formula$variables
}

svyby <- function (formula, by, design, ...) UseMethod("svyby", design)




svyby.default<-function(formula, by, design, FUN,..., deff=FALSE, keep.var=TRUE,
                        keep.names=TRUE, verbose=FALSE, vartype=c("se","ci","ci","cv","cvpct","var"),
                        drop.empty.groups=TRUE, covmat=FALSE, return.replicates=FALSE, na.rm.by=FALSE,
                        na.rm.all=FALSE,
                        multicore=getOption("survey.multicore")){

  if (inherits(by, "formula"))
    byfactors<-model.frame(by, model.frame(design), na.action=na.pass)
  else
    byfactors<-as.data.frame(by)

  if(covmat || return.replicates){
    if (!inherits(design,"svyrep.design"))
      stop("covmat=TRUE not implemented for this design type")
  }

  if (multicore && !require("parallel",quietly=TRUE))
    multicore<-FALSE

  ## some people insist on using vectors rather than formulas
  ## so I suppose we should be nice to them
  if (!inherits(formula, "formula")){
      if (NROW(formula)!=length(byfactor))
          stop("'formula' is the wrong length")
      if (!(is.data.frame(formula) ||
            is.matrix(formula) ||
            is.vector(formula))){
          stop("invalid type for 'formula'")
      }
  }

  hasdeff<- is.character(deff) || deff
  
  ## all combinations that actually occur in this design
  byfactor<-do.call("interaction", byfactors)
  dropped<- weights(design,"sampling")==0
  if (na.rm.by) dropped<-dropped | apply(byfactors, 1, function(x) any(is.na(x)))
  if (na.rm.all){
    if (inherits(formula,"formula"))
      allx<-model.frame(formula,model.frame(design),na.action=na.pass)
    else
      allx<-formula
    dropped <- dropped | (!complete.cases(allx))
  }
  uniquelevels<-sort(unique(byfactor[!dropped]))
  uniques <- match(uniquelevels, byfactor)

  
  if(missing(vartype)) vartype<-"se"
  vartype<-match.arg(vartype,several.ok=TRUE)
  nvartype<-which(eval(formals(sys.function())$vartype) %in% vartype)
  if(any(is.na(nvartype))) stop("invalid vartype")
  
  if (keep.var){
      unwrap <-function(x){
        rval<-c(coef(x))
        nvar<-length(rval)
        rval<-c(rval,c(se=SE(x),
                       ci_l=confint(x)[,1],
                       ci_u=confint(x)[,2],
                       cv=cv(x,warn=FALSE),
                       `cv%`=cv(x,warn=FALSE)*100,
                       var=SE(x)^2)[rep((nvartype-1)*(nvar),each=nvar)+(1:nvar)])
        if(!is.null(attr(x,"deff")))
          rval<-c(rval,DEff=deff(x))
        rval
      }

      ## In dire need of refactoring (or rewriting)
      ## but it seems to work.
      results<-(if (multicore) mclapply else lapply)(uniques,
                      function(i){
                        if(verbose && !multicore) print(as.character(byfactor[i]))
                        if (inherits(formula,"formula"))
                          data<-formula
                        else
                          data<-subset(formula, byfactor %in% byfactor[i])
                        if (covmat || return.replicates) {
                          FUN(data,
                              design[byfactor %in% byfactor[i],],
                              deff=deff,...,return.replicates=TRUE)
                        } else {
                          FUN(data,
                              design[byfactor %in% byfactor[i],],
                              deff=deff,...)
                        }
                      })
      rval<-t(sapply(results, unwrap))
      if (covmat || return.replicates) {
        replicates<-do.call(cbind,lapply(results,"[[","replicates"))
        colnames(replicates)<-rep(as.character(uniquelevels), each=NCOL(replicates)/length(uniquelevels))
        covmat.mat<-svrVar(replicates,design$scale,design$rscales, mse=design$mse,coef=as.vector(sapply(results,coef)))
      } else{
        covmats<-lapply(results,vcov)
        ncovmat<-sum(sapply(covmats,ncol))
        covmat.mat<-matrix(0,ncol=ncovmat,nrow=ncovmat)
        j<-0
        for(i in 1:length(covmats)){
          ni<-nrow(covmats[[i]])
          covmat.mat[j+(1:ni),j+(1:ni)]<-covmats[[i]]
          j<-j+ni
        }
      }      
    } else {
      unwrap2 <- function(x){
          if(!is.null(attr(x, "deff")))
              c(statistic = unclass(x),
                DEff = deff(x))
          else c(statistic = unclass(x))
      }
      rval<-sapply(uniques,
                   function(i) {
                     if(verbose) print(as.character(byfactor[i]))
                     if (inherits(formula,"formula"))
                           data<-formula
                     else
                       data<-subset(formula, byfactor %in% byfactor[i])
                     unwrap2(FUN(data,
                                 design[byfactor %in% byfactor[i],],
                                 deff=deff,...))}
                   )
      if (is.matrix(rval)) rval<-t(rval)
  }

  nr<-NCOL(rval)
  nstats<-nr/(1+ keep.var*(length(vartype)+ ("ci" %in% vartype)) + hasdeff)

              
  if (nr>1)
    rval<-cbind(byfactors[uniques,,drop=FALSE], rval)
  else
    rval <-cbind(byfactors[uniques,,drop=FALSE], statistic=rval)

  expand.index<-function(index,reps,x=FALSE){
    ns<-max(index)
    if (x){
      i<-matrix(1:(ns*reps),ncol=reps)
      rval<-t(i[index,])
      
    } else{
      i<-matrix(1:(ns*reps), ncol=reps, nrow=ns, byrow=TRUE)
      rval<- i[index,]
    }
    as.vector(rval)
  }

  if(drop.empty.groups){
      if (keep.names)
          rownames(rval)<-paste(byfactor[uniques])
      rval<-rval[order(byfactor[uniques]),]

      i<-expand.index(order(byfactor[uniques]),nstats)
      if (keep.var)
        covmat.mat<-covmat.mat[i,i]

  } else {
      a<-do.call("expand.grid", lapply(byfactors,function(f) levels(as.factor(f))))
      a<-cbind(a,matrix(NA, ncol=nr, nrow=nrow(a)))
      names(a)<-names(rval)
      a[match(byfactor[uniques], levels(byfactor)),]<-rval
      rval<-a
      if (keep.names)
          rownames(rval)<-levels(byfactor)
      if (keep.var){
        tmp<-matrix(ncol=nrow(a)*nstats,nrow=nrow(a)*nstats)
        i<-expand.index(match(byfactor[uniques], levels(byfactor)),nstats,TRUE)
        tmp[i,i]<-covmat.mat
        covmat.mat<-tmp
      }
  }
                  
  attr(rval,"svyby")<-list(margins=1:NCOL(byfactors),nstats=nstats,
                           vars=if(keep.var) length(vartype) else 0,
                           deffs=deff,
                           statistic=deparse(substitute(FUN)),
                           variables= names(rval)[-(1:NCOL(byfactors))][1:nstats],
                           vartype=vartype
                           )
  if (!keep.names)
    rownames(rval)<-1:NROW(rval)

  if(covmat)
    attr(rval,"var")<-covmat.mat
  if (return.replicates)
    attr(rval,"replicates")<-replicates
  attr(rval,"call")<-sys.call()
  class(rval)<-c("svyby","data.frame")
  rval
}



svyby.DBIsvydesign<-function(formula, by, design,...){
  design$variables<-cbind(getvars(formula,design$db$connection, design$db$tablename,updates=design$updates, subset=design$subset),
                          getvars(by,design$db$connection, design$db$tablename,updates=design$updates, subset=design$subset))
  class(design)<-setdiff(class(design),"DBIsvydesign")
  svyby(formula,by,design,...)
}


svyby.ODBCsvydesign<-function(formula, by, design,...){
  design$variables<-cbind(getvars(formula,design$db$connection, design$db$tablename,updates=design$updates),
                          getvars(by,design$db$connection, design$db$tablename,updates=design$updates))
  class(design)<-setdiff(class(design),"ODBCsvydesign")
  svyby(formula,by,design,...)
}
