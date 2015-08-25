svyrmpg_lin <- function(formula, design, order =.50, percent = .60, ncom , h, comp, ARPT, ...){
  w<-weights(design)
  ind<-names(w)
  N<-sum(w)
  inc <- terms.formula(formula)[[2]]
  df <- model.frame(design)
  incvar<-df[[as.character(inc)]]
  arpt <- ARPT$value
  linarpt <- ARPT$lin
  arpr <-sum((incvar<=arpt)*w)/N
  dsub<- subset (design, subset= (incvar<= arpt))
  medp <- svyquantile(x = formula, dsub, .5, method="constant")
  medp<-as.vector(medp)
  RMPG <- 1 - (medp/ arpt)
  Fprimemedp <- densfun(formula = formula, design = design , medp, htot=h, fun="F")
  Fprimearpt<-  densfun(formula = formula, design = design, arpt, htot=h, fun="F")
  # linearize cdf of ARPT
  ifarpr0<-(1/N)*((incvar<=arpt)-arpr)
  names(ifarpr0)<-names(w)
  ifarpr0<-complete(ifarpr0,ncom)
  ifarpr <- ifarpr0+Fprimearpt*linarpt
  # linearize cdf of medp
  ifmedp <-(1/N)*((incvar<=medp)-0.5*arpr)
  names(ifmedp) <- names(w)
  ifmedp<- complete(ifmedp, ncom)
  # linearize median of poor
  linmedp <- (0.5*ifarpr-ifmedp)/Fprimemedp
  # linearize RMPG
  linrmpg <- (medp*linarpt/(arpt*arpt))-(linmedp/arpt)
  list(value = RMPG, lin = linrmpg)
}

svyrmpg_rep <- function(formula, design, order =.50, percent = .60, ARPT, ...){
  w<-weights(design)
  ind<-names(w)
  N<-sum(w)
  inc <- terms.formula(formula)[[2]]
  df <- model.frame(design)
  incvar<-df[[as.character(inc)]]
  arpt <- svyarpt_rep(formula = formula, design = design, order = order,
    percent =percent,...)
  arpr <-sum((incvar<=arpt)*w)/N
  dsub<- subset (design, subset= (incvar<= arpt))
  medp <- svyquantile(x = formula, dsub, .5, method="constant")
  medp<-as.vector(medp)
  RMPG <- 1 - (medp/ arpt)
  RMPG
}
