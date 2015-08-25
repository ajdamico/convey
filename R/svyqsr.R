svyqsr_lin <- function(formula, design, alpha= .20, ncom,comp,incvec, ...) {
  inc <- terms.formula(formula)[[2]]
  df <- model.frame(design)
  incvar<-df[[as.character(inc)]]
  w<-weights(design)
  ind<-names(w)
  alpha1<- alpha
  alpha2<- 1-alpha
  quant_inf <-  svyquantile(x = formula, design = design, quantiles = alpha1,
    method = "constant")
  quant_inf<-as.vector(quant_inf)
  quant_sup <- svyquantile(x = formula, design = design, quantiles= alpha2,
    method = "constant")
  quant_sup <- as.vector(quant_sup)
  tot_var<- sum(incvar*w)
  rich<- (incvar > quant_sup)*incvar
  S80 <- sum(rich*w)
  poor<- (incvar <= quant_inf)*incvar
  S20 <- sum(poor*w)
  qsr<- S80/S20
  # Linearization of S20
  lin_S20 <- isq(formula = formula, design = design, alpha1, type="inf", h=NULL,
    ncom= ncom, comp = FALSE, incvec = incvec)$lin
  # Linearization of S80
  lin_S80 <- isq(formula = formula, design = design, alpha2, type="sup", h=NULL,
    ncom= ncom, comp=FALSE, incvec=incvec)$lin

  # LINEARIZED VARIABLE OF THE SHARE RATIO

  lin_qsr<-(S20*lin_S80-S80*lin_S20)/(S20*S20)

  names(lin_qsr)<-ind
  lin_qsr_comp<-complete(lin_qsr, ncom)
  if(comp) lin<-lin_qsr_comp else lin<-lin_qsr
  list(value = qsr, lin = lin_qsr)
}

svyqsr_rep <- function(formula, design, alpha= .20, ...) {
  inc <- terms.formula(formula)[[2]]
  df <- model.frame(design)
  incvar<-df[[as.character(inc)]]
  w<-weights(design)
  ind<-names(w)
  alpha1<- alpha
  alpha2<- 1-alpha
  quant_inf <-  svyquantile(x = formula, design = design, quantiles = alpha1,
    method = "constant")
  quant_inf<-as.vector(quant_inf)
  quant_sup <- svyquantile(x = formula, design = design, quantiles= alpha2,
    method = "constant")
  quant_sup <- as.vector(quant_sup)
  tot_var<- sum(incvar*w)
  rich<- (incvar > quant_sup)*incvar
  S80 <- sum(rich*w)
  poor<- (incvar <= quant_inf)*incvar
  S20 <- sum(poor*w)
  qsr<- S80/S20
  qsr
}

