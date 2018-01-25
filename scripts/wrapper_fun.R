

convey_wrapper <- function(convey_fun, x,  na.rm = FALSE, vartype = c("se", "ci", "var", "cv"), .svy = current_svy(), ...)

{
if (missing(vartype)) vartype <- "se"
  vartype <- match.arg(vartype, several.ok = TRUE)
  .svy <- srvyr::set_survey_vars(.svy, x )
  attributes(.svy)$full_design <- srvyr::set_survey_vars(attributes(.svy)$full_design,x)
if (length(group_vars(.svy))== 0){
  out <- convey_fun(~`__SRVYR_TEMP_VAR__`, na.rm = na.rm, design = .svy)
  out <- srvyr::get_var_est(out, vartype)}else{
  grps_formula <- survey::make.formula(group_vars(.svy))
  out <- survey::svyby(~`__SRVYR_TEMP_VAR__`, grps_formula, convey_fun, na.rm = na.rm, design = .svy)
  out <- srvyr::get_var_est(out, vartype, grps = group_vars(.svy))
  out
}

out

}

####
# Example from ?convey::svygini
suppressPackageStartupMessages({
  library(srvyr)
  library(survey)
  library(convey)
  library(vardpoor)
})
data(eusilc) ; names( eusilc ) <- tolower( names( eusilc ) )

# Setup for survey package
des_eusilc <- svydesign(
  ids = ~rb030,
  strata = ~db040,
  weights = ~rb050,
  data = eusilc
)
des_eusilc <- convey_prep(des_eusilc)

# Setup for srvyr package
srvyr_eusilc <- eusilc %>%
  as_survey(
    ids = rb030,
    strata = db040,
    weights = rb050
  ) %>%
  convey_prep()

## Ungrouped
# Calculate ungrouped for survey package
svygini(~eqincome, design = des_eusilc)

# With our new function
convey_wrapper(convey_fun =svygini, x=srvyr_eusilc$variables$eqincome, .svy = srvyr_eusilc)

# And finally, the more typical way through summarize
srvyr_eusilc %>%
  summarize(eqincome = convey_wrapper(convey_fun =svygini, eqincome))

## Groups
# Calculate by groups for survey
survey::svyby(~eqincome, ~rb090, des_eusilc, convey::svygini)

# With our new function
convey_wrapper(svygini, srvyr_eusilc$variables$eqincome, .svy = group_by(srvyr_eusilc, rb090))

# And finally, the more typical way through summarize
srvyr_eusilc %>%
  group_by(rb090) %>%
  summarize(eqincome = convey_wrapper(convey_fun =svygini, eqincome))



