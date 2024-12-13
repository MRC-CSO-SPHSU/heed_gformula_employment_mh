library(dplyr)
library(tidyr)
library(haven)
library(nnet)
library(broom)
library(openxlsx)
library(sandwich)
library(lmtest)
library(MASS)
library(tibble)

## NOTES ## 
# Check estimate directions make sense
# Test whether odds between levels are equivalent


setwd('N:/Work/UC_microsim/initial_output')


#data_test <- read_dta('T:/projects/HEED/DataAnalysis/hfcovid_analysis.dta')

#names(data_test)

data <- read_dta('T:/projects/HEED/DataAnalysis/hfcovid_analysis.dta') %>%
  dplyr::select(pidp, hidp, wave,
         sex_dv, age_dv, age_sq, hiqual_dv,
         scsf1, sclfsato, sf12pcs_dv, sf12mcs_dv,
         gor_dv,
         pidp,
         intdaty_dv,
         scghq1_dv,
         econ_benefits, home_owner, mastat_dv, dnc, ydses_c5, dlltsd,
         indscus_lw
         ) %>% 
  rename( 
    dag = age_dv,
    dag_sq = age_sq,
    dgn = sex_dv,
    deh_c3 = hiqual_dv,
    dhe = scsf1,
    drgnl = gor_dv,
    dcpst = mastat_dv
         ) %>%
   drop_na()


names(data)
  
## Stata codes: L. = lagged, i. = factored, ib8. = set reference level for factor to 8, 


## create fractored variables, including outcomes

data2 <- data %>%
  mutate_at(c('dgn', 'deh_c3', 'econ_benefits', 'home_owner', 'dcpst', 'ydses_c5', 'drgnl', 'dhe', 'sclfsato'), as.factor)

## lagged variables to be used as confounders: econ_benefits, home_owner, mastat_dv, dnc, gor_dv, and outcome measures

data3 <- data2 %>%
  group_by(pidp) %>%
  mutate(econ_benefits.L = lag(econ_benefits),
         home_owner.L = lag(home_owner),
         dcpst.L = lag(dcpst),
         dnc.L = lag(dnc),
         dhe.L = lag(dhe),
         sclfsato.L = lag(sclfsato),
         sf12pcs_dv.L = lag(sf12pcs_dv),
         sf12mcs_dv.L = lag(sf12mcs_dv),
         drgnl.L = lag(drgnl),
         dag.L = lag(dag),
         dag_sq.L = lag(dag_sq)
         ) %>%
  ungroup()


data3$drgnl.L <- relevel(data3$drgnl.L, ref = 7)

## Outcome: self-rated health (dhe), as multinomial logit regression)

levels(data3$dhe)
data3$dhe <- as.factor(data3$dhe)


srh_model <- polr(dhe ~ econ_benefits.L + home_owner.L + dcpst.L + dnc.L + dhe.L + drgnl.L + #lag confounders
                    dgn + dag + dag_sq + deh_c3 + intdaty_dv, # confounders
                data = data3,
                weights = indscus_lw,
                Hess = T)
summary(srh_model)
srh_model_coef <- exp(srh_model$coefficients)
srh_model_vcov <- exp(vcovCL(srh_model, cluster = ~pidp))[1:26,1:26]
srh_model_cols <- cbind(srh_model_coef, srh_model_vcov)
srh_model_cols2 <- as.data.frame(srh_model_cols)
srh_model_cols3 <- rownames_to_column(srh_model_cols2, "REGRESSOR") %>%
  rename('COEFFICIENT' = 'srh_model_coef')

write.xlsx(srh_model_cols3, file = 'srh_ouput.xlsx')

## Ouctome: SF12 physical (sf12pcs_dv), as linear regression


data3$sf12pcs_dv <- as.numeric(data3$sf12pcs_dv)

sf12pcs_model <- lm(sf12pcs_dv ~ econ_benefits.L + home_owner.L + dcpst.L + dnc.L + sf12pcs_dv.L + sf12mcs_dv.L + drgnl.L + #lag confounders
                      dgn + dag + dag_sq + deh_c3 + intdaty_dv, # confounders
                    data = data3,
                    weights = indscus_lw
)
sf12pcs_model_coef <- sf12pcs_model$coefficients
sf12pcs_model_vcov <- vcovCL(sf12pcs_model, cluster = ~pidp)[1:25,1:25]
sf12pcs_model_cols <-  cbind(sf12pcs_model_coef, sf12pcs_model_vcov)
sf12pcs_model_cols2 <- as.data.frame(sf12pcs_model_cols)
sf12pcs_model_cols3 <- rownames_to_column(sf12pcs_model_cols2, "REGRESSOR") %>%
  rename('COEFFICIENT' = 'sf12pcs_model_coef')

write.xlsx(sf12pcs_model_cols3, file = 'sf12pcs_output.xlsx')

## Ouctome: SF12 mental (sf12mcs_dv), as linear regression

data3$sf12mcs_dv <- as.numeric(data3$sf12mcs_dv)

sf12mcs_model <- lm(sf12mcs_dv ~ econ_benefits.L + home_owner.L + dcpst.L + dnc.L + sf12mcs_dv.L + sf12pcs_dv.L + drgnl.L + dag.L + dag_sq.L +#lag confounders
                      dgn  + deh_c3 + intdaty_dv,
                    data = data3,
                    weights = indscus_lw
)

sf12mcs_model_coef <- sf12mcs_model$coefficients
sf12mcs_model_vcov <- vcovCL(sf12mcs_model, cluster = ~pidp)[1:25,1:25]
sf12mcs_model_cols <-  cbind(sf12mcs_model_coef, sf12mcs_model_vcov)
sf12mcs_model_cols2 <- as.data.frame(sf12mcs_model_cols)
sf12mcs_model_cols3 <- rownames_to_column(sf12mcs_model_cols2, "REGRESSOR") %>%
  rename('COEFFICIENT' = 'sf12mcs_model_coef')

write.xlsx(sf12mcs_model_cols3, file = 'sf12mcs_output.xlsx')


## Ouctome: life satisfaction (sclsato), as logit multinomial

levels(data3$sclfsato)
data3$sclfsato <- as.factor(data3$sclfsato)

lfsat_model <- polr(sclfsato ~ econ_benefits.L + home_owner.L + dcpst.L + dnc.L + sclfsato.L + drgnl.L + #lag confounders
                      dgn + dag + dag_sq + deh_c3 + intdaty_dv, 
                        data = data3,
                        weights = indscus_lw,
                    Hess = T
)

lfsat_coef <- exp(lfsat_model$coefficients)
lfsat_vcov <- exp(vcovCL(lfsat_model, cluster = ~pidp))[1:28,1:28]
lfsat_cols <- cbind(lfsat_coef, lfsat_vcov)
lfsat_cols2 <- as.data.frame(lfsat_cols)
lfsat_cols3 <- rownames_to_column(lfsat_cols2, "REGRESSOR") %>%
  rename('COEFFICIENT' = 'lfsat_coef')

write.xlsx(lfsat_cols3, file = 'lfsat_ouput.xlsx')




