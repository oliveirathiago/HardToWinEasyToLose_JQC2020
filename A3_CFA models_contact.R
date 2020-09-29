load('data/df_contact.Rdata')

packages <- list("lavaan", "dplyr")
install <- packages[(!packages %in% installed.packages()[, "Package"])]
if(length(install)) install.packages(packages)
library(lavaan)
library(dplyr)

############################################################################

df_contact <- df_contact %>%
  mutate_at(vars(proc12:outcome42), ordered)

## CFA models to assess empirical distinguishness

cfa1factor <- ' Factor =~ proc12 + proc22 + proc32 + proc42 + proc52                # One-factor solution
                        + outcome12 + outcome22 + outcome32 + outcome42
              '

cfa2factor <- ' Process =~ proc12 + proc22 + proc32 + proc42 + proc52               # Two-factor solution
                Outcome =~ outcome12 + outcome22 + outcome32 + outcome42
              '

cfa1factor.fit <- cfa(cfa1factor, std.lv=T, data=df_contact,
                      ordered = c('proc12', 'proc22', 'proc32', 'proc42', 'proc52',
                                  'outcome12', 'outcome22', 'outcome32', 'outcome42'))   # Estimates one-factor model
cfa2factor.fit <- cfa(cfa2factor, std.lv=T, data=df_contact,
                      ordered = c('proc12', 'proc22', 'proc32', 'proc42', 'proc52',
                                  'outcome12', 'outcome22', 'outcome32', 'outcome42'))   # Estimates two-factor model

cbind(OneFactor = inspect(cfa1factor.fit, 'fit.measures'), 
      TwoFactor = inspect(cfa2factor.fit, 'fit.measures') 
)[c('chisq', 'df', 'pvalue', 'cfi', 'tli', 'rmsea', 'rmsea.ci.lower', 'rmsea.ci.upper'),]   # Comparing model fit

summary(cfa2factor.fit, standardized = T)                                                   # Analyse factor loadings

cfa2factor_new <- ' Process =~ proc12 + proc32 + proc42 + proc52                            # Two-factor solution without proc22
                    Outcome =~ outcome12 + outcome22 + outcome32 + outcome42
                  '
cfa2factor_new.fit <- cfa(cfa2factor_new, std.lv=T, data=df_contact,
                          ordered = c('proc12', 'proc22', 'proc32', 'proc42', 'proc52',
                                      'outcome12', 'outcome22', 'outcome32', 'outcome42'))  # Estimates new two-factor model

# Table A3
summary(cfa1factor.fit, standardized = T)
summary(cfa2factor.fit, standardized = T)
summary(cfa2factor_new.fit, standardized = T) 

# Table A3
cbind(OneFactor = inspect(cfa1factor.fit, 'fit.measures'), 
      TwoFactor = inspect(cfa2factor.fit, 'fit.measures'),
      TwoFactorNew = inspect(cfa2factor_new.fit, 'fit.measures')
)[c('chisq', 'df', 'pvalue', 'cfi', 'tli', 'rmsea', 'rmsea.ci.lower', 'rmsea.ci.upper'),]
