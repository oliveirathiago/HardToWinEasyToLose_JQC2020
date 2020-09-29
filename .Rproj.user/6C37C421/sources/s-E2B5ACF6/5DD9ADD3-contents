load('data/df.Rdata')

packages <- list("lavaan", "dplyr")
install <- packages[(!packages %in% installed.packages()[, "Package"])]
if(length(install)) install.packages(packages)
library(lavaan)
library(dplyr)

df <- df %>%
   mutate_at(vars(duty11:eff32), ordered)


cfa1factor <- ' Factor =~ pj11 + pj21 + pj31 + pj41             # One-factor solution
                        + eff11 + eff21 + eff31
                        + duty11 + duty21
              '

cfa2factor <- ' Trust       =~ pj11 + pj21 + pj31 + pj41        # Two-factor solution
                            + eff11 + eff21 + eff31
                Legitimacy  =~ duty11 + duty21
              '

cfa3factor <- ' pj   =~ pj11 + pj21 + pj31 + pj41               # Three-factor solution
                eff  =~ eff11 + eff21 + eff31
                duty =~ duty11 + duty21
              '


cfa1factor.fit <- cfa(cfa1factor, std.lv=T, data=df,
                      ordered = c('pj11', 'pj21', 'pj31', 'pj41',
                                  'eff11', 'eff21', 'eff31',
                                  'duty11', 'duty21'))            # Estimates one-factor model

cfa2factor.fit <- cfa(cfa2factor, std.lv=T, data=df,
                      ordered = c('pj11', 'pj21', 'pj31', 'pj41',
                                  'eff11', 'eff21', 'eff31',
                                  'duty11', 'duty21'))            # Estimates two-factor model

cfa3factor.fit <- cfa(cfa3factor, std.lv=T, data=df,
                      ordered = c('pj11', 'pj21', 'pj31', 'pj41',
                                  'eff11', 'eff21', 'eff31',
                                  'duty11', 'duty21'))            # Estimates three-factor model


# Table A1
cbind(OneFactor = inspect(cfa1factor.fit, 'fit.measures'), 
      TwoFactor = inspect(cfa2factor.fit, 'fit.measures'), 
      ThreeFactor = inspect(cfa3factor.fit, 'fit.measures')
      )[c('chisq', 'df', 'pvalue', 'cfi', 'tli', 'rmsea', 'rmsea.ci.lower', 'rmsea.ci.upper'),]

# Table A2
summary(cfa3factor.fit, standardized = T)

