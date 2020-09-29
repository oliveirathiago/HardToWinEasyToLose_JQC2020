load('data/df.Rdata')

packages <- list("lavaan", "dplyr")
install <- packages[(!packages %in% installed.packages()[, "Package"])]
if(length(install)) install.packages(packages)
library(lavaan)
library(dplyr)

# Testing Measurement Equivalence

# Trust in procedural justice
cfa.pj.configural <- 'pj1 =~ pj11 + pj21 + pj31 + pj41
                      pj2 =~ pj12 + pj22 + pj32 + pj42

                          pj11 ~~ pj12
                          pj21 ~~ pj22
                          pj31 ~~ pj32
                          pj41 ~~ pj42 
                      '
cfa.pj.configural.fit <- cfa(cfa.pj.configural, std.lv=T, data=df, missing='fiml')

cfa.pj.weak <- 'pj1 =~ a1*pj11 + a2*pj21 + a3*pj31 + a4*pj41
                pj2 =~ a1*pj12 + a2*pj22 + a3*pj32 + a4*pj42

                          pj11 ~~ pj12
                          pj21 ~~ pj22
                          pj31 ~~ pj32
                          pj41 ~~ pj42 

                          pj1 ~ 0*1
                          pj2 ~ 1 
                '
cfa.pj.weak.fit <- cfa(cfa.pj.weak, std.lv=T, data=df, missing='fiml')

cfa.pj.strong <- ' pj1 =~ a1*pj11 + a2*pj21 + a3*pj31 + a4*pj41
                   pj2 =~ a1*pj12 + a2*pj22 + a3*pj32 + a4*pj42

                          pj11 ~ c1*1
                          pj21 ~ c2*1
                          pj31 ~ c3*1
                          pj41 ~ c4*1
                          pj12 ~ c1*1
                          pj22 ~ c2*1
                          pj32 ~ c3*1
                          pj42 ~ c4*1

                          pj11 ~~ pj12
                          pj21 ~~ pj22
                          pj31 ~~ pj32
                          pj41 ~~ pj42 

                          pj1 ~ 0*1
                          pj2 ~ 1
              '
cfa.pj.strong.fit <- cfa(cfa.pj.strong, std.lv=T, data=df, missing='fiml')

cfa.pj.strict <- ' pj1 =~ a1*pj11 + a2*pj21 + a3*pj31 + a4*pj41
                   pj2 =~ a1*pj12 + a2*pj22 + a3*pj32 + a4*pj42

                          pj11 ~ c1*1
                          pj21 ~ c2*1
                          pj31 ~ c3*1
                          pj41 ~ c4*1
                          pj12 ~ c1*1
                          pj22 ~ c2*1
                          pj32 ~ c3*1
                          pj42 ~ c4*1

                          pj11 ~~ pj12
                          pj21 ~~ pj22
                          pj31 ~~ pj32
                          pj41 ~~ pj42 

                          pj11 ~~ u1*pj11
                          pj21 ~~ u2*pj21
                          pj31 ~~ u3*pj31
                          pj31 ~~ u4*pj41
                          pj12 ~~ u1*pj12
                          pj22 ~~ u2*pj22
                          pj32 ~~ u3*pj32
                          pj42 ~~ u4*pj42 

                          pj1 ~ 0*1
                          pj2 ~ 1
              '
cfa.pj.strict.fit <- cfa(cfa.pj.strict, std.lv=T, data=df, missing='fiml')  

lavTestLRT(cfa.pj.configural.fit, cfa.pj.weak.fit)  # p=0.4039
lavTestLRT(cfa.pj.weak.fit, cfa.pj.strong.fit)      # p=0.7614
lavTestLRT(cfa.pj.strong.fit, cfa.pj.strict.fit)    # p<0.0001

round(cbind(no_constraint=inspect(cfa.pj.configural.fit, 'fit.measures'), 
            weak=inspect(cfa.pj.weak.fit, 'fit.measures'),
            strong=inspect(cfa.pj.strong.fit, 'fit.measures'),
            strict=inspect(cfa.pj.strong.fit, 'fit.measures'))[c('chisq', 'rmsea',
                                                                 'cfi', 'tli',
                                                                 'aic', 'bic'
                                                                 ), ], 3)


# Trust in police effectiveness

cfa.eff.configural <- ' eff1 =~ eff11 + eff21 + eff31 
                        eff2 =~ eff12 + eff22 + eff32 

                        eff11 ~~ eff12
                        eff21 ~~ eff22
                        eff31 ~~ eff32 '
cfa.eff.configural.fit <- cfa(cfa.eff.configural, std.lv=T, data=df, missing='fiml')

cfa.eff.weak <- ' eff1 =~ a1*eff11 + a2*eff21 + a3*eff31
                  eff2 =~ a1*eff12 + a2*eff22 + a3*eff32

                  eff11 ~~ eff12
                  eff21 ~~ eff22
                  eff31 ~~ eff32

                  eff1 ~ 0*1
                  eff2 ~ 1 '
cfa.eff.weak.fit <- cfa(cfa.eff.weak, std.lv=T, data=df, missing='fiml')

cfa.eff.strong <- ' eff1 =~ a1*eff11 + a2*eff21 + a3*eff31 
                    eff2 =~ a1*eff12 + a2*eff22 + a3*eff32 

                    eff11 ~ c1*1
                    eff21 ~ c2*1
                    eff31 ~ c3*1

                    eff12 ~ c1*1
                    eff22 ~ c2*1
                    eff32 ~ c3*1

                    eff11 ~~ eff12
                    eff21 ~~ eff22
                    eff31 ~~ eff32 

                    eff1 ~ 0*1
                    eff2 ~ 1'
cfa.eff.strong.fit <- cfa(cfa.eff.strong, std.lv=T, data=df, missing='fiml')


cfa.eff.strict <- ' eff1 =~ a1*eff11 + a2*eff21 + a3*eff31 
                    eff2 =~ a1*eff12 + a2*eff22 + a3*eff32 

                    eff11 ~ c1*1
                    eff21 ~ c2*1
                    eff31 ~ c3*1

                    eff12 ~ c1*1
                    eff22 ~ c2*1
                    eff32 ~ c3*1

                    eff11 ~~ eff12
                    eff21 ~~ eff22
                    eff31 ~~ eff32 

                    eff11 ~~ u1*eff11
                    eff21 ~~ u2*eff21
                    eff31 ~~ u3*eff31
                    eff12 ~~ u1*eff12
                    eff22 ~~ u2*eff22
                    eff32 ~~ u3*eff32

                    eff1 ~ 0*1
                    eff2 ~ 1'
cfa.eff.strict.fit <- cfa(cfa.eff.strict, std.lv=T, data=df, missing='fiml')    

lavTestLRT(cfa.eff.configural.fit, cfa.eff.weak.fit)
lavTestLRT(cfa.eff.weak.fit, cfa.eff.strong.fit)    
lavTestLRT(cfa.eff.strong.fit, cfa.eff.strict.fit)  

round(cbind(configural=inspect(cfa.eff.configural.fit, 'fit.measures'), 
            weak=inspect(cfa.eff.weak.fit, 'fit.measures'),
            strong=inspect(cfa.eff.strong.fit, 'fit.measures'),
            strict=inspect(cfa.eff.strict.fit, 'fit.measures'))[c('chisq', 'rmsea',
                                                                  'cfi', 'tli',
                                                                  'aic', 'bic'
                                                                  ), ], 3)