load('data/df_contact.RData')

if('MplusAutomation' %in% installed.packages()[, "Package"] == F) {
  install.packages('MplusAutomation')
}
library(MplusAutomation)

prepareMplusData(df_contact, 'mplus_data/data_for_lca.dat')