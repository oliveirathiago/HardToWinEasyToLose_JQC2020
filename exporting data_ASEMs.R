source("A4_LCA models.R")
library(MplusAutomation)

## Exporting data to mplus

prepareMplusData(df, "mplus_data/fulldata.dat")

load('data/df_contact.RData')
df_contact <- df_contact %>%
  mutate_at(vars(proc12:outcome42), as.numeric)

prepareMplusData(df_contact, "mplus_data/contactdata.dat")

