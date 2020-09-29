load('data/df.Rdata')

packages <- list("ggplot2", "dplyr", "MplusAutomation")
install <- packages[(!packages %in% installed.packages()[, "Package"])]
if(length(install)) install.packages(packages)
library(ggplot2)
library(dplyr)

pjcl  <- read.table('data/pjcl.txt')
outcl <- read.table('data/outcl.txt')

pjcl <- pjcl %>%
  rename('id' = 'V5',
         'pjcl' = 'V9')

outcl <- outcl %>%
  rename('id' = 'V5',
         'outcl' = 'V9')

df <- left_join(df, pjcl[, c('id', 'pjcl')], by = 'id') %>%
  left_join(outcl[, c('id', 'outcl')], by = 'id')

df$pjcl[df$contact1==0] <- 0
df$outcl[df$contact1==0] <- 0

df$pjcl <- df$pjcl %>%
  recode('0' = '1',   # no contact (coded 1)
         '1' = '2',   # positive contact (coded 2)
         '3' = '3',   # negative contact (coded 3)
         '2' = '4')   # neutral contact (coded 4)

df$outcl <- df$outcl %>%
  recode('0' = '1',   # no contact (coded 1)
         '1' = '2',   # positive contact (coded 2)
         '3' = '3',   # negative contact (coded 3)
         '2' = '4')   # neutral contact (coded 4)

df <- df %>%
  mutate_at(vars(pjcl, outcl), as.numeric)

###########################
# Graphs

resp <- factor(rep(c("Strongly disagree", "Disagree",
                     "Neither agree nor disagree",
                     "Agree", "Strongly agree"),12), levels = c("Strongly disagree", "Disagree",
                                                                "Neither agree nor disagree",
                                                                "Agree", "Strongly agree"))
ind_proc <- c(rep('proc12',15),
              rep('proc32',15),
              rep('proc42',15),
              rep('proc52',15))

ind_outcome <- c(rep('outcome12',15),
                 rep('outcome22',15),
                 rep('outcome32',15),
                 rep('outcome42',15))

class_proc <- as.factor(rep(rep(1:3, each = 5),4))
class_outcome <- as.factor(rep(rep(1:3, each = 5),4))

prob_proc <- c(0.000, 0.000, 0.113, 0.221, 0.666, # proc12 class 1
               0.005, 0.000, 0.112, 0.872, 0.011, # proc12 class 2
               0.056, 0.181, 0.565, 0.186, 0.011, # proc12 class 3
               0.054, 0.081, 0.187, 0.189, 0.488, # proc32 class 1
               0.000, 0.076, 0.244, 0.680, 0.000, # proc32 class 2
               0.084, 0.228, 0.641, 0.047, 0.000, # proc32 class 3
               0.000, 0.000, 0.032, 0.030, 0.938, # proc42 class 1
               0.000, 0.000, 0.122, 0.878, 0.000, # proc42 class 2
               0.051, 0.139, 0.603, 0.206, 0.000, # proc42 class 3
               0.000, 0.000, 0.000, 0.000, 1.000, # proc52 class 1
               0.005, 0.002, 0.021, 0.958, 0.013, # proc52 class 2
               0.061, 0.110, 0.434, 0.342, 0.053) # proc52 class 3

prob_outcome <- c(0.000, 0.000, 0.016, 0.055, 0.929, # out12 class 1
                  0.004, 0.000, 0.112, 0.879, 0.005, # out12 class 2
                  0.163, 0.325, 0.420, 0.078, 0.014, # out12 class 3
                  0.000, 0.000, 0.015, 0.027, 0.958, # out22 class 1
                  0.000, 0.000, 0.037, 0.963, 0.000, # out22 class 2
                  0.123, 0.239, 0.581, 0.051, 0.007, # out22 class 3
                  0.000, 0.029, 0.044, 0.075, 0.852, # out32 class 1
                  0.000, 0.005, 0.088, 0.894, 0.014, # out32 class 2
                  0.130, 0.170, 0.357, 0.327, 0.016, # out32 class 3
                  0.000, 0.000, 0.046, 0.105, 0.849, # out42 class 1
                  0.005, 0.026, 0.203, 0.755, 0.011, # out42 class 2
                  0.165, 0.333, 0.482, 0.020, 0.000) # out42 class 3
                  
data_process <- data.frame(resp = resp,
                           ind_proc = ind_proc,
                           prob_proc = prob_proc,
                           class_proc = class_proc)

data_process$class_proc <- recode(data_process$class_proc, 
                                  '1' = 'Positive',
                                  '2' = 'Neutral',
                                  '3' = 'Negative')
data_process$class_proc <- factor(data_process$class_proc,
                                  levels = c('Negative',
                                             'Neutral',
                                             'Positive'))

data_outcome <- data.frame(resp = resp,
                           ind_outcome = ind_outcome,
                           prob_outcome = prob_outcome,
                           class_outcome = class_outcome)

data_outcome$class_outcome <- recode(data_outcome$class_outcome, 
                                     '1' = 'Positive',
                                     '2' = 'Neutral',
                                     '3' = 'Negative')
data_outcome$class_outcome <- factor(data_outcome$class_outcome,
                                     levels = c('Negative',
                                                'Neutral',
                                                'Positive'))

ind.labs_proc <- c("Opportunities to express your views...",
                   "Your views were considered...",
                   "You were given an honest explanation...",
                   "You understood why the police...")
names(ind.labs_proc) <- c("proc12", "proc32", "proc42", "proc52")

ind.labs_outcome <- c("Satisfied with the outcome",
                      "The outcome was fair",
                      "The outcome was expected",
                      "The outcome was deserved")
names(ind.labs_outcome) <- c("outcome12", "outcome22", "outcome32", "outcome42")

graph_proc <- ggplot(data_process, aes(y = prob_proc, x = resp, group=class_proc, colour = class_proc)) + 
  geom_point(size=2) + geom_line() + facet_wrap( ~ ind_proc, ncol=2,
                                                 labeller = labeller(ind_proc = ind.labs_proc)) + 
  ylab("Probability") + xlab("") + labs(colour = "Latent class") +
  theme(axis.text.x = element_text(angle = 30, hjust=1))


graph_outcome <- ggplot(data_outcome, aes(y = prob_outcome, x = resp, group=class_outcome, colour = class_outcome)) + 
  geom_point(size=2) + geom_line() + facet_wrap( ~ ind_outcome, ncol=2,
                                                 labeller = labeller(ind_outcome = ind.labs_outcome)) + 
  ylab("Probability") + xlab("") + labs(colour = "Latent class") +
  theme(axis.text.x = element_text(angle = 30, hjust=1))

pdf('plots/LCA_process.pdf')
graph_proc
dev.off()

pdf('plots/LCA_outcome.pdf')
graph_outcome
dev.off()