rows <- c('process positive', 'process neutral', 'process negative',
          'outcome positive', 'outcome neutral', 'outcome negative')

pj_lb <- c(0.517 - 1.96 *0.123,  # process positive pj2
           0.114 - 1.96 *0.074,  # process neutral  pj2
           -0.256 - 1.96 *0.074, # process negative pj2
           0.271 - 1.96 *0.099,  # outcome positive pj2
           0.069 - 1.96 *0.066,  # outcome neutral pj2
           -0.365 - 1.96 *0.095) # outcome negative pj2

pj_ub <- c(0.517 + 1.96 *0.123,  # process positive pj2
           0.114 + 1.96 *0.074,  # process neutral  pj2
           -0.256 + 1.96 *0.074, # process negative pj2
           0.271 + 1.96 *0.099,  # outcome positive pj2
           0.069 + 1.96 *0.066,  # outcome neutral pj2
           -0.365 + 1.96 *0.095) # outcome negative pj2

eff_lb <- c(0.293 - 1.96 *0.172,  # process positive eff2
           0.010 - 1.96 *0.072,  # process neutral eff2
           -0.321 - 1.96 *0.071, # process negative eff2
           0.219 - 1.96 *0.123,  # outcome positive eff2
           0.011 - 1.96 *0.064,  # outcome neutral eff2
           -0.536 - 1.96 *0.086) # outcome negative eff2

eff_ub <- c(0.293 + 1.96 *0.172,  # process positive eff2
           0.010 + 1.96 *0.072,  # process neutral eff2
           -0.321 + 1.96 *0.071, # process negative eff2
           0.219 + 1.96 *0.123,  # outcome positive eff2
           0.011 + 1.96 *0.064,  # outcome neutral eff2
           -0.536 + 1.96 *0.086) # outcome negative eff2

duty_lb <- c(0.721 - 1.96 *0.127,  # process positive duty2
            -0.134 - 1.96 *0.080, # process neutral duty2
            -0.199 - 1.96 *0.079, # process negative duty
            0.577 - 1.96 *0.117,  # outcome positive duty2
            -0.158 - 1.96 *0.074, # outcome neutral duty2
            -0.280 - 1.96 *0.095) # outcome negative duty2

duty_ub <- c(0.721 + 1.96 *0.127,  # process positive duty2
            -0.134 + 1.96 *0.080, # process neutral duty2
            -0.199 + 1.96 *0.079, # process negative duty
            0.577 + 1.96 *0.117,  # outcome positive duty2
            -0.158 + 1.96 *0.074, # outcome neutral duty2
            -0.280 + 1.96 *0.095) # outcome negative duty2

table_ci <- data.frame(pj_lb = pj_lb, pj_ub = pj_ub,
                       eff_lb = eff_lb, eff_ub = eff_ub,
                       duty_lb = duty_lb, duty_ub = duty_ub)

row.names(table_ci) <- rows

table_ci