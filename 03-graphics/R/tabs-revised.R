# Code for tables to accompany 03-draft-revised---------------------------------

# TABLE: DEMOGRAPHICS--- -------------------------------------------------------
labels(mockdata) <- c(sex = "Gender", 
                      race = 'Race',
                      age = 'Age, years',
                      bmi = 'Body Mass Index (kg/m2)')

demo_tab <- mockdata %>% 
  tableby(arm ~ 
            chisq(sex, digits.pct = 1) + 
            chisq(race, digits.pct = 1) +
            anova(age, digits = 1) +  
            anova(bmi, digits = 1), 
          data = .) %>% 
  summary(digits = 2, 
          digits.p = 2, 
          digits.pct = 1, 
          pfootnote = TRUE) 

# TABLE: Follow-up time in days and status--------------------------------------
labels(mockdata) <- c(fu_time = 'Follow-up (days)',
                      fu_fct = 'Vital Status')

mycontrols <- tableby.control(numeric.stats=c("median", "q1q3"))

fu_tab <- mockdata %>% 
  tableby(arm ~ 
            chisq(fu_fct, digits.pct = 1) +
            anova(fu_time, digits = 0), 
          data = .,
          control = mycontrols)  %>% 
  summary(digits = 2, 
          digits.p = 2, 
          digits.pct = 1, 
          pfootnote = TRUE)

# TABLE: adverse events---------------------------------------------------------
ae_tab <- mockdata %>% 
  tableby(arm ~ ae_blood_clot + ae_vomiting + ae_diarrhea + ae_neuropathy + ae_low_wbc, 
          data = .) %>% 
  summary(digits = 2, 
          digits.p = 2, 
          digits.pct = 1, 
          pfootnote = TRUE)
