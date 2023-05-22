library(gt)
library(gtsummary)
library(sjPlot)
library(jtools)

setwd("Tables and Figures/")

# Table S1: Included vs. Excluded
tables1 <- tbl_summary(
          hps,
          by = study,
          digits = list(all_continuous() ~ 1,
                        all_categorical() ~ c(0,1)),
          type = NULL,
          value = NULL,
          missing = "ifany",
          missing_text = NULL,
          sort = NULL,
          percent = "column",
          include = c(age,raceeth,gender, educ, income_recode, marital_recode,thhld_numper,
                      seeing_recode2, hearing_recode2, remembering_recode2, mobility_recode2, selfcare_recode2, understand_recode2,
                      symptmnow_recode1, symptomimpact),
          label = list(age ~ "Age", 
                       raceeth ~ "Race/Ethnicity", 
                       gender ~"Gender",
                       educ ~ "Education", 
                       income_recode ~ "Income (2021)", 
                       marital_recode ~ "Marital Status",
                       thhld_numper ~ "People in Household",
                       seeing_recode2 ~ "Difficuly seeing", 
                       hearing_recode2 ~ "Difficulty hearing", 
                       remembering_recode2 ~ "Difficulty cognitive", 
                       mobility_recode2 ~ "Difficulty with mobility", 
                       selfcare_recode2 ~ "Difficulty with self-care", 
                       understand_recode2 ~ "Difficulty communicating",
                       symptmnow_recode1 ~ "Current symptoms",
                       symptomimpact ~ "Symptom impact on day-to-day activities"
                       
                       
                       )) %>% 
    add_overall(last = TRUE) %>% 
    add_p(list(all_continuous() ~ "t.test", all_categorical() ~ "chisq.test"))

tables1 %>% as_gt() %>% gtsave(file = "Table S1.rtf")
tables1 %>% as_gt() %>% gtsave(file = "Table S1.html")


# Table 4 Models: Functional Impairment - Difficulty w/ Household Expenses
t4a_hearing <- svyglm(dv_difficulty2 ~ hearing_recode2 + tenure_recode2 + age + gender + week_f, 
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t4a_seeing <- svyglm(dv_difficulty2 ~ seeing_recode2 + tenure_recode2 + age + gender + week_f, 
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t4a_remembering <- svyglm(dv_difficulty2 ~ remembering_recode2 + tenure_recode2 + age + gender + week_f, 
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t4a_mobility <- svyglm(dv_difficulty2 ~ mobility_recode2 + tenure_recode2 + age + gender + week_f, 
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t4a_selfcare <- svyglm(dv_difficulty2 ~ selfcare_recode2 + tenure_recode2 + age + gender + week_f, 
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t4a_understand <- svyglm(dv_difficulty2 ~ understand_recode2 + tenure_recode2 + age + gender + week_f, 
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))


# Table 4 Models: Functional Impairment - Behind on Housing Payments
t4b_hearing <- svyglm(dv_current ~ hearing_recode2 + tenure_recode2 + age + gender + week_f, 
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t4b_seeing <- svyglm(dv_current ~ seeing_recode2 + tenure_recode2 + age + gender + week_f, 
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t4b_remembering <- svyglm(dv_current ~ remembering_recode2 + tenure_recode2 + age + gender + week_f, 
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t4b_mobility <- svyglm(dv_current ~ mobility_recode2 + tenure_recode2 + age + gender + week_f, 
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t4b_selfcare <- svyglm(dv_current ~ selfcare_recode2 + tenure_recode2 + age + gender + week_f, 
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t4b_understand <- svyglm(dv_current ~ understand_recode2 + tenure_recode2 + age + gender + week_f, 
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

# Table 4 Models: Functional Impairment - Risk of Eviction/Foreclosure
t4c_hearing <- svyglm(dv_evictforeclose ~ hearing_recode2 + tenure_recode2 + age + gender + week_f, 
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t4c_seeing <- svyglm(dv_evictforeclose ~ seeing_recode2 + tenure_recode2 + age + gender + week_f, 
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t4c_remembering <- svyglm(dv_evictforeclose ~ remembering_recode2 + tenure_recode2 + age + gender + week_f, 
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t4c_mobility <- svyglm(dv_evictforeclose ~ mobility_recode2 + tenure_recode2 + age + gender + week_f, 
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t4c_selfcare <- svyglm(dv_evictforeclose ~ selfcare_recode2 + tenure_recode2 + age + gender + week_f, 
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t4c_understand <- svyglm(dv_evictforeclose ~ understand_recode2 + tenure_recode2 + age + gender + week_f, 
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

export_summs(t4a_hearing, t4a_seeing, t4a_remembering, t4a_understand, t4a_mobility,t4a_selfcare,
             t4b_hearing, t4b_seeing, t4b_remembering, t4b_understand, t4b_mobility,t4b_selfcare,
             t4c_hearing, t4c_seeing, t4c_remembering, t4c_understand, t4c_mobility,t4c_selfcare,
             exp=TRUE,
             error_format = "[{conf.low}, {conf.high}]",
             error_pos = "same",
                   coefs = c("Moderate" = "hearing_recode2Moderate difficulty",
                             "Severe" = "hearing_recode2Severe difficulty",
                             
                             "Moderate" = "seeing_recode2Moderate difficulty",
                             "Severe" = "seeing_recode2Severe difficulty",
                             
                             "Moderate" = "remembering_recode2Moderate difficulty",
                             "Severe" = "remembering_recode2Severe difficulty",
                             
                             "Moderate" = "understand_recode2Moderate difficulty",
                             "Severe" = "understand_recode2Severe difficulty",
                             
                             "Moderate" = "mobility_recode2Moderate difficulty",
                             "Severe" = "mobility_recode2Severe difficulty",
                             
                             "Moderate" = "selfcare_recode2Moderate difficulty",
                             "Severe" = "selfcare_recode2Severe difficulty"),
             
             model.names = c("t4a_hearing", "t4a_seeing", "t4a_remembering", "t4a_understand", "t4a_mobility","t4a_selfcare",
             "t4b_hearing", "t4b_seeing", "t4b_remembering", "t4b_understand", "t4b_mobility","t4b_selfcare",
             "t4c_hearing", "t4c_seeing", "t4c_remembering", "t4c_understand", "t4c_mobility","t4c_selfcare"),
             to.file = "xlsx",
             file.name = "Table S2.xlsx"
)

################### SENSITIVITY ANALYSES ###################################################################

##### Table 2a: Difficulty with household expenses ##### 
t2a_adj <- svyglm(dv_difficulty2  ~ longcovid_positives*tenure_recode2 + age + gender + week_f + educ + income + thhld_numper, 
                    design =  hps_svy_include, 
                    family=quasipoisson(link="log"))

###### Table 2b: Behind on rent/mortgage payments ##### 
t2b_adj <- svyglm(dv_current  ~ longcovid_positives*tenure_recode2 + age + gender + week_f + educ + income + thhld_numper, 
                    design = hps_svy_include, 
                    family=quasipoisson(link="log"))

##### Table 2d: Likely Eviction or Foreclosure ##### 
t2d_adj <- svyglm(dv_evictforeclose  ~ longcovid_positives*tenure_recode2 + age + gender + week_f + educ + income + thhld_numper, 
                    design = hps_svy_include, 
                    family=quasipoisson(link="log"))

##### TABLE 2 Output: Summarize Results  ##### 

export_summs(t2a_adj,t2b_adj,t2d_adj, 
             exp=TRUE,ci_level = .95,
             error_format = "({conf.low}-{conf.high})",
             model.names = c("Difficulty With Expenses",
                             "Behind on Rent/Mortgage",
                             #"Rental Assistance Application",
                             "Likely Eviction/Foreclosure"),
               #coefs = c("Long COVID" = "longcovid_positivesLong COVID"),
             to.file = "docx",
             file.name = "Table S3.docx")

##### Table 3a: Current Long COVID ##### 
t3a1_adj     <- svyglm(dv_difficulty2 ~ symptmnow_recode1 + tenure_recode2 + age + gender + week_f + raceeth + educ + income + thhld_numper, 
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t3a2_adj     <- svyglm(dv_current ~ symptmnow_recode1 + tenure_recode2 + age + gender + week_f + raceeth + educ + income + thhld_numper, 
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t3a3_adj     <- svyglm(dv_evictforeclose ~ symptmnow_recode1 + tenure_recode2 + age + gender + week_f + raceeth + educ + income + thhld_numper,  
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

###### Table 3b: Symptom Impact
t3b1_adj <- svyglm(dv_difficulty2 ~ symptomimpact + tenure_recode2 + age + gender + week_f + raceeth + educ + income + thhld_numper, 
                    design =  subset(hps_svy_include, longcovid == 1 & symptmnow == 1), 
                    family=quasipoisson(link="log"))

t3b2_adj <- svyglm(dv_current ~ symptomimpact + tenure_recode2 + age + gender + week_f + raceeth + educ + income + thhld_numper, 
                    design =  subset(hps_svy_include, longcovid == 1 & symptmnow == 1), 
                    family=quasipoisson(link="log"))

t3b3_adj <- svyglm(dv_evictforeclose ~ symptomimpact + tenure_recode2 + age + gender + week_f + raceeth + educ + income + thhld_numper, 
                    design =  subset(hps_svy_include, longcovid == 1 & symptmnow == 1), 
                    family=quasipoisson(link="log"))

export_summs(t3a1_adj, t3a2_adj, t3a3_adj, t3b1_adj, t3b2_adj, t3b3_adj, 
             exp=TRUE,ci_level = .95,
             error_format = "({conf.low}-{conf.high})",
             model.names = c("Current Symptoms - Difficulty w/ Expenses",
                             "Current Symptoms - Current w/ Payments",
                             "Current Symptoms - Likely Eviction",
                             "Symptom Impact - Difficulty w/ Expenses",
                             "Symptom Impact - Current w/ Payments",
                             "Symptom Impact - Likely Eviction"),
               coefs = c("Current Symptoms" = "symptmnow_recode11",
                         "Symptom Impact - A little" = "symptomimpactA little",
                         "Symptom Impact - A lot" = "symptomimpactA lot"),
             
             to.file = "docx",
             file.name = "Table S4.docx")

# Table 4 Models: Functional Impairment - Difficulty w/ Household Expenses
t4a_hearing_adj <- svyglm(dv_difficulty2 ~ hearing_recode2 + tenure_recode2 + age + gender + week_f + raceeth + educ + income + thhld_numper, 
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t4a_seeing_adj <- svyglm(dv_difficulty2 ~ seeing_recode2 + tenure_recode2 + age + gender + week_f + raceeth + educ + income + thhld_numper, 
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t4a_remembering_adj <- svyglm(dv_difficulty2 ~ remembering_recode2 + tenure_recode2 + age + gender + week_f + raceeth + educ + income + thhld_numper, 
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t4a_mobility_adj <- svyglm(dv_difficulty2 ~ mobility_recode2 + tenure_recode2 + age + gender + week_f + raceeth + educ + income + thhld_numper, 
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t4a_selfcare_adj <- svyglm(dv_difficulty2 ~ selfcare_recode2 + tenure_recode2 + age + gender + week_f + raceeth + educ + income + thhld_numper, 
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t4a_understand_adj <- svyglm(dv_difficulty2 ~ understand_recode2 + tenure_recode2 + age + gender + week_f + raceeth + educ + income + thhld_numper, 
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))


# Table 4 Models: Functional Impairment - Behind on Housing Payments
t4b_hearing_adj <- svyglm(dv_current ~ hearing_recode2 + tenure_recode2 + age + gender + week_f + raceeth + educ + income + thhld_numper,  
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t4b_seeing_adj <- svyglm(dv_current ~ seeing_recode2 + tenure_recode2 + age + gender + week_f + raceeth + educ + income + thhld_numper,  
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t4b_remembering_adj <- svyglm(dv_current ~ remembering_recode2 + tenure_recode2 + age + gender + week_f + raceeth + educ + income + thhld_numper,  
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t4b_mobility_adj <- svyglm(dv_current ~ mobility_recode2 + tenure_recode2 + age + gender + week_f + raceeth + educ + income + thhld_numper,  
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t4b_selfcare_adj <- svyglm(dv_current ~ selfcare_recode2 + tenure_recode2 + age + gender + week_f + raceeth + educ + income + thhld_numper,  
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t4b_understand_adj <- svyglm(dv_current ~ understand_recode2 + tenure_recode2 + age + gender + week_f + raceeth + educ + income + thhld_numper,  
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

# Table 4 Models: Functional Impairment - Risk of Eviction/Foreclosure
t4c_hearing_adj <- svyglm(dv_evictforeclose ~ hearing_recode2 + tenure_recode2 + age + gender + week_f + raceeth + educ + income + thhld_numper,  
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t4c_seeing_adj <- svyglm(dv_evictforeclose ~ seeing_recode2 + tenure_recode2 + age + gender + week_f + raceeth + educ + income + thhld_numper,  
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t4c_remembering_adj <- svyglm(dv_evictforeclose ~ remembering_recode2 + tenure_recode2 + age + gender + week_f + raceeth + educ + income + thhld_numper,  
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t4c_mobility_adj <- svyglm(dv_evictforeclose ~ mobility_recode2 + tenure_recode2 + age + gender + week_f + raceeth + educ + income + thhld_numper,  
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t4c_selfcare_adj <- svyglm(dv_evictforeclose ~ selfcare_recode2 + tenure_recode2 + age + gender + week_f + raceeth + educ + income + thhld_numper,  
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t4c_understand_adj <- svyglm(dv_evictforeclose ~ understand_recode2 + tenure_recode2 + age + gender + week_f + raceeth + educ + income + thhld_numper,  
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

export_summs(t4a_hearing_adj, t4a_seeing_adj, t4a_remembering_adj, t4a_understand_adj, t4a_mobility_adj,t4a_selfcare_adj,
             t4b_hearing_adj, t4b_seeing_adj, t4b_remembering_adj, t4b_understand_adj, t4b_mobility_adj,t4b_selfcare_adj,
             t4c_hearing_adj, t4c_seeing_adj, t4c_remembering_adj, t4c_understand_adj, t4c_mobility_adj,t4c_selfcare_adj,
             exp=TRUE,
             error_format = "[{conf.low}, {conf.high}]",
             error_pos = "same",
                   coefs = c("Moderate" = "hearing_recode2Moderate difficulty",
                             "Severe" = "hearing_recode2Severe difficulty",
                             
                             "Moderate" = "seeing_recode2Moderate difficulty",
                             "Severe" = "seeing_recode2Severe difficulty",
                             
                             "Moderate" = "remembering_recode2Moderate difficulty",
                             "Severe" = "remembering_recode2Severe difficulty",
                             
                             "Moderate" = "understand_recode2Moderate difficulty",
                             "Severe" = "understand_recode2Severe difficulty",
                             
                             "Moderate" = "mobility_recode2Moderate difficulty",
                             "Severe" = "mobility_recode2Severe difficulty",
                             
                             "Moderate" = "selfcare_recode2Moderate difficulty",
                             "Severe" = "selfcare_recode2Severe difficulty"),
             
             model.names = c("t4a_hearing", "t4a_seeing", "t4a_remembering", "t4a_understand", "t4a_mobility","t4a_selfcare",
             "t4b_hearing", "t4b_seeing", "t4b_remembering", "t4b_understand", "t4b_mobility","t4b_selfcare",
             "t4c_hearing", "t4c_seeing", "t4c_remembering", "t4c_understand", "t4c_mobility","t4c_selfcare"),
             to.file = "xlsx",
             file.name = "Table S5.xlsx"
)

