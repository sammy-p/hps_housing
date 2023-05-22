library(gt)
library(gtsummary)
library(sjPlot)
library(jtools)

setwd("Tables and Figures/")

######## Table 1 ######## 
# Table: Distribution of Housing Insecurity Indicators and Demographic Covariates by COVID-19 Status
table1 <- tbl_svysummary(
          hps_svy_include,
          by = longcovid,
          statistic = list(all_continuous() ~ "{mean} ({sd})",
                           all_categorical() ~ "{n_unweighted} ({p}%)"),
          digits = list(all_continuous() ~ 1,
                        all_categorical() ~ c(0,1)),
          missing = "ifany",
          percent = "column",
          include = c(agegp,raceeth,gender, educ, income_recode, marital_recode,thhld_numper,
                      tenure_recode,current,monthsbehind_recode2,
                      evict_foreclose ,rentassist_recode,difficulty,
                      seeing_recode2, hearing_recode2, remembering_recode2, mobility_recode2, selfcare_recode2, understand_recode2,
                      symptmnow_recode1, symptomimpact),
          
          label = list(agegp ~ "Age", 
                       raceeth ~ "Race/Ethnicity", 
                       gender ~"Gender",
                       educ ~ "Education", 
                       income_recode ~ "Income (2021)", 
                       marital_recode ~ "Marital Status",
                       thhld_numper ~ "People in Household",
                       tenure_recode ~ "Housing Tenure", 
                       current ~ "Current on Rent/Mortgage Payments", 
                       monthsbehind_recode2 ~ "Months Behind on Rent/Mortgage Payments",
                       evict_foreclose ~ "Eviction/Foreclosure in Next 2 Months",
                       rentassist_recode ~ "Application for Emergency Rental Assistance",
                       difficulty ~ "Difficulty Making Living Expenses",
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
    #add_p(all_categorical() ~ "svy.chisq.test")

table1

table1 %>% as_gt() %>% gtsave(file = "Table1.rtf")

####################### Descriptive Statistics ####################### 

# Prevalence of outcome and symptom variables

prop.table(svytable(~insecurity_index,design=hps_svy_include))
svyciprop(~insecurity_index2
          ,design=hps_svy_include)

svyciprop(~dv_difficulty2,design=hps_svy_include)
svyciprop(~dv_current,design=hps_svy_include)
svyciprop(~dv_evictforeclose,design=hps_svy_include)
svyciprop(~dv_rentassist,design=hps_svy_include)

svyciprop(~symptmnow_recode1,design=subset(hps_svy_include,longcovid==1))
svyciprop(~symptomimpact,design=subset(hps_svy_include,longcovid==1))

######## Table 2 ######## 
# Table 2 Models: Difficulty with household expenses ##### 
t2a     <- svyglm(dv_difficulty2  ~ longcovid_positives*tenure_recode2 + age + gender + week_f, 
                    design = hps_svy_include, 
                    family=quasipoisson(link="log"))

# Table 2 Models: Behind on rent/mortgage payments ##### 
t2b     <- svyglm(dv_current  ~ longcovid_positives*tenure_recode2 + age + gender + week_f, 
                    design = hps_svy_include, 
                    family=quasipoisson(link="log"))

# Table 2 Models: Likely Eviction or Foreclosure
t2c     <- svyglm(dv_evictforeclose  ~ longcovid_positives*tenure_recode2 + age + gender + week_f, 
                    design = hps_svy_include, 
                    family=quasipoisson(link="log"))

#Table 2 Output: Summarize Results

export_summs(t2a,t2b,t2c, 
             exp=TRUE,ci_level = .95,
             error_format = "({conf.low}-{conf.high})",
             model.names = c("Difficulty With Expenses",
                             "Behind on Rent/Mortgage",
                             #"Rental Assistance Application",
                             "Likely Eviction/Foreclosure"),
               #coefs = c("Long COVID" = "longcovid_positivesLong COVID"),
             to.file = "docx",
             file.name = "C:/Users/subli/Dropbox/Projects/COVID/HPS Papers/Precarious Housing/Tables and Figures/Table 2a.docx")




######## Table 3a: Current Long COVID ######## 
t3a1     <- svyglm(dv_difficulty2 ~ symptmnow_recode1 + tenure_recode2 + age + gender + week_f, 
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t3a2     <- svyglm(dv_current ~ symptmnow_recode1 + tenure_recode2 + age + gender + week_f, 
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

t3a3     <- svyglm(dv_evictforeclose ~ symptmnow_recode1 + tenure_recode2 + age + gender + week_f, 
                    design =  subset(hps_svy_include, longcovid == 1), 
                    family=quasipoisson(link="log"))

###### Table 3b: Symptom Impact
t3b1 <- svyglm(dv_difficulty2 ~ symptomimpact + tenure_recode2 + age + gender + week_f, 
                    design =  subset(hps_svy_include, longcovid == 1 & symptmnow == 1), 
                    family=quasipoisson(link="log"))

t3b2 <- svyglm(dv_current ~ symptomimpact + tenure_recode2 + age + gender + week_f, 
                    design =  subset(hps_svy_include, longcovid == 1 & symptmnow == 1), 
                    family=quasipoisson(link="log"))

t3b3 <- svyglm(dv_evictforeclose ~ symptomimpact + tenure_recode2 + age + gender + week_f, 
                    design =  subset(hps_svy_include, longcovid == 1 & symptmnow == 1), 
                    family=quasipoisson(link="log"))

export_summs(t3a1, t3a2, t3a3, t3b1, t3b2, t3b3, 
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
             file.name = "C:/Users/subli/Dropbox/Projects/COVID/HPS Papers/Precarious Housing/Tables and Figures/Table 3.docx")

######## Table 4 ######## 

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
             file.name = "C:/Users/subli/Dropbox/Projects/COVID/HPS Papers/Precarious Housing/Tables and Figures/Table S2.xlsx"
)


export_summs(t4a_hearing, t4a_seeing, t4a_remembering, t4a_understand, t4a_mobility,t4a_selfcare,
             t4b_hearing, t4b_seeing, t4b_remembering, t4b_understand, t4b_mobility,t4b_selfcare,
             t4c_hearing, t4c_seeing, t4c_remembering, t4c_understand, t4c_mobility,t4c_selfcare,
             exp=TRUE,
             error_format = "",
             error_pos = "same",
             stars=NULL,
             statistics = c(""),
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
             file.name = "C:/Users/subli/Dropbox/Projects/COVID/HPS Papers/Precarious Housing/Tables and Figures/Table S2b.xlsx"
)

######## Figure 1: Functional Impairment and Housing ######## 
library(broom)

model_names <- c("t4a_hearing", "t4a_seeing", "t4a_remembering", "t4a_understand", "t4a_mobility", "t4a_selfcare",
                 "t4b_hearing", "t4b_seeing", "t4b_remembering", "t4b_understand", "t4b_mobility", "t4b_selfcare",
                 "t4c_hearing", "t4c_seeing", "t4c_remembering", "t4c_understand", "t4c_mobility", "t4c_selfcare")

# Loop through the list of model names and apply tidy() function
fig2_results <- data.frame()
for (model_name in model_names) {
  model <- get(model_name)  # Get the model object
  tidy_result <- tidy(model, conf.int = TRUE)[2:3, c(1, 2, 6, 7)]  # Apply tidy() and extract specific columns
  tidy_result$model_name <- model_name  # Add model name as a column
  
  # Exponentiate coefficient estimates and confidence intervals
  tidy_result[, 2:4] <- exp(tidy_result[, 2:4])
  fig2_results <- rbind(fig2_results, tidy_result)
}

# Reset row names
rownames(fig2_results) <- NULL
fig2_results$term <- ifelse(grepl("Moderate", fig2_results$term), "Moderate Limitation",
                     ifelse(grepl("Severe", fig2_results$term), "Severe Limitation", 
                     fig2_results$term))

fig2_results$model <- ifelse(grepl("t4a", fig2_results$model_name), "Difficulty with Expenses",
                      ifelse(grepl("t4b", fig2_results$model_name), "Behind on Payments", 
                      ifelse(grepl("t4c", fig2_results$model_name), "Likely Eviction/Foreclosure", 
                      fig2_results$model_name)))

fig2_results$impairment <- ifelse(grepl("hearing", fig2_results$model_name), "Hearing",
                           ifelse(grepl("seeing", fig2_results$model_name), "Seeing", 
                           ifelse(grepl("remembering", fig2_results$model_name), "Cognitive", 
                           ifelse(grepl("understand", fig2_results$model_name), "Communication", 
                           ifelse(grepl("mobility", fig2_results$model_name), "Mobility",
                           ifelse(grepl("selfcare", fig2_results$model_name), "Self Care",

                           fig2_results$model_name))))))

fig2_results$model_name <- paste0(fig2_results$model_name,"_",fig2_results$term)
fig2_results <- fig2_results %>% arrange(term,model) 
fig2_results$modeln <- c(1:36)
bind_rows()
fig2_results


ggplot(fig2_results,aes(color=impairment)) + 
    geom_point(aes(x=estimate,y=model), position = position_dodge2(width=1)) + 
    geom_linerange(aes(y=model,xmin = conf.low, xmax = conf.high), position = position_dodge2(width=1)) +
    geom_vline(xintercept = 1,linetype = 2)+
    xlim(c(0,4.5)) + xlab("Prevalence Ratio (95% CI)") + ylab("") +
    facet_wrap(~term,ncol=1,strip.position = "right") + 
    #scale_y_discrete(limits = c("Hearing","Seeing","Mobility","Remembering","Self Care","Understand"),expand = expansion(add=1))+
    scale_y_discrete(limits = c("Behind on Payments",
                                "",
                                "Difficulty with Expenses",
                                "",
                                "Likely Eviction/Foreclosure"),
                     labels = c("Behind on Rent or \nMortgage Payments",
                                "",
                                "Significant Difficulty w/ \nHousehold Expenses",
                                "",
                                "Likely Eviction \nor Foreclosure"),
                     expand = expansion(add=1))+
    scale_color_manual(values=c('#e41a1c','#377eb8','#4daf4a','#984ea3','#ff7f00','#FFC900')) +
    theme_classic() + theme(legend.position = "bottom",legend.title= element_blank())
    

ggsave(filename = "C:/Users/subli/Dropbox/Projects/COVID/HPS Papers/Precarious Housing/Tables and Figures/Fig2 AJPH.tiff",
  plot = last_plot(),
  width = 5,
  height = 6,
  units = "in",
  dpi = 400
)


