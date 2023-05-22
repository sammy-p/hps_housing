library(tidyverse)
library(readr)
library(survey)

setwd("Data/")

# ### Read in raw data from 3 weeks of HPS survey
# hps49_raw <- read_csv("HPS_Week49_PUF_CSV/pulse2022_puf_49.csv")
# hps50_raw <- read_csv("HPS_Week50_PUF_CSV/pulse2022_puf_50.csv")
# hps51_raw <- read_csv("HPS_Week51_PUF_CSV/pulse2022_puf_51.csv")
# hps52_raw <- read_csv("HPS_Week52_PUF_CSV/pulse2022_puf_52.csv")
# hps53_raw <- read_csv("HPS_Week53_PUF_CSV/pulse2023_puf_53.csv")
# hps54_raw <- read_csv("HPS_Week54_PUF_CSV/pulse2023_puf_54.csv")
# hps55_raw <- read_csv("HPS_Week55_PUF_CSV/pulse2023_puf_55.csv")
# hps56_raw <- read_csv("HPS_Week56_PUF_CSV/pulse2023_puf_56.csv")
#
# ### Combine files
# hps_raw <- plyr::rbind.fill(hps49_raw,hps50_raw,hps51_raw,hps52_raw,hps53_raw,hps54_raw,hps55_raw,hps56_raw)
#
# save(hps_raw, file = "hps_raw49_56")
# rm(hps49_raw,hps50_raw,hps51_raw,hps52_raw,hps53_raw,hps54_raw,hps55_raw,hps56_raw)

load("hps_raw49_56")

### Subset, Label and Recode Variables
hps <- hps_raw %>%
  select(
    SCRAM, PWEIGHT, HWEIGHT, REGION, EST_MSA, EST_ST, WEEK,
    TBIRTH_YEAR, RHISPANIC, RRACE, GENID_DESCRIBE,
    EEDUC, INCOME, MS, SEXUAL_ORIENTATION,THHLD_NUMPER,
    HADCOVIDRV, WHENCOVID,
    SYMPTOMS, LONGCOVID, SYMPTMNOW, SYMPTMIMPCT,
    TENURE, LIVQTRRV, TRENTAMT, RENTCHNG,
    RENTCUR, MORTCUR, TMNTHSBHND,
    RENTASSIST, EXPNS_DIF, EVICT, FORCLOSE,
    SEEING, HEARING, REMEMBERING, MOBILITY, SELFCARE, UNDERSTAND
  ) %>%
  mutate(
    pweight8 = PWEIGHT / 8,
    age = 2022 - TBIRTH_YEAR,
    agegp = factor(
      case_when(
        age %in% c(17:24) ~ 1,
        age %in% c(25:44) ~ 2,
        age %in% c(45:64) ~ 3,
        age > 64 ~ 4
      ),
      levels = c(1, 2, 3, 4),
      labels = c("18-24", "25-44", "45-64", "65+")
    ),
    week_f = as.factor(WEEK),
    week_recode = factor(WEEK,
      levels = c(49, 50, 51, 52, 53, 54, 55, 56),
      labels = c(
        "September 14 - September 28",
        "October 5 - October 17",
        "November 2 - November 14",
        "December 9 - December 19",
        "January 4 - January 16",
        "February 1 - February 13",
        "March 1 - March 13",
        "March 29 - April 10"
      )
    ),
    gender = factor(case_when(GENID_DESCRIBE %in% c(1, 2, 3, 4, -99) ~ GENID_DESCRIBE),
      levels = c(1, 2, 3, 4, -99),
      labels = c(
        "Male",
        "Female",
        "Transgender",
        "None of the Above",
        "No Response"
      )
    ),
    educ = factor(
      case_when(
        EEDUC %in% c(1:2) ~ 1,
        EEDUC %in% c(3:3) ~ 2,
        EEDUC %in% c(4:5) ~ 3,
        EEDUC %in% c(6:6) ~ 4,
        EEDUC %in% c(7:7) ~ 5
      ),
      levels = c(1, 2, 3, 4, 5),
      labels = c(
        "Less Than High School",
        "High School Graduate",
        "Some College or Associates",
        "Bachelors Degree",
        "Graduate Degree"
      )
    ),
    raceeth = factor(
      case_when(
        RRACE == 1 & RHISPANIC == 1 ~ 1, # 1 White Non-Hispanic
        RRACE == 1 & RHISPANIC == 2 ~ 2, # 2 White Hispanic
        RRACE == 2 ~ 3, # 3 Black
        RRACE == 3 ~ 4, # 5 Asian
        RRACE == 4 ~ 5, # 6 Other
        # RRACE == -99 ~ -99,
        TRUE ~ NA_real_
      ),
      levels = c(1, 2, 3, 4, 5),
      labels = c(
        "White Non-Hispanic",
        "White Hispanic",
        "Black",
        "Asian",
        "Other"
      )
    ),
    income_recode = factor(
      case_when(
        INCOME %in% 7:8 ~ 7,
        TRUE ~ INCOME
      ),
      levels = c(1, 2, 3, 4, 5, 6, 7, -99),
      labels = c(
        "Less than $25,000",
        "$25,000 - $34,999",
        "$35,000 - $49,999",
        "$50,000 - $74,999",
        "$75,000 - $99,999",
        "$100,000 - $149,999",
        "$150,000 and above",
        "No Response"
      )
    ),
    marital_recode = factor(
      case_when(
        MS == 1 ~ 1,
        MS == 5 ~ 3,
        MS == -99 ~ -99,
        MS %in% c(2, 3, 4) ~ 2,
        MS %in% c(-88) ~ NA_real_
      ),
      levels = c(1, 2, 3, -99),
      labels = c(
        "Married",
        "Widowed/Divorced/Separated",
        "Never Married",
        "No Response"
      )
    ),

    longcovid_all = factor(
      case_when(
        HADCOVIDRV == 2 ~ 0,
        LONGCOVID == 1 ~ 1,
        LONGCOVID == 2 ~ 0,
        TRUE ~ NA_real_
      ),
      levels = c(0, 1),
      labels = c("No Long COVID", "Long COVID")
    ),
    longcovid_positives = factor(
      case_when(
        LONGCOVID == 1 ~ 1,
        LONGCOVID == 2 ~ 0,
        TRUE ~ NA_real_
      ),
      levels = c(0, 1),
      labels = c("No Long COVID", "Long COVID")
    ),
    hadcovid_recode1 = as.factor(case_when(
      HADCOVIDRV == 1 ~ 1,
      HADCOVIDRV == 2 ~ 0,
      TRUE ~ NA_real_
    )),
    
    symptmnow_recode1 = as.factor(case_when(
      SYMPTMNOW == 1 ~ 1,
      SYMPTMNOW == 2 ~ 0,
      TRUE ~ NA_real_
    )),
    symptomimpact = factor(
      case_when(
        SYMPTMIMPCT == 1 ~ 2,
        SYMPTMIMPCT == 2 ~ 1,
        SYMPTMIMPCT == 3 ~ 0
      ),
      levels = c(0, 1, 2),
      labels = c("Not at all", "A little", "A lot")
    ),
    symptomimpact_missing = factor(
      case_when(
        LONGCOVID == 1 &
          SYMPTMNOW == 1 &
          SYMPTMIMPCT %in% c(-88, -99) ~ 1,
        LONGCOVID == 1 &
          SYMPTMNOW == 1 &
          is.na(SYMPTMIMPCT) ~ 1,
        SYMPTMIMPCT %in% 1:3 ~ 0
      ),
      levels = c(0, 1)
    ),
    
    
    tenure_recode = relevel(factor(
      case_when(
        TENURE %in% c(1:4) ~ TENURE,
        TENURE %in% c(-88, -99) ~ NA_real_
      ),
      levels = c(1, 2, 3, 4),
      labels = c(
        "Owned, no mortgage",
        "Owned with mortgage/loan",
        "Rented",
        "Occupied without rent payments"
      )
    ), ref = 2),
    
    
    tenure_recode2 = factor(
      case_when(
        TENURE %in% c(1:2) ~ 1,
        TENURE %in% c(3:4) ~ 2,
        TENURE %in% c(-88, -99) ~ NA_real_
      ),
      levels = c(1, 2),
      labels = c(
        "Owned",
        "Rented"
      )
    ),
    difficulty = factor(
      case_when(
        EXPNS_DIF %in% c(1, 2, 3, 4) ~ EXPNS_DIF,
        EXPNS_DIF %in% c(-99,-88) ~ NA_real_
      ),
      levels = c(1, 2, 3, 4),
      labels = c(
        "Not at all difficult",
        "A little difficult",
        "Somewhat difficult",
        "Very difficult"
      )
    ),
    difficulty_recode = factor(
      case_when(
        EXPNS_DIF %in% c(1:2) ~ 0,
        EXPNS_DIF %in% c(3:4) ~ 1,
        EXPNS_DIF %in% c(-88:-99) ~ NA_real_
      ),
      levels = c(0, 1),
      labels = c(
        "Not at all or a little difficult",
        "Somewhat or very difficult"
      )
    ),
    difficulty_recode2 = factor(
      case_when(
        EXPNS_DIF %in% c(1:3) ~ 0,
        EXPNS_DIF %in% c(4:4) ~ 1,
        EXPNS_DIF %in% c(-88:-99) ~ NA_real_
      ),
      levels = c(0, 1),
      labels = c(
        "Not at all to Somewhat Difficult",
        "Very difficult"
      )
    ),
    dv_difficulty = case_when(
      EXPNS_DIF %in% c(1:2) ~ 0,
      EXPNS_DIF %in% c(3:4) ~ 1,
      EXPNS_DIF %in% c(-88:-99) ~ NA_real_
    ),
    dv_difficulty2 = case_when(
      EXPNS_DIF %in% c(1:3) ~ 0,
      EXPNS_DIF %in% c(4:4) ~ 1,
      EXPNS_DIF %in% c(-88:-99) ~ NA_real_
    ),
    current = factor(
      case_when(
        MORTCUR == 1 | RENTCUR == 1 ~ 0,
        MORTCUR == 2 | RENTCUR == 2 ~ 1,
        MORTCUR %in% c(-88, -99) & RENTCUR %in% c(-88, -99) ~ NA_real_
      ),
      levels = c(0, 1),
      labels = c("Current", "Not Current")
    ),
    dv_current = case_when(
      TENURE %in% c(1, 4) ~ 0,
      MORTCUR == 1 | RENTCUR == 1 ~ 0,
      MORTCUR == 2 | RENTCUR == 2 ~ 1,
      MORTCUR %in% c(-88, -99) & RENTCUR %in% c(-88, -99) ~ NA_real_
    ),
    evict_recode = factor(
      case_when(
        EVICT %in% c(1, 2, 3, 4, -99) ~ EVICT,
        EVICT %in% c(-88) ~ NA_real_
      ),
      levels = c(1, 2, 3, 4, -99),
      labels = c(
        "Very Likely",
        "Somewhat Likely",
        "Not Very Likely",
        "Not Likely at All",
        "No Response"
      )
    ),
    foreclose_recode = factor(
      case_when(
        FORCLOSE %in% c(1, 2, 3, 4, -99) ~ FORCLOSE,
        FORCLOSE %in% c(-88) ~ NA_real_
      ),
      levels = c(1, 2, 3, 4, -99),
      labels = c(
        "Very Likely",
        "Somewhat Likely",
        "Not Very Likely",
        "Not Likely at All",
        "No Response"
      )
    ),
    evict_foreclose = factor(
      case_when(
        FORCLOSE == 1 | EVICT == 1 ~ 1,
        FORCLOSE == 2 | EVICT == 2 ~ 2,
        FORCLOSE == 3 | EVICT == 3 ~ 3,
        FORCLOSE == 4 | EVICT == 4 ~ 4,
        # FORCLOSE == -99 & EVICT == -88 ~ -99,
        # FORCLOSE == -88 & EVICT == -99 ~ -99,
        RENTCUR == 1 | MORTCUR == 1 ~ 0
      ),
      levels = c(1, 2, 3, 4, 0),
      labels = c(
        "Very Likely",
        "Somewhat Likely",
        "Not Very Likely",
        "Not Likely at All",
        "Current on Rent/Mortgage"
      )
    ),
    dv_evictforeclose = case_when(
      TENURE %in% c(1, 4) ~ 0,
      RENTCUR == 1 | MORTCUR == 1 ~ 0,
      FORCLOSE %in% c(1:2) | EVICT %in% c(1:2) ~ 1,
      FORCLOSE %in% c(3:4) | EVICT %in% c(3:4) ~ 0
    ),
    rentcur_recode = factor(
      case_when(
        RENTCUR == 1 ~ 0,
        RENTCUR == 2 ~ 1,
        RENTCUR == -99 ~ -99,
        RENTCUR %in% c(-88) ~ NA_real_
      ),
      levels = c(0, 1, -99),
      labels = c(
        "Current",
        "Not Current",
        "No Response"
      )
    ),
    mortcur_recode = factor(
      case_when(
        MORTCUR == 1 ~ 0,
        MORTCUR == 2 ~ 1,
        MORTCUR == -99 ~ -99,
        MORTCUR %in% c(-88) ~ NA_real_
      ),
      levels = c(0, 1, -99),
      labels = c("Current", "Not Current", "No Response")
    ),
    monthsbehind = as.numeric(case_when(
      TMNTHSBHND %in% 0:8 ~ TMNTHSBHND,
      TMNTHSBHND %in% c(-88, -99) ~ NA_real_
    )),
    monthsbehind_recode = factor(
      case_when(
        TMNTHSBHND %in% c(0:1) ~ 1,
        TMNTHSBHND %in% c(2:3) ~ 2,
        TMNTHSBHND %in% c(4:8) ~ 3,
        TMNTHSBHND %in% c(-88:-99) ~ NA_real_,
      ),
      levels = c(1, 2, 3),
      labels = c("0-1", "2-3", "4+")
    ),
    monthsbehind_recode2 = factor(
      case_when(
        RENTCUR == 1 | MORTCUR == 1 ~ 0,
        TMNTHSBHND %in% c(0:1) ~ 1,
        TMNTHSBHND %in% c(2:3) ~ 2,
        TMNTHSBHND %in% c(4:8) ~ 3,
        TMNTHSBHND == -99 ~ -99,
        TMNTHSBHND %in% c(-88) ~ NA_real_
      ),
      levels = c(0, 1, 2, 3, -99),
      labels = c("Current", "0-1", "2-3", "4+", "No Response")
    ),
    rentassist_recode = factor(
      case_when(
        RENTASSIST %in% c(1, 2, 3, 4, -99) ~ RENTASSIST,
        RENTASSIST %in% c(-88) ~ NA_real_
      ),
      levels = c(1, 2, 3, 4, -99),
      labels = c(
        "My household applied and received assistance",
        "My household applied and is waiting for a response",
        "My household applied and the application was denied",
        "My household did not apply",
        "No Response"
      )
    ),
    dv_rentassist = case_when(
      TENURE %in% c(1, 2, 4) ~ 0,
      RENTASSIST %in% c(1:3) ~ 1,
      RENTASSIST == 4 ~ 0,
      RENTASSIST %in% c(-88, -99) ~ NA_real_
    ),
    insecurity_index = dv_difficulty2 + dv_current + dv_rentassist + dv_evictforeclose,
    insecurity_index2 = factor(
      case_when(
        insecurity_index == 0 ~ 0,
        insecurity_index %in% 1:4 ~ 1
      ),
      levels = c(0, 1),
      labels = c("0", "1-4")
    ),
    seeing_recode = factor(
      case_when(
        SEEING == -88 ~ NA_real_,
        SEEING %in% c(-99, 1, 2, 3, 4) ~ SEEING
      ),
      levels = c(1, 2, 3, 4, -99),
      labels = c(
        "No - no difficulty",
        "Yes - some difficulty",
        "Yes - a lot of difficulty",
        "Cannot do at all",
        "Did not report"
      )
    ),
    hearing_recode = factor(
      case_when(
        HEARING == -88 ~ NA_real_,
        HEARING %in% c(-99, 1, 2, 3, 4) ~ HEARING
      ),
      levels = c(1, 2, 3, 4, -99),
      labels = c(
        "No - no difficulty",
        "Yes - some difficulty",
        "Yes - a lot of difficulty",
        "Cannot do at all",
        "Did not report"
      )
    ),
    remembering_recode = factor(
      case_when(
        REMEMBERING == -88 ~ NA_real_,
        REMEMBERING %in% c(-99, 1, 2, 3, 4) ~ REMEMBERING
      ),
      levels = c(1, 2, 3, 4, -99),
      labels = c(
        "No - no difficulty",
        "Yes - some difficulty",
        "Yes - a lot of difficulty",
        "Cannot do at all",
        "Did not report"
      )
    ),
    mobility_recode = factor(
      case_when(
        MOBILITY == -88 ~ NA_real_,
        MOBILITY %in% c(-99, 1, 2, 3, 4) ~ MOBILITY
      ),
      levels = c(1, 2, 3, 4, -99),
      labels = c(
        "No - no difficulty",
        "Yes - some difficulty",
        "Yes - a lot of difficulty",
        "Cannot do at all",
        "Did not report"
      )
    ),
    selfcare_recode = factor(
      case_when(
        SELFCARE == -88 ~ NA_real_,
        SELFCARE %in% c(-99, 1, 2, 3, 4) ~ SELFCARE
      ),
      levels = c(1, 2, 3, 4, -99),
      labels = c(
        "No - no difficulty",
        "Yes - some difficulty",
        "Yes - a lot of difficulty",
        "Cannot do at all",
        "Did not report"
      )
    ),
    understand_recode = factor(
      case_when(
        UNDERSTAND == -88 ~ NA_real_,
        UNDERSTAND %in% c(-99, 1, 2, 3, 4) ~ UNDERSTAND
      ),
      levels = c(1, 2, 3, 4, -99),
      labels = c(
        "No - no difficulty",
        "Yes - some difficulty",
        "Yes - a lot of difficulty",
        "Cannot do at all",
        "Did not report"
      )
    ),
    seeing_recode2 = factor(
      case_when(
        SEEING == -88 ~ NA_real_,
        SEEING == 1 ~ 0,
        SEEING == 2 ~ 1,
        SEEING %in% c(3, 4) ~ 2,
        SEEING == -99 ~ -99,
      ),
      levels = c(0, 1, 2, -99),
      labels = c(
        "No difficulty",
        "Moderate difficulty",
        "Severe difficulty",
        "Did not report"
      )
    ),
    hearing_recode2 = factor(
      case_when(
        HEARING == -88 ~ NA_real_,
        HEARING == 1 ~ 0,
        HEARING == 2 ~ 1,
        HEARING %in% c(3, 4) ~ 2,
        HEARING == -99 ~ -99,
      ),
      levels = c(0, 1, 2, -99),
      labels = c(
        "No difficulty",
        "Moderate difficulty",
        "Severe difficulty",
        "Did not report"
      )
    ),
    remembering_recode2 = factor(
      case_when(
        REMEMBERING == -88 ~ NA_real_,
        REMEMBERING == 1 ~ 0,
        REMEMBERING == 2 ~ 1,
        REMEMBERING %in% c(3, 4) ~ 2,
        REMEMBERING == -99 ~ -99,
      ),
      levels = c(0, 1, 2, -99),
      labels = c(
        "No difficulty",
        "Moderate difficulty",
        "Severe difficulty",
        "Did not report"
      )
    ),
    mobility_recode2 = factor(
      case_when(
        MOBILITY == -88 ~ NA_real_,
        MOBILITY == 1 ~ 0,
        MOBILITY == 2 ~ 1,
        MOBILITY %in% c(3, 4) ~ 2,
        MOBILITY == -99 ~ -99,
      ),
      levels = c(0, 1, 2, -99),
      labels = c(
        "No difficulty",
        "Moderate difficulty",
        "Severe difficulty",
        "Did not report"
      )
    ),
    selfcare_recode2 = factor(
      case_when(
        SELFCARE == -88 ~ NA_real_,
        SELFCARE == 1 ~ 0,
        SELFCARE == 2 ~ 1,
        SELFCARE %in% c(3, 4) ~ 2,
        SELFCARE == -99 ~ -99,
      ),
      levels = c(0, 1, 2, -99),
      labels = c(
        "No difficulty",
        "Moderate difficulty",
        "Severe difficulty",
        "Did not report"
      )
    ),
    understand_recode2 = factor(
      case_when(
        UNDERSTAND == -88 ~ NA_real_,
        UNDERSTAND == 1 ~ 0,
        UNDERSTAND == 2 ~ 1,
        UNDERSTAND %in% c(3, 4) ~ 2,
        UNDERSTAND == -99 ~ -99,
      ),
      levels = c(0, 1, 2, -99),
      labels = c(
        "No difficulty",
        "Moderate difficulty",
        "Severe difficulty",
        "Did not report"
      )
    ),
    
    
    inclusion = case_when(
        HADCOVIDRV == 1 &
  WHENCOVID %in% c(2,3) &
  LONGCOVID %in% c(1,2) &
  (LONGCOVID == 2 | (LONGCOVID == 1 & SYMPTMNOW %in% c(1,2)) ) &      
  TENURE %in% c(1,2,3,4) &
  is.na(dv_difficulty) == FALSE &
  is.na(dv_current) == FALSE &
  is.na(dv_evictforeclose) == FALSE & 
  INCOME != -88 ~ 1,
  TRUE ~ 0
      ),
  
  study = factor(case_when(inclusion == 1 ~ 1,
                           inclusion == 0 & HADCOVIDRV == 1 & WHENCOVID %in% c(2,3) ~ 0,
                           TRUE ~ NA_real_),
                 levels = c(0,1),
                 labels = c("Excluded", "Included")
                 )


  ) %>%
 rename_all(tolower)


# write.csv(hps,file="C:/Users/subli/Dropbox/Class/2023 Spring/Exec/hps_housing.csv")

# Missing Data

# Missing COVID-19 history
length(hps$scram[hps$hadcovidrv == -88 | (hps$hadcovidrv == 1 & hps$whencovid == -88)])
length(hps$scram[hps$hadcovidrv == 1 & hps$whencovid == 1])

# Missing Long COVID
length(hps$scram[hps$hadcovidrv == 1 & 
                 hps$whencovid %in% c(2,3) & 
                 hps$longcovid == -88])

# Missing Housing Tenure
length(hps$scram[hps$hadcovidrv == 1 & 
                 hps$whencovid %in% c(2,3) & 
                 hps$longcovid %in% c(1,2) &
                 hps$tenure == -88])

# Missing Housing Insecurity Measures
length(hps$scram[hps$hadcovidrv == 1 &                  # Had COVID
                 hps$whencovid %in% c(2,3) &            # More than 1 month ago
                 hps$longcovid %in% c(1,2) &            # NOT missing LC
                 hps$tenure %in% c(1,2,3,4) &           # NOT missing tenure
                 (is.na(hps$dv_difficulty) == TRUE |
                 is.na(hps$dv_current) == TRUE |
                 is.na(hps$dv_evictforeclose) == TRUE)  # YES Missing HI indicators
                 ])


# Missing Functional Impairment
length(hps$scram[hps$hadcovidrv == 1 &                      # Had COVID
                 hps$whencovid %in% c(2,3) &                # More than 1 month ago
                 hps$longcovid %in% c(1,2) &                # Not missing LC
                 hps$tenure %in% c(1,2,3,4) &               # Not missing tenure
                 (is.na(hps$dv_difficulty) == FALSE &
                 is.na(hps$dv_current) == FALSE &
                 is.na(hps$dv_evictforeclose) == FALSE) &   # NOT Missing HI indicators
                          
                     (hps$remembering == -88 |
                     hps$understand == -88 |
                     hps$mobility == -88 |
                     hps$selfcare == -88 |
                     hps$seeing == -88 |
                     hps$hearing == -88)                 # Yes missing FI 
                     
                 ])

# Missing Income
length(hps$scram[hps$hadcovidrv == 1 &                  # Had COVID
                 hps$whencovid %in% c(2,3) &            # More than 1 month ago
                 hps$longcovid %in% c(1,2) &            # Not missing LC
                 hps$tenure %in% c(1,2,3,4) &           # Not missing tenure
                 (is.na(hps$dv_difficulty) == FALSE &
                  is.na(hps$dv_current) == FALSE &
                  is.na(hps$dv_evictforeclose) == FALSE) &   # NOT Missing HI indicators

                     (hps$income == -88)                 # Yes missing income
                     
                 ])

# Missing covariates
length(hps$scram[hps$hadcovidrv == 1 & 
                 hps$whencovid %in% c(2,3) & 
                 hps$longcovid %in% c(1,2) &
                 hps$tenure %in% c(1,2,3,4) & 
                    (hps$tenure  &
                    (hps$expns_dif==-88 | 
                    (hps$tenure == 3 & hps$rentcur == -88) | 
                    (hps$tenure == 2 & hps$mortcur == -88) |
                    (hps$rentcur == 2 & hps$evict == -88) | 
                    (hps$mortcur == 2 & hps$forclose == -88))
                     ) &
                 hps$income == - 88
                     ])

### Create Survey object from combined dataframe


hps_include <- subset(hps, 
  hadcovidrv == 1 &
  whencovid %in% c(2,3) &
  longcovid %in% c(1,2) &
  (longcovid == 2 | (longcovid == 1 & symptmnow %in% c(1,2)) ) &      
  tenure %in% c(1,2,3,4) &
  is.na(dv_difficulty) == FALSE &
  is.na(dv_current) == FALSE &
  is.na(dv_evictforeclose) == FALSE & 
  income != -88)

hps_svy <- svydesign(id = ~scram, weights = ~pweight8, data = hps)

hps_svy_include <- svydesign(id = ~scram, weights = ~pweight8, data = hps_include)




