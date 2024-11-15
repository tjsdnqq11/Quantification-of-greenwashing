################################################################################
##### 라이브러리 & 전처리 알고리즘 세팅
################################################################################
library(dplyr)
library(tidyr)
library(readxl)
library(MASS)
library(factoextra)
library(corrplot)
library(car)
library(MLmetrics)
library(caret)
library(randomForest)
library(nnet)
library(openxlsx)

setwd("/Users/seonukim/Documents/fdacenter/02_14")
set.seed(2023)
options(scipen = 100000)

standard_scale <- function(data) {
  numeric_columns <- sapply(data, is.numeric)
  data_numeric <- data[, numeric_columns]
  data_scaled <- as.data.frame(scale(data_numeric))
  data[, numeric_columns] <- data_scaled
  return(data)
}

Level_Processing <- function(var){
  var <- if_else(var == "A+", 100,
                 if_else(var == "A", 87.5,
                         if_else(var == "B+", 75,
                                 if_else(var == "B", 62.5,
                                         if_else(var == "C+", 50,
                                                 if_else(var == "C", 37.5,
                                                         if_else(var == "D+", 25, 12.5)))))))
  return(var)                                                       
}

Green_Washing <- function(df){
  E <- df$Grade_Environment
  S <- df$Grade_Society
  G <- df$Grade_Governance
  Total <- df$Total_grade
  E <- Level_Processing(E)
  S <- Level_Processing(S)
  G <- Level_Processing(G)
  Total <- Level_Processing(Total)
  
  CSR_E <- df$CSR_Environment
  CSR_S <- df$CSR_Social
  CSR_G <- df$CSR_Governance
  
  dis_total = (CSR_E*E)+(CSR_S*S)+(CSR_G*G)
  
  GW_index = dis_total - Total
  return(GW_index)
}

normal <- function(x) {
  (x - mean(x)) / sd(x)
}

summarize_columns <- function(df) {
  summary_df <- do.call(rbind, lapply(df, function(x) {
    c(min = min(x, na.rm = TRUE),
      q1 = quantile(x, 0.25, na.rm = TRUE),
      median = median(x, na.rm = TRUE),
      mean = mean(x, na.rm = TRUE),
      q3 = quantile(x, 0.75, na.rm = TRUE),
      max = max(x, na.rm = TRUE),
      sd = sd(x, na.rm = TRUE))
  }))
  # 결과를 데이터 프레임으로 변환하고 열 이름을 추가합니다.
  summary_df <- as.data.frame(t(summary_df))
  return(summary_df)
}
################################################################################
##### 데이터 셋 로드 및 병합 + 그린워싱 변수 생성
################################################################################
install.packages("read_excel")

ESG <- read_xlsx("ESG_rating_remain_modify.xlsx")
ESG_SCORE <- read.csv("ESG_SCORE.csv")[,-c(1,6)]
ESG_BERTS_SCORE <- read.csv("esg_bert_result.csv")[,-1]
FN <- read.csv("financial_data.csv")[,-c(1,2)]

names(ESG) <- c("company_name", "Total_grade", "Grade_Environment", "Grade_Society", "Grade_Governance")
names(ESG_SCORE) <- c("company_name", "CSR_Social", "CSR_Governance", "CSR_Environment")
names(ESG_BERTS_SCORE)
names(FN) <- c("company_name", "ROA", "ROE", "Tobins.q", "OCF", "Leverage", "Firm_Size")


ESG_BERTS_SCORE$company_name <- gsub(".json", "", ESG_BERTS_SCORE$CSR)
ESG_BERTS_SCORE <- ESG_BERTS_SCORE[,-27]
ESG_BERTS_SCORE[is.na(ESG_BERTS_SCORE)] <- 0

df <- left_join(ESG, ESG_BERTS_SCORE, by = "company_name")
df <- left_join(df, ESG_SCORE, by = "company_name")
df <- left_join(df, FN, by = "company_name")
df <- na.omit(df)

co2_df <- read.csv("co2_energy_release.csv")[,-c(1, 6, 8, 9)]
names(co2_df) <- c("company_name", "year", "industry_category", "co2", "energy")
co2_df$company_name <- gsub("\\(주\\)*", '', co2_df$company_name)
co2_df$company_name <- gsub("주식회사*", '', co2_df$company_name)
co2_df$company_name <- gsub(" ", '', co2_df$company_name)
co2_df$company_name <- gsub("\\(유\\)*", '', co2_df$company_name)
co2_df$company_name <- gsub("디비", "DB", co2_df$company_name)
co2_df$company_name <- gsub("엘지", "LG", co2_df$company_name)
co2_df$company_name <- gsub("에스케이", "SK", co2_df$company_name)
co2_df$company_name <- gsub("동원F&B", "동원F&B", co2_df$company_name)
co2_df$company_name <- gsub("에스디아이", "SDI", co2_df$company_name)

df %>% 
  left_join(co2_df, by = "company_name") %>% 
  filter(year == 2019) %>% 
  filter(company_name == "포스코인터내셔널") -> mini_df

df %>% 
  left_join(co2_df, by = "company_name") %>% 
  filter(!is.na(co2) & year == 2020) -> df

df <- rbind(df, mini_df)
df[,-43] -> df

df$Green_Washing <- Green_Washing(df)

df$Total_grade <- as.factor(df$Total_grade)
df$Grade_Environment <- as.factor(df$Grade_Environment)
df$Grade_Society <- as.factor(df$Grade_Society)
df$Grade_Governance <- as.factor(df$Grade_Governance)

df$Total_grade <- factor(df$Total_grade, levels = c("A+", "A", "B+", "B", "C+", "C", "D+", "D"))
df$Grade_Environment <- factor(df$Total_grade, levels = c("A+", "A", "B+", "B", "C+", "C", "D+", "D"))
df$Grade_Society <- factor(df$Grade_Society, levels = c("A+", "A", "B+", "B", "C+", "C", "D+", "D"))
df$Grade_Governance <- factor(df$Grade_Governance, levels = c("A+", "A", "B+", "B", "C+", "C", "D+", "D"))
df[,-c(32:34, 2:5)] -> df

################################################################################
##### 독립1 : greenwashing ~ 버트 파생변수
################################################################################
ESG <- read_excel("ESG_rating_remain_modify.xlsx")
ESG_SCORE <- read.csv("ESG_SCORE.csv")[,-c(1,6)]
ESG_BERTS_SCORE <- read.csv("esg_bert_result.csv")[,-1]

names(ESG) <- c("company_name", "Total_grade", "Grade_Environment", "Grade_Society", "Grade_Governance")
names(ESG_SCORE) <- c("company_name", "CSR_Social", "CSR_Governance", "CSR_Environment")
names(ESG_BERTS_SCORE)

ESG_BERTS_SCORE$company_name <- gsub(".json", "", ESG_BERTS_SCORE$CSR)
ESG_BERTS_SCORE <- ESG_BERTS_SCORE[,-27]
ESG_BERTS_SCORE[is.na(ESG_BERTS_SCORE)] <- 0

df_1 <- left_join(ESG, ESG_BERTS_SCORE, by = "company_name")
df_1 <- left_join(df_1, ESG_SCORE, by = "company_name")

df_1$Green_Washing <- Green_Washing(df_1)
df_1$Total_grade <- as.factor(df_1$Total_grade)
df_1$Grade_Environment <- as.factor(df_1$Grade_Environment)
df_1$Grade_Society <- as.factor(df_1$Grade_Society)
df_1$Grade_Governance <- as.factor(df_1$Grade_Governance)

df_1$Total_grade <- factor(df_1$Total_grade, levels = c("A+", "A", "B+", "B", "C+", "C", "D+", "D"))
df_1$Grade_Environment <- factor(df_1$Total_grade, levels = c("A+", "A", "B+", "B", "C+", "C", "D+", "D"))
df_1$Grade_Society <- factor(df_1$Grade_Society, levels = c("A+", "A", "B+", "B", "C+", "C", "D+", "D"))
df_1$Grade_Governance <- factor(df_1$Grade_Governance, levels = c("A+", "A", "B+", "B", "C+", "C", "D+", "D"))
df_1 <- drop_na(df_1)

##### 전처리 이후 데이터 셋 142개
df_1company_name <- df_1$company_name
df_1 <- df_1[,-c(1:5)]

##### discriptive statitic
df_1_summary_stats_df <- t(summarize_columns(df_1))
df_1_mean_values <- sapply(df_1, mean, na.rm = TRUE)
df_1_sd_values <- sapply(df_1, sd, na.rm = TRUE)
df_1_stats_df <- data.frame(Mean = df_1_mean_values, sd_values = df_1_sd_values)
df_1_summary_stats_df <- cbind(df_1_summary_stats_df, df_1_stats_df)
df_1_summary_stats_df

df_1_summary_stats_df$IndexValue = row.names(df_1_summary_stats_df)

write.xlsx(df_1_summary_stats_df, "descriptive1.xlsx")


##### stepwise
model <- lm(Green_Washing ~ ., data = df_1[,-c(27:29)])
df_1.lm.step <- step(model, direction = "both")
summary(df_1.lm.step) 
df_1_model <- lm(formula = Green_Washing ~ df_1[,-c(27:29)], data = df_1[,-c(27:29)])
summary(df_1_model)

# 원하는 컬럼만 선택
df_1_selected <- df_1[, c("Green_Washing", "Director_Removal", "Competitive_Behavior", "Customer_Welfare")]


# Coefficients:
#   Estimate Std. Error t value  Pr(>|t|)    
# (Intercept)            12.733      2.922   4.358 0.0000254 ***
#   Director_Removal      -10.264      6.175  -1.662    0.0987 .  
# Competitive_Behavior   -8.251      5.349  -1.542    0.1253    
# Customer_Welfare       -6.439      3.390  -1.899    0.0596 .  
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4.855 on 138 degrees of freedom
# Multiple R-squared:  0.1263,	Adjusted R-squared:  0.1073 
# F-statistic: 6.648 on 3 and 138 DF,  p-value: 0.0003163

################################################################################
##### 독립2-1 : greenwashing ~ 버트 파생 + 에미션 변수
################################################################################
ESG <- read_excel("ESG_rating_remain_modify.xlsx")
ESG_SCORE <- read.csv("ESG_SCORE.csv")[,-c(1,6)]
ESG_BERTS_SCORE <- read.csv("esg_bert_result.csv")[,-1]
co2_df_2_1 <- read.csv("co2_energy_release.csv")[,-c(1, 6, 8, 9)]

names(ESG) <- c("company_name", "Total_grade", "Grade_Environment", "Grade_Society", "Grade_Governance")
names(ESG_SCORE) <- c("company_name", "CSR_Social", "CSR_Governance", "CSR_Environment")
names(ESG_BERTS_SCORE)
names(co2_df_2_1) <- c("company_name", "year", "industry_category", "co2", "energy")
co2_df_2_1$company_name <- gsub("\\(주\\)*", '', co2_df_2_1$company_name)
co2_df_2_1$company_name <- gsub("주식회사*", '', co2_df_2_1$company_name)
co2_df_2_1$company_name <- gsub(" ", '', co2_df_2_1$company_name)
co2_df_2_1$company_name <- gsub("\\(유\\)*", '', co2_df_2_1$company_name)
co2_df_2_1$company_name <- gsub("디비", "DB", co2_df_2_1$company_name)
co2_df_2_1$company_name <- gsub("엘지", "LG", co2_df_2_1$company_name)
co2_df_2_1$company_name <- gsub("에스케이", "SK", co2_df_2_1$company_name)
co2_df_2_1$company_name <- gsub("동원F&B", "동원F&B", co2_df_2_1$company_name)
co2_df_2_1$company_name <- gsub("에스디아이", "SDI", co2_df_2_1$company_name)
ESG_BERTS_SCORE$company_name <- gsub(".json", "", ESG_BERTS_SCORE$CSR)
ESG_BERTS_SCORE <- ESG_BERTS_SCORE[,-27]
ESG_BERTS_SCORE[is.na(ESG_BERTS_SCORE)] <- 0

df_2_1 <- join(ESG, ESG_BERTS_SCORE, by = "company_name")
df_2_1 <- left_join(df_2_1, ESG_SCORE, by = "company_name")

df_2_1 %>% 
  left_join(co2_df_2_1, by = "company_name") %>% 
  filter(year == 2019) %>% 
  filter(company_name == "포스코인터내셔널") -> mini_df_2_1


df_2_1 %>% 
  left_join(co2_df_2_1, by = "company_name") %>% 
  filter(!is.na(co2) & year == 2020) -> df_2_1

df_2_1 <- rbind(df_2_1, mini_df_2_1)
df_2_1[,-35] -> df_2_1

df_2_1$Green_Washing <- Green_Washing(df_2_1)
df_2_1$Total_grade <- as.factor(df_2_1$Total_grade)
df_2_1$Grade_Environment <- as.factor(df_2_1$Grade_Environment)
df_2_1$Grade_Society <- as.factor(df_2_1$Grade_Society)
df_2_1$Grade_Governance <- as.factor(df_2_1$Grade_Governance)

df_2_1$Total_grade <- factor(df_2_1$Total_grade, levels = c("A+", "A", "B+", "B", "C+", "C", "D+", "D"))
df_2_1$Grade_Environment <- factor(df_2_1$Total_grade, levels = c("A+", "A", "B+", "B", "C+", "C", "D+", "D"))
df_2_1$Grade_Society <- factor(df_2_1$Grade_Society, levels = c("A+", "A", "B+", "B", "C+", "C", "D+", "D"))
df_2_1$Grade_Governance <- factor(df_2_1$Grade_Governance, levels = c("A+", "A", "B+", "B", "C+", "C", "D+", "D"))
df_2_1 <- drop_na(df_2_1)

##### 전처리 이후 데이터 셋 59개
df_2_1company_name <- df_2_1$company_name
df_2_1 <- df_2_1[,-c(1:5, 32:34)]

##### discriptive statitic
df_2_1_summary_stats_df <- t(summarize_columns(df_2_1[,-27]))
df_2_1_mean_values <- sapply(df_2_1[,-27], mean, na.rm = TRUE)
df_2_1_sd_values <- sapply(df_2_1[,-27], sd, na.rm = TRUE)
df_2_1_stats_df <- data.frame(Mean = df_2_1_mean_values, sd_values = df_2_1_sd_values)
df_2_1_summary_stats_df <- cbind(df_2_1_summary_stats_df, df_2_1_stats_df)
df_2_1_summary_stats_df

df_2_1_summary_stats_df$IndexValue = row.names(df_2_1_summary_stats_df)

write.xlsx(df_2_1_summary_stats_df, "descriptive4.xlsx")

##### stepwise

# 특이사항 : industry_category가 stepwise에서 적용되지 않음
#            따라서 industry_category가 변수를 제외한 후 stepwise에서
#            stepwise에서 산출된 모델에 industry_category를
#            추가한 모델과 추가하지 않은 모델 비교

model <- lm(Green_Washing ~ . , data = df_2_1[,-27])
df_2_1.lm.step <- step(model, direction = "both")
summary(df_2_1.lm.step) 
df_2_1_model <- lm(formula = Green_Washing ~ Supply_Chain_Management + Systemic_Risk_Management + 
                     Customer_Welfare + co2, data = df_2_1)
summary(df_2_1_model)

df_2_selected <- df_2_1[, c("Green_Washing", "Supply_Chain_Management", "Systemic_Risk_Management", "Customer_Welfare", "co2")]

#####  industry_category 제외 모델
df_2_1_model <- lm(formula = Green_Washing ~ Supply_Chain_Management + Systemic_Risk_Management + 
                   Customer_Welfare + co2, data = df_2_1)
summary(df_2_1_model)

# Coefficients:
#   Estimate     Std. Error t value  Pr(>|t|)    
# (Intercept)               10.5246752545   3.2334541267   3.255   0.00196 ** 
#   Supply_Chain_Management  -25.1904485341   5.3737311933  -4.688 0.0000192 ***
#   Systemic_Risk_Management   9.6114393955   6.2318369052   1.542   0.12884    
# Customer_Welfare           6.5787765293   4.8071615096   1.369   0.17681    
# co2                        0.0000003894   0.0000002441   1.595   0.11655    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4.479 on 54 degrees of freedom
# Multiple R-squared:  0.3193,	Adjusted R-squared:  0.2689 
# F-statistic: 6.334 on 4 and 54 DF,  p-value: 0.0002968


#####  industry_category 포함 모델
df_2_1_model <- lm(formula = Green_Washing ~ Supply_Chain_Management + Systemic_Risk_Management + 
                     Customer_Welfare + co2 + industry_category, data = df_2_1)
summary(df_2_1_model)

# Coefficients:
#   Estimate     Std. Error t value Pr(>|t|)    
# (Intercept)                     10.4695781223   4.1116283019   2.546 0.014462 *  
#   Supply_Chain_Management        -25.0390993590   5.8678641559  -4.267 0.000104 ***
#   Systemic_Risk_Management         8.4135235116   7.2746966041   1.157 0.253699    
# Customer_Welfare                 8.3704930001   5.7582107129   1.454 0.153136    
# co2                              0.0000004193   0.0000002685   1.561 0.125628    
# industry_category건설           -2.0169803152   4.0460766659  -0.499 0.620613    
# industry_category교통(화물)      3.5378893281   4.2028847653   0.842 0.404465    
# industry_category산업           -0.0854680491   2.5575588849  -0.033 0.973493    
# industry_category석유화학        1.5917078879   5.2925818782   0.301 0.765027    
# industry_category수송            1.3336942646   3.6890560495   0.362 0.719436    
# industry_category식료품 제조업  -3.9248585529   4.1079252442  -0.955 0.344578    
# industry_category전기전자       -0.5960650502   5.2538409827  -0.113 0.910187    
# industry_category전환            1.4546164537   5.6094916695   0.259 0.796603    
# industry_category철강            2.3012854313   5.3442426236   0.431 0.668854    
# industry_category통신            6.7141779949   5.6370992839   1.191 0.240012    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 4.67 on 44 degrees of freedom
# Multiple R-squared:  0.397,	Adjusted R-squared:  0.2052 
# F-statistic: 2.069 on 14 and 44 DF,  p-value: 0.03373

################################################################################
##### 독립3-1 : greenwashing ~ 버트 파생 + 재무변수 + 에미션 변수
################################################################################
ESG <- read_excel("ESG_rating_remain_modify.xlsx")
ESG_SCORE <- read.csv("ESG_SCORE.csv")[,-c(1,6)]
ESG_BERTS_SCORE <- read.csv("esg_bert_result.csv")[,-1]
FN <- read.csv("financial_data.csv")[,-c(1,2)]
co2_df_3_1 <- read.csv("co2_energy_release.csv")[,-c(1, 6, 8, 9)]

names(ESG) <- c("company_name", "Total_grade", "Grade_Environment", "Grade_Society", "Grade_Governance")
names(ESG_SCORE) <- c("company_name", "CSR_Social", "CSR_Governance", "CSR_Environment")
names(ESG_BERTS_SCORE)
names(FN) <- c("company_name", "ROA", "ROE", "Tobins.q", "OCF", "Leverage", "Firm_Size")
names(co2_df_3_1) <- c("company_name", "year", "industry_category", "co2", "energy")
co2_df_3_1$company_name <- gsub("\\(주\\)*", '', co2_df_3_1$company_name)
co2_df_3_1$company_name <- gsub("주식회사*", '', co2_df_3_1$company_name)
co2_df_3_1$company_name <- gsub(" ", '', co2_df_3_1$company_name)
co2_df_3_1$company_name <- gsub("\\(유\\)*", '', co2_df_3_1$company_name)
co2_df_3_1$company_name <- gsub("디비", "DB", co2_df_3_1$company_name)
co2_df_3_1$company_name <- gsub("엘지", "LG", co2_df_3_1$company_name)
co2_df_3_1$company_name <- gsub("에스케이", "SK", co2_df_3_1$company_name)
co2_df_3_1$company_name <- gsub("동원F&B", "동원F&B", co2_df_3_1$company_name)
co2_df_3_1$company_name <- gsub("에스디아이", "SDI", co2_df_3_1$company_name)

ESG_BERTS_SCORE$company_name <- gsub(".json", "", ESG_BERTS_SCORE$CSR)
ESG_BERTS_SCORE <- ESG_BERTS_SCORE[,-27]
ESG_BERTS_SCORE[is.na(ESG_BERTS_SCORE)] <- 0

df_3_1 <- left_join(ESG, ESG_BERTS_SCORE, by = "company_name")
df_3_1 <- left_join(df_3_1, ESG_SCORE, by = "company_name")
df_3_1 <- left_join(df_3_1, FN, by = "company_name")

df_3_1 %>% 
  left_join(co2_df_3_1, by = "company_name") %>% 
  filter(year == 2019) %>% 
  filter(company_name == "포스코인터내셔널") -> mini_df_3_1

df_3_1 %>% 
  left_join(co2_df_3_1, by = "company_name") %>% 
  filter(!is.na(co2) & year == 2020) -> df_3_1

df_3_1 <- rbind(df_3_1, mini_df_3_1)
df_3_1[,-41] -> df_3_1

df_3_1$Green_Washing <- Green_Washing(df_3_1)
df_3_1$Total_grade <- as.factor(df_3_1$Total_grade)
df_3_1$Grade_Environment <- as.factor(df_3_1$Grade_Environment)
df_3_1$Grade_Society <- as.factor(df_3_1$Grade_Society)
df_3_1$Grade_Governance <- as.factor(df_3_1$Grade_Governance)

df_3_1$Total_grade <- factor(df_3_1$Total_grade, levels = c("A+", "A", "B+", "B", "C+", "C", "D+", "D"))
df_3_1$Grade_Environment <- factor(df_3_1$Total_grade, levels = c("A+", "A", "B+", "B", "C+", "C", "D+", "D"))
df_3_1$Grade_Society <- factor(df_3_1$Grade_Society, levels = c("A+", "A", "B+", "B", "C+", "C", "D+", "D"))
df_3_1$Grade_Governance <- factor(df_3_1$Grade_Governance, levels = c("A+", "A", "B+", "B", "C+", "C", "D+", "D"))
df_3_1 <- drop_na(df_3_1)

##### 전처리 이후 데이터 셋 47개
df_3_1company_name <- df_3_1$company_name
df_3_1 <- df_3_1[,-c(1:5, 32:34, 41)]

##### discriptive statitic
df_3_1_summary_stats_df <- t(summarize_columns(df_3_1[,-33]))
df_3_1_mean_values <- sapply(df_3_1[,-33], mean, na.rm = TRUE)
df_3_1_sd_values <- sapply(df_3_1[,-33], sd, na.rm = TRUE)
df_3_1_stats_df <- data.frame(Mean = df_3_1_mean_values, sd_values = df_3_1_sd_values)
df_3_1_summary_stats_df <- cbind(df_3_1_summary_stats_df, df_3_1_stats_df)
df_3_1_summary_stats_df

df_3_1_summary_stats_df$IndexValue = row.names(df_3_1_summary_stats_df)

write.xlsx(df_3_1_summary_stats_df, "descriptive3.xlsx")

##### stepwise

model <- lm(Green_Washing ~ ., data = df_3_1)
df_3_1.lm.step <- step(model, direction = 'both')
summary(df_3_1.lm.step) 

#####  industry_category 제외 모델
df_3_1_model <- lm(formula = Green_Washing ~ Waste_And_Hazardous_Materials_Management + 
                     GHG_Emissions + Critical_Incident_Risk_Management + Physical_Impacts_Of_Climate_Change + 
                     Labor_Practices + Employee_Health_And_Safety + Product_Quality_And_Safety + 
                     Ecological_Impacts + Product_Design_And_Lifecycle_Management + 
                     Employee_Engagement_Inclusion_And_Diversity + Director_Removal + 
                     Management_Of_Legal_And_Regulatory_Framework + Competitive_Behavior + 
                     Customer_Welfare + ROA + ROE + Tobins.q + OCF + Leverage + 
                     Firm_Size + energy, data = df_3_1[,-33])
summary(df_3_1_model)

# 원하는 컬럼만 선택하여 새로운 데이터 프레임 생성
df_3_selected <- df_3_1[, c("Green_Washing", "Waste_And_Hazardous_Materials_Management", "GHG_Emissions", "Critical_Incident_Risk_Management", "Physical_Impacts_Of_Climate_Change", "Labor_Practices", "Employee_Health_And_Safety", "Product_Quality_And_Safety", "Ecological_Impacts", "Product_Design_And_Lifecycle_Management", "Employee_Engagement_Inclusion_And_Diversity", "Director_Removal", "Management_Of_Legal_And_Regulatory_Framework", "Competitive_Behavior", "Customer_Welfare", "ROA", "ROE", "Tobins.q", "OCF", "Leverage", "Firm_Size", "energy")]


# Coefficients:
#   Estimate         Std. Error
# (Intercept)                                    8.94781698834913  11.49312209728558
# Waste_And_Hazardous_Materials_Management      13.08635518421305   6.63576441203955
# GHG_Emissions                                 11.49281079773080   8.67985524529899
# Critical_Incident_Risk_Management             10.10891851457491   8.70048223969540
# Physical_Impacts_Of_Climate_Change            -8.22982036529070   4.14634732768067
# Labor_Practices                              -13.34205456510051   5.67616650035396
# Employee_Health_And_Safety                    23.12347915450504  13.25543745900641
# Product_Quality_And_Safety                     9.03676097144209   4.83954278717602
# Ecological_Impacts                            -7.62317497516850   6.00807931808521
# Product_Design_And_Lifecycle_Management      -35.94414614257411   9.66860321164796
# Employee_Engagement_Inclusion_And_Diversity  -15.04012984632840   8.80937425158073
# Director_Removal                             -44.88955742693246  12.27169884684726
# Management_Of_Legal_And_Regulatory_Framework   8.37910054254906   7.20166510637309
# Competitive_Behavior                          26.41513729124484   9.30889226592205
# Customer_Welfare                              23.39364053846861   5.83087646853758
# ROA                                          -47.52071380518384  19.95631285937878
# ROE                                           55.34549521927566  24.20521664469864
# Tobins.q                                      -2.65563872558611   1.13515528283298
# OCF                                           -0.00000000064189   0.00000000026179
# Leverage                                      -5.70958341077523   3.21518636321411
# Firm_Size                                      0.00000000010609   0.00000000004086
# energy                                         0.00004435419341   0.00001870733434
# t value Pr(>|t|)    
# (Intercept)                                    0.779  0.44356    
# Waste_And_Hazardous_Materials_Management       1.972  0.05976 .  
# GHG_Emissions                                  1.324  0.19746    
# Critical_Incident_Risk_Management              1.162  0.25626    
# Physical_Impacts_Of_Climate_Change            -1.985  0.05824 .  
# Labor_Practices                               -2.351  0.02694 *  
#   Employee_Health_And_Safety                     1.744  0.09336 .  
# Product_Quality_And_Safety                     1.867  0.07363 .  
# Ecological_Impacts                            -1.269  0.21619    
# Product_Design_And_Lifecycle_Management       -3.718  0.00102 ** 
#   Employee_Engagement_Inclusion_And_Diversity   -1.707  0.10016    
# Director_Removal                              -3.658  0.00119 ** 
#   Management_Of_Legal_And_Regulatory_Framework   1.163  0.25561    
# Competitive_Behavior                           2.838  0.00889 ** 
#   Customer_Welfare                               4.012  0.00048 ***
#   ROA                                           -2.381  0.02519 *  
#   ROE                                            2.287  0.03096 *  
#   Tobins.q                                      -2.339  0.02760 *  
#   OCF                                           -2.452  0.02154 *  
#   Leverage                                      -1.776  0.08794 .  
# Firm_Size                                      2.596  0.01555 *  
#   energy                                         2.371  0.02576 *  
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 3.087 on 25 degrees of freedom
# Multiple R-squared:  0.7258,	Adjusted R-squared:  0.4954 
# F-statistic: 3.151 on 21 and 25 DF,  p-value: 0.003496


#####  industry_category 포함 모델
df_3_1_model <- lm(formula = Green_Washing ~ Waste_And_Hazardous_Materials_Management + 
                     GHG_Emissions + Critical_Incident_Risk_Management + Physical_Impacts_Of_Climate_Change + 
                     Labor_Practices + Employee_Health_And_Safety + Product_Quality_And_Safety + 
                     Ecological_Impacts + Product_Design_And_Lifecycle_Management + 
                     Employee_Engagement_Inclusion_And_Diversity + Director_Removal + 
                     Management_Of_Legal_And_Regulatory_Framework + Competitive_Behavior + 
                     Customer_Welfare + ROA + ROE + Tobins.q + OCF + Leverage + 
                     Firm_Size + energy + industry_category, data = df_3_1)
summary(df_3_1_model)

# Coefficients:
#   Estimate         Std. Error t value Pr(>|t|)   
# (Intercept)                                   13.93796352569833  14.23331887145932   0.979  0.34119   
# Waste_And_Hazardous_Materials_Management      23.81754132179348   8.03003769978194   2.966  0.00866 **
#   GHG_Emissions                                 15.37706339184989  11.51213354171386   1.336  0.19925   
# Critical_Incident_Risk_Management             11.41981623912442  11.12651976503120   1.026  0.31911   
# Physical_Impacts_Of_Climate_Change            -6.53336277809478   4.34167926293990  -1.505  0.15073   
# Labor_Practices                              -17.84544014329159   6.12681911879186  -2.913  0.00970 **
#   Employee_Health_And_Safety                    19.13041117113210  18.12904021366157   1.055  0.30608   
# Product_Quality_And_Safety                    15.70366020787047   5.55658693034451   2.826  0.01165 * 
#   Ecological_Impacts                           -13.30279478481770   8.44528974547864  -1.575  0.13364   
# Product_Design_And_Lifecycle_Management      -41.71682390909830  14.21235594548705  -2.935  0.00924 **
#   Employee_Engagement_Inclusion_And_Diversity  -15.96678687723780  10.13529688478063  -1.575  0.13360   
# Director_Removal                             -53.96505515893661  13.63233787972315  -3.959  0.00101 **
#   Management_Of_Legal_And_Regulatory_Framework   4.12278404296482   7.98471386590972   0.516  0.61227   
# Competitive_Behavior                          20.06803638708159  11.89976167179781   1.686  0.10998   
# Customer_Welfare                              20.96640948397032   7.43964294902937   2.818  0.01184 * 
#   ROA                                          -55.36558055189850  24.66741145324763  -2.244  0.03840 * 
#   ROE                                           64.32494544547023  29.88098944521878   2.153  0.04600 * 
#   Tobins.q                                      -2.83480802118013   1.16120466374627  -2.441  0.02587 * 
#   OCF                                           -0.00000000066514   0.00000000032620  -2.039  0.05730 . 
# Leverage                                      -6.66762879080959   3.56808162358070  -1.869  0.07900 . 
# Firm_Size                                      0.00000000011083   0.00000000005187   2.137  0.04745 * 
#   energy                                         0.00004037966811   0.00001902404042   2.123  0.04878 * 
#   industry_category건설                          0.39832330812562   4.23367759048190   0.094  0.92614   
# industry_category교통(화물)                    8.51811839459932   4.83693612841130   1.761  0.09620 . 
# industry_category산업                          4.26137783706792   2.68362057775457   1.588  0.13073   
# industry_category수송                          3.16007908550588   3.82836079228338   0.825  0.42056   
# industry_category식료품 제조업                -1.33975628750274   3.47220245169944  -0.386  0.70439   
# industry_category전기전자                      1.93477951189262   4.88434738300547   0.396  0.69695   
# industry_category전환                          3.68825082727203   5.91403989536768   0.624  0.54114   
# industry_category철강                          5.60585247791041   4.99656201806673   1.122  0.27748   
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 2.939 on 17 degrees of freedom
# Multiple R-squared:  0.8309,	Adjusted R-squared:  0.5426 
# F-statistic: 2.881 on 29 and 17 DF,  p-value: 0.01264
ESG <- read_excel("ESG_rating_remain_modify.xlsx")
ESG_SCORE <- read.csv("ESG_SCORE.csv")[,-c(1,6)]
ESG_BERTS_SCORE <- read.csv("esg_bert_result.csv")[,-1]
FN <- read.csv("financial_data.csv")[,-c(1,2)]
#co2_df_3_1 <- read.csv("co2_energy_release.csv")[,-c(1, 6, 8, 9)]

names(ESG) <- c("company_name", "Total_grade", "Grade_Environment", "Grade_Society", "Grade_Governance")
names(ESG_SCORE) <- c("company_name", "CSR_Social", "CSR_Governance", "CSR_Environment")
names(ESG_BERTS_SCORE)
names(FN) <- c("company_name", "ROA", "ROE", "Tobins.q", "OCF", "Leverage", "Firm_Size")
#names(co2_df_3_1) <- c("company_name", "year", "industry_category", "co2", "energy")

ESG_BERTS_SCORE$company_name <- gsub(".json", "", ESG_BERTS_SCORE$CSR)
ESG_BERTS_SCORE <- ESG_BERTS_SCORE[,-27]
ESG_BERTS_SCORE[is.na(ESG_BERTS_SCORE)] <- 0

df_4_1 <- left_join(ESG, ESG_BERTS_SCORE, by = "company_name")
df_4_1 <- left_join(df_4_1, ESG_SCORE, by = "company_name")
df_4_1 <- left_join(df_4_1, FN, by = "company_name")



df_4_1$Green_Washing <- Green_Washing(df_4_1)
df_4_1$Total_grade <- as.factor(df_4_1$Total_grade)
df_4_1$Grade_Environment <- as.factor(df_4_1$Grade_Environment)
df_4_1$Grade_Society <- as.factor(df_4_1$Grade_Society)
df_4_1$Grade_Governance <- as.factor(df_4_1$Grade_Governance)

df_4_1$Total_grade <- factor(df_4_1$Total_grade, levels = c("A+", "A", "B+", "B", "C+", "C", "D+", "D"))
df_4_1$Grade_Environment <- factor(df_4_1$Total_grade, levels = c("A+", "A", "B+", "B", "C+", "C", "D+", "D"))
df_4_1$Grade_Society <- factor(df_4_1$Grade_Society, levels = c("A+", "A", "B+", "B", "C+", "C", "D+", "D"))
df_4_1$Grade_Governance <- factor(df_4_1$Grade_Governance, levels = c("A+", "A", "B+", "B", "C+", "C", "D+", "D"))
df_4_1 <- drop_na(df_4_1)

##### 전처리 이후 데이터 셋 93개
df_4_1company_name <- df_4_1$company_name
df_4_1 <- df_4_1[,-c(1:5, 32:34)]

##### discriptive statitic
df_4_1_summary_stats_df <- t(summarize_columns(df_4_1))
df_4_1_mean_values <- sapply(df_4_1, mean, na.rm = TRUE)
df_4_1_sd_values <- sapply(df_4_1, sd, na.rm = TRUE)
df_4_1_stats_df <- data.frame(Mean = df_4_1_mean_values, sd_values = df_4_1_sd_values)
df_4_1_summary_stats_df <- cbind(df_4_1_summary_stats_df, df_4_1_stats_df)
df_4_1_summary_stats_df

df_4_1_summary_stats_df$IndexValue = row.names(df_4_1_summary_stats_df)

write.xlsx(df_4_1_summary_stats_df, "descriptive2.xlsx")


##### stepwise
model <- lm(Green_Washing ~ ., data = df_4_1)
df_4_1.lm.step <- step(model)
summary(df_4_1.lm.step) 

# 원하는 컬럼만 선택하여 새로운 데이터 프레임 생성
df_4_selected <- df_4_1[, c("Green_Washing", "GHG_Emissions", "Supply_Chain_Management", "Employee_Health_And_Safety", "OCF", "Firm_Size")]


#######################
add_group <- function(df) {
  median_value <- median(df$Green_Washing, na.rm = TRUE)
  df <- df %>%
    mutate(group = ifelse(Green_Washing > median_value, "Upper Half", "Lower Half"))
  return(df)
}

# FN과 df_4_1에 그룹 변수 추가
FN_grouped <- add_group(df_1)
df_4_1_grouped <- add_group(df_4_1)

# 두 데이터프레임을 결합하여 하나의 데이터프레임으로 만들기
combined_df1 <- bind_rows(FN_grouped %>% mutate(dataset = "Kospi200"),
                         df_4_1_grouped %>% mutate(dataset = "Selected"))

# 상자 수염 그림 생성
ggplot(combined_df, aes(x = dataset, y = Green_Washing, fill = group)) +
  geom_boxplot() +
  facet_wrap(~dataset, scales = "free") +
  labs(
       x = "Dataset",
       y = "Green_Washing",
       fill = "Group") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
###############
remove_outliers <- function(df) {
  Q1 <- quantile(df$Green_Washing, 0.25, na.rm = TRUE)
  Q3 <- quantile(df$Green_Washing, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  df <- df %>%
    filter(Green_Washing >= (Q1 - 1.5 * IQR) & Green_Washing <= (Q3 + 1.5 * IQR))
  return(df)
}

# 각 데이터프레임에 아웃라이어 제거 및 그룹 변수 추가
FN_processed <- FN_grouped %>%
  remove_outliers() %>%
  add_group()

df_4_1_processed <- df_4_1 %>%
  remove_outliers() %>%
  add_group()

# 두 데이터프레임을 결합
combined_df_processed <- bind_rows(FN_processed %>% mutate(dataset = "Kospi200"),
                                   df_4_1_processed %>% mutate(dataset = "Selected"))

# 아웃라이어 제거된 데이터로 상자 수염 그림 생성
ggplot(combined_df_processed, aes(x = dataset, y = Green_Washing, fill = group)) +
  geom_boxplot() +
  facet_wrap(~dataset, scales = "free") +
  labs(
       x = "Dataset",
       y = "Green_Washing",
       fill = "Group") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
##################
FN_grouped <- add_group(df_1)
df_3_1_grouped <- add_group(df_3_1)

# 두 데이터프레임을 결합하여 하나의 데이터프레임으로 만들기
combined_df3 <- bind_rows(FN_grouped %>% mutate(dataset = "Kospi200"),
                         df_3_1_grouped %>% mutate(dataset = "Selected"))

# 상자 수염 그림 생성
ggplot(combined_df3, aes(x = dataset, y = Green_Washing, fill = group)) +
  geom_boxplot() +
  facet_wrap(~dataset, scales = "free") +
  labs(
    x = "Dataset",
    y = "Green_Washing",
    fill = "Group") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))
###############



# 각 데이터프레임에 아웃라이어 제거 및 그룹 변수 추가
FN_processed <- FN_grouped %>%
  remove_outliers() %>%
  add_group()

df_3_1_processed <- df_3_1 %>%
  remove_outliers() %>%
  add_group()

df_2_1

# 두 데이터프레임을 결합
combined_df_processed <- bind_rows(FN_processed %>% mutate(dataset = "Kospi200"),
                                   df_3_1_processed %>% mutate(dataset = "Selected"))

# 아웃라이어 제거된 데이터로 상자 수염 그림 생성
ggplot(combined_df_processed, aes(x = dataset, y = Green_Washing, fill = group)) +
  geom_boxplot() +
  facet_wrap(~dataset, scales = "free") +
  labs(
    x = "Dataset",
    y = "Green Washing",
    fill = "Group") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5))


#################
install.packages("tidyverse")
library(tidyverse)
library(ggplot2)
library(stats)
library(car)
# 정규성 검사
shapiro_test_kospi200 <- shapiro.test(df_1$Green_Washing)
shapiro_test_kospi200

shapiro_test_selected <- shapiro.test(df_4_1$Green_Washing)
shapiro_test_selected

shapiro_test_selected <- shapiro.test(df_3_1$Green_Washing)
shapiro_test_selected

shapiro_test_selected <- shapiro.test(df_2_1$Green_Washing)
shapiro_test_selected

Kospi200 <- data.frame(Green_Washing = df_1$Green_Washing, group = 'Kospi200')
Selected2 <- data.frame(Green_Washing = df_4_1$Green_Washing, group = 'Selected')
Selected4 <- data.frame(Green_Washing = df_3_1$Green_Washing, group = 'Selected')
Selected3 <- data.frame(Green_Washing = df_2_1$Green_Washing, group = 'Selected')



# 두 데이터프레임 결합
combined_data2 <- rbind(Kospi200, Selected2)
combined_data4 <- rbind(Kospi200, Selected4)
combined_data3 <- rbind(Kospi200, Selected3)

# 등분산성 검사
wilcox_test_result2 <- wilcox.test(Green_Washing ~ group, data = combined_data2)

wilcox_test_result3 <- wilcox.test(Green_Washing ~ group, data = combined_data3)

wilcox_test_result4 <- wilcox.test(Green_Washing ~ group, data = combined_data4)

wilcox_test_result2

wilcox_test_result3

wilcox_test_result4

# 가설 검정
if (shapiro_test_kospi200$p.value > 0.05 & shapiro_test_selected$p.value > 0.05 & levene_test$p.value > 0.05) {
  # 두 집단 모두 정규 분포를 따르며 등분산성을 가지는 경우
  t_test_result <- t.test(firmsize ~ group, data = combined_data, var.equal = TRUE)
} else {
  # 정규 분포를 따르지 않거나 등분산성이 없는 경우
  t_test_result <- wilcox.test(firmsize ~ group, data = combined_data)
}

# 결과 출력
print(t_test_result)
######################
summary(df_1.lm.step) 
summary(df_4_1.lm.step) 
summary(df_3_1_model)
summary(df_2_1.lm.step)

################
set.seed(123) # 재현 가능한 결과를 위한 시드 설정
control <- trainControl(method="cv", number=10, savePredictions = TRUE) # 10-Fold CV 설정

# 데이터 준비
df <- df_1_selected # 가정: 'df_1.lm.step'가 데이터 프레임 이름
y <- df$Green_Washing
X <- df[,-ncol(df)]

splitIndex <- createDataPartition(y, p = 0.8, list = FALSE) # 80% 훈련 데이터, 20% 테스트 데이터

# 훈련 데이터와 테스트 데이터 생성
train <- df[splitIndex,]
test <- df[-splitIndex,]

y_train <- train$Green_Washing
X_train <- train[,-ncol(df)]
y_test <- test$Green_Washing
X_test <- test[,-ncol(df)]
# ANN 모델
model_ann <- train(x = X, y = y, method = "nnet", trControl = control, linout = TRUE, trace = FALSE)
# LR 모델
model_lr <- train(x = X, y = y, method = "lm", trControl = control)
# RF 모델
model_rf <- train(x = X, y = y, method = "rf", trControl = control)
# XGB 모델
model_xgb <- train(x = X_train, y = y_train, method = "xgbLinear")

# 결과 요약
results <- resamples(list(ANN=model_ann, LR=model_lr, RF=model_rf, XGB=model_xgb))
summary(results)


################
# ggplot2 패키지 로드
library(ggplot2)

# model_xgb를 사용하여 test 데이터셋의 예측값 계산
predicted_values <- predict(model_xgb, newdata = test)


# 실제값이 test 데이터셋에 포함되어 있다고 가정하고, 예를 들어 컬럼 이름이 Actual_Value라고 가정
actual_values <- test$Green_Washing

# 데이터 프레임 생성
data <- data.frame(Actual = actual_values, Predicted = predicted_values)

# 실제값과 예측값 비교 플롯 생성 및 회귀선 추가
ggplot(data, aes(x = Actual, y = Predicted)) +
  geom_point() + # 실제값과 예측값의 산점도
  geom_smooth(method = "lm", se = FALSE, color = "blue", aes(linetype = "Regression Line")) + # 회귀선 추가
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red", aes(linetype = "Perfect Prediction")) + # 완벽한 예측선 추가
  scale_linetype_manual("Line Type", values = c("Regression Line" = "solid", "Perfect Prediction" = "dashed")) + # 선 유형 범례 정의
  labs(title = "Actual vs. Predicted Values", x = "Actual Values", y = "Predicted Values") +
  theme_minimal()
