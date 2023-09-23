# ChatGPT AES Final Models

library(tidyverse)
library(lme4)
library(car)

# Read in data:
df <- read.csv("response_grades_for_modeling.csv")

# Make Factors
df$respondent <- as.factor(df$respondent) %>% relevel(ref = "human")
df$ml_model <- factor(df$ml_model, levels = c("SVR", "XGBRFRegressor",
                                                  "CNN", "GRU",  
                                                  "BERT", "RoBERT"))
df$prompt_id <- as.factor(df$prompt_id)
df$prompt_type <- as.factor(df$prompt_type)


# MODEL WITHOUT GPT-4 ########################################################################

# filter out gpt_4 responses: 
df_without_gpt_4 <- df %>% mutate(gpt = gpt3) %>% select(!gpt3) %>% filter(gpt4 == 0)

random_intercept_model_wo_gpt4 <- lme4::lmer("score ~ prompt_type * gpt * ml_model + wordcount + testbed_aes + (1 | prompt_type/prompt_id)", data = df_without_gpt_4, )

anova_table_wo_gpt4 <- anova(random_intercept_model_wo_gpt4) 
print(anova_table_wo_gpt4)



# MODEL WITH GPT-4 #############################################################
random_intercept_model_w_gpt4 <- lme4::lmer("score ~ prompt_type * respondent * ml_model + wordcount + testbed_aes + (1 | prompt_type/prompt_id)", data = df)

summary(random_intercept_model_w_gpt4)

anova_table_w_gpt4 <- anova(random_intercept_model_w_gpt4) 
print(anova_table_w_gpt4)

