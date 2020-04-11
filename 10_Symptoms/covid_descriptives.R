rm(list=ls())

library(tidyverse)

df <- read.csv("CovidTextBasedSymptoms.csv", stringsAsFactors = F) %>%
  # Fix some misspellings
  mutate(symptom = gsub("feve\\\\","fever",symptom)) %>%
  mutate(symptom = gsub("cough. Flu","cough, Flu",symptom)) %>%
  mutate(symptom = gsub("cough with sputum","cough, sputum", symptom)) %>%
  # Death is sometimes coded as 1/0 sometimes with date of death. Recode as logical
  mutate(death_lgl = (death != "0")*1)

# Countries
table(df$country)/nrow(df)*100

# Unfiltered list of symptoms in the file
df$symptom %>%
  unique() %>%
  map(str_split,pattern=",") %>%
  do.call(c,.) %>%
  do.call(c,.) %>%
  map_chr(function(s) gsub("^ *","",s)) %>%
  unique()

symptoms <- c(
  "pneumonia", "cough", "fever", "sore throat", "chills", "joint pain", "throat pain", "runny nose", "fatigue",
  "abdominal pain", "diarrhea", "cold", "vomiting", "loss of appetite", "malaise", "headache", "difficulty breathing",
  "sputum", "breathlessness", "nausea", "nasal discharge", "muscle pain", "respiratory distress", "mild cough",
  "throat discomfort", "sneeze", "heavy head", "chest pain", "dyspnea", "thirst", "dry cough", "Flu",
  "mild fever", "breathing problems", "high fever", "muscle cramps", "aching muscles", "sore body", "chest discomfort",
  "chill", "shortness of breath", "muscle aches", "reflux", "physical discomfort", "itchy throat",
  "tired", "myalgia", "dry mouth")

df.symptoms <- data.frame(map(symptoms,function(s) grepl(s,df$symptom)*1))
colnames(df.symptoms) <- map_chr(symptoms,function(s) paste0("symptom_",gsub(" ","_",s)))

df <- bind_cols(df,df.symptoms)

# Occurences of each symptom
(df.symptoms %>% colSums())/nrow(df)*100

# Quick and dirty logistic regression on death predictor symptoms
frml <- paste0("death_lgl ~ ", paste0(map_chr(symptoms,function(s) paste0("symptom_",gsub(" ","_",s))),collapse = " + ") ) %>% as.formula()
glm(frml,data=df,family = binomial(link = "logit"))
# Perfect separation... Do it Bayesian
library(arm)
bayesglm(frml,data=df,family = binomial(link = "logit"))


write.csv(df,"CovidTextBasedSymptoms_cleaned.csv")
