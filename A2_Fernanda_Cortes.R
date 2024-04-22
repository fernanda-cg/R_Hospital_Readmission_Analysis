#' Author: Fernanda Cortes
#' Date: Mar 31, 2023
#' Purpose: A2 - Hospital Readmission EDA & Modeling
#' 

## Set the working directory
setwd("~/Documents/Visualizing & Analyzing Data with R/hult_r_class/personalFiles/A2 - Hospital EDA")

# Libraries
options(scipen = 999)
library(tm)
library(qdapRegex)
library(powerjoin)
library(dplyr)
library(scales)
library(stringr)
library(tidyr)
library(corrplot)
library(ggplot2)
library(ggthemes)
library(viridis)
library(RColorBrewer)
library(radiant.data)
library(DataExplorer)
library(tidyverse)
library(sf)
library(mapview)
library(ggmap)
library(plotly)
library(vtreat)
library(ModelMetrics)
library(MLmetrics)
library(pROC)
library(caret)
library(rpart.plot) #visualizing
library(ranger)


# Using Joining Supplemental, written by professor Ted Kwartler, to get one csv

# List all the files paths of interest from your local machine
allTrainFiles <- list.files(path = '~/Documents/Visualizing & Analyzing Data with R/hult_r_class/personalFiles/A2 - Hospital EDA/A2_Hospital_Readmission/caseData/Train',
                       pattern = '*.csv',
                       full.names = T)

allTestFiles <- list.files(path = '~/Documents/Visualizing & Analyzing Data with R/hult_r_class/personalFiles/A2 - Hospital EDA/A2_Hospital_Readmission/caseData/Test',
                            pattern = '*.csv',
                            full.names = T)

# Read in a list, list apply a function to an object ie read.csv to the file paths
allTrainDF <- lapply(allTrainFiles, read.csv)

allTestDF <- lapply(allTestFiles, read.csv)

# Using data.table rbindlist put all the data together
pTrain <- power_left_join(allTrainDF, by = "tmpID")

pTest <- power_left_join(allTestDF, by = "tmpID")

#write.csv(pTrain, 'patientsTrain.csv', row.names = F)

# Copy patients data frame to drop columns and fill nulls where needed
pTrain_c <- data.frame(pTrain)
pTest_c <- data.frame(pTest)

##########################################
##### DATA CLEANING AND ORGANIZATION #####
##########################################

# Find how many NAs values each column has to decide what to do with each
pTrain_c[pTrain_c == '' | pTrain_c == '?'] <- NA
countNulls <- data.frame(nulls = colSums(is.na(pTrain_c)))
countNulls$percentage <- countNulls$nulls / nrow(pTrain_c)
as.data.frame(countNulls)

# There are a few columns with NAs in the database, 
# we'll check the unique values to determine if we have to input or alter cols
sapply(pTrain_c[, row.names(countNulls)], unique)

# Drop 'constant' columns, ie columns that only have one value
constant <- c('examide', 'troglitazone', 'acetohexamide', 'citoglipton')
pTrain_c <- pTrain_c[, !names(pTrain_c) %in% constant]

# Unify 'not available' in admission_type_id, discharge_disposition_id, admission_source_id
unify <- c('admission_type_id', 'discharge_disposition_id', 'admission_source_id')

for (col_name in unify) {
  pTrain_c[[col_name]][pTrain_c[[col_name]] == 'Not Mapped' | 
                         #is.na(pTrain_c[[col_name]]) | 
                         pTrain_c[[col_name]] == 'Not Available'] <- NA
}

# Turn entries into factors for max_glu_serum and A1Cresult
pTrain_c$max_glu_serum <- factor(pTrain_c$max_glu_serum, 
                                       levels = c("None",
                                                  "Norm",
                                                  ">200",
                                                  ">300"),
                                       labels = c(0,1,2,3))
#pTrain_c$max_glu_serum <- as.numeric(pTrain_c$max_glu_serum)

pTrain_c$A1Cresult <- factor(pTrain_c$A1Cresult, 
                                     levels = c("None",
                                                "Norm",
                                                ">7",
                                                ">8"),
                                     labels = c(0,1,2,3))
#pTrain_c$A1Cresult <- as.numeric(pTrain_c$A1Cresult)


# For medicine columns, change entries to 0 = No, 1 = Down, 2 = Steady, 3 = Up
medicine <- c('metformin', 'repaglinide', 'nateglinide', 'chlorpropamide', 
              'glimepiride', 'glipizide', 'glyburide', 'tolbutamide', 
              'pioglitazone', 'rosiglitazone', 'acarbose', 'miglitol', 'tolazamide',
              'insulin')

for (col_name in medicine) {
  pTrain_c[[col_name]] <- factor(pTrain_c[[col_name]],
                                 levels = c("No",
                                            "Down",
                                            "Steady",
                                            "Up"),
                                 labels = c(0,1,2,3))
}

# Change diabetesMed and change to bool
pTrain_c$change[pTrain_c$change == 'Ch'] <- 'Yes'
pTrain_c$change <- as.logical(pTrain_c$change == 'Yes')
pTrain_c$diabetesMed <- as.logical(pTrain_c$diabetesMed == 'Yes')

# Drop entries that are registered as 'Expired' in discharge_disposition_id
pTrain_c <- subset(pTrain_c, discharge_disposition_id != "Expired")


##########################################
################### EDA ##################
##########################################
df <- pTrain_c

summary(df)

# Chose color palette 
display.brewer.pal(n = 9, name = 'PuBu')
brewer.pal(n = 9, name = "PuBu")

# Age boxplot
ggplot(df, aes(x = readmitted_y, y = age, fill = readmitted_y)) +
  geom_boxplot(color = 'darkslategray3', outlier.shape = 1) +
  stat_summary(fun = mean, geom = "point", shape = 5, size = 3, color = "darkslategray3") +
  labs(title = "Age Distribution",
       y = "Age") +
  scale_fill_manual(values = c("#023858", "#3690C0"), 
                    labels = c("No", "Yes")) +
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  guides(fill = guide_legend(title = "Readmission")) +
  geom_text(data = df %>% 
              group_by(readmitted_y) %>% 
              summarise(mean_age = mean(age)),
            aes(x = readmitted_y, y = mean_age, label = sprintf("%.1f", mean_age)), 
            size = 3, color = 'white', vjust = 2.25)
ggsave('segmented_age_boxplot.jpg')

# Weight boxplot
ggplot(df, aes(x = readmitted_y, y = wgt, fill = readmitted_y)) +
  geom_boxplot(color = 'darkslategray3', outlier.shape = 1) +
  stat_summary(fun = mean, geom = "point", shape = 5, size = 3, color = "darkslategray3") +
  labs(title = "Weight Distribution",
       y = "Weight") +
  scale_fill_manual(values = c("#023858", "#3690C0"), 
                    labels = c("No", "Yes")) +
  theme_linedraw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank()) +
  guides(fill = guide_legend(title = "Readmission")) +
  geom_text(data = df %>% 
              group_by(readmitted_y) %>% 
              summarise(mean_wgt = mean(wgt)),
            aes(x = readmitted_y, y = mean_wgt, label = sprintf("%.1f", mean_wgt)), 
            size = 3, color = 'white', vjust = -1.25)
ggsave('segmented_wgt_boxplot.jpg')


# Distribution of T/F on readmitted column
# This visualization is used to understand the number of patients that were 
# readmitted vs the ones that have not been readmitted
ggplot(df, aes(x = '', fill = readmitted_y)) +
  geom_bar(position = 'fill') +
  labs(title = "Number of Readmissions", 
       x = "Readmitted", y = "Count") +
  scale_fill_manual(values = c("#023858", "#3690C0")) +
  theme_linedraw() +
  guides(fill = FALSE) +
  scale_y_continuous(labels = scales::percent_format()) +
  
# By gender
  ggplot(df, aes(x = gender, fill = readmitted_y)) +
  geom_bar(position = 'fill') +
  labs(title = "by Gender", 
       x = "Gender", y = "Count") +
  scale_fill_manual(values = c("#023858", "#3690C0"), 
                    labels = c("No", "Yes")) +
  theme_linedraw() +
  guides(fill = FALSE) +
  scale_y_continuous(labels = scales::percent_format()) +

# By admission type
ggplot(df, aes(x = admission_type_id, fill = readmitted_y)) +
  geom_bar(position = 'fill', na.rm = TRUE) +
  labs(title = "by Admission type", 
       x = "Admission type", y = "Count") +
  scale_fill_manual(values = c("#023858", "#3690C0"), 
                    labels = c("No", "Yes")) +
  theme_linedraw() +
  guides(fill = FALSE) +
  scale_y_continuous(labels = scales::percent_format()) +
  
# By # comorbidity
#ggplot(df, aes(x = total_comorb, fill = readmitted_y)) +
#  geom_bar(position = 'fill', na.rm = TRUE) +
#  labs(title = "by Number of Comorbidities", 
#       x = "# of comorbidities", y = "Count") +
#  scale_fill_manual(values = c("#023858", "#3690C0"), 
#                    labels = c("No", "Yes")) +
#  theme_linedraw() +

# By # diagnoses
ggplot(df, aes(x = number_diagnoses, fill = readmitted_y)) +
  geom_bar(position = 'fill', na.rm = TRUE) +
  labs(title = "by Number of Diagnoses", 
       x = "# of Diagnoses", y = "Count") +
  scale_x_continuous(breaks = seq(min(df$number_diagnoses), max(df$number_diagnoses), by = 1)) +
  scale_fill_manual(values = c("#023858", "#3690C0"), 
                    labels = c("No", "Yes")) +
  theme_linedraw() +
  guides(fill = guide_legend(title = "Readmission")) + 
  scale_y_continuous(labels = scales::percent_format())
  
ggsave('readmitted_dist.jpg', width = 10)


# Histogram with density curve for age distribution
ggplot(df, aes(x = age, fill = readmitted_y)) +
  geom_histogram(binwidth = 1, position = "identity") +
  scale_fill_manual(values = c("#023858", "#3690C0"), 
                    labels = c("No", "Yes")) +
  theme_linedraw() +
  labs(title = "Age Distribution",
       x = "Age (years)",
       y = "Count") +
  guides(fill = guide_legend(title = "Readmission")) +
  geom_density(aes(y = ..count.., fill = readmitted_y), 
               alpha = 0.5, 
               color = "black", 
               size = 1.0) +
  scale_y_continuous(sec.axis = dup_axis()) +
  theme(legend.position = c(0.9, 0.9),
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill = NA),
        legend.box.background = element_rect(colour = "black"))
ggsave('segmented_age_dist.jpg')


##########################################
########### FEATURE ENGINEERING ##########
##########################################

# There are three columns that describe different diagnosis, they will be changed 
# so we can keep the relevant info only
diagCol <- c('diag_1_desc','diag_2_desc', 'diag_3_desc')

# Create a new data frame that contains the diagnosis columns to analyze them
diagnosis <- subset(pTrain, select = diagCol)
diagnosis$all <- paste(diagnosis$diag_1_desc, 
                       diagnosis$diag_2_desc, 
                       diagnosis$diag_3_desc, 
                       sep = ". ")

# Drop common words and then separate each word into a new column 
# to get the most repeated word. Using class material on text cleaning.
# Options & Functions
Sys.setlocale('LC_ALL','C')

tryTolower <- function(x){
  y         = NA
  try_error = tryCatch(tolower(x), error = function(e) e)
  if (!inherits(try_error, 'error')){}
  y         = tolower(x)
  return(y)
}

cleanCorpus <- function(corpus, customStopwords){
  corpus <- tm_map(corpus, content_transformer(qdapRegex::rm_url))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, stripWhitespace)
  corpus <- tm_map(corpus, removeNumbers)
  corpus <- tm_map(corpus, content_transformer(tryTolower))
  corpus <- tm_map(corpus, removeWords, customStopwords)
  return(corpus)
}

# Create custom stop words. Some were added after a first round of checking most 
# frequent words.
customStopwords <- c(stopwords('english'), 'unspecified', 'region', 'site', 'type',
                     'without', 'diabetes', 'uncontrolled', 'mention', 'stated',
                     'mellitus', 'disease', 'essential', 'stage', 'specified',
                     'elsewhere', 'classified', 'manifestations', 'secondary',
                     'face', 'simple', 'organism', 'wall', 'tract', 'episode', 
                     'care', 'native', 'closed', 'generalized', 'and / or', 
                     'and/or', 'andor')

# Build a volatile corpus
txtCorpus <- VCorpus(VectorSource(diagnosis$all))

# Preprocess the corpus
txtCorpus <- cleanCorpus(txtCorpus, customStopwords)

# Make TDM
diagDTM  <- DocumentTermMatrix(txtCorpus)
diagDTMm <- as.matrix(diagDTM)
dim(diagDTMm)

diagFreq <- colSums(diagDTMm)
diagFreq <- data.frame(word=names(diagFreq),
                        frequency=diagFreq, 
                        row.names = NULL)

# Simple barplot; values greater than 50 
topWords <- subset(diagFreq, diagFreq$frequency >= 200) 
topWords <- topWords[order(topWords$frequency, decreasing=F),]
#as.data.frame(topWords[order(topWords$word, decreasing=F),])$word

# Chg to factor for ggplot
topWords$word <- factor(topWords$word, 
                                levels=unique(as.character(topWords$word))) 

ggplot(subset(topWords, frequency > 600), aes(x=word, y=frequency)) + 
  geom_bar(stat="identity", fill='#023858') + 
  coord_flip()+ theme_gdocs() +
  geom_text(aes(label=frequency), colour="white",hjust=1.25, size=3.5) +
  theme_linedraw() +
  labs(title="Top Medical Terms",
       x="Medical Term",
       y="Frequency",
       caption="Subset of Diagnoses with Frequency >= 600")
ggsave('topTerms.jpg')

# Using the graph and additional sources, we'll select the most important
# comorbidities for diabetes. For practical reasons, the graph only shows
# words that occur more than 600 times; nevertheless, for the analysis 
# all the words with frequency >= 200 were used. 

#### Group by different medical conditions, diag, disease, etc. 
# Create a vector of medical types
cardiovascular <- c("angina", "myocardial", "heart", "cardiac", "coronary", 
                    "artery", "endomyocardial", "postmyocardial", "mitral", 
                    "infarction", "anterolateral", "congestive",
                    "heart", "tachycardia", "valve", "vessel", "graft", 
                    "paroxysmal", "supraventricular")

neurological <- c("myelopathy", "neurological", "cerebral", "alteration", 
                  "consciousness", "disorder")

gastrointestinal <- c("decubitus", "intestine", "cholecystitis", 
                      "cholelithiasis", "ulcer")

renal <- c("renal", "kidney", "urinary")

pulmonary <- c("asthma", "bronchitis", "pulmonary", "pneumonia", "abnormality",
               "airway", "collapse", "respiratory")

infections <- c("streptococcal", "septicemia", "infection", "streptococcus", 
                "bronchitis", "pneumonia", "abscess")

metabolic_endocrine <- c("hypercholesterolemia", "hypernatremia", "hyperosmolality", 
                         "thyroid", "hypertension", "atherosclerosis")

hematological <- c("anemia", "hemorrhage", "thrombosis", "deficiency", 
                   "iron", "sideroblastic")

failure <- "failure"
juvenile <- "juvenile"
malignant <- "malignant"
neoplasm <- "neoplasm"

### Create columns with diagnosis information
# Create a new column for every medical condition
medical_t <- c('cardiovascular', 'neurological', 'gastrointestinal', 'renal',
               'pulmonary', 'infections', 'metabolic_endocrine', 'hematological',
               'complication', 'failure', 'juvenile', 'malignant', 'neoplasm')

for (j in medical_t) {
  pTrain_c[[j]] <- 0
  # Loop through the rows of your data frame
  for (i in 1:nrow(pTrain_c)) {
    # Check if any of the words appear in diag_1_desc, diag_2_desc, or diag_3_desc using grepl
    if (any(grepl(paste(j, collapse="|"), 
                  tolower(c(pTrain_c$diag_1_desc[i], 
                    pTrain_c$diag_2_desc[i], 
                    pTrain_c$diag_3_desc[i]))))) {
      # Set the value of column to 1 if any word is found
      pTrain_c[[j]][i] <- 1
    }
  }
}


# Drop original diagnosis columns
pTrain_c <- pTrain_c[, !names(pTrain_c) %in% diagCol]

# Add column to get total number of comorb
pTrain_c$total_comorb <- 0
for (j in medical_t) {
  pTrain_c$total_comorb <- pTrain_c$total_comorb + pTrain_c[[j]]
}

# Change medical_t to factor
for (i in medical_t) {
  pTrain_c[[i]] <- as.factor(pTrain_c[[i]])
  }


##########################################
########## APPLY CHANGES TO TEST #########
##########################################
pTest_c[pTest_c == '' | pTest_c == '?'] <- NA

# Drop 'constant' columns, ie columns that only have one value
pTest_c <- pTest_c[, !names(pTest_c) %in% constant]

# Unify 'not available' in admission_type_id, discharge_disposition_id, admission_source_id
for (col_name in unify) {
  pTest_c[[col_name]][pTest_c[[col_name]] == 'Not Mapped' | 
                        #is.na(pTest_c[[col_name]]) | 
                        pTest_c[[col_name]] == 'Not Available'] <- NA
}

# Turn entries into factors for max_glu_serum and A1Cresult
pTest_c$max_glu_serum <- factor(pTest_c$max_glu_serum, 
                                levels = c("None",
                                           "Norm",
                                           ">200",
                                           ">300"),
                                labels = c(0,1,2,3))
#pTest_c$max_glu_serum <- as.numeric(pTest_c$max_glu_serum)

pTest_c$A1Cresult <- factor(pTest_c$A1Cresult, 
                            levels = c("None",
                                       "Norm",
                                       ">7",
                                       ">8"),
                            labels = c(0,1,2,3))
#pTest_c$A1Cresult <- as.numeric(pTest_c$A1Cresult)


# For medicine columns, change entries to 0 = No, 1 = Down, 2 = Steady, 3 = Up
for (col_name in medicine) {
  pTest_c[[col_name]] <- factor(pTest_c[[col_name]],
                                levels = c("No",
                                           "Down",
                                           "Steady",
                                           "Up"),
                                labels = c(0,1,2,3))
}

# Change diabetesMed and change to bool
pTest_c$change[pTest_c$change == 'Ch'] <- 'Yes'
pTest_c$change <- as.logical(pTest_c$change == 'Yes')
pTest_c$diabetesMed <- as.logical(pTest_c$diabetesMed == 'Yes')

# Drop entries that are registered as 'Expired' in discharge_disposition_id
pTest_c <- subset(pTest_c, discharge_disposition_id != "Expired")

# Apply diagnosis changes
for (j in medical_t) {
  pTest_c[[j]] <- 0
  # Loop through the rows of your data frame
  for (i in 1:nrow(pTest_c)) {
    # Check if any of the words appear in diag_1_desc, diag_2_desc, or diag_3_desc using grepl
    if (any(grepl(paste(j, collapse="|"), 
                  tolower(c(pTest_c$diag_1_desc[i], 
                            pTest_c$diag_2_desc[i], 
                            pTest_c$diag_3_desc[i]))))) {
      # Set the value of column to 1 if any word is found
      pTest_c[[j]][i] <- 1
    }
  }
}


# Drop original diagnosis columns
pTest_c <- pTest_c[, !names(pTest_c) %in% diagCol]

# Add column to get total number of comorb
pTest_c$total_comorb <- 0
for (j in medical_t) {
  pTest_c$total_comorb <- pTest_c$total_comorb + pTest_c[[j]]
}

# Change medical_t to factor
for (i in medical_t) {
  pTest_c[[i]] <- as.factor(pTest_c[[i]])
}

######################################################################### MODELS

###################### LOGISTIC REGRESSION
################################################################################

##########################################
################ MODIFY ##################
##########################################

# Turn data class for non-numeric columns into factor

for (i in colnames(pTrain_c)) {
  if (i != "readmitted_y" & !is.numeric(pTrain_c[[i]]) & !is.integer(pTrain_c[[i]])) {
    pTrain_c[[i]] <- as.factor(pTrain_c[[i]])
  }
}


sapply(pTrain_c, class)

for (i in colnames(pTest_c)) {
  if (i != "readmitted_y" & !is.numeric(pTest_c[[i]]) & !is.integer(pTest_c[[i]])) {
    pTest_c[[i]] <- as.factor(pTest_c[[i]])
  }
}

# Identify the informative and target
names(pTrain_c)
targetVar       <- 'readmitted_y'
informativeVars <- names(pTrain_c)[!names(pTrain_c) %in% c('tmpID', 'readmitted_y')]

##########################################
################ SAMPLE ##################
##########################################

# Segment the prep data
set.seed(1234)
idx         <- sample(1:nrow(pTrain_c),.1*nrow(pTrain_c))
prepData    <- pTrain_c[idx,]
nonPrepData <- pTrain_c[-idx,]
#idKeeps <- pTrain_c$tmpID[-idx] #tracking id for later

# Design a "C"ategorical variable plan 
plan <- designTreatmentsC(prepData, 
                          informativeVars,
                          targetVar, 1)

# Apply to xVars
treatedX <- prepare(plan, nonPrepData)

##########################################
############### MODEL ####################
##########################################

# Fit a logistic regression model
fit <- glm(readmitted_y ~., data = treatedX, family ='binomial')
summary(fit)

# Backward Variable selection to reduce chances of multi-colinearity
#bestFit <- step(fit, direction='backward') # This takes 45+ minutes

summary(bestFit)

# Compare model size
length(coefficients(fit))
length(coefficients(bestFit))

##########################################
################ PREDICT #################
##########################################

# Predict on test data
trainPred <- predict(bestFit,  treatedX, type='response')
tail(trainPred)

# Classify 
cutoff      <- 0.5
trainClasses <- ifelse(trainPred >= cutoff, 1,0)

# Organize w/Actual
resultsTrain <- data.frame(actual  = nonPrepData$readmitted_y,
                      id    = nonPrepData$tmpID,
                      classes = as.logical(trainClasses),
                      probs   = trainPred)
head(resultsTrain)

# What is the accuracy?
Accuracy(resultsTrain$classes, resultsTrain$actual) #65.38%

# Get test predictions
testTreated <- prepare(plan, pTest_c)
patientPreds <- predict(bestFit,  testTreated, type='response')
tail(patientPreds)

# Classify 
cutoff      <- 0.5
patientClasses <- ifelse(patientPreds >= cutoff, 1,0)

#### ASSESS
# Organize w/Actual
results <- data.frame(actual  = pTest_c$readmitted_y,
                      id    = pTest_c$tmpID,
                      classes = as.logical(patientClasses),
                      probs   = patientPreds)
head(results)

# Get a confusion matrix
(confMat <- ConfusionMatrix(results$classes, results$actual))

#y_pred
#y_true     0    1
#FALSE 1123  251
#TRUE   557  374

# What is the accuracy?
Accuracy(results$classes, results$actual) #64.95%

# Visually how well did we separate our classes?
ggplot(results, aes(x=probs, color=as.factor(actual))) +
  geom_density() + 
  geom_vline(aes(xintercept = cutoff), color = '#023858')

# ROC; chg boolean to 1/0
ROCobj <- roc(results$classes, results$actual*1)
plot(ROCobj)

# AUC; chg boolean to 1/0
AUC(results$actual*1,results$classes)


############################ DECISION TREE
################################################################################

# No further modification needed nor sampling
anyNA(treatedX)
sum(is.na(treatedX))

##########################################
############### MODEL ####################
##########################################

# Fit a decision tree with caret using treated train data
set.seed(1234)
fitDT <- train(as.factor(readmitted_y) ~ ., 
               data = treatedX, 
               method = "rpart", 
               tuneGrid = data.frame(cp = c(0.0001, 0.001, 0.005, 0.01, 0.05, 0.07, 0.1, 0.25)),
               control = rpart.control(minsplit = 1, minbucket = 2)) 

# Examine
fitDT

# Plot the CP Accuracy Relationship to adjust the tuneGrid inputs
plot(fitDT)

# Plot a pruned tree
prp(fitDT$finalModel, extra = 1)


##########################################
################ PREDICT #################
##########################################

# Make some predictions on the training set
trainCaretDT <- predict(fitDT, treatedX)
head(trainCaretDT)

# Get the conf Matrix
confusionMatrix(trainCaretDT, as.factor(treatedX$readmitted_y))

#Confusion Matrix and Statistics

#Reference
#Prediction FALSE TRUE
#FALSE  3326 2017
#TRUE    290  580

#Accuracy : 0.6287               
#95% CI : (0.6165, 0.6407)     
#No Information Rate : 0.582                
#P-Value [Acc > NIR] : 0.00000000000003421  


# Now more consistent accuracy & fewer rules!
testCaretDT <- predict(fitDT,testTreated)
confusionMatrix(testCaretDT,as.factor(testTreated$readmitted_y))

# Confusion Matrix and Statistics

# Reference
# Prediction FALSE TRUE
# FALSE  1272  729
# TRUE    102  202

# Accuracy : 0.6395               
# 95% CI : (0.6195, 0.6591)     
# No Information Rate : 0.5961               
# P-Value [Acc > NIR] : 0.00001071 

# Get probabilities from predict()
testCaretDTProbs <- predict(fitDT,testTreated,type = 'prob')
head(testCaretDTProbs)

# Classify 
cutoff      <- 0.45
patientClassesDT <- ifelse(testCaretDTProbs$`TRUE` >= cutoff, TRUE,FALSE)
sum(sapply(patientClassesDT, isTRUE))

#### ASSESS
# Organize w/Actual
resultsDT <- data.frame(actual  = pTest_c$readmitted_y,
                      id    = pTest_c$tmpID,
                      classes = patientClassesDT,
                      probs   = testCaretDTProbs)
head(resultsDT)


# What is the accuracy after modifying cutoff?
Accuracy(resultsDT$classes, resultsDT$actual) #64.42%

# ROC; chg boolean to 1/0
ROCobj <- roc(resultsDT$classes, resultsDT$actual*1)
plot(ROCobj)

# AUC; chg boolean to 1/0
AUC(resultsDT$actual*1,resultsDT$classes)

############################ RANDOM FOREST
################################################################################

# No further modification needed nor sampling

##########################################
############### MODEL ####################
##########################################

# Fit a random forest model with Ranger
fitRF <- ranger(as.factor(readmitted_y) ~ .,
                     data  = treatedX, 
                     num.trees = 120,
                     importance = 'permutation',
                     mtry  = 1, 
                     probability = T)

# Examine
fitRF

# Look at var importance
varImpDF <- data.frame(variables = names(importance(fitRF)),
                       importance = importance(fitRF),
                       row.names = NULL)
varImpDF <- varImpDF[order(varImpDF$importance, decreasing = T),]
ggplot(varImpDF, aes(x=importance, y = reorder(variables, importance))) + 
  geom_bar(stat='identity', position = 'dodge') + 
  ggtitle('Variable Importance') + 
  theme_gdocs()

# Confusion Matrix
trainClassRF <- predict(fitRF, treatedX)
# In ranger objects, the predictions are within a list and need to be declared
head(trainClassRF$predictions)

# Using the prediction probability list element, classify with 0.50 cutoff 
classTrainOutcomeRF <- ifelse(trainClassRF$predictions[,2]>=0.45,TRUE,FALSE)
confusionMatrix(as.factor(classTrainOutcomeRF), 
                as.factor(treatedX$readmitted_y))

#Confusion Matrix and Statistics

#Reference
#Prediction FALSE TRUE
#FALSE  3310 1461
#TRUE    306 1136

#Accuracy : 0.7156              
#95% CI : (0.7042, 0.7268)      
#No Information Rate : 0.582               
#P-Value [Acc > NIR] : < 0.00000000000000022             


##### Try with less variables, dropping any that have less than 0.000125 var imp
varImpDF_subset <- subset(varImpDF, importance > 0.000125) 

# Fit second random forest model 
fitRF2 <- ranger(as.factor(readmitted_y) ~ .,
                 data  = treatedX[, c(varImpDF_subset$variables, 'readmitted_y')],
                 num.trees = 120,
                 importance = 'permutation',
                 mtry  = 1, 
                 probability = TRUE)

# Examine
fitRF2

# Look at improved var importance
varImpDF2 <- data.frame(variables = names(importance(fitRF2)),
                       importance = importance(fitRF2),
                       row.names = NULL)
varImpDF2 <- varImpDF2[order(varImpDF2$importance, decreasing = T),]
ggplot(varImpDF2, aes(x=importance, y = reorder(variables, importance))) + 
  geom_bar(stat='identity', position = 'dodge', fill = "#023858") + 
  ggtitle('Variable Importance') + 
  theme_gdocs() +
  scale_fill_manual(values = c("#023858"))  

ggsave('final_varImp.jpg', width = 15, height = 10)

# Confusion Matrix
trainClassRF2 <- predict(fitRF2, treatedX)
# In ranger objects, the predictions are within a list and need to be declared
head(trainClassRF2$predictions)

# Using the prediction probability list element, classify with 0.50 cutoff 
classTrainOutcomeRF2 <- ifelse(trainClassRF2$predictions[,2]>=0.45,TRUE,FALSE)
confusionMatrix(as.factor(classTrainOutcomeRF2), 
                as.factor(treatedX$readmitted_y)) # Accuracy 74.39%

#Confusion Matrix and Statistics

#Reference
#Prediction FALSE TRUE
#FALSE  3118 1093
#TRUE    498 1504

#Accuracy : 0.7439               
#95% CI : (0.7329, 0.7547)     
#No Information Rate : 0.582                
#P-Value [Acc > NIR] : < 0.00000000000000022


# Predict with test data
testClassRF2 <- predict(fitRF2, testTreated)
# In ranger objects, the predictions are within a list and need to be declared
head(trainClassRF2$predictions)
# Get probabilities from predict()
testCaretRF2Probs <- testClassRF2$predictions


# Classify. Using the prediction probability list element, classify with 0.45 cutoff 
patientClassesRF2 <- ifelse(testClassRF2$predictions[,2]>=0.45,TRUE,FALSE)
confusionMatrix(as.factor(patientClassesRF2), 
                as.factor(testTreated$readmitted_y)) # Accuracy 64.9%

#Confusion Matrix and Statistics

#Reference
#Prediction FALSE TRUE
#FALSE  1089  525
#TRUE    285  406

#Accuracy : 0.6486               
#95% CI : (0.6287, 0.6681)    
#No Information Rate : 0.5961              
#P-Value [Acc > NIR] : 0.0000001269              


#### ASSESS
# Organize w/Actual
resultsRF <- data.frame(actual  = pTest_c$readmitted_y,
                        id    = pTest_c$tmpID,
                        classes = patientClassesRF2,
                        probs   = testCaretRF2Probs)
head(resultsRF)

# ROC; chg boolean to 1/0
ROCobjRF <- roc(resultsRF$classes, resultsRF$actual*1)
plot(ROCobjRF)

# AUC; chg boolean to 1/0
AUC(resultsRF$actual*1,resultsRF$classes)


## Parameter tuning for RF using second RF model variables
grid <- expand.grid(.mtry = c(1,2),
                    .splitrule = 'extratrees',
                    .min.node.size = c(1,2))
fitControl <- trainControl(method = "CV",
                           number = 2,
                           verboseIter = TRUE,
                           classProbs = TRUE)

# Create a factor variable
treatedX$readmitted_y <- factor(treatedX$readmitted_y)

# Get the levels of the factor variable
levels(treatedX$readmitted_y) <- make.names(levels(treatedX$readmitted_y))

fitRF_tuned <- train(readmitted_y ~ .,
                     data = treatedX[, c(varImpDF_subset$variables, 'readmitted_y')],
                     method = 'ranger', # Use the ranger algorithm for random forest
                     #num.trees = 200, # Letting caret decide # of trees
                     tuneGrid = grid, # Use the grid of hyperparameters for tuning
                     trControl = fitControl) # Use the specified cross-validation scheme
              

fitRF_tuned$finalModel

##########################################
################ PREDICT #################
##########################################


# Make some predictions on the training set
trainCaretRF_t <- predict(fitRF_tuned, treatedX)
head(trainCaretRF_t)

# Get the conf Matrix
confusionMatrix(trainCaretRF_t, as.factor(treatedX$readmitted_y)) # Accuracy 85.7%


#Confusion Matrix and Statistics

#Reference
#Prediction FALSE TRUE
#FALSE  3544   816
#TRUE     72  1781

#Accuracy : 0.8571                
#95% CI : (0.8481, 0.8657)     
#No Information Rate : 0.582                
#P-Value [Acc > NIR] : < 0.00000000000000022

# Predict on test data
testCaretRF_t <- predict(fitRF_tuned,testTreated)

# Generate class probabilities for the test set using the trained model
testCaretRF_tProbs <- predict(fitRF_tuned, testTreated,type = 'prob')
head(testCaretRF_tProbs)

# Classify 
cutoff      <- 0.45
patientClassesRF_t <- ifelse(testCaretRF_tProbs$`TRUE` >= cutoff, TRUE,FALSE)

#### ASSESS
# Organize w/Actual
resultsRF_t <- data.frame(actual  = pTest_c$readmitted_y,
                        id    = pTest_c$tmpID,
                        classes = as.logical(patientClassesRF_t),
                        probs   = testCaretRF_tProbs)
head(resultsRF_t)

# Get a confusion matrix
(confMat_RFt <- ConfusionMatrix(resultsRF_t$classes, resultsRF_t$actual))

#y_pred
#y_true  FALSE TRUE
#FALSE  1020  354
#TRUE    441  490

# What is the accuracy?
Accuracy(resultsRF_t$classes, resultsRF_t$actual) #65.50%

# ROC; chg boolean to 1/0
ROCobjRF_t <- roc(resultsRF_t$classes, resultsRF_t$actual*1)
plot(ROCobjRF_t)

# AUC; chg boolean to 1/0
AUC(resultsRF_t$actual*1,resultsRF_t$classes)


################################################################ JOINING RESULTS

# Rename probabilities columns to identified them easier
colnames(results)[4] <- "logRegProbs.TRUE"
colnames(resultsDT)[4] <- "DTprobs.FALSE" 
colnames(resultsDT)[5] <- "DTprobs.TRUE"
colnames(resultsRF)[4] <- "RFprobs.FALSE" 
colnames(resultsRF)[5] <- "RFprobs.TRUE"
colnames(resultsRF_t)[4] <- "RF_tprobs.FALSE" 
colnames(resultsRF_t)[5] <- "RF_tprobs.TRUE"

# Join the data frames and select probability columns
finalResult <- results %>% 
  select(id, actual, logRegProbs.TRUE) %>% 
  inner_join(resultsDT %>% select(id, DTprobs.TRUE), by = "id") %>% 
  inner_join(resultsRF %>% select(id, RFprobs.TRUE), by = "id") %>%
  inner_join(resultsRF_t %>% select(id, RF_tprobs.TRUE), by = "id")

colnames(finalResult)[3] <- "logReg"
colnames(finalResult)[4] <- "DT"
colnames(finalResult)[5] <- "RF"
colnames(finalResult)[6] <- "RF_t"

# Get the average probability
finalResult$AVGprobs <- rowMeans(finalResult[, c("logReg", "DT", "RF", "RF_t")])
finalResult$finalClass <- ifelse(finalResult$AVGprobs >= 0.45,TRUE,FALSE)

confusionMatrix(as.factor(finalResult$finalClass), 
                as.factor(finalResult$actual)) 
#Confusion Matrix and Statistics

#Reference
#Prediction FALSE TRUE
#FALSE  1118  517
#TRUE    256  414

#Accuracy : 0.6646               
#95% CI : (0.645, 0.6839) 
#No Information Rate : 0.5961               
#P-Value [Acc > NIR] : 0.000000000007004 



# Order by AVGprobs to get top 100 patients
finalResult <- finalResult[order(finalResult$AVGprobs, decreasing=T),]
head(finalResult)

# Add final probability to test data set
pTest_c$AVGprobs <- inner_join(pTest_c, 
                               finalResult %>% select(id, AVGprobs), 
                               by = c("tmpID" = "id"))$AVGprobs


# Saving csv file
write.csv(head(finalResult, 100), 'finalPatients.csv', row.names = F)
write.csv(finalResult, 'finalResults.csv', row.names = F)

##########################################
################### EDA ##################
##########################################

# Get list of 100 ids to subset test data set
patients <- head(finalResult, 100)$id
topPatients <- subset(pTest_c, tmpID %in% patients)


# Combine the two data frames for graphs
df_combined <- rbind(transform(pTrain_c, group = "Train"), 
                     transform(topPatients, group = "Top 100"))

# Age boxplot
ggplot(df_combined, aes(x = group, y = age, fill = group)) +
  geom_boxplot(color = 'darkslategray3', outlier.shape = 1) +
  stat_summary(fun = mean, geom = "point", shape = 5, size = 3, color = "darkslategray3") +
  labs(title = "Age Distribution by Group",
       x = "Group",
       y = "Age") +
  scale_fill_manual(values = c("Train" = "#023858", "Top 100" = "#3690C0")) +
  theme_linedraw() +
  guides(fill = guide_legend(title = "Patient Type")) +
  geom_text(data = df_combined %>% 
              group_by(group) %>% 
              summarise(mean_age = mean(age)),
            aes(x = group, y = mean_age, label = sprintf("%.1f", mean_age)), 
            size = 3, color = 'white', vjust = 2.25) +
  geom_text(data = df_combined %>% 
              group_by(group) %>% 
              summarise(min_age = min(age)), 
            aes(x = group, y = min_age, label = sprintf("Min: %d", min_age)), 
            size = 3, color = 'black', vjust = 1.8)

ggsave('age_boxplot_100vsAll.jpg')

# Weight boxplot
ggplot(df_combined, aes(x = group, y = wgt, fill = group)) +
  geom_boxplot(color = 'darkslategray3', outlier.shape = 1) +
  stat_summary(fun = mean, geom = "point", shape = 5, size = 3, color = "darkslategray3") +
  labs(title = "Weight Distribution by Group",
       x = "Weight",
       y = "Age") +
  scale_fill_manual(values = c("Train" = "#023858", "Top 100" = "#3690C0")) +
  theme_linedraw() +
  guides(fill = guide_legend(title = "Patient Type")) +
  geom_text(data = df_combined %>% 
              group_by(group) %>% 
              summarise(mean_wgt = mean(wgt)),
            aes(x = group, y = mean_wgt, label = sprintf("%.1f", mean_wgt)), 
            size = 3, color = 'white', vjust = 2.25) +
  geom_text(data = df_combined %>% 
              group_by(group) %>% 
              summarise(min_wgt = min(wgt)), 
            aes(x = group, y = min_wgt, label = sprintf("Min: %d", min_wgt)), 
            size = 3, color = 'black', vjust = 1.8)

ggsave('wgt_boxplot_100vsAll.jpg')

# Distribution of days in hospital
ggplot(df_combined, aes(x = time_in_hospital, fill = group)) +
  geom_density(alpha = 0.6) +
  labs(title = "Time in Hospital Distribution by Group",
       x = "Time in Hospital",
       y = "Density") +
  scale_fill_manual(values = c("Train" = "#023858", "Top 100" = "#3690C0")) +
  theme_linedraw() +
  guides(fill = guide_legend(title = "Patient Type"))
ggsave('days_inHosp_100vsAll.jpg')

# Inpatient boxplot
ggplot(df_combined, aes(x = group, y = number_inpatient, fill = group)) +
  geom_boxplot(color = 'darkslategray3', outlier.shape = 1) +
  stat_summary(fun = mean, geom = "point", shape = 5, size = 3, color = "darkslategray3") +
  labs(title = "Number of Inpatient Visits by Group",
       x = "Group",
       y = "Number of Inpatient Visits") +
  scale_fill_manual(values = c("Train" = "#023858", "Top 100" = "#3690C0")) +
  theme_linedraw() +
  guides(fill = guide_legend(title = "Patient Type")) +
  geom_text(data = df_combined %>% 
              group_by(group) %>% 
              summarise(mean_inpatient = mean(number_inpatient)),
            aes(x = group, y = mean_inpatient, label = sprintf("%.1f", mean_inpatient)), 
            size = 3, color = 'white', vjust = 2)
ggsave('inpatient_100vsAll.jpg')

# Define the list of columns to obtain boxplots from 
cols <- c("num_lab_procedures", 
          "num_procedures", 
          "num_medications", 
          "number_outpatient", 
          "number_emergency", 
          "number_inpatient",
          "number_diagnoses")

# Loop through the list of columns
for(col in cols) {
  ggplot(df_combined, aes(x = group, y = !!sym(col), fill = group)) +
    geom_boxplot(color = 'darkslategray3', outlier.shape = 1) +
    stat_summary(fun = mean, geom = "point", shape = 5, size = 3, color = "darkslategray3") +
    labs(title = paste0("Number of ", str_to_title(str_replace(str_remove(str_remove(col, "num_"),"number_"),"_", " ")), " by Group"),
         x = "Group",
         y = paste0("Number of ", str_to_title(str_replace(str_remove(str_remove(col, "num_"),"number_"),"_", " ")))) +
    scale_fill_manual(values = c("Train" = "#023858", "Top 100" = "#3690C0")) +
    theme_linedraw() +
    guides(fill = guide_legend(title = "Patient Type")) +
    geom_text(data = df_combined %>% 
                group_by(group) %>% 
                summarise(mean_val = mean(!!sym(col))),
              aes(x = group, y = mean_val, label = sprintf("%.1f", mean_val)), 
              size = 3, color = 'white', vjust = 2)
  
  ggsave(paste0(str_remove(col, "num_"), "_100vsAll.jpg"))
}

# Loop for probability vs columns
for(col in cols) {
  ggplot(pTest_c, aes(x = !!sym(col), y = AVGprobs, fill = gender)) +
    scale_color_distiller(palette = "PuBu") +
    geom_point(color = "#3690C0", alpha = 0.4) +
    geom_smooth(formula = y ~ x, se = TRUE, color = "#045A8D") +
    labs(title = paste0("Probability vs. Number of ", 
                        str_to_title(str_replace(str_remove(str_remove(col, "num_"),"number_"),"_", " "))),
         x = paste0("Number of ", str_to_title(str_replace(str_remove(str_remove(col, "num_"),"number_"),"_", " "))),
         y = "Probability") +
    guides(fill = guide_legend(title = "Gender")) +
    theme_linedraw()
  
  ggsave(paste0("probVS", str_remove(col, "num_"), ".jpg"))
}

# End
