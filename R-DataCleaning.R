######## ----------  INFORMATION VISUALIZATION  ---------------  ############
######## ----------  PORJECT CODE  ---------------  ############
######## ----------  GROUP 3 -----------  ##########
######## Aarjav Jain

df = read.csv(file.choose(), sep = ",", header = TRUE, na.strings = "")
str(df1)
na.error = which(!complete.cases(df1))
df1 = df1[-na.error,]

options(scipen = 10)
boxplot(df1$AMT_CREDIT~df1$NAME_CONTRACT_TYPE)
head(df1$AMT_CREDIT)
t_type.of.loan = table(df1$NAME_CONTRACT_TYPE)

### Data Cleaning
## Removing columns 23 to 28 and 42:95
c1 = c(23:28, 42:95)
credit = df[, -c1]

## Consolidating faulty address columns into one
credit$FAULTY.ADD = rep(0)

for (i in 1:nrow(df))
{
 s[i] = sum(df[i,35:40])
 if (s[i] > 0) 
   {credit$FAULTY.ADD[i] = 1}
 else { credit$FAULTY.ADD[i] = 0}
}
head(s)
summary(credit$FAULTY.ADD)
head(credit$FAULTY.ADD)

## Removing columns 29:34
head(credit[1:5,29:34])
credit = credit[,-c(29:34)]

## Consolidating Flag Document columns 
credit$FLAG_DOC = rep(0)
head(df[,97:116])

for (i in 1:nrow(df))
{
  s[i] = sum(df[i,97:116])
}

credit$FLAG_DOC = s
credit$FLAG_DOC

# removing columns 31:50
head(credit[1:5, 31:50])
summary(credit[,31:50])
credit = credit[,-c(31:50)]

## Removing columns 31:36
head(credit[,31:36])
credit = credit[,-c(31:36)]


## Removing columns 25:26
credit = credit[,-c(25,26)]

## Removing column 1 - ID
credit = credit[,-1]

######### --------------  CLEANING COMPLETE  --------------- ##########
# Final dataset - credit 
str(credit)
dim(credit) # 307511 records and 30 variables


########## --------------  Creating Dummy Variables --------------- ################
credit.dummys = credit
credit.dummys = credit.dummys[,-1]
install.packages("fastDummies")
library(fastDummies)
credit.dummies = dummy_cols(credit.dummys, select_columns = c("NAME_CONTRACT_TYPE", "CODE_GENDER", "FLAG_OWN_CAR", "FLAG_OWN_REALTY", 
                                                              "NAME_TYPE_SUITE", "NAME_INCOME_TYPE", "NAME_EDUCATION_TYPE", "NAME_FAMILY_STATUS",
                                                              "NAME_HOUSING_TYPE", "OCCUPATION_TYPE", "WEEKDAY_APPR_PROCESS_START", "ORGANIZATION_TYPE"), 
                            ignore_na = TRUE, remove_selected_columns = TRUE)

### Removing additional redundant variables
credit.dummies = credit.dummies[,-c(1,9,11,12)]
credit.dummies = credit.dummies[,-c(9,11)]
credit.dummies$NAME_FAMILY_STATUS_Unknown  = NULL ## because it had null values

# Columns = 135, Rows = 


########## --------------  Normalizing the Dataset --------------- ################ RUN THIS PART
z = credit.dummies[,3]
install.packages("normalr")
library(normalr)
install.packages("clusterSim")
library(clusterSim)
b = normalize(a, method = "standardize")


######### --------------  Performing Logistic Regression --------------- ################
install.packages("caret", dependencies = TRUE)
install.packages("rlang", type = "source")
install.packages("arm", dependencies = TRUE)
library(caret, ggplot2)
library(arm)
a = train(TARGET ~ ., data = credit.dummies, method = "bayesglm", preProcess = c("center", "scale"), na.action = na.pass)
b = summary(a)

c = coefficients(b)
write.csv(c, file = "model.csv")
coefficients(b)
b
summary(a)

######### -------------- Inserting the Logistic Regression Output -------- ################
logis_output = read.csv(file = "model.csv", sep = ",", header = TRUE, na.strings = "") ## AS dataframe

model_signi = model[3:12,]
DM_heatmap = model_signi[,1:2]
DM_heatmap = t(DM_heatmap)
t(model_signi)
boxplot(model_signi$Estimate~model_signi$X, ylim = c(-0.05, 0.06))
install.packages("dotwhisker", dependencies = TRUE)
library(dotwhisker)

plot(model_signi$Estimate)
######## --------------  HEAT MAP ------------------ ###############
install.packages("ggplot2", "gplots")
library(ggplot2, gplots)
logis_output_DM = data.matrix(DM_heatmap)
heatmap.2(logis_output_DM, main = "Heatmap", trace = "none", margins = c(10,12))
heatmap(logis_output_DM)


######### ----------------- Other Plots --------------- ################

barplot(credit$TARGET)
