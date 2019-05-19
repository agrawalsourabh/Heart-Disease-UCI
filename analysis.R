# Adding Libraries
library(ggplot2)
library(caret)

# Importing Data

our.data = read.csv("heart.csv")
str(our.data)
head(our.data)

# Check missing values

count_na = function(x){
  na_varname = c()
  na_count = c()
  na_perc = c()
  
  for (i in 1:ncol(x)) {
    
    if(sum(is.na(x[i])) >  0){
      varname = colnames(x[i])
      count = sum(is.na(x[i]))
      perc = count/nrow(x) * 100
      perc = round(perc, digits = 2)
      
      na_varname = c(na_varname, varname)
      na_count = c(na_count, count)
      na_perc = c(na_perc, perc)
    }
    
    
  }
  
  missing_table = data.frame(na_varname, na_count, na_perc)
  return(missing_table)
}

count_na(x = our.data)

# so, our data contains 0 missing values

# factors our data

# sex
str(our.data$sex)

our.data$sex = as.factor(our.data$sex)

# plot graph to check the distribution of sex, using bar plot
ggplot(data = our.data, mapping = aes(x = sex)) +
  geom_bar(fill = "#6fed93") +
  labs(title = "Sex Distribution", xlab = "Sex")

# Chest Pain CP
str(our.data$cp)
table(our.data$cp)

our.data$cp = as.factor(our.data$cp)

# Plot graph to check the distribution of Chest Pain CP using bar plot
ggplot(data = our.data, mapping = aes(x = cp)) +
  geom_bar(fill = "#ea954f") +
  labs(title = "CP Distribution", xlab = "CP")

# resting electrocardiographic results restecg
str(our.data$restecg)
table(our.data$restecg)

our.data$restecg = as.factor(our.data$restecg)

# Plot graph to check the distribution of resting electrocardiographic results restecg using bar plot
ggplot(data = our.data, mapping = aes(x = restecg)) +
  geom_bar(fill = "#d8e03c") +
  labs(title = "RestECG Distribution", xlab = "Rest ECG")

# Exercise Induced Anigma exang
str(our.data$exang)
table(our.data$exang)

our.data$exang = as.factor(our.data$exang)

# Plot graph to check the distribution of Exercise Induced Anigma exang using bar plot
ggplot(data = our.data, mapping = aes(x = exang)) +
  geom_bar(fill = "#33c4c6") +
  labs(title = "exang Distribution", xlab = "Exang")

# ca
str(our.data$ca)
table(our.data$ca)

our.data$ca = as.factor(our.data$ca)

# Plot graph to check the distribution of Exercise Induced Anigma exang using bar plot
ggplot(data = our.data, mapping = aes(x = ca)) +
  geom_bar(fill = "#2f6566") +
  labs(title = "ca Distribution", xlab = "ca")

# thal
str(our.data$thal)
table(our.data$thal)

our.data$thal = as.factor(our.data$thal)

# Plot graph to check the distribution of Exercise Induced Anigma exang using bar plot
ggplot(data = our.data, mapping = aes(x = thal)) +
  geom_bar(fill = "#b3e4e5") +
  labs(title = "thal Distribution", xlab = "thal")

# target
str(our.data$target)
table(our.data$target)

our.data$target = as.factor(our.data$target)

# Plot graph to check the distribution of Exercise Induced Anigma exang using bar plot
ggplot(data = our.data, mapping = aes(x = target)) +
  geom_bar(fill = "#132f30") +
  labs(title = "target Distribution", xlab = "target")

# =======================================================================
#   SAMPLING THE DATA INTO TRAINING AND TEST SET
# =======================================================================

indexs = createDataPartition(our.data$target, 
                             p = 0.7, 
                             times = 1, 
                             list = F)

trd = our.data[indexs, ]
tsd = our.data[-indexs, ]

# ========================================================================
#   FITTING MODEL
# ========================================================================

# Setup CARER to do 10 cross validation repeated 3 times and uses grid search
# for optimal model

train.control = trainControl(method = "repeatedcv", 
                             number = 10, 
                             repeats = 3, 
                             search = "grid")

caret.cv = train(target ~.,
                 data = trd, 
                 trcontrol = train.control, 
                 method = "xgbTree")

caret.cv

# Predicting the target value for test data
our.predict = predict(caret.cv, tsd)

# Confusion Matrix
confusionMatrix(our.predict, tsd$target)


