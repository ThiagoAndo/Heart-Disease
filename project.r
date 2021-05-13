
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# These are some packages that probably you have to install on your machine to run the 
# following code 

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

library(readr)  # read_delim()

library(stringi)# stri_read_lines()

library(stringr)# str_split()

library(dplyr)

library(ggplot2)

library(ggthemes)

library(pROC)

library(visdat) #vis_miss()

library(skimr) #skim()

library(data.table) #as.data.table(a)

library(randomForest)

library(caret)

library(gbm)

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Bro, I made  set_categorical_external to read and set the right categorical
# variables in the respectves places I also put the variables and factor names on two external
# files caled col_names.txt and factors.txt to make the code more compact these two files
# have to be in the same folder of prokecttt.r.

# I wrote a function called internal_function that is in (Halit\outhers\internal_function.txt)
# this function has the factors name on it and you can replace set_categorical_external 
# with it, if you prefer.

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
files <- dir("data", full = T)

names(files) <- gsub("\\.csv|\\.txt", "", dir("data"))

heart_data <- read.csv(files["heart_cleveland_upload"])

set_categorical_external <- function(){
   
   col_names <- str_split(stri_read_lines(files["col_names"])," ")
   names(col_names) <- c("var_name", "fac_name")
   level <- str_split(stri_read_lines(files["factors"])," ")
   heart_data <- read.csv(files["heart_cleveland_upload"], col.names = col_names[["var_name"]])
   heart_data2 <- as.data.table(heart_data)
 
   for(i in seq_along(level)){
      values <-  heart_data[ ,col_names[["fac_name"]][i] , drop = T] 
      
      heart_data2[, col_names[["fac_name"]][i] := factor(values,levels = sort(unique(values)),
                             labels = level[[i]])][,"blood_sugar" :=
                                                      ifelse(heart_data2$blood_sugar==1,
                                                             TRUE, FALSE)]
   }
   return(as.data.frame(heart_data2))
}

heart_named <- set_categorical_external()


#=================================== EDA ====================================================



#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# Here I made Exploratory Data analysis as it is required on the statement of the project
# run the plots and check out the patterns on the dataset that it revels 

#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

glimpse(heart_named)

summary(heart_named)


heart_named %>%
        is.na() %>% 
        colSums()


# Visualize all of the missingness in the `riskfactors`  dataset
visdat::vis_miss(heart_named)


# This skim function shows a summary of all variables in the data set 
skim(heart_named)


ggplot(heart_named, aes(x=condition, fill=condition)) + 
   geom_bar(stat="count") +
   geom_text(aes(label = ..count..), stat = "count", vjust = 1.5, colour = "white",
             fontface = "bold",size = 7)+
   theme_minimal() +
   scale_fill_manual(values =c("#808080", "#B3B6B7"))+
   ggtitle("Count of Presence and Absence of Heart Disease") +
   xlab("Heart Disease") +
   ylab("Count")+
   theme(
      axis.text.y = element_blank(),
      axis.text.x =  element_text(size = 15, color="black") ,
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      plot.title = element_text(size = 17),
      legend.position = "none") 

   
ggplot(heart_named, aes(sex, fill = sex))+
   geom_bar(stat="count") +
   geom_text(aes(label = ..count..), stat = "count",  vjust = 1.5, colour = "white",
             fontface = "bold",size = 7)+
   facet_grid(cols = vars(condition),labeller = label_both)+
   theme_minimal()  +
   scale_fill_manual(values = c("#808080", "#B3B6B7"))+
   ggtitle("Heart Diseases Frequency for Sexes")+
   theme(
      axis.text.y = element_blank(),
      axis.text.x =  element_text(size = 12, color = "black") ,
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      strip.text.x = element_text(size = 14),
      plot.title = element_text(size = 17),
      legend.position = "none") 

                  
     
                       
ggplot(heart_named,aes(age, fill= condition)) +
   geom_histogram(aes(y=..density..),breaks=seq(29, 80, by=1), color="grey17") +
   facet_wrap(~condition, ncol=1,scale="fixed") +
   theme_clean()+
   scale_fill_manual(values =c("#808080", "#B3B6B7"))+
   scale_x_continuous(breaks=seq(29, 80, by=5))+
   xlab("Age") +
   ylab("Density") +
   ggtitle("Age Histogram")+
   theme(
         plot.title = element_text(size = 17),
         strip.text.x = element_text(size = 14),
         legend.position = "none") 



ggplot(heart_named,aes(blood_pressure, fill=condition)) +
   geom_histogram(aes(y=..density..),binwidth = 1, color="grey17") +
   geom_density(alpha=.1, fill="black")+
   facet_wrap(~condition, ncol=1,scale="fixed") +
   scale_x_continuous(breaks=seq(94.0, 200.0, by=10))+
   theme_clean()   +
   scale_fill_manual(values = c("#808080", "#B3B6B7")) +
   xlab("Blood Pressure") +
   ylab("Density") +
   ggtitle("Resting Blood Pressure (in mm Hg on admission to the hospital)")+
   theme(
      plot.title = element_text(size = 17),
      strip.text.x = element_text(size = 14),
      legend.position = "none") 



ggplot(heart_named, aes(y = heart_rate, fill =condition))+
   geom_boxplot()+
   scale_fill_manual(values = c("#808080", "#B3B6B7")) +
   scale_y_continuous(breaks=seq(70.0, 200.0, by=30))+
   theme_minimal()+
   ggtitle("Resting Blood Pressure (in mm Hg on admission to the hospital)")+
   theme(
      plot.title = element_text(size = 17)) 



ggplot(heart_named,aes(chestpain, fill=condition)) +
   geom_bar(stat="count") +
   geom_text(aes(label = ..count..), stat = "count", vjust = -.5, colour = "black",
             fontface = "bold" )+
   facet_grid(cols = vars(condition), labeller = label_both)+
   theme_minimal()  +
   scale_fill_manual(values = c("#808080", "#B3B6B7"))+
   ggtitle("Bar Chart of Chest Pain Type for Condition")+
   theme(
      axis.text.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x=element_text(angle=35, hjust=1, vjust=1.2, color="black"),
      plot.title = element_text(size = 17),
      strip.text.x = element_text(size = 14),
      legend.position = "none")



ggplot(heart_named, aes( heart_rate, fill =condition))+
   geom_density(alpha= .5)+
   scale_fill_manual(values = c("#404040", "#B3B6B7")) +
   theme_minimal()+
   xlab("Resting Blood Pressure (in mm Hg on admission to the hospital)") +
   ggtitle("Distributions of Blood Pressure")



ggplot(heart_named,aes(thal, fill=condition)) +
   geom_bar(stat="count") +
   geom_text(aes(label = ..count..), stat = "count", vjust = -.5, colour = "black",
             fontface = "bold")+
   facet_grid(cols = vars(condition), labeller = label_both)+
   theme_minimal()  +
   scale_fill_manual(values = c("#808080", "#B3B6B7"))+
   ggtitle("Blood Disorder Thalassemia")+
   theme(
      axis.text.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x=element_text(angle=35, hjust=1, vjust=1.2, color="black"),
      plot.title = element_text(size = 17),
      strip.text.x = element_text(size = 14),
      legend.position = "none")


ggplot(heart_named,aes(exer_induc_angina, fill=condition)) +
   geom_bar(stat="count") +
   geom_text(aes(label = ..count..), stat = "count", vjust = -.3, colour = "black",
             fontface = "bold", size = 5)+
   facet_grid(cols = vars(condition))+
   theme_minimal()  +
   scale_fill_manual(values = c("#808080", "#B3B6B7"))+
   ggtitle("Exercise Induced Angina")+
   theme(
      axis.text.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x=element_text(size = 13, color="black"),
      plot.title = element_text(size = 17),
      strip.text.x = element_text(size = 14),
      legend.position = "none")


ggplot(heart_named, aes(y = oldpeak, fill =condition))+
   geom_boxplot()+
   scale_fill_manual(values = c("#808080", "#B3B6B7")) +
   theme_minimal()+
   ggtitle("Distribution of ST Depression Induced by Exercise Relative to Rest")  


ggplot(heart_named, aes( oldpeak, fill =condition))+
   geom_density(alpha= .5)+
   scale_fill_manual(values = c("#404040", "#B3B6B7")) +
   theme_minimal()+
   ggtitle("Distribution of ST Depression Induced by Exercise Relative to Rest")



ggplot(heart_named, aes(major_vessel, fill = condition))+
   geom_bar(stat="count") +
   geom_text(aes(label = ..count..), stat = "count", vjust = -.5, colour = "black",
             fontface = "bold")+
   facet_grid(cols = vars(condition))+
   theme_minimal()  +
   scale_fill_manual(values = c("#808080", "#B3B6B7"))+
   ggtitle("Heart Diseases Frequency for Number of major vessels")+
   theme(
      axis.text.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      axis.text.x=element_text( color="black"),
      plot.title = element_text(size = 17),
      strip.text.x = element_text(size = 14),
      legend.position = "none")

# CONCLUSION ================================================================================
# the patterns that are visible in this data set are people with chest pain called asymptomatic
# tend to have heart disease also that males are the majority with hart disease and age are 
# correlated with heart disease.

# it is possible to see strange phenomena in this data set people has an abnormal heart rate
# also, people how have heart disease are more likely to practice exercise and have a low 
# heart hate it may happen  they began to exercise after being diagnosed with heart 
# disease because they are in majority of people with a fixed defect and reversible defect.

# The variables in this data set that might be well explanatory variables are age, sex, 
# chest pain, blood pressure.
#============================================================================================

#============================================================================================
heart_data_2 <- heart_named %>%
   select(chestpain,thal,major_vessel,heart_rate,oldpeak,blood_pressure,
          condition,age,sex)

set.seed(754)

glimpse(heart_data_2)


n <- nrow(heart_data_2)

n_train <- round(0.74 * n)

train_indices <- sample(1:n, n_train)

train <- heart_data_2[train_indices, ]

test  <- heart_data_2[-train_indices, ]

#============================================================================================

#===========================================================================================


# In this exercise, you will use the n this exercise, you will use the randomForest::tuneRF()
# to tune mtry (by training several models). This function is a specific utility to tune the
# mtry parameter based on OOB error, which is helpful when you want a quick & easy way to 
# tune your model. A more generic way of tuning Random Forest parameters will be presented 
# in the following exercise.

# Execute the tuning process

# If you just want to return the best RF model (rather than results)
# you can set `doBest = TRUE` in `tuneRF()` to return the best RF model
# instead of a set performance matrix.

set.seed(754)  
model <- tuneRF(x = subset(train, select = -condition),
                y = train$condition,
                ntreeTry = 500,
                doBest = TRUE)


test$target_pred <- predict(object = model, 
                            newdata = test)


confusionMatrix(data = as.factor(test$target_pred),         
                reference = as.factor(test$condition))
 
 
 ROC <- roc(ifelse(test$target_pred == "disease", 1,0),
            ifelse(test$condition   == "disease", 1,0))
 
 # Plot the ROC curve
 plot(ROC, col = "blue")
 
 # Calculate the area under the curve (AUC)
 auc(ROC)

f <- table(test$condition,test$target_pred)
t.df<-as.data.frame(f)


ggplot(data = t.df, aes(x = Var1, y = Var2, label=Freq)) +
   geom_tile(aes(fill = Freq)) +
   scale_fill_gradient(low="#E0E0E0" , high= "#808080") +
   theme_minimal() +
   xlab("Actual Heart Disease") +
   ylab("Predicted Heart Disease") +
   geom_text(size=8) +
   ggtitle("Random Forest")


importance <- importance(model)

varImportance <- data.frame(variables = row.names(importance), 
                            importance = round(importance[1:nrow(importance)],2))

# Create a rank variable based on importance
rankImportance <- varImportance %>%
   mutate(rank = paste0('#',dense_rank(desc(importance))))

# Use ggplot2 to visualize the relative importance of variables
ggplot(rankImportance, aes(x = reorder(variables, importance), 
                           y = importance, fill = importance)) +
   geom_bar(stat='identity') + 
   geom_text(aes(x = variables, y = 0.5, label = rank),
             hjust=0, vjust=0.55, size = 4, colour = 'red') +
   labs(x = 'Variables') +
   coord_flip() + 
   theme_minimal()

#============================================================================================

set.seed(754)

model <- glm(condition ~ ., 
             data = train, family = "binomial")

test$target_pred  <- predict(model ,newdata = test, 
                             type = "response")

test$target_pred <- ifelse(test$target_pred >= 0.504,"disease", "no_disease")


confusionMatrix(data = factor(test$target_pred, levels = c("no_disease","disease")),         
                reference = as.factor(test$condition))


ROC <- roc(ifelse(test$target_pred == "disease", 1,0), 
           ifelse(test$condition   == "disease", 1,0))


# Plot the ROC curve
plot(ROC, col = "red")

# Calculate the area under the curve (AUC)
auc(ROC)

f <- table(test$condition,test$target_pred)
t.df<-as.data.frame(f)


ggplot(data = t.df, aes(x = Var1, y = Var2, label=Freq)) +
   geom_tile(aes(fill = Freq)) +
   scale_fill_gradient(low="#E0E0E0" , high= "#808080") +
   theme_minimal() +
   xlab("Actual Heart Disease") +
   ylab("Predicted Heart Disease") +
   geom_text(size=8) +
   ggtitle("Logistic Regression")

#============================================================================================
heart_data_2 <- heart_named %>%
                select(chestpain,thal,major_vessel,heart_rate,oldpeak,blood_pressure,
                       condition,age)

set.seed(754)

n <- nrow(heart_data_2)

n_train <- round(0.75 * n)

train_indices <- sample(1:n, n_train)

train <- heart_data_2[train_indices, ]

test <-  heart_data_2[-train_indices, ]

# Here you wil use the gbm() function to train a GBM classifier to predict loan default. You
# Will train a 10,000 tree GB< ib tge credut_train dataset, which is preloaded into your 
# workspace.
# Using such a large number of trees (10,000) is probably not optimal for a GBM model, but 
# we will build more trees than we need and then select the optimal number of trees based 
# on early performance-based stopping. The best GBM model will likely contain fewer trees 
# than we started with.

# For binary classification, gbm() requires the response to be encoded as 0/1 (numeric), so
# we will have to convert from a "no/yes" factor to a 0/1 numeric response column.

# Also, the the gbm() function requires the user to specify a distribution argument. For a 
# binary classification problem, you should set distribution = "bernoulli". The Bernoulli 
# distribution models a binary response
# Convert "yes" to 1, "no" to 0

train$condition <- ifelse(train$condition == "disease", 1,0)
#
## Train a 10000-tree GBM model
set.seed(754)

model <- gbm(formula = condition ~ ., 
                    distribution = "bernoulli", 
                    data = train,
                    n.trees = 10000)



#==================== EARLY STOPPING IN GBMs
# Optimal ntree estimate based on OOB
ntree_opt_oob <- gbm.perf(object = model, 
                          method = "OOB", 
                          oobag.curve = TRUE)




#==================== PREDICTING USING A (GBM) MODEL

# The gbm package uses predict() function to generate predictions from a model, similar to 
# many other machine learning packages in R. Whn you see a function like predict() packages
# that works on many different types of input GBM moodel, , a RF model, a GLM model, etc), 
# that indicates that predict() is an "alias" for a GBM-specific version of that function. 
# The GBM specific version of that function is predict.gbm(), but for convenience sake, we 
# can just use predict() (either works).

# One thing that's particular to the predict.gbm() however, is that you need to specify the
# number of trees used in the prediction. There is no default, so you have to specify this 
# manually. For now, we can use the same number of trees that we specified when training 
# the model, which is 10,000 (though this may not be the optimal number to use).

# Another argument that you can specify is type, which is only relevant to Bernoulli and 
# Poisson distributed outcomes. When using Bernoulli loss, the returned value is on the log
# odds scale by default and for Poisson, it's on the log scale. If instead you specify type
# = "response", then gbm converts the predicted values back to the same scale as the outcome
# This will convert the predicted values into probabilities for Bernoulli and expected 
# counts for Poisson.

# Since we converted the training response col, let's also convert the test response col
test$condition<- ifelse(test$condition == "disease", 1,0)

set.seed(754)
# Generate predictions on the test set (scale to response)
test$target_pred <- predict(object  =  model, 
                            newdata = test,
                            n.trees = ntree_opt_oob,
                            type = "response")


test$target_pred <- ifelse(test$target_pred >= 0.504, "disease", "no_disease")

test$condition   <- ifelse(test$condition == 1, "disease", "no_disease")


confusionMatrix(data = factor(test$target_pred, levels = c("no_disease","disease")),         
                reference = as.factor(test$condition))

ROC <- roc(ifelse(test$target_pred == "disease", 1,0),
           ifelse(test$condition   == "disease", 1,0))

# Plot the ROC curve
plot(ROC, col = "red")

# Calculate the area under the curve (AUC)
auc(ROC)


f <- table(test$condition,test$target_pred)
t.df<-as.data.frame(f)


ggplot(data = t.df, aes(x = Var1, y = Var2, label=Freq)) +
   geom_tile(aes(fill = Freq)) +
   scale_fill_gradient(low="#E0E0E0" , high= "#808080") +
   theme_minimal() +
   xlab("Actual Heart Disease") +
   ylab("Predicted Heart Disease") +
   geom_text(size=8) +
   ggtitle("GBM Model")







































