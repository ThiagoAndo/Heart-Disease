
library(readr)         # read data

library(stringi)       # string manipulation

library(stringr)       # string manipulation

library(data.table)    # data manipulation

library(dplyr)         # data manipulation

library(ggplot2)       # visualization

library(ggthemes)      # visualization

library(caret)         # confusin matrix

library(visdat)        # visualization of missing data 

library(skimr)         # data summary

library(pROC)          # model validation

library(randomForest)  # Macchine Learning

library(gbm)           # Macchine Learning




# Loading the data, variables names, and categorical descriptions
files <- dir("data", full = T)
names(files) <- gsub("\\.csv|\\.txt", "", dir("data"))


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


#=================================== (EDA) ====================================================


glimpse(heart_named)

summary(heart_named)


heart_named %>%
        is.na() %>% 
        colSums()


# Visualize all of the missingness in the `riskfactors`  dataset
vis_miss(heart_named)


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




ggplot(heart_data_2, aes(age, blood_pressure, color = condition))+ 
   geom_point(size = 4, alpha= 0.6)+
   scale_color_manual(values = c("#A0A0A0", "black"))+
   scale_fill_manual(values = c("#808080", "#B3B6B7"))+
   facet_grid(cols = vars(sex))+
   geom_smooth(method = "lm",se =FALSE )+
   theme_minimal()  +
   ggtitle("Relationship Between Age and Blood Pressure")+
   theme(
      axis.text.x=element_text( color="black"),
      plot.title = element_text(size = 17),
      strip.text.x = element_text(size = 14))





ggplot(heart_data_2, aes(age, heart_rate, color = condition))+
   geom_point(size = 4, alpha= 0.6)+
   scale_color_manual(values = c("#A0A0A0", "black"))+
   scale_fill_manual(values = c("#black", "#black"))+
   facet_grid(cols = vars(sex))+
   geom_smooth(method = "lm",se =FALSE )+
   theme_minimal()  +
   ggtitle("Relationship Between Age and Blood Pressure")+
   theme(
      axis.text.x=element_text( color="black"),
      plot.title = element_text(size = 17),
      strip.text.x = element_text(size = 14))



ggplot(heart_data_2, aes(heart_rate , blood_pressure, color = condition))+
   geom_point(size = 4, alpha= 0.6)+
   scale_color_manual(values = c("#A0A0A0", "black"))+
   scale_fill_manual(values = c("#black", "#black"))+
   facet_grid(cols = vars(sex))+
   geom_smooth(method = "lm",se =FALSE )+
   theme_minimal()  +
   ggtitle("Relationship Between Age and Blood Pressure")+
   theme(
      axis.text.x=element_text( color="black"),
      plot.title = element_text(size = 17),
      strip.text.x = element_text(size = 14))




ggplot(heart_data_2, aes(heart_rate , oldpeak, color = condition))+
   geom_point(size = 4, alpha= 0.6)+
   scale_color_manual(values = c("#A0A0A0", "black"))+
   scale_fill_manual(values = c("#black", "#black"))+
   facet_grid(cols = vars(sex))+
   geom_smooth(method = "lm",se =FALSE )+
   theme_minimal()  +
   ggtitle("Relationship Between Age and Blood Pressure")+
   theme(
      axis.text.x=element_text( color="black"),
      plot.title = element_text(size = 17),
      strip.text.x = element_text(size = 14))






#============================================================================================
heart_data_2 <- heart_named %>%
   select(chestpain,thal,major_vessel,heart_rate,oldpeak,blood_pressure,
          condition,age,sex)



n <- nrow(heart_data_2)

n_train <- round(0.74 * n)

set.seed(754)
train_indices <- sample(1:n, n_train)

train <- heart_data_2[train_indices, ]

test  <- heart_data_2[-train_indices, ]

#============================================================================================


set.seed(754)  
model <- tuneRF(x = subset(train, select = -condition),
                y = train$condition,
                ntreeTry = 500,
                doBest = TRUE)


model$confusion[c(1,2),c(1,2)]


round((1 - (sum(model$confusion[c(2,3)]) / sum(model$confusion[c(1,2,3,4)]))),2)


set.seed(754)
test$target_pred <- predict(object = model, 
                            newdata = test)


confusionMatrix(data = as.factor(test$target_pred),         
                reference = as.factor(test$condition))

cm$overall[1]
 
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
# Train a Logistic Regression model
model <- glm(condition ~ ., 
             data = train, family = "binomial")


set.seed(754)
# Generate predictions on the test set
test$target_pred  <- predict(model ,newdata = test, 
                             type = "response")

test$target_pred <- ifelse(test$target_pred >= 0.504,"disease", "no_disease")


confusionMatrix(data = as.factor(test$target_pred),         
                reference = as.factor(test$condition))



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




ROC <- suppressMessages(roc(ifelse(test$target_pred == "disease", 1,0),
                            ifelse(test$condition   == "disease", 1,0)))

# Plot the ROC curve
plot(ROC, col = "black")

# Calculate the area under the curve (AUC)
auc(ROC)


#============================================================================================

heart_data_2 <- heart_named %>%
                select(chestpain,thal,major_vessel,heart_rate,oldpeak,blood_pressure,
                       condition,age)


n <- nrow(heart_data_2)

n_train <- round(0.75 * n)

set.seed(754)
train_indices <- sample(1:n, n_train)

train <- heart_data_2[train_indices, ]

test <-  heart_data_2[-train_indices, ]

train$condition <- ifelse(train$condition == "disease", 1,0)


## Train a 10000-tree GBM model
set.seed(754)

model <- gbm(formula = condition ~ ., 
                    distribution = "bernoulli", 
                    data = train,
                    n.trees = 10000)



set.seed(754)
#==================== EARLY STOPPING IN GBMs
# Optimal ntree estimate based on OOB
ntree_opt_oob <- gbm.perf(object = model, 
                          method = "OOB", 
                          oobag.curve = TRUE,
                          plot.it = FALSE)




#==================== PREDICTING USING A (GBM) MODEL
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

ROC <- suppressMessages(roc(ifelse(test$target_pred == "disease", 1,0),
                            ifelse(test$condition   == "disease", 1,0)))

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







































