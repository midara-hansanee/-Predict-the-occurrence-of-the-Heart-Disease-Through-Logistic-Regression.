#install.packages("party")
#install.packages("ggplot2")
#install.packages("GGally")
#install.packages("tidyverse")
#install.packages("corrplot")
#install.packages("RColorBrewer")
library(party)
library(epitools)
library(ggplot2)
library(GGally)
library(tidyverse)
library(corrplot) 
library(RColorBrewer)

Heart<-read.csv("C:\\Users\\Midara\\Downloads\\regression\\heart.csv")

data(Heart) #loadng  diabetes dataset


# Discover the parameteres of the data frame
summary(Heart)
dim(Heart)
str(Heart)
head(Heart)

#convert dependent variable (Class.Variable) to factor
Heart$target<- as.factor(Heart$target)
#build new dataset with selected variables
Heart <- Heart [, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14)]
head(Heart )
View(Heart )
str(Heart)

#Explore all the variables in the research dataset
#cor() function compute the variance of x and the covariance or correlation of x and y if these are vectors. If x and y are matrices then the covariances (or correlations) between the columns of x and the columns of y are computed.

Heart_cor<-cor(Heart [,-14])
View(Heart_cor)

#Visualize Correlation Matrix using Correlogram
#Correlogram is a graph of correlation matrix. Useful to highlight the most correlated variables in a data table. In this plot, correlation coefficients are colored according to the value. Correlation matrix can be also reordered according to the degree of association between variables

corrplot(Heart_cor, type="upper", order="hclust", col=brewer.pal(n=4, 
                                                                 name="RdYlBu"))
#The correlation coefficient is interpreted as:
#If ρXY=1, then X and Y are perfectly, positively, linearly correlated.
#If ρXY=−1, then X and Y are perfectly, negatively, linearly correlated.
#If ρXY=0, then X and Y are completely, un-linearly correlated. 
#If ρXY>0, then X and Y are positively, linearly correlated, but not perfectly so.
#If ρXY<0, then X and Y are negatively, linearly correlated, but not perfectly so.

#ggpairs() function is a special form of a function that produces a pairwise comparison of multivariate data. 
#By default, ggpairs() function provides two different comparisons of each pair of columns and displays either the density or count of the respective variable along the diagonal. 
#With different parameter settings, the diagonal can be replaced with the axis values and variable labels.

#Plot 2

ggpairs(data=Heart , title="Heart_data")

#Plot 

ggpairs(data=Heart,mapping=aes(color=target),title="Heart_data")
#Plot 3
#ggscatmat is similar to ggpairs but only works for purely numeric multivariate data. It is faster than ggpairs, because less choices need to be made. It creates a matrix with scatterplots in the lower diagonal, densities on the diagonal and correlations written in the upper diagonal.

ggscatmat(data=Heart,color="target",alpha=0.8)



#model 1 - Building a linear regression model with age and trestbps variables
lm1 <- lm( age~ trestbps, data = Heart)
summary(lm1)

ggplot(Heart, aes(x =age, y =trestbps)) + 
  geom_point() + 
  geom_smooth(method = "lm", color = "red")


#model 2 -Building a linear regression model with age vs chol
lm1 <- lm( age~ chol ,data = Heart)
summary(lm1)


#Plot linear regression model


ggplot(Heart, aes(x =age, y = chol)) + 
  geom_point() + 
  geom_smooth(method = "lm", color = "red")

#Now we will divide our sample into 70% Training and 30% Validation parts.
# Load the Heart Disease dataset
# Set the seed for reproducibility
set.seed(42)

# Generate a vector to assign each row to either training or validation set
pd <- sample(2, nrow(Heart), replace = TRUE, prob = c(0.75, 0.25))

# Create the training set
train <- Heart[pd == 2, ]
head(train)

# Create the validation set
validate <- Heart[pd == 2, ]
head(validate)


#Lets build a logistic regression model to check whether we can predict a person has CHD for given variables.
#Creating a logistic regression model should look very similar to creating a linear regression model. 
#However, instead of lm() we use glm(). Also, note that we must specify family = "binomial" for a binary classification context.


#Model 3 - targe

model_glm <- glm( target~age+sex+cp+trestbps+chol+restecg+thalach+thal,
                  data = Heart, family = "binomial")
summary(model_glm) 

# Predict probabilities using the model
Heart$predicted_prob <- predict(model_glm, type = "response")

# Visualize the predictions
Heart %>%
  ggplot(aes(x = thalach, y =predicted_prob )) +
  geom_point(alpha = 0.15) +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  ggtitle("Logistic Regression Model Fit") +
  xlab("Maximum Heart Rate (thalach)") +
  ylab("Predicted Probability of Heart Disease")

#Logistic Regression Model Prediction
#First of all we should understand how to use the predict() function with glm(). 
#In order to return probabilities, we must specify type = "response". 
#As mentioned before, these predicted values are probabliliites, not classifications. 
#We must “manually” convert the probabilities to classifications. Traditionally, a midpoint value such as 0.5 is used to “categorize” the probabilities.

trn_pred <- ifelse(predict(model_glm, type = "response") >0.8, "1", "0")
trn_pred
#Logistic Regression Model Evaluation
#One of the best way evaluate a classification models is to compare the actual response values with the predicted ones using a cross-table, which is often called a confusion matrix. 
#This matrix can be generated with the base table() function.
#Making predictions on the train dataset.
trn_tab <- table(predicted = trn_pred, actual = train$target) 
trn_tab 

#Model Evaluation
sum(diag(trn_tab))/sum(trn_tab)
#Making predictions on the validate dataset.

tst_pred <- ifelse(predict(model_glm, newdata = validate, type = 
                             "response") > 0.5, "1", "0")
tst_tab <- table(predicted = tst_pred, actual = validate$target)
tst_tab
#Model Evaluation
sum(diag(tst_tab))/sum(tst_tab)
# Model 4 - Lets build a logistic regression model to check whether we can predict a person has CHD for given all the independent variables.
model_glm <- glm(target~ cp, data = train, family = "binomial")
summary(model_glm)
#We must “manually” convert the probabilities to classifications.
trn_pred <- ifelse(predict(model_glm, type = "response") >0.5, "1", "0")
#Making predictions on the train set.
trn_tab <- table(predicted = trn_pred, actual = train$target)
trn_tab
#Model Evaluation
sum(diag(trn_tab))/sum(trn_tab)


