# Data Preprocessing Template

# Importing the dataset
dataset = read.csv('50_Startups.csv')
dataset$State = factor(dataset$State, levels=c('New York','California','Florida'),labels=c(1,2,3))
View(dataset)

# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Profit, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
View(test_set)
View(training_set)
regressor = lm(formula = Profit ~ . ,
               data = training_set)
y_pred = predict(regressor, newdata = test_set)



#building model using backward elimination

regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend + State, data = dataset)
summary(regressor)
regressor = lm(formula = Profit ~ R.D.Spend + Administration + Marketing.Spend , data = dataset)
summary(regressor)
regressor = lm(formula = Profit ~ R.D.Spend + Marketing.Spend, data = dataset)
summary(regressor)
regressor = lm(formula = Profit ~ R.D.Spend , data = dataset)
summary(regressor)

#plot(training set)
library(ggplot2)
ggplot() +
  geom_point(aes(x = training_set$R.D.Spend, y = training_set$Profit),
             colour = 'red') +
  geom_line(aes(x = training_set$R.D.Spend, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('RD Spend v/s Profit (Training set)') +
  xlab('R.D Spend') +
  ylab('Profit')

#plot(test set)
ggplot() +
  geom_point(aes(x = test_set$R.D.Spend, y = test_set$Profit),
             colour = 'red') +
  geom_line(aes(x = training_set$R.D.Spend, y = predict(regressor, newdata = training_set)),
            colour = 'blue') +
  ggtitle('RD Spend v/s profit (Test set)') +
  xlab('RD') +
  ylab('Profit')