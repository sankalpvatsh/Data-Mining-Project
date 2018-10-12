library(ggplot2)
library(dplyr)
library(devtools)
library(modelr)
library(tidyr)
library(corrplot)
library(lattice)
library(caTools)
library(dummies)
library(fastDummies)
library(class)
library(readxl)
library(caret)
library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(tidyverse) # lots of useful packages for transformation
library(ggthemes) # extra themes for ggplot2
library(stringr) # for pattern matching
library(repr)
library(plotly)
library(lift)

install.packages("lift")

mcds <- data.frame(read_csv('menu.csv'))
menu <- mcds[,-c(1,2,3)]
names(mcds)
mcds.category <- mcds[,c(1)]
View(menu)
View(mcds)
str(menu)

ggplot(menu) +
  geom_bar(aes(x = reorder(Category, table(menu)[Category])), fill = "lightblue") +
  coord_flip() +
  labs(x = NULL)

splom(~menu[c(4,6,11,13,15,17,19,20)], groups = NULL, data = menu, axis.line.tck = 0, axis.text.alpha = 0,col="blue")

cr <- cor(menu[c(4,6,11,13,15,17,19,20)])
corrplot(cr,method = "number")

set.seed(2) #to get the same split everytime
split <- sample.split(menu$Calories,SplitRatio = 0.60)
train <- subset(menu,split == "TRUE")
test <- subset(menu, split == "FALSE")
mcds.train <- subset(mcds.category,split == "TRUE")
mcds.test <- subset(mcds.category, split == "FALSE")

View(test)

plot(train$Total.Fat,train$Calories,main = "Scatter Plot",xlab = "Total Fat", ylab = "Calories")
abline(lm(train$Calories~train$Total.Fat),col=3)

#Conditional Expectation Plot
dataexp <- summarise(group_by(train,Total.Fat),calmean = mean(Calories))
plot(dataexp$Total.Fat,dataexp$calmean,xlab = "Total Fat",ylab = "mean - calories",main = "Conditional 
     Expectation(mean) Plot")

model <- lm(Calories~ Total.Fat + Protein + Carbohydrates, data = train)
summary(model)

plotLift(predictions)



menu%>%add_predictions(model) %>% ggplot(aes(x=Total.Fat + Protein + Carbohydrates)) + 
  geom_point(aes(y=Calories)) + geom_line(aes(y=pred,color='red'))

par(mfrow = c(2,2))

plot(model, which = 1:4)

View(train)

train <- train[-c(120,121,259),]
model1 <- lm(Calories~ Total.Fat + Protein + Carbohydrates, data = train)
summary(model1)

predictions <- predict(model,test)
predictions

View(train)

rmse(model1,train$Calories)

plot(test$Calories,type = "l",lty = 1.8, col="red")
lines(predictions,type = "l", col = "green")

predict(model1,data.frame(Total.Fat = 10,Protein = 35,Carbohydrates = 43))

gain <- gains(train$Calories, predictions, groups=length(predictions))
# plot lift chart
plot(c(0,gain$cume.pct.of.total*sum(train$Calories))~c(0,gain$cume.obs),
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(train$Calories))~c(0, dim(train)[1]), lty=2)

#knn
View(train)
View(test)
View(mcds.train)
View(mcd.test)
category.train <- mcds[c(1)]
View(category.train)
len(train)
knn.1<- knn(train, test, mcds.train, k=1)
View(knn.1)
knn.11 <- as.numeric(knn.1)
View(knn.11)

new.item <- c(20,10,11,1230,5,1112312,12123123,1,0,1,12,89,09,2,3,4,6,7,7,14,21)

knn.1_1<- knn(train, new.item, mcds.train, k=5)

knn.1_1

set.seed(400)
ctrl <- trainControl(method="repeatedcv",repeats = 10) #,classProbs=TRUE,summaryFunction = twoClassSummary)
knnFit <- train(Category ~ ., data = mcds, method = "knn", trControl = ctrl, preProcess = c("center","scale"), tuneLength = 20)
knnFit

gain <- gains(test$Calories, knn.11, groups=length(knn.11))
# plot lift chart
plot(c(0,gain$cume.pct.of.total*sum(test$Calories))~c(0,gain$cume.obs),
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(test$Calories))~c(0, dim(train)[1]), lty=2)

#CART

dataset.mcds<- mcds[ ,c("Calories.from.Fat","Total.Fat","Total.Fat....Daily.Value." ,
                        "Saturated.Fat",  "Saturated.Fat....Daily.Value.","Trans.Fat",                   
                         "Cholesterol" ,"Cholesterol....Daily.Value.",  
                         "Sodium" ,"Sodium....Daily.Value.",       
                         "Carbohydrates", "Carbohydrates....Daily.Value.",
                         "Dietary.Fiber","Dietary.Fiber....Daily.Value.",
                         "Sugars","Protein","Vitamin.A....Daily.Value.","Vitamin.C....Daily.Value.",    
                         "Calcium....Daily.Value.", "Iron....Daily.Value."     )]

# Data pre processing
set.seed(123)

mcds <- data.frame(read_csv('menu.csv'))
menu <- mcds[,-c(1,2,3)]
names(mcds)
mcds.category <- mcds[,c(1)]
View(menu)
View(mcds)
str(menu)
View(train)
View(test)

set.seed(2) #to get the same split everytime
split <- sample.split(menu$Calories,SplitRatio = 0.60)
train <- subset(menu,split == "TRUE")
valid <- subset(menu, split == "FALSE")

set.seed(7)
ss <- sample(1:3,size=nrow(menu),replace=TRUE,prob=c(0.5,0.3,0.2))
train <- menu[ss==1,]
validation <- menu[ss==2,]
test <- menu[ss==3,]

View(test)

#Code for running a random forest, plotting variable importance plot, and computing accuracy.
#Variable importance plot from Random forest (Personal Loan Example) (Section 9.9))
####
install.packages("randomForest")
library(randomForest)
library(rpart)
library(rpart.plot)
train.reg<- rpart(Calories ~ ., data = train, method = "anova", 
                  control = rpart.control(ntree = 100, cp= 0.001, mtry = 4,importance = TRUE))

printcp(train.reg)
plotcp(train.reg)
plot(train.reg, uniform=TRUE, 
     main="Regression Tree for Calories of Items")
text(train.reg, use.n=TRUE, all=TRUE, cex=.8)
prp(train.reg, type = 1, extra = 1, split.font = 1, varlen = -10)

summary(train.reg)

install.packages("ModelMetrics")
library(ModelMetrics)

train.pred<- predict(train.reg, train)
train.pred
x <- rmse(train.pred, train$Calories)
x


valid.pred<- predict(train.reg, validation)
valid.pred
y <- rmse(valid.pred, test$Calories)
y

test.pred <- predict(train.reg, test)
test.pred
z <- rmse(test.pred, test$Calories)
z

par(mfrow = c(1,2))
boxplot(y, col="green")
boxplot(z, col="yellow")



install.packages("binr")
library(binr)
library(dplyr)

predCalories <- data.frame( Calories.from.Fat=120,
                               Total.Fat=15,
                               Total.Fat....Daily.Value. =23,
                               Saturated.Fat=9,
                               Saturated.Fat....Daily.Value.=11,
                               Trans.Fat=1,                   
                               Cholesterol = 50,
                               Cholesterol....Daily.Value. =158, 
                               Sodium=900,
                               Sodium....Daily.Value.=36,       
                               Carbohydrates=50,
                               Carbohydrates....Daily.Value.=10,
                               Dietary.Fiber=10,
                               Dietary.Fiber....Daily.Value.=25,
                               Sugars=45,
                               Protein=54,
                               Vitamin.A....Daily.Value.=12,
                               Vitamin.C....Daily.Value.=34,
                               Calcium....Daily.Value.=43,
                               Iron....Daily.Value.=56
                               )

Caloriespred <- predict(train.reg, predCalories)
Caloriespred

gain <- gains(train$Calories, train.pred, groups=length(train.pred))
# plot lift chart
plot(c(0,gain$cume.pct.of.total*sum(train$Calories))~c(0,gain$cume.obs),
     xlab="# cases", ylab="Cumulative", main="", type="l")
lines(c(0,sum(train$Calories))~c(0, dim(train)[1]), lty=2)

heights <- gain$mean.resp/mean(valid.df$Personal.Loan)
midpoints <- barplot(heights, names.arg = gain$depth, ylim = c(0,9),
                     xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise lift chart")


install.packages("gains")
library(gains)

##Extra

menu <- read.csv("menu.csv", header=T) # Load the data
new_names <- gsub(pattern = "*....Daily.Value.", replacement=".DV", names(menu))
names(menu) <- new_names
View(menu2)

drinks.oz <- menu[str_detect(menu$Serving.Size, " fl oz.*"),]
drinks.ml <- menu[str_detect(menu$Serving.Size, 'carton'),]

#drinks - keep the numbers and convert ounces to mililiters (1 oz = 29.5735 ml)
#round the values to zero decimal places 
drinks.oz$Serving.Size <- 
  round(as.numeric(gsub(" fl oz.*", "", drinks.oz$Serving.Size))*29.5735,0)
drinks.ml$Serving.Size <- 
  round(as.numeric(gsub(".*\\((.*)\\ ml).*", "\\1", drinks.ml$Serving.Size)),0)

#food - select only fields that contain "g" string
#keep the numbers and round the values to zero decimal places
food.g <- menu[str_detect(menu$Serving.Size, 'g'),] 
food.g$Serving.Size <- 
  round(as.numeric(gsub(".*\\((.*)\\ g).*", "\\1", food.g$Serving.Size)),0)

#combine all those data frames by rows into new data frame
#create new column with Type of Item as either 'drink' or 'food'
menu2 <- rbind(drinks.oz,drinks.ml)
menu2$Type <- rep("drinks.ml", nrow(menu2))
food.g$Type <- rep("food.g", nrow(food.g))
menu2 <- rbind(menu2,food.g)

options(repr.plot.height=3, repr.plot.width=6)
ggplot(menu2) +
  geom_bar(aes(x = reorder(Category, table(Category)[Category])), fill = "lightblue") +
  coord_flip() +
  labs(x = NULL)

options(repr.plot.height=3, repr.plot.width=6)
ggplot(menu2, aes(x = Calories)) +
  geom_histogram(aes(y = ..density..), fill = "blue", binwidth = 40, color="gray") + 
  geom_density() +
  scale_x_continuous(breaks = seq(min(menu$Calories), max(menu$Calories), by = 200)) 


#Conditional density estimate - calories by category
options(repr.plot.height=4, repr.plot.width=6)
ggplot(menu2, aes(x = Calories, fill = Category)) +
  geom_density(position = "fill") +
  scale_fill_brewer() +
  labs(fill="")

View(menu2)

ggplot(menu2, aes(x = factor(Category), y = Carbohydrates....Daily.Value.)) +
  geom_violin(aes(fill = factor(Category))) + 
  geom_boxplot(width = 0.05) + 
  guides(fill=FALSE) +
  ggtitle("Violin plot -- Fulfillment of daily recommended carbs") + 
  labs(x = "Category", y = "% fulfillment")

ggplot(menu2, aes(x = factor(Category), y = Protein)) +
  geom_violin(aes(fill = factor(Category))) + 
  geom_boxplot(width = 0.05) + 
  guides(fill=FALSE) +
  ggtitle("Violin plot -- Fulfillment of daily recommended Proteins") + 
  labs(x = "Category", y = "% fulfillment")

ggplot(menu2, aes(x = factor(Category), y = Total.Fat)) +
  geom_violin(aes(fill = factor(Category))) + 
  geom_boxplot(width = 0.05) + 
  guides(fill=FALSE) +
  ggtitle("Violin plot -- Fulfillment of daily recommended Fat") + 
  labs(x = "Category", y = "% fulfillment")

options(repr.plot.height=4, repr.plot.width=6)
ggplot(menu2, aes(y = Calories, x = Serving.Size)) +
  geom_jitter(size=0.5) +
  stat_density2d() +
  geom_smooth(method='lm', se=F, colour=1) +
  facet_wrap( ~ Type, scales = "free_x") +
  theme(panel.border = element_rect(colour = 1, fill=NA))

1:5

c(1:5)


