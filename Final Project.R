#### Final Project R ####

library(readxl)
library(plotrix)
library(ggplot2)
library(caret)
library(janitor)
library(lattice)
library(cvms)
library(ggimage)
library(rsvg)


DataSet = read_excel("C:/Users/Administrator/Desktop/DataSet/McDonalds.xlsx")
### Load DataSet

########## Data processing ##########

duplicates = data.frame(duplicated(DataSet))
colnames(duplicates) = 'Is the value missing?'
### No duplicate values

FrequencyTable_Category = data.frame(table(DataSet$Category))
FrequencyTable_Category$FrequencyPercentage = round((FrequencyTable_Category$Freq/sum(FrequencyTable_Category$Freq))*100)
FrequencyTable_Category = FrequencyTable_Category [order(FrequencyTable_Category $Freq),]
colnames(FrequencyTable_Category) = c("Category","Frequency","Frequency Percentage")
add_precent = c(FrequencyTable_Category$`Frequency Percentage`)
FrequencyTable_Category$`Frequency Percentage` = paste(add_precent,"%",sep ="")
###### Frequency Table Category ##########

FrequencyTable_HarmfulDish = data.frame(table(DataSet$`Harmful Dish`))
FrequencyTable_HarmfulDish$Frequency_Percentage = round((FrequencyTable_HarmfulDish$Freq/sum(FrequencyTable_HarmfulDish$Freq))*100)
FrequencyTable_HarmfulDish = FrequencyTable_HarmfulDish [order(FrequencyTable_HarmfulDish $Freq),]
colnames(FrequencyTable_HarmfulDish) = c("Harmful Dish ","Frequency","Frequency Percentage")
add_precent = c(FrequencyTable_HarmfulDish$`Frequency Percentage`)
FrequencyTable_HarmfulDish$`Frequency Percentage` = paste(add_precent,"%",sep ="")
###### Frequency Table Healthy dishes ##########

FrequencyTable_SubCategory = data.frame(table(DataSet$`Sub Category`))
FrequencyTable_SubCategory$Frequency_Percentage = round((FrequencyTable_SubCategory$Freq/sum(FrequencyTable_SubCategory$Freq))*100)
FrequencyTable_SubCategory = FrequencyTable_SubCategory[order(FrequencyTable_SubCategory$Frequency),]
colnames(FrequencyTable_SubCategory) = c("Sub Category ","Frequency","Frequency Percentage")
add_precent = c(FrequencyTable_SubCategory$`Frequency Percentage`)
FrequencyTable_SubCategory$`Frequency Percentage` = paste(add_precent,"%",sep ="")
###### Frequency Table Sub Category ##########


slices = FrequencyTable_HarmfulDish$Frequency
lables = c("Unhealthy Harmful Dish","Harmful Dish")
pct = round(slices/sum(slices)*100)
lables = paste(lables,pct)
lables = paste(lables,"%",sep ="")
pie3D(explode = 0.1 ,slices,labels = lables, col =c("green", "red"),main="Pie Chart of McDonald's Harmful Dishes", radius = 0.8, height = 0.1, theta = 0.8)
####### Pie Chart McDonald's Harmful Dishes ########

slices = FrequencyTable_Category$Frequency
lables = c("Beef & Pork"," Beverages","Breakfast","Chicken & Fish","Coffee & Tea","Desserts","Salads","Smoothies & Shakes","Snacks & Sides")
pct = round(slices/sum(slices)*100)
lables = paste(lables,pct)
lables = paste(lables,"%",sep ="")
pie3D(slices ,labels = lables, col = rainbow(length(lables)),main="Pie Chart of Categories McDonald's  Dishes",radius =0.7, height = 0.15, theta = 0.8,explode=0.05)
####### Pie Chart Categories of McDonald's  dishes ########

counts = table(DataSet$Category)
DataFrame =data.frame(counts)
BarPlot = barplot(counts, main= "Categories McDonald's Dishes", xlab = "Categories",ylab = "Amount of Dishes",cex.names = 0.65,names.arg = gsub("\\s","\n",DataFrame$Var1),ylim=c(0,100),col= rainbow(9))
text(x = BarPlot , y=counts, labels=round(counts, 0 ), pos = 3 , xpd = NA)
######### Bar Chart Categories McDonald's Dishes ##########

counts = table(DataSet$`Harmful Dish`)
DataFrame =data.frame(counts)
BarPlot = barplot(counts, main= "Harmful Dishes McDonald's ",ylab = "Amount of Dishes",cex.names = 0.7,names.arg = gsub("\\s","\n",DataFrame$Var1),ylim=c(0,200),col= c("red","green"))
text(x = BarPlot , y=counts, labels=round(counts, 0 ), pos = 3 , xpd = NA)
######### Bar Chart McDonald'sHarmful Dishes ##########

counts = table(DataSet$`Sub Category`)
DataFrame =data.frame(counts)
BarPlot = barplot(counts, main= "Sub Categories McDonald's Dishes ",xlab = "Sub Categories",ylab = "Amount of Dishes",cex.names = 0.55,names.arg = gsub("\\s","\n",DataFrame$Var1),ylim=c(0,200),col= rainbow(23))
text(x = BarPlot , y=counts, labels=round(counts, 0 ), pos = 3 , xpd = NA)
######### Bar Chart Sub Category ##########

Br = seq(0,100,by=10)
Ranges = paste(head(Br,-1),Br[-1],sep = " - ")
Frequency = hist(DataSet$Protein, breaks = Br , include.lowest = TRUE , plot = TRUE , xlab = "Protien(gram)" , ylab = "Count Dishes", xlim = c(0,60), ylim = c(0,120),main = "Histogram Amount of Protein Per Dish",col = 'orange')
data.frame(range = Ranges,frequency = Frequency$counts)
grid()
######### Histogram Protien ##########


Br = seq(0,100,by=10)
Ranges = paste(head(Br,-1),Br[-1],sep = " - ")
Frequency = hist(DataSet$`Iron (% Daily Value)`, breaks = Br , include.lowest = TRUE , plot = TRUE , xlab = "Iron(% Daily Value)" , ylab = "Count Dishes", xlim = c(0,50), ylim = c(0,200),main = "Histogram Amount of Iron Per Dish",col = 'orange')
data.frame(range = Ranges,frequency = Frequency$counts)
grid()
######### Histogram Iron ##########

Statistical_Iron = round(data.frame(median(DataSet$`Iron (% Daily Value)`),mean(DataSet$`Iron (% Daily Value)`),sd(DataSet$`Iron (% Daily Value)`),max(DataSet$`Iron (% Daily Value)`),min(DataSet$`Iron (% Daily Value)`),var(DataSet$`Iron (% Daily Value)`)))
colnames(Statistical_Iron) = c("Median","Mean","sd","Max","Min","Variance")
###### Statistical metrics Iron ######

Statistical_Protien = round(data.frame(median(DataSet$Protein),mean(DataSet$Protein),sd(DataSet$Protein),max(DataSet$Protein),min(DataSet$Protein),var(DataSet$Protein)))
colnames(Statistical_Protien) = c("Median","Mean","sd","Max","Min","Variance")
###### Statistical metrics Protien ######


Statistical_Calories = round(data.frame(median(DataSet$Calories),mean(DataSet$Calories),sd(DataSet$Calories),max(DataSet$Calories),min(DataSet$Calories),var(DataSet$Calories),frequency(DataSet$Calories)))
colnames(Statistical_Calories) = c("Median","Mean","sd","Max","Min"," Variance")
###### Statistical Calories ######


summary(DataSet)
################# Statistical Report ##################


############### Scatter Plot And line Regression #################
colnames(DataSet)[1] = c('Class')
ggplot(data = DataSet)+geom_point(mapping=aes(x=Calories,y=Cholesterol,colour= Class),size = 2)+geom_smooth(mapping = aes(x=Calories,y=Cholesterol), method="lm")

colnames(DataSet)[26] = c('Iron')
ggplot(data = DataSet)+geom_point(mapping=aes(x=Protein,y=Iron,colour= Class),size = 2)+geom_smooth(mapping = aes(x=Protein,y=Iron), method="lm")

colnames(DataSet)[8] = c('TotalFat')
ggplot(data = DataSet)+geom_point(mapping=aes(x=Sugars,y=TotalFat,colour= Class),size = 2)+geom_smooth(mapping = aes(x=Sugars,y=TotalFat))

colnames(DataSet)[3] = c('SubCategory')
ggplot(data=DataSet)+geom_jitter(mapping=aes(x=Category,y=SubCategory,colour=Class),size=2)


################ Box Plot ###########
ggplot(data=DataSet)+geom_boxplot(mapping=aes(x=Protein,y=Iron,fill=Class))+geom_jitter(mapping=aes(x=Protein,y=Iron,color = Category))
ggplot(data=DataSet)+geom_boxplot(mapping=aes(x=Calories,y=Cholesterol,fill=Class))+geom_jitter(mapping=aes(x=Calories,y=Cholesterol,color = SubCategory))
ggplot(data=DataSet)+geom_boxplot(mapping=aes(x=Sugars,y=TotalFat,fill=Class))


############ Density Plot ################
ggplot(data=DataSet)+geom_density(mapping=aes(x=Calories,fill=Category,alpha=0.2))
ggplot(data=DataSet)+geom_density(mapping=aes(x=Protein,fill=Class,alpha=0.2),kernel = "epanechnikov")
ggplot(data=DataSet)+geom_density(mapping=aes(x=Cholesterol,fill=Class,alpha=0.5), kernel = "rectangular")
ggplot(data=DataSet)+geom_density(mapping=aes(x=Sugars,fill=Category,alpha=0.5))



###################### Normalize the data #########################

####### Entering proportions for data by batch weight ######

DataSet$Calories =round(DataSet$Calories/DataSet$`Serving Size (oz)`)
colnames(DataSet)[6] = "Calories/Serving Size"

DataSet$`Calories from Fat` = round(DataSet$`Calories from Fat`/DataSet$`Serving Size (oz)`)
colnames(DataSet)[7] = "Calories from Fat/Serving Size"

DataSet$`TotalFat` = round(DataSet$`TotalFat`/DataSet$`Serving Size (oz)`)
colnames(DataSet)[8] = "Total Fat/Serving Size"

DataSet$`Saturated Fat` = round(DataSet$`Saturated Fat`/DataSet$`Serving Size (oz)`)
colnames(DataSet)[10] = "Saturated Fat/Serving Size"

DataSet$`Trans Fat` = DataSet$`Trans Fat`/DataSet$`Serving Size (oz)`
colnames(DataSet)[12] = "Trans Fat/Serving Size"

DataSet$Cholesterol = round(DataSet$Cholesterol/DataSet$`Serving Size (oz)`)
colnames(DataSet)[13] = "Cholesterol/Serving Size"

DataSet$Sodium = round(DataSet$Sodium/DataSet$`Serving Size (oz)`)
colnames(DataSet)[15] = "Sodium/Serving Size"

DataSet$Carbohydrates = round(DataSet$Carbohydrates/DataSet$`Serving Size (oz)`)
colnames(DataSet)[17] = "Carbohydrates/Serving Size"

DataSet$Protein = round(DataSet$Protein/DataSet$`Serving Size (oz)`)
colnames(DataSet)[22] = "Protein/Serving Size"

DataSet$Sugars = round(DataSet$Sugars/DataSet$`Serving Size (oz)`)
colnames(DataSet)[21] = "Sugars/Serving Size"

DataSet$`Dietary Fiber` = round(DataSet$`Dietary Fiber`/DataSet$`Serving Size (oz)`)
colnames(DataSet)[19] = "Dietary Fiber/Serving Size"

DataSet$`Serving Size (oz)` = round(DataSet$`Serving Size (oz)`)


## Normalize the categorical columns ##

DataSet$Category = factor(DataSet$Category)
levels(DataSet$Category) = c(0,1,2,3,4,5,6,7,8)

DataSet$`SubCategory` = factor(DataSet$`SubCategory`)
levels(DataSet$`SubCategory`) = c(0,1,2,3,4,5,6,7,8,9, 10, 11, 12, 13, 14, 15,16, 17, 18, 19, 20, 21, 22)


###from int to double###

DataSet$Category =  as.double(DataSet$Category)
DataSet$`SubCategory` =  as.double(DataSet$`SubCategory`)


######## Create a function to normalize variables #############

normalize = function(x){
  
  return ((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
  }

### Apply function to the data set ###

Normaliztion_data = as.data.frame(lapply(DataSet[,-c(1,4)],normalize))

#################### Making Testing Set and Training Set ####################

library(ISLR2)
library(tidyr)

train.index = sample(1:nrow(DataSet), 0.8*nrow(DataSet))
train.set = Normaliztion_data[train.index,]
test.set = Normaliztion_data[-train.index,]
train.Target = DataSet[train.index,]$Class
test.Target = DataSet[-train.index,1]$Class

#################### Knn Algorithm ##############

library(class)

Knn_Algorithm_K3 = data.frame(knn(train.set,test.set,cl=train.Target,k=3))
colnames(Knn_Algorithm_K3) = "Classification results"

Knn_Algorithm_K5 = data.frame(knn(train.set,test.set,cl=train.Target,k=5))
colnames(Knn_Algorithm_K5) = "Classification results"

Knn_Algorithm_K7 = data.frame(knn(train.set,test.set,cl=train.Target,k=7))
colnames(Knn_Algorithm_K7) = "Classification results"

confusion_Matrix_Knn_K3 = table(knn(train.set,test.set,cl=train.Target,k=3),test.Target)
confusion_Matrix_Knn_K5 = table(knn(train.set,test.set,cl=train.Target,k=5),test.Target)
confusion_Matrix_Knn_K7 = table(knn(train.set,test.set,cl=train.Target,k=7),test.Target)
############# Confusion Matrix Knn  Algorithm #########

Knn_K3_pre_table = tibble("Actually" = test.Target ,"Predicted" = knn(train.set,test.set,cl=train.Target,k=3))
Knn_K3_table = table(Knn_K3_pre_table)
Knn_K3_cfm = as_tibble(Knn_K3_table)
plot_confusion_matrix(Knn_K3_cfm, 
                      target_col = "Actually", 
                      prediction_col = "Predicted",
                      counts_col = "n",
                      palette = "Purples",
                      add_row_percentages = FALSE,
                      add_col_percentages = FALSE,
                      counts_on_top = TRUE) +  ggplot2::labs(x ="Predicted" , y ="Actually",title = "KNN Alogrithm" )
############## Plot Confusion Matrix Knn  Algorithm ########## 



Knn_Metrics_K3 = confusionMatrix(confusion_Matrix_Knn_K3, mode = "everything")
Knn_Metrics_K5= confusionMatrix(confusion_Matrix_Knn_K5, mode = "everything")
Knn_Metrics_K7 = confusionMatrix(confusion_Matrix_Knn_K7, mode = "everything")
############# Metrics of Knn classifier ############# 

print(Knn_Metrics_K3)



############ Map missing Values #######
library(Amelia)
missmap(obj = Normaliztion_data)
############ There are no missing values #######



##########Correlation##########

#### correlation matrix ####

Normaliztion_data.cor = cor(Normaliztion_data)

library(corrplot)
library(RColorBrewer)

corrplot (Normaliztion_data.cor,
         col = colorRampPalette(c("white", "deepskyblue", "blue4"))(50),
         method = "color",
         addCoef.col = " black",
         number.cex = 0.6,
         addgrid.col = "white",
         type = "lower",
         tl.pos = "b",
         tl.cex = 0.7,
         tl.col = "black",
         order = "original",
         mar = c(0.1,0.1,0.1,0.1))


corrplot(Normaliztion_data.cor,
         col = colorRampPalette(c("red","orange" ,"deepskyblue", "blue4"))(9),
         method = "number",
         number.cex = 0.5,
         bg = "light grey",
         type = "full",
         tl.pos = "id",
         tl.cex = 0.7,
         title = "Full size ,Matrix",
         mar = c(0.001,0.001,1.5,5))



#####correlation to the Target column#####
DataSet$Class = factor(DataSet$Class)
levels(DataSet$Category) = c(0,1)
DataSet$Class =  as.double(DataSet$Class)

corToTarget = cor(newDataSet, DataSet$Class)
corToTarget = abs(corToTarget)
corToTarget = with(corToTarget, corToTarget[order(corToTarget[,1])])
barplot(corToTarget, beside = T, col = rainbow(nrow(corToTarget)),  names.arg = c("category","SubCategory","ServingSize","Calories","CaloriesFromFat","TotalFat","TotalFat%","SatruatedFat","SatruatedFat%","TransFat","Cholesterol","Cholesterol%","Sodium","Sodium%","Carcohydrates","Carcohydrates%","DietaryFiber","DietaryFiber%","Sugars","Protein","Vitamin A","Vitamin C","Calcium","Iron"), las=1,horiz=T)





################ Normalization Data With Target ########

Normaliztion_data$Target = factor(DataSet$Class)
train.index = sample(1:nrow(DataSet), 0.8*nrow(DataSet))
train.set = Normaliztion_data[train.index,] 
test.set =  Normaliztion_data[-train.index,]


############ Logistic regression ################

logit.model = glm(Target ~ . , data = train.set, family = binomial)
summary(logit.model)

odds.ratio = exp(coef(logit.model))


prob = predict(logit.model, newdata = test.set, type = "response")
predictions = rep("Harmful",nrow(test.set))
predictions[prob>0.5] = "Unharmful"
confusion_Matrix_LogisticRegression = table(predict = predictions,actual = test.set$Target)
################### confusion Matrix Logistic Regression ###########

logistic_pre_table = tibble("Actually" = test.set$Target,"Predicted" = predictions)
logistic_table = table(logistic_pre_table)
print(logistic_table)
logistic_cfm = as_tibble(logistic_table)
plot_confusion_matrix(logistic_cfm, 
                      target_col = "Actually", 
                      prediction_col = "Predicted",
                      counts_col = "n",
                      palette = "Greens",
                      add_row_percentages = FALSE,
                      add_col_percentages = FALSE,
                      counts_on_top = TRUE)+  ggplot2::labs(x ="Predicted" , y ="Actually",title = "Logistic Regression" )
################## Plot Confusion matrix Logistic regression #######

Logistic_Metrics = confusionMatrix(confusion_Matrix_LogisticRegression, mode = "everything")
print(Logistic_Metrics)

############## Metrics of Logistic regression ###########





################# Decision Tree ####################
library(tree)
library(rpart)
library(rpart.plot)
library(maptree)

tree.model = rpart(Target ~., data = train.set , method = 'class')
rpart.plot(tree.model , type = 5 ,digits = 4 , fallen.leaves = F ,extra = 101,tweak=1.6,leaf.round=5)


tree.model.new = tree(Target ~., train.set)
cv.tree = cv.tree(tree.model.new, FUN = prune.misclass)
plot(cv.tree$size, cv.tree$dev, type = "b", xlab = "Tree Size",
     ylab = "Deviance")


tree_prune = prune.misclass(tree.model.new, best = 5)
plot(tree_prune)
text(tree_prune, cex=0.6, pretty=0)


pred.value = predict(tree_prune, newdata = test.set, type = "class")
confusion_Matrix_DecisionTree = table(predict = pred.value, actual = test.set$Target)
###################### confusion Matrix Decision Tree ############## 

DecisionTree_pre_table = tibble("Actually" = test.set$Target,"Predicted" = pred.value)
DecisionTree_table = table(DecisionTree_pre_table)
DecisionTree_cfm = as_tibble(DecisionTree_table)
plot_confusion_matrix(DecisionTree_cfm , 
                      target_col = "Actually", 
                      prediction_col = "Predicted",
                      counts_col = "n",
                      palette = "Reds",
                      add_row_percentages = FALSE,
                      add_col_percentages = FALSE,
                      counts_on_top = TRUE) +  ggplot2::labs(x ="Predicted" , y ="Actually",title = "Decision Tree" )
################## Plot Confusion matrix Logistic regression #######

DecisionTree_Metrics = confusionMatrix(confusion_Matrix_DecisionTree , mode = "everything")
print(DecisionTree_Metrics)
############## Metrics of Decision Tree ###########




######### Random Forest ###########

library(randomForest)

rf.model = randomForest(Target ~ . , data = train.set, na.action = na.roughfix, mtry = 3, importance = T)
pred.value = predict(rf.model, newdata = test.set)

importance(rf.model)
varImpPlot(rf.model)

RandomForest_pre_table = tibble("Actually" = test.set$Target,"Predicted" = pred.value)
RandomForest_table = table(RandomForest_pre_table)
RandomForest_cfm = as_tibble(RandomForest_table)
plot_confusion_matrix(RandomForest_cfm , 
                      target_col = "Actually", 
                      prediction_col = "Predicted",
                      counts_col = "n",
                      palette = "Blues",
                      add_row_percentages = FALSE,
                      add_col_percentages = FALSE,
                      counts_on_top = TRUE) +  ggplot2::labs(x ="Predicted" , y ="Actually",title = "Random Forest" )
################## Plot Confusion matrix Random Forest #######


confusion_Matrix_RandomForest = table(predict = pred.value, actual = test.set$Target)
###################### confusion Matrix Random Forest ############## 


RandomForest_Metrics = confusionMatrix(confusion_Matrix_RandomForest , mode = "everything")
print(RandomForest_Metrics)
############## Metrics of Random Forest ###########