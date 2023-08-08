
# Loading the Datasets
setwd("C:/Users/Admin/Desktop/study/IITK/internship_project")
lungcap=read.csv("LungCapData2.csv")
View(lungcap)
str(lungcap)

table(lungcap$Gender)
barplot(table(lungcap$Gender),col="yellow")
#males are relatively higher than females

hist(lungcap$Height, col="pink", xlab="Height", main="Histogram of Height")
#max number of people have height between 56 & 68
hist(lungcap$LungCap ,col="green", xlab="Lung capacity", main="Histogram of Lung Capacity")
#max no. of ppl have lungcap between 2 & 8
hist(lungcap$Age, col="blue", xlab="Age", main="Histogram of Age" )
#max no. of ppl have age btwn 6 & 12

plot(lungcap$Age,lungcap$LungCap, col="blue",main="Scatterplot of Age VS Lung Capacity", xlab="Age", ylab="Lung Capacity")
# younger age people have less lung capacity in comparison to teenagers.

plot(lungcap$Height,lungcap$LungCap, col="brown", main="Scatter plot of Height VS Lung Capacity", xlab="Height", ylab="Lung Capacity")
# we can see that height and lungcap have high correlation.

# descriptive analysis
mean(lungcap$Age)
mean(lungcap$Height)
mean(lungcap$LungCap)

min(lungcap$Age)
max(lungcap$Age)

min(lungcap$LungCap)
max(lungcap$LungCap)

min(lungcap$Height)
max(lungcap$Height)


# checking the avg lung capacties of male and female
male_data=lungcap[lungcap$Gender=="male",]
mean(male_data$LungCap)
# from this we can see that avg lung capacity in males is 6.437

female_data=lungcap[lungcap$Gender=="female",]
mean(female_data$LungCap)
# lung capacity of females(5.35) is lesser than in males(6.437)



#checking the lungcapcity of smokers and non smokers

smoke_data=lungcap[lungcap$Smoke=="yes",]
nonsmoke_data=lungcap[lungcap$Smoke=="no",]

mean(smoke_data$LungCap)
mean(nonsmoke_data$LungCap)
# this result is quite questionnable.

#checking for male smokers and non smokers
malesmoke=lungcap[lungcap$Gender=="male"&lungcap$Smoke=="yes",]
mean(malesmoke$LungCap)
malenonsmoke=lungcap[lungcap$Gender=="male"&lungcap$Smoke=="no",]
mean(malenonsmoke$LungCap)

#checking for female smokers and non smokers
femalesmoke=lungcap[lungcap$Gender=="female"&lungcap$Smoke=="yes",]
mean(femalesmoke$LungCap)
femalenonsmoke=lungcap[lungcap$Gender=="female"&lungcap$Smoke=="no",]
mean(femalenonsmoke$LungCap)

#checking for different age groups 
plot(lungcap$Age,lungcap$LungCap)
#from this plot we can see that people between age 10 and 19 have higher lung capacity

# as we can see there's no smokers in the age group 3-8 the comparison is not unbiased

#so, there's no person in 0 to 8 age who smokes
#and generally the lung capacity of kids is less


#so we will find the lungcapcity of smokers and non smokers for the age group 9-19

newdata = lungcap[lungcap$Age>9,]
newnonsmoke = newdata[newdata$Smoke=="no",]
newsmoke = newdata[newdata$Smoke=="yes",]
mean(newnonsmoke$LungCap)
mean(newsmoke$LungCap)

#now the lung capacity of smokers is lesser than non smokers

#### BOXPLOTS

boxplot(lungcap$LungCap~lungcap$Gender, col="pink", main="Boxplots of Male and Female Lung Capacity") 
# average lung capcity in males is higher than in females

boxplot(lungcap$LungCap~lungcap$Height ,col="yellow", main="Boxplots of Different Heights against Lung Capacity", xlab="Height",ylab="Lung capacity")
# taller people have high average lung capacity



boxplot(lungcap$Height~lungcap$Smoke, main="Boxplots of Smoke VS Height", xlab="Smoke", ylab="Height", col="yellow")
# poeple who smokes are taller, which is obvious as they are older in age.

# Now we will split the data into training and testing.

split_val <- sample(nrow(newdata) , nrow(newdata)*0.8)
train <- newdata[split_val,]
test <- newdata[-split_val,]

attach(train)

mod1=lm(data = train ,LungCap~ Age+ Height+ Gender+ Smoke)
summary(mod1)

mod2=lm(data=train, LungCap~Age+Height+Smoke )
summary(mod2)

mod3=lm(data = train ,LungCap~ Age+ Height+ Gender)
summary(mod3)

# from all the three models we can see that there's no significant
#change in r square, so we can fit the model using all the predictors.

y_pred <- predict(mod1 , test[,-2])
y_pred_train <- predict(mod1 , train[,-2])


(cor(train$LungCap , y_pred_train))
(cor(test$LungCap , y_pred))

# We can see that correlation between actual and predicted values is around 80%, which is significant and we can say
#model is accurate.

