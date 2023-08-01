# Loading the Datasets

lungcap=read.csv("Lung_cap.csv")
View(lungcap)
str(lungcap)

table(lungcap$Gender)
barplot(table(lungcap$Gender))
#males are relatively higher than females

hist(lungcap$Height)
#max number of people have height between 56 & 68
hist(lungcap$LungCap)
#max no. of ppl have lungcap btwn 2 & 8
hist(lungcap$Age)
#max no. of ppl have age btwn 6 & 12

plot(lungcap$Age,lungcap$LungCap)
# younger age people have less lung capacity in comparison to teenagers.

plot(lungcap$Height,lungcap$LungCap)
# we can see that height and lungcap have high corr.

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
barplot(male_data$LungCap)
# from this we can see that avg lung capacity in males is 6.437

female_data=lungcap[lungcap$Gender=="female",]
mean(female_data$LungCap)
# lung capacity of females(5.35) is lesser than in males(6.437)



#checking the lungcapcity of smokers and non smokers

smoke_data=lungcap[lungcap$Smoke=="yes",]
boxplot(smoke_data$LungCap)
nonsmoke_data=lungcap[lungcap$Smoke=="no",]


mean(smoke_data$LungCap)
mean(nonsmoke_data$LungCap)

hist(smoke_data$LungCap)
hist(nonsmoke_data$LungCap)


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
#from this plot we can see that people between age 10 and 15 have higher lung capacity

age1_ns=lungcap[lungcap$Age>0&lungcap$Age<8&lungcap$Smoke=="no",]
mean(age1_ns$LungCap)
age1_s=lungcap[lungcap$Age>0&lungcap$Age<8&lungcap$Smoke=="yes",]
mean(age1_s$LungCap)
#so, there's no person in 0 to 8 age who smokes
#and generally the lung capacity of kids is less

age2_ns=lungcap[lungcap$Age>8&lungcap$Smoke=="no",]
mean(age2_ns$LungCap)
age2_s=lungcap[lungcap$Age>8&lungcap$Smoke=="yes",]
mean(age2_s$LungCap)

#### BOXPLOTS

boxplot(lungcap$LungCap~lungcap$Gender) 
# avg lung capcity in males is higher than in females

boxplot(lungcap$LungCap~lungcap$Height)
# taller people have high avg lung capacity

quantile(nonsmoke_data$LungCap)
boxplot(lungcap$LungCap~lungcap$Smoke)
# we can see some outliers in the boxplots of nonsmokers
#UF formula: q3+1.5(q3-q1)
#LF formula: q3-1.5*(q3-q1)
upper_fence=7.144+1.5*(7.144-3.76)
# uf = 12.22

boxplot(lungcap$Age~lungcap$Smoke)
# we can see some outliers in the data of people who don't smoke



boxplot(lungcap$Height~lungcap$Smoke)
# poeple who smokes are taller, which is obvious as they are older in age.

newdata=lungcap[lungcap$Age>9,]
newnonsmoke=newdata[newdata$Smoke=="no",]
newsmoke=newdata[newdata$Smoke=="yes",]
mean(newnonsmoke$LungCap)
mean(newsmoke$LungCap)
# to make the data unbiased we have taken the ages starting from 9, as there are no people who smokes in the age group 0-9.
mod=lm(newdata$LungCap~newdata$Age+newdata$Height+newdata$Gender+newdata$Smoke)
summary(mod)
# the p values of all the attributes is less than 0.05, so we don't have to remove any of the attributes and we can fit a regression line based on this model.





