# Reading the csv file and checking the dimension
Titanic<-read.csv('titanic.csv')
dim(Titanic)

#Average age and rounding offto nearest integer
meanAge<-sum(na.omit(Titanic$Age))/length(na.omit(Titanic$Age))
meanAge
Titanic$Age[na.omit(Titanic$Age)]<-meanAge
Titanic$Age<-round(Titanic$Age)

#Creating a new variable age category 
Titanic$AgeCat[Titanic$Age>=0&Titanic$Age<=16]<-"0-16"
Titanic$AgeCat[Titanic$Age>=17&Titanic$Age<=32]<-"17-32"
Titanic$AgeCat[Titanic$Age>=33&Titanic$Age<=48]<-"33-48"
Titanic$AgeCat[Titanic$Age>=49&Titanic$Age<=64]<-"49-64"
Titanic$AgeCat[Titanic$Age>=65]<-"65 and above"

#Replacing the integer value of 0 and 1 in the survivor variable
Titanic$Survived[Titanic$Survived==0]<-"Not Survived"
Titanic$Survived[Titanic$Survived==1]<-"Survived"

#Converting the integer and character vectors to factor variables
Titanic$Pclass<-factor(Titanic$Pclass)
Titanic$AgeCat<-factor(Titanic$AgeCat)
Titanic$Survived<-factor(Titanic$Survived)
Titanic$Embarked<-as.character(Titanic$Embarked)
Titanic$Embarked[Titanic$Embarked=="S"]<-"Southhampton"
Titanic$Embarked[Titanic$Embarked=="C"]<-"Cherbourg"
Titanic$Embarked[Titanic$Embarked=="Q"]<-"Queenstown"
Titanic$Embarked<-factor(Titanic$Embarked)

#Removing the other redundant variables such as Ticket and Cabin from the titanic data frame
Titanic=Titanic[c(-9,-11)]
View(Titanic)

#saving the file as titanicnew
write.csv (Titanic, file = "D:/Spring 2019/BUAN 6380 Data Visualization/Assignments/Exercise 7/kmean/titanicNew.csv")

#Decision tree
#converting SibSp and Parch into categorical variables and adding them to the data
decision_tree<-Titanic
SibSpCat= ifelse(decision_tree$SibSp >=3,'>=3','<3')
decision_tree <- data.frame(decision_tree,SibSpCat)
decision_tree$SibSpCat<-as.factor(decision_tree$SibSpCat)
ParchCat= ifelse(decision_tree$Parch >=3,'>=3','<3')
decision_tree <- data.frame(decision_tree,ParchCat)
decision_tree$ParchCat<-as.factor(decision_tree$ParchCat)

#Splitting training and testing data
set.seed(1)

test = sample(1:nrow(decision_tree),nrow(decision_tree)/3)
train = -test
training_data = decision_tree[train,]
testing_data = decision_tree[test,]
testing_survived = decision_tree$Survived[test]

#Installing packages to build the tree
library(rpart)
library(rattle)

tree_model=rpart(Survived ~ Pclass + Sex + AgeCat + Embarked + SibSpCat + ParchCat,data = training_data, method = "class", control = rpart.control(minsplit = 10,cp =0.00))
fancyRpartPlot(tree_model, sub = "decision_tree")
tree_predict = predict(tree_model, testing_data, type="class")

mean(tree_predict != testing_survived)

## K means Clustering##

titanicNew<-read.csv("D:/Spring 2019/BUAN 6380 Data Visualization/Assignments/Exercise 7/kmean/titanicNew.csv")
titanicUpdated<-titanicNew

SurvivedNum<-ifelse(titanicUpdated$Survived=="Not Survived",0,1)
titanicUpdated <-data.frame(titanicUpdated,SurvivedNum)

SexN<-ifelse(titanicUpdated $Sex=="male",1,0)
titanicUpdated <-data.frame(titanicUpdated, SexN)

EmbarkedN<-ifelse(titanicUpdated$Embarked=="Southampton",1,ifelse(titanicUpdated $Embarked=="Cherbourg",2,0))
titanicUpdated <-data.frame(titanicUpdated, EmbarkedN)

write.csv (titanicUpdated, file = "D:/Spring 2019/BUAN 6380 Data Visualization/Assignments/Exercise 7/kmean/titanicUpdated.csv")

#Normalizing data
Titanic.scaled<-scale(data.frame(Titanic$Age,Titanic$Parch,Titanic$SibSp,Titanic$Fare))
colnames(Titanic.scaled)

#selecting opyimum number of cluster
totwss<-vector()
btwss<-vector()
for(i in 2:15)
{
  set.seed(1234)
  temp<-kmeans(na.omit(Titanic.scaled),centers = i)
  totwss[i]<-temp$tot.withinss
  btwss[i]<-temp$betweens
}

plot(totwss,xlab = "Number of Cluster", type="b",ylab = "Total Within Sum of Square")
plot(btwss,xlab = "Number of Cluster", type="b",ylab = "Total Between Sum of Square")


##Tableau/R Integration##

install.packages("Rserve")
library(Rserve)
Rserve()


