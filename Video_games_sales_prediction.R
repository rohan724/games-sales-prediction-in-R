


#   -------------------------------REGRESSION TREE--------------------------------
library(randomForest)
library(tidyr)
library(caret)
library(rpart)
library(rpart.plot)
library(xlsx)
library(corrplot)
library(forecast)
library(ggplot2)

sales = read.csv("Video_Games_Sales_as_at_22_Dec_2016.csv", header=T, na.strings=c(""," ", "NA", "N/A"))
str(sales)
summary(sales)
#DATA CLEANING
sales$User_Score[sales$User_Score == 'tbd' & !is.na(sales$User_Score)] <- NA
sales$User_Score <- as.numeric(as.character(sales$User_Score))
sales <- sales[ !( sales$Year_of_Release > 2017 | is.na(sales$Year_of_Release)),]
sales$Rating[sales$Rating == 'K-A' & !is.na(sales$Rating) ] <- 'E'
sales$Rating[sales$Rating == 'RP' & !is.na(sales$Rating) ] <- NA
sales$Ratingn <- as.numeric(as.character(factor(sales$Rating, levels =c("RP", "EC", "E" ,"E10+", "T", "M","AO"), labels = c(NA,1,2,3,4,5,6))))


levels(sales$Platform)
nintendoplatforms = c("3DS","DS","GB","GBA","N64","GC", "NES","SNES","Wii","WiiU")
sonyplatforms = c("PS","PS2","PSP","PS3","PS4","PSV")
segaplatforms = c("GEN","SCD","DC","GG")
msplatforms = c("XB","X360", "XOne")
otherplatforms = c("2006","3DO","NG","PCFX","TG16")
#Assigning the platform vendor to every title
sales$Platformvendor[sales$Platform %in% nintendoplatforms] <- "nintendo"
sales$Platformvendor[sales$Platform %in% sonyplatforms] <- "sony"
sales$Platformvendor[sales$Platform %in% msplatforms] <- "microsoft"
sales$Platformvendor[sales$Platform %in% segaplatforms] <- "sega"
sales$Platformvendor[sales$Platform == "PC"] <- "comp"
sales$Platformvendor[is.na(sales$Platformvendor)] <- "other"
#adding first party publishers
sonypublishers <- c("Sony Computer Entertainment","Sony Computer Entertainment America",
                    "Sony Computer Entertainment Europe",
                    "Sony Music Entertainment",
                    "Sony Online Entertainment")
sales$FP <- FALSE
sales$FP[(sales$Publisher %in% sonypublishers) & (sales$Platform == "PS3")] <- TRUE
sales$FP[(sales$Publisher %in% 'Microsoft Game Studios') & (sales$Platform %in% 'X360')] <- TRUE
sales$FP[(sales$Publisher %in% "Nintendo") & (sales$Platform %in% "Wii")] <- TRUE

sonydevs <- c('Sony Online Entertainment', 'Polyphony Digital','SCE Japan Studio',
              'SCE Santa Monica','SCE Studio Cambridge','SCE/WWS, Media Molecule',
              'SCEA San Diego Studios','SCEA','SCEA, Zindagi Games', 'SCEE',
              'SCEE London Studio','SCEJ','Guerrilla')

sales<-na.omit(sales)
sales_NA<-sales[(sales$NA_Sales>0),]
sales_JP<-sales[(sales$JP_Sales>0),]
sales_EU<-sales[(sales$EU_Sales>0),]
#View(sales_NA$NA_Sales)
sales$Global_Sales<-log10(sales$Global_Sales)
sales_NA$NA_Sales<-log10(sales_NA$NA_Sales)
sales_JP$JP_Sales<-log10(sales_JP$JP_Sales)
sales_EU$EU_Sales<-log10(sales_EU$EU_Sales)

#splitting data
sales_NA<-na.omit(sales_NA)
set.seed(123)
index <- sample(1:nrow(sales_NA),size = 0.7*nrow(sales_NA)) 
sales_NA.train <- sales_NA[index,]
sales_NA.test <- sales_NA[-index,]

sales_EU<-na.omit(sales_EU)
index <- sample(1:nrow(sales_EU),size = 0.7*nrow(sales_EU)) 
sales_EU.train <- sales_EU[index,]
sales_EU.test <- sales_EU[-index,]


sales_JP<-na.omit(sales_JP)
index <- sample(1:nrow(sales_JP),size = 0.7*nrow(sales_JP)) 
sales_JP.train <- sales_JP[index,]
sales_JP.test <- sales_JP[-index,]


names(sales_NA)

NA.tree <- rpart(NA_Sales ~ Genre+Platform+FP+Year_of_Release,
                 method="anova", data = sales_NA.train)
rpart.plot(NA.tree, type = 1)

EU.tree <- rpart(EU_Sales ~ Genre+Platform+FP+Year_of_Release
                  , data = sales_EU.train)

JP.tree <- rpart(JP_Sales ~ Genre+Platform+FP+Year_of_Release
                  , data = sales_JP.train)


RMSE(predict(NA.tree, sales_NA.train[,c(4,2,19,3)]), sales_NA.train$NA_Sales)
RMSE(predict(EU.tree, sales_EU.train[,c(4,2,19,3)]), sales_EU.train$EU_Sales)
RMSE(predict(JP.tree, sales_JP.train[,c(4,2,19,3)]), sales_JP.train$JP_Sales)

RMSE(predict(NA.tree, sales_NA.test[,c(4,2,19,3)]), sales_NA.test$NA_Sales)
RMSE(predict(EU.tree, sales_EU.test[,c(4,2,19,3)]), sales_EU.test$EU_Sales)
RMSE(predict(JP.tree, sales_JP.test[,c(4,2,19,3)]), sales_JP.test$JP_Sales)




#   -------------------------------MULTIPLLE LINEAR REGRESSION--------------------------------




vg<-read.csv("Video_Games_Sales_as_at_22_Dec_2016.csv")

str(vg)
summary(vg)
vg$Year_of_Release <- as.numeric(as.character(vg$Year_of_Release))
vg<-na.omit(vg)
vg_filter<-vg[(vg$Year_of_Release<=2016)&(vg$Year_of_Release>2009),]
vg_filter_NA<-vg_filter[(vg_filter$NA_Sales>0),]
vg_filter_JP<-vg_filter[(vg_filter$JP_Sales>0),]
vg_filter_EU<-vg_filter[(vg_filter$EU_Sales>0),]

#corrplot(cor(vg_filter),xlab="Correlation Plot")

hist(vg_filter$Global_Sales, main="Global Sales", xlab="Global Sales", breaks=200)
hist(vg_filter_NA$NA_Sales, main="North Am Sales", xlab="NA Sales", breaks=250)
hist(vg_filter_JP$JP_Sales, main="Japanese Sales", xlab="Japanese Sales", breaks=250)
hist(vg_filter_EU$EU_Sales, main="European Sales", xlab="European Sales", breaks=250)

vg_filter$Global_Sales<-log10(vg_filter$Global_Sales)
vg_filter_NA$NA_Sales<-log10(vg_filter_NA$NA_Sales)
vg_filter_JP$JP_Sales<-log10(vg_filter_JP$JP_Sales)
vg_filter_EU$EU_Sales<-log10(vg_filter_EU$EU_Sales)

#summary(vg_filter)
#summary(vg_filter_EU)
#summary(vg_filter_JP)
#summary(vg_filter_NA)

hist(vg_filter$Global_Sales, main="Global Sales", xlab="Log of Global Sales")
hist(vg_filter_NA$NA_Sales, main="North Am Sales", xlab="Log of NA Sales", breaks=20)
hist(vg_filter_JP$JP_Sales, main="Japanese Sales", xlab="Log of Japanese Sales", breaks=15)
hist(vg_filter_EU$EU_Sales, main="European Sales", xlab="Log European Sales", breaks=15)

summary(vg_filter)





#NA Sales
model_NA<-lm(NA_Sales ~Genre+Platform+Publisher+Developer , data=vg_filter_NA)
#only print significant factors
model1<-coef(summary(model_NA))
sig_model_NA<-model1[model1[,"Pr(>|t|)"]<.05,]
printCoefmat(sig_model_NA)


# JP
model_JP<-lm(JP_Sales ~ Genre+Platform+Publisher+Developer , data=vg_filter_JP)
#only print significant factors
model2<-coef(summary(model_JP))
sig_model_JP<-model2[model2[,"Pr(>|t|)"]<.05,]
printCoefmat(sig_model_JP)


#EU
model_EU<-lm(EU_Sales ~ Genre+Platform+Publisher+Developer , data=vg_filter_EU)
#only print significant factors
model3<-coef(summary(model_EU))
sig_model_EU<-model3[model3[,"Pr(>|t|)"]<.05,]
printCoefmat(sig_model_EU)


#Global
model_GL<-lm(Global_Sales ~ Genre+Platform+Publisher+Developer, data=vg_filter)
#only print significant factors
model4<-coef(summary(model_GL))
sig_model_GL<-model4[model4[,"Pr(>|t|)"]<.05,]
printCoefmat(sig_model_GL)

#summary(model_GL)
summary(model_NA)
summary(model_EU)
summary(model_JP)

predictions_JP <- model_JP %>% predict(vg_filter_JP)
RMSE(predictions_JP, vg_filter_JP$JP_Sales)
predictions_NA <- model_NA %>% predict(vg_filter_NA)
RMSE(predictions_NA, vg_filter_NA$NA_Sales)
predictions_EU <- model_EU %>% predict(vg_filter_EU)
RMSE(predictions_EU, vg_filter_EU$EU_Sales)


d_NA<-vg_filter_NA
d_NA$predicted <- predictions_NA   
ggplot(d_NA, aes(x = d_NA$NA_Sales, y =d_NA$predicted )) + geom_point()

d_EU<-vg_filter_EU
d_EU$predicted <- predictions_EU   
ggplot(d_EU, aes(x = d_EU$EU_Sales, y =d_EU$predicted )) + geom_point()

d_JP<-vg_filter_JP
d_JP$predicted <- predictions_JP  
ggplot(d_JP, aes(x = d_JP$JP_Sales, y =d_JP$predicted )) + geom_point()














