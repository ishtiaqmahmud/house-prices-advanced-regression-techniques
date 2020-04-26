#check working directory 
getwd()

#install packages
library(psych)
library(ggplot2)
library(e1071)
library(reshape2)
library(Hmisc)
library(dplyr)
library(corrplot)
library(tidyr)
library(GGally)
library(Amelia)
library(VIM)
library(mice)
library(caret)
library(lattice)
library(randomForest)
library(Boruta)
library(mlbench)
library(car)
library(xgboost)
options(scipen=10000)


#import dataset  
raw_dataset = read.csv("train.csv", header=TRUE)



#################################################################################
############################### DATA UNDERSTANDING ##############################
#################################################################################
head(raw_dataset, 10)   #display first 10 entries

dim(raw_dataset)       #number of rows and columns (observations and variables)

#summary(raw_dataset)  


knitr::kable(
  raw_dataset %>% 
    psych::describe() %>% 
    select(n, mean, sd, min, median, max, skew, kurtosis))  


str(raw_dataset)      

any(is.na(raw_dataset))   #check null values (T or F)


#convert datatypes to correct one's
#MSsubclass, overallqual, overallcond integer to character/Factor

raw_dataset$MSSubClass = as.factor(raw_dataset$MSSubClass)
raw_dataset$OverallQual = as.factor(raw_dataset$OverallQual)
raw_dataset$OverallCond = as.factor(raw_dataset$OverallCond)

#drop id as it not useful for modeling 
raw_dataset$Id = NULL


#################################################################################
######################## DATA EXPLORATION: TRAGET VARIABLE ######################
#################################################################################

#histogram 
ggplot(raw_dataset, aes(x = SalePrice, fill = ..count..)) +
  geom_histogram(bins = 30) +
  labs(title = "Histogram of SalePrice", y="Count of houses", x="SalePrice") +
  theme(plot.title = element_text(hjust = 0.5))


#another method to check distribution 
qqnorm(raw_dataset$SalePrice, ylab="Sample Quantiles of SalePrice")
qqline(raw_dataset$SalePrice, col="red")

#outliers
ggplot(raw_dataset, aes(x="", y=SalePrice))+
  geom_boxplot()+
  labs(title = "Boxplot of traget variable")+
  theme(axis.text.x = element_text(angle=-90 , hjust=1), plot.title = element_text(hjust = 0.5))

skewness(raw_dataset$SalePrice)  #less than -1 or greater than +1, the data is highly skewed

#Our target variable, SalePrice is not normally distributed.
#Our target variable is right-skewed.
#There are multiple outliers in the variable.

####Take log of the target variable and fed that into the models later####



#################################################################################
######################## DATA EXPLORATION: NUMERIC VARIABLE #####################
#################################################################################


############## univariate analysis ############
list_num_cols = sapply(raw_dataset, is.numeric)
list_num_cols

numeric_data = raw_dataset[, list_num_cols]
str(numeric_data)
head(numeric_data)

melt_data = melt(numeric_data, id="Id")
head(melt_data)

ggplot(melt_data, aes(value))+geom_histogram(bins=5, fill="#FF9999", color="black")+
  facet_wrap(~variable, scales = "free_x")+theme_bw()+
  theme(axis.text.x = element_text(angle=-90 , hjust=1))

ggplot(melt_data, aes(variable, value))+geom_boxplot() + facet_wrap(~variable, scale="free")


head(numeric_data, 100)




#sale price with house type (bldg type)
ggplot(raw_dataset, aes(SalePrice)) +
  geom_histogram(aes(fill = BldgType), binwidth = 20000) +
  labs(title="House SalePrice by BldgType(Type of House)", x="House SalePrice", y="Count")+
  theme(plot.title = element_text(hjust = 0.5)) 
#conclusion: house type single family detached price range ranges from somewhere 50000 to 300000. 
#For Two-family Conversion, Duplex, Townhouse End Unit and Townhouse Inside Unit, 
#most of house prices are ranging from 75000 to 210000
# lowest and highest price range fall under single family detached

# sale price with OverallQual 
ggplot(raw_dataset, aes(SalePrice)) +
  geom_histogram(aes(fill = OverallQual), binwidth = 20000) +
  labs(title="House SalePrice by OverallQual(Material and Finish of the House)", x="House SalePrice", y="Count")+
  theme(plot.title = element_text(hjust = 0.5)) 
#conclusion: Most house sold are with OverallQuall of 4,5,6 and 7, equivalent to "Below Average", "Average", "Above Average" and "Good" 
#The higher rate of overall quality, the higher house sale price 
#positive correlation 


# sale price with OverallCond 
ggplot(raw_dataset, aes(SalePrice)) +
  geom_histogram(aes(fill = OverallCond), binwidth = 20000) +
  labs(title="House SalePrice by OverallCond(Condition of the house)", x="House SalePrice", y="Count")+
  theme(plot.title = element_text(hjust = 0.5)) 
#Didn't found any relationship, for example some average(number 5) condition of the house are sold in high price 
#very good (number 8) condition of the house are sold in less price 


############## biunivariate analysis ############

#year built vs saleprice
ggplot(numeric_data, aes(YearBuilt, SalePrice))+geom_point(col="brown3")+theme_bw()+
  labs(title="Sale Price vs Built Year",  y="SalePrice")+geom_smooth(method="lm", se=F, color="blue")+
  theme(axis.text.x = element_text(angle=-90 , hjust=1), plot.title = element_text(hjust = 0.5))+
  scale_x_continuous("YearBuilt", breaks = seq(1872,2010,by = 10))
#roughly after 2000 their is increse in house price   

#total rooms above grade vs sale price
ggplot(numeric_data, aes(TotRmsAbvGrd, SalePrice))+geom_jitter(col="forestgreen")+
  theme_bw()+geom_smooth(method="lm", se=F, color="gold")+
  labs(y="House SalePrice", title="Sale Price vs Total Rooms Above Grade")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous("Total Rooms Above Grade", breaks = seq(0,14,by = 1))
#correalted to each other, their are outliers after 10

#grade living area sq ft vs sale price
ggplot(numeric_data, aes(GrLivArea, SalePrice))+geom_jitter(col="deeppink")+
  theme_bw()+geom_smooth(method="lm", se=F, color="forestgreen")+
  labs(title="Sale Price vs Above Grade Living Area sq ft",  x="Above Grade Living Area sq ft", y="House SalePrice")+
  theme(plot.title = element_text(hjust = 0.5))
#correalted to each other, their are outliers near 5000 

#garage area vs sale price
ggplot(numeric_data, aes(GarageArea, SalePrice))+geom_jitter(col="chocolate3")+
  theme_bw()+geom_smooth(method="lm", se=F, color="black")+
  labs(title="Sale Price vs Garage Area",  y="House SalePrice")+
  theme(axis.text.x = element_text(angle=-90 , hjust=1), plot.title = element_text(hjust = 0.5))+
  scale_x_continuous("Garage Area", breaks = seq(0,1500,by = 100))
#correlated to each other 
# we have couple of datapoints near zero means no garage
numeric_data %>%
  filter(GarageArea>1200)
#filtered to see if there are outliers after 1200, few of them are outliers and few of them are not



#full bath above garde vs sale price 
ggplot(numeric_data, aes(FullBath, SalePrice))+geom_jitter(col="deeppink")+
  theme_bw()+geom_smooth(method="lm", se=F, color="forestgreen")+
  labs(title="Sale Price vs FullBath Above Grade",  x="FullBath Above Grade", y="House SalePrice")+
  theme(plot.title = element_text(hjust = 0.5))


#First floor sq ft vs SalePrice
ggplot(numeric_data, aes(X1stFlrSF, SalePrice))+geom_jitter(col="darkcyan")+
  theme_bw()+geom_smooth(method="lm", se=F, color="darkgoldenrod4")+
  labs(title="Sale Price vs First floor sq ft", x="1st Flr SF", y="House SalePrice")+
  theme( plot.title = element_text(hjust = 0.5))
#perfect line, correaltion exists

#Year remodel date vs SalePrice
ggplot(numeric_data, aes(YearRemodAdd, SalePrice))+geom_jitter(col="orange1")+
  theme_bw()+geom_smooth(method="lm", se=F, color="darkgoldenrod4")+
  labs(title="Sale Price vs Remodel Year", x="YearRemodAdd", y="House SalePrice")+
  theme( plot.title = element_text(hjust = 0.5))
#little bit of correaltion exists


#total basment area sq ft vs sale price
ggplot(numeric_data, aes(TotalBsmtSF, SalePrice))+geom_jitter(col="darkslateblue")+
  theme_bw()+geom_smooth(method="lm", se=F, color="orange")+
  labs(title="Sale Price vs Total Basement Area", y="House SalePrice")+
  theme(axis.text.x = element_text(angle=-90 , hjust=1), plot.title = element_text(hjust = 0.5))+
  scale_x_continuous("Total Basement Area", breaks = seq(0,6000,by = 1000))
#we have couple of datapoints near zero means no basement
#correalted to each other
numeric_data %>%
  filter(TotalBsmtSF>5000)
#by fltering total basement found it data point near 6000 is outlier(all other features are with high vlaues)  


#Garage cars vs SalePrice
ggplot(numeric_data, aes(GarageCars, SalePrice))+geom_jitter(col="darkcyan")+
  theme_bw()+geom_smooth(method="lm", se=F, color="darkgoldenrod4")+
  labs(title="Sale Price vs Garage Cars", x="GarageCars", y="House SalePrice")+
  theme( plot.title = element_text(hjust = 0.5))
#perfect line, correaltion exists



ggplot(numeric_data, aes(BedroomAbvGr, SalePrice))+geom_jitter(col="maroon3")+
  theme_bw()+geom_smooth(method="lm",se=F,color="yellow")+
  labs(title="Sale Price vs Bedroom Above Grade", x="Bedroom Above Grade", y="House SalePrice")+
  theme( plot.title = element_text(hjust = 0.5))
#line is not best fit line their is no correlation 


ggplot(numeric_data, aes(MoSold, SalePrice))+geom_jitter(col="deepskyblue3")+theme_bw()+
  labs(title="Sale Price vs Month Sold", y="House SalePrice")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_x_continuous("Month Sold", breaks = seq(0,12,by = 1))
#Month sold and saleprice are not correlated
# but can see in may, june, july saleprice is more specailly in july


#################################################################################
######################## DATA EXPLORATION: CATEGORICAL VARIABLE #################
#################################################################################

############## univariate and bivariate analysis ############

#filter only character and factor data
category_data = raw_dataset[, sapply(raw_dataset, class) %in% c('character', 'factor')]
category_data

# frequency table of all categorical variables
frequency_table = apply(category_data,2, table)
frequency_table

# use ggplot to plot all categorical variables with bar chart
category_data %>%
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_bar()


### MSZoning vs saleprice ###
ggplot(category_data, aes(MSZoning))+
  geom_bar(aes(fill=MSZoning), width =0.9)+
  labs(title = "Zoning Classification")+
  theme(plot.title = element_text(hjust = 0.5)) 
#Residential low density type is more

ggplot(raw_dataset, aes(MSZoning, SalePrice))+
  geom_boxplot(aes(fill=MSZoning), width =0.9)+
  labs(title = "MSZoning vs SalePrice")+
  theme(plot.title = element_text(hjust = 0.5))
#When MSZoning plotted with saleprice floating village prices are high
#Commerical usally are high but price is low
#Lot of outliers in residential low density



### OverallQual vs saleprice ####
ggplot(category_data, aes(OverallQual))+
  geom_bar(aes(fill=OverallQual), width =0.9)+
  labs(title = "OverallQual")+
  theme(plot.title = element_text(hjust = 0.5)) 
#Mostly the house sold are of average to good condition

ggplot(raw_dataset, aes(OverallQual, SalePrice))+
  geom_boxplot(aes(fill=OverallQual), width =0.9)+
  labs(title = "OverallQual vs SalePrice")+
  theme(plot.title = element_text(hjust = 0.5))
#price increase and condition increase



### MSSubClass  vs saleprice ####
ggplot(category_data, aes(MSSubClass))+
  geom_bar(aes(fill=MSSubClass), width =0.9)+
  labs(title = "House Type")+
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(raw_dataset, aes(MSSubClass, SalePrice))+
  geom_boxplot(aes(fill=MSSubClass), width =0.9)+
  labs(title = "House Type vs SalePrice")+
  theme(plot.title = element_text(hjust = 0.5))
#didnt find any realtionship but 
# 60 120 75 and 20 are having hight  saleprice
# 60 ->  2-story 1946 and newer
# 120 -> 1-story  1946 and newer
#75 -> 2-1/2 storyall ages
#20 -> 1-story  1946 and newer


###  Basement condition vs saleprice ####
ggplot(category_data, aes(BsmtCond))+
  geom_bar(aes(fill=BsmtCond), width =0.9)+
  labs(title = "Basement condition")+
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(raw_dataset, aes(BsmtCond, SalePrice))+
  geom_boxplot(aes(fill=BsmtCond), width =0.9)+
  labs(title = "Basement condition vs saleprice")+
  theme(plot.title = element_text(hjust = 0.5))
# no realtionship

###  Garage type vs saleprice ####
ggplot(category_data, aes(GarageType))+
  geom_bar(aes(fill=GarageType), width =0.9)+
  labs(title = "GarageType")+
  theme(axis.text.x = element_text(angle=-90 , hjust=1), plot.title = element_text(hjust = 0.5)) 

ggplot(raw_dataset, aes(GarageType, SalePrice))+
  geom_boxplot(aes(fill=GarageType), width =0.9)+
  labs(title = "GarageType vs SalePrice")+
  theme(axis.text.x = element_text(angle=-90 , hjust=1), plot.title = element_text(hjust = 0.5))
# built in (garage part of house) garage price is high, then attached garage



###  Garage condition vs saleprice ####
ggplot(category_data, aes(GarageCond))+
  geom_bar(aes(fill=GarageCond), width =0.9)+
  labs(title = "Garage condition")+
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(raw_dataset, aes(GarageCond, SalePrice))+
  geom_boxplot(aes(fill=GarageCond), width =0.9)+
  labs(title = "Garage condition vs SalePrice")+
  theme(plot.title = element_text(hjust = 0.5))
# interesting fact found 
# good condition garage price is low  - Gd
# average condition garage price is high - TA


###  Exterior type vs saleprice ####
ggplot(category_data, aes(ExterCond))+
  geom_bar(aes(fill=ExterCond), width =0.9)+
  labs(title = "Exterior condition")+
  theme(plot.title = element_text(hjust = 0.5)) 

ggplot(raw_dataset, aes(ExterCond, SalePrice))+
  geom_boxplot(aes(fill=ExterCond), width =0.9)+
  labs(title = "Exterior condition vs SalePrice")+
  theme(plot.title = element_text(hjust = 0.5))
# interesting fact found 
# Excellent(ex), good(gd) exterior condition have low price 
# average(TA) condition exterior condition is high price
#outliers present


ggplot(raw_dataset, aes(Neighborhood, SalePrice))+
  geom_boxplot(aes(fill=Neighborhood), width =0.9)+
  labs(title = "Neighborhood vs saleprice")+
  theme(axis.text.x = element_text(angle=-90 , hjust=1), plot.title = element_text(hjust = 0.5))

#################################################################################
################  RELATIONSHIP BETWEEN  VARIABLES   #####################
#################################################################################
cor_val = rcorr(as.matrix(numeric_data))
cor_val

#heat map for all numeric variables 
corrplot(cor_val$r, type = "lower",order = "hclust", tl.col = "black", tl.srt = 45)

#plotted pair plot of highly correalted numeric variable with sale price, frequency plot, scatter plot and cor val
ggpairs(raw_dataset, columns = c("SalePrice","OverallQual","GrLivArea", "GarageCars", "GarageArea", "TotalBsmtSF","X1stFlrSF",  "FullBath", "TotRmsAbvGrd",    "YearBuilt", "YearRemodAdd",  "GarageYrBlt","Fireplaces"),
        lower = list(continuous = "smooth"))





#################################################################################
################################  Feature Engineering   #########################
#################################################################################

#before starting store dataset in one variable 
clean_dataset = raw_dataset


#############################  Missing value ###############################

#checks which column have missing values, returns index of the column
NAcols = which(colSums(is.na(clean_dataset))>0)  

#returns the columns with total number of missing values 
colSums(sapply(clean_dataset[NAcols], is.na))   

length(NAcols)   

NA_dataset = clean_dataset[, NAcols]  #display's only na value data

#percentage of missing values columnwise 
cmiss = function(m)
{
  sum(is.na(m))/length(m)*100
}
sapply(NA_dataset, cmiss)


#visualization of misisng values 
missmap(NA_dataset, y.cex = 0.8, x.cex=0.8, gap.xaxis = 5, main="Missing Data Map")

aggr(NA_dataset, col=c('navyblue','red'), numbers=TRUE, sortVars=TRUE, labels=names(NA_dataset), cex.axis=.7, gap=0, ylab=c("Histogram of missing data","Pattern"))


###### work on clean dataset for cleaning part  

# 1) PoolQC (pool quality) column 
#NA   No Pool
#Replace NA with None
class(clean_dataset$PoolQC)   #it is factor
levels(clean_dataset$PoolQC)  #NA is not treated as level  

clean_dataset$PoolQC = addNA(clean_dataset$PoolQC) #so add NA as level 
levels(clean_dataset$PoolQC) = c(levels(clean_dataset$PoolQC), 'None')

clean_dataset$PoolQC[is.na(clean_dataset$PoolQC)] = 'None'   

levels(clean_dataset$PoolQC)  # check level again 

summary(clean_dataset$PoolQC)


# 2) MiscFeature (Miscellaneous feature) column
#Elev Elevator
#Gar2 2nd Garage (if not described in garage section)
#Othr Other
#Shed Shed (over 100 SF)
#TenC Tennis Court
#NA   None

clean_dataset$MiscFeature = addNA(clean_dataset$MiscFeature) 
levels(clean_dataset$MiscFeature) = c(levels(clean_dataset$MiscFeature), 'None')
clean_dataset$MiscFeature[is.na(clean_dataset$MiscFeature)] = 'None'   

# 3)Fence column 
#GdPrv  Good Privacy
#MnPrv  Minimum Privacy
#GdWo Good Wood
#MnWw Minimum Wood/Wire
#NA   No Fence

clean_dataset$Fence = addNA(clean_dataset$Fence) 
levels(clean_dataset$Fence) = c(levels(clean_dataset$Fence), 'None')
clean_dataset$Fence[is.na(clean_dataset$Fence)] = 'None'  


#4) Alley column
#Grvl Gravel
#Pave Paved
#NA   No alley access

clean_dataset$Alley = addNA(clean_dataset$Alley) 
levels(clean_dataset$Alley) = c(levels(clean_dataset$Alley), 'None')
clean_dataset$Alley[is.na(clean_dataset$Alley)] = 'None'  


#5) FireplaceQu column
clean_dataset$Fireplaces
clean_dataset$FireplaceQu

#if fireplaces is zero then obviously frieplacequ is NA 
clean_dataset$FireplaceQu = addNA(clean_dataset$FireplaceQu)
levels(clean_dataset$FireplaceQu) = c(levels(clean_dataset$FireplaceQu), 'None')
clean_dataset$FireplaceQu[is.na(clean_dataset$FireplaceQu)] = 'None'  


#6)  GarageType, GarageFinish, GarageQual, GarageCond
#if garage type is (NA - No garage) then obviously GarageFinish, GarageQual, GarageCond (NA- no garage)
# GarageYrBlt will be 0
clean_dataset$GarageType = addNA(clean_dataset$GarageType)
levels(clean_dataset$GarageType) = c(levels(clean_dataset$GarageType), 'None')
clean_dataset$GarageType[is.na(clean_dataset$GarageType)] = 'None'  

clean_dataset$GarageFinish = addNA(clean_dataset$GarageFinish)
levels(clean_dataset$GarageFinish) = c(levels(clean_dataset$GarageFinish), 'None')
clean_dataset$GarageFinish[is.na(clean_dataset$GarageFinish)] = 'None'  

clean_dataset$GarageQual = addNA(clean_dataset$GarageQual)
levels(clean_dataset$GarageQual) = c(levels(clean_dataset$GarageQual), 'None')
clean_dataset$GarageQual[is.na(clean_dataset$GarageQual)] = 'None' 

clean_dataset$GarageCond = addNA(clean_dataset$GarageCond)
levels(clean_dataset$GarageCond) = c(levels(clean_dataset$GarageCond), 'None')
clean_dataset$GarageCond[is.na(clean_dataset$GarageCond)] = 'None' 

clean_dataset$GarageYrBlt[is.na(clean_dataset$GarageYrBlt)] = 0  


#7) BsmtQual,BsmtCond,BsmtExposure,BsmtFinType1,BsmtFinType2
# NA means No baSement for above columns
clean_dataset$BsmtQual = addNA(clean_dataset$BsmtQual)
levels(clean_dataset$BsmtQual) = c(levels(clean_dataset$BsmtQual), 'None')
clean_dataset$BsmtQual[is.na(clean_dataset$BsmtQual)] = 'None'  

clean_dataset$BsmtCond = addNA(clean_dataset$BsmtCond)
levels(clean_dataset$BsmtCond) = c(levels(clean_dataset$BsmtCond), 'None')
clean_dataset$BsmtCond[is.na(clean_dataset$BsmtCond)] = 'None'  

clean_dataset$BsmtExposure = addNA(clean_dataset$BsmtExposure)
levels(clean_dataset$BsmtExposure) = c(levels(clean_dataset$BsmtExposure), 'None')
clean_dataset$BsmtExposure[is.na(clean_dataset$BsmtExposure)] = 'None'  

clean_dataset$BsmtFinType1 = addNA(clean_dataset$BsmtFinType1)
levels(clean_dataset$BsmtFinType1) = c(levels(clean_dataset$BsmtFinType1), 'None')
clean_dataset$BsmtFinType1[is.na(clean_dataset$BsmtFinType1)] = 'None'  

clean_dataset$BsmtFinType2 = addNA(clean_dataset$BsmtFinType2)
levels(clean_dataset$BsmtFinType2) = c(levels(clean_dataset$BsmtFinType2), 'None')
clean_dataset$BsmtFinType2[is.na(clean_dataset$BsmtFinType2)] = 'None'  


#8)  MasVnrType, MasVnrArea
#if MasVnrType is NA means None and MasVnrArea will be zero

clean_dataset$MasVnrType = addNA(clean_dataset$MasVnrType)
levels(clean_dataset$MasVnrType) = c(levels(clean_dataset$MasVnrType), 'None')
clean_dataset$MasVnrType[is.na(clean_dataset$MasVnrType)] = 'None'  

clean_dataset$MasVnrArea[is.na(clean_dataset$MasVnrArea)] = 0  


#9) Electrical
summary(clean_dataset$Electrical)
#only one column have missing value so we can replace with maximum occurance value

clean_dataset$Electrical[is.na(clean_dataset$Electrical)] = "SBrkr"  

#10) LotFrontage  column

#lotf_mean = clean_dataset %>%
#group_by(Neighborhood) %>%
#summarise(Lotf_val = mean(LotFrontage, na.rm=TRUE))%>% 
#print(n = 25)

#Replaced NA with mean of lotfrontage group by neighborhood
for (i in 1:nrow(clean_dataset)){
  if(is.na(clean_dataset$LotFrontage[i])){
    clean_dataset$LotFrontage[i] = as.integer(mean(clean_dataset$LotFrontage[clean_dataset$Neighborhood==clean_dataset$Neighborhood[i]], na.rm=TRUE)) 
  }
}

#check for null values again 
cmiss = function(m)
{
  sum(is.na(m))/length(m)*100
}
sapply(clean_dataset, cmiss)

any(is.na(clean_dataset))


#############################  outliers ###############################


#store clean dataset into variable outlier_treat
outlier_treat = clean_dataset



list_num = sapply(clean_dataset, is.numeric)
list_num

num_data = clean_dataset[, list_num_cols]
str(num_data)
head(num_data)

melt_df = melt(num_data, id="Id")
head(melt_df)

ggplot(melt_df, aes(factor(variable), value))+geom_boxplot() + facet_wrap(~variable, scale="free")
#boxplot to check outliers 

#to check on which index outlier are there 
which(clean_dataset$OverallQual %in% boxplot(clean_dataset$OverallQual)$out)
which(clean_dataset$GrLivArea %in% boxplot(clean_dataset$GrLivArea)$out)
which(clean_dataset$GarageCars %in% boxplot(clean_dataset$GarageCars)$out)
which(clean_dataset$GarageArea %in% boxplot(clean_dataset$GarageArea)$out)
which(clean_dataset$TotalBsmtSF %in% boxplot(clean_dataset$TotalBsmtSF)$out)
which(clean_dataset$X1stFlrSF %in% boxplot(clean_dataset$X1stFlrSF)$out)
which(clean_dataset$TotRmsAbvGrd %in% boxplot(clean_dataset$TotRmsAbvGrd)$out)
which(clean_dataset$YearBuilt %in% boxplot(clean_dataset$YearBuilt)$out)
which(clean_dataset$FullBath %in% boxplot(clean_dataset$FullBath)$out)
which(clean_dataset$Neighborhood %in% boxplot(clean_dataset$Neighborhood)$out)
which(clean_dataset$MSSubClass %in% boxplot(clean_dataset$MSSubClass)$out)
which(clean_dataset$GarageType %in% boxplot(clean_dataset$GarageType)$out)
which(clean_dataset$LotArea %in% boxplot(clean_dataset$LotArea)$out)



#create function for percentile capping
outlier_capping = function(x){
  for (i in which(sapply(x, is.numeric))) {
    quantiles <- quantile( x[,i], c(.05, .95 ), na.rm =TRUE)
    x[,i] = ifelse(x[,i] < quantiles[1] , quantiles[1], x[,i])
    x[,i] = ifelse(x[,i] > quantiles[2] , quantiles[2], x[,i])}
  x}

# Replacing extreme values with percentiles
capped_dataset = outlier_capping(outlier_treat)

summary(clean_dataset$GrLivArea)
summary(capped_dataset$GrLivArea)


#check whether outliers are there or not
#we can check with other variables as well
boxplot(clean_dataset$GrLivArea)
boxplot(capped_dataset$GrLivArea)

boxplot(clean_dataset$TotalBsmtSF)
boxplot(capped_dataset$TotalBsmtSF)

#we can check with other variables as well
which(capped_dataset$GrLivArea %in% boxplot(capped_dataset$GrLivArea)$out)

#to view dataset
View(capped_dataset)



#########################   transformation #########################

#filtering out numeric data to do transform
log_features = capped_dataset[ ,c("LotFrontage","LotArea","MasVnrArea","BsmtFinSF1","BsmtFinSF2","BsmtUnfSF",
                                  "TotalBsmtSF","X1stFlrSF","X2ndFlrSF","LowQualFinSF","GrLivArea",
                                  "BsmtFullBath","BsmtHalfBath","FullBath","HalfBath","BedroomAbvGr","KitchenAbvGr",
                                  "TotRmsAbvGrd","Fireplaces","GarageCars","GarageArea","WoodDeckSF","OpenPorchSF",
                                  "EnclosedPorch","X3SsnPorch","ScreenPorch","PoolArea","MiscVal")]

#select features that we donot want to transform
no_log_features = capped_dataset[, -which(names(capped_dataset) %in% names(log_features))]

#check the columns
dim(log_features)
dim(no_log_features)

#perform transformation 
log(log_features)


data_combine = cbind(log_features, no_log_features)
summary(data_combine)



#########################   Dummy encoding #########################


dmy = dummyVars(" ~ .", data = data_combine)
dummy_encoding = data.frame(predict(dmy, newdata = data_combine))
dummy_encoding

#check and view the dataset after dummy encoding
dim(dummy_encoding)
View(dummy_encoding)


#################  Removing levels with few or no observations ##############
# Variables with near zero variance

#returns zero variance variable
remove_cols  = nearZeroVar(dummy_encoding, names = T)
remove_cols

all_cols = names(dummy_encoding)
all_cols

no_zerovariance_dataset = dummy_encoding[,setdiff(all_cols, remove_cols)]
View(no_zerovariance_dataset)

#####################   feature selection ####################


#method one
set.seed(111)
boruta = Boruta(SalePrice ~ ., data = no_zerovariance_dataset, doTrace = 2, maxRuns = 500)
print(boruta)

plot(boruta, las = 2, cex.axis = 0.7)
plotImpHistory(boruta)

getSelectedAttributes(boruta, withTentative = F)


# Tentative Fix
bor <- TentativeRoughFix(boruta)
print(bor)
attStats(boruta)


View(no_zerovariance_dataset)

#method two
#Using random forest for variable selection
rfModel = randomForest(SalePrice~., data = no_zerovariance_dataset)
rfModel

# Getting the list of important variables
importance(rfModel)

#view the important variable 
varImpPlot(rfModel)

colnames(no_zerovariance_dataset)

##########   Dealing with skewness of response variable #############
no_zerovariance_dataset$SalePrice = log(no_zerovariance_dataset$SalePrice)


#histogram 
ggplot(no_zerovariance_dataset, aes(x = SalePrice, fill = ..count..)) +
  geom_histogram(bins = 30) +
  labs(title = "Histogram of SalePrice", y="Count of houses", x="SalePrice") +
  theme(plot.title = element_text(hjust = 0.5))


#another method to check distribution 
qqnorm(no_zerovariance_dataset$SalePrice, ylab="Sample Quantiles of SalePrice")
qqline(no_zerovariance_dataset$SalePrice, col="red")





###################################################################
################################  Model   #########################
###################################################################

#based on feature selection
model_dataset = no_zerovariance_dataset[ ,c("SalePrice","GrLivArea","TotalBsmtSF","X1stFlrSF","X2ndFlrSF",
                                            "YearBuilt","TotRmsAbvGrd","GarageArea","OverallQual.6","LotArea","YearRemodAdd",
                                            "GarageCars","FullBath","ExterQual.TA","KitchenQual.TA","Fireplaces",
                                            "BsmtFinSF1", "OpenPorchSF", "MSSubClass.60","MSZoning.RM","KitchenQual.Ex", "MSZoning.RL")]


#train and test data
set.seed(42)
partition = createDataPartition(y = model_dataset$SalePrice, p = 0.7, list = F)
trainingdata = model_dataset[partition, ]
testingdata =  model_dataset[-partition, ]


trControl =  trainControl(method="repeatedcv", number=10, repeats=3, search="random")




##########Repeated cross validation  - linear regression ##############

set.seed(222)
lm_model  = train(SalePrice~., data = trainingdata, method = "lm",
                  trControl = trControl)

print(lm_model)
plot(lm_model)

#predict the test values
predicted_values_lm = predict(lm_model, testingdata)
predicted_values_lm


#evaluation of model
#method one: correlation between predicted values by model and test data 
cor(predicted_values_lm,testingdata$SalePrice)

#method two: comparing RMSE values
#find the  error -> error = predicted - actual
error = (predicted_values_lm - testingdata$SalePrice)
#RMSE of both training and test data
RMSE_NewData_lm = sqrt(mean(error^2))
RMSE_lmmodel = lm_model$results$RMSE

#the RMSE values are close and they are not overfitting or underfitting as well
RMSE_lmmodel
RMSE_NewData_lm


#visualization of results 
plot(predicted_values_lm, testingdata$SalePrice)
qqPlot(predicted_values_lm)


##########Repeated cross validation  - random forest##############

set.seed(222)
rf_model  = train(SalePrice~., data = trainingdata, method = "rf",
                  trControl = trControl, importance = TRUE,
                  nodesize = 14, ntree=503)


print(rf_model)
plot(rf_model)


#predict the test values
predicted_values_rf = predict(rf_model, testingdata)
predicted_values_rf


#evaluation of model
#method one: correlation between predicted values by model and test data 
cor(predicted_values_rf,testingdata$SalePrice)

#method two: comparing RMSE values
#find the  error -> error = predicted - actual
error = (predicted_values_rf - testingdata$SalePrice)
#RMSE of both training and test data
RMSE_NewData_rf = sqrt(mean(error^2))
RMSE_rfModel = rf_model$results$RMSE

#the RMSE values are close and they are not overfitting or underfitting as well
RMSE_rfModel
RMSE_NewData_rf


#visualization of results 
plot(predicted_values_rf, testingdata$SalePrice)
qqPlot(predicted_values_rf)



##########Repeated cross validation  - xgboost##############

set.seed(222)
xgb_model  = train(SalePrice~., data = trainingdata, method = "xgbTree",
                   trControl = trControl, nround = 1000)

print(xgb_model)
plot(xgb_model)


#predict the test values
predicted_values_xgb = predict(xgb_model, testingdata)
predicted_values_xgb

#evaluation of model
#method one: correlation between predicted values by model and test data 
cor(predicted_values_xgb,testingdata$SalePrice)

#method two: comparing RMSE values
#find the  error -> error = predicted - actual
error = (predicted_values_xgb - testingdata$SalePrice)
#RMSE of both training and test data
RMSE_NewData_xgb = sqrt(mean(error^2))
RMSE_xgbmodel = xbg_model$results$RMSE


#the RMSE values are close and they are not overfitting or underfitting as well
RMSE_xgbmodel
RMSE_NewData_xgb

#visualization of results 
plot(predicted_values_xgb, testingdata$SalePrice)
qqPlot(predicted_values_xgb)

