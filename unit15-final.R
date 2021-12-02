

rm(list = ls())

library(vtable)
library(scales)
library(corrplot)
library(nortest)
library(randomForest)
library(foreign)
library(GGally)
library(haven)
library(magrittr)
library(data.table)
library(dplyr)
library(plyr)
library(nycflights13)
library(tidyverse)
library(datasets)
library(readxl)
library(maps)
library(plotly)
library(DT)
library(tidytext)
library(plyr)
library(factoextra)
library(readxl)
library(plotly)
library(naivebayes)
library(caTools)
library(devtools)
library(ggcorrplot)
library(usethis)
library(fastDummies)
library(recipes)
library(caretEnsemble)
library(readr)
library("gplots")
library(dominanceanalysis)
library(caTools)
library(randomForest) 
library(xgboost) 
library(data.table)
library(plyr)
library(nycflights13)
library(datasets)
library(readxl)
library(magrittr)
library(maps)
library(plotly)
library(plyr)
library(GGally)
library(readxl)
library(plotly)
library(graphics)
library(e1071)
library(caTools)
library(ggplot2)
library(caret)
library(caretEnsemble)
library(psych)
library(GGally)
library(rpart)
library(randomForest)
library(readr)
library(vtable)
library(scales)
library(gridExtra)
library(corrplot)
library(nortest)
library(class)
library(randomForest)
library(foreign)
library(foreign)
library(GGally)
library(data.table)
library(plyr)
library(ggmap)
library(nycflights13)
library(datasets)
library(readxl)
library(DataExplorer)
library(maps)
library(plotly)
library(plyr)
library(GGally)
library(readxl)
library(plotly)
library(mice)
library(caTools)
library(lattice)
library(ggcorrplot)
library(usethis)
library(fastDummies)
library(recipes)
library(GGally)
library(caretEnsemble)
library(Amelia)
library(GGally)
library(randomForest)
library(readr)
library(aod)
library("gplots")
library(caret)
library(dominanceanalysis)
library(caTools)
library(randomForest) # for fitting RFs
library(skimr)
library(GGally)
library(plotly)
library(viridis)
library(caret)
library(randomForest)
library(rpart.plot)
library(corrgram)
library(ggthemes)
library(treemap)
library(treemapify)
library(repr)
library(cowplot)
library(magrittr)
library(ggpubr)
library(RColorBrewer)
library(plotrix)
library(ggrepel)
library(forcats)
library(reshape2)
library(caTools)
library(tree)
library(rattle)
library(bitops)
options(repr.plot.width=8, repr.plot.height=6)
options(warn=-1)


#Lets import the data sets

df=read.csv("/Users/owner/Desktop/final_project/CaseStudy2-data.csv",stringsAsFactors = TRUE)

cs2.NoAttrition =  read.csv("/Users/owner/Desktop/final_project/CaseStudy2CompSet No Attrition.csv",stringsAsFactors = TRUE)

cs2.NoSalary = read.csv("/Users/owner/Desktop/final_project/CaseStudy2CompSet No Salary.csv",stringsAsFactors = TRUE)


#visualize the missing data

sum(is.na(df))
## [1] 0
(sum(is.na(df))/prod(dim(df)))*100

df %>% group_by(JobRole) %>% summarise(n=n()) %>% arrange(desc(n))


df$Educational_Levels <-  ifelse(df$Education == 1, "Without College D.",
                                 ifelse(df$Education == 2 , "College D.",
                                        ifelse(df$Education == 3, "Bachelors D.",
                                               ifelse(df$Education == 4, "Masters D.", "Phd D."))))

st(df)

#Define the type of variables

df$WorkLifeBalance  =  as.factor(df$WorkLifeBalance)
df$JobRole =  as.factor(df$JobRole  )
df$JobInvolvement=as.factor(df$JobInvolvement)
df$JobSatisfaction=as.factor(df$JobSatisfaction)
df$JobLevel=as.factor(df$JobLevel)
df$JobSatisfaction =  as.factor(df$JobSatisfaction)
df$ TrainingTimesLastYear  =  as.factor(df$TrainingTimesLastYear)
df$ PerformanceRating =  as.factor(df$ PerformanceRating)
df$StockOptionLevel =  as.factor(df$StockOptionLevel)
df$RelationshipSatisfaction =  as.factor(df$RelationshipSatisfaction)
df$Education =  as.factor(df$Education)
df$EnvironmentSatisfaction=as.factor(df$EnvironmentSatisfaction)
df$BusinessTravel=as.factor(df$BusinessTravel)
df$JobSatisfaction=as.factor(df$JobSatisfaction)
df$EnvironmentSatisfaction=as.factor(df$EnvironmentSatisfaction)
df$PerformanceRating=as.factor(df$PerformanceRating)
df$TrainingTimesLastYear=as.factor(df$TrainingTimesLastYear)
df$RelationshipSatisfaction=as.factor(df$RelationshipSatisfaction)
df$WorkLifeBalance=as.factor(df$WorkLifeBalance)
df$Attrition <- as.factor(df$Attrition)
table(df$Attrition)



cs2.NoAttrition$WorkLifeBalance  =  as.factor(cs2.NoAttrition$WorkLifeBalance)
cs2.NoAttrition$JobRole =  as.factor(cs2.NoAttrition$JobRole  )
cs2.NoAttrition$JobInvolvement=as.factor(cs2.NoAttrition$JobInvolvement)
cs2.NoAttrition$JobSatisfaction=as.factor(cs2.NoAttrition$JobSatisfaction)
cs2.NoAttrition$JobLevel=as.factor(cs2.NoAttrition$JobLevel)
cs2.NoAttrition$JobSatisfaction =  as.factor(cs2.NoAttrition$JobSatisfaction)
cs2.NoAttrition$ TrainingTimesLastYear  =  as.factor(cs2.NoAttrition$TrainingTimesLastYear)
cs2.NoAttrition$ PerformanceRating =  as.factor(cs2.NoAttrition$ PerformanceRating)
cs2.NoAttrition$StockOptionLevel =  as.factor(cs2.NoAttrition$StockOptionLevel)
cs2.NoAttrition$RelationshipSatisfaction =  as.factor(cs2.NoAttrition$RelationshipSatisfaction)
cs2.NoAttrition$Education =  as.factor(cs2.NoAttrition$Education)
cs2.NoAttrition$EnvironmentSatisfaction=as.factor(cs2.NoAttrition$EnvironmentSatisfaction)
cs2.NoAttrition$BusinessTravel=as.factor(cs2.NoAttrition$BusinessTravel)
cs2.NoAttrition$JobSatisfaction=as.factor(cs2.NoAttrition$JobSatisfaction)
cs2.NoAttrition$EnvironmentSatisfaction=as.factor(cs2.NoAttrition$EnvironmentSatisfaction)
cs2.NoAttrition$PerformanceRating=as.factor(cs2.NoAttrition$PerformanceRating)
cs2.NoAttrition$TrainingTimesLastYear=as.factor(cs2.NoAttrition$TrainingTimesLastYear)
cs2.NoAttrition$RelationshipSatisfaction=as.factor(cs2.NoAttrition$RelationshipSatisfaction)
cs2.NoAttrition$WorkLifeBalance=as.factor(cs2.NoAttrition$WorkLifeBalance)




cs2.NoSalary$WorkLifeBalance  =  as.factor(cs2.NoSalary$WorkLifeBalance)
cs2.NoSalary$JobRole =  as.factor(cs2.NoSalary$JobRole  )
cs2.NoSalary$JobInvolvement=as.factor(cs2.NoSalary$JobInvolvement)
cs2.NoSalary$JobSatisfaction=as.factor(cs2.NoSalary$JobSatisfaction)
cs2.NoSalary$JobLevel=as.factor(cs2.NoSalary$JobLevel)
cs2.NoSalary$JobSatisfaction =  as.factor(cs2.NoSalary$JobSatisfaction)
cs2.NoSalary$ TrainingTimesLastYear  =  as.factor(cs2.NoSalary$TrainingTimesLastYear)
cs2.NoSalary$ PerformanceRating =  as.factor(cs2.NoSalary$ PerformanceRating)
cs2.NoSalary$StockOptionLevel =  as.factor(cs2.NoSalary$StockOptionLevel)
cs2.NoSalary$RelationshipSatisfaction =  as.factor(cs2.NoSalary$RelationshipSatisfaction)
cs2.NoSalary$Education =  as.factor(cs2.NoSalary$Education)
cs2.NoSalary$EnvironmentSatisfaction=as.factor(cs2.NoSalary$EnvironmentSatisfaction)
cs2.NoSalary$BusinessTravel=as.factor(cs2.NoSalary$BusinessTravel)
cs2.NoSalary$JobSatisfaction=as.factor(cs2.NoSalary$JobSatisfaction)
cs2.NoSalary$EnvironmentSatisfaction=as.factor(cs2.NoSalary$EnvironmentSatisfaction)
cs2.NoSalary$PerformanceRating=as.factor(cs2.NoSalary$PerformanceRating)
cs2.NoSalary$TrainingTimesLastYear=as.factor(cs2.NoSalary$TrainingTimesLastYear)
cs2.NoSalary$RelationshipSatisfaction=as.factor(cs2.NoSalary$RelationshipSatisfaction)
cs2.NoSalary$WorkLifeBalance=as.factor(cs2.NoSalary$WorkLifeBalance)
cs2.NoSalary$Attrition <- factor(cs2.NoSalary$Attrition)

Attrition.Yes = subset(df, Attrition == "Yes")
Attrition.Yes
Attrition.No = subset(df, Attrition == "No")
#Plot data types
(d1 = as.data.frame(table(sapply(df, class))))


ggplot(d1, aes(x = Var1, y = Freq)) +
  geom_bar(stat = "identity", col = "blue", fill = "lightblue") +
  labs(x = "Type of Class", y = "Frequency", title = "Column type Frequency")+
  theme_bw()


st(df)


#Let’s have a better understanding about each feature through a correlation plot

cols = c("#4c86ad", "#f5dfb3")
df %>%
  dplyr::select(Attrition,MonthlyIncome,YearsSinceLastPromotion,YearsWithCurrManager,YearsAtCompany,YearsInCurrentRole,TotalWorkingYears ) %>%
  GGally::ggpairs(
    lower = list(
      continuous = GGally::wrap("points", col = cols[1],alpha=0.6),
      combo = GGally::wrap("box", fill = "white", col ="black")
    ),
    upper = list(
      continuous = GGally::wrap("cor", col = cols[1]),
      combo = GGally::wrap("facetdensity", col = "black")
    ),
    diag = list(
      continuous = GGally::wrap("barDiag", fill = cols[2], col ="black", bins = 18),
      discrete = GGally::wrap("barDiag", fill = cols[2], col ="black"))
  )


cols = c("#4c86ad", "#f5dfb3")
df %>%
  dplyr::select(Attrition,PercentSalaryHike,MonthlyIncome,HourlyRate,MonthlyRate,DistanceFromHome ) %>%
  GGally::ggpairs(
    lower = list(
      continuous = GGally::wrap("points", col = cols[1],alpha=0.6),
      combo = GGally::wrap("box", fill = "white", col ="black")
    ),
    upper = list(
      continuous = GGally::wrap("cor", col = cols[1]),
      combo = GGally::wrap("facetdensity", col = "black")
    ),
    diag = list(
      continuous = GGally::wrap("barDiag", fill = cols[2], col ="black", bins = 18),
      discrete = GGally::wrap("barDiag", fill = cols[2], col ="black"))
  )


#EDA of bivariate data

#Monthly Income by Gender
Income_by_Gender <- ggplot(df, aes(x=Gender, y=MonthlyIncome, color=Gender, fill=Gender)) + geom_boxplot() + 
  scale_fill_manual(values=c("#F5A9F2", "#5882FA")) + scale_color_manual(values=c("#FE2EF7", "#5858FA")) +
  coord_flip() + labs(title="Are there any Gender Disparities in Income?")
Income_by_Gender
#Plot the relationship between categorical variables with Attrition:
  
  #-------More graphs to explore the data-----
ggplot(df, aes(OverTime, ..count..)) + geom_bar(aes(fill = Attrition) ,position = "dodge")


ggplot(df, aes(JobInvolvement, ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")


ggplot(df, aes(JobSatisfaction, ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")


ggplot(df, aes(StockOptionLevel, ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")


ggplot(df, aes(NumCompaniesWorked, ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")


ggplot(df, aes(JobSatisfaction, ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")


ggplot(df, aes(JobSatisfaction, ..count..)) + geom_bar(aes(fill = JobRole), position = "dodge")


ggplot(df, aes(WorkLifeBalance , ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")


ggplot(df, aes(TrainingTimesLastYear  , ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")


ggplot(df, aes(PerformanceRating  , ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")


ggplot(df, aes(StockOptionLevel   , ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")


ggplot(df, aes(RelationshipSatisfaction    , ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")


ggplot(df, aes(EnvironmentSatisfaction    , ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")


ggplot(df, aes(Education, ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")


ggplot(df, aes(BusinessTravel, ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")


ggplot(df, aes(JobSatisfaction, ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")


ggplot(df, aes(EnvironmentSatisfaction, ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")


ggplot(df, aes(NumCompaniesWorked, ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")


ggplot(df, aes(PerformanceRating, ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")


ggplot(df, aes(RelationshipSatisfaction, ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")


ggplot(df, aes(TrainingTimesLastYear, ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")


ggplot(df, aes(WorkLifeBalance, ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")


ggplot(df, aes(OverTime, ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")


ggplot(df, aes(JobInvolvement, ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")


ggplot(df, aes(JobInvolvement, ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")


ggplot(df, aes(JobSatisfaction, ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")


ggplot(df, aes(JobLevel, ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")


ggplot(df, aes(OverTime, ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")


#distribution of job satisfaction in attrition.

ggplot(df, aes(JobSatisfaction, ..count..)) + geom_bar(aes(fill = Attrition), position = "dodge")


#————continious variables————-

#Evaluation the numeric variables in those with and without attrition using boxplot

ggplot(df, aes(x=Attrition, y=PercentSalaryHike)) +  geom_boxplot(fill='green')


ggplot(df, aes(x=Attrition, y=MonthlyIncome)) +  geom_boxplot(fill='green')
colnames(df)



ggplot(df, aes(x=JobRole, y=MonthlyIncome)) +  geom_boxplot(fill='green')


ggplot(df, aes(x=Attrition, y= HourlyRate)) +  geom_boxplot(fill='green')


ggplot(df, aes(x=JobSatisfaction, y= HourlyRate)) +  geom_boxplot(fill='green')


ggplot(df, aes(x=JobSatisfaction, y=MonthlyIncome)) +  geom_boxplot(fill='green')


ggplot(df, aes(x=Attrition, y=DistanceFromHome)) +  geom_boxplot(fill='green')


ggplot(df, aes(x=JobSatisfaction, y=DistanceFromHome)) +  geom_boxplot(fill='green')


ggplot(df, aes(x=Attrition, y=YearsSinceLastPromotion)) +  geom_boxplot(fill='green')


ggplot(df, aes(x=Attrition, y=YearsWithCurrManager)) +  geom_boxplot(fill='green')


ggplot(df, aes(x=Attrition, y=YearsAtCompany)) +  geom_boxplot(fill='green')


ggplot(df, aes(x=Attrition, y=YearsInCurrentRole)) +  geom_boxplot(fill='green')


ggplot(df, aes(x=Attrition, y=TotalWorkingYears)) +  geom_boxplot(fill='green')


#Evaluation the numeric variables in those with and without attrition using scatterplot

ggplot(df,aes(TotalWorkingYears,MonthlyIncome,color=Attrition))+
  geom_point(shape=4,size=2)+
  geom_smooth(method=lm,se=F)

ggplot(df,aes(YearsWithCurrManager,YearsInCurrentRole,color=Attrition))+
  geom_point(shape=4,size=2)+
  geom_smooth(method=loess,se=F)

ggplot(df,aes(YearsInCurrentRole,MonthlyIncome,color=Attrition))+
  geom_point(shape=4,size=2)+
  geom_smooth(method=loess,se=F)


ggplot(df,aes(YearsAtCompany,MonthlyIncome,color=Attrition))+
  geom_point(shape=4,size=2)+
  geom_smooth(method=loess,se=F)


ggplot(df,aes(YearsSinceLastPromotion,YearsInCurrentRole,color=Attrition))+
  geom_point(shape=4,size=2)+
  geom_smooth(method=loess,se=F)


ggplot(df,aes(YearsSinceLastPromotion,YearsInCurrentRole,color=Attrition))+
  geom_point(shape=4,size=2)+
  geom_smooth(method=loess,se=F)


ggplot(df,aes(TotalWorkingYears,YearsSinceLastPromotion,color=Attrition))+
  geom_point(shape=4,size=2)+
  geom_smooth(method=loess,se=F)


#Evaluation of monthly income using histogram

### 1. Monthly Income Variable
ggplot(df, aes(x = MonthlyIncome)) + 
  geom_histogram(aes(y = stat(density)), col = "blue", fill = "gold") + 
  geom_density(col = "red", size = 1) + 
  labs(x = "Monthly Income (Salary)",y = " ",
       title = "Histogram for Monthly Income") +
  theme_bw()


range(df$MonthlyIncome)
## [1]  1081 19999
# Monthly Income variable is not normally distributed and variable is positively skewed.
#2. Attrition variable
ggplot(df, aes(x = Attrition, y = prop.table(stat(count)),
               label = scales::percent(prop.table(stat(count))))) + 
  geom_bar(col = "blue", fill = "lightblue")+
  geom_text(stat = 'count', size = 5)+
  labs(y = "Frequency", title = "Barplot for Attrition") +
  theme_bw()


# In this data set 16% percent belongs to Attrition Yes group.
#3. Monthly Income and Attrition
ggplot(df, aes(x = Attrition, y = MonthlyIncome)) + 
  geom_boxplot(col = "blue", fill = "gold") + 
  labs(y = "Monthly Income (Salary)",x = "Attrition",
       title = "Boxplots of Monthly Income for Attrition groups") +
  theme_bw()


# According to the median values, employees who have lower salary tends to leave their current jobs.
#Income and Level of Attrition

options(repr.plot.width=8, repr.plot.height=7) 

per.sal <- df %>% select(Attrition, PercentSalaryHike, MonthlyIncome) %>% 
  ggplot(aes(x=PercentSalaryHike, y=MonthlyIncome)) + geom_jitter(aes(col=Attrition), alpha=0.5) + 
  theme_economist() + theme(legend.position="none") + scale_color_manual(values=c("#58FA58", "#FA5858")) + 
  labs(title="Income and its Impact on Attrition") + theme(plot.title=element_text(hjust=0.5, color="white"), plot.background=element_rect(fill="#0D7680"),
                                                           axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
                                                           axis.title=element_text(colour="white"))

perf.inc <- df %>% select(PerformanceRating, MonthlyIncome, Attrition) %>% group_by(factor(PerformanceRating), Attrition) %>% 
  ggplot(aes(x=factor(PerformanceRating), y=MonthlyIncome, fill=Attrition)) + geom_violin() + coord_flip() + facet_wrap(~Attrition) + 
  scale_fill_manual(values=c("#58FA58", "#FA5858")) + theme_economist() + 
  theme(legend.position="bottom", strip.background = element_blank(), strip.text.x = element_blank(), 
        plot.title=element_text(hjust=0.5, color="white"), plot.background=element_rect(fill="#0D7680"),
        axis.text.x=element_text(colour="white"), axis.text.y=element_text(colour="white"),
        axis.title=element_text(colour="white"), 
        legend.text=element_text(color="white")) + 
  labs(x="Performance Rating",y="Monthly Income") 


plot_grid(per.sal, perf.inc, nrow=2)


#4. Monthly Income and Age with Attrition
ggplot(df, aes(x = Age, y = MonthlyIncome)) + 
  geom_point(col = "blue") + 
  geom_smooth(method='lm', formula= y~x, se = F, col = "red", size = 1) +
  labs(y = "Monthly Income (Salary)",x = "Age",
       title = "Scatter plot for Monthly Income vs Age for Attrition") +
  theme_bw() + 
  facet_wrap( ~ Attrition)


cor(df$Age, df$MonthlyIncome)
## [1] 0.4842883
# There is a positive relationship between Monthly income and Age for both groups. When Age increases, Monthly income also increase. 
#5. Monthly Income and Business Travel groups | Attrition and Business Travel groups
ggplot(df, aes(x = BusinessTravel, fill = Attrition)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "Business Travel", y = "Attrition",
       title = "Business Travel with Attrition") + 
  theme_bw()


ggplot(df, aes(x = BusinessTravel, y = MonthlyIncome)) +
  geom_boxplot(fill = "gold") + 
  labs(x = "Business Travel", y = "Montly Income",
       title = "Montly Income for Business Travel groups") +
  theme_bw()


# Employees who are travel frequently have highest attrition rates and Non- travel group has lowest Income.
#6. Hourly Rate, Daily Rate and Monthly Rate with Monthly Income and Attrition
p1 = ggplot(df, aes(x = HourlyRate, y = MonthlyIncome)) +
  geom_point(col = "blue") + 
  geom_smooth(method='lm', formula= y~x, se = F, col = "red", size = 1) +
  labs(x = "Hourly Rate", y = "Monthly Income", title = "Monthly Income vs Hourly Rate") +
  theme_bw() + 
  facet_wrap( ~ Attrition)

p2 = ggplot(df, aes(x = DailyRate, y = MonthlyIncome)) +
  geom_point(col = "blue") + 
  geom_smooth(method='lm', formula= y~x, se = F, col = "red", size = 1) +
  labs(x = "Daily Rate", y = "Monthly Income", title = "Monthly Income vs Daily Rate") +
  theme_bw() +
  facet_wrap( ~ Attrition)

p3 = ggplot(df, aes(x = MonthlyRate, y = MonthlyIncome)) +
  geom_point(col = "blue") + 
  geom_smooth(method='lm', formula= y~x, se = F, col = "red", size = 1) + 
  labs(x = "Monthly Rate", y = "Monthly Income", title = "Monthly Income vs Monthly Rate") +
  theme_bw() + 
  facet_wrap( ~ Attrition)

grid.arrange(p1,p2,p3)


round(cor(Attrition.Yes[c("HourlyRate","DailyRate","MonthlyRate","MonthlyIncome","DistanceFromHome","TotalWorkingYears","YearsInCurrentRole","YearsAtCompany","YearsWithCurrManager","YearsSinceLastPromotion")]),2)

round(cor(Attrition.No[c("HourlyRate","DailyRate","MonthlyRate","MonthlyIncome","DistanceFromHome","TotalWorkingYears","YearsInCurrentRole","YearsAtCompany","YearsWithCurrManager","YearsSinceLastPromotion")]),2)

#7. Monthly Income and Department | Attrition and Department
ggplot(df, aes(x = Department, fill = Attrition)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "Department", y = "Attrition",
       title = "Department with Attrition") + 
  theme_bw()


ggplot(df, aes(x = Department, y = MonthlyIncome)) +
  geom_boxplot(fill = "gold") + 
  labs(x = "Department", y = "Montly Income",
       title = "Montly Income for Departments") +
  theme_bw()



#8. Distance from home with Monthly Income and Attrition
ggplot(df, aes(x = DistanceFromHome, y = MonthlyIncome)) +
  geom_point(col = "blue") + 
  geom_smooth(method='lm', formula= y~x, se = F, col = "red", size = 1) +
  labs(x = "Distance From Home", y = "Monthly Income", 
       title = "Monthly Income vs Distance From Home") +
  theme_bw() + 
  facet_wrap( ~ Attrition)


cor(Attrition.Yes$DistanceFromHome, Attrition.Yes$MonthlyIncome)

cor(Attrition.No$DistanceFromHome, Attrition.No$MonthlyIncome)

#There is no relationship between Distance From Home and Monthly Income for both Attrition groups.
#9. Monthly Income and Education | Attrition and Education
df$Education = as.factor(df$Education)

ggplot(df, aes(x = Education, fill = Attrition)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "Education", y = "Attrition",
       title = "Education with Attrition") + 
  theme_bw()


ggplot(df, aes(x = Education, y = MonthlyIncome)) +
  geom_boxplot(fill = "gold") + 
  labs(x = "Education", y = "Montly Income",
       title = "Montly Income for Education") +
  theme_bw()


# Highest attrition rates has education level 1. 
# Highest median income has education level 5 group.
#10. Monthly Income and Education Filed | Attrition and Education Filed
ggplot(df, aes(x = EducationField, fill = Attrition)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "Education Filed", y = "Attrition",
       title = "Education Filed with Attrition") + 
  theme_bw()


ggplot(df, aes(x = EducationField, y = MonthlyIncome)) +
  geom_boxplot(fill = "gold") + 
  labs(x = "Education Filed", y = "Montly Income",
       title = "Montly Income for Education Filed") +
  theme_bw()


# Highest attrition rates has Human Resource education field. 
# Highest median income has Marketing group.
#11. Monthly Income and Environment Satisfaction | Attrition and Environment Satisfaction
df$EnvironmentSatisfaction = as.factor(df$EnvironmentSatisfaction)
ggplot(df, aes(x = EnvironmentSatisfaction, fill = Attrition)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "Environment Satisfaction", y = "Attrition",
       title = "Environment Satisfaction with Attrition") + 
  theme_bw()


ggplot(df, aes(x = EnvironmentSatisfaction, y = MonthlyIncome)) +
  geom_boxplot(fill = "gold") + 
  labs(x = "Environment Satisfaction", y = "Montly Income",
       title = "Montly Income for Environment Satisfaction") +
  theme_bw()


# Employees who are less satisfy about their environment has higher attrition rates. 
# Median incomes are very similar in all satisfaction levels.
#12. Monthly Income and Gender | Attrition and Gender
ggplot(df, aes(x = Gender, fill = Attrition)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "Gender", y = "Attrition",
       title = "Gender with Attrition") + 
  theme_bw()


ggplot(df, aes(x = Gender, y = MonthlyIncome)) +
  geom_boxplot(fill = "gold") + 
  labs(x = "Gender", y = "Montly Income",
       title = "Montly Income for Gender") +
  theme_bw()


# Both male and female group has similar attrition rates.
#13. Monthly Income and Job Involvement | Attrition and Job Involvement
df$JobInvolvement = as.factor(df$JobInvolvement)

ggplot(df, aes(x = JobInvolvement, fill = Attrition)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "Job Involvement", y = "Attrition",
       title = "Job Involvement with Attrition") + 
  theme_bw()


ggplot(df, aes(x = JobInvolvement, y = MonthlyIncome)) +
  geom_boxplot(fill = "gold") + 
  labs(x = "Job Involvement", y = "Montly Income",
       title = "Montly Income for Job Involvement") +
  theme_bw()


# Employees with lower job involvement, have very higher attrition rate.
#14. Monthly Income and Job Involvement | Attrition and Job Involvement
df$JobLevel = as.factor(df$JobLevel)

ggplot(df, aes(x = JobLevel, fill = Attrition)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "Job Level", y = "Attrition",
       title = "Job Level with Attrition") + 
  theme_bw()


ggplot(df, aes(x = JobLevel, y = MonthlyIncome)) +
  geom_boxplot(fill = "gold") + 
  labs(x = "Job Level", y = "Montly Income",
       title = "Montly Income for Job Level") +
  theme_bw()


# Employees who are in job level 1 have highest attrition rate.
# When job level increases, monthly income also increases.
#15. Monthly Income and Job Role | Attrition and Job Role
ggplot(df, aes(x = JobRole, fill = Attrition)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "Job Role", y = "Attrition",
       title = "Job Role with Attrition") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))


ggplot(df, aes(x = JobRole, y = MonthlyIncome)) +
  geom_boxplot(fill = "gold") + 
  labs(x = "Job Role", y = "Montly Income",
       title = "Montly Income for Job Role") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))


# Sales representatives have higher attrition rate and lowest income.
# Managers abd Research directors have highest monthly income and lower attrition rate.
#16. Monthly Income and Job Satisfaction | Attrition and Job Satisfaction
df$JobSatisfaction = as.factor(df$JobSatisfaction)

ggplot(df, aes(x = JobSatisfaction, fill = Attrition)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "Job Satisfaction", y = "Attrition",
       title = "Job Satisfaction with Attrition") + 
  theme_bw() 


ggplot(df, aes(x = JobSatisfaction, y = MonthlyIncome)) +
  geom_boxplot(fill = "gold") + 
  labs(x = "Job Satisfaction", y = "Montly Income",
       title = "Montly Income for Job Satisfaction") +
  theme_bw() 


# Employees who are less satisfy with their jobs have higher attrition rates.
#17. Monthly Income and Marital Status | Attrition and Marital Status
ggplot(df, aes(x = MaritalStatus, fill = Attrition)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "Marital Status", y = "Attrition",
       title = "Marital Status with Attrition") + 
  theme_bw() 


ggplot(df, aes(x = MaritalStatus, y = MonthlyIncome)) +
  geom_boxplot(fill = "gold") + 
  labs(x = "Marital Status", y = "Montly Income",
       title = "Montly Income for Marital Status") +
  theme_bw() 


# Single employees have highest attrition rates.
#18. Monthly Income and Over Time | Attrition and Over Time
ggplot(df, aes(x = OverTime, fill = Attrition)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "Over Time", y = "Attrition",
       title = "Over Time with Attrition") + 
  theme_bw() 


ggplot(df, aes(x = OverTime, y = MonthlyIncome)) +
  geom_boxplot(fill = "gold") + 
  labs(x = "Over Time", y = "Montly Income",
       title = "Montly Income for Over Time") +
  theme_bw() 


# Employees who work over time have higher attrition rate and lower monthly income.
#19. Number of Companies Worked with Monthly Income and Attrition
ggplot(df, aes(x = NumCompaniesWorked, y = MonthlyIncome)) +
  geom_point(col = "blue") + 
  geom_smooth(method='lm', formula= y~x, se = F, col = "red", size = 1) +
  labs(x = "Number of Companies Worked", y = "Monthly Income", 
       title = "Monthly Income vs Number of Companies Worked") +
  theme_bw() + 
  facet_wrap( ~ Attrition)


cor(Attrition.Yes$NumCompaniesWorked, Attrition.Yes$MonthlyIncome)

cor(Attrition.No$NumCompaniesWorked, Attrition.No$MonthlyIncome)

# There is a poor relationship between Monthly income and number of companies worked.
#20. Percent Salary Hike with Monthly Income and Attrition
ggplot(df, aes(x = PercentSalaryHike, y = MonthlyIncome)) +
  geom_point(col = "blue") + 
  geom_smooth(method='lm', formula= y~x, se = F, col = "red", size = 1) +
  labs(x = "Percent Salary Hike", y = "Monthly Income", 
       title = "Monthly Income vs Percent Salary Hike") +
  theme_bw() + 
  facet_wrap( ~ Attrition)


cor(Attrition.Yes$PercentSalaryHike, Attrition.Yes$MonthlyIncome)
## [1] -0.03764575
cor(Attrition.No$PercentSalaryHike, Attrition.No$MonthlyIncome)

# It seems that Percent Salary Hike increases, monthly income decrease.
#21. Monthly Income and Relationship Satisfaction | Attrition and Relationship Satisfaction
df$RelationshipSatisfaction = as.factor(df$RelationshipSatisfaction)

ggplot(df, aes(x = RelationshipSatisfaction, fill = Attrition)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "Relationship Satisfaction", y = "Attrition",
       title = "Relationship Satisfaction with Attrition") + 
  theme_bw() 


ggplot(df, aes(x = RelationshipSatisfaction, y = MonthlyIncome)) +
  geom_boxplot(fill = "gold") + 
  labs(x = "Relationship Satisfaction", y = "Montly Income",
       title = "Montly Income for Relationship Satisfaction") +
  theme_bw() 


# Employees who have low relationship satisfaction, have a higher attrition rate. But median incomes are similar in all levels.
#22. Monthly Income and Stock Option Level | Attrition and Stock Option Level
df$StockOptionLevel = as.factor(df$StockOptionLevel)

ggplot(df, aes(x = StockOptionLevel, fill = Attrition)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "Stock Option Level", y = "Attrition",
       title = "Stock Option Level with Attrition") + 
  theme_bw() 


ggplot(df, aes(x = StockOptionLevel, y = MonthlyIncome)) +
  geom_boxplot(fill = "gold") + 
  labs(x = "Stock Option Level", y = "Montly Income",
       title = "Montly Income for Stock Option Level") +
  theme_bw() 


# Stock option levels 0 and 3 have higher attrition rates and lower median incomes.
#23. Total Working Years with Monthly Income and Attrition
ggplot(df, aes(x = TotalWorkingYears, y = MonthlyIncome)) +
  geom_point(col = "blue") + 
  geom_smooth(method='lm', formula= y~x, se = F, col = "red", size = 1) +
  labs(x = "Total Working Years", y = "Monthly Income", 
       title = "Monthly Income vs Total Working Years") +
  theme_bw() + 
  facet_wrap( ~ Attrition)


cor(Attrition.Yes$TotalWorkingYears, Attrition.Yes$MonthlyIncome)

cor(Attrition.No$TotalWorkingYears, Attrition.No$MonthlyIncome)

# There is strong positive relationship between Total Working Years and Monthly Income both attrition groups.
#24. Training Times Last Year with Monthly Income and Attrition
ggplot(df, aes(x = TrainingTimesLastYear, y = MonthlyIncome)) +
  geom_point(col = "blue") + 
  geom_smooth(method='lm', formula= y~x, se = F, col = "red", size = 1) +
  labs(x = "Training Times Last Year", y = "Monthly Income", 
       title = "Monthly Income vs Training Times Last Year") +
  theme_bw() + 
  facet_wrap( ~ Attrition)


#25. Monthly Income and Work Life Balance | Attrition and Work Life Balance
df$WorkLifeBalance = as.factor(df$WorkLifeBalance)

ggplot(df, aes(x = WorkLifeBalance, fill = Attrition)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent) + 
  labs(x = "Work Life Balance", y = "Attrition",
       title = "Work Life Balance with Attrition") + 
  theme_bw() 


ggplot(df, aes(x = WorkLifeBalance, y = MonthlyIncome)) +
  geom_boxplot(fill = "gold") + 
  labs(x = "Work Life Balance", y = "Montly Income",
       title = "Montly Income for Work Life Balance") +
  theme_bw() 


# Employees who have poor work life balance have higher attrition rate and lowest median income.
#26. Years At Company with Monthly Income and Attrition
ggplot(df, aes(x = YearsAtCompany, y = MonthlyIncome)) +
  geom_point(col = "blue") + 
  geom_smooth(method='lm', formula= y~x, se = F, col = "red", size = 1) +
  labs(x = "Years At Company", y = "Monthly Income", 
       title = "Monthly Income vs Years At Company") +
  theme_bw() + 
  facet_wrap( ~ Attrition)


cor(Attrition.Yes$YearsAtCompany, Attrition.Yes$MonthlyIncome)
## [1] 0.456972
cor(Attrition.No$YearsAtCompany, Attrition.No$MonthlyIncome)
## [1] 0.6450931
# There is higher positive relationship between Years At Company and Monthly Income both attrition groups.
#27. Years In Current Role with Monthly Income and Attrition
ggplot(df, aes(x = YearsInCurrentRole, y = MonthlyIncome)) +
  geom_point(col = "blue") + 
  geom_smooth(method='lm', formula= y~x, se = F, col = "red", size = 1) +
  labs(x = "Years In Current Role", y = "Monthly Income", 
       title = "Monthly Income vs Years In Current Role") +
  theme_bw() + 
  facet_wrap( ~ Attrition)


cor(Attrition.Yes$YearsInCurrentRole, Attrition.Yes$MonthlyIncome)

cor(Attrition.No$YearsInCurrentRole, Attrition.No$MonthlyIncome)

# There is positive relationship between Years In Current Role and Monthly Income both attrition groups.
# Employees who work more than 15 years in current role, do not leave the company.
#28. Years Since Last Promotion with Monthly Income and Attrition
ggplot(df, aes(x = YearsSinceLastPromotion, y = MonthlyIncome)) +
  geom_point(col = "blue") + 
  geom_smooth(method='lm', formula= y~x, se = F, col = "red", size = 1) +
  labs(x = "Years Since Last Promotion", y = "Monthly Income", 
       title = "Monthly Income vs Years Since Last Promotion") +
  theme_bw() + 
  facet_wrap( ~ Attrition)


cor(Attrition.Yes$YearsSinceLastPromotion, Attrition.Yes$MonthlyIncome)

cor(Attrition.No$YearsSinceLastPromotion, Attrition.No$MonthlyIncome)

# There is positive relationship between Years Since Last Promotion and Monthly Income both attrition groups.
#29. Years With Current Manager with Monthly Income and Attrition
ggplot(df, aes(x = YearsWithCurrManager, y = MonthlyIncome)) +
  geom_point(col = "blue") + 
  geom_smooth(method='lm', formula= y~x, se = F, col = "red", size = 1) +
  labs(x = "Years With Current Manager", y = "Monthly Income", 
       title = "Monthly Income vs Years With Current Manager") +
  theme_bw() + 
  facet_wrap( ~ Attrition)


cor(Attrition.Yes$YearsWithCurrManager, Attrition.Yes$MonthlyIncome)

cor(Attrition.No$YearsWithCurrManager, Attrition.No$MonthlyIncome)

#———evaluate the normality of variables———-

shapiro.test(df$MonthlyRate)
## 
##  Shapiro-Wilk normality test
## 
## data:  df$MonthlyRate

shapiro.test(df$PercentSalaryHike)
## 
##  Shapiro-Wilk normality test
## 
## data:  df$PercentSalaryHike
## 
shapiro.test(df$MonthlyIncome)
## 
##  Shapiro-Wilk normality test
## 
## data:  df$MonthlyIncome

shapiro.test(df$HourlyRate)
## 
##  Shapiro-Wilk normality test
## 
## data:  df$HourlyRate
## W = 0.95517, p-value = 1.221e-15
shapiro.test(df$YearsSinceLastPromotion)
## 
##  Shapiro-Wilk normality test
## 
## data:  df$YearsSinceLastPromotion
## W = 0.70474, p-value < 2.2e-16
shapiro.test(df$YearsWithCurrManager)
## 
##  Shapiro-Wilk normality test
## 
## data:  df$YearsWithCurrManager
## W = 0.89891, p-value < 2.2e-16
shapiro.test(df$YearsAtCompany)
## 
##  Shapiro-Wilk normality test
## 
## data:  df$YearsAtCompany
## W = 0.85504, p-value < 2.2e-16
shapiro.test(df$YearsInCurrentRole)
## 
##  Shapiro-Wilk normality test
## 
## data:  df$YearsInCurrentRole
## W = 0.89509, p-value < 2.2e-16
shapiro.test(df$TotalWorkingYears)
## 
##  Shapiro-Wilk normality test
## 
## data:  df$TotalWorkingYears
## W = 0.90948, p-value < 2.2e-16
shapiro.test(df$NumCompaniesWorked)
## 
##  Shapiro-Wilk normality test
## 
## data:  df$NumCompaniesWorked
## W = 0.84746, p-value < 2.2e-16
#———————————- # Man Whitney test to evaluate the difference between numeric variables in people with and without Attrition:

wilcox.test(NumCompaniesWorked ~ Attrition,data = df, alternative = "two.sided")
## 
##  Wilcoxon rank sum test with continuity correction
## 
## data:  NumCompaniesWorked by Attrition

## alternative hypothesis: true location shift is not equal to 0
wilcox.test(MonthlyIncome ~ Attrition,data = df, alternative = "two.sided")
## 
##  Wilcoxon rank sum test with continuity correction
## 
## data:  MonthlyIncome by Attrition
## alternative hypothesis: true location shift is not equal to 0
wilcox.test(PercentSalaryHike ~ Attrition,data = df, alternative = "two.sided")
## 
##  Wilcoxon rank sum test with continuity correction
## 
## data:  PercentSalaryHike by Attrition
## alternative hypothesis: true location shift is not equal to 0
wilcox.test(HourlyRate ~ Attrition,data = df, alternative = "two.sided")
## 
##  Wilcoxon rank sum test with continuity correction
## 
## data:  HourlyRate by Attrition
## alternative hypothesis: true location shift is not equal to 0
wilcox.test(DistanceFromHome ~ Attrition,data = df, alternative = "two.sided")
## 
##  Wilcoxon rank sum test with continuity correction
## 
## data:  DistanceFromHome by Attrition
## alternative hypothesis: true location shift is not equal to 0
wilcox.test(YearsSinceLastPromotion ~ Attrition,data = df, alternative = "two.sided")
## 
##  Wilcoxon rank sum test with continuity correction
## 
## data:  YearsSinceLastPromotion by Attrition
## alternative hypothesis: true location shift is not equal to 0
wilcox.test(YearsWithCurrManager ~ Attrition,data = df, alternative = "two.sided")
## 
##  Wilcoxon rank sum test with continuity correction
## 
## data:  YearsWithCurrManager by Attrition
## alternative hypothesis: true location shift is not equal to 0
wilcox.test(YearsAtCompany ~ Attrition,data = df, alternative = "two.sided")
## 
##  Wilcoxon rank sum test with continuity correction
## 
## data:  YearsAtCompany by Attrition
## W = 66124, p-value = 3.11e-08
## alternative hypothesis: true location shift is not equal to 0
wilcox.test(YearsInCurrentRole ~ Attrition,data = df, alternative = "two.sided")
## 
##  Wilcoxon rank sum test with continuity correction
## 
## data:  YearsInCurrentRole by Attrition
## W = 65436, p-value = 9.482e-08
## alternative hypothesis: true location shift is not equal to 0
wilcox.test(TotalWorkingYears ~ Attrition,data = df, alternative = "two.sided")
## 
##  Wilcoxon rank sum test with continuity correction
## 
## data:  TotalWorkingYears by Attrition
## alternative hypothesis: true location shift is not equal to 0
wilcox.test(MonthlyRate ~ Attrition,data = df, alternative = "two.sided")
## 
##  Wilcoxon rank sum test with continuity correction
## 
## data:  MonthlyRate by Attrition
## alternative hypothesis: true location shift is not equal to 0
#MonthlyIncome(p-value = 4.074e-09),DistanceFromHome(p-value = 0.02725),
#YearsWithCurrManager(p-value = 9.347e-07),YearsAtCompany(p-value = 3.11e-08),
#YearsInCurrentRole(p-value =9.482e-08),TotalWorkingYears(p-value = 4.042e-09) 
#were different with those with Attrition and those without Attrition
#Evaluate the relationship between categorical variables with Attrition using chi chisquare test

chisq.test(df$JobSatisfaction,df$Attrition)
## 
##  Pearson's Chi-squared test
## 
## data:  df$JobSatisfaction and df$Attrition
## X-squared = 11.109, df = 3, p-value = 0.01115
chisq.test(df$OverTime,df$Attrition)
## 
##  Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  df$OverTime and df$Attrition
## X-squared = 62.762, df = 1, p-value = 2.333e-15
chisq.test(df$JobInvolvement,df$Attrition)
## 
##  Pearson's Chi-squared test
## 
## data:  df$JobInvolvement and df$Attrition
chisq.test(df$StockOptionLevel,df$Attrition)
## 
##  Pearson's Chi-squared test
## 
## data:  df$StockOptionLevel and df$Attrition
## X-squared = 56.245, df = 3, p-value = 3.724e-12
chisq.test(df$NumCompaniesWorked,df$Attrition)
## 
##  Pearson's Chi-squared test
## 
## data:  df$NumCompaniesWorked and df$Attrition
chisq.test(df$WorkLifeBalance,df$Attrition)
## 
##  Pearson's Chi-squared test
## 
## data:  df$WorkLifeBalance and df$Attrition
chisq.test(df$TrainingTimesLastYear,df$Attrition)
## 
##  Pearson's Chi-squared test
## 
## data:  df$TrainingTimesLastYear and df$Attrition
chisq.test(df$PerformanceRating,df$Attrition)
## 
##  Pearson's Chi-squared test with Yates' continuity correction
## 
## data:  df$PerformanceRating and df$Attrition
chisq.test(df$RelationshipSatisfaction,df$Attrition)
## 
##  Pearson's Chi-squared test
## 
## data:  df$RelationshipSatisfaction and df$Attrition
chisq.test(df$EnvironmentSatisfaction,df$Attrition)
## 
##  Pearson's Chi-squared test
## 
## data:  df$EnvironmentSatisfaction and df$Attrition
chisq.test(df$JobLevel,df$Attrition)
## 
##  Pearson's Chi-squared test
## 
## data:  df$JobLevel and df$Attrition
chisq.test(df$JobRole,df$Attrition)
## 
##  Pearson's Chi-squared test
## 
## data:  df$JobRole and df$Attrition
## X-squared = 60.543, df = 8, p-value = 3.647e-10
chisq.test(df$Department,df$Attrition)
## 
##  Pearson's Chi-squared test
## 
## data:  df$Department and df$Attrition
chisq.test(df$NumCompaniesWorked,df$Attrition)
## 
##  Pearson's Chi-squared test
## 
## data:  df$NumCompaniesWorked and df$Attrition
#JobSatisfaction(p-value = 0.01115), OverTime(p-value = 2.333e-15),JobInvolvement(p-value = 5.211e-09),
# StockOptionLevel(p-value = 3.724e-12),WorkLifeBalance(p-value = 0.002495),df$EnvironmentSatisfaction(p-value = 0.01054) 
#Department(p-value = 0.009424),df$JobRole(p-value = 3.647e-10),NumCompaniesWorked(p-value = 0.01678)
#Evaluation the correlations with Monthly Income
cor.test(df$Age, df$MonthlyIncome,
         method= "spearman",
         exact=FALSE,
         alternative="two.side")
## 
##  Spearman's rank correlation rho
## 
## data:  df$Age and df$MonthlyIncome
## S = 60400498, p-value < 2.2e-16
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##       rho 
## 0.4496556
#There is a positive correlation between age and income(R2=0.44,p-value < 2.2e-16)
cor.test(df$PercentSalaryHike, df$MonthlyIncome,
         method= "spearman",
         exact=FALSE,
         alternative="two.side")
## 
##  Spearman's rank correlation rho
## 
## data:  df$PercentSalaryHike and df$MonthlyIncome
## S = 116154632, p-value = 0.0854
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##         rho 
## -0.05835313
#There is a weak correlation between PercentSalaryHike and income
cor.test(df$HourlyRate, df$MonthlyIncome,
         method= "spearman",
         exact=FALSE,
         alternative="two.side")
## 
##  Spearman's rank correlation rho
## 
## data:  df$HourlyRate and df$MonthlyIncome
## S = 110777571, p-value = 0.7828
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##         rho 
## -0.00935957
#There is a weak correlation between HourlyRate and income
cor.test(df$DistanceFromHome, df$MonthlyIncome,
         method= "spearman",
         exact=FALSE,
         alternative="two.side")
## 
##  Spearman's rank correlation rho
## 
## data:  df$DistanceFromHome and df$MonthlyIncome
## S = 107844895, p-value = 0.6091
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##        rho 
## 0.01736176
#There is a weak correlation between DistanceFromHome and income
cor.test(df$YearsSinceLastPromotion, df$MonthlyIncome,
         method= "spearman",
         exact=FALSE,
         alternative="two.side")
## 
##  Spearman's rank correlation rho
## 
## data:  df$YearsSinceLastPromotion and df$MonthlyIncome
## S = 80301098, p-value = 8.191e-16
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##       rho 
## 0.2683295
#There is a positive correlation between YearsSinceLastPromotion and income(R2=0.2683295,p-value = 8.191e-16)
cor.test(df$YearsWithCurrManager, df$MonthlyIncome,
         method= "spearman",
         exact=FALSE,
         alternative="two.side")
## 
##  Spearman's rank correlation rho
## 
## data:  df$YearsWithCurrManager and df$MonthlyIncome
## S = 69749771, p-value < 2.2e-16
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##       rho 
## 0.3644688
#There is a positive correlation between YearsWithCurrManager and income(R2=0.3644688, p-value < 2.2e-16)
cor.test(df$YearsAtCompany, df$MonthlyIncome,
         method= "spearman",
         exact=FALSE,
         alternative="two.side")
## 
##  Spearman's rank correlation rho
## 
## data:  df$YearsAtCompany and df$MonthlyIncome
## S = 59312414, p-value < 2.2e-16
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##       rho 
## 0.4595697
#There is a positive correlation between YearsAtCompany and income(R2=0.4595697 , p-value < 2.2e-16)
cor.test(df$YearsInCurrentRole, df$MonthlyIncome,
         method= "spearman",
         exact=FALSE,
         alternative="two.side")
## 
##  Spearman's rank correlation rho
## 
## data:  df$YearsInCurrentRole and df$MonthlyIncome
## S = 65683339, p-value < 2.2e-16
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##       rho 
## 0.4015205
#There is a positive correlation between YearsInCurrentRole and income(R2=0.4015205  , p-value < 2.2e-16)
cor.test(df$TotalWorkingYears, df$MonthlyIncome,
         method= "spearman",
         exact=FALSE,
         alternative="two.side")
## 
##  Spearman's rank correlation rho
## 
## data:  df$TotalWorkingYears and df$MonthlyIncome
## S = 31181970, p-value < 2.2e-16
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##       rho 
## 0.7158827
#There is a strong positive correlation between TotalWorkingYears and income(R2=0.7158827   , p-value < 2.2e-16)
cor.test(df$MonthlyRate, df$MonthlyIncome,
         method= "spearman",
         exact=FALSE,
         alternative="two.side")
## 
##  Spearman's rank correlation rho
## 
## data:  df$MonthlyRate and df$MonthlyIncome
## S = 100095425, p-value = 0.009429
## alternative hypothesis: true rho is not equal to 0
## sample estimates:
##        rho 
## 0.08797174
#-------------------------------
#Since MonthlyIncome variable is not normally distributed we use Kruskal-Wallis tests and Mann whitney U test for following analysis.
#1. Monthly Income vs Attrition
wilcox.test(MonthlyIncome ~ Attrition,data = df, alternative = "two.sided")
## 
##  Wilcoxon rank sum test with continuity correction
## 
## data:  MonthlyIncome by Attrition
## W = 67118, p-value = 4.074e-09
## alternative hypothesis: true location shift is not equal to 0
#Monthlyincome  signifficantly different in people with or without Attrition d
#p-value = 4.074e-09


### 2. Monthly Income vs BusinessTravel
kruskal.test(MonthlyIncome ~ BusinessTravel,data = df)
## 
##  Kruskal-Wallis rank sum test
## 
## data:  MonthlyIncome by BusinessTravel
## Kruskal-Wallis chi-squared = 2.2416, df = 2, p-value = 0.326
kruskal.test(MonthlyIncome ~ NumCompaniesWorked,data = df)
## 
##  Kruskal-Wallis rank sum test
## 
## data:  MonthlyIncome by NumCompaniesWorked
## Kruskal-Wallis chi-squared = 54.478, df = 9, p-value = 1.531e-08
### 3. Monthly Income vs Department
kruskal.test(MonthlyIncome ~ Department,data = df)
## 
##  Kruskal-Wallis rank sum test
## 
## data:  MonthlyIncome by Department
## Kruskal-Wallis chi-squared = 25.546, df = 2, p-value = 2.836e-06
#Monthly income has a significant relationship with Department(p-value = 2.836e-06)


### 4. Monthly Income vs EducationField
kruskal.test(MonthlyIncome ~ EducationField,data = df)
## 
##  Kruskal-Wallis rank sum test
## 
## data:  MonthlyIncome by EducationField
## Kruskal-Wallis chi-squared = 15.544, df = 5, p-value = 0.008274
#Monthly income has a significant relationship with EducationField(p-value = 0.008274)


### 5. Monthly Income vs Gender
wilcox.test(MonthlyIncome ~ Gender,data = df, alternative = "two.sided")
## 
##  Wilcoxon rank sum test with continuity correction
## 
## data:  MonthlyIncome by Gender
## W = 98590, p-value = 0.04623
## alternative hypothesis: true location shift is not equal to 0
#p-value = 0.04623


### 6. Monthly Income vs JobRole
kruskal.test(MonthlyIncome ~ JobRole,data = df)
## 
##  Kruskal-Wallis rank sum test
## 
## data:  MonthlyIncome by JobRole
## Kruskal-Wallis chi-squared = 636.1, df = 8, p-value < 2.2e-16
#p-value < 2.2e-16


### 7. Monthly Income vs MaritalStatus
kruskal.test(MonthlyIncome ~ MaritalStatus,data = df)
## 
##  Kruskal-Wallis rank sum test
## 
## data:  MonthlyIncome by MaritalStatus
## Kruskal-Wallis chi-squared = 9.358, df = 2, p-value = 0.009288
#p-value = 0.009288


### 8. Monthly Income vs OverTime
wilcox.test(MonthlyIncome ~ OverTime,data = df, alternative = "two.sided")
## 
##  Wilcoxon rank sum test with continuity correction
## 
## data:  MonthlyIncome by OverTime
## W = 79554, p-value = 0.6161
## alternative hypothesis: true location shift is not equal to 0
### 9. Monthly Income vs Education
kruskal.test(MonthlyIncome ~ Education,data = df)
## 
##  Kruskal-Wallis rank sum test
## 
## data:  MonthlyIncome by Education
## Kruskal-Wallis chi-squared = 20.448, df = 4, p-value = 0.0004072
#p-value = 0.0004072

### 10. Monthly Income vs EnvironmentSatisfaction
kruskal.test(MonthlyIncome ~ EnvironmentSatisfaction,data = df)
## 
##  Kruskal-Wallis rank sum test
## 
## data:  MonthlyIncome by EnvironmentSatisfaction
## Kruskal-Wallis chi-squared = 1.4961, df = 3, p-value = 0.6832
### 11. Monthly Income vs JobInvolvement
kruskal.test(MonthlyIncome ~ JobInvolvement,data = df)
## 
##  Kruskal-Wallis rank sum test
## 
## data:  MonthlyIncome by JobInvolvement
## Kruskal-Wallis chi-squared = 0.24444, df = 3, p-value = 0.9701
### 12. Monthly Income vs JobLevel
kruskal.test(MonthlyIncome ~ JobLevel,data = df)
## 
##  Kruskal-Wallis rank sum test
## 
## data:  MonthlyIncome by JobLevel
## Kruskal-Wallis chi-squared = 744.02, df = 4, p-value < 2.2e-16
#p-value < 2.2e-16

### 13. Monthly Income vs JobSatisfaction
kruskal.test(MonthlyIncome ~ JobSatisfaction,data = df)
## 
##  Kruskal-Wallis rank sum test
## 
## data:  MonthlyIncome by JobSatisfaction
## Kruskal-Wallis chi-squared = 1.3648, df = 3, p-value = 0.7138
### 14. Monthly Income vs RelationshipSatisfaction
kruskal.test(MonthlyIncome ~ RelationshipSatisfaction,data = df)
## 
##  Kruskal-Wallis rank sum test
## 
## data:  MonthlyIncome by RelationshipSatisfaction
## Kruskal-Wallis chi-squared = 1.4622, df = 3, p-value = 0.691
### 15. Monthly Income vs StockOptionLevel
kruskal.test(MonthlyIncome ~ StockOptionLevel,data = df)
## 
##  Kruskal-Wallis rank sum test
## 
## data:  MonthlyIncome by StockOptionLevel
## Kruskal-Wallis chi-squared = 8.8154, df = 3, p-value = 0.03185
#p-value = 0.03185

### 16. Monthly Income vs WorkLifeBalance
kruskal.test(MonthlyIncome ~ WorkLifeBalance,data = df)
## 
##  Kruskal-Wallis rank sum test
## 
## data:  MonthlyIncome by WorkLifeBalance
## Kruskal-Wallis chi-squared = 1.1367, df = 3, p-value = 0.7682
####17. Monthly Income vs NumCompaniesWorked
kruskal.test(MonthlyIncome ~ NumCompaniesWorked,data = df)
## 
##  Kruskal-Wallis rank sum test
## 
## data:  MonthlyIncome by NumCompaniesWorked
## Kruskal-Wallis chi-squared = 54.478, df = 9, p-value = 1.531e-08
#p-value = 1.531e-08

# Median monthly incomes are not significantly differ in BusinessTravel, EnvironmentSatisfaction,
# OverTime,JobInvolvement,JobSatisfaction, RelationshipSatisfaction and WorkLifeBalance variables.


#Monthly income had a significant relationship with StockOptionLevel,JobLevel, Education,
#MaritalStatus, JobRole, Gender, EducationField, Department, Attrition,
#NumCompaniesWorked
colnames(cs2.NoAttrition)
#add some new variables to datasets increase the accuracy

df=df%>%
  mutate(year2=YearsInCurrentRole/YearsAtCompany)%>%
  mutate(year4=YearsInCurrentRole/TotalWorkingYears)%>%
  mutate(year5=df$YearsAtCompany/TotalWorkingYears)





cs2.NoAttrition=cs2.NoAttrition%>%
  mutate(year2=cs2.NoAttrition$YearsInCurrentRole/cs2.NoAttrition$YearsAtCompany)%>%
  mutate(year4=cs2.NoAttrition$YearsInCurrentRole/cs2.NoAttrition$TotalWorkingYears)%>%
  mutate(year5=cs2.NoAttrition$YearsAtCompany/cs2.NoAttrition$TotalWorkingYears)


cs2.NoSalary=cs2.NoSalary%>%
  mutate(year2=cs2.NoSalary$YearsInCurrentRole/cs2.NoSalary$YearsAtCompany)%>%
  mutate(year4=cs2.NoSalary$YearsInCurrentRole/cs2.NoSalary$TotalWorkingYears)%>%
  mutate(year5=cs2.NoSalary$YearsAtCompany/cs2.NoSalary$TotalWorkingYears)


df$year2=as.numeric(df$year2)
df$year4=as.numeric(df$year4)
df$year5=as.numeric(df$year5)

cs2.NoAttrition$year2=as.numeric(cs2.NoAttrition$year2)
cs2.NoAttrition$year4=as.numeric(cs2.NoAttrition$year4)
cs2.NoAttrition$year5=as.numeric(cs2.NoAttrition$year5)


cs2.NoSalary$year2=as.numeric(cs2.NoSalary$year2)
cs2.NoSalary$year4=as.numeric(cs2.NoSalary$year4)
cs2.NoSalary$year5=as.numeric(cs2.NoSalary$year5)

#Dealing with missing data on in the new datasets

plot_missing(df)


sum(is.na(df))

for(i in 1:ncol(df))
{
  if(is.numeric(df[,i]))
  {
    df[is.na(df[,i]), i] <- median(df[,i], na.rm = TRUE)
  }
}
sum(is.na(df))

for(i in 1:ncol(cs2.NoAttrition))
{
  if(is.numeric(cs2.NoAttrition[,i]))
  {
    cs2.NoAttrition[is.na(cs2.NoAttrition[,i]), i] <- median(cs2.NoAttrition[,i], na.rm = TRUE)
  }
}
sum(is.na(cs2.NoAttrition))

for(i in 1:ncol(cs2.NoSalary))
{
  if(is.numeric(cs2.NoSalary[,i]))
  {
    cs2.NoSalary[is.na(cs2.NoSalary[,i]), i] <- median(cs2.NoSalary[,i], na.rm = TRUE)
  }
}
plot_missing(df)


sum(is.na(df))

#statistical analysis on the new variables:
  
wilcox.test(year2 ~ Attrition,data = df, alternative = "two.sided")


wilcox.test(year4 ~ Attrition,data = df, alternative = "two.sided")

cor.test(df$year5, df$MonthlyIncome,
         method= "spearman",
         exact=FALSE,
         alternative="two.side")

#define the most important variables to determine attrition

library(xgboost)
tree <- rpart(Attrition ~ ., data = df)
# Fit an RF
set.seed(101) # for reproducibility
rfo <- randomForest(Attrition ~ ., data = df, importance = TRUE)
# Fit a GBM
set.seed(102) # for reproducibility

# Extract VI scores from each model
vi_tree <- tree$variable.importance
vi_rfo <- rfo$variable.importance # or use `randomForest::importance(rfo)`


# Load required packages
library(vip)

vi(tree) # CART-like decision tree

vi(rfo) # RF

p1 <- vip(tree) + ggtitle("Single tree")

p2 <- vip(rfo) + ggtitle("Random forest")




# Display plots in a grid (Figure 1)
grid.arrange(p1, p2, p3, nrow = 1)





df_attrition=df%>%
  select(OverTime
         ,JobSatisfaction,
         JobInvolvement,
         StockOptionLevel,
         WorkLifeBalance,
         EnvironmentSatisfaction,
         Department,
         JobRole,
         NumCompaniesWorked,
         MonthlyIncome,
         DistanceFromHome,
         YearsWithCurrManager,
         YearsAtCompany,
         YearsInCurrentRole,
         TotalWorkingYears,
         year2,
         year4
  )


cs2.NoAttrition=cs2.NoAttrition%>%select(
  OverTime,
  JobSatisfaction,
  JobInvolvement,
  StockOptionLevel,
  WorkLifeBalance,
  EnvironmentSatisfaction,
  Department,
  JobRole,
  NumCompaniesWorked,
  MonthlyIncome,
  DistanceFromHome,
  YearsWithCurrManager,
  YearsAtCompany,
  YearsInCurrentRole,
  TotalWorkingYears,
  year2,
  year4,
  
)


df_for_salary=df%>%
  select(Age, YearsSinceLastPromotion, YearsSinceLastPromotion,
         YearsWithCurrManager,
         year5,
         YearsAtCompany,
         YearsInCurrentRole,
         TotalWorkingYears,
         StockOptionLevel,
         JobLevel,
         Education,
         MaritalStatus,
         JobRole, 
         Gender, 
         EducationField,
         Department, 
         Attrition,
         NumCompaniesWorked)


cs2.NoSalary=cs2.NoSalary%>%
  select(Age, YearsSinceLastPromotion, YearsSinceLastPromotion,
         YearsWithCurrManager,
         year5,
         YearsAtCompany,
         YearsInCurrentRole,
         TotalWorkingYears,
         StockOptionLevel,
         JobLevel,
         Education,
         MaritalStatus,
         JobRole, 
         Gender, 
         EducationField,
         Department, 
         Attrition,
         NumCompaniesWorked
  )
#Convert categorical variables into numerical(dummies)


dmy <- dummyVars(" ~ .", data = df_attrition)
df_attrition1 <- data.frame(predict(dmy, newdata = df_attrition))



dmy1=dummyVars(" ~ .", data = cs2.NoAttrition)
cs2.NoAttrition_test<- data.frame(predict(dmy1, newdata = cs2.NoAttrition))

dmy2<- dummyVars(" ~ .", data = df_for_salary)
df_for_salary1 <- data.frame(predict(dmy2, newdata = df_for_salary))


cs2.NoSalary$StockOptionLevel=as.factor(cs2.NoSalary$StockOptionLevel)
dmy3=dummyVars(" ~ .", data = cs2.NoSalary)
cs2.NoSalary_test<- data.frame(predict(dmy3, newdata = cs2.NoSalary))


df_attrition2=cbind(df_attrition1,new_col=df["Attrition"])

df_for_salary2=cbind(df_for_salary1,new_col=df["MonthlyIncome"])


#Normalizing the numerical variables

library(tidyverse)

normalize=function(x){
  return((x-min(x))/(max(x)-min(x)))
}

df_attrition2=df_attrition2%>%
  mutate(DistanceFromHome=normalize(DistanceFromHome))%>%
  mutate(YearsWithCurrManager=normalize(YearsWithCurrManager))%>%
  mutate(YearsAtCompany=normalize(YearsAtCompany))%>%
  mutate(TotalWorkingYears=normalize(TotalWorkingYears))%>%
  mutate(YearsInCurrentRole=normalize(YearsInCurrentRole))%>%
  mutate(MonthlyIncome=normalize(MonthlyIncome))

cs2.NoAttrition_test= cs2.NoAttrition_test%>%
  mutate(DistanceFromHome=normalize(DistanceFromHome))%>%
  mutate(YearsWithCurrManager=normalize(YearsWithCurrManager))%>%
  mutate(YearsAtCompany=normalize(YearsAtCompany))%>%
  mutate(TotalWorkingYears=normalize(TotalWorkingYears))%>%
  mutate(YearsInCurrentRole=normalize(YearsInCurrentRole))%>%
  mutate(MonthlyIncome=normalize(MonthlyIncome))



df_for_salary2=df_for_salary2%>%
  mutate(Age=normalize(Age))%>%
  mutate(YearsWithCurrManager=normalize(YearsWithCurrManager))%>%
  mutate(YearsSinceLastPromotion=normalize(YearsSinceLastPromotion))%>%
  mutate(YearsAtCompany=normalize(YearsAtCompany))%>%
  mutate(year5=normalize(year5))%>%
  mutate(TotalWorkingYears=normalize(TotalWorkingYears))


cs2.NoSalary_test= cs2.NoSalary_test%>%
  mutate(Age=normalize(Age))%>%
  mutate(YearsWithCurrManager=normalize(YearsWithCurrManager))%>%
  mutate(YearsSinceLastPromotion=normalize(YearsSinceLastPromotion))%>%
  mutate(YearsAtCompany=normalize(YearsAtCompany))%>%
  mutate(year5=normalize(year5))%>%
  mutate(TotalWorkingYears=normalize(TotalWorkingYears))
#———————————–








plot_missing(df)
sum(is.na(df))

for(i in 1:ncol(df))
{
  if(is.numeric(df[,i]))
  {
    df[is.na(df[,i]), i] <- median(df[,i], na.rm = TRUE)
  }
}
sum(is.na(df))


for(i in 1:ncol(cs2.NoAttrition))
{
  if(is.numeric(cs2.NoAttrition[,i]))
  {
    cs2.NoAttrition[is.na(cs2.NoAttrition[,i]), i] <- median(cs2.NoAttrition[,i], na.rm = TRUE)
  }
}
sum(is.na(cs2.NoAttrition))


for(i in 1:ncol(cs2.NoSalary))
{
  if(is.numeric(cs2.NoSalary[,i]))
  {
    cs2.NoSalary[is.na(cs2.NoSalary[,i]), i] <- median(cs2.NoSalary[,i], na.rm = TRUE)
  }
}
plot_missing(df)
sum(is.na(df))




set.seed(1234)
sample_set=sample(nrow(df_attrition2),round(nrow(df_attrition2)*0.75),replace=FALSE)
df_attrition2_train=df_attrition2[sample_set,]
df_attrition2_test=df_attrition2[-sample_set,]
naive =  naiveBayes(Attrition ~ ., data = df_attrition2_train)
dim(df_attrition2_train)
dim(df_attrition2_test)
pred =predict(naive,df_attrition2_test,type="class")

pred_table=table(df_attrition2_test$Attrition,pred)
pred_table
sum(diag(pred_table))/nrow(df_attrition2_test)
tab.naive = table(predicted = pred,Actual =  df_attrition2_test$Attrition)
(sensitivity = round(tab.naive[2,2]*100/(tab.naive[2,2] + tab.naive[1,2])))
(specificity = round(tab.naive[1,1]*100/(tab.naive[1,1] + tab.naive[2,1])))

final.cls.model = naiveBayes(Attrition ~ ., data = df_attrition2)
pred4 = predict(final.cls.model, newdata = df_attrition2)

tab.naive = table(predicted = pred4,Actual =  df_attrition2$Attrition)
(sensitivity = round(tab.naive[2,2]*100/(tab.naive[2,2] + tab.naive[1,2])))
(specificity = round(tab.naive[1,1]*100/(tab.naive[1,1] + tab.naive[2,1])))
attrition_matrix=confusionMatrix(pred4,df_attrition2$Attrition,positive= "Yes")
attrition_matrix
precision=posPredValue(pred4,df_attrition2$Attrition,positive= "Yes") 
precision
sensitivity=sensitivity(pred4,df_attrition2$Attrition,positive= "Yes")
sensitivity
specificity=specificity(pred,df_attrition2$Attrition,negative= "No")
specificity
f1_score=(2*precision*sensitivity)/(precision+sensitivity)
f1_score
predfinal=predict(final.cls.model,newdata=cs2.NoAttrition_test)

predfinal
cs2.NoAttrition1 =  read.csv("/Users/owner/Desktop/final_project/CaseStudy2CompSet No Attrition.csv")

pred.df = data.frame(ID = cs2.NoAttrition1$ID, Attrition = predfinal)

write.csv(pred.df,"/Users/owner/Desktop/final_project/omid.Case2PredictionsMehrpour.Attrition.csv",row.names = FALSE)



Grid = data.frame(usekernel=TRUE,laplace = 0,adjust=1)
mdl = train(Attrition ~ .,data=df_attrition2,method="naive_bayes",
            trControl=trainControl(method="none"),
            tuneGrid=Grid)
varImp(mdl)




library(gbm)
library(gbm)
df_attrition2$Attrition = ifelse(df_attrition2$Attrition == "Yes",1,0)
gbm.mod_final = gbm(formula = Attrition ~ ., data = df_attrition2,interaction.depth = 3,
                    distribution = "bernoulli", n.trees = 5000,shrinkage = 0.1,
                    n.minobsinnode = 10,cv.folds = 10)
pred10 = predict(gbm.mod_final, newdata = df_attrition2,type = "response")
pred10 = ifelse(pred10 > 0.5,1,0)
tab.gbm = table(predicted = pred10,Actual =  df_attrition2$Attrition)
(sensitivity = round(tab.gbm[2,2]*100/(tab.gbm[2,2] + tab.gbm[1,2])))
(specificity = round(tab.gbm[1,1]*100/(tab.gbm[1,1] + tab.gbm[2,1])))

names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "MaritalStatus.Divorced"] <- "MaritalStatusDivorced"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "MaritalStatus.Married"] <- "MaritalStatusMarried"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "MaritalStatus.Single"] <- "MaritalStatusSingle"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "Gender.Female"] <- "GenderFemale"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "Gender.Male"] <- "GenderMale"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "EducationField.Human.Resources"] <- "EducationFieldHuman.Resources"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "EducationField.Life.Sciences"] <- "EducationFieldLife.Sciences"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "EducationField.Marketing"] <- "EducationFieldMarketing"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "EducationField.Medical"] <- "EducationFieldMedical"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "EducationField.Other"] <- "EducationFieldOther"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "EducationField.Technical.Degree"] <- "EducationFieldTechnical.Degree"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "Department.Human.Resources"] <- "DepartmentHuman.Resources"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "Department.Research...Development"] <- "DepartmentResearch...Development"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "Department.Human.Resources"] <- "DepartmentHuman.Resources"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "Department.Research...Development"] <- "DepartmentResearch...Development"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "Department.Sales"] <- "DepartmentSales"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "EducationField.Human.Resources"] <- "EducationFieldHuman.Resources"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "EducationField.Life.Sciences"] <- "EducationFieldLife.Sciences"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "EducationField.Human.Resources"] <- "EducationFieldHuman.Resources"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "EducationField.Marketing"] <- "EducationFieldMarketing"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "EducationField.Medical"] <- "EducationFieldMedical"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "EducationField.Other"] <- "EducationFieldOther"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "EducationField.Technical.Degree"] <- "EducationFieldTechnical.Degree"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "Gender.Female"] <- "GenderFemale"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "Gender.Male"] <- "GenderMale"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "MaritalStatus.Divorced"] <- "MaritalStatusDivorced"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "MaritalStatus.Married"] <- "MaritalStatusMarried"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "MaritalStatus.Single"] <- "MaritalStatusSingle"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "OverTime.No"] <- "OverTimeNo"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "OverTime.Yes"] <- "OverTimeYes"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "MaritalStatus.Divorced"] <- "MaritalStatusDivorced"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "MaritalStatus.Married"] <- "MaritalStatusMarried"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "Department.Human.Resources"] <- "DepartmentHuman.Resources"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "Department.Research...Development"] <- "DepartmentResearch...Development"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "Department.Sales"] <- "DepartmentSales"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "EducationField.Human.Resources"] <- "EducationFieldHuman.Resources"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "EducationField.Life.Sciences"] <- "EducationFieldLife.Sciences"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "EducationField.Human.Resources"] <- "EducationFieldHuman.Resources"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "EducationField.Marketing"] <- "EducationFieldMarketing"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "EducationField.Medical"] <- "EducationFieldMedical"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "EducationField.Other"] <- "EducationFieldOther"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "EducationField.Technical.Degree"] <- "EducationFieldTechnical.Degree"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "Gender.Female"] <- "GenderFemale"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "Gender.Male"] <- "GenderMale"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "MaritalStatus.Divorced"] <- "MaritalStatusDivorced"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "MaritalStatus.Married"] <- "MaritalStatusMarried"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "MaritalStatus.Single"] <- "MaritalStatusSingle"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "OverTime.No"] <- "OverTimeNo"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "OverTime.Yes"] <- "OverTimeYes"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "Department.Human.Resources"] <- "DepartmentHuman.Resources"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "Department.Research...Development"] <- "DepartmentResearch...Development"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "Department.Sales"] <- "DepartmentSales"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "EducationField.Human.Resources"] <- "EducationFieldHuman.Resources"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "EducationField.Life.Sciences"] <- "EducationFieldLife.Sciences"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "EducationField.Human.Resources"] <- "EducationFieldHuman.Resources"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "EducationField.Marketing"] <- "EducationFieldMarketing"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "EducationField.Medical"] <- "EducationFieldMedical"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "EducationField.Other"] <- "EducationFieldOther"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "EducationField.Technical.Degree"] <- "EducationFieldTechnical.Degree"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "Gender.Female"] <- "GenderFemale"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "Gender.Male"] <- "GenderMale"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "MaritalStatus.Divorced"] <- "MaritalStatusDivorced"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "MaritalStatus.Married"] <- "MaritalStatusMarried"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "MaritalStatus.Single"] <- "MaritalStatusSingle"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "OverTime.No"] <- "OverTimeNo"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "OverTime.Yes"] <- "OverTimeYes"
names(cs2.NoAttrition_test)[names(cs2.NoAttrition_test) == "OverTime.Yes"] <- "OverTimeYes"
pred10 = predict(gbm.mod_final, newdata = cs2.NoAttrition_test,type = "response")
pred10 = ifelse(pred10 > 0.5,1,0)



library(class)
idx = sample.int(n = nrow(df_attrition2), size = floor(0.75*nrow(df_attrition2)), replace = F)
train = df_attrition2[idx,]

colnames(train)
test = df_attrition2[-idx,]
trn_target = train$Attrition
colnames(train)
trn = train[,-44]
tst_target = test$Attrition
tst = test[,-44]

pred = knn(train = trn, test = tst, cl = trn_target, k = 6)
model_table=table(tst_target,pred)
model_table
sum(diag(model_table))/nrow(tst)


Accuracy = NULL
mis = NULL
sen = NULL
spe = NULL

for(i in 1:50)
{
  pred = knn(train = trn, test = tst, cl = trn_target, k = i)
  head(pred)
  model_table=table(trn_target)
  tab = table(Predicted = pred, Real = tst_target)
  Accuracy[i] = ((tab[1,1] + tab[2,2])/sum(tab))*100
  mis[i] = round((tab[1,2]+tab[2,1])/sum(tab),2)
  sen[i] = round(tab[2,2]/(tab[2,2]+tab[1,2]),2)
  spe[i] = round(tab[1,1]/(tab[1,1]+tab[2,1]),2)
}
plot(x = c(1:50), y = Accuracy, xlab = "k", pch = 19, type = "b")
abline(v = which.max(Accuracy), col = "red", lwd = 2)

data.frame(Measure = c("Accuracy","Misclassification Rate","Sensitivity","Specificity"),
           Value = c(round(Accuracy[6],2),round(mis[6],2),round(sen[6],2),round(spe[6],2)))




colnames(df_for_salary2)
sum(is.na(df_for_salary2))

for(i in 1:ncol(df_for_salary2))
{
  if(is.numeric(df_for_salary2[,i]))
  {
    df_for_salary2[is.na(df_for_salary2[,i]), i] <- median(df_for_salary2[,i], na.rm = TRUE)
  }
}


set.seed(2021) 
train_ind = sample(seq_len(nrow(df_for_salary2)), size = floor(0.7 * nrow(df_for_salary2)))
train = df_for_salary2[train_ind, ]
test = df_for_salary2[-train_ind, ]


model2 = lm(MonthlyIncome ~ ., data = train)

summary(model2)
pred1 = predict(model2, newdata = test)
(RMSE.test = sqrt(mean((pred1 - test$MonthlyIncome)^2)))

final.reg.model = lm(MonthlyIncome ~ ., data = df_for_salary2)
summary(final.reg.model)
pred1 = predict(final.reg.model, newdata = df_for_salary2)
(RMSE.tr.reg = sqrt(mean((pred1 - df_for_salary2$MonthlyIncome)^2)))


cs2.NoSalary$StockOptionLevel=as.factor(cs2.NoSalary$StockOptionLevel)
colnames(df_for_salary2)
cs2.NoSalary_test$MonthlyIncome=NaN

colnames(cs2.NoSalary_test)
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "MaritalStatus.Divorced"] <- "MaritalStatusDivorced"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "MaritalStatus.Married"] <- "MaritalStatusMarried"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "MaritalStatus.Single"] <- "MaritalStatusSingle"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "Gender.Female"] <- "GenderFemale"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "Gender.Male"] <- "GenderMale"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "EducationField.Human.Resources"] <- "EducationFieldHuman.Resources"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "EducationField.Life.Sciences"] <- "EducationFieldLife.Sciences"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "EducationField.Marketing"] <- "EducationFieldMarketing"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "EducationField.Medical"] <- "EducationFieldMedical"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "EducationField.Other"] <- "EducationFieldOther"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "EducationField.Technical.Degree"] <- "EducationFieldTechnical.Degree"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "Department.Human.Resources"] <- "DepartmentHuman.Resources"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "Department.Research...Development"] <- "DepartmentResearch...Development"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "Department.Human.Resources"] <- "DepartmentHuman.Resources"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "Department.Research...Development"] <- "DepartmentResearch...Development"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "Department.Sales"] <- "DepartmentSales"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "EducationField.Human.Resources"] <- "EducationFieldHuman.Resources"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "EducationField.Life.Sciences"] <- "EducationFieldLife.Sciences"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "EducationField.Human.Resources"] <- "EducationFieldHuman.Resources"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "EducationField.Marketing"] <- "EducationFieldMarketing"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "EducationField.Medical"] <- "EducationFieldMedical"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "EducationField.Other"] <- "EducationFieldOther"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "EducationField.Technical.Degree"] <- "EducationFieldTechnical.Degree"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "Gender.Female"] <- "GenderFemale"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "Gender.Male"] <- "GenderMale"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "MaritalStatus.Divorced"] <- "MaritalStatusDivorced"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "MaritalStatus.Married"] <- "MaritalStatusMarried"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "MaritalStatus.Single"] <- "MaritalStatusSingle"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "OverTime.No"] <- "OverTimeNo"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "OverTime.Yes"] <- "OverTimeYes"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "MaritalStatus.Divorced"] <- "MaritalStatusDivorced"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "MaritalStatus.Married"] <- "MaritalStatusMarried"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "Department.Human.Resources"] <- "DepartmentHuman.Resources"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "Department.Research...Development"] <- "DepartmentResearch...Development"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "Department.Sales"] <- "DepartmentSales"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "EducationField.Human.Resources"] <- "EducationFieldHuman.Resources"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "EducationField.Life.Sciences"] <- "EducationFieldLife.Sciences"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "EducationField.Human.Resources"] <- "EducationFieldHuman.Resources"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "EducationField.Marketing"] <- "EducationFieldMarketing"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "EducationField.Medical"] <- "EducationFieldMedical"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "EducationField.Other"] <- "EducationFieldOther"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "EducationField.Technical.Degree"] <- "EducationFieldTechnical.Degree"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "Gender.Female"] <- "GenderFemale"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "Gender.Male"] <- "GenderMale"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "MaritalStatus.Divorced"] <- "MaritalStatusDivorced"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "MaritalStatus.Married"] <- "MaritalStatusMarried"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "MaritalStatus.Single"] <- "MaritalStatusSingle"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "OverTime.No"] <- "OverTimeNo"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "OverTime.Yes"] <- "OverTimeYes"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "Department.Human.Resources"] <- "DepartmentHuman.Resources"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "Department.Research...Development"] <- "DepartmentResearch...Development"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "Department.Sales"] <- "DepartmentSales"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "EducationField.Human.Resources"] <- "EducationFieldHuman.Resources"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "EducationField.Life.Sciences"] <- "EducationFieldLife.Sciences"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "EducationField.Human.Resources"] <- "EducationFieldHuman.Resources"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "EducationField.Marketing"] <- "EducationFieldMarketing"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "EducationField.Medical"] <- "EducationFieldMedical"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "EducationField.Other"] <- "EducationFieldOther"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "EducationField.Technical.Degree"] <- "EducationFieldTechnical.Degree"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "Gender.Female"] <- "GenderFemale"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "Gender.Male"] <- "GenderMale"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "MaritalStatus.Divorced"] <- "MaritalStatusDivorced"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "MaritalStatus.Married"] <- "MaritalStatusMarried"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "MaritalStatus.Single"] <- "MaritalStatusSingle"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "OverTime.No"] <- "OverTimeNo"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "OverTime.Yes"] <- "OverTimeYes"
names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "OverTime.Yes"] <- "OverTimeYes"

names(cs2.NoSalary_test)[names(cs2.NoSalary_test) == "MaritalStatusDivorced"] <- "MaritalStatus.Divorced"

colnames(cs2.NoSalary_test)
cs2.NoSalary1 = read.csv("/Users/owner/Desktop/final_project/CaseStudy2CompSet No Salary.csv",stringsAsFactors = TRUE)
pred2 = predict(final.reg.model, newdata = cs2.NoSalary_test)


pred.df = data.frame(ID = cs2.NoSalary1$ID, MonthlyIncome = pred2)

write.csv(pred.df,"/Users/owner/Desktop/final_project/omid.Case2PredictionsMehrpour Salary.csv")

