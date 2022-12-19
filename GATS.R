# R CODE FOR PAPER: SOCIAL DETERMINANTS OF HEALTH, RELIGIOSITY AND TOBACCO USE IN SUB-SAHARAN AFRICA: EVIDENCE FROM THE GLOBAL ADULT TOBACCO SURVEY IN SEVEN COUNTRIES.

#All associated datasets are freely downloadable from the GTSS data portal at https://nccd.cdc.gov/GTSS/rdPage.aspx?rdReport=OSH_GTSS.ExploreByLocation&rdRequestForwarding=Form
#The author imported the Stata datasets for each country into R for analysis
#The following analysis for the Nigeria dataset is annotated in detail. Other countries follow.
#The analysis of the pooled dataset comes at the latter part of the script
#Forward all correspondence to stephen.ogbodo@mail.mcgill.ca


#NIGERIA

library(haven)
library(descr)

#Import Stata dataset (downloaded from https://nccd.cdc.gov/GTSSDataSurveyResources/Ancillary/DownloadAttachment.aspx?DatasetID=2132)
NG<- read_dta("C://Dataset//NG2012.dta")

#Select relevant variables (see codebook in downloaded file above)
ng<- subset(NG, select = c(CASEID, H02_4, gatsweight, gatscluster, region, residence, age, A01, A04, A05, A06a, A06b, A06c, A06d, A06e, A06f, A06g, A06h, A06i, A06j, A06k, A06m, A10, A11, B01, B03, C01, C03, H02a, H02b, H02c, H03))

#Remove redundant original dataset
rm(NG)
freq(ng$H02a)

#Create 3 categories of tobacco smoking knowledge variable
ng$know<- NA
ng$know[ng$H02a==1 & ng$H02b==1 & ng$H02c==1]<-1
ng$know[ng$H02a==1 & ng$H02b==1 & ng$H02c!=1]<-2
ng$know[ng$H02a==1 & ng$H02b!=1 & ng$H02c==1]<-2
ng$know[ng$H02a!=1 & ng$H02b==1 & ng$H02c==1]<-2
ng$know[ng$H02a==1 & ng$H02b!=1 & ng$H02c!=1]<-3
ng$know[ng$H02a!=1 & ng$H02b==1 & ng$H02c!=1]<-3
ng$know[ng$H02a!=1 & ng$H02b!=1 & ng$H02c==1]<-3
ng$know[ng$H02a!=1 & ng$H02b!=1 & ng$H02c!=1]<-3
ng$know <- factor(ng$know,
                  levels = c(1,2,3),
                  labels = c("Good Knowledge", "Some Knowledge",
                             "Poor Knowledge"))
freq(ng$know)

#Remove redundant variables
ng<- subset(ng, select = -c(H02a, H02b, H02c))

#Create current tobacco use variable
ng$tob<- NA
ng$tob[ng$B01==1 | ng$B01 ==2]<-1
ng$tob[ng$C01==1 | ng$C01 ==2]<-1
ng$tob[is.na(ng$tob)]=0
freq(ng$tob)
ng$tob <- factor(ng$tob,
                 levels = c(0,1),
                 labels = c("Not current user", "Current User"))

#Create wealth index
ng$wealth<- NA
ng$wealth[is.na(ng$wealth)]=0
ng$wealth[ng$A06a==1]<-(ng$wealth+2)
ng$wealth[ng$A06b==1]<-(ng$wealth+2)
ng$wealth[ng$A06c==1]<-(ng$wealth+1)
ng$wealth[ng$A06d==1]<-(ng$wealth+1)
ng$wealth[ng$A06e==1]<-(ng$wealth+2)
ng$wealth[ng$A06f==1]<-(ng$wealth+1)
ng$wealth[ng$A06g==1]<-(ng$wealth+1)
ng$wealth[ng$A06h==1]<-(ng$wealth+2)
ng$wealth[ng$A06i==1]<-(ng$wealth+1)
ng$wealth[ng$A06j==1]<-(ng$wealth+1)
ng$wealth[ng$A06k==1]<-(ng$wealth+1)
ng$wealth[ng$A06m==1]<-(ng$wealth+1)
freq(ng$wealth)
class(ng$wealth)
boxplot(ng$wealth)
ng<- subset(ng, select = -c(A06a, A06b, A06c, A06d, A06e, A06f, A06g, A06h, A06i, A06j, A06k, A06m))

#Create age categories
ng$ageCAT<-NA
ng$ageCAT[ng$age<=24]<-1
ng$ageCAT[ng$age>24 & ng$age<=34]<-2
ng$ageCAT[ng$age>34 & ng$age<=44]<-3
ng$ageCAT[ng$age>44 & ng$age<=54]<-4
ng$ageCAT[ng$age>54 & ng$age<=64]<-5
ng$ageCAT[ng$age>64]<-6
ng$ageCAT <- factor(ng$ageCAT,
                    levels = c(1,2,3,4,5,6),
                    labels = c("15-24", "25-34",
                               "35-44", "45-54", "55-64", "65+"))
freq(ng$ageCAT)

#Create sex variable
ng$sex<-NA
ng$sex[ng$A01==2]<-1
ng$sex[ng$A01==1]<-2
ng$sex <- factor(ng$sex,
                 levels = c(1,2),
                 labels = c("Female", "Male"))
freq(ng$sex)

#Create rural/urban residence variable
ng$rsd<-NA
ng$rsd[ng$residence==1]<-1
ng$rsd[ng$residence==2]<-2
ng$rsd<- factor(ng$rsd,
                levels = c(1,2),
                labels=c("Urban", "Rural"))
freq(ng$rsd)

#Create education variable
ng$edu<-NA
ng$edu[ng$A04==7]<-1
ng$edu[ng$A04==6]<-2
ng$edu[ng$A04==5]<-2
ng$edu[ng$A04==4]<-3
ng$edu[ng$A04==3]<-3
ng$edu[ng$A04==2]<-4
ng$edu[ng$A04==1]<-4
ng$edu<- factor(ng$edu,
                levels = c(1,2,3,4),
                labels=c("Completed College/University", "Completed Secondary/High School", "Completed Primary/Less than Secondary", "No Formal Education/Less than Primary"))
freq(ng$edu)

#Create religion variable
ng$rel<-NA
ng$rel[ng$A10==2]<-1
ng$rel[ng$A10==1]<-2
ng$rel[ng$A10==3]<-3
ng$rel[ng$A10==5]<-4
ng$rel<- factor(ng$rel,
                levels = c(1,2,3,4),
                labels=c("Islam", "Christianity", "Traditional/Other", "None"))
freq(ng$rel)

#Create marital status variable
ng$mar<-NA
ng$mar[ng$A11==1]<-1
ng$mar[ng$A11==2]<-2
ng$mar[ng$A11==3]<-3
ng$mar[ng$A11==4]<-3
ng$mar[ng$A11==5]<-4
ng$mar<- factor(ng$mar,
                levels = c(1,2,3,4),
                labels=c("Single", "Married", "Divorced/Separated", "Widowed"))
freq(ng$mar)

#Re-arrange levels of marital status variable
ng$marr<-NA
ng$marr[ng$mar=="Single"]<-2
ng$marr[ng$mar=="Married"]<-1
ng$marr[ng$mar=="Divorced/Separated"]<-3
ng$marr[ng$mar=="Widowed"]<-4
ng$marr<- factor(ng$marr,
                 levels = c(1,2,3,4),
                 labels=c("Married", "Single", "Divorced/Separated", "Widowed"))
freq(ng$marr)

#Create employment status variable
ng$empl<-NA
ng$empl[ng$A05==4]<-3
ng$empl[ng$A05==1]<-1
ng$empl[ng$A05==2]<-1
ng$empl[ng$A05==3]<-1
ng$empl[ng$A05==7]<-2
ng$empl[ng$A05==8]<-2
ng$empl[ng$A05==5]<-4
ng$empl[ng$A05==6]<-5
ng$empl<- factor(ng$empl,
                 levels = c(1,2,3,4,5),
                 labels=c("Employed", "Unemployed", "Student", "Homemaker", "Retired"))
freq(ng$empl)

#Convert wealth index into quintiles
quantile(ng$wealth, probs = seq(0, 1, 1/5))
ng$wealthq<-NA
ng$wealthq[ng$wealth<=2]<-1
ng$wealthq[ng$wealth==3 | ng$wealth==4]<-2
ng$wealthq[ng$wealth==5]<- 3
ng$wealthq[ng$wealth==6 | ng$wealth==7]<-4
ng$wealthq[ng$wealth>7]<-5
ng$wealthq<- factor(ng$wealthq,
                    levels = c(1,2,3,4,5),
                    labels=c("Lowest", "Low", "Middle", "High", "Highest"))
freq(ng$wealthq)

#Re-arrange quintiles with "Highest" as reference
ng$wq<-NA
ng$wq[ng$wealthq=="Highest"]<-1
ng$wq[ng$wealthq=="High"]<-2
ng$wq[ng$wealthq=="Middle"]<-3
ng$wq[ng$wealthq=="Low"]<-4
ng$wq[ng$wealthq=="Lowest"]<-5
ng$wq<- factor(ng$wq,
               levels = c(1,2,3,4,5),
               labels=c("Highest", "High", "Middle", "Low", "Lowest"))
freq(ng$wq)

#Create former smoker variable
ng$former<-NA
ng$former[(ng$B03==1 | ng$B03==2 | ng$C03==1 | ng$B03==2) & (ng$tob=="Not current user")]<-1
ng$former[is.na(ng$former)]=2
freq(ng$former)

ng$never<-NA
ng$never[ng$B03==3 & ng$C03==3]<-1
ng$never[is.na(ng$never)]=2
freq(ng$never)

#Create variable for "does your religion condemn smoking?"
ng$dis<-NA
ng$dis[ng$H02_4==1]<-1
ng$dis[ng$H02_4==2]<-2
ng$dis[ng$H02_4==7]<-3
ng$dis[ng$H02_4==9]<-3
ng$dis<- factor(ng$dis,
                levels = c(1,2,3),
                labels=c("Yes", "No", "Don't Know"))
freq(ng$dis)

#Proportion of religious people who answered "yes" to the above 
crosstab(ng$rel, ng$dis, prop.r = TRUE)

#Center all sample weights to ease regression analysis
ng$weight<-ng$gatsweight/10000

#Remove redundant variables
ng<- subset(ng, select = -c(age, A01, A04, A05, A10, A11, gatsweight))


#DESCRIPTIVE STATS

#Table 1
library(expss) #This package enables the computation of frequencies using sampling weights
fre(ng$sex, weight = ng$weight)
fre(ng$ageCAT, weight = ng$weight)
fre(ng$residence, weight = ng$weight)
fre(ng$edu, weight = ng$weight)
fre(ng$rel, weight = ng$weight)
fre(ng$mar, weight = ng$weight)
fre(ng$empl, weight = ng$weight)
fre(ng$wealthq, weight = ng$weight)
fre(ng$know, weight = ng$weight)
fre(ng$tob, weight = ng$weight)
fre(ng$former, weight = ng$weight)
fre(ng$never, weight = ng$weight)

#Table 2
ng1<-subset(ng, ng$sex=="Male")   #Create dataset of only males
fre(ng1$tob, weight = ng1$weight) #Weighted prevalence of tobacco use among males
ng1<-subset(ng, ng$sex=="Female") #Create dataset of only females
fre(ng1$tob, weight = ng1$weight) #Weighted prevalence of tobacco use among females
ng1<-subset(ng, ng$ageCAT=="15-24") #Create dataset of people aged 15-24
fre(ng1$tob, weight = ng1$weight)   #Weighted prevalence of tobacco use among people aged 15-24 
ng1<-subset(ng, ng$ageCAT=="25-34")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="35-44")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="45-54")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="55-64")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="65+")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rsd=="Urban")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rsd=="Rural")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$edu=="Completed College/University")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$edu=="Completed Secondary/High School")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$edu=="Completed Primary/Less than Secondary")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$edu=="No Formal Education/Less than Primary")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rel=="Islam")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rel=="Christianity")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rel=="Traditional/Other")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rel=="None")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$marr=="Single")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$marr=="Married")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$marr=="Divorced/Separated")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$marr=="Widowed")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Employed")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Unemployed")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Student")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Homemaker")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Retired")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="Lowest")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="Low")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="Middle")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="High")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="Highest")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$know=="Good Knowledge")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$know=="Some Knowledge")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$know=="Poor Knowledge")
fre(ng1$tob, weight = ng1$weight)

#Current tobacco use status (last 3 rows of table 1)
ng$tob2<-NA
ng$tob2[ng$B01==1]<-1
ng$tob2[ng$B01==2]<-1
ng$tob2[ng$C01==1]<-2
ng$tob2[ng$C01==2]<-2
ng$tob2[(ng$B01==1 | ng$B01==2)&(ng$C01==1 | ng$C01==2) ]<-3
ng$tob2[is.na(ng$tob2)]<-4
ng$tob2<- factor(ng$tob2,
               levels = c(1,2,3,4),
               labels=c("Smoking", "Smokeless", "Both", "None"))

ng<- subset(ng, select = -c(mar, CASEID, region, residence, B01, B03, C01, C03))

#For the pooled analysis (see end of document), create universal wealth index which is a percentage of the maximum wealth points for each country.
#For Nigeria, the max wealth points is 13. As such, each participant's wealth index is a percentage of 13.
#This is necessary because the assets used in each country (and consequent maximum wealth points) vary slightly.
#This way, everyone in the 7 countries get comparable wealth indices calculated from their country references.
max(ng$wealth)
ng$wealthc<- (ng$wealth*100)/13
freq(ng$wealthc)
ng$country<- NA #Create new variable for country
ng$country[is.na(ng$country)]<-"Nigeria" #Assign "Nigeria" for everyone
nigeria<-ng # save the finished dataset as "nigeria" for use later during pooled analysis


#LOGISTIC REGRESSION

library(miceadds) #Executes cluster-robust standard errors for general linear models
library(sandwich) #Required by miceadds
mod1<-glm.cluster(data=ng, formula=tob ~ sex+ageCAT+rsd+edu+rel+marr+empl+wq+know, cluster= ng$gatscluster, weights = ng$weight, family=binomial)
summary(mod1)
exp(cbind(OR = coef(mod1), confint.default(mod1)))

#Tests of Linear Trend

ng$know1<-factor(ng$know, ordered = TRUE, levels = c("Poor Knowledge", "Some Knowledge", "Good Knowledge"))
ng$know1
lm1<-glm(ng$tob~ng$know1, family=binomial)
summary(lm1)

ng$age1<-factor(ng$ageCAT, ordered = TRUE, levels = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"))
ng$age1
lm1<-glm(ng$tob~ng$age1, family=binomial)
summary(lm1)

ng$edu1 <-factor(ng$edu, ordered = TRUE, levels = c("No Formal Education/Less than Primary", "Completed Primary/Less than Secondary", "Completed Secondary/High School", "Completed College/University"))
ng$edu1
lm1<-glm(ng$tob~ng$edu1, family=binomial)
summary(lm1)

ng$wealth1<-factor(ng$wealthq, ordered = TRUE, levels = c("Lowest", "Low", "Middle", "High", "Highest"))
ng$wealth1
lm1<-glm(ng$tob~ng$wealth1, family=binomial)
summary(lm1)

#Tests for model fit

par(mfrow = c(1, 1))
plot(mod1$glm_res)

#The line of the Residuals vs Fitted plot is horizontal at 0, indicating no real issues with linearity
#The residuals mostly follow the line of the Normal Q-Q plot, indicating normally distributed residuals
#Unevenly spread points around the line of the Scale-Location plot suggests issues with heteroscedasticity
#The Residuals vs Leverage plot shows no influential outliers.


#CAMEROON

library(haven)

#Import dataset (download from https://nccd.cdc.gov/GTSSDataSurveyResources/Ancillary/DownloadAttachment.aspx?DatasetID=3272)
CM<- read_dta("C://Dataset//CM2015.dta")
ng<- subset(CM, select = c(gatsweight, gatscluster, residence, age, A01, A04, A05, A06a, A06b, A06c, A06d, A06e, A06f, A06g, A06h, A06i, A06k, A06l, A06m, A06n, A06o, A06p, A06q, A06r, A06s, A10, A11, B01, B03, C01, C03, H02a, H02b, H02c, H03))
rm(CM)
freq(ng$H02a)
ng$know<- NA
ng$know[ng$H02a==1 & ng$H02b==1 & ng$H02c==1]<-1
ng$know[ng$H02a==1 & ng$H02b==1 & ng$H02c!=1]<-2
ng$know[ng$H02a==1 & ng$H02b!=1 & ng$H02c==1]<-2
ng$know[ng$H02a!=1 & ng$H02b==1 & ng$H02c==1]<-2
ng$know[ng$H02a==1 & ng$H02b!=1 & ng$H02c!=1]<-3
ng$know[ng$H02a!=1 & ng$H02b==1 & ng$H02c!=1]<-3
ng$know[ng$H02a!=1 & ng$H02b!=1 & ng$H02c==1]<-3
ng$know[ng$H02a!=1 & ng$H02b!=1 & ng$H02c!=1]<-3
ng$know <- factor(ng$know,
                  levels = c(1,2,3),
                  labels = c("Good Knowledge", "Some Knowledge",
                             "Poor Knowledge"))
freq(ng$know)
ng<- subset(ng, select = -c(H02a, H02b, H02c))
ng$tob<- NA
ng$tob[ng$B01==1 | ng$B01 ==2]<-1
ng$tob[ng$C01==1 | ng$C01 ==2]<-1
ng$tob[is.na(ng$tob)]=0
freq(ng$tob)
ng$tob <- factor(ng$tob,
                 levels = c(0,1),
                 labels = c("Not current user", "Current User"))
ng$wealth<- NA
ng$wealth[is.na(ng$wealth)]=0
ng$wealth[ng$A06a==1]<-(ng$wealth+2)
ng$wealth[ng$A06b==1]<-(ng$wealth+2)
ng$wealth[ng$A06c==1]<-(ng$wealth+1)
ng$wealth[ng$A06d==1]<-(ng$wealth+1)
ng$wealth[ng$A06e==1]<-(ng$wealth+2)
ng$wealth[ng$A06f==1]<-(ng$wealth+1)
ng$wealth[ng$A06g==1]<-(ng$wealth+1)
ng$wealth[ng$A06h==1]<-(ng$wealth+2)
ng$wealth[ng$A06i==1]<-(ng$wealth+1)
ng$wealth[ng$A06k==1]<-(ng$wealth+1)
ng$wealth[ng$A06l==1]<-(ng$wealth+1)
ng$wealth[ng$A06m==1]<-(ng$wealth+1)
ng$wealth[ng$A06p==1]<-(ng$wealth+1)
ng$wealth[ng$A06q==1]<-(ng$wealth+1)
ng$wealth[ng$A06r==1]<-(ng$wealth+1)
ng$wealth[ng$A06s==1]<-(ng$wealth+1)

freq(ng$wealth)
class(ng$wealth)
boxplot(ng$wealth)

ng<- subset(ng, select = -c(A06a, A06b, A06c, A06d, A06e, A06f, A06g, A06h, A06i, A06k, A06l, A06m, A06n, A06o, A06p, A06q, A06r, A06s))

ng$ageCAT<-NA
ng$ageCAT[ng$age<=24]<-1
ng$ageCAT[ng$age>24 & ng$age<=34]<-2
ng$ageCAT[ng$age>34 & ng$age<=44]<-3
ng$ageCAT[ng$age>44 & ng$age<=54]<-4
ng$ageCAT[ng$age>54 & ng$age<=64]<-5
ng$ageCAT[ng$age>64]<-6
ng$ageCAT <- factor(ng$ageCAT,
                    levels = c(1,2,3,4,5,6),
                    labels = c("15-24", "25-34",
                               "35-44", "45-54", "55-64", "65+"))
freq(ng$ageCAT)

ng$sex<-NA
ng$sex[ng$A01==2]<-1
ng$sex[ng$A01==1]<-2
ng$sex <- factor(ng$sex,
                 levels = c(1,2),
                 labels = c("Female", "Male"))

freq(ng$sex)

ng$rsd<-NA
ng$rsd[ng$residence==1]<-1
ng$rsd[ng$residence==2]<-2
ng$rsd<- factor(ng$rsd,
                levels = c(1,2),
                labels=c("Urban", "Rural"))
freq(ng$rsd)

ng$edu<-NA
ng$edu[ng$A04==9]<-1
ng$edu[ng$A04==8]<-2
ng$edu[ng$A04==7]<-2
ng$edu[ng$A04==6]<-3
ng$edu[ng$A04==5]<-3
ng$edu[ng$A04==4]<-3
ng$edu[ng$A04==3]<-3
ng$edu[ng$A04==2]<-4
ng$edu[ng$A04==1]<-4
ng$edu<- factor(ng$edu,
                levels = c(1,2,3,4),
                labels=c("Completed College/University", "Completed Secondary/High School", "Completed Primary/Less than Secondary", "No Formal Education/Less than Primary"))
freq(ng$edu)

ng$rel<-NA
ng$rel[ng$A10==4]<-1
ng$rel[ng$A10==1]<-2
ng$rel[ng$A10==2]<-2
ng$rel[ng$A10==3]<-2
ng$rel[ng$A10==5]<-3
ng$rel[ng$A10==6]<-3
ng$rel[ng$A10==7]<-4
ng$rel<- factor(ng$rel,
                levels = c(1,2,3,4),
                labels=c("Islam", "Christianity", "Traditional/Other", "None"))
freq(ng$rel)

ng$mar<-NA
ng$mar[ng$A11==1]<-1
ng$mar[ng$A11==2]<-2
ng$mar[ng$A11==3]<-3
ng$mar[ng$A11==4]<-3
ng$mar[ng$A11==5]<-4
ng$mar<- factor(ng$mar,
                levels = c(1,2,3,4),
                labels=c("Single", "Married", "Divorced/Separated", "Widowed"))
freq(ng$mar)

ng$marr<-NA
ng$marr[ng$mar=="Single"]<-2
ng$marr[ng$mar=="Married"]<-1
ng$marr[ng$mar=="Divorced/Separated"]<-3
ng$marr[ng$mar=="Widowed"]<-4
ng$marr<- factor(ng$marr,
                 levels = c(1,2,3,4),
                 labels=c("Married", "Single", "Divorced/Separated", "Widowed"))
freq(ng$marr)

ng$empl<-NA
ng$empl[ng$A05==4]<-3
ng$empl[ng$A05==1]<-1
ng$empl[ng$A05==2]<-1
ng$empl[ng$A05==3]<-1
ng$empl[ng$A05==7]<-2
ng$empl[ng$A05==8]<-2
ng$empl[ng$A05==5]<-4
ng$empl[ng$A05==6]<-5
ng$empl<- factor(ng$empl,
                 levels = c(1,2,3,4,5),
                 labels=c("Employed", "Unemployed", "Student", "Homemaker", "Retired"))
freq(ng$empl)

quantile(ng$wealth, probs = seq(0, 1, 1/5))
ng$wealthq<-NA
ng$wealthq[ng$wealth<=5]<-1
ng$wealthq[ng$wealth==6 | ng$wealth==7]<-2
ng$wealthq[ng$wealth==8 | ng$wealth==9]<- 3
ng$wealthq[ng$wealth==10]<-4
ng$wealthq[ng$wealth>=11 ]<-5
ng$wealthq<- factor(ng$wealthq,
                    levels = c(1,2,3,4,5),
                    labels=c("Lowest", "Low", "Middle", "High", "Highest"))
freq(ng$wealthq)
ng$wq<-NA
ng$wq[ng$wealthq=="Highest"]<-1
ng$wq[ng$wealthq=="High"]<-2
ng$wq[ng$wealthq=="Middle"]<-3
ng$wq[ng$wealthq=="Low"]<-4
ng$wq[ng$wealthq=="Lowest"]<-5
ng$wq<- factor(ng$wq,
               levels = c(1,2,3,4,5),
               labels=c("Highest", "High", "Middle", "Low", "Lowest"))
freq(ng$wq)
ng$former<-NA
ng$former[(ng$B03==1 | ng$B03==2 | ng$C03==1 | ng$B03==2) & (ng$tob=="Not current user")]<-1
ng$former[is.na(ng$former)]=2
freq(ng$former)

ng$never<-NA
ng$never[ng$B03==3 & ng$C03==3]<-1
ng$never[is.na(ng$never)]=2
freq(ng$never)

ng$weight<-ng$gatsweight/10000

ng<- subset(ng, select = -c(age, A01, A04, A05, A10, A11, gatsweight))
freq(ng$residence)

#DESCRIPTIVE STATS

#Table 1
library(expss)
fre(ng$sex, weight = ng$weight)
fre(ng$ageCAT, weight = ng$weight)
fre(ng$residence, weight = ng$weight)
fre(ng$edu, weight = ng$weight)
fre(ng$rel, weight = ng$weight)
fre(ng$mar, weight = ng$weight)
fre(ng$empl, weight = ng$weight)
fre(ng$wealthq, weight = ng$weight)
fre(ng$know, weight = ng$weight)
fre(ng$tob, weight = ng$weight)
fre(ng$former, weight = ng$weight)
fre(ng$never, weight = ng$weight)

#Table 2
ng1<-subset(ng, ng$sex=="Male")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$sex=="Female")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="15-24")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="25-34")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="35-44")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="45-54")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="55-64")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="65+")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rsd=="Urban")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rsd=="Rural")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$edu=="Completed College/University")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$edu=="Completed Secondary/High School")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$edu=="Completed Primary/Less than Secondary")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$edu=="No Formal Education/Less than Primary")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rel=="Islam")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rel=="Christianity")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rel=="Traditional/Other")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rel=="None")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$marr=="Single")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$marr=="Married")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$marr=="Divorced/Separated")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$marr=="Widowed")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Employed")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Unemployed")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Student")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Homemaker")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Retired")
fre(ng1$tob, weight = ng1$weight)
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="Lowest")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="Low")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="Middle")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="High")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="Highest")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$know=="Good Knowledge")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$know=="Some Knowledge")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$know=="Poor Knowledge")
fre(ng1$tob, weight = ng1$weight)

ng$tob2<-NA
ng$tob2[ng$B01==1]<-1
ng$tob2[ng$B01==2]<-1
ng$tob2[ng$C01==1]<-2
ng$tob2[ng$C01==2]<-2
ng$tob2[(ng$B01==1 | ng$B01==2)&(ng$C01==1 | ng$C01==2) ]<-3
ng$tob2[is.na(ng$tob2)]<-4
ng$tob2<- factor(ng$tob2,
                 levels = c(1,2,3,4),
                 labels=c("Smoking", "Smokeless", "Both", "None"))
freq(ng$tob2)
fre(ng$tob2, weight = ng$weight)
ng<- subset(ng, select = -c(mar, residence, B01, B03, C01, C03))

#Create wealth index for pooled dataset
max(ng$wealth)
ng$wealthc<- (ng$wealth*100)/15
freq(ng$wealthc)

ng$country<- NA
ng$country[is.na(ng$country)]<-"Cameroon"
cameroon<-ng


#LOGISTIC REGRESSION

library(miceadds)
mod1<-glm.cluster(data=ng, formula=tob ~ sex+ageCAT+rsd+edu+rel+marr+empl+wealth+know, cluster= ng$gatscluster, weights = ng$weight, family=binomial)
summary(mod1)
exp(cbind(OR = coef(mod1), confint.default(mod1)))

#Test of Linear Trend

ng$know1<-factor(ng$know, ordered = TRUE, levels = c("Poor Knowledge", "Some Knowledge", "Good Knowledge"))
ng$know1
lm1<-glm(ng$tob~ng$know1, family=binomial)
summary(lm1)

ng$age1<-factor(ng$ageCAT, ordered = TRUE, levels = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"))
ng$age1
lm1<-glm(ng$tob~ng$age1, family=binomial)
summary(lm1)

ng$edu1 <-factor(ng$edu, ordered = TRUE, levels = c("No Formal Education/Less than Primary", "Completed Primary/Less than Secondary", "Completed Secondary/High School", "Completed College/University"))
ng$edu1
lm1<-glm(ng$tob~ng$edu1, family=binomial)
summary(lm1)

ng$wealth1<-factor(ng$wealthq, ordered = TRUE, levels = c("Lowest", "Low", "Middle", "High", "Highest"))
ng$wealth1
lm1<-glm(ng$tob~ng$wealth1, family=binomial)
summary(lm1)

#Tests for model fit
par(mfrow = c(2, 2))
plot(mod1$glm_res)


#ETHIOPIA

library(haven)
#Import dataset (downlaod from https://nccd.cdc.gov/GTSSDataSurveyResources/Ancillary/DownloadAttachment.aspx?DatasetID=3367)
ET<- read_dta("C://Dataset//ET2018.dta")
ng<- subset(ET, select = c(gatsweight, gatscluster, Residence, AGE, A01, A04, A05, A06A, A06B, A06C, A06D, A06E, A06F, A06G, A06H, A06I, A06J, A06K, A06L, A06M, A10, A11, B01, B03, C01, C03, H02A, H02B, H02C, H03))
rm(ET)
freq(ng$H02A)
ng$know<- NA
ng$know[ng$H02A==1 & ng$H02B==1 & ng$H02C==1]<-1
ng$know[ng$H02A==1 & ng$H02B==1 & ng$H02C!=1]<-2
ng$know[ng$H02A==1 & ng$H02B!=1 & ng$H02C==1]<-2
ng$know[ng$H02A!=1 & ng$H02B==1 & ng$H02C==1]<-2
ng$know[ng$H02A==1 & ng$H02B!=1 & ng$H02C!=1]<-3
ng$know[ng$H02A!=1 & ng$H02B==1 & ng$H02C!=1]<-3
ng$know[ng$H02A!=1 & ng$H02B!=1 & ng$H02C==1]<-3
ng$know[ng$H02A!=1 & ng$H02B!=1 & ng$H02C!=1]<-3
ng$know <- factor(ng$know,
                  levels = c(1,2,3),
                  labels = c("Good Knowledge", "Some Knowledge",
                             "Poor Knowledge"))
freq(ng$know)
ng<- subset(ng, select = -c(H02A, H02B, H02C))
ng$tob<- NA
ng$tob[ng$B01==1 | ng$B01 ==2]<-1
ng$tob[ng$C01==1 | ng$C01 ==2]<-1
ng$tob[is.na(ng$tob)]=0
freq(ng$tob)
ng$tob <- factor(ng$tob,
                 levels = c(0,1),
                 labels = c("Not current user", "Current User"))
ng$wealth<- NA
ng$wealth[is.na(ng$wealth)]=0
ng$wealth[ng$A06A==1]<-(ng$wealth+2)
ng$wealth[ng$A06B==1]<-(ng$wealth+2)
ng$wealth[ng$A06C==1]<-(ng$wealth+1)
ng$wealth[ng$A06D==1]<-(ng$wealth+1)
ng$wealth[ng$A06E==1]<-(ng$wealth+2)
ng$wealth[ng$A06F==1]<-(ng$wealth+1)
ng$wealth[ng$A06G==1]<-(ng$wealth+1)
ng$wealth[ng$A06H==1]<-(ng$wealth+2)
ng$wealth[ng$A06I==1]<-(ng$wealth+1)
ng$wealth[ng$A06J==1]<-(ng$wealth+1)
ng$wealth[ng$A06K==1]<-(ng$wealth+1)
ng$wealth[ng$A06L==1]<-(ng$wealth+1)
ng$wealth[ng$A06M==1]<-(ng$wealth+1)

freq(ng$wealth)
class(ng$wealth)
boxplot(ng$wealth)

ng<- subset(ng, select = -c(A06A, A06B, A06C, A06D, A06E, A06F, A06G, A06H, A06I, A06J, A06K, A06L, A06M))

ng$ageCAT<-NA
ng$ageCAT[ng$AGE<=24]<-1
ng$ageCAT[ng$AGE>24 & ng$AGE<=34]<-2
ng$ageCAT[ng$AGE>34 & ng$AGE<=44]<-3
ng$ageCAT[ng$AGE>44 & ng$AGE<=54]<-4
ng$ageCAT[ng$AGE>54 & ng$AGE<=64]<-5
ng$ageCAT[ng$AGE>64]<-6
ng$ageCAT <- factor(ng$ageCAT,
                    levels = c(1,2,3,4,5,6),
                    labels = c("15-24", "25-34",
                               "35-44", "45-54", "55-64", "65+"))
freq(ng$ageCAT)

ng$sex<-NA
ng$sex[ng$A01==2]<-1
ng$sex[ng$A01==1]<-2
ng$sex <- factor(ng$sex,
                 levels = c(1,2),
                 labels = c("Female", "Male"))

freq(ng$sex)

ng$rsd<-NA
ng$rsd[ng$Residence==1]<-1
ng$rsd[ng$Residence==2]<-2
ng$rsd<- factor(ng$rsd,
                levels = c(1,2),
                labels=c("Urban", "Rural"))
freq(ng$rsd)

ng$edu<-NA
ng$edu[ng$A04==6]<-1
ng$edu[ng$A04==7]<-1
ng$edu[ng$A04==5]<-2
ng$edu[ng$A04==4]<-3
ng$edu[ng$A04==3]<-3
ng$edu[ng$A04==2]<-4
ng$edu[ng$A04==1]<-4
ng$edu<- factor(ng$edu,
                levels = c(1,2,3,4),
                labels=c("Completed College/University", "Completed Secondary/High School", "Completed Primary/Less than Secondary", "No Formal Education/Less than Primary"))
freq(ng$edu)

ng$rel<-NA
ng$rel[ng$A10==1]<-1
ng$rel[ng$A10==2]<-2
ng$rel[ng$A10==3]<-3
ng$rel[ng$A10==4]<-4
ng$rel[ng$A10==7]<-4
ng$rel<- factor(ng$rel,
                levels = c(1,2,3,4),
                labels=c("Islam", "Christianity", "Traditional/Other", "None"))
freq(ng$rel)

ng$mar<-NA
ng$mar[ng$A11==1]<-1
ng$mar[ng$A11==2]<-2
ng$mar[ng$A11==3]<-2
ng$mar[ng$A11==4]<-3
ng$mar[ng$A11==5]<-3
ng$mar[ng$A11==6]<-4
ng$mar<- factor(ng$mar,
                levels = c(1,2,3,4),
                labels=c("Single", "Married", "Divorced/Separated", "Widowed"))
freq(ng$mar)

ng$marr<-NA
ng$marr[ng$mar=="Single"]<-2
ng$marr[ng$mar=="Married"]<-1
ng$marr[ng$mar=="Divorced/Separated"]<-3
ng$marr[ng$mar=="Widowed"]<-4
ng$marr<- factor(ng$marr,
                 levels = c(1,2,3,4),
                 labels=c("Married", "Single", "Divorced/Separated", "Widowed"))
freq(ng$marr)

ng$empl<-NA
ng$empl[ng$A05==4]<-3
ng$empl[ng$A05==1]<-1
ng$empl[ng$A05==2]<-1
ng$empl[ng$A05==3]<-1
ng$empl[ng$A05==7]<-2
ng$empl[ng$A05==8]<-2
ng$empl[ng$A05==5]<-4
ng$empl[ng$A05==6]<-5
ng$empl<- factor(ng$empl,
                 levels = c(1,2,3,4,5),
                 labels=c("Employed", "Unemployed", "Student", "Homemaker", "Retired"))
freq(ng$empl)

quantile(ng$wealth, probs = seq(0, 1, 1/5))
ng$wealthq<-NA
ng$wealthq[ng$wealth<=2]<-1
ng$wealthq[ng$wealth==3]<-2
ng$wealthq[ng$wealth==4]<- 3
ng$wealthq[ng$wealth==5]<-4
ng$wealthq[ng$wealth>=6 ]<-5
ng$wealthq<- factor(ng$wealthq,
                    levels = c(1,2,3,4,5),
                    labels=c("Lowest", "Low", "Middle", "High", "Highest"))
freq(ng$wealthq)

ng$wq<-NA
ng$wq[ng$wealthq=="Highest"]<-1
ng$wq[ng$wealthq=="High"]<-2
ng$wq[ng$wealthq=="Middle"]<-3
ng$wq[ng$wealthq=="Low"]<-4
ng$wq[ng$wealthq=="Lowest"]<-5
ng$wq<- factor(ng$wq,
               levels = c(1,2,3,4,5),
               labels=c("Highest", "High", "Middle", "Low", "Lowest"))
freq(ng$wq)
ng$former<-NA
ng$former[(ng$B03==1 | ng$B03==2 | ng$C03==1 | ng$B03==2) & (ng$tob=="Not current user")]<-1
ng$former[is.na(ng$former)]=2
freq(ng$former)

ng$never<-NA
ng$never[ng$B03==3 & ng$C03==3]<-1
ng$never[is.na(ng$never)]=2
freq(ng$never)

ng$weight<-ng$gatsweight/10000

ng<- subset(ng, select = -c(AGE, A01, A04, A05, A10, A11, gatsweight))

#DESCRIPTIVE STATS

#Table 1
library(expss)
fre(ng$sex, weight = ng$weight)
fre(ng$ageCAT, weight = ng$weight)
fre(ng$rsd, weight = ng$weight)
fre(ng$edu, weight = ng$weight)
fre(ng$rel, weight = ng$weight)
fre(ng$mar, weight = ng$weight)
fre(ng$empl, weight = ng$weight)
fre(ng$wealthq, weight = ng$weight)
fre(ng$know, weight = ng$weight)
fre(ng$tob, weight = ng$weight)
fre(ng$former, weight = ng$weight)
fre(ng$never, weight = ng$weight)

#Table 2
ng1<-subset(ng, ng$sex=="Male")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$sex=="Female")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="15-24")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="25-34")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="35-44")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="45-54")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="55-64")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="65+")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rsd=="Urban")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rsd=="Rural")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$edu=="Completed College/University")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$edu=="Completed Secondary/High School")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$edu=="Completed Primary/Less than Secondary")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$edu=="No Formal Education/Less than Primary")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rel=="Islam")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rel=="Christianity")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rel=="Traditional/Other")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rel=="None")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$marr=="Single")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$marr=="Married")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$marr=="Divorced/Separated")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$marr=="Widowed")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Employed")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Unemployed")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Student")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Homemaker")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Retired")
fre(ng1$tob, weight = ng1$weight)
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="Lowest")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="Low")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="Middle")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="High")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="Highest")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$know=="Good Knowledge")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$know=="Some Knowledge")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$know=="Poor Knowledge")
fre(ng1$tob, weight = ng1$weight)

ng$tob2<-NA
ng$tob2[ng$B01==1]<-1
ng$tob2[ng$B01==2]<-1
ng$tob2[ng$C01==1]<-2
ng$tob2[ng$C01==2]<-2
ng$tob2[(ng$B01==1 | ng$B01==2)&(ng$C01==1 | ng$C01==2) ]<-3
ng$tob2[is.na(ng$tob2)]<-4
ng$tob2<- factor(ng$tob2,
                 levels = c(1,2,3,4),
                 labels=c("Smoking", "Smokeless", "Both", "None"))
freq(ng$tob2)
fre(ng$tob2, weight = ng$weight)
fre(ng$tob, weight = ng$weight)

#LOGISTIC REGRESSION

library(miceadds)
mod1<-glm.cluster(data=ng, formula=tob ~ sex+ageCAT+rsd+edu+rel+marr+empl+wq+know, cluster= ng$gatscluster, weights = ng$weight, family=binomial)
summary(mod1)
exp(cbind(OR = coef(mod1), confint.default(mod1)))

#Test of Linear Trend

ng$know1<-factor(ng$know, ordered = TRUE, levels = c("Poor Knowledge", "Some Knowledge", "Good Knowledge"))
ng$know1
lm1<-glm(ng$tob~ng$know1, family=binomial)
summary(lm1)

ng$age1<-factor(ng$ageCAT, ordered = TRUE, levels = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"))
ng$age1
lm1<-glm(ng$tob~ng$age1, family=binomial)
summary(lm1)

ng$edu1 <-factor(ng$edu, ordered = TRUE, levels = c("No Formal Education/Less than Primary", "Completed Primary/Less than Secondary", "Completed Secondary/High School", "Completed College/University"))
ng$edu1
lm1<-glm(ng$tob~ng$edu1, family=binomial)
summary(lm1)

ng$wealth1<-factor(ng$wealthq, ordered = TRUE, levels = c("Lowest", "Low", "Middle", "High", "Highest"))
ng$wealth1
lm1<-glm(ng$tob~ng$wealth1, family=binomial)
summary(lm1)

#Tests for model fit
par(mfrow = c(2, 2))
plot(mod1$glm_res)

ng<- subset(ng, select = -c(mar, B01, B03, C01, C03))

#Create universal wealth index for pooled dataset
max(ng$wealth)
ng$wealthc<- (ng$wealth*100)/8
freq(ng$wealthc)

ng$country<- NA
ng$country[is.na(ng$country)]<-"Ethiopia"
ethiopia<-ng
ethiopia<- subset(ethiopia, select = -c(Residence))



#KENYA

library(haven)
#Import dataset (download from https://nccd.cdc.gov/GTSSDataSurveyResources/Ancillary/DownloadAttachment.aspx?DatasetID=3259)
KN<- read_dta("C://Dataset//KN2015.dta")
ng<- subset(KN, select = c(gatsweight, gatscluster, RESIDENCE, age, A01, A04, A05, A06a, A06b, A06c, A06d, A06e, A06f, A06g, A06h, A06i, A06j, A06k, A06m, A06n, A10, A11, B01, B03, C01, C03, H02a, H02b, H02c, H03))
rm(KN)
freq(ng$H02a)
ng$know<- NA
ng$know[ng$H02a==1 & ng$H02b==1 & ng$H02c==1]<-1
ng$know[ng$H02a==1 & ng$H02b==1 & ng$H02c!=1]<-2
ng$know[ng$H02a==1 & ng$H02b!=1 & ng$H02c==1]<-2
ng$know[ng$H02a!=1 & ng$H02b==1 & ng$H02c==1]<-2
ng$know[ng$H02a==1 & ng$H02b!=1 & ng$H02c!=1]<-3
ng$know[ng$H02a!=1 & ng$H02b==1 & ng$H02c!=1]<-3
ng$know[ng$H02a!=1 & ng$H02b!=1 & ng$H02c==1]<-3
ng$know[ng$H02a!=1 & ng$H02b!=1 & ng$H02c!=1]<-3
ng$know <- factor(ng$know,
                  levels = c(1,2,3),
                  labels = c("Good Knowledge", "Some Knowledge",
                             "Poor Knowledge"))
freq(ng$know)
ng<- subset(ng, select = -c(H02a, H02b, H02c))
ng$tob<- NA
ng$tob[ng$B01==1 | ng$B01 ==2]<-1
ng$tob[ng$C01==1 | ng$C01 ==2]<-1
ng$tob[is.na(ng$tob)]=0
freq(ng$tob)
ng$tob <- factor(ng$tob,
                 levels = c(0,1),
                 labels = c("Not current user", "Current User"))
ng$wealth<- NA
ng$wealth[is.na(ng$wealth)]=0
ng$wealth[ng$A06a==1]<-(ng$wealth+2)
ng$wealth[ng$A06b==1]<-(ng$wealth+2)
ng$wealth[ng$A06c==1]<-(ng$wealth+1)
ng$wealth[ng$A06d==1]<-(ng$wealth+1)
ng$wealth[ng$A06e==1]<-(ng$wealth+2)
ng$wealth[ng$A06f==1]<-(ng$wealth+1)
ng$wealth[ng$A06g==1]<-(ng$wealth+1)
ng$wealth[ng$A06h==1]<-(ng$wealth+2)
ng$wealth[ng$A06i==1]<-(ng$wealth+1)
ng$wealth[ng$A06j==1]<-(ng$wealth+1)
ng$wealth[ng$A06k==1]<-(ng$wealth+1)
ng$wealth[ng$A06m==1]<-(ng$wealth+0.5)
ng$wealth[ng$A06n==1]<-(ng$wealth+1)
freq(ng$wealth)
class(ng$wealth)
boxplot(ng$wealth)

ng<- subset(ng, select = -c(A06a, A06b, A06c, A06d, A06e, A06f, A06g, A06h, A06i, A06j, A06k, A06m, A06n))

ng$ageCAT<-NA
ng$ageCAT[ng$age<=24]<-1
ng$ageCAT[ng$age>24 & ng$age<=34]<-2
ng$ageCAT[ng$age>34 & ng$age<=44]<-3
ng$ageCAT[ng$age>44 & ng$age<=54]<-4
ng$ageCAT[ng$age>54 & ng$age<=64]<-5
ng$ageCAT[ng$age>64]<-6
ng$ageCAT <- factor(ng$ageCAT,
                    levels = c(1,2,3,4,5,6),
                    labels = c("15-24", "25-34",
                               "35-44", "45-54", "55-64", "65+"))
freq(ng$ageCAT)

ng$sex<-NA
ng$sex[ng$A01==2]<-1
ng$sex[ng$A01==1]<-2
ng$sex <- factor(ng$sex,
                 levels = c(1,2),
                 labels = c("Female", "Male"))

freq(ng$sex)

ng$rsd<-NA
ng$rsd[ng$RESIDENCE==1]<-1
ng$rsd[ng$RESIDENCE==2]<-2
ng$rsd<- factor(ng$rsd,
                levels = c(1,2),
                labels=c("Urban", "Rural"))
freq(ng$rsd)

ng$edu<-NA
ng$edu[ng$A04==8]<-1
ng$edu[ng$A04==7]<-1
ng$edu[ng$A04==6]<-1
ng$edu[ng$A04==5]<-2
ng$edu[ng$A04==4]<-3
ng$edu[ng$A04==3]<-3
ng$edu[ng$A04==2]<-4
ng$edu[ng$A04==1]<-4
ng$edu<- factor(ng$edu,
                levels = c(1,2,3,4),
                labels=c("Completed College/University", "Completed Secondary/High School", "Completed Primary/Less than Secondary", "No Formal Education/Less than Primary"))
freq(ng$edu)

ng$rel<-NA
ng$rel[ng$A10==2]<-1
ng$rel[ng$A10==3]<-2
ng$rel[ng$A10==4]<-3
ng$rel[ng$A10==5]<-3
ng$rel[ng$A10==6]<-4
ng$rel<- factor(ng$rel,
                levels = c(1,2,3,4),
                labels=c("Islam", "Christianity", "Traditional/Other", "None"))
freq(ng$rel)

ng$mar<-NA
ng$mar[ng$A11==1]<-1
ng$mar[ng$A11==2]<-2
ng$mar[ng$A11==3]<-3
ng$mar[ng$A11==4]<-3
ng$mar[ng$A11==5]<-4
ng$mar<- factor(ng$mar,
                levels = c(1,2,3,4),
                labels=c("Single", "Married", "Divorced/Separated", "Widowed"))
freq(ng$mar)

ng$marr<-NA
ng$marr[ng$mar=="Single"]<-2
ng$marr[ng$mar=="Married"]<-1
ng$marr[ng$mar=="Divorced/Separated"]<-3
ng$marr[ng$mar=="Widowed"]<-4
ng$marr<- factor(ng$marr,
                 levels = c(1,2,3,4),
                 labels=c("Married", "Single", "Divorced/Separated", "Widowed"))
freq(ng$marr)

ng$empl<-NA
ng$empl[ng$A05==4]<-3
ng$empl[ng$A05==1]<-1
ng$empl[ng$A05==2]<-1
ng$empl[ng$A05==3]<-1
ng$empl[ng$A05==7]<-2
ng$empl[ng$A05==8]<-2
ng$empl[ng$A05==5]<-4
ng$empl[ng$A05==6]<-5
ng$empl<- factor(ng$empl,
                 levels = c(1,2,3,4,5),
                 labels=c("Employed", "Unemployed", "Student", "Homemaker", "Retired"))
freq(ng$empl)

quantile(ng$wealth, probs = seq(0, 1, 1/5))
ng$wealthq<-NA
ng$wealthq[ng$wealth<=1]<-1
ng$wealthq[ng$wealth==1.5 | ng$wealth==2.0]<-2
ng$wealthq[ng$wealth==2.5 | ng$wealth==3.0]<-3
ng$wealthq[ng$wealth==3.5 | ng$wealth==4.0]<-4
ng$wealthq[ng$wealth>=4.5 ]<-5
ng$wealthq<- factor(ng$wealthq,
                    levels = c(1,2,3,4,5),
                    labels=c("Lowest", "Low", "Middle", "High", "Highest"))
freq(ng$wealthq)
ng$wq<-NA
ng$wq[ng$wealthq=="Highest"]<-1
ng$wq[ng$wealthq=="High"]<-2
ng$wq[ng$wealthq=="Middle"]<-3
ng$wq[ng$wealthq=="Low"]<-4
ng$wq[ng$wealthq=="Lowest"]<-5
ng$wq<- factor(ng$wq,
               levels = c(1,2,3,4,5),
               labels=c("Highest", "High", "Middle", "Low", "Lowest"))
freq(ng$wq)
ng$former<-NA
ng$former[(ng$B03==1 | ng$B03==2 | ng$C03==1 | ng$B03==2) & (ng$tob=="Not current user")]<-1
ng$former[is.na(ng$former)]=2
freq(ng$former)

ng$never<-NA
ng$never[ng$B03==3 & ng$C03==3]<-1
ng$never[is.na(ng$never)]=2
freq(ng$never)

ng$weight<-ng$gatsweight/10000

ng<- subset(ng, select = -c(age, A01, A04, A05, A10, A11, gatsweight))

#DESCRIPTIVE STATS

#Table 1
library(expss)
fre(ng$sex, weight = ng$weight)
fre(ng$ageCAT, weight = ng$weight)
fre(ng$rsd, weight = ng$weight)
fre(ng$edu, weight = ng$weight)
fre(ng$rel, weight = ng$weight)
fre(ng$mar, weight = ng$weight)
fre(ng$empl, weight = ng$weight)
fre(ng$wealthq, weight = ng$weight)
fre(ng$know, weight = ng$weight)
fre(ng$tob, weight = ng$weight)
fre(ng$never, weight = ng$weight)
fre(ng$former, weight = ng$weight)

#Table 2
ng1<-subset(ng, ng$sex=="Male")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$sex=="Female")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="15-24")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="25-34")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="35-44")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="45-54")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="55-64")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="65+")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rsd=="Urban")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rsd=="Rural")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$edu=="Completed College/University")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$edu=="Completed Secondary/High School")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$edu=="Completed Primary/Less than Secondary")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$edu=="No Formal Education/Less than Primary")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rel=="Islam")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rel=="Christianity")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rel=="Traditional/Other")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rel=="None")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$marr=="Single")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$marr=="Married")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$marr=="Divorced/Separated")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$marr=="Widowed")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Employed")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Unemployed")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Student")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Homemaker")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Retired")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="Lowest")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="Low")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="Middle")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="High")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="Highest")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$know=="Good Knowledge")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$know=="Some Knowledge")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$know=="Poor Knowledge")
fre(ng1$tob, weight = ng1$weight)

ng$tob2<-NA
ng$tob2[ng$B01==1]<-1
ng$tob2[ng$B01==2]<-1
ng$tob2[ng$C01==1]<-2
ng$tob2[ng$C01==2]<-2
ng$tob2[(ng$B01==1 | ng$B01==2)&(ng$C01==1 | ng$C01==2) ]<-3
ng$tob2[is.na(ng$tob2)]<-4
ng$tob2<- factor(ng$tob2,
                 levels = c(1,2,3,4),
                 labels=c("Smoking", "Smokeless", "Both", "None"))
freq(ng$tob2)
fre(ng$tob2, weight = ng$weight)

#LOGISTIC REGRESSION

library(miceadds)
mod1<-glm.cluster(data=ng, formula=tob ~ sex+ageCAT+rsd+edu+rel+marr+empl+wq+know, cluster= ng$gatscluster, weights = ng$weight, family=binomial)
summary(mod1)
exp(cbind(OR = coef(mod1), confint.default(mod1)))

#Test of Linear Trend

ng$know1<-factor(ng$know, ordered = TRUE, levels = c("Poor Knowledge", "Some Knowledge", "Good Knowledge"))
ng$know1
lm1<-glm(ng$tob~ng$know1, family=binomial)
summary(lm1)

ng$age1<-factor(ng$ageCAT, ordered = TRUE, levels = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"))
ng$age1
lm1<-glm(ng$tob~ng$age1, family=binomial)
summary(lm1)

ng$edu1 <-factor(ng$edu, ordered = TRUE, levels = c("No Formal Education/Less than Primary", "Completed Primary/Less than Secondary", "Completed Secondary/High School", "Completed College/University"))
ng$edu1
lm1<-glm(ng$tob~ng$edu1, family=binomial)
summary(lm1)

ng$wealth1<-factor(ng$wq, ordered = TRUE, levels = c("Lowest", "Low", "Middle", "High", "Highest"))
ng$wealth1
lm1<-glm(ng$tob~ng$wealth1, family=binomial)
summary(lm1)

ng<- subset(ng, select = -c(mar, B01, B03, C01, C03, RESIDENCE))
max(ng$wealth)
ng$wealthc<- (ng$wealth*100)/9.5
freq(ng$wealthc)
ng$country<- NA
ng$country[is.na(ng$country)]<-"Kenya"
kenya<-ng

#Tests for model fit

par(mfrow = c(2, 2))
plot(mod1$glm_res)


#SENEGAL

library(haven)
#Import dataset (download from https://nccd.cdc.gov/GTSSDataSurveyResources/Ancillary/DownloadAttachment.aspx?DatasetID=3299)
SN<- read_dta("C://Dataset//SN2017.dta")
ng<- subset(SN, select = c(residence, gatsweight,gatscluster, age, A01, A04, A05, A06a, A06b, A06c, A06d, A06e, A06f, A06g, A06h, A06i, A06j, A06k, A06l, A06m, A06n, A11, B01, B03, C01, C03, H02a, H02b, H02c, H03))
rm(SN)
freq(ng$H02a)
ng$know<- NA
ng$know[ng$H02a==1 & ng$H02b==1 & ng$H02c==1]<-1
ng$know[ng$H02a==1 & ng$H02b==1 & ng$H02c!=1]<-2
ng$know[ng$H02a==1 & ng$H02b!=1 & ng$H02c==1]<-2
ng$know[ng$H02a!=1 & ng$H02b==1 & ng$H02c==1]<-2
ng$know[ng$H02a==1 & ng$H02b!=1 & ng$H02c!=1]<-3
ng$know[ng$H02a!=1 & ng$H02b==1 & ng$H02c!=1]<-3
ng$know[ng$H02a!=1 & ng$H02b!=1 & ng$H02c==1]<-3
ng$know[ng$H02a!=1 & ng$H02b!=1 & ng$H02c!=1]<-3
ng$know <- factor(ng$know,
                  levels = c(1,2,3),
                  labels = c("Good Knowledge", "Some Knowledge",
                             "Poor Knowledge"))
freq(ng$know)
ng<- subset(ng, select = -c(H02a, H02b, H02c))
ng$tob<- NA
ng$tob[ng$B01==1 | ng$B01 ==2]<-1
ng$tob[ng$C01==1 | ng$C01 ==2]<-1
ng$tob[is.na(ng$tob)]=0
freq(ng$tob)
ng$tob <- factor(ng$tob,
                 levels = c(0,1),
                 labels = c("Not current user", "Current User"))
ng$wealth<- NA
ng$wealth[is.na(ng$wealth)]=0
ng$wealth[ng$A06a==1]<-(ng$wealth+2)
ng$wealth[ng$A06b==1]<-(ng$wealth+2)
ng$wealth[ng$A06c==1]<-(ng$wealth+1)
ng$wealth[ng$A06d==1]<-(ng$wealth+1)
ng$wealth[ng$A06e==1]<-(ng$wealth+2)
ng$wealth[ng$A06f==1]<-(ng$wealth+0.5)
ng$wealth[ng$A06g==1]<-(ng$wealth+1)
ng$wealth[ng$A06h==1]<-(ng$wealth+2)
ng$wealth[ng$A06i==1]<-(ng$wealth+1)
ng$wealth[ng$A06j==1]<-(ng$wealth+1)
ng$wealth[ng$A06k==1]<-(ng$wealth+1)
ng$wealth[ng$A06l==1]<-(ng$wealth+1)
ng$wealth[ng$A06m==1]<-(ng$wealth+1)
ng$wealth[ng$A06n==1]<-(ng$wealth+0.5)
freq(ng$wealth)
class(ng$wealth)
boxplot(ng$wealth)

ng<- subset(ng, select = -c(A06a, A06b, A06c, A06d, A06e, A06f, A06g, A06h, A06i, A06j, A06k, A06m, A06n))

ng$ageCAT<-NA
ng$ageCAT[ng$age<=24]<-1
ng$ageCAT[ng$age>24 & ng$age<=34]<-2
ng$ageCAT[ng$age>34 & ng$age<=44]<-3
ng$ageCAT[ng$age>44 & ng$age<=54]<-4
ng$ageCAT[ng$age>54 & ng$age<=64]<-5
ng$ageCAT[ng$age>64]<-6
ng$ageCAT <- factor(ng$ageCAT,
                    levels = c(1,2,3,4,5,6),
                    labels = c("15-24", "25-34",
                               "35-44", "45-54", "55-64", "65+"))
freq(ng$ageCAT)

ng$sex<-NA
ng$sex[ng$A01==2]<-1
ng$sex[ng$A01==1]<-2
ng$sex <- factor(ng$sex,
                 levels = c(1,2),
                 labels = c("Female", "Male"))

freq(ng$sex)

ng$rsd<-NA
ng$rsd[ng$residence==1]<-1
ng$rsd[ng$residence==2]<-2
ng$rsd<- factor(ng$rsd,
                levels = c(1,2),
                labels=c("Urban", "Rural"))
freq(ng$rsd)

ng$edu<-NA
ng$edu[ng$A04==8]<-1
ng$edu[ng$A04==9]<-1
ng$edu[ng$A04==10]<-1
ng$edu[ng$A04==7]<-2
ng$edu[ng$A04==3]<-3
ng$edu[ng$A04==4]<-3
ng$edu[ng$A04==5]<-3
ng$edu[ng$A04==6]<-3
ng$edu[ng$A04==2]<-4
ng$edu[ng$A04==1]<-4
ng$edu<- factor(ng$edu,
                levels = c(1,2,3,4),
                labels=c("Completed College/University", "Completed Secondary/High School", "Completed Primary/Less than Secondary", "No Formal Education/Less than Primary"))
freq(ng$edu)

ng$rel<-NA
ng$rel[ng$A10==2]<-1
ng$rel[ng$A10==3]<-2
ng$rel[ng$A10==4]<-3
ng$rel[ng$A10==5]<-3
ng$rel[ng$A10==6]<-4
ng$rel<- factor(ng$rel,
                levels = c(1,2,3,4),
                labels=c("Islam", "Christianity", "Traditional/Other", "None"))
freq(ng$rel)

ng$mar<-NA
ng$mar[ng$A11==1]<-1
ng$mar[ng$A11==2]<-2
ng$mar[ng$A11==3]<-2
ng$mar[ng$A11==4]<-3
ng$mar[ng$A11==5]<-4
ng$mar<- factor(ng$mar,
                levels = c(1,2,3,4),
                labels=c("Single", "Married", "Divorced/Separated", "Widowed"))
freq(ng$mar)

ng$marr<-NA
ng$marr[ng$mar=="Single"]<-2
ng$marr[ng$mar=="Married"]<-1
ng$marr[ng$mar=="Divorced/Separated"]<-3
ng$marr[ng$mar=="Widowed"]<-4
ng$marr<- factor(ng$marr,
                 levels = c(1,2,3,4),
                 labels=c("Married", "Single", "Divorced/Separated", "Widowed"))
freq(ng$marr)

ng$empl<-NA
ng$empl[ng$A05==4]<-3
ng$empl[ng$A05==1]<-1
ng$empl[ng$A05==2]<-1
ng$empl[ng$A05==3]<-1
ng$empl[ng$A05==7]<-2
ng$empl[ng$A05==8]<-2
ng$empl[ng$A05==5]<-4
ng$empl[ng$A05==6]<-5
ng$empl<- factor(ng$empl,
                 levels = c(1,2,3,4,5),
                 labels=c("Employed", "Unemployed", "Student", "Homemaker", "Retired"))
freq(ng$empl)

quantile(ng$wealth, probs = seq(0, 1, 1/5))
ng$wealthq<-NA
ng$wealthq[ng$wealth<=3.5]<-1
ng$wealthq[ng$wealth==4.0 | ng$wealth==4.5| ng$wealth==5.0| ng$wealth==5.5]<-2
ng$wealthq[ng$wealth==6.0 | ng$wealth==6.5| ng$wealth==7.0]<-3
ng$wealthq[ng$wealth==7.5 | ng$wealth==8.0]<-4
ng$wealthq[ng$wealth>=8.5 ]<-5
ng$wealthq<- factor(ng$wealthq,
                    levels = c(1,2,3,4,5),
                    labels=c("Lowest", "Low", "Middle", "High", "Highest"))
freq(ng$wealthq)

ng$wq<-NA
ng$wq[ng$wealthq=="Highest"]<-1
ng$wq[ng$wealthq=="High"]<-2
ng$wq[ng$wealthq=="Middle"]<-3
ng$wq[ng$wealthq=="Low"]<-4
ng$wq[ng$wealthq=="Lowest"]<-5
ng$wq<- factor(ng$wq,
               levels = c(1,2,3,4,5),
               labels=c("Highest", "High", "Middle", "Low", "Lowest"))
freq(ng$wq)
ng$former<-NA
ng$former[(ng$B03==1 | ng$B03==2 | ng$C03==1 | ng$B03==2) & (ng$tob=="Not current user")]<-1
ng$former[is.na(ng$former)]=2
freq(ng$former)

ng$never<-NA
ng$never[ng$B03==3 & ng$C03==3]<-1
ng$never[is.na(ng$never)]=2
freq(ng$never)

ng$weight<-ng$gatsweight/10000

ng<- subset(ng, select = -c(age, A01, A04, A05, A11, A06l, gatsweight))


#DESCRIPTIVE STATS

#Table 1
library(expss)
fre(ng$sex, weight = ng$weight)
fre(ng$ageCAT, weight = ng$weight)
fre(ng$rsd, weight = ng$weight)
fre(ng$edu, weight = ng$weight)
fre(ng$rel, weight = ng$weight)
fre(ng$mar, weight = ng$weight)
fre(ng$empl, weight = ng$weight)
fre(ng$wealthq, weight = ng$weight)
fre(ng$know, weight = ng$weight)
fre(ng$tob, weight = ng$weight)
fre(ng$never, weight = ng$weight)
fre(ng$former, weight = ng$weight)

#Table 2
ng1<-subset(ng, ng$sex=="Male")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$sex=="Female")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="15-24")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="25-34")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="35-44")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="45-54")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="55-64")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="65+")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rsd=="Urban")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rsd=="Rural")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$edu=="Completed College/University")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$edu=="Completed Secondary/High School")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$edu=="Completed Primary/Less than Secondary")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$edu=="No Formal Education/Less than Primary")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$marr=="Single")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$marr=="Married")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$marr=="Divorced/Separated")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$marr=="Widowed")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Employed")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Unemployed")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Student")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Homemaker")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Retired")
fre(ng1$tob, weight = ng1$weight)
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="Lowest")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="Low")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="Middle")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="High")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="Highest")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$know=="Good Knowledge")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$know=="Some Knowledge")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$know=="Poor Knowledge")
fre(ng1$tob, weight = ng1$weight)

ng$tob2<-NA
ng$tob2[ng$B01==1]<-1
ng$tob2[ng$B01==2]<-1
ng$tob2[ng$C01==1]<-2
ng$tob2[ng$C01==2]<-2
ng$tob2[(ng$B01==1 | ng$B01==2)&(ng$C01==1 | ng$C01==2) ]<-3
ng$tob2[is.na(ng$tob2)]<-4
ng$tob2<- factor(ng$tob2,
                 levels = c(1,2,3,4),
                 labels=c("Smoking", "Smokeless", "Both", "None"))
freq(ng$tob2)
fre(ng$tob2, weight = ng$weight)


#LOGISTIC REGRESSION

library(miceadds)
mod1<-glm.cluster(data=ng, formula=tob ~ sex+ageCAT+rsd+edu+marr+empl+wq+know, cluster= ng$gatscluster, weights = ng$weight, family=binomial)
summary(mod1)
exp(cbind(OR = coef(mod1), confint.default(mod1)))

#Test of Linear Trend

ng$know1<-factor(ng$know, ordered = TRUE, levels = c("Poor Knowledge", "Some Knowledge", "Good Knowledge"))
ng$know1
lm1<-glm(ng$tob~ng$know1, family=binomial)
summary(lm1)

ng$age1<-factor(ng$ageCAT, ordered = TRUE, levels = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"))
ng$age1
lm1<-glm(ng$tob~ng$age1, family=binomial)
summary(lm1)

ng$edu1 <-factor(ng$edu, ordered = TRUE, levels = c("No Formal Education/Less than Primary", "Completed Primary/Less than Secondary", "Completed Secondary/High School", "Completed College/University"))
ng$edu1
lm1<-glm(ng$tob~ng$edu1, family=binomial)
summary(lm1)

ng$wealth1<-factor(ng$wealthq, ordered = TRUE, levels = c("Lowest", "Low", "Middle", "High", "Highest"))
ng$wealth1
lm1<-glm(ng$tob~ng$wealth1, family=binomial)
summary(lm1)

#Tests for model fit
par(mfrow = c(2, 2))
plot(mod1$glm_res)

ng<- subset(ng, select = -c(mar, residence, B01, B03, C01, C03))

#Create universal wealth index for pooled dataset
max(ng$wealth)
ng$wealthc<- (ng$wealth*100)/13
freq(ng$wealthc)
ng$country<- NA
ng$country[is.na(ng$country)]<-"Senegal"
senegal<-ng



#TANZANIA

library(haven)
#Import dataset (download from https://nccd.cdc.gov/GTSSDataSurveyResources/Ancillary/DownloadAttachment.aspx?DatasetID=3434)
TZ<- read_dta("C://Dataset//TZ2018.dta")
ng<- subset(TZ, select = c(gatsweight, residence, gatscluster, AGE, A01, A04, A05, A06A, A06B, A06C, A06D, A06E, A06F, A06G, A06H, A06I, A06J, A06K, A06L, A06M, A11, B01, B03, C01, C03, H02A, H02B, H02C, H03))
rm(TZ)
freq(ng$H02A)
ng$know<- NA
ng$know[ng$H02A==1 & ng$H02B==1 & ng$H02C==1]<-1
ng$know[ng$H02A==1 & ng$H02B==1 & ng$H02C!=1]<-2
ng$know[ng$H02A==1 & ng$H02B!=1 & ng$H02C==1]<-2
ng$know[ng$H02A!=1 & ng$H02B==1 & ng$H02C==1]<-2
ng$know[ng$H02A==1 & ng$H02B!=1 & ng$H02C!=1]<-3
ng$know[ng$H02A!=1 & ng$H02B==1 & ng$H02C!=1]<-3
ng$know[ng$H02A!=1 & ng$H02B!=1 & ng$H02C==1]<-3
ng$know[ng$H02A!=1 & ng$H02B!=1 & ng$H02C!=1]<-3
ng$know <- factor(ng$know,
                  levels = c(1,2,3),
                  labels = c("Good Knowledge", "Some Knowledge",
                             "Poor Knowledge"))
freq(ng$know)
ng<- subset(ng, select = -c(H02A, H02B, H02C))
ng$tob<- NA
ng$tob[ng$B01==1 | ng$B01 ==2]<-1
ng$tob[ng$C01==1 | ng$C01 ==2]<-1
ng$tob[is.na(ng$tob)]=0
freq(ng$tob)
ng$tob <- factor(ng$tob,
                 levels = c(0,1),
                 labels = c("Not current user", "Current User"))
ng$wealth<- NA
ng$wealth[is.na(ng$wealth)]=0
ng$wealth[ng$A06A==1]<-(ng$wealth+2)
ng$wealth[ng$A06B==1]<-(ng$wealth+2)
ng$wealth[ng$A06C==1]<-(ng$wealth+1)
ng$wealth[ng$A06D==1]<-(ng$wealth+1)
ng$wealth[ng$A06E==1]<-(ng$wealth+2)
ng$wealth[ng$A06F==1]<-(ng$wealth+1)
ng$wealth[ng$A06G==1]<-(ng$wealth+1)
ng$wealth[ng$A06H==1]<-(ng$wealth+2)
ng$wealth[ng$A06I==1]<-(ng$wealth+1)
ng$wealth[ng$A06J==1]<-(ng$wealth+1)
ng$wealth[ng$A06K==1]<-(ng$wealth+0.5)
ng$wealth[ng$A06L==1]<-(ng$wealth+1)
ng$wealth[ng$A06M==1]<-(ng$wealth+1)

freq(ng$wealth)
class(ng$wealth)
boxplot(ng$wealth)

ng<- subset(ng, select = -c(A06A, A06B, A06C, A06D, A06E, A06F, A06G, A06H, A06I, A06J, A06K, A06L, A06M))

ng$ageCAT<-NA
ng$ageCAT[ng$AGE<=24]<-1
ng$ageCAT[ng$AGE>24 & ng$AGE<=34]<-2
ng$ageCAT[ng$AGE>34 & ng$AGE<=44]<-3
ng$ageCAT[ng$AGE>44 & ng$AGE<=54]<-4
ng$ageCAT[ng$AGE>54 & ng$AGE<=64]<-5
ng$ageCAT[ng$AGE>64]<-6
ng$ageCAT <- factor(ng$ageCAT,
                    levels = c(1,2,3,4,5,6),
                    labels = c("15-24", "25-34",
                               "35-44", "45-54", "55-64", "65+"))
freq(ng$ageCAT)

ng$sex<-NA
ng$sex[ng$A01==2]<-1
ng$sex[ng$A01==1]<-2
ng$sex <- factor(ng$sex,
                 levels = c(1,2),
                 labels = c("Female", "Male"))

freq(ng$sex)

ng$rsd<-NA
ng$rsd[ng$residence==1]<-1
ng$rsd[ng$residence==2]<-2
ng$rsd<- factor(ng$rsd,
                levels = c(1,2),
                labels=c("Urban", "Rural"))
freq(ng$rsd)

ng$edu<-NA
ng$edu[ng$A04==10]<-1
ng$edu[ng$A04==9]<-2
ng$edu[ng$A04==7]<-2
ng$edu[ng$A04==8]<-2
ng$edu[ng$A04==6]<-2
ng$edu[ng$A04==3]<-3
ng$edu[ng$A04==4]<-3
ng$edu[ng$A04==5]<-3
ng$edu[ng$A04==2]<-4
ng$edu[ng$A04==1]<-4
ng$edu<- factor(ng$edu,
                levels = c(1,2,3,4),
                labels=c("Completed College/University", "Completed Secondary/High School", "Completed Primary/Less than Secondary", "No Formal Education/Less than Primary"))
freq(ng$edu)

ng$rel<-NA
ng$rel[ng$A10==1]<-1
ng$rel[ng$A10==2]<-2
ng$rel[ng$A10==3]<-3
ng$rel[ng$A10==4]<-4
ng$rel[ng$A10==7]<-4
ng$rel<- factor(ng$rel,
                levels = c(1,2,3,4),
                labels=c("Islam", "Christianity", "Traditional/Other", "None"))
freq(ng$rel)

ng$mar<-NA
ng$mar[ng$A11==6]<-1
ng$mar[ng$A11==1]<-2
ng$mar[ng$A11==2]<-2
ng$mar[ng$A11==3]<-2
ng$mar[ng$A11==4]<-3
ng$mar[ng$A11==5]<-3
ng$mar[ng$A11==7]<-4
ng$mar<- factor(ng$mar,
                levels = c(1,2,3,4),
                labels=c("Single", "Married", "Divorced/Separated", "Widowed"))
freq(ng$mar)

ng$marr<-NA
ng$marr[ng$mar=="Single"]<-2
ng$marr[ng$mar=="Married"]<-1
ng$marr[ng$mar=="Divorced/Separated"]<-3
ng$marr[ng$mar=="Widowed"]<-4
ng$marr<- factor(ng$marr,
                 levels = c(1,2,3,4),
                 labels=c("Married", "Single", "Divorced/Separated", "Widowed"))
freq(ng$marr)

ng$empl<-NA
ng$empl[ng$A05==4]<-3
ng$empl[ng$A05==1]<-1
ng$empl[ng$A05==2]<-1
ng$empl[ng$A05==3]<-1
ng$empl[ng$A05==7]<-2
ng$empl[ng$A05==8]<-2
ng$empl[ng$A05==5]<-4
ng$empl[ng$A05==6]<-5
ng$empl<- factor(ng$empl,
                 levels = c(1,2,3,4,5),
                 labels=c("Employed", "Unemployed", "Student", "Homemaker", "Retired"))
freq(ng$empl)

quantile(ng$wealth, probs = seq(0, 1, 1/5))
ng$wealthq<-NA
ng$wealthq[ng$wealth<=1.5]<-1
ng$wealthq[ng$wealth==2.0 |ng$wealth==2.5 |ng$wealth==3.0 |ng$wealth==3.5 | ng$wealth==4.0]<-2
ng$wealthq[ng$wealth==4.5 | ng$wealth==5.0|ng$wealth==5.5 |ng$wealth==6.0 ]<-3
ng$wealthq[ng$wealth==6.5 |ng$wealth==7.0 | ng$wealth==7.5 |ng$wealth==8.0 ]<-4
ng$wealthq[ng$wealth>=8.0 ]<-5
ng$wealthq<- factor(ng$wealthq,
                    levels = c(1,2,3,4,5),
                    labels=c("Lowest", "Low", "Middle", "High", "Highest"))
freq(ng$wealthq)
ng$wq<-NA
ng$wq[ng$wealthq=="Highest"]<-1
ng$wq[ng$wealthq=="High"]<-2
ng$wq[ng$wealthq=="Middle"]<-3
ng$wq[ng$wealthq=="Low"]<-4
ng$wq[ng$wealthq=="Lowest"]<-5
ng$wq<- factor(ng$wq,
                 levels = c(1,2,3,4,5),
                 labels=c("Highest", "High", "Middle", "Low", "Lowest"))
freq(ng$wq)

ng$former<-NA
ng$former[(ng$B03==1 | ng$B03==2 | ng$C03==1 | ng$B03==2) & (ng$tob=="Not current user")]<-1
ng$former[is.na(ng$former)]=2
freq(ng$former)

ng$never<-NA
ng$never[ng$B03==3 & ng$C03==3]<-1
ng$never[is.na(ng$never)]=2
freq(ng$never)

ng$weight<-ng$gatsweight/10000

ng<- subset(ng, select = -c(AGE, A01, A04, A05, A11, gatsweight))


#DESCRIPTIVE STATS

#Table 1
library(expss)
fre(ng$sex, weight = ng$weight)
fre(ng$ageCAT, weight = ng$weight)
fre(ng$rsd, weight = ng$weight)
fre(ng$edu, weight = ng$weight)
fre(ng$rel, weight = ng$weight)
fre(ng$mar, weight = ng$weight)
fre(ng$empl, weight = ng$weight)
fre(ng$wealthq, weight = ng$weight)
fre(ng$know, weight = ng$weight)
fre(ng$tob, weight = ng$weight)
fre(ng$former, weight = ng$weight)
fre(ng$never, weight = ng$weight)

#Table 2
ng1<-subset(ng, ng$sex=="Male")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$sex=="Female")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="15-24")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="25-34")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="35-44")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="45-54")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="55-64")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="65+")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rsd=="Urban")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rsd=="Rural")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$edu=="Completed College/University")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$edu=="Completed Secondary/High School")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$edu=="Completed Primary/Less than Secondary")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$edu=="No Formal Education/Less than Primary")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rel=="Islam")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rel=="Christianity")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rel=="Traditional/Other")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rel=="None")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$marr=="Single")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$marr=="Married")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$marr=="Divorced/Separated")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$marr=="Widowed")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Employed")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Unemployed")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Student")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Homemaker")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Retired")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="Lowest")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="Low")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="Middle")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="High")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="Highest")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$know=="Good Knowledge")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$know=="Some Knowledge")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$know== "Poor Knowledge")
fre(ng1$tob, weight = ng1$weight)

ng$tob2<-NA
ng$tob2[ng$B01==1]<-1
ng$tob2[ng$B01==2]<-1
ng$tob2[ng$C01==1]<-2
ng$tob2[ng$C01==2]<-2
ng$tob2[(ng$B01==1 | ng$B01==2)&(ng$C01==1 | ng$C01==2) ]<-3
ng$tob2[is.na(ng$tob2)]<-4
ng$tob2<- factor(ng$tob2,
                 levels = c(1,2,3,4),
                 labels=c("Smoking", "Smokeless", "Both", "None"))
freq(ng$tob2)
fre(ng$tob2, weight = ng$weight)

#LOGISTIC REGRESSION

library(miceadds)
mod1<-glm.cluster(data=ng, formula=tob ~ sex+ageCAT+rsd+edu+marr+empl+wq+know, cluster= ng$gatscluster, weights = ng$weight, family=binomial)
summary(mod1)
exp(cbind(OR = coef(mod1), confint.default(mod1)))

#Test of Linear Trend

ng$know1<-factor(ng$know, ordered = TRUE, levels = c("Poor Knowledge", "Some Knowledge", "Good Knowledge"))
ng$know1
lm1<-glm(ng$tob~ng$know1, family=binomial)
summary(lm1)

ng$age1<-factor(ng$ageCAT, ordered = TRUE, levels = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"))
ng$age1
lm1<-glm(ng$tob~ng$age1, family=binomial)
summary(lm1)

ng$edu1 <-factor(ng$edu, ordered = TRUE, levels = c("No Formal Education/Less than Primary", "Completed Primary/Less than Secondary", "Completed Secondary/High School", "Completed College/University"))
ng$edu1
lm1<-glm(ng$tob~ng$edu1, family=binomial)
summary(lm1)

ng$wealth1<-factor(ng$wq, ordered = TRUE, levels = c("Lowest", "Low", "Middle", "High", "Highest"))
ng$wealth1
lm1<-glm(ng$tob~ng$wealth1, family=binomial)
summary(lm1)

#Tests for model fit
par(mfrow = c(2, 2))
plot(mod1$glm_res)

ng<- subset(ng, select = -c(mar, residence, B01, B03, C01, C03))

#Create universal wealth index for pooled dataset
max(ng$wealth)
ng$wealthc<- (ng$wealth*100)/12.5
freq(ng$wealthc)
ng$country<- NA
ng$country[is.na(ng$country)]<-"Tanzania"
tanzania<-ng



#UGANDA

library(haven)
#Import dataset (download from https://nccd.cdc.gov/GTSSDataSurveyResources/Ancillary/DownloadAttachment.aspx?DatasetID=3269)
UG<- read_dta("C://Dataset//UG2017.dta")
ng<- subset(UG, select = c(gatsweight, H02_4, gatscluster, residence, age, A01, A04, A05, A06a, A06b, A06c, A06d, A06e, A06f, A06g, A06h, A06i, A06j, A06k, A06l, A06m, A06n, A10, A11, B01, B03, C01, C03, H02a, H02b, H02c, H03))
rm(UG)
ng$know<- NA
ng$know[ng$H02a==1 & ng$H02b==1 & ng$H02c==1]<-1
ng$know[ng$H02a==1 & ng$H02b==1 & ng$H02c!=1]<-2
ng$know[ng$H02a==1 & ng$H02b!=1 & ng$H02c==1]<-2
ng$know[ng$H02a!=1 & ng$H02b==1 & ng$H02c==1]<-2
ng$know[ng$H02a==1 & ng$H02b!=1 & ng$H02c!=1]<-3
ng$know[ng$H02a!=1 & ng$H02b==1 & ng$H02c!=1]<-3
ng$know[ng$H02a!=1 & ng$H02b!=1 & ng$H02c==1]<-3
ng$know[ng$H02a!=1 & ng$H02b!=1 & ng$H02c!=1]<-3
ng$know <- factor(ng$know,
                  levels = c(1,2,3),
                  labels = c("Good Knowledge", "Some Knowledge",
                             "Poor Knowledge"))
freq(ng$know)
ng<- subset(ng, select = -c(H02a, H02b, H02c))
ng$tob<- NA
ng$tob[ng$B01==1 | ng$B01 ==2]<-1
ng$tob[ng$C01==1 | ng$C01 ==2]<-1
ng$tob[is.na(ng$tob)]=0
freq(ng$tob)
ng$tob <- factor(ng$tob,
                 levels = c(0,1),
                 labels = c("Not current user", "Current User"))
ng$wealth<- NA
ng$wealth[is.na(ng$wealth)]=0
ng$wealth[ng$A06a==1]<-(ng$wealth+2)
ng$wealth[ng$A06b==1]<-(ng$wealth+2)
ng$wealth[ng$A06c==1]<-(ng$wealth+1)
ng$wealth[ng$A06d==1]<-(ng$wealth+1)
ng$wealth[ng$A06e==1]<-(ng$wealth+2)
ng$wealth[ng$A06f==1]<-(ng$wealth+1)
ng$wealth[ng$A06g==1]<-(ng$wealth+1)
ng$wealth[ng$A06h==1]<-(ng$wealth+2)
ng$wealth[ng$A06i==1]<-(ng$wealth+1)
ng$wealth[ng$A06j==1]<-(ng$wealth+1)
ng$wealth[ng$A06k==1]<-(ng$wealth+1)
ng$wealth[ng$A06l==1]<-(ng$wealth+1)
ng$wealth[ng$A06m==1]<-(ng$wealth+1)
ng$wealth[ng$A06n==1]<-(ng$wealth+1)

library(descr)
freq(ng$wealth)
class(ng$wealth)
boxplot(ng$wealth)

ng<- subset(ng, select = -c(A06a, A06b, A06c, A06d, A06e, A06f, A06g, A06h, A06i, A06k, A06l, A06m, A06n))

ng$ageCAT<-NA
ng$ageCAT[ng$age<=24]<-1
ng$ageCAT[ng$age>24 & ng$age<=34]<-2
ng$ageCAT[ng$age>34 & ng$age<=44]<-3
ng$ageCAT[ng$age>44 & ng$age<=54]<-4
ng$ageCAT[ng$age>54 & ng$age<=64]<-5
ng$ageCAT[ng$age>64]<-6
ng$ageCAT <- factor(ng$ageCAT,
                    levels = c(1,2,3,4,5,6),
                    labels = c("15-24", "25-34",
                               "35-44", "45-54", "55-64", "65+"))
freq(ng$ageCAT)

ng$sex<-NA
ng$sex[ng$A01==2]<-1
ng$sex[ng$A01==1]<-2
ng$sex <- factor(ng$sex,
                 levels = c(1,2),
                 labels = c("Female", "Male"))

freq(ng$sex)

ng$rsd<-NA
ng$rsd[ng$residence==1]<-1
ng$rsd[ng$residence==2]<-2
ng$rsd<- factor(ng$rsd,
                levels = c(1,2),
                labels=c("Urban", "Rural"))
freq(ng$rsd)

ng$edu<-NA
ng$edu[ng$A04==7]<-1
ng$edu[ng$A04==8]<-1
ng$edu[ng$A04==5]<-2
ng$edu[ng$A04==6]<-2
ng$edu[ng$A04==3]<-3
ng$edu[ng$A04==4]<-3
ng$edu[ng$A04==2]<-4
ng$edu[ng$A04==1]<-4
ng$edu<- factor(ng$edu,
                levels = c(1,2,3,4),
                labels=c("Completed College/University", "Completed Secondary/High School", "Completed Primary/Less than Secondary", "No Formal Education/Less than Primary"))
freq(ng$edu)

ng$rel<-NA
ng$rel[ng$A10==3]<-1
ng$rel[ng$A10==1]<-2
ng$rel[ng$A10==2]<-2
ng$rel[ng$A10==4]<-2
ng$rel[ng$A10==6]<-2
ng$rel[ng$A10==7]<-2
ng$rel[ng$A10==8]<-3
ng$rel[ng$A10==9]<-3
ng$rel[ng$A10==10]<-4
ng$rel<- factor(ng$rel,
                levels = c(1,2,3,4),
                labels=c("Islam", "Christianity", "Traditional/Other", "None"))
freq(ng$rel)

#Variable for "does your religion condemn smoking?"
ng$dis<-NA
ng$dis[ng$H02_4==1]<-1
ng$dis[ng$H02_4==2]<-2
ng$dis[ng$H02_4==7]<-3
ng$dis[ng$H02_4==9]<-3
ng$dis<- factor(ng$dis,
                levels = c(1,2,3),
                labels=c("Yes", "No", "Don't Know"))
freq(ng$dis)

ng$mar<-NA
ng$mar[ng$A11==1]<-1
ng$mar[ng$A11==2]<-2
ng$mar[ng$A11==3]<-3
ng$mar[ng$A11==4]<-3
ng$mar[ng$A11==5]<-4
ng$mar<- factor(ng$mar,
                levels = c(1,2,3,4),
                labels=c("Single", "Married", "Divorced/Separated", "Widowed"))
freq(ng$mar)

ng$marr<-NA
ng$marr[ng$mar=="Single"]<-2
ng$marr[ng$mar=="Married"]<-1
ng$marr[ng$mar=="Divorced/Separated"]<-3
ng$marr[ng$mar=="Widowed"]<-4
ng$marr<- factor(ng$marr,
                 levels = c(1,2,3,4),
                 labels=c("Married", "Single", "Divorced/Separated", "Widowed"))
freq(ng$marr)

ng$empl<-NA
ng$empl[ng$A05==4]<-3
ng$empl[ng$A05==1]<-1
ng$empl[ng$A05==2]<-1
ng$empl[ng$A05==3]<-1
ng$empl[ng$A05==7]<-2
ng$empl[ng$A05==8]<-2
ng$empl[ng$A05==5]<-4
ng$empl[ng$A05==6]<-5
ng$empl<- factor(ng$empl,
                 levels = c(1,2,3,4,5),
                 labels=c("Employed", "Unemployed", "Student", "Homemaker", "Retired"))
freq(ng$empl)

quantile(ng$wealth, probs = seq(0, 1, 1/5))
ng$wealthq<-NA
ng$wealthq[ng$wealth<=3]<-1
ng$wealthq[ng$wealth==4]<-2
ng$wealthq[ng$wealth==5 | ng$wealth==6]<-3
ng$wealthq[ng$wealth==7 | ng$wealth==8]<-4
ng$wealthq[ng$wealth>=9 ]<-5
ng$wealthq<- factor(ng$wealthq,
                    levels = c(1,2,3,4,5),
                    labels=c("Lowest", "Low", "Middle", "High", "Highest"))
freq(ng$wealthq)
ng$wq<-NA
ng$wq[ng$wealthq=="Highest"]<-1
ng$wq[ng$wealthq=="High"]<-2
ng$wq[ng$wealthq=="Middle"]<-3
ng$wq[ng$wealthq=="Low"]<-4
ng$wq[ng$wealthq=="Lowest"]<-5
ng$wq<- factor(ng$wq,
               levels = c(1,2,3,4,5),
               labels=c("Highest", "High", "Middle", "Low", "Lowest"))
freq(ng$wq)
ng$former<-NA
ng$former[(ng$B03==1 | ng$B03==2 | ng$C03==1 | ng$B03==2) & (ng$tob=="Not current user")]<-1
ng$former[is.na(ng$former)]=2
freq(ng$former)

ng$never<-NA
ng$never[ng$B03==3 & ng$C03==3]<-1
ng$never[is.na(ng$never)]=2
freq(ng$never)

ng$weight<-ng$gatsweight/10000

ng<- subset(ng, select = -c(age, A01, A04, A05, A10, A11, gatsweight))


#DESCRIPTIVE STATS

#Table 1
library(expss)
freq(ng$sex, weight = ng$weight)
fre(ng$ageCAT, weight = ng$weight)
fre(ng$residence, weight = ng$weight)
fre(ng$edu, weight = ng$weight)
fre(ng$rel, weight = ng$weight)
fre(ng$mar, weight = ng$weight)
fre(ng$empl, weight = ng$weight)
fre(ng$wealthq, weight = ng$weight)
fre(ng$know, weight = ng$weight)
fre(ng$tob, weight = ng$weight)
fre(ng$former, weight = ng$weight)
fre(ng$never, weight = ng$weight)

#Table 2
ng1<-subset(ng, ng$sex=="Male")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$sex=="Female")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="15-24")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="25-34")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="35-44")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="45-54")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="55-64")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="65+")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rsd=="Urban")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rsd=="Rural")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$edu=="Completed College/University")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$edu=="Completed Secondary/High School")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$edu=="Completed Primary/Less than Secondary")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$edu=="No Formal Education/Less than Primary")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rel=="Islam")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rel=="Christianity")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rel=="Traditional/Other")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rel=="None")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$marr=="Single")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$marr=="Married")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$marr=="Divorced/Separated")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$marr=="Widowed")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Employed")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Unemployed")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Student")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Homemaker")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Retired")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="Lowest")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="Low")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="Middle")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="High")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wealthq=="Highest")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$know=="Good Knowledge")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$know=="Some Knowledge")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$know=="Poor Knowledge")
fre(ng1$tob, weight = ng1$weight)

ng$tob2<-NA
ng$tob2[ng$B01==1]<-1
ng$tob2[ng$B01==2]<-1
ng$tob2[ng$C01==1]<-2
ng$tob2[ng$C01==2]<-2
ng$tob2[(ng$B01==1 | ng$B01==2)&(ng$C01==1 | ng$C01==2) ]<-3
ng$tob2[is.na(ng$tob2)]<-4
ng$tob2<- factor(ng$tob2,
                 levels = c(1,2,3,4),
                 labels=c("Smoking", "Smokeless", "Both", "None"))
freq(ng$tob2)
fre(ng$tob2, weight = ng$weight)

#LOGISTIC REGRESSION

library(miceadds)
mod1<-glm.cluster(data=ng, formula=tob ~ sex+ageCAT+rsd+edu+rel+marr+empl+wq+know, cluster= ng$gatscluster, weights = ng$weight, family=binomial)
summary(mod1)
exp(cbind(OR = coef(mod1), confint.default(mod1)))

#Test of Linear Trend

ng$know1<-factor(ng$know, ordered = TRUE, levels = c("Poor Knowledge", "Some Knowledge", "Good Knowledge"))
ng$know1
lm1<-glm(ng$tob~ng$know1, family=binomial)
summary(lm1)

ng$age1<-factor(ng$ageCAT, ordered = TRUE, levels = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"))
ng$age1
lm1<-glm(ng$tob~ng$age1, family=binomial)
summary(lm1)

ng$edu1 <-factor(ng$edu, ordered = TRUE, levels = c("No Formal Education/Less than Primary", "Completed Primary/Less than Secondary", "Completed Secondary/High School", "Completed College/University"))
ng$edu1
lm1<-glm(ng$tob~ng$edu1, family=binomial)
summary(lm1)

ng$wealth1<-factor(ng$wealthq, ordered = TRUE, levels = c("Lowest", "Low", "Middle", "High", "Highest"))
ng$wealth1
lm1<-glm(ng$tob~ng$wealth1, family=binomial)
summary(lm1)

#Tests for model fit
par(mfrow = c(2, 2))
plot(mod1$glm_res)

ng<- subset(ng, select = -c(mar, residence, B01, B03, C01, C03))

#Create universal wealth index for pooled dataset
max(ng$wealth)
ng$wealthc<- (ng$wealth*100)/14
freq(ng$wealthc)

ng$country<- NA
ng$country[is.na(ng$country)]<-"Uganda"
ng<-subset(ng, rel!="None")
uganda<-ng
uganda<- subset(uganda, select = -c(A06j))


#POOLED DATASET

# Drop "does your religion condemn smoking" variables from Nigeria and Uganda to homogenize variables across all countries
uganda1<- subset(uganda, select = -c(H02_4, dis))
nigeria1<- subset(nigeria, select = -c(H02_4, dis))
ng<- rbind(cameroon, ethiopia, kenya, nigeria1, uganda1, senegal, tanzania)
ng<- rbind(cameroon, ethiopia, kenya, nigeria1, uganda1) #Preferred, as Senegal and Tanzania do not have data on religiosity - a variable of particular interest to this study
complete<-ng

#Create variable for knowledge of harms of SMOKELESS tobacco use
ng$know2<-NA
ng$know2[ng$H03==1]<-1
ng$know2[is.na(ng$know2)]<-2
ng$know2<- factor(ng$know2,
               levels = c(1,2),
               labels=c("Good Knowledge", "Poor Knowledge"))
par(mfrow = c(1, 1))
freq(ng$know2)

#Table 1
library(expss)
fre(ng$sex, weight = ng$weight)
fre(ng$ageCAT, weight = ng$weight)
fre(ng$rsd, weight = ng$weight)
fre(ng$edu, weight = ng$weight)
fre(ng$rel, weight = ng$weight)
fre(ng$mar, weight = ng$weight)
fre(ng$empl, weight = ng$weight)
fre(ng$wq, weight = ng$weight)
fre(ng$know, weight = ng$weight)
fre(ng$tob, weight = ng$weight)
fre(ng$former, weight = ng$weight)
fre(ng$never, weight = ng$weight)

#Table 2
ng1<-subset(ng, ng$sex=="Male")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$sex=="Female")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="15-24")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="25-34")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="35-44")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="45-54")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="55-64")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$ageCAT=="65+")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rsd=="Urban")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rsd=="Rural")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$edu=="Completed College/University")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$edu=="Completed Secondary/High School")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$edu=="Completed Primary/Less than Secondary")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$edu=="No Formal Education/Less than Primary")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rel=="Islam")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rel=="Christianity")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rel=="Traditional/Other")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$rel=="None")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$marr=="Single")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$marr=="Married")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$marr=="Divorced/Separated")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$marr=="Widowed")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Employed")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Unemployed")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Student")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Homemaker")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$empl=="Retired")
fre(ng1$tob, weight = ng1$weight)
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wq=="Lowest")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wq=="Low")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wq=="Middle")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wq=="High")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$wq=="Highest")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$know=="Good Knowledge")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$know=="Some Knowledge")
fre(ng1$tob, weight = ng1$weight)
ng1<-subset(ng, ng$know=="Poor Knowledge")
fre(ng1$tob, weight = ng1$weight)

fre(ng$tob2, weight = ng$weight)


#LOGISTIC REGRESSION

library(miceadds)
mod1<-glm.cluster(data=ng, formula=tob ~ sex+ageCAT+rsd+edu+rel+marr+empl+wq+know, cluster= ng$gatscluster, weights = ng$weight, family=binomial)
summary(mod1)
exp(cbind(OR = coef(mod1), confint.default(mod1)))


#Test of Linear Trend

ng$know1<-factor(ng$know, ordered = TRUE, levels = c("Poor Knowledge", "Some Knowledge", "Good Knowledge"))
ng$know1
lm1<-glm(ng$tob~ng$know1, family=binomial)
summary(lm1)

ng$age1<-factor(ng$ageCAT, ordered = TRUE, levels = c("15-24", "25-34", "35-44", "45-54", "55-64", "65+"))
ng$age1
lm1<-glm(ng$tob~ng$age1, family=binomial)
summary(lm1)

ng$edu1 <-factor(ng$edu, ordered = TRUE, levels = c("No Formal Education/Less than Primary", "Completed Primary/Less than Secondary", "Completed Secondary/High School", "Completed College/University"))
ng$edu1
lm1<-glm(ng$tob~ng$edu1, family=binomial)
summary(lm1)

ng$wealth1<-factor(ng$wq, ordered = TRUE, levels = c("Lowest", "Low", "Middle", "High", "Highest"))
ng$wealth1
lm1<-glm(ng$tob~ng$wealth1, family=binomial)
summary(lm1)
levels(ng$tob2)

#Tests for model fit
par(mfrow = c(2, 2))
plot(mod1$glm_res)


#STRATIFIED BY TOBACCO CATEGORY: SMOKED & SMOKELESS
library(broom.mixed)
library(jtools) #Plot regression coefficients
par(mfrow = c(1, 1))

#Create dependent variable for current tobacco smoking
ng1<-ng
ng1$sl <-NA
ng1$sl[ng1$tob2=="Smoking"]<-1
ng1$sl[ng1$tob2=="Both"]<-1
ng1$sl[ng1$tob2=="Smokeless"]<-0
ng1$sl[ng1$tob2=="None"]<-0
freq(ng1$sl)

#Logistic Regression
logistic.model1 <- glm(sl ~  sex+ageCAT+rsd+edu+rel+marr+empl+wq+know+country, data = ng1, weights = ng1$weight, family=binomial)
summ(logistic.model1,  robust="HC1", cluster="gatscluster", exp=TRUE, digits = 4)

#Create dependent variable for current smokeless tobacco use
ng1<-ng
ng1$sl <-NA
ng1$sl[ng1$tob2=="Smoking"]<-0
ng1$sl[ng1$tob2=="Both"]<-1
ng1$sl[ng1$tob2=="Smokeless"]<-1
ng1$sl[ng1$tob2=="None"]<-0
freq(ng1$sl)

#Logistic Regression
logistic.model2 <- glm(sl ~  sex+ageCAT+rsd+edu+rel+marr+empl+wq+know2+country, data = ng1, weights = ng1$weight, family=binomial)
summ(logistic.model2,  robust="HC1", cluster="gatscluster", exp=TRUE, digits = 4)

#Plot coefficients and 95% CI from both smoking and SLT models
plot_summs(logistic.model1, logistic.model2, cluster="gatscluster", robust=TRUE, exp=TRUE, coefs = c("Male"="sexMale", "25-34 years"="ageCAT25-34", "35-44 years"="ageCAT35-44", "45-54 years"="ageCAT45-54", "55-64 years"="ageCAT55-64", "65+ years"="ageCAT65+", "Rural"="rsdRural", "Completed Secondary/High School" = "eduCompleted Secondary/High School", "Completed Primary/Less than Secondary"="eduCompleted Primary/Less than Secondary", "No Formal Education/Less than Primary"="eduNo Formal Education/Less than Primary", "Christian"="relChristianity", "Traditional/Other"="relTraditional/Other", "No Religion"="relNone", "Never Married"="marrSingle", "Divorced or Separated"="marrDivorced/Separated", "Widowed"="marrWidowed", "Unemployed"="emplUnemployed", "Wealth - Rich"="wqHigh","Wealth - Middle"="wqMiddle", "Wealth - Poor"="wqLow", "Wealth - Poorest"="wqLowest", "Some Knowledge of smoking harms"="knowSome Knowledge", "Poor Knowledge of smoking harms"="knowPoor Knowledge", "Poor Knowledge of SLT harms"="know2Poor Knowledge"), model.names=c("Tobacco smoking", "Smokeless tobacco use"), legend.title = "Tobacco use category" ) 


#STRATIFID BY SEX

#Women
ng2<-subset(ng1, ng1$sex=="Female") #Create a version of pooled dataset with females only
#Repeat the above analysis for smoked and smokeless tobacco use.

#Men
ng2<-subset(ng1, ng1$sex=="Male") #Create a version of pooled dataset with females only
#Repeat the above analysis for smoked and smokeless tobacco use.


#FINAL ANALYSIS: PARTICIPANTS WHO WERE ASKE IF THEIR RELIGION DISAPPROVES OF SMOKING

ng1<-rbind(nigeria, uganda)
crosstab(ng$rel, ng$dis, prop.c = TRUE, weight = ng$weight)

#Tobacco Smoking
ng1$sl <-NA
ng1$sl[ng1$tob2=="Smoking"]<-1
ng1$sl[ng1$tob2=="Both"]<-1
ng1$sl[ng1$tob2=="Smokeless"]<-0
ng1$sl[ng1$tob2=="None"]<-0
freq(ng1$sl)

#Logistic Model
mod1<-glm.cluster(data=ng1, formula=sl ~ sex+ageCAT+rsd+edu+rel+dis+marr+empl+wq+know+country, cluster= ng1$gatscluster, weights = ng1$weight, family=binomial)
summary(mod1)
exp(cbind(OR = coef(mod1), confint.default(mod1)))

#Smokeless Tobacco Use
ng1$sl <-NA
ng1$sl[ng1$tob2=="Smoking"]<-0
ng1$sl[ng1$tob2=="Both"]<-1
ng1$sl[ng1$tob2=="Smokeless"]<-1
ng1$sl[ng1$tob2=="None"]<-0
freq(ng1$sl)

#Knowledge of the harms of Smokeless tobacco use
ng1$know2<-NA
ng1$know2[ng1$H03==1]<-1
ng1$know2[is.na(ng1$know2)]<-2
ng1$know2<- factor(ng1$know2,
                   levels = c(1,2),
                   labels=c("Good Knowledge", "Poor Knowledge"))
freq(ng1$know2)

#Logistic Model
mod1<-glm.cluster(data=ng1, formula=sl ~ sex+ageCAT+rsd+edu+rel+dis+marr+empl+wq+know2+country, cluster= ng1$gatscluster, weights = ng1$weight, family=binomial)
summary(mod1)
exp(cbind(OR = coef(mod1), confint.default(mod1)))

#THE END
