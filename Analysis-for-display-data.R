### Load all the packages needed for this project

rm(list = ls())
load.packages<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}

load.packages(ggplot2)
load.packages(lubridate)
load.packages(foreach)
load.packages(doParallel)
load.packages(openair)

library(ggplot2)
library(openair)
library(lubridate)
library(foreach)
library(doParallel)
library(plyr)
registerDoParallel(cores=4) 

##********************************************************
## 1) Pull in 'Anonymized Display Data.csv'. 
setwd('C:/Users/Alice/Desktop') # Set my current work directory
data = read.csv("Anonymized Display Data.csv") # Read and name the dataset as data
##********************************************************

##********************************************************
## 2) Perform your typical data exploration techniques when 
##      working with a new dataset.
##********************************************************

head(data)      # Get the first six rows of the dataset
tail(data)      # Get last 6 rows of the dataset
summary(data)   # Get a basic summary of the dataset
dim(data)       # Check the dimension of the dataset
edit(data)      # Open data editor
str(data)       # Provides the structure of the dataset
names(data)     # Lists variables in the dataset

##********************************************************
## 3) Create a data frame that contains only data for Client A 
##      and Client B and limit the columns to date, client, and 
##      impressions. Do not aggregate any rows of data.
##********************************************************

subdata= subset(data, client_id == "Client A" | client_id == "Client B", 
                select = c(date, client_id, impressions) )
subdata       # subdata constains date, client_id, and impressions
head(subdata) # Get the first six rows of subdata

##********************************************************
## 4) Plot mediacost by date for Client G for the month of June. 
##      Use any plotting system you prefer.
##********************************************************

### Select data in June

data$date = ymd(data$date)
data.June = selectByDate(data, start = "2016-06-01", end = "2016-06-30") 

### Select mediacost by date for Client G for the month of June

June.client_G = subset(data.June, client_id =='Client G',select = c(mediacost, date)) 

### Use ggplot to visualize June.Client_G

ggplot(June.client_G, aes(date, mediacost)) + labs(title = "Client G Mediacost in June") + 
    geom_bar(stat = "identity", position = "dodge", fill = "lightskyblue") +
    xlab("June") + ylab("Daily Mediacost")+theme(plot.title = element_text(hjust = 0.5))


##********************************************************
## 5) Plot CPM by date and tactic for all tactics. Use any 
##      plotting system you prefer.
##      Note: CPM = mediacost / (impressions / 1000)
##            CPM stands for Cost Per 1000 Impressions
##********************************************************

### Compute CPM according to the formula above
CPM = data$mediacost / (data$impressions / 1000) 

### new_data contains CPM and the original dataset
new_data = as.data.frame(cbind(CPM, data)) 


### Plot CPM by date and tactic for all the tactics
ggplot(new_data, aes(x = date, y = ifelse(is.na(CPM), 0, CPM), fill=tactic)) + 
    theme_bw() + geom_bar(stat="identity", position = "identity")+
    ggtitle("CPM By Date For All Tactics")+
    theme(plot.title = element_text(hjust = 0.5)) + ylab("CPM")+xlab("Date")+
    coord_cartesian(ylim=c(0,1000))


### According to the plot above, we can see that if we plot CPM by date for all the tactics,
### the plot is very cluttered and busy. To solve this issue, we can choose to clean the orignial data
### or to select some of the tactics to do in-depth analysis.
### So,to make the plot more readable, I selected top several most commonly used tactics as an example

### First, I selected the tactics by taking a look at the frequency of all the tactics

qplot(factor(tactic), data=new_data, geom="bar", main = "Barplot for Tactic",
      xlab ="Tactic",ylab = "Frequency",fill=factor(tactic))+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))+
      theme(plot.title = element_text(hjust = 0.5))

### According the barplot above, the top 4 frequently used tactics are "audience",
### "contextual", "pbt", and "rtg". So I took these four tactics out to make further analysis
### as following. If needed, I can also analyze other tactics specifically

### new_data2 contains tactics: audience, contextual, pbt, and rtg

new_data2= subset(new_data, (tactic == "audience" | tactic == "contextual" | 
                               tactic=="pbt"| tactic=="rtg"))
head(new_data2)

### Analyze CPM for the four tactics (audience, contextual, pbt, rtg) by month

new_data2$month = month(as.POSIXlt(new_data2$date, format="%Y-%m-%d"))
 
ggplot(na.omit(new_data2), aes(x = month, y = ifelse(is.na(CPM), 0, CPM), fill=tactic)) +
    geom_bar(stat = "identity", position = "dodge")+
    ggtitle("CPM By Date For \n Audience, Contextual, PBT, RTG")+
    theme(plot.title = element_text(hjust = 0.5))+ylab("CPM") +xlab("Month")
   
  
##********************************************************
##
## 6) Forecast impressions in Australia for Client F by day
##      for August.
##********************************************************


### Pull out the data for Client F and Australia

new_data3=subset(data, client_id == 'Client F') #new_data3 contains data for Client F
new_data4=subset(new_data3, country == 'au') # new_data 4 constains data for Client F and Australia

### Use BIC to measure the optimal model order

BIC_log=rep(0,10)
foreach (n=1:3) %dopar% {
 fit=arima(data$impression, order = c(2*n,0,2*n-1))
 BIC_log[n]=BIC(fit)
}

### By continuously increasing the model order, the minumum BIC value is found at ARMA(2,1).
### Then ARMA(2,1) is used for prediction

fit = arima(new_data4$impression, order = c(2,0,1))
BIC(fit)
aug_data=predict(fit, 31) #aug_data means predicted impression in August

### Plot predicted impressions in August
days =seq(as.Date("2016/8/1"), as.Date("2016/8/31"), by = "days")
plot(days, aug_data$pred, type = "b", main ="Predicted Impressions In August", xlab="Date",
     ylab = "Impressions",pch =19, cex.axis=1, lwd = 2, col = "lightskyblue") 
lines(days, aug_data$pred, col = "lightskyblue")

### Plot impressions from Jan to August
aug.imp = data.frame(cbind(days,aug_data$pred)) # Convert predicted impressions in Aug into data.frame
names(aug.imp)[1] = paste("date") # Change the colomn name in aug.imp
names(aug.imp)[2] = paste("impressions")  # Change the colomn name in aug.imp
Imp.seven = new_data4[,c("impressions","date")] #Select impressions from Jan to Jul

new_data5 = rbind.data.frame(aug.imp,Imp.seven) # Combind impressions from Jan to Aug
new_data5$date = as.Date(new_data5$date,origin="1970-01-01")
new_data5$month = month(as.POSIXlt(new_data5$date, format="%Y-%m-%d"))

### Calculate the mean impression for each month
month_imp = ddply(new_data5, .(month), summarize,  meanIMP=mean(impressions))

### Plot monthly average impressions from Jan to Aug 
ggplot(month_imp, aes(month, meanIMP)) + labs(title = "Average Impressions From Jan to Aug") + 
  geom_bar(stat = "identity", position = "dodge", fill = "lightskyblue") +
  xlab("Jan to Aug") + ylab("Average Impressions")+
  geom_smooth(method = 'loess')+theme(plot.title = element_text(hjust = 0.5))

### Plot confidence interval for my predictions

aug_imp =as.numeric((aug_data$pred)) #August Impressions Prediction
# Assuming Gaussian residues (as an adequate model is assumed), 
# the distribution of the error of the model should follow a Gaussian distribution. 
# So to find a 95% confidence interval for the prediction, the normal distribution table is used. 
# Through looking up Z0.975 (two-tailed distribution is assumed) in the table, the value comes up as 1.96.

aug_lb=aug_data$pred-1.96*sqrt(aug_data$se) #Get 95% confidence interval lower bound
aug_ub=aug_data$pred+1.96*sqrt(aug_data$se) #Get hight 95% confidence interval upper bound

CI=data.frame(days,aug_imp,aug_lb, aug_ub) #Combind all the three intervals

ggplot(data = CI, aes(x = days)) + 
  geom_point(aes(y = CI$aug_ub, color = "95% CI UB"))+
  geom_line(aes(y = CI$aug_imp, color = "Prediction")) +
  geom_point(aes(y = CI$aug_lb, color = "95% CI LB")) + 
  labs(y = "Predicted Impressions in August")+
  scale_colour_manual("", 
                      breaks = c("Prediction", "95% CI LB", "95% CI UB"),
                      values = c("lightskyblue", "green", "dark red"))+
  scale_x_date(date_minor_breaks = "1 day")+
  ggtitle("Confidence Interval")+
  theme(plot.title = element_text(hjust = 0.5))+xlab("Date")
  


#########################################################
##
## End of Project by Alice Zhang. Thank you!
##      
#########################################################
