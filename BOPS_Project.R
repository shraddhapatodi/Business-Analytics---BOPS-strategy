#==================================================================
## Title: BUY-ONLINE-PICKUP-INSTORE(BOPS) STRATEGY FOR RETAILERS
## Submitted by: Arjoo Gangwal, Randall Nguyen, Shraddha Patodi
#==================================================================

#==================================================================
## SET UP R MARKDOWN
#==================================================================

#Clearing the working space
rm(list = ls())

#Setting the working directory
setwd("/Users/ag/Desktop/BOPS")

#Install Packages
install.packages("readstata13")
install.packages("stargazer")
install.packages("ggplot2")
install.packages("gdata")
install.packages("ggeffects")
install.packages("QuantPsyc")
install.packages("VIF")
install.packages("multiwayvcov")
install.packages("lmtest")
install.packages("AER")
install.packages("usdm")
install.packages("aod")
install.packages("mfx")

#Load libraries
library(stargazer)
library(gdata)
library(ggplot2)
library(psych) 
library(ggeffects)
library(QuantPsyc)
library(VIF)
library(lmtest)
library(sandwich)
library(multiwayvcov)
library(AER)
library(readstata13)
library(usdm)
library(aod)
library(Rcpp)
library(mfx)
library(nnet)
library(reshape2)

# turning off scientific notation except for big numbers
options(scipen = 9)

#====================================================================================#
## Data Ingestion
#====================================================================================#
```{r}
consumer_level = read.dta13("/Users/ag/Desktop/BOPS/consumer\ level\ data.dta")
ODS_prod_cat = read.dta13("/Users/ag/Desktop/BOPS/online\ daily\ prod_cat\ sales-returns\ data.dta")
ODS_sal_ret = read.dta13("/Users/ag/Desktop/BOPS/online\ daily\ sales-returns\ data.dta")
trans_level = read.dta13("/Users/ag/Desktop/BOPS/transaction\ level\ data.dta")
```
#=======================================================================================#
## Question1: What is the impact of implementing BOPS strategy on online channel sales?
#=======================================================================================#
```{r}
df1<- data.frame(ODS_sal_ret)

# Summary statistics
stargazer(df1, type="text", median=TRUE, iqr=TRUE,digits=1, title="Descriptive Statistics")

#Raw data Plots

#Plotting salesvalue with bops_in_effect to see a correlation
ggplot(df1, aes(y=salesvalue, x=bops_in_effect)) + geom_point(size = 2.5, colour = "black") + xlab("Bops in Effect") + ylab("Sales($)")

#We also plot the distribution of salesvalue
ggplot(df1, aes(x=salesvalue)) + geom_histogram(colour="green")

#Using log(salesvalue) the histogram is more normal. So we will use log(salesvalue) for our model
ggplot(df1, aes(x=log(salesvalue))) + geom_histogram(colour="green") 

#We also plot the distribution of salesquantity
ggplot(df1, aes(x=salesquantity)) + geom_histogram(colour="green") 

#Using log(salesquantity) the histogram is more normal. So we will use log(salesquantity) for our model
ggplot(df1, aes(x=log(salesquantity))) + geom_histogram(colour="green") 


#Creating a new variables for ln values
df1$log_salesvalue <- log(df1$salesvalue+1)
df1$log_salesquantity <- log(df1$salesquantity)


#Replacing Null Values with Mean
df1$avg_age[is.na(df1$avg_age)] <- mean(df1$avg_age, na.rm = TRUE)

df1$avg_female[is.na(df1$avg_female)] <- mean(df1$avg_female, na.rm = TRUE)

df1$avg_income[is.na(df1$avg_income)] <- mean(df1$avg_income, na.rm = TRUE)

df1$avg_homeowner[is.na(df1$avg_homeowner)] <- mean(df1$avg_homeowner, na.rm = TRUE)

df1$avg_residency[is.na(df1$avg_residency)] <- mean(df1$avg_residency, na.rm = TRUE)

df1$avg_childowner[is.na(df1$avg_childowner)] <- mean(df1$avg_childowner, na.rm = TRUE)

#Filtering the data before 27th September,2012 when bops was only implemented on stores 2 & 6
df2 <- df1[(df1$day<789),]

#Divide the data into two groups one where BOPS was implemented(according to the timeline), other when BOPS was not implemented.Before 1st Aug 2011, BOPS wasn't implemented, After 1st Aug 2011 BOPS was implemented. This is done to get data into a single time frame to measure the pure impact of BOPS.
df2$BIE <- ifelse(df2$day<366,0,1)
#Grouping store into a varible group_store as BOPS was implemented on store 2&6 simultaneously and on store 5998 later
df2$group_store <- ifelse((df2$store_number == 2) | (df2$store_number == 6),1,0)

# Summary statistics
stargazer(df2, type="text", median=TRUE, iqr=TRUE,digits=1, title="Descriptive Statistics")

# Checking for multi-collinearity
df_mcol1 = df2[c("BIE", "group_store","month_dummy","year","day","avg_female","avg_age","avg_income","avg_homeowner","avg_residency","avg_childowner")]
cor(df_mcol1)
vifcor(df_mcol1)

#Since the day, year and BIE variables have multicollinearity, upon checking their correlations we observe that year and day have high correlation with BIE
#Removing year and day from the data and recalculating VIF Scores

df_mcol1 = df2[c("BIE", "group_store","month_dummy","avg_female","avg_age","avg_income","avg_homeowner","avg_residency","avg_childowner")]
cor(df_mcol1)
vifcor(df_mcol1)
#By removing day and year, we removed the multicollinearity from the data

#=================================IMPACT ON SALES VALUE======================================================#


#Final Model for measuring the impact of BOPS on salesvalue
model1a<- lm(log_salesvalue~BIE+group_store+(BIE*group_store)+factor(month_dummy)+avg_female+avg_age+avg_income+avg_homeowner+avg_residency+avg_childowner, data=df2)
stargazer(model1a,
          title="Regression Results", type="text", 
          column.labels=c("Model-2"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

#Test for heteroskedastacity
gqtest(model1a) # Goldfeld-Quandt test indicates no heteroskedasticity
bptest(model1a) # Breusch-Pagan test indicates heteroskedasticity

# Reoplacing SEs with Robust SEs since there is heteroskedasticity in the data
HWrobstder1a <- sqrt(diag(vcovHC(model1a, type="HC1"))) # produces Huber-White robust standard errors 
stargazer(model1a, model1a,
          se=list(NULL, HWrobstder1a),
          title="OLS Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

# Visualize the output using marginal effects
meffects1a <- ggpredict(model1a, terms=c("BIE", "group_store"))   
ggplot(meffects1a,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
  xlab("BOPS IN EFFECT") + ylab("Sales Value") +
  labs(colour="group_store") + 
  scale_colour_discrete(labels=c("5998","2&6")) +
  scale_x_continuous(breaks=c(0,1), labels=c("BOPS not in effect", "BOPS in effect")) +
  theme(axis.title.x=element_blank())

#The interaction coefficient for BIE:group_store is significant with value -0.4781. This means that the implementation of BOPS is associated with 47.81% decrease in the sales value.

#================================IMPACT ON SALES QUANTITY=======================================================#
#Since sales quantity is a count variable using poisson and negative binomial models
poisson1b <- glm(log_salesquantity~BIE+group_store+(BIE*group_store)+factor(month_dummy)+avg_female+avg_age+avg_income+avg_homeowner+avg_residency+avg_childowner, family="poisson", data=df2)
stargazer(poisson1b,  
          title="Poisson Results", type="text", 
          column.labels=c("Model-1b"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) 

# Model fit assessment - Poisson
poisson1bn <- glm(log_salesquantity~1, data=df2, family="poisson") 

#Likelihood ration test
lrtest(poisson1b, poisson1bn) #gives a significant p-value which indicates model does not fit the data.

#Since Poisson doesn't fit the data, we will check for negative binomial model.
negbin1b <- glm.nb(log_salesquantity~BIE+group_store+(BIE*group_store)+factor(month_dummy)+avg_female+avg_age+avg_income+avg_homeowner+avg_residency+avg_childowner, data = df2) 
stargazer(negbin1b,
          apply.coef = exp, t.auto=F, p.auto = F,
          title="Negative Bionomial Results", type="text", 
          column.labels=c("IRRs"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

# Model fit assessment - Negative Binomial
negbin1bn <- glm.nb(log_salesquantity ~ 1, data = df2) 
lrtest(negbin1b, negbin1bn) ## Test results signifies that the model fits the data.

#Test for heteroskedastacity
gqtest(negbin1b) # Goldfeld-Quandt test indicates no heteroskedasticity
bptest(negbin1b) # Breusch-Pagan test indicates heteroskedasticity

# Reoplacing SEs with Robust SEs since there is heteroskedasticity in the data
HWrobstder1b <- sqrt(diag(vcovHC(negbin1b, type="HC1"))) # produces Huber-White robust standard errors 
stargazer(negbin1b, negbin1b,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(NULL, HWrobstder1a),
          title="NB Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))
#The interaction coefficient for BIE_timeline:group_store is significant with value 0.8947. This means that the implementation of BOPS is associated with 10.53% decrease in the sales quantity.

# Visualize the output
meffects1b <- ggpredict(negbin1b, terms=c("BIE", "group_store")) # generates a tidy data frame  
ggplot(meffects1b,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
  xlab("BOPS IN EFFECT") + ylab("Sales Quantity") +
  labs(colour="group_store") + 
  scale_colour_discrete(labels=c("5998","2&6")) +
  scale_x_continuous(breaks=c(0,1), labels=c("BOPS not in effect", "BOPS in effect")) +
  theme(axis.title.x=element_blank()) 

```
#=======================================================================================#
## Question2: What is the impact of implementing BOPS strategy on online channel returns?
#=======================================================================================#

#We also plot the distribution of returnvalue
ggplot(df2, aes(x=returnvalue)) + geom_histogram(colour="green")

#Using log(returnvalue) the histogram is more normal. So we will use log(returnvalue) for our model
ggplot(df2, aes(x=log(returnvalue))) + geom_histogram(colour="green") 

#We also plot the distribution of returnquantity
ggplot(df2, aes(x=returnquantity)) + geom_histogram(colour="green") 

#Using log(returnquantity) the histogram is more normal. So we will use log(returnquantity) for our model
ggplot(df2, aes(x=log(returnquantity))) + geom_histogram(colour="green")

#Creating new variables for ln values
df2$log_returnquantity <- log(df2$returnquantity+1)
df2$log_returnvalue <- log(df2$returnvalue+1)

#====================================IMPACT ON RETURN VALUE===================================================#
#Final Model for measuring the impact of BOPS on returnvalue
model2a<- lm(log_returnvalue~BIE+group_store+(BIE*group_store)+factor(month_dummy)+log_salesvalue+avg_female+avg_age+avg_income+avg_homeowner+avg_residency+avg_childowner, data=df2)
stargazer(model2a,
          title="Regression Results", type="text", 
          column.labels=c("Model-2a"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) 

#Test for heteroskedastacity
gqtest(model2a) # Goldfeld-Quandt test indicates no heteroskedasticity
bptest(model2a) # Breusch-Pagan test indicates heteroskedasticity

# Reoplacing SEs with Robust SEs since there is heteroskedasticity in the data
HWrobstder2a <- sqrt(diag(vcovHC(model2a, type="HC1"))) # produces Huber-White robust standard errors 
stargazer(model2a, model2a,
          se=list(NULL, HWrobstder1a),
          title="OLS Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))
#The interaction coefficient for BIE:group_store is significant with value -0.6669. This means that the implementation of BOPS is associated with 66.69% decrease in the return value.

# Visualize the output using marginal effects
meffects2a <- ggpredict(model2a, terms=c("BIE", "group_store"))   

ggplot(meffects2a,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
  xlab("BOPS In Effect") + ylab("Return Value") +
  labs(colour="group_store") + 
  scale_colour_discrete(labels=c("5998","2&6")) +
  scale_x_continuous(breaks=c(0,1), labels=c("BOPS not in effect", "BOPS in effect")) +
  theme(axis.title.x=element_blank())

#===================================IMPACT ON RETURN QUANTITY====================================================#

#Poisson model for measuring the impact of BOPS on returnquantity
poisson2b <- glm(returnquantity~BIE+group_store+(BIE*group_store)+factor(month_dummy)+log_salesquantity+avg_female+avg_age+avg_income+avg_homeowner+avg_residency+avg_childowner, family="poisson", data=df2)
stargazer(poisson2b,  
          title="Poisson Results", type="text", 
          column.labels=c("Model-2b"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) 

# Model fit assessment - Poisson
poisson2bn <- glm(returnquantity~1, data=df2, family="poisson") 
lrtest(poisson1b, poisson2bn) #Likelihood ratio test signifies that model does not fit the data.

#Since Poisson doesn't fit the data, we will check for negative binomial model.
negbin2b <- glm.nb(returnquantity~BIE+group_store+(BIE*group_store)+factor(month_dummy)+log_salesquantity+avg_female+avg_age+avg_income+avg_residency+avg_homeowner+avg_childowner, data = df2) 
stargazer(negbin2b,
          apply.coef = exp, t.auto=F, p.auto = F,
          title="Negative Bionomial Results", type="text", 
          column.labels=c("IRRs"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

# Model fit assessment - Negative Binomial
negbin2bn <- glm.nb(returnquantity ~ 1, data = df2) 
lrtest(negbin1b, negbin2bn) ## Likelihood ratio test results signifies that the model fits the data.
#Test for heteroskedastacity
gqtest(negbin2b) # Goldfeld-Quandt test indicates no heteroskedasticity
bptest(negbin2b) # Breusch-Pagan test indicates heteroskedasticity

# Reoplacing SEs with Robust SEs since there is heteroskedasticity in the data
HWrobstder2b <- sqrt(diag(vcovHC(negbin2b, type="HC1"))) # produces Huber-White robust standard errors 
stargazer(negbin2b, negbin2b,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(NULL, HWrobstder2b),
          title="NB Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))
#The interaction coefficient for BIE_timeline:group_store is significant with value 0.8294. This means that the implementation of BOPS is associated with 17.06% decrease in the return quantity.

# Visualize the output
meffects2b <- ggpredict(negbin2b, terms=c("BIE", "group_store")) # generates a tidy data frame  
ggplot(meffects2b,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
  xlab("BOPS IN EFFECT") + ylab("Return Quantity") +
  labs(colour="group_store") + 
  scale_colour_discrete(labels=c("5998","2&6")) +
  scale_x_continuous(breaks=c(0,1), labels=c("BOPS not in effect", "BOPS in effect")) +
  theme(axis.title.x=element_blank()) 
```
#===============================================================================================#
## Question3: What is the impact of using the BOPS service on online customer purchase behavior?
#===============================================================================================#
```{r}
#Excluding rows where child is NA or homeowner is NA
customer_df <-consumer_level[which((consumer_level$child =="Y" | consumer_level$child =="N"),(consumer_level$homeowner_code=="R" | consumer_level$homeowner_code=="O")),]

# Summary statistics
stargazer(customer_df, type="text", median=TRUE, iqr=TRUE,digits=1, title="Descriptive Statistics")

#Raw data Plots to see the distribution of sales value, sales quantity and ln(salesvalue) and ln(salesquantity)
ggplot(customer_df, aes(x=salesvalue)) + geom_histogram(colour="green")

ggplot(customer_df, aes(x=log(salesvalue))) + geom_histogram(colour="green") 

ggplot(customer_df, aes(x=salesquantity)) + geom_histogram(colour="green") 

ggplot(customer_df, aes(x=log(salesquantity))) + geom_histogram(colour="green") #Still skewed so we will use salesquantity in our model


#Creating a new variable for ln salesvalue
customer_df$log_salesvalue <- log(customer_df$salesvalue+1)

#Creating dummy variable from factor variables ## this is done to ensure that these variables get included in descriptive stats and multi-collinearity test
customer_df$childnew = as.numeric(ifelse(customer_df$child=="Y",1,ifelse(customer_df$child=="N",0,customer_df$child)))
customer_df$homeowner_codenew = as.numeric(ifelse(customer_df$homeowner_code=="O",1,ifelse(customer_df$homeowner_code=="R",0,customer_df$homeowner_code)))

#Replacing missing values with median age_band
customer_df$age_band[is.na(customer_df$age_band)] = median(customer_df$age_band, na.rm = TRUE)

## Check for Multicollinearity 
df3 <- customer_df[c("homeowner_codenew","purchase_time_period","length_of_residence","est_income_code","age_band", "bops_in_effect", "bops_user", "childnew","female","store_number")]
cor(df3)# Generates the correlation matrix
vifcor(df3) # Calculates VIF scores
#Since all VIF scores are less than 3, no multicollinearity exists

#=======================================================================================#
## IMPACT ON SALES VALUE
#=======================================================================================#

#Model when Female is not included as a Control Variable
model3a_wf<- lm(log_salesvalue~bops_in_effect+bops_user+(bops_in_effect*bops_user)+ purchase_time_period + est_income_code + age_band + factor(store_number)+ childnew + homeowner_codenew, data=customer_df)
stargazer(model3a_wf,
          title="Regression Results", type="text", 
          column.labels=c("Model-3a: Without Female"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

# Check for heteroskedasticity
pred<-predict(model3a_wf) #obtain fitted values
residual=resid(model3a_wf) # obtain residuals

#Let's check the heteroscadasticity from the fitted values and residuals
df4 <- data.frame(pred,residual)
ggplot(df4, aes(y=residual, x=pred)) + geom_point(size=1.5)

gqtest(model3a_wf) # Goldfeld-Quandt test indicates no heteroskedasticity
bptest(model3a_wf) # Breusch-Pagan test indicates heteroskedasticity

# Since there is heteroskedasticity in the data, we will replace SEs with robust SEs.

HWrobstder3a <- sqrt(diag(vcovHC(model3a_wf, type="HC1"))) # produces Huber-White robust standard errors  

stargazer(model3a_wf, model3a_wf,  
          se=list(NULL, HWrobstder3a),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001)) 

#The interaction coefficient for bops_in_effect:bops_user is significant with value (-0.0444). This means that the usage of BOPS service is associated with 4.44% decrease in sales value. 
# Visualize the output
meffects3a <- ggpredict(model3a_wf, terms=c("bops_in_effect","bops_user")) 
ggplot(meffects3a,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
  xlab("bops_in_effect") + ylab("Sales Value") +
  labs(colour="bops_user") + 
  scale_colour_discrete(labels=c("Not used BOPS","Used BOPS")) +
  scale_x_continuous(breaks=c(0,1), labels=c("bops_not_in_effect", "bops_in_effect")) +
  theme(axis.title.x=element_blank())

#=======================================================================================#
## IMPACT ON SALES QUANTITY
#=======================================================================================#

poisson3b <- glm(salesquantity~bops_in_effect+bops_user+bops_in_effect*bops_user + purchase_time_period + est_income_code + age_band + factor(store_number) + homeowner_codenew +childnew, family="poisson", data=customer_df)
stargazer(poisson3b,  
          title="Poisson Results", type="text", 
          column.labels=c("Model-3b"),
          df=FALSE, digits=2, star.cutoffs = c(0.05,0.01,0.001)) 

## Model fit assessment 
poisson3bn <- glm(salesquantity~1,family="poisson", data=customer_df) 
##running a comparison with null model. 

lrtest(poisson3b, poisson3bn) 
# We conclude that the model does not fit because the goodness-of-fit chi-squared test is statistically significant. If the test had not been statistically significant, it would indicate that the data fit the model well.

negbin3b <- glm.nb(salesquantity~bops_in_effect+bops_user+bops_in_effect*bops_user + purchase_time_period + est_income_code + age_band + factor(store_number) + homeowner_codenew +childnew, data=customer_df) 
stargazer(negbin3b,
          apply.coef = exp, t.auto=F, p.auto = F,
          title="Negative Bionomial Results", type="text", 
          column.labels=c("IRRs"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

# Model fit assessment - Negative Binomial
negbin3bn <- glm.nb(salesquantity ~ 1, data = customer_df) 
lrtest(negbin3b, negbin3bn) ## Test results signifies that the model fits the data.

#Test for heteroskedastacity
gqtest(negbin3b) # Goldfeld-Quandt test indicates no heteroskedasticity
bptest(negbin3b) # Breusch-Pagan test indicates heteroskedasticity

# Reoplacing SEs with Robust SEs since there is heteroskedasticity in the data
HWrobstder3b <- sqrt(diag(vcovHC(negbin3b, type="HC1"))) # produces Huber-White robust standard errors 
stargazer(negbin3b, negbin3b,
          apply.coef = exp, t.auto=F, p.auto = F,
          se=list(NULL, HWrobstder2b),
          title="NB Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))
#The interaction coefficient for BIE_timeline:group_store is significant with value 0.9586. This means that the implementation of BOPS is associated with 4.14% decrease in the sales quantity.



# Visualize the output
meffects3b <- ggpredict(negbin3b, terms=c("bops_in_effect", "bops_user")) # generates a tidy data frame  
ggplot(meffects3b,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
  xlab("BOPS IN EFFECT") + ylab("Sales Quantity") +
  labs(colour="bops_user") + 
  scale_colour_discrete(labels=c("Not used BOPS","Used BOPS")) +
  scale_x_continuous(breaks=c(0,1), labels=c("BOPS not in effect", "BOPS in effect")) +
  theme(axis.title.x=element_blank()) 
```

#=======================================================================================#
## Question4: What is the impact of using the BOPS service on online customer return behavior?
#=======================================================================================#
```{r}

df5 <- data.frame(trans_level)

# Summary statistics
stargazer(df5, type="text", median=TRUE, iqr=TRUE,digits=1, title="Descriptive Statistics")

ggplot(df5, aes(x=price)) + geom_histogram(colour="green", bins = 30)
ggplot(df5, aes(x=log(price))) + geom_histogram(colour="green", bins = 30) #Histogram plot shows that log-transformed price has more normal distribution. Therefore, we will use log-transformed price, instead of the raw price.
df5$log_price <- log(df5$price+1)

#Removing all rows where BOPS is NA
df6<- df5[which(df5$bops == 0 | df5$bops == 1),]

# Summary statistics
stargazer(df6, type="text", median=TRUE, iqr=TRUE,digits=1, title="Descriptive Statistics")

# Converting child variable to a dummy variable.
df6$childnew = ifelse(df6$child== "Y",1,0) 
# Converting homeowner_code to a dummy variable.
df6$homeowner_codenew = ifelse(df6$homeowner_code== "O",1,0) 

# To deal with NA values of est_income_code and age band, we replace them with their median values
df6$est_income_code[is.na(df6$est_income_code)] <- median(df6$est_income_code, na.rm = TRUE)
df6$age_band[is.na(df6$age_band)] <- median(df6$age_band, na.rm = TRUE)

# To deal with NA values of length of residence we replace them with its mean values
df6$length_of_residence[is.na(df6$length_of_residence)] <- mean(df6$length_of_residence, na.rm = TRUE)

# Checking for multi-collinearity
df6a=df6[c("bops","month_dummy","female","product_category","log_price","year", "age_band", "est_income_code")]
cor(df6a) 
vifcor(df6a) # Since the VIF score < 3, we can safely assumen that there is No multi-collinearity

#Removing rows where Female is NA
df7<- df6[which(df6$female == 0 | df6$female == 1),]
#Removing remaining NA values
df7 <- na.omit(df7)

# Summary statistics
stargazer(df7, type="text", median=TRUE, iqr=TRUE,digits=1, title="Descriptive Statistics")
sum(df7$return==0) #887623 observations
sum(df7$return==1) #99476 observations
#We have 99476 observations with Return=1 and 887623 observations with Return=0. And hence we satisfy the minimum 10:1 ratio of valid cases to independent variables requirement to run a logit model

logit4<- glm(return~bops+log_price+factor(product_category)+factor(store_number)+factor(month_dummy)+year+est_income_code+female+age_band, data=df7, family="binomial") 
# Obtaining odd ratios , hence apply.coef=exp
stargazer(logit4, 
          apply.coef = exp, t.auto=F, p.auto = F,
          title="Regression Results", type="text", 
          column.labels=c("OddsRatios"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))
#Interpretation : for a one unit increase in Bops purchase, the odds of being returned (versus not being returned) increase by a factor of 1.2092. 

# Model fit assessment
logit4n <- glm(return~1, data=df7, family="binomial")
lrtest(logit4, logit4n) #Comparing the null model to our model for determining the model fit. The chi-square of 29953 with 36 degrees of freedom and an associated p-value of less than 0.001 tells us that our model as a whole fits significantly better than the null model.

# Obtaining marginal effects for better interpretation of results
meffects4 <- logitmfx(return~bops+log_price+factor(product_category)+factor(store_number)+factor(month_dummy)+year+est_income_code+female+age_band, data=df7, robust = TRUE) # We can generate the marginal effects with this command. The one unit increase in selling pressure increases the probability of return by 0.168, holding other variables at their means
marginaleffects4 <- meffects4$mfxest[,1]
rob.std.err42 <- meffects4$mfxest[,2]
stargazer(logit4, 
          omit=c("Constant"),
          coef = list(marginaleffects4), se = list(rob.std.err42),
          title="Regression Results", type="text", 
          column.labels=c("Marginal Effects"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))
#Interpretation: Usage of BOPS service increases the return probability by 15.9 percentage points, holding other variables at their means

pred4 = predict(logit4, data=df7,type = "response") # Generting predicted probabilities
return_prediction4 <- ifelse(pred4 >= 0.5,1,0) # If the predicted probability is greater than 0.5, then the predicted classification will be a return (return==1), otherwise it will be a no return (return==0)
misClasificError4 <- mean(return_prediction4 != df7$return) # counting number of wrong classifications
#misClasificError4 ( mean gives ratio of observations in the sample that is misclassified )
print(paste('Accuracy:',1-misClasificError4)) # calculating the correct classification rate. Accuracy is 0.8992, meaning the model correctly determines the membership (being 0 vs 1) for 89.92% of all observations
table(df7$return, pred4>=0.5) # Generating the confusion matrix

#=========================================TREATING ENDOGENEITY==============================================#
# Since using BOPS services is a customer's decision , we suspect that key independent variable is endogenous as there may be other factors involved in that decision like distance of the customer's residence from the store. 
# Child,length of residence and home_owner are the potential instrument variables. 
model4b<- ivreg(return~bops+log_price+product_category+factor(store_number)+factor(month_dummy)+year+est_income_code+female+age_band|length_of_residence+childnew+homeowner_codenew+log_price+product_category+factor(store_number)+factor(month_dummy)+year+est_income_code+female+age_band, data=df7)
summary(model4b,diagnostics = TRUE) 
stargazer(model4b,  
          title="Regression Results", type="text", 
          column.labels=c("2-SLS with 3 IVs"),
          df=FALSE, digits=3, star.cutoffs = c(0.05,0.01,0.001))


df7a=df7[c("bops","month_dummy","female","product_category","log_price","year", "age_band", "est_income_code","length_of_residence","childnew","homeowner_codenew")]
cor(df7a)

#Running the model with all 3 potential IVs, we see Sargan statistic is significant, meaning some instruments are weak instruments. 
model4c<- ivreg(return~bops+log_price+product_category+factor(store_number)+factor(month_dummy)+year+est_income_code+female+age_band|length_of_residence+homeowner_codenew+childnew+log_price+product_category+factor(store_number)+factor(month_dummy)+year+est_income_code+female+age_band, data=df7)
summary(model4c,diagnostics = TRUE)
#Running the model without homeowner_codenew
model4d<- ivreg(return~bops+log_price+product_category+factor(store_number)+factor(month_dummy)+year+est_income_code+female+age_band|length_of_residence+childnew+log_price+product_category+factor(store_number)+factor(month_dummy)+year+est_income_code+female+age_band, data=df7)
summary(model4d,diagnostics = TRUE)
# The significant test statistics indicates that endogeneity exists. Also, relevance and exogeneity assumptions have passed. Sargan statistic is not significant, meaning the instruments are exogenous. 
#Weak instrument statistic indicates the instruments are relevant. Wu-hausman test statistics is also significant which indicates that endogeneity exists.
stargazer(model4d,  
          title="Regression Results", type="text", 
          column.labels=c("2-SLS"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))
#After treating endogeneity in our model, the coefficient for BOPS in 2SLS model is 0.3683. This means that a BOPS user is 36.83% more likely to return a jewellery item.
```
#=====================================================================================================#
## Question5: What is the impact of implementing BOPS strategy on product-level sales and returns?
#=====================================================================================================#
```{r}
df8<- data.frame(ODS_prod_cat)

# Summary statistics
stargazer(df8, type="text", median=TRUE, iqr=TRUE,digits=1, title="Descriptive Statistics")

#Raw data Plots

#Plotting salesvalue with bops_in_effect to see a correlation
ggplot(df8, aes(y=salesvalue, x=bops_in_effect)) + geom_point(size = 2.5, colour = "black") + xlab("Bops in Effect") + ylab("Sales($)")

#We also plot the distribution of salesvalue
ggplot(df8, aes(x=salesvalue)) + geom_histogram(colour="green")

#Using log(salesvalue) the histogram is more normal. So we will use log(salesvalue) for our model
ggplot(df8, aes(x=log(salesvalue))) + geom_histogram(colour="green") 

#We also plot the distribution of salesquantity
ggplot(df8, aes(x=salesquantity)) + geom_histogram(colour="green") 

#As the distrubtion of log(salesquantity) doesn't change, we use salesquantity in our model
ggplot(df8, aes(x=log(salesquantity))) + geom_histogram(colour="green") 

#We also plot the distribution of returnvalue
ggplot(df8, aes(x=returnvalue)) + geom_histogram(colour="green")

#Using log(returnvalue) the histogram is more normal. So we will use log(returnvalue) for our model
ggplot(df8, aes(x=log(returnvalue))) + geom_histogram(colour="green") 

#We also plot the distribution of returnquantity
ggplot(df8, aes(x=returnquantity)) + geom_histogram(colour="green") 

#As the distrubtion of log(returnquantity) doesn't change, we use returnquantity in our model
ggplot(df8, aes(x=log(returnquantity))) + geom_histogram(colour="green")

#Creating new variables for ln values
df8$log_returnvalue <- log(df8$returnvalue+1)
df8$log_salesvalue <- log(df8$salesvalue+1)



#Replacing Null Values with Mean
df8$avg_age[is.na(df8$avg_age)] <- mean(df8$avg_age, na.rm = TRUE)

df8$avg_female[is.na(df8$avg_female)] <- mean(df8$avg_female, na.rm = TRUE)

df8$avg_income[is.na(df8$avg_income)] <- mean(df8$avg_income, na.rm = TRUE)

df8$avg_homeowner[is.na(df8$avg_homeowner)] <- mean(df8$avg_homeowner, na.rm = TRUE)

df8$avg_residency[is.na(df8$avg_residency)] <- mean(df8$avg_residency, na.rm = TRUE)

df8$avg_childowner[is.na(df8$avg_childowner)] <- mean(df8$avg_childowner, na.rm = TRUE)

#Filtering the data before 27th September,2012 when bops was only implemented on stores 2 & 6
df9 <- df8[(df8$day<789),]

#Divide the data into two groups one where BOPS was implemented, other when BOPS was not implemented.Before 1st Aug 2011, BOPS wasn't implemented, After 1st Aug 2011 BOPS was implemented.
df9$BIE <- ifelse(df9$day<366,0,1)
df9$group_store <- ifelse((df9$store_number == 2) | (df9$store_number == 6),1,0)

# Summary statistics
stargazer(df9, type="text", median=TRUE, iqr=TRUE,digits=1, title="Descriptive Statistics")

#Check for Multicollinearity
df_mcol1 = df9[c("BIE", "group_store","month_dummy","product_category","avg_female","avg_age","avg_income","avg_homeowner","avg_residency","avg_childowner")]
cor(df_mcol1)
vifcor(df_mcol1)
#No multicollinearity exists
#=======================================================================================#
## IMPACT ON SALES VALUE AND RETURN VALUE
#=======================================================================================#
#Impact on Sales Value
#Final Model for measuring the impact of BOPS on salesvalue on product level
model5a<- lm(log_salesvalue~BIE+group_store+(BIE*group_store)+factor(product_category)+factor(month_dummy)+avg_female+avg_age+avg_income+avg_homeowner+avg_residency+avg_childowner, data=df9)
stargazer(model5a,
          title="Regression Results", type="text", 
          column.labels=c("Model-5a"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))
#Test for heteroskedastacity
gqtest(model5a) # Goldfeld-Quandt test indicates no heteroskedasticity
bptest(model5a) # Breusch-Pagan test indicates heteroskedasticity

# Reoplacing SEs with Robust SEs since there is heteroskedasticity in the data
HWrobstder5a <- sqrt(diag(vcovHC(model5a, type="HC1"))) # produces Huber-White robust standard errors 
clusrobstder5a <- sqrt(diag(cluster.vcov(model5a, df9$product_category ))) # produces clustered robust standard errors using product_category as clustering variable
stargazer(model5a, model5a,model5a,
          se=list(NULL, HWrobstder1a, clusrobstder5a),
          title="OLS Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE","Clustered-SE"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))
#The interaction coefficient for BIE:group_store is significant with value -0.2874. This means that the implementation of BOPS is associated with 28.74% decrease in the sales value.

# Visualize the output using marginal effects
meffects5a <- ggpredict(model5a, terms=c("BIE", "group_store"))   
ggplot(meffects5a,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
  xlab("BOPS IN EFFECT") + ylab("Sales Value") +
  labs(colour="group_store") + 
  scale_colour_discrete(labels=c("5998","2&6")) +
  scale_x_continuous(breaks=c(0,1), labels=c("BOPS not in effect", "BOPS in effect")) +
  theme(axis.title.x=element_blank())

##INTERPRETATION 

#Impact on Return Value
#Final Model for measuring the impact of BOPS on returnvalue on product level
model5b <- lm(log_returnvalue~BIE+group_store+(BIE*group_store)+factor(product_category)+factor(month_dummy)+avg_female+avg_age+avg_income+avg_homeowner+avg_residency+avg_childowner+log_salesvalue, data=df9)
stargazer(model5b,
          title="Regression Results", type="text", 
          column.labels=c("Model-5b"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))
#Test for heteroskedastacity
gqtest(model5b) # Goldfeld-Quandt test indicates no heteroskedasticity
bptest(model5b) # Breusch-Pagan test indicates heteroskedasticity

# Reoplacing SEs with Robust SEs since there is heteroskedasticity in the data
HWrobstder5b <- sqrt(diag(vcovHC(model5b, type="HC1"))) # produces Huber-White robust standard errors 
clusrobstder5b <- sqrt(diag(cluster.vcov(model5b, df9$product_category ))) # produces clustered robust standard errors using product_category as clustering variable
stargazer(model5b, model5b,model5b,
          se=list(NULL, HWrobstder1b, clusrobstder5b),
          title="OLS Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE","Clustered-SE"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))
#The interaction coefficient for BIE:group_store is significant with value -0.3302. This means that the implementation of BOPS is associated with 33.02% decrease in the return value.

# Visualize the output using marginal effects
meffects5b <- ggpredict(model5b, terms=c("BIE", "group_store"))   
ggplot(meffects5b,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
  xlab("BOPS IN EFFECT") + ylab("Return Value") +
  labs(colour="group_store") + 
  scale_colour_discrete(labels=c("5998","2&6")) +
  scale_x_continuous(breaks=c(0,1), labels=c("BOPS not in effect", "BOPS in effect")) +
  theme(axis.title.x=element_blank())


#=======================================================================================#
## IMPACT ON SALES QUANTITY AND RETURN QUANTITY
#=======================================================================================#

#Impact on Sales Quantity
#Poisson model since Salesquantity is a count variable
poisson5a <- glm(salesquantity~BIE+group_store+(BIE*group_store)+factor(product_category)+factor(month_dummy)+avg_female+avg_age+avg_income+avg_homeowner+avg_residency+avg_childowner, family="poisson", data=df9)
stargazer(poisson5a,  
          title="Poisson Results", type="text", 
          column.labels=c("Count Model-5a"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

#Model fit test using a null model.
poisson5an <- glm(salesquantity~1, data=df9, family="poisson") 
#Likelihood Ratio Test
lrtest(poisson5a, poisson5an) #Significant p value which means data doesn't fit the model.

#Since Poisson model does not fit the data, we run the negative binomial model on the data
negbin5a <- glm.nb(salesquantity~BIE+group_store+(BIE*group_store)+factor(product_category)+factor(month_dummy)+avg_female+avg_age+avg_income+avg_homeowner+avg_residency+avg_childowner, data = df9) 
stargazer(negbin5a,
          apply.coef = exp, t.auto=F, p.auto = F,
          title="Negative Bionomial Results", type="text", 
          column.labels=c("IRRs"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))
#Testing model fit by comparing with null model
negbin5an <- glm.nb(salesquantity ~ 1, data = df9) 
#Likelihood Ratio test
lrtest(negbin5a, negbin5an )#Significant p value which means data fits the model.
#Test for heteroskedastacity
gqtest(negbin5a) # Goldfeld-Quandt test indicates no heteroskedasticity
bptest(negbin5a) # Breusch-Pagan test indicates heteroskedasticity

# Reoplacing SEs with Robust SEs since there is heteroskedasticity in the data
HWrobstder5aq <- sqrt(diag(vcovHC(negbin5a, type="HC1"))) # produces Huber-White robust standard errors 
clusrobstder5aq <- sqrt(diag(cluster.vcov(negbin5a, df9$product_category ))) # produces clustered robust standard errors using product_category as clustering variable
stargazer(negbin5a, negbin5a,negbin5a,
          se=list(NULL, HWrobstder5aq, clusrobstder5aq),
          apply.coef = exp, t.auto=F, p.auto = F,
          title="OLS Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE","Clustered-SE"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))
#The interaction coefficient of BIE:group_store is significant with IRR value 0.6850. This means that BOPS implementation is associated with 31.50% decrease in product-level sales quantity.

# Visualize the output
meffects5aq <- ggpredict(negbin5a, terms=c("BIE", "group_store")) # generates a tidy data frame  
ggplot(meffects5aq,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
  xlab("BOPS IN EFFECT") + ylab("Sales Quantity") +
  labs(colour="group_store") + 
  scale_colour_discrete(labels=c("5998","2&6")) +
  scale_x_continuous(breaks=c(0,1), labels=c("BOPS not in effect", "BOPS in effect")) +
  theme(axis.title.x=element_blank()) 


#Impact on Return Quantity
#Poisson model since Returnquantity is a count variable
poisson5b <- glm(returnquantity~BIE+group_store+(BIE*group_store)+factor(product_category)+salesquantity+factor(month_dummy)+year+day+avg_female+avg_age+avg_income+avg_homeowner+avg_residency+avg_childowner, family="poisson", data=df9)
stargazer(poisson5b,  
          title="Poisson Results", type="text", 
          column.labels=c("Count Model-5b"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

#Testing model fit by comparing with null model
poisson5bn <- glm(returnquantity~1, data=df9, family="poisson") 
lrtest(poisson5b, poisson5bn) #Significant p value which indicates data doesn't fit the model

#Since Poisson model does not fit the data, we run the negative binomial model on the data
negbin5b <- glm.nb(returnquantity~BIE+group_store+(BIE*group_store)+factor(product_category)+salesquantity+factor(month_dummy)+year+day+avg_female+avg_age+avg_income+avg_homeowner+avg_residency+avg_childowner, data = df9) 
stargazer(negbin5b,
          apply.coef = exp, t.auto=F, p.auto = F,
          title="Negative Bionomial Results", type="text", 
          column.labels=c("IRRs"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))
#Testing model fit by comparing with null model
negbin5bn <- glm.nb(returnquantity ~ 1, data = df9) 
lrtest(negbin5b, negbin5bn )#Significant p value which indicates data fits the model
#Test for heteroskedastacity
gqtest(negbin5b) # Goldfeld-Quandt test indicates no heteroskedasticity
bptest(negbin5b) # Breusch-Pagan test indicates heteroskedasticity

# Reoplacing SEs with Robust SEs since there is heteroskedasticity in the data
HWrobstder5bq <- sqrt(diag(vcovHC(negbin5b, type="HC1"))) # produces Huber-White robust standard errors 
clusrobstder5bq <- sqrt(diag(cluster.vcov(negbin5b, df9$product_category ))) # produces clustered robust standard errors using product_category as clustering variable
stargazer(negbin5b, negbin5b,negbin5b,
          se=list(NULL, HWrobstder5bq, clusrobstder5bq),
          apply.coef = exp, t.auto=F, p.auto = F,
          title="OLS Results", type="text", 
          column.labels=c("Normal SE", "HW-Robust SE","Clustered-SE"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))
# The interaction coefficient of BIE:group_store is significant with IRR value 0.6188. This means that BOPS implementation is associated with 38.12% decrease in product-level return quantity.
# Visualize the output
meffects5bq <- ggpredict(negbin5b, terms=c("BIE", "group_store")) # generates a tidy data frame  
ggplot(meffects5bq,aes(x, predicted, colour=group)) + geom_line(size=1.3) + 
  xlab("BOPS IN EFFECT") + ylab("Return Quantity") +
  labs(colour="group_store") + 
  scale_colour_discrete(labels=c("5998","2&6")) +
  scale_x_continuous(breaks=c(0,1), labels=c("BOPS not in effect", "BOPS in effect")) +
  theme(axis.title.x=element_blank()) 

#=================================================================================================##
## Question6: How does the impact of implementing BOPS strategy vary across product categories?
#=================================================================================================##
```{r}
df10 <- data.frame(ODS_prod_cat)
# Summary statistics
stargazer(df10, type="text", median=TRUE, iqr=TRUE,digits=1, title="Descriptive Statistics")

#Replacing Null Values with Mean
df10$avg_age[is.na(df10$avg_age)] <- mean(df10$avg_age, na.rm = TRUE)

df10$avg_female[is.na(df10$avg_female)] <- mean(df10$avg_female, na.rm = TRUE)

df10$avg_income[is.na(df10$avg_income)] <- mean(df10$avg_income, na.rm = TRUE)

df10$avg_homeowner[is.na(df10$avg_homeowner)] <- mean(df10$avg_homeowner, na.rm = TRUE)

df10$avg_residency[is.na(df10$avg_residency)] <- mean(df10$avg_residency, na.rm = TRUE)

df10$avg_childowner[is.na(df10$avg_childowner)] <- mean(df10$avg_childowner, na.rm = TRUE)

#Filtering the data before 27th September,2012 when bops was only implemented on stores 2 & 6
df11 <- df10[(df10$day<786),]

#Divide the data into two groups one where BOPS was implemented, other when BOPS was not implemented.Before 1st Aug 2011, BOPS wasn't implemented, After 1st Aug 2011 BOPS was implemented.
df11$BIE <- ifelse(df11$day<366,0,1)
df11$group_store <- ifelse((df11$store_number == 2) | (df11$store_number == 6),1,0)

# Summary statistics
stargazer(df11, type="text", median=TRUE, iqr=TRUE,digits=1, title="Descriptive Statistics")
df11$product_category <-as.numeric(df11$product_category)
#Check for Multi-Collinearity
df11M=df11[c("BIE","group_store","month_dummy","year","product_category","salesvalue", "avg_female","avg_age","avg_income","avg_homeowner","avg_residency","avg_childowner")]
cor(df11M) 
vifcor(df11M)

#Since VIF score of BIE and year is greater than 3, multi-collinearity exists in the data, to remove it, we again checked the correlation matrix and found that year and BIE have a very high correlation of about 0.728. So to remove the multi-collinearity from our theoritical model, we would not include year in our model
df11M1=df11[c("BIE","group_store","month_dummy","product_category","salesvalue", "avg_female","avg_age","avg_income","avg_homeowner","avg_residency","avg_childowner")]
cor(df11M1) 
vifcor(df11M1)
#No multi-collinearity exists now after removing year
#=======================================================================================#
## IMPACT ON SALES VALUE
#=======================================================================================#

#We also plot the distribution of salesvalue to check the distribution of salesvalue
ggplot(df11, aes(x=salesvalue)) + geom_histogram(colour="green")

#Using log(salesvalue) the histogram is more normal. So we will use log(salesvalue) for our model
ggplot(df11, aes(x=log(salesvalue))) + geom_histogram(colour="green") 

#Creating new variable log_salesvalue
df11$log_salesvalue <- log(df11$salesvalue+1)

#Converting product_category into a factor variable
df11$product_category<-as.factor(df11$product_category)

#Initial Model for measuring the impact of BOPS on salesvalue on product category level. Since we have to see the pure impact of BOPS on all categories, we have to interact BIE with group_store and product_category.(Triple Interaction).
model6a<- lm(log_salesvalue~BIE+group_store+(BIE*group_store*product_category)+product_category+factor(month_dummy)+avg_female+avg_age+avg_income+avg_homeowner+avg_residency+avg_childowner, data=df11)
stargazer(model6a,
          title="Regression Results", type="text", 
          column.labels=c("Model-6a"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

# Check for heteroskedasticity
gqtest(model6a) # Goldfeld-Quandt test indicates heteroskedasticity
bptest(model6a) # Breusch-Pagan test indicates heteroskedasticity

# Since there is heteroskedasticity in the data, we will replace SEs with clustered robust SEs since the data is clustered on product categories.
#Final Model
clusrobstder6a <- sqrt(diag(cluster.vcov(model6a, df11$store_number ))) # produces clustered robust standard errors using store_number as clustering variable
stargazer(model6a, model6a,
          se=list(NULL,clusrobstder6a),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE","Clustered SE"),
          df=FALSE, digits=3, star.cutoffs = c(0.05,0.01,0.001))

##===================================Marginal Effects===============================================##

#Generating marginal effects 
meffects6a <- ggpredict(model6a, terms=c("BIE", "group_store","product_category")) 

#Renaming the column names and assigning category names to the factor variable product_category to gain clarity in the marginal effects plot
meffects6a_columns <- meffects6a[c("x","predicted","group", "facet")]
colnames(meffects6a_columns) <- c("BIE", "predicted_salesvalue","group_store" , "product_category" )
levels(meffects6a_columns$product_category) <- c('1-Bridal', '2-Gold Wed Bands', '3-Solitaires',   '4-Diamond Fashion', '5-Semi Precious', '6-Mens', '7-Gold Earrings', '8-In House Special Event', '9-Beads','10-Piercings / Close Out', '11-Diamond Solitaires Jewelry', '12-Gold Chain / Jewelry ', '13-Watches', '14-Pre-Owned', '15-Specialized Jewelry', '17-Events', '19-Repair / Warranty','20-Diamond Wedding Bands','21-Sterling Silver')

#Plotting marginal effects across all product categories
ggplot(meffects6a_columns, aes(BIE, y= predicted_salesvalue, colour =group_store)) + geom_line() +
  facet_wrap(product_category ~ ., scales="free")+ 
  scale_colour_discrete(labels=c("5998","2&6"))+ 
  xlab("BOPS IN EFFECT") + ylab("Predicted Sales Value")

#Since we cannot interpret this plot into asoociated values, we subset the data for each product category to find the impact of BOPS on salesvalue of each categegory. 
#This is a sample model, for product_category 1. We ran the same model for all categories. The coefficients of each category are listed below.

#Subsetting the data for product_category = 1
df11_1<- subset(df11, product_category==21)
table(df11$product_category)
table(df11_1$store_number)
#Model
model6a_1= lm(log_salesvalue~BIE+group_store+(BIE*group_store)+factor(month_dummy)+avg_female+avg_age+avg_income+avg_homeowner+avg_residency+avg_childowner, data=df11_1)
stargazer(model6a_1,
          title="Regression Results", type="text", 
          column.labels=c("Prod_Cat_1"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

# Check for heteroskedasticity
gqtest(model6a_1) # Goldfeld-Quandt test indicates heteroskedasticity
bptest(model6a_1) # Breusch-Pagan test indicates heteroskedasticity

# Since there is heteroskedasticity in the data, we will replace SEs with clustered robust SEs.
HWrobstder6a_1 <- sqrt(diag(vcovHC(model6a_1, type="HC1"))) 
clusrobstder6a_1 <- sqrt(diag(cluster.vcov(model6a_1, df11_1$store_number))) # produces clustered robust standard errors using store_number as clustering variable
stargazer(model6a_1, model6a_1, model6a_1,
          se=list(NULL,HWrobstder6a_1,clusrobstder6a_1),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE","Robust SE","Clustered SE"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

# Interaction coefficient is significant for product categories: 1,2,4,5,9,11,12,13,14,21
#  Therefore, BOPS implementation for these categories is aasociated with: 
#   Product Category 1: 31.46% decrease in sales value
#   Product Category 2: 38.74% decrease in sales value
#   Product Category 3: 6.5% decrease in sales value
#   Product Category 4: 61.2% decrease in sales value
#   Product Category 5: 26.40% decrease in sales value
#   Product Category 9: 35.75% increase in sales value
#   Product Category 11: 17.81% decrease in sales value
#   Product Category 12: 31.70% decrease in sales value
#   Product Category 13: 11.96% increase in sales value
#   Product Category 14: 114.33% decrease in sales value
#   Product Category 21: 38.79% decrease in sales value

# Interaction coefficient is not significant for product categories: 6,7,20
# No coefficients available for categories: 8,10,15,16,17,18,19 because we either do not have product categories in the subset of the data or we have only one group of store for the product category. So these categories cannot be analysed.

#=======================================================================================#
## IMPACT ON RETURN VALUE
#=======================================================================================#

#We also plot the distribution of returnvalue to check the distribution of returnvalue
ggplot(df11, aes(x=returnvalue)) + geom_histogram(colour="green")

#Using log(returnvalue) the histogram is more normal. So we will use log(returnvalue) for our model
ggplot(df11, aes(x=log(returnvalue))) + geom_histogram(colour="green") 

#Creating new variable log_returnvalue
df11$log_returnvalue <- log(df11$returnvalue+1)

#Initial Model for measuring the impact of BOPS on salesvalue on product category level. Since we have to see the pure impact of BOPS on all categories, we have to interact BIE with group_store and product_category.(Triple Interaction).
model6b<- lm(log_returnvalue~BIE+group_store+(BIE*group_store*product_category)+product_category+log_salesvalue+factor(month_dummy)+avg_female+avg_age+avg_income+avg_homeowner+avg_residency+avg_childowner, data=df11)
stargazer(model6b,
          title="Regression Results", type="text", 
          column.labels=c("Model-6b"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

# Check for heteroskedasticity
gqtest(model6b) # Goldfeld-Quandt test indicates heteroskedasticity
bptest(model6b) # Breusch-Pagan test indicates heteroskedasticity

# Since there is heteroskedasticity in the data, we will replace SEs with clustered robust SEs since the data is clustered on product categories.
#Final Model
clusrobstder6b <- sqrt(diag(cluster.vcov(model6b, df11$store_number ))) # produces clustered robust standard errors using store_number as clustering variable
stargazer(model6b, model6b,
          se=list(NULL,clusrobstder6b),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE","Clustered SE"),
          df=FALSE, digits=3, star.cutoffs = c(0.05,0.01,0.001))

##===================================Marginal Effects===============================================##

#Generating marginal effects 
meffects6b <- ggpredict(model6b, terms=c("BIE", "group_store","product_category")) 

#Renaming the column names and assigning category names to the factor variable product_category to gain clarity in the marginal effects plot
meffects6b_columns <- meffects6b[c("x","predicted","group", "facet")]
colnames(meffects6b_columns) <- c("BIE", "predicted_returnvalue","group_store" , "product_category" )
levels(meffects6b_columns$product_category) <- c('1-Bridal', '2-Gold Wed Bands', '3-Solitaires',   '4-Diamond Fashion', '5-Semi Precious', '6-Mens', '7-Gold Earrings', '8-In House Special Event', '9-Beads','10-Piercings / Close Out', '11-Diamond Solitaires Jewelry', '12-Gold Chain / Jewelry ', '13-Watches', '14-Pre-Owned', '15-Specialized Jewelry', '17-Events', '19-Repair / Warranty','20-Diamond Wedding Bands','21-Sterling Silver')

#Plotting marginal effects across all product categories
ggplot(meffects6b_columns, aes(BIE, y= predicted_returnvalue, colour =group_store)) + geom_line() +
  facet_wrap(product_category ~ ., scales="free")+ 
  scale_colour_discrete(labels=c("5998","2&6"))+ 
  xlab("BOPS IN EFFECT") + ylab("Predicted Return Value")

#Since we cannot interpret this plot into asoociated values, we subset the data for each product category to find the impact of BOPS on salesvalue of each categegory. 
#This is a sample model, for product_category 1. We ran the same model for all categories. The coefficients of each category are listed below.

#Subsetting the data for product_category = 1
df11_2<- subset(df11, product_category==21)
table(df11$product_category)
table(df11_2$store_number)
#Model
model6b_1= lm(log_returnvalue~BIE+group_store+(BIE*group_store)+factor(month_dummy)+log_salesvalue+avg_female+avg_age+avg_income+avg_homeowner+avg_residency+avg_childowner, data=df11_2)
stargazer(model6b_1,
          title="Regression Results", type="text", 
          column.labels=c("Prod_Cat_1"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))

# Check for heteroskedasticity
gqtest(model6b_1) # Goldfeld-Quandt test indicates heteroskedasticity
bptest(model6b_1) # Breusch-Pagan test indicates heteroskedasticity

# Since there is heteroskedasticity in the data, we will replace SEs with clustered robust SEs.
HWrobstder6b_1 <- sqrt(diag(vcovHC(model6b_1, type="HC1"))) 
clusrobstder6b_1 <- sqrt(diag(cluster.vcov(model6b_1, df11_2$store_number))) # produces clustered robust standard errors using store_number as clustering variable
stargazer(model6b_1, model6b_1, model6b_1,
          se=list(NULL,HWrobstder6b_1,clusrobstder6b_1),
          title="Regression Results", type="text", 
          column.labels=c("Normal SE","Robust SE","Clustered SE"),
          df=FALSE, digits=4, star.cutoffs = c(0.05,0.01,0.001))


# Interaction coefficient is significant for categories - 1,4,11,12,13,20,21.
# Therefore, BOPS implementation for these categories is aasociated with: 
# Product Category 1: 41.87% decrease in return value
# Product Category 4: 94.23% decrease in return value
# Product Category 11: 38.76% increase in return value
# Product Category 12: 32.44% decrease in return value
# Product Category 13: 80.53% decrease in return value
# Product Category 20: 91.10% decrease in return value
# Product Category 21: 51.89% decrease in return value

# Interaction coefficients are insignificant for following categories- 2,3,5,6,7,9,14
# No coefficients available for categories: 8,10,15,16,17,18,19 because we either do not have product categories in the subset of the data or we have only one group of store for the product category. So these categories cannot be analysed.
#=======================================================================================#
## END OF CODE
#=======================================================================================#
```







