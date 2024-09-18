

################# ------- R-Script for the econometrics file  ------------###############

     #------ Our theme is the following : Digital finance and Financial inclusion in developing countries  -----#

# To do this, we use data from the World Development Indicator (WDI) and the World Bank's Global Findex; 

# So let's talk about our data collection method
    #First, which countries are referred to as developing countries ? 
       # we have taken the list provided on the website of the United Nations (152 countries in total);
    # Second, due to the lack of data for all developing countries for the most recent periods, 
       # our sample of countries has been reduced to less than 150 countries for the 2018 data;
       # Then, in order to keep only countries with data on all our variables, we have again removed some observations;
       # So finally, our sample consists of 92 countries;

# Now let's look at the variables we retained from the literature review on our topic and briefly describe these variables


# country name: all countries selected for this work

# deposit account: share of the population with a deposit account in the country

# loans: share of population with loans within the country

# savings: share of population with savings in the country

# digital_fin: digital finance captured through the share of the population using the internet or mobile in the country

# acces_elec: share of population with access to electricity in the country

# employ_agri: share of agricultural employment in the country

# self_employ: share of self-employment in the country

# foreign_DI: share of foreign direct investment in gross domestic product

# gdp: gross domestic product at constant rate 2015

# CPI: the consumer price index used to capture the level of inflation within the country

# urban_pop: the number of people living in urban areas

# rural_pop: the number of people living in rural areas

# total_pop: the number of the total population of the country

# pop_density: Density of the population within the country

# HIC: Human capital index (between 0 and 1): which allow us to capture the level of education and access to care in the country

# FI_Index: Financial inclusion Index (between 0 and 1): We constructed this index ourselves following the method of Oumarou and Celestin (2021)



#-----------So let's begin our estimation work

# Let's clear our working environment 
rm(list=ls())

# let's define a working directory
setwd("C:/Users/AN/Documents/Master_SE/Cours/Econométrie appliqué 1/R/Présentation")

# Let's install and load all the libraries we will need later



library(readxl)
library(haven)
library(dplyr)
library(sandwich)
library(lmtest)
library(corrplot)
library(tibble)
library (ggplot2)
library(AER)
library(tidyselect)
library(MASS)


# Let's import our excel data in R

Base <- read_excel("Master_SE/Cours/Econométrie appliqué 1/R/Présentation/Données/Base.xlsx")
View(Base)

# Our variables are in character format. We will therefore proceed to transform them
  # into numeric and put our table in data.frame format. So let's do it.


Base <- as_tibble(Base)

base1 <- as.character(Base$'Country Name')
base2 <-as.numeric(Base$deposit_account)
base3 <-as.numeric(Base$loans)
base4 <-as.numeric(Base$savings)
base5 <-as.numeric(Base$digital_fin)
base6 <-as.numeric(Base$acces_elec)
base7 <-as.numeric(Base$employ_agri)
base8 <-as.numeric(Base$self_employ)
base9 <-as.numeric(Base$foreign_DI)
base10 <-as.numeric(Base$gdp)
base11 <-as.numeric(Base$ipc)
base12 <-as.numeric(Base$urban_pop)
base13 <-as.numeric(Base$rural_pop)
base14 <-as.numeric(Base$total_pop)
base15 <-as.numeric(Base$pop_density)
base16 <-as.numeric(Base$hci)
base17 <-as.numeric(Base$FI)


base1 <- as_tibble(base1)
base2 <- as_tibble(base2)
base3 <- as_tibble(base3)
base4 <- as_tibble(base4)
base5 <- as_tibble(base5)
base6 <- as_tibble(base6)
base7 <- as_tibble(base7)
base8 <- as_tibble(base8)
base9 <- as_tibble(base9)
base10 <- as_tibble(base10)
base11 <- as_tibble(base11)
base12 <- as_tibble(base12)
base13 <- as_tibble(base13)
base14 <- as_tibble(base14)
base15 <- as_tibble(base15)
base16 <- as_tibble(base16)
base17 <- as_tibble(base17)
 
      
      # Let's transform the foreign_DI and HCI variables into categorical variables
base9 <- base9 %>% mutate(value= ifelse(value<=10, 0, ifelse(value>10 | value<=20, 
                                                                          1, 2)))

base16 <- base16 %>% mutate(value= ifelse(value<0.5, 0, 1))


    #Now gather the individual tables and name this base_esti

base_esti <- cbind(base1, base2, base3, base4, base5, base6, base7, base8, base9, base10, base11,
                   base12, base13, base14, base15, base16, base17)

sum(is.na(base_esti)) # No missing values


   # Let's delete all the individual tables to keep only
     # our original base and the new estimation base

rm(base1, base2, base3, base4, base5, base6, base7, base8, base9, base10, base11, base12, base13, base14, 
   base15, base16, base17)

    # let's label each variable in base_esti

colnames(base_esti) <- c( "country_name", "deposit_account", 
                          "loans", "savings", "digital_fin", "acces_elec", 
                          "employ_agri", "self_employ", "foreign_DI", "gdp", 
                          "CPI", "urban_pop", "rural_pop", "total_pop", 
                          "pop_density", "HCI", "FI_Index")


# To interpret in terms of elasticity, we take variables we take some variables in logarithmic form
       # So let's create log form of variables in base_esti

base_esti <- base_esti %>% 
  mutate(log_dep_account= log(deposit_account), log_loans= log(loans), log_savings= log(savings),
         log_digital_fin= log(digital_fin), log_gdp = log(gdp),log_CPI= log(CPI), 
         log_urban_pop= log(urban_pop), log_pop_density= log(pop_density))


# Let's get some descriptive statistics for our estimation variables

base_esti <- base_esti %>% filter(log_digital_fin >0, log_CPI >0)

desc_var <- base_esti %>% select(log_dep_account, log_loans, log_savings, FI_Index, log_digital_fin,
                                 acces_elec, employ_agri, foreign_DI, log_gdp,
                                 log_CPI, log_urban_pop, HCI)
                                 


stargazer(as.data.frame(desc_var), 
          type = "text",
          title = "Table: Descriptive statistics of the main variables",
          covariate.labels = c("Deposit account(log)", "Loans (log)","Savings (log)", " Financial inclusion index (0-1)",
                               "Digital finance (log)", "Access to electricity (percent)", "Farm employment (percent)", 
                               "Foreign direct investment (factor)", "Gross domestic product (log)",
                               "Consumer price index (log)", "Urban population (log)", "Human capital index (factor)"))




# Let's get the pairwise correlation matrix out 



tab_corr(xxx, na.deletion= c("pairwise"),
         triangle = "lower",
         file="correlation_matrix.rtf")


corr.matrix1 <- cor(desc_var, y= NULL, use = "pairwise.complete.obs")
corrplot(corr.matrix1, method = 'ellipse',  diag = F, addCoef.col ='black', number.cex = 0.7, type= "lower", 
         sig.level = c(0.01, 0.05, 0.10), pch.cex= 0.9, insig= 'label_sig', pch.col ='grey20', tl.col="black",
         tl.cex=0.8, pch=1:4, number.digits = 3)


#We have four models in all. Indeed, in order to study all the dimension of financial inclusion
  # we first take the variables "log_dep_account", "log_loans", "log_savings", and in the fourth model, 
  # we use the financial inclusion index.


# First, let's estimate our models through the ols method

    ## First model (ols and some tests on the normality and on the quality of fit in general)


ols1 <- lm (log_dep_account ~ log_digital_fin + acces_elec + employ_agri + foreign_DI
           + log_gdp + log_CPI + HCI + log_urban_pop, data=base_esti)
summary(ols1)

plot(ols1, which = 1)

plot(ols1, which = 2) 

resid1<-as_tibble(residuals(ols1))
resid1 %>%
  ggplot(aes(value)) + 
  geom_density(color = "black", fill="grey") +
  xlab("Residuals") +
  ylab("") +
  ggtitle("Density of the residuals") 

shapiro.test(residuals(ols1))

bptest(ols1)

     ## Second model (ols and some tests on the normality and on the quality of fit in general)

ols2 <- lm (log_loans ~ log_digital_fin + acces_elec + employ_agri + foreign_DI
            + log_gdp + log_CPI + HCI + log_urban_pop, data=base_esti)
summary(ols2)


plot(ols2, which = 1)

plot(ols2, which = 2)  

resid2<-as_tibble(residuals(ols2))
resid2 %>%
  ggplot(aes(value)) + 
  geom_density(color = "black", fill="grey") +
  xlab("Residuals") +
  ylab("") +
  ggtitle("Density of the residuals") 

shapiro.test(residuals(ols2))
 
bptest(ols2)

      ## Third model (ols and some tests on the normality and on the quality of fit in general)

ols3 <- lm (log_savings ~ log_digital_fin + acces_elec + employ_agri + foreign_DI
            + log_gdp + log_CPI + HCI + log_urban_pop, data=base_esti)
summary(ols3)

plot(ols3, which = 1)

plot(ols3, which = 2) 

resid3<-as_tibble(residuals(ols3))
resid3 %>%
  ggplot(aes(value)) + 
  geom_density(color = "black", fill="grey") +
  xlab("Residuals") +
  ylab("") +
  ggtitle("Density of the residuals") 

shapiro.test(residuals(ols3))

bptest(ols3)

       ## Fourth model (ols and some tests on the normality and on the quality of fit in general)

ols4 <- lm (FI_Index ~ log_digital_fin + acces_elec + employ_agri + foreign_DI
            + log_gdp + log_CPI + HCI + log_urban_pop, data=base_esti)
summary(ols4)

plot(ols4, which = 1)

plot(ols4, which = 2)  

resid4<-as_tibble(residuals(ols4))
resid4 %>%
  ggplot(aes(value)) + 
  geom_density(color = "black", fill="grey") +
  xlab("Residuals") +
  ylab("") +
  ggtitle("Density of the residuals") 

shapiro.test(residuals(ols4))

bptest(ols4)

   # OLS Results
stargazer(ols1, ols2, ols3, ols4, 
          type = "text",
          dep.var.labels = c("Deposit account(log)", "Loans(log)", "Savings(log)", "Financial inclusion"),
          covariate.labels = c("Digital finance",
                               "Access to electricity", "Farm employment", "Foreign direct investment","Gross domestic product",
                               "Consumer price index(log)", "Human capital index", "Urban population(log)"))


#----------- Estimation with instrumental variable method

    # We took as instruments, the log_urban_pop and the log_pop_density


         ## First model

ivreg1 <- ivreg(log_dep_account ~ log_digital_fin +acces_elec + employ_agri + foreign_DI + log_gdp + log_CPI + HCI|
                 . - log_digital_fin + log_urban_pop + log_pop_density, data = base_esti) 

coeftest(ivreg, vcov = vcovHC, type = "HC0")

summary(ivreg, vcov = sandwich, df = Inf, diagnostics = TRUE)


      ## Second model

ivreg2 <- ivreg(log_loans ~ log_digital_fin + acces_elec + employ_agri + foreign_DI + log_gdp + log_CPI + HCI |
                  . - log_digital_fin + log_urban_pop + log_pop_density, data = base_esti) 

coeftest(ivreg2, vcov = vcovHC, type = "HC0")

summary(ivreg2, vcov = sandwich, df = Inf, diagnostics = TRUE)


      ## Third model

ivreg3 <- ivreg(log_savings ~ log_digital_fin + acces_elec + employ_agri + foreign_DI + log_gdp + log_CPI + HCI |
                  . - log_digital_fin  + log_urban_pop + log_pop_density, data = base_esti) 

coeftest(ivreg3, vcov = vcovHC, type = "HC0")

summary(ivreg3, vcov = sandwich, df = Inf, diagnostics = TRUE)


       ## Fourth model

ivreg4 <- ivreg(FI_Index ~ log_digital_fin + acces_elec + employ_agri + foreign_DI + log_gdp + log_CPI +HCI |
                  . - log_digital_fin + log_urban_pop + log_pop_density, data = base_esti) 

coeftest(ivreg4, vcov = vcovHC, type = "HC0")

summary(ivreg4, vcov = sandwich, df = Inf, diagnostics = TRUE)



# 2SLS Results
stargazer(ivreg1, ivreg2, ivreg3, ivreg4, 
          type = "text",
          dep.var.labels = c("Deposit account(log)", "Loans(log)", "Savings(log)", "Financial inclusion"),
          covariate.labels = c("Digital finance",
                               "Access to electricity", "Farm employment", "Foreign direct investment","Gross domestic product",
                               "Consumer price index(log)", "Human capital index"))



######---------------------------------------------------------Thanks---------------------------------------------------------###############






               






































