
rm(list = ls())

library(haven)
library(tidyr)
library(dplyr)
library(zoo)
library(stringr)
library(glmnet)
library(cowplot)
library(ggplot2)

setwd("C:/Users/wb519872/WBG/Xiaoou Zhu - Fall 2022 South Asia Economic Focus/Chapter 1/LASSO")

# Load quarterly GDP -------------------------------------------------------

# gdp <- read.csv("Real GDP IND LKA.csv") %>%
#   select(date, real_gdp_IND) %>%
#   filter(!is.na(real_gdp_IND)) %>%
#   rename(gdp = real_gdp_IND) %>%
#   mutate(date = as.yearmon(date, format="%b-%y")) %>%
#   mutate(date = as.yearqtr(date)) %>%
#   mutate(gdp = log(1+gdp/100))



# Load quarterly data ------------------------------------------------------

qq1 <- read.csv("0927_quarterly_external_motherfile.csv", header=FALSE)

qq1 <- qq1[,as.character( qq1[3,]) %in% c("country", "India")] 

nq <- paste0(qq1[2,], qq1[4,])

nq[1] <- "date"

names(qq1) <- nq
rm(nq)

qq1 <- qq1[-c(1:6),]

qq1 <- qq1[,-20]

qq1 <- qq1 %>%
  mutate(date = as.yearmon(date, format="%m/%d/%Y")) %>%
  mutate(date = as.yearqtr(date)) 




qq2 <- read.csv("quarterly_macro_motherfile.csv", header=FALSE)
qq2 <- qq2[,as.character( qq2[3,]) %in% c("country", "India")] 
nq <- paste0(qq2[2,], qq2[4,])
nq[1] <- "date"
names(qq2) <- nq
rm(nq)
qq2 <- qq2[-c(1:6),]
qq2 <- qq2 %>%
  mutate(date = as.yearmon(date)) %>%
  mutate(date = as.yearqtr(date)) 

drop <- c("Nominal GDP, saUSD mn",                                                                  
          "India: GDP: Private Final Consumption Expenditure(NSA, Bil.Apr.11-Mar.12.Rupees)Rupee bn",
          "India: GDP: Government Final Consumption Expenditure(NSA, Bil.Apr.11-Mar.12.INR)Rupee bn",
          "India: GDP: Gross Fixed Capital Formation (NSA, Bil.Apr.11-Mar.12.Rupees)Rupee bn" ,      
          "India: GDP: Exports of Goods and Services (NSA, Bil.Apr.11-Mar.12.Rupees)Rupee bn" ,      
          "India: GDP: Imports of Goods and Services (NSA, Bil.Apr.11-Mar.12.Rupees)Rupee bn" ,      
          "India: GDP at Market Prices (NSA, Bil.Apr.11-Mar.12.Rupees)Rupee bn",                     
          "India: GDP: Agriculture, Forestry & Fishing (NSA, Bil.Apr.11-Mar.12.Rupees)Rupee bn",     
          "India: GDP: Mining & Quarrying (NSA, Bil.Apr.11-Mar.12.Rupees)Rupee bn",                  
          "India: GDP: Manufacturing (NSA, Bil.Apr.11-Mar.12.Rupees)Rupee bn",                       
          "India: GDP: Construction (NSA, Bil.Apr.11-Mar.12.Rupees)Rupee bn",                        
          "India: GDP: Services (NSA, Bil.Apr.11-Mar.12.Rupees)Rupee bn")


qq <- qq1 %>%
  rename(gdp = `Real GDP, sa2010 USD mn`) %>%
  merge(qq2, by = c("date"), all = TRUE) %>% 
  select(-drop) %>% 
  filter(!is.na(date)) %>%
  gather(key = type, value = value, -date) %>% 
  mutate(value = as.numeric(value)) %>%
  filter(!is.na(value)) %>% 
  spread(key = type, value= value) 
  





############################## LOAD MONTHLY DATA

mm1 <- read.csv("0927_monthly_external_motherfile.csv", header=FALSE)
mm1 <- mm1[,as.character( mm1[3,]) %in% c("country", "India")] 

nq <- paste0(mm1[2,], mm1[4,])
nq[1] <- "date"
names(mm1 ) <- nq
rm(nq)
mm1  <- mm1 [-c(1:6),]

mm1 <- mm1  %>%
  mutate(date = as.yearmon(date, format="%m/%d/%Y")) %>%
  mutate(date = as.yearqtr(date)) %>% 
  filter(!is.na(date)) %>%    
  gather(key = type, value = value, -date) %>% 
  mutate(value = as.numeric(value)) %>%
  filter(!is.na(value)) %>% 
  group_by(date, type) %>%
  summarise(value = mean(value)) %>%
  ungroup() %>%
  spread(key = type, value= value)








mm2 <- read.csv("monthly_macro_motherfile.csv", header=FALSE)
mm2 <- mm2[,as.character( mm2[3,]) %in% c("country", "India")] 

nq <- paste0(mm2[2,], mm2[4,])
nq[1] <- "date"
names(mm2 ) <- nq
rm(nq)
mm2  <- mm2 [-c(1:6),]

mm2 <- mm2  %>%
  mutate(date = as.yearmon(date, format="%Y-%m-%d")) %>%
  mutate(date = as.yearqtr(date)) %>% 
  filter(!is.na(date)) %>%    
  gather(key = type, value = value, -date) %>% 
  mutate(value = as.numeric(value)) %>%
  filter(!is.na(value)) %>% 
  group_by(date, type) %>%
  summarise(value = mean(value)) %>%
  ungroup() %>%
  spread(key = type, value= value)




  


############################## LOADING DAILY DATA

aa1 <- read.csv("0927_daily_external_motherfile.csv", header=FALSE)
aa1 <- aa1[,as.character( aa1[3,]) %in% c("country", "India")] 

nq <- paste0(aa1[2,], aa1[4,])
nq[1] <- "date"
names(aa1 ) <- nq
rm(nq)
aa1  <- aa1 [-c(1:6),]

aa1 <- aa1  %>%
  mutate(date = as.yearmon(date, format="%m/%d/%Y")) %>%
  mutate(date = as.yearqtr(date)) %>% 
  filter(!is.na(date)) %>%    
  gather(key = type, value = value, -date) %>% 
  mutate(value = as.numeric(value)) %>%
  filter(!is.na(value)) %>% 
  group_by(date, type) %>%
  summarise(value = mean(value)) %>%
  ungroup() %>%
  spread(key = type, value= value)







aa2 <- read.csv("daily_macro_motherfile.csv", header=FALSE)
aa2 <- aa2[,as.character( aa2[3,]) %in% c("country", "India")] 

nq <- paste0(aa2[2,], aa2[4,])
nq[1] <- "date"
names(aa2 ) <- nq
rm(nq)
aa2  <- aa2 [-c(1:6),]

aa2 <- aa2  %>%
  mutate(date = as.yearmon(date, format="%Y-%m-%d")) %>%  
  mutate(date = as.yearqtr(date)) %>% 
  filter(!is.na(date)) %>%    
  gather(key = type, value = value, -date) %>% 
  mutate(value = as.numeric(value)) %>% 
  filter(!is.na(value)) %>% 
  group_by(date, type) %>%
  summarise(value = mean(value)) %>%
  ungroup() %>%
  spread(key = type, value= value)






############################## LASSO Analysis ############################################ >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

dd <- qq %>%
  merge(mm1, by = c("date"), all = TRUE) %>%
  merge(mm2, by = c("date"), all = TRUE) %>%
  merge(aa1, by = c("date"), all = TRUE) %>%
  merge(aa2, by = c("date"), all = TRUE) %>%
  filter(date >= as.yearqtr("2011 Q1")) %>%
  select(date, gdp, everything())

dd_change <- dd %>%
  gather(key = variable, value = value, -date) %>% 
  group_by(variable) %>%
  summarise(value = log(value)-log(lag(value,4)),
            date = date) %>%
  filter(!is.na(value)) %>%  
  filter(!is.nan(value)) %>%
  spread(key = variable, value = value) %>%
  select(date, everything()) 

names(dd_change) <- paste0("DD ", names(dd_change))
names(dd_change)[1] <- "date"


ff <- dd %>%
  select(-gdp) %>%
  merge(dd_change, by = c("date"), all = TRUE) %>%
  rename(gdp = `DD gdp`) %>%
  select(date, gdp, everything()) %>%
  filter(date >= as.yearqtr("2012 Q1")) %>%
  filter(date <= as.yearqtr("2022 Q3")) 

ff <- ff[ , colSums(is.na(ff)) <=1 ] 

ff <- ff %>%
  select(-c(`Motor Vehicle Sales: DM: Two Wheelers (TW)Unit`,
            `Passenger Traffic: All AirportsPerson`,
            `Cargo Traffic: All AirportsTon`,
            `Production: Crude SteelMetric Ton th`,
            `SCB: Outstanding: Bank Credit: FoodINR mn`,
            `Consumer Confidence: Net Balance: Monthly: India% Point`))

################# >>>>> LASSO

# cc <- ff[ , colSums(is.na(ff)) <=1 ] %>%
#   filter(date < as.yearqtr("2022 Q1")) 

cc <- ff

y <- cc %>%
  filter(date < as.yearqtr("2022 Q2")) %>% 
  select(gdp) %>% 
  as.matrix()  

x <- cc %>%
  select(-gdp)

x <- x[ , colSums(is.na(x)) ==0 ] 

x <- x %>%
  filter(date < as.yearqtr("2022 Q2")) %>% 
  select(-date) %>% 
  as.matrix() 



alpha1.fit <- cv.glmnet(x, y, 
                        type.measure = "mse", 
                        alpha=1, family="gaussian")

# alpha1.predicted <- predict(alpha1.fit, s = alpha1.fit$lambda.min, newx = x)   %>% as.array()


################# >>>>> OLS

rr <- predict(alpha1.fit, s = alpha1.fit$lambda.min, type="coefficients")  %>%
  as.matrix() 

rr <- names(rr[rr >0,1])

rr <- rr[!(rr %in% c("(Intercept)"))]

ols_mod <- lm( gdp ~ ., 
               data = ff %>% 
                 mutate(gdp = if_else(date==as.yearqtr("2022 Q3"), NaN, gdp)) %>%
                 select(gdp, rr)   ) 


################# >>>>> Plot line

dd_IND <- ff %>%  
  select(date, gdp, rr) %>% 
  mutate(pred_ols = predict(ols_mod, ff %>% select(date, gdp, rr) )) %>%   
  select(date, pred_ols, gdp) %>% 
  filter(date >= as.yearqtr("2019 Q1")) %>%  
  filter(!is.na(pred_ols)) %>%
  mutate(gdp = (exp(gdp)-1)*100,
         pred_ols = (exp(pred_ols)-1)*100) %>%
  mutate(pred_ols = round(pred_ols, digits = 2)) 


dd_IND %>%
  write.csv(file="LASSO_results_India.csv")
  
dd_IND %>%
  ggplot( ) +
  geom_col(aes(x=date, y = gdp)) +
  geom_line(aes(x=date, y = pred_ols), color = "red", size=2 ) +  
  geom_label(aes(x=date, y = pred_ols,label=pred_ols)) +
  labs(title = "India", x = NULL, y=NULL) +
  theme_bw()

ols_mod













