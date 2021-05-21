rm(list=ls())
library(Quandl)
library(tidyverse)
library(quantmod)
#install.packages("tfplot")
library(tfplot)
#install.packages("car")
library(car)

##############################
# Data Pull & Wrangling:

## Collect Raw Data
bitcoin <- Quandl("BCHAIN/MKPRU", api_key="Bjcsj4rxQbWasXJA_usP")
usdollar <- Quandl("FRED/DTWEXBGS", api_key="Bjcsj4rxQbWasXJA_usP")
real10y <- Quandl("FRED/DFII10", api_key="Bjcsj4rxQbWasXJA_usP")
goldprice <- Quandl("LBMA/GOLD", api_key="Bjcsj4rxQbWasXJA_usP")
FedBalSht <- Quandl("FRED/WALCL", api_key="Bjcsj4rxQbWasXJA_usP")
Sentiment <- Quandl("AAII/AAII_SENTIMENT", api_key="Bjcsj4rxQbWasXJA_usP")

## Remove NAs
bitcoin <- na.omit(bitcoin)
usdollar <- na.omit(usdollar)
real10y <- na.omit(real10y)
goldprice <- na.omit(goldprice)
FedBalSht <- na.omit(FedBalSht)
Sentiment <- na.omit(Sentiment)

## Convert to XTS, Date becomes Index, Remove "old" date
bitcoin <- as.xts(bitcoin, order.by = bitcoin$Date)
bitcoin <- subset(bitcoin, select=("Value"))
## Rename Value column to Series label
colnames(bitcoin)[1] <- "Bitcoin (USD)"


# write.csv(as.data.frame(bitcoin), "C:/Users/WG_Ro/OneDrive/AAA - STUFF/Research/Alternatives/Crypto/Bitcoin since 2009.csv")


## Bitcoin, while "zero" before March 2010, still is not actually 0
bitcoin[is.na(bitcoin)] <- 0
na.locf(bitcoin, na.rm = TRUE)

## Convert to XTS, Date becomes Index, Remove "old" date
usdollar <- as.xts(usdollar, order.by = usdollar$Date)
usdollar <- subset(usdollar, select=("Value"))
colnames(usdollar)[1] <- "TW US Dollar"

## Convert to XTS, Date becomes Index, Remove "old" date
real10y <- as.xts(real10y, order.by = real10y$Date)
real10y <- subset(real10y, select=("Value"))
colnames(real10y)[1] <- "Real 10y yield"

## Convert to XTS, Date becomes Index, Remove "old" date
goldprice <- as.xts(goldprice, order.by = goldprice$Date)
goldprice <- subset(goldprice, select=("USD (PM)"))
colnames(goldprice)[1] <- "Gold price (USD)"

## Convert to XTS, Date becomes Index, Remove "old" date
FedBalSht <- as.xts(FedBalSht, order.by = FedBalSht$Date)
FedBalSht <- subset(FedBalSht, select=("Value"))
colnames(FedBalSht)[1] <- "Fed Balance Sheet (US$mn)"

## Convert to XTS, Date becomes Index, Remove "old" date
Sentiment <- as.xts(Sentiment, order.by = Sentiment$Date)
Sentiment <- subset(Sentiment, select=("Bullish"))
colnames(Sentiment)[1] <- "AAII Sentiment (Bullish)"


## Ensure all data (bar Date) is numeric
storage.mode(bitcoin) <- "numeric"
storage.mode(usdollar) <- "numeric"
storage.mode(goldprice) <- "numeric"
storage.mode(FedBalSht) <- "numeric"
storage.mode(Sentiment) <- "numeric"


## Merge ONLY Daily Series into one object
DailySeries <- merge.xts(bitcoin, usdollar, goldprice, real10y)

## Because Bitcoin is traded 24/7, we remove weekends (NAs for all but Bitcoin)
DailySeries <- na.omit(DailySeries)
data.class(DailySeries)


## Bitcoin data really only starts in 2011 - so make index of dates that removes priors
Dates3 <- seq.Date(from=as.Date("2011-01-01"), to=end(DailySeries), by="day")
DailySeries <- DailySeries[Dates3]

## See data so far
# view(DailySeries)


## The above clean daily series object can now be used for TS Analysis
## ANOVA, Correlation, Scatterplots, boxplots, etc



## Ahead of Regression Analysis:

## Daily returns / %age changes can be calculated to make data "Stationary"
daily_ <- as.data.frame(DailySeries)
dailypercent <- 100 * (lead(daily_) - daily_)/daily_

## change Inf to 0
dailypercent$Real.10y.yield[which(is.infinite(dailypercent$Real.10y.yield) == TRUE)] <-0

#### DailyChgs <- diff(DailySeries[,0:3])/abs(DailySeries[-nrow(DailySeries),0:3]) * 100
#### Returns are already in % (i.e., 5.4 means 5.4%)
#### I omitted to real yield from this calculation, because it is sometimes zero
#### making %age changes infinite or undefined

DailyChgs <- dailypercent

#### Double removes scientific notation which is confusing
#### storage.mode(DailyChgs) <- "double"

#### Remove the first row NaN - generated from chg transformation
## remove last entry of NA, calculation changed using lead()
DailyChgs <- DailyChgs[-nrow(DailyChgs),]

#### To handle real yield, I will take the log()
#### reals <- log(DailySeries[, 'Real.10y.yield'])

#### Merge daily Chgs of BTC, USD & Gold, with log(Real Rates)
#### DailyChgs <- merge(DailyChgs, reals)

## Scrub for NaNs
DailyChgs <- na.omit(DailyChgs)

## Scrub for all +/- infinity results & NaNs
DailyChgs <- DailyChgs[is.finite(rowSums(DailyChgs)),]




## The above clean daily series object can now be used for Regression Analysis
## lm(bitcoin (USD) ~ US Dollar, Gold Price (USD), Real 10y yield)
## Analysis of t-stats, p-values and R-Sq required & discussion

## NOTE: May have to further transform depending on Unit Root tests for Stationary
## See https://otexts.com/fpp2/stationarity.html



# FYI
## To convert XTS to Tibble, use this code
## XTSobject %>% fortify.zoo %>% as.tibble


## Now to handle & wrangle weekly data

## Merge weekly series, FedBalSht & Sentiment
FedSentimentWeekly <- merge.xts(FedBalSht, Sentiment)

## Examining the data, we see that while both series are weekly, BUT,
## different days are used, so we have to transform the Fed data by one day
## Create adjusted Fed data to bring appropriate dates to merge with Sentiment
FedBalShtAdjust <- na.locf(FedSentimentWeekly$Fed.Balance.Sheet..US.mn., fromLast=TRUE)

## Re-merge with adjusted Fed data
FedSentimentWeekly <- merge.xts(FedBalShtAdjust, Sentiment)

## Remove NAs - Only applicable to Sentiment data
FedSentimentWeekly <- na.omit(FedSentimentWeekly)

## Fed data really only starts in 2003 - so make index of dates that removes priors
Dates <- seq.Date(from=as.Date("2003-01-02"), to=end(FedSentimentWeekly), by="day")

## Filter merged object just for relevant dates (2003 to end)
FedSentimentWeekly <- FedSentimentWeekly[Dates]
# View(FedSentimentWeekly)

## Analysis that includes Fed & Sentiment, needs to have all data as weekly
## Daily data can be used for simple analysis of ONLY BTC, GOLD, USD & RealRates


## I tried to change the periodicity using to.period(), BUT,
## The dates just wouldn't align - it insisted on having data at an unusable date (per Fed & Sentiment dates)
## So, I will merge daily & weekly, and remove NAs, having the relevant daily value match date of weekly series

## Merge daily series of original daily data, to weekly FedSentiment
WeeklyTotal <- merge(DailySeries, FedSentimentWeekly)

## This data only starts in 2011 - so make index of dates that removes priors
Dates2 <- seq.Date(from=as.Date("2011-01-01"), to=end(FedSentimentWeekly), by="day")

# Only dates after 2011
WeeklyTotal <- WeeklyTotal[Dates2]

## Fill Last Obs Carried Forward - applicable to Weekly Series
WeeklyTotal <- na.locf(WeeklyTotal, na.rm=TRUE)


## Drop the NAs, mostly derived from weekly series
WeeklyTotal <- na.omit(WeeklyTotal)
## This drops daily data where weekly is NA
## This is the same as end-of-period periodicity change, without date alignment issues

# view(WeeklyTotal)
## Remember, this is price "as is" data - not change or log data

#########

# Merge Levels & Changes Daily data
library(lubridate)
## change the column names of DailyChgs so it does not collide 
## with DailySeries in the merge process
names(DailyChgs) <- c("BitcoinChgPct", "USDChgPct", "GoldChgPct", "Real10yChgPct")

## add a date column to DailyChgs to be used in inner join
DailyC <- DailyChgs %>% mutate(Date_ = ymd(rownames(DailyChgs))) %>% select(Date_, BitcoinChgPct, USDChgPct, GoldChgPct, Real10yChgPct)

## make DailySeries into data frame
DailySeriesDf <- as.data.frame(DailySeries)
## add a column date to DailySeries
DailyS <- DailySeriesDf %>% mutate(Date_ = ymd(rownames(DailySeriesDf))) %>% select(Date_,Bitcoin..USD., TW.US.Dollar, Gold.price..USD., Real.10y.yield)

## change WeeklyTotal to dataframe
WeeklyTotalDf <- as.data.frame(WeeklyTotal)

## add a column date to WeeklyTotal
WeeklyT <- WeeklyTotalDf %>% mutate(Date_ = ymd(rownames(WeeklyTotalDf))) %>% select(Date_, Fed.Balance.Sheet..US.mn., AAII.Sentiment..Bullish.)

## add daily % change to Fed balannce 
FedBalChgPct <- (100 *(lead(WeeklyT$Fed.Balance.Sheet..US.mn.) - WeeklyT$Fed.Balance.Sheet..US.mn.)/WeeklyT$Fed.Balance.Sheet..US.mn.)

WeeklyT <- WeeklyT %>% mutate(FedBalChgPct = FedBalChgPct)

## remove NA
WeeklyT <- drop_na(WeeklyT)

## remove rownames
rownames(DailyS) <- NULL
rownames(DailyC) <- NULL
rownames(WeeklyT) <- NULL

## change to tibble format
DailyS <- tibble(DailyS)
DailyC <- tibble(DailyC)
WeeklyT <- tibble(WeeklyT)

## merge DailyS and DailyC
Chgs_Levels <- inner_join(DailyS, DailyC, by = "Date_")


#### Chgs_Levels <- merge(DailySeries, DailyChgs)

#### Clarify Column Names
#### colnames(Chgs_Levels) <- c("BitcoinPriceUSD", "USDlevel", "GoldPrice", "Real10yLevel", 
####                           "BitcoinChgPct", "USDChgPct", "GoldChgPct", "LogReal10y") 

# Remove day one NA Change
Chgs_Levels <- drop_na(Chgs_Levels)

# view(Chgs_Levels)

## Merge Daily Levels & Chgs to FedBS & Sentiment Levels
# hybrid <- merge(Chgs_Levels, WeeklyTotal[,4:6])
hybrid <- inner_join(Chgs_Levels, WeeklyT, by = "Date_")
#### hybrid <- cbind(Chgs_Levels, WeeklyTotal[, 5:6])

# Remove day one NA Change
hybrid <- drop_na(hybrid)


# Clarify Column Names
colnames(hybrid) <- c("Date_", "BitcoinUSD", "USDlevel", "GoldPrice", "Real10yLevel", 
  "BitcoinChgPct", "USDChgPct", "GoldChgPct", "Real10yChgPct",
  "FedBal", "AAIIBulls", "FedBalChgPct") 
#### colnames(hybrid) <- c("BitcoinPriceUSD", "USDlevel", "GoldPrice", "Real10yLevel", 
####                      "BitcoinChgPct", "USDChgPct", "GoldChgPct", "LogReal10y",
####                      "Real10yLevel", "FedBal", "AAIIBulls") 

# Seek to drop extra "Real10yLevel"
keep <- c("BitcoinPriceUSD", "USDlevel", "GoldPrice", "Real10yLevel", 
          "BitcoinChgPct", "USDChgPct", "GoldChgPct", "LogReal10y",
          "FedBal", "AAIIBulls")
hybrid <- as.data.frame(hybrid)
hybrid <- hybrid[keep]
hybrid <- as.xts(hybrid)
# view(hybrid)


##############################
# Data Pull & Wrangling:

## We now have daily data for Bitcoin, USD, Gold & Real Rates
# view(DailySeries)

## We now have daily price change data for the above also
# view(DailyChgs)

## We now have daily levels and changes for Bitcoin, USD, Gold & Real Rates in one XTS object
# view(Chgs_Levels)

## We now have weekly data for Bitcoin, USD, Gold, Real rates, FedBal & AAII Sentiment 
# view(WeeklyTotal)


##############################
### Choose the best model
library(ALSM)
library(modelr)

hybrid_ <- hybrid %>% 
  mutate(LBitcoinUSD = log2(BitcoinUSD+1)) %>%
  select(-BitcoinUSD) %>%
  relocate(LBitcoinUSD, .after = Date_)

## split data for training and model validation
set.seed(54321)

train <- sample_n(hybrid_, floor(nrow(hybrid_) * 0.75))
test <- subset(hybrid_, !(Date_%in%train$Date_))

## use daily % change dataset only
hybridP <- train[, c(6:9, 11, 12)] 
(hybridModelselect <- model.s(hybridP[, -1], hybridP$BitcoinChgPct))
## sort by AIC: compares the quality of a set of statistical models to each other
hybridModelselect[order(hybridModelselect[, 11]), decreasing = TRUE]
## the R^2 and adj. R^2 are pretty low so we will not use them for model selection

## model selection using the levels only
train.levels <- train[, c(2:5, 11:12)]
(trainModelselect <- model.s(train.levels[, -1], train.levels$LBitcoinUSD))

## sort by AIC and obtain the best 5 value (lowest is the best)
(trainModels <- head(trainModelselect[order(trainModelselect[, 11]), decreasing = TRUE], n = 5))
## the R^2's are better, but at max ~ 45% only.  We will go with Levels only. 

## confirm models
## rather than going by forward selection, meaning adding one parameter at a time, we will do a full
## model and remove 1 parameter at a time to check for an improvement
## full model  
btc.full <- lm(train.levels$LBitcoinUSD ~ ., train.levels[, - 1])
summary(btc.full)
## summary indicates that we can potentially drop GoldPrice, Real10y and FedBal

## from full model remove 1 parameter at a time to confirm model.s values
btc.back <- step(btc.full, direction = "backward")
## backward-selection recommends dropping GoldPrice, Real10y and FedBal
anova(btc.back, btc.full)
## our reduced model hybrid.train$LBitcoinUSD ~ USDlevel + GoldPrice, AAIIBulls

## test our model in test dataset
hybrid.test <- test[, c(2:5, 11:12)]
btc.test <- lm(LBitcoinUSD ~ USDlevel + GoldPrice + AAIIBulls, data = hybrid.test)
summary(btc.test)

## Model Validation
## subset of our models
train2 <- hybrid.train %>% select(LBitcoinUSD, USDlevel, GoldPrice, AAIIBulls)
test2 <- hybrid.test %>% select(LBitcoinUSD, USDlevel, GoldPrice, AAIIBulls)
## validate model
modelval(train2[, 2:4], train2[, 1], test2[, 2:4], test2[, 1])
btc.hat <- predict(btc.back, data.frame(test2[, 2:4]))
(mspr <- mean((test2$LBitcoinUSD-btc.hat)^2))
summary(btc.back)$sigma^2
## MSPR in in test model is lower, but relatively close, than MSE in train




###############################
### Perform regression analysis:




#### Bitcoin level versus levels of Gold, Rates & USD
#### Explore results: t-stats, p-values, Adj-R-sq, AIC
#### model1 <- lm(BitcoinPriceUSD ~ USDlevel + GoldPrice + Real10yLevel, data=Chgs_Levels)
#### summary(model1)
#### AIC(model1)

#### Bitcoin level versus levels & changes of Gold & USD, and level & log of Rates
#### Explore results: t-stats, p-values, Adj-R-sq, AIC
#### model2 <- lm(BitcoinPriceUSD ~ USDlevel + GoldPrice + Real10yLevel + 
####               USDChgPct + GoldChgPct + LogReal10y,
####             data=Chgs_Levels)
#### summary(model2)
#### AIC(model2)

#### Bitcoin level versus levels & changes of Gold & USD, level & log of Rates, FedBS level, and Sentiment level
#### Explore results: t-stats, p-values, Adj-R-sq, AIC
#### model3 <- lm(BitcoinPriceUSD ~ USDlevel + GoldPrice + Real10yLevel + 
####                USDChgPct + GoldChgPct + LogReal10y + FedBal + AAIIBulls,
####              data=hybrid)
#### summary(model3)
#### AIC(model3)


#### Bitcoin changes versus levels & changes in Gold & USD, level & log of Rates, 
#### levels of FedBS & Sentiment
#### Explore results: t-stats, p-values, Adj-R-sq, AIC
#### model4 <- lm(BitcoinChgPct ~ USDlevel + GoldPrice + Real10yLevel + 
####               USDChgPct + GoldChgPct + LogReal10y + FedBal + AAIIBulls,
####             data=hybrid)
#### summary(model4)
#### AIC(model4)

# Discuss AIC & R-sq differences of various above regressions - specify best model and why

# 
# 
# #Histogram of Residuals
# hist(model$resid, 
#      main="Histogram of Residuals",
#      xlab="Residual frequency",
#      ylab="Residuals", breaks = 10000)
# 
# # Inputs vs Residuals Plot
# residualPlot(model1)
# residualPlot(model2)
# residualPlot(model3)
# residualPlot(model4)
# 
# # Theoretical Quantiles of Residuals
# qqnorm(model1$resid)
# qqline(model1$resid)
# 
# qqnorm(model2$resid)
# qqline(model2$resid)
# 
# qqnorm(model3$resid)
# qqline(model3$resid)
# 
# qqnorm(model4$resid)
# qqline(model4$resid)


# Jarque-Berra Test
#install.packages("fBasics")
library(fBasics)
jarqueberaTest(model1$resid) #Test residuals for normality
#Null Hypothesis: Skewness and Kurtosis are equal to zero
## asymptomatic pvalue is 2.2e-16, so our our 
qqnorm(btc.m2$residuals)
qqline(btc.m2$residuals, col = "red")


# Durbin-Watson Test
#install.packages("lmtest")
library(lmtest) #dwtest
dwtest(btc.m2) #Test for independence of residuals
#Null Hypothesis: Errors are serially UNcorrelated


# Dickey-Fuller Tests
# install.packages("tseries")
library(tseries)
adf.test(hybrid2$BitcoinUSD,k=0)
adf.test(hybrid2$USDlevel,k=0)
adf.test(hybrid2$GoldPrice,k=0)
adf.test(hybrid2$Real10yLevel,k=0)
adf.test(hybrid2$AAIIBulls,k=0)
# Null hypothesis that a unit root is present in an autoregressive model.

#############################
# Plots
library(ggplot2)
# install.packages("hrbrthemes")
library(hrbrthemes)
# install.packages("plotly")
# library(plotly)
# install.packages("viridis")
library(viridis)

# colnames(WeeklyTotal)

ggplot(WeeklyTotal, 
       aes(x=Fed.Balance.Sheet..US.mn., y=Bitcoin..USD.,  
           alpha=TW.US.Dollar, size=TW.US.Dollar, color=TW.US.Dollar)) + 
  geom_point(size=6) +
  theme_ipsum()


ggplot(WeeklyTotal, aes(x=Fed.Balance.Sheet..US.mn., y=Bitcoin..USD., size = Real.10y.yield, 
                        color = 'blue')) +
  geom_point(alpha=0.7) +
  scale_size(range = c(1.4, 19), name="Population (M)") +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  theme_ipsum() +
  theme(legend.position="none")

## Correlations Plot - this takes some time as there are 11 variables
# install.packages("GGally")
GGally::ggpairs(hybrid)



## Cross Validation Exercise

# Create a split partition - to split data into train & test subsets
hybrid <- na.omit(hybrid)
hybridTib <- is_tibble(hybrid)
hybrid <- as.data.frame(hybrid)

# Define 75% split index
smp_size <- floor(0.75 * nrow(hybrid))

# Create random sample of 75:25 indices
train_index <- sample(seq_len(nrow(hybrid)), size = smp_size)

# Use indices to split data into two subsets (75% & 25%)
train_data <- hybrid[train_index,]
test_data <- hybrid[-train_index,]

# Build regression model with Training data
reg_model <- lm(BitcoinPriceUSD ~ ., data = train_data)

# Use Test data to make predictions & compute R-Sq, RMSE, and MAE
predictions <- reg_model %>% predict(test_data)

## Confirm that R-Square is similar to prior regressions
data.frame(R2 = R2(predictions, test.data$BitcoinPriceUSD),
           RMSE = RMSE(predictions, test.data$BitcoinPriceUSD),
           MAE = MAE(predictions, test.data$BitcoinPriceUSD))


### Random Forest Exercise


#install.packages("randomForest")
library(randomForest) 
set.seed(1)

# Convert XTS to Data Frame
hybridDF <- as.data.frame(hybrid)
# view(hybridDF)

bag.hybrid=randomForest(BitcoinPriceUSD ~. , data=hybridDF,
                        subset=train_index, mtry=9, importance=TRUE)

## Show that 97% of Variance is Explained !
bag.hybrid


## Further train/test split (50:50) for Random Forest
library(MASS)
# install.packages("ISLR")
library(ISLR)
# install.packages("tree")
library(tree)
set.seed(1)

# Equally split data
train = sample(1:nrow(hybridDF), nrow(hybridDF)/2)
test= hybridDF[-train ,]

# Train regression tree
tree.hybrid <- tree(BitcoinPriceUSD ~ ., hybridDF, subset=train)

library(rpart)
hybrid.rpart <- rpart(BitcoinPriceUSD ~ ., hybridDF, subset=train)
plot(hybrid.rpart)
text(hybrid.rpart)

# See Summary
summary(tree.hybrid)
plot(tree.hybrid)
text(tree.hybrid)

# Remove all Global Environment Objects & Values
# rm(list = ls()) 



# importance(rf.hybridDF)
# varImpPlot(rf.boston)