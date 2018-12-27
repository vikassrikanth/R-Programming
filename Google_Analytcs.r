I have included some of the packages to get desired results and get good graphs to highlight the relationships between the variables. Please note that I have not included install.packages in my code (please include it while running the code).

# LOAD THE LIBRARIES 

library(data.table)
library(jsonlite)
library(readr)
library(dplyr)
library(tidyverse)
library(magrittr)
library(lubridate)
library(purrr)
library(ggplot2)
library(gridExtra)
library(countrycode)
library(highcharter)
library(zoo)
library(TTR)
library(xts)
library(digest)
library(ggExtra)
library(grid)
library(mime)
library(Biobase)
library(caret)
library(scales)
library(tibble)
library(timeDate)
library(ggthemes)
library(DataExplorer)
library(cowplot)
library(rlist)

# LOAD THE DATA SETS #

train <- read_csv("train.csv")
test <- read_csv("test.csv")
submission <- read_csv("sample_submission_v2.csv")

# MAKE A FUNCTION TO CONVERT JSON TO DF #

jsontodf <- function(col){
  list.stack(lapply(col, function(j){
    as.list(unlist(fromJSON(j)))}) , fill=TRUE)
}

tr_device <- jsontodf(train$device)
tr_geoNetwork <- jsontodf(train$geoNetwork)
tr_totals <- jsontodf(train$totals)
tr_trafficSource <- jsontodf(train$trafficSource)

# FLATTENING THE JSON 

flatten_json <- . %>% 
  str_c(., collapse = ",") %>% 
  str_c("[", ., "]") %>% 
  fromJSON(flatten = T)

parse <- . %>% 
  bind_cols(flatten_json(.$device)) %>%
  bind_cols(flatten_json(.$geoNetwork)) %>% 
  bind_cols(flatten_json(.$trafficSource)) %>% 
  bind_cols(flatten_json(.$totals)) %>% 
  select(-device, -geoNetwork, -trafficSource, -totals)


test <- parse(test)
te_device <- test[,c("browser", "browserVersion","browserSize","operatingSystem","operatingSystemVersion",
                     "isMobile","mobileDeviceBranding", "mobileDeviceModel", "mobileInputSelector", "mobileDeviceInfo",
                     "mobileDeviceMarketingName","flashVersion","language","screenColors","screenResolution","deviceCategory")]
te_geoNetwork <- test[,c("continent","subContinent","country","region","metro","city","cityId","networkDomain","latitude",
                         "longitude","networkLocation")]
te_totals <- test[,c("visits","hits","pageviews","bounces","newVisits")]
te_trafficSource <- test[,c("campaign","source","medium","keyword","adwordsClickInfo.criteriaParameters", "isTrueDirect",
                            "referralPath","adwordsClickInfo.page","adwordsClickInfo.slot","adwordsClickInfo.gclId",
                            "adwordsClickInfo.adNetworkType","adwordsClickInfo.isVideoAd","adContent")]


#Check to see if the training and test sets have the same column names
setequal(names(tr_device), names(te_device))
setequal(names(tr_geoNetwork), names(te_geoNetwork))
setequal(names(tr_totals), names(te_totals))
setequal(names(tr_trafficSource), names(te_trafficSource))

#As expected, all are equal except for the totals - which includes the target, transactionRevenue
#Clearly this should only appear in the training set
names(tr_totals)
names(tr_device)
names(tr_geoNetwork)
names(tr_trafficSource)
names(te_totals)

#Combine to make the full training and test sets
train <- train %>%
  cbind(tr_device, tr_geoNetwork, tr_totals, tr_trafficSource) %>%
  select(-device, -geoNetwork, -totals, -trafficSource)


test <- test %>%
  cbind(te_device, te_geoNetwork, te_totals, te_trafficSource) %>%
  select(-device, -geoNetwork, -totals, -trafficSource)

glimpse(train)
glimpse(test)

#Variable Typecasting
train$visits <- as.numeric(train$visits)
train$hits <- as.numeric(train$hits)
train$pageviews <- as.numeric(train$pageviews)
train$bounces <- as.numeric(train$bounces)
train$newVisits <- as.numeric(train$newVisits)
train$transactionRevenue <- as.numeric(train$transactionRevenue)


test$visits <- as.numeric(test$visits)
test$hits <- as.numeric(test$hits)
test$pageviews <- as.numeric(test$pageviews)
test$bounces <- as.numeric(test$bounces)
test$newVisits <- as.numeric(test$newVisits)


train$date <- as.Date(strptime(train$date, format = "%Y%m%d"))
test$date <- as.Date(strptime(test$date, format = "%Y%m%d"))

head(train,2)
#class(train)
str(train)

# GETTING RID OF COLUMNS WITH ONLY ONE UNIQUE VALUE SINCE THEY WON'T BE USEFUL #

uniqvals <- sapply(train, n_distinct)
print(uniqvals)
todel <- names(uniqvals[uniqvals == 1])
train %<>% select(-one_of(todel))
test %<>% select(-one_of(todel))

# PLOTTING THE PERCENTAGE OF MISSING VALUES IN THE TRAINING SET #

plot_missing(train)

# PLOTTING THE TARGET VARIABLE (TRANSACTION REVENUE) FREQUENCY DISTRIBUTION #

train  %>% 
  ggplot() + geom_density(aes(as.numeric(transactionRevenue))) +
  labs(x = "transactionRevenue",
       title = "Frequency Distribution of transactionRevenue") +
  theme_minimal() -> p1

train  %>% 
  ggplot() + geom_density(aes(log(as.numeric(transactionRevenue)))) +
  labs(x = "transactionRevenue",
       title = "Frequency Distribution of Natural Log transactionRevenue") +
  theme_minimal() -> p2

plot_grid(p1,p2)


# PLOTTING THE TRANSACTION REVENUE FOR TRAINING AND TEST SET

t1 <- train %>% mutate(date = ymd(date), 
                       year_month = make_date(year(date), month(date))) %>% 
  group_by(year_month) %>% count() %>% 
  ggplot(aes(x = year_month, y = n)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = "", y = "transactions", title = "Training Set") +
  theme_minimal() +
  scale_x_date(labels = date_format("%Y - %m"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  geom_vline(aes(xintercept = max(year_month), colour = "red"), size = 1) +
  theme(legend.position="none")


t2 <- test %>% mutate(date = ymd(date), 
                      year_month = make_date(year(date), month(date))) %>% 
  group_by(year_month) %>% count() %>% 
  ggplot(aes(x = year_month, y = n)) +
  geom_bar(stat="identity", fill="steelblue") +
  labs(x = "", y = "transactions",  title = "Test") +
  theme_minimal() +
  scale_x_date(labels = date_format("%Y - %m"))+
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

grid.arrange(t1,t2)

# WE SHOULD ASSUME THAT NA IN TRANSACTION REVENUE MEANS THAT TRANSACTION WAS NOT MADE AND SO #
# WE SHOULD CONVERT THE NA TO 0 #

tr <- train$transactionRevenue
summary(tr)
tr[is.na(tr)] <- 0
summary(tr)

# VISITS TO SITE VS TRANSACTION REVENUE OVER TIME #

t3 <- train %>% 
  bind_cols(as_tibble(tr)) %>% 
  group_by(date) %>% 
  summarize(visits = n()) %>% 
  ungroup() %>% 
  ggplot(aes(x = date, y = visits)) + 
  geom_line() +
  geom_smooth() + 
  labs(x = "") +
  theme_minimal()

t4 <- train %>% 
  bind_cols(as_tibble(tr)) %>% 
  group_by(date) %>% 
  summarize(revenue = mean(value)) %>% 
  ungroup()  %>% 
  ggplot(aes(x = date, y = revenue)) + 
  geom_line() +
  geom_smooth() +
  labs(x = "") +
  theme_minimal()

grid.arrange(t3,t4)

# NOW TO PLOT TRANSACTION REVENUE AND FREQUENCY OF USE OF DIFFERENT BROWSERS #

t5 <- train %>% 
  bind_cols(as_tibble(tr)) %>% 
  group_by(browser) %>% 
  summarize(visits = n()) %>% 
  top_n(9,wt=visits) %>%
  ggplot(aes(x = browser, y = visits)) + 
  geom_bar(fill="#6666c3", stat="identity") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme_minimal() 

t6<- train %>% 
  bind_cols(as_tibble(tr)) %>% 
  group_by(browser) %>% 
  summarise(revenue = sum(value)) %>%
  top_n(9,wt=revenue) %>%
  ggplot(aes(x = browser, y = revenue)) +
  geom_bar(fill="#6666c3", stat="identity") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme_minimal() 

grid.arrange(t5,t6)

# TO GET TRANSACTION REVENUE AND FREQUENCY OF DIFFERENT CHANNEL GROUPINGS #

t7 <- train %>% 
  bind_cols(as_tibble(tr)) %>% 
  group_by(channelGrouping) %>% 
  summarize(visits = n()) %>% 
  ggplot(aes(x = channelGrouping, y = visits)) + 
  geom_bar(fill="#6666c3", stat="identity") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme_minimal() 

t8 <- train %>% 
  bind_cols(as_tibble(tr)) %>% 
  group_by(channelGrouping) %>% 
  summarise(revenue = sum(value)) %>%
  ggplot(aes(x = channelGrouping, y = revenue)) +
  geom_bar(fill="#6666c2", stat="identity") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

grid.arrange(t7,t8)

# TO PLOT TRANSACTION REVENUE AND FREQUENCY FOR OPERATING SYSTEMS #

t9 <- train %>% 
  bind_cols(as_tibble(tr)) %>% 
  group_by(operatingSystem) %>% 
  summarize(visits = n()) %>%
  ggplot(aes(x = operatingSystem, y = visits)) + 
  geom_bar(fill="#6666c3", stat="identity") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme_minimal() 

t10 <- train %>% 
  bind_cols(as_tibble(tr)) %>% 
  group_by(operatingSystem) %>% 
  summarise(revenue = sum(value)) %>%
  top_n(10,wt=revenue) %>%
  ggplot(aes(x = operatingSystem, y = revenue)) +
  geom_bar(fill="#6666c1", stat="identity") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

grid.arrange(t9,t10)


# TO PLOT TRANSACTION REVENUE AND FREQUENCY FOR DIFFERENT DEVICES #

t11 <- train %>% 
  bind_cols(as_tibble(tr)) %>% 
  group_by(deviceCategory) %>% 
  summarize(visits = n()) %>%
  top_n(10,wt=visits) %>%
  ggplot(aes(x = deviceCategory, y = visits)) + 
  geom_bar(fill="#6666c3", stat="identity") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme_minimal() 

t12 <- train %>% 
  bind_cols(as_tibble(tr)) %>% 
  group_by(deviceCategory) %>% 
  summarise(revenue = sum(value)) %>%
  top_n(10,wt=revenue) %>%
  ggplot(aes(x = deviceCategory, y = revenue)) +
  geom_bar(fill="#6666c1", stat="identity") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

grid.arrange(t11,t12)


# TO PLOT TRANSACTION REVENUE AND FREQUENCY FOR DIFFERENT COUNTRIES #

t13 <- train %>% 
  bind_cols(as_tibble(tr)) %>% 
  group_by(country) %>% 
  summarize(visits = n()) %>%
  top_n(10,wt=visits) %>%
  ggplot(aes(x = country, y = visits)) + 
  geom_bar(fill="#6666c3", stat="identity") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme_minimal() 

t14 <- train %>% 
  bind_cols(as_tibble(tr)) %>% 
  group_by(country) %>% 
  summarise(revenue = sum(value)) %>%
  top_n(10,wt=revenue) %>%
  ggplot(aes(x = country, y = revenue)) +
  geom_bar(fill="#6666c1", stat="identity") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

grid.arrange(t13,t14)


# TO PLOT TRANSACTION REVENUE AND FREQUENCY FOR DIFFERENT CONTINENT #

t15 <- train %>% 
  bind_cols(as_tibble(tr)) %>% 
  group_by(continent) %>% 
  summarize(visits = n()) %>%
  top_n(10,wt=visits) %>%
  ggplot(aes(x = continent, y = visits)) + 
  geom_bar(fill="#6666c3", stat="identity") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme_minimal() 

t16 <- train %>% 
  bind_cols(as_tibble(tr)) %>% 
  group_by(continent) %>% 
  summarise(revenue = sum(value)) %>%
  top_n(10,wt=revenue) %>%
  ggplot(aes(x = continent, y = revenue)) +
  geom_bar(fill="#6666c1", stat="identity") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

grid.arrange(t15,t16)



# TO PLOT TRANSACTION REVENUE AND FREQUENCY FOR DIFFERENT SUBCONTINENT #

t17 <- train %>% 
  bind_cols(as_tibble(tr)) %>% 
  group_by(subContinent) %>% 
  summarize(visits = n()) %>%
  top_n(10,wt=visits) %>%
  ggplot(aes(x = subContinent, y = visits)) + 
  geom_bar(fill="#6666c3", stat="identity") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme_minimal() 

t18 <- train %>% 
  bind_cols(as_tibble(tr)) %>% 
  group_by(subContinent) %>% 
  summarise(revenue = sum(value)) %>%
  top_n(10,wt=revenue) %>%
  ggplot(aes(x = subContinent, y = revenue)) +
  geom_bar(fill="#6666c1", stat="identity") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

grid.arrange(t17,t18)


# TO PLOT TRANSACTION REVENUE AND FREQUENCY FOR DIFFERENT REGION #

t19 <- train %>% 
  bind_cols(as_tibble(tr)) %>% 
  group_by(region) %>% 
  summarize(visits = n()) %>%
  top_n(10,wt=visits) %>%
  ggplot(aes(x = region, y = visits)) + 
  geom_bar(fill="#6666c3", stat="identity") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme_minimal() 

t20 <- train %>% 
  bind_cols(as_tibble(tr)) %>% 
  group_by(region) %>% 
  summarise(revenue = sum(value)) %>%
  top_n(10,wt=revenue) %>%
  ggplot(aes(x = region, y = revenue)) +
  geom_bar(fill="#6666c1", stat="identity") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

grid.arrange(t19,t20)



# TO PLOT TRANSACTION REVENUE AND FREQUENCY FOR DIFFERENT CAMPAIGN #

t21 <- train %>% 
  bind_cols(as_tibble(tr)) %>% 
  group_by(campaign) %>% 
  summarize(visits = n()) %>%
  top_n(10,wt=visits) %>%
  ggplot(aes(x = campaign, y = visits)) + 
  geom_bar(fill="#6666c3", stat="identity") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme_minimal() 

t22 <- train %>% 
  bind_cols(as_tibble(tr)) %>% 
  group_by(campaign) %>% 
  summarise(revenue = sum(value)) %>%
  top_n(10,wt=revenue) %>%
  ggplot(aes(x = campaign, y = revenue)) +
  geom_bar(fill="#6666c1", stat="identity") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

grid.arrange(t21,t22)

#Date Wise Aggregation

date_wise <- train  %>% 
  group_by(date)  %>% 
  summarise(daily_visits = sum(visits, na.rm = TRUE),
            daily_hits = sum(hits, na.rm = TRUE),
            daily_pageviews = sum(pageviews, na.rm = TRUE),
            daily_bounces = sum(bounces, na.rm = TRUE),
            daily_newVisits = sum(newVisits, na.rm = TRUE),
            daily_transactionRevenue = sum(transactionRevenue, na.rm =TRUE)
  )

ggplot(date_wise,aes(date,daily_visits)) + geom_line() +
  theme_economist() +
  labs(title = "Daily Visits Trend",
       x = "Date",
       y = "Daily Visits") +
  geom_smooth() -> p1

ggplot(date_wise,aes(date,daily_hits)) + geom_line() +
  theme_economist() +
  labs(title = "Daily Hits Trend",
       x = "Date",
       y = "Daily Hits") +
  geom_smooth() -> p2

ggplot(date_wise,aes(date,daily_newVisits)) + geom_line() +
  theme_economist() +
  labs(title = "Daily new Visits Trend",
       x = "Date",
       y = "Daily new Visits") +
  geom_smooth() -> p3

ggplot(date_wise,aes(date,daily_bounces)) + geom_line() +
  theme_economist() +
  labs(title = "Daily Bounces Trend",
       x = "Date",
       y = "Daily Bounces") +
  geom_smooth() -> p4

plot_grid(p1,p2,p3,p4, ncol = 2)

date_wise$daily_bouncerate <- (date_wise$daily_bounces / date_wise$daily_visits) * 100

ggplot(date_wise,aes(date,daily_bouncerate)) + geom_line() +
  theme_economist() +
  labs(title = "Daily Bounce Rate Trend",
       subtitle = "Bounce Rate = Bounces / Visits ",
       x = "Date",
       y = "Daily Bounce Rate in %") +
  geom_smooth()

ggplot(date_wise,aes(date,daily_transactionRevenue)) + geom_line() +
  theme_economist() +
  labs(title = "Daily Transaction Revenue Trend",
       #subtitle = "Bounce Rate = Bounces / Visits ",
       x = "Date",
       y = "Daily Transaction Revenue") +
  geom_smooth() -> p1

ggplot(date_wise,aes(date,log(daily_transactionRevenue))) + geom_line() +
  theme_economist() +
  labs(title = "Daily Log Transaction Revenue Trend",
       #subtitle = "Bounce Rate = Bounces / Visits ",
       x = "Date",
       y = "Daily Log Transaction Revenue") +
  geom_smooth() -> p2

plot_grid(p1,p2)

#Now, Let's replicate the same for Test Data
date_wise_test <- test  %>% 
  group_by(date)  %>% 
  summarise(daily_visits = sum(visits, na.rm = TRUE),
            daily_hits = sum(hits, na.rm = TRUE),
            daily_pageviews = sum(pageviews, na.rm = TRUE),
            daily_bounces = sum(bounces, na.rm = TRUE),
            daily_newVisits = sum(newVisits, na.rm = TRUE)
  )
ggplot(date_wise_test,aes(date,daily_visits)) + geom_line() +
  theme_economist() +
  labs(title = "Daily Visits Trend",
       x = "Date",
       y = "Daily Visits") +
  geom_smooth() -> p1
ggplot(date_wise_test,aes(date,daily_hits)) + geom_line() +
  theme_economist() +
  labs(title = "Daily Hits Trend",
       x = "Date",
       y = "Daily Hits") +
  geom_smooth() -> p2
ggplot(date_wise_test,aes(date,daily_newVisits)) + geom_line() +
  theme_economist() +
  labs(title = "Daily new Visits Trend",
       x = "Date",
       y = "Daily new Visits") +
  geom_smooth() -> p3
ggplot(date_wise_test,aes(date,daily_bounces)) + geom_line() +
  theme_economist() +
  labs(title = "Daily Bounces Trend",
       x = "Date",
       y = "Daily Bounces") +
  geom_smooth() -> p4
plot_grid(p1,p2,p3,p4, ncol = 2)

date_wise_test$daily_bouncerate <- (date_wise_test$daily_bounces / date_wise_test$daily_visits) * 100
ggplot(date_wise_test,aes(date,daily_bouncerate)) + geom_line() +
  theme_economist() +
  labs(title = "Daily Bounce Rate Trend",
       subtitle = "Bounce Rate = Bounces / Visits ",
       x = "Date",
       y = "Daily Bounce Rate in %") +
  geom_smooth()

# TO PLOT DIFFERENT TRANSACTION REVENUE AND FREQUENCY FOR ISMOBILE #

t11 <- train %>% 
  bind_cols(as_tibble(tr)) %>% 
  group_by(isMobile) %>% 
  summarize(visits = n()) %>%
  ggplot(aes(x = isMobile, y = visits)) + 
  geom_bar(fill="#6666c3", stat="identity") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

t12 <- train %>% 
  bind_cols(as_tibble(tr)) %>% 
  group_by(isMobile) %>% 
  summarise(revenue = sum(value)) %>%
  top_n(10,wt=revenue) %>%
  ggplot(aes(x = isMobile, y = revenue)) +
  geom_bar(fill="#6666c1", stat="identity") +
  theme_minimal() +
  scale_y_continuous(labels = comma) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

grid.arrange(t11,t12)



# ___________________________ # ___________________________ #

#Let's move to Log as we have to predict Log of Revenue and some naive feature engineering

train$transactionRevenue[is.na(train$transactionRevenue)] <- 0
train$log_revenue <- log1p(train$transactionRevenue)

#feature engineering
train$dayofweek <- weekdays(train$date)
train$month <- months.Date(train$date)
train$operatingSystem <- ifelse(train$operatingSystem %in% c("Android","Macintosh","Linux",
                                                             "Windows","iOS","Chrome OS"),
                                train$operatingSystem,
                                "Others")

train$operatingSystem <- ifelse(train$operatingSystem %in% c("Android","Macintosh","Linux",
                                                             "Windows","iOS","Chrome OS"),
                                train$operatingSystem,
                                "Others")

test$dayofweek <- weekdays(test$date)
test$month <- months.Date(test$date)

test$operatingSystem <- ifelse(test$operatingSystem %in% c("Android","Macintosh","Linux",
                                                           "Windows","iOS","Chrome OS"),
                               test$operatingSystem,

                               "Others")
#Create Dummy variables – 

install.packages("dummies")
library(dummies)

train <- cbind(train, dummy(train$channelGrouping), dummy(train$isMobile), dummy(train$region),
               dummy(train$operatingSystem))
#train$isMobile <- as.numeric(train$isMobile)


test$dayofweek <- weekdays(test$date)
test$month <- months.Date(test$date)

test$operatingSystem <- ifelse(test$operatingSystem %in% c("Android","Macintosh","Linux",
                                                           "Windows","iOS","Chrome OS"),
                               test$operatingSystem,
                               "Others")


test <- cbind(test, dummy(test$channelGrouping), dummy(test$isMobile), dummy(test$region),
              dummy(test$operatingSystem))



library(h2o)
#h2o.init(nthreads=-1,max_mem_size='10G')
h2o.init(nthreads = -1, max_mem_size = '10g', ip = 'localhost', port = 54321)
#h2o.init()

x <- as.h2o(train)

features <- colnames(train)[!(colnames(train) %in% c("date",
                                                     "visitStartTime",
                                                     "visitEndTime",
                                                     "fullVisitorId",
                                                     
                                                     "sessionId",
                                                     "visitId", "flashVersion", "browserSize", "transactionRevenue",
                                                     #"revenue_yes",
                                                     # "channelGrouping",
                                                     "log_revenue"))]


model <- h2o.randomForest(x=features,
                          y="log_revenue", 
                          ntrees = 50,
                          max_depth = 30,
                          nbins_cats = 100,  
                          training_frame=x)

summary(model)

# To plot features by important

h2o.varimp_plot(model)


# To create the submission file

submission <- data.frame(fullVisitorId=test$fullVisitorId, PredictedLogRevenue=predictions)
sample_sub <- read_csv("sample_submission.csv")
names(submission) <- names(sample_sub)

submission <- submission  %>% group_by(fullVisitorId)  %>% summarise(PredictedLogRevenue = sum(PredictedLogRevenue))
nrow(submission) == nrow(sample_sub)
write.csv(submission, "ga_pred.csv",row.names=T) #Writing it into a new CSV file
class(submission)

table(submission$fullVisitorId==sample_sub$fullVisitorId)
