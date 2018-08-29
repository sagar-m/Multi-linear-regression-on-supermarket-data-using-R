setwd("/Volumes/KINGSTON/Aquila/")
getOption("scipen") 
options("scipen"=10) 



library(data.table)
data <- fread("aquila.csv")

str(data)


data$Region <- as.factor(data$Region)
data$Gender <- as.factor(data$Gender)
data$Account_Type <- as.factor(data$Account_Type)
data$Merchant <- as.factor(data$Merchant)
data$Savings_AC <- as.factor(data$Savings_AC)
#data$Spend <- as.numeric(data$Spend)
#data$Annual_Salary <- as.numeric(data$Annual_Salary)
data$Transactions <- as.numeric(data$Transactions)
data2$Age <- as.numeric(data2$Age)
data$Customers <- as.numeric(data$Customers)

library(lubridate)
library(zoo)

data$Date <- as.yearmon(data$Date)
data$Date <- as.Date(paste(data$Date,"-01",sep=""))

data$Mon <- format(data$Date,"%m")
data$Year <- format(data$Date,"%Y")

## dplyr

data %>%
group_by(Merchant, Account_Type) %>%
summarise(avg_spend = mean(Spend),
trx_avg = mean(Transactions),
avg_sal = mean(Annual_Salary, na.rm=T))





######
summary(data)
tail(data$Mon)
tail(data$Date)

library(ggplot2)
library(ggthemes)
library(dplyr)

ggplot(data, aes(x = Merchant, fill= Customers)) +
  geom_histogram(stat = "count", binwidth = 0.3, position = "stack")


ggplot(data, aes(x = Merchant, fill= Gender)) +
  geom_histogram(stat = "count", binwidth = 0.3, position = "stack")


#barplot

d <- ggplot(data, aes(Merchant,Transactions)) + scale_y_continuous("Transactions", limits = c(0, 100),   breaks = seq(0, 100, 10),   expand = c(0, 0)) +
  scale_x_discrete("Iodine") +
  theme_classic()

d + 
  stat_summary(fun.y = mean, geom = "bar",   fill = "grey50")


library(GGally)

ggpairs(
  data, columns = c(6,8,13), ggplot2::aes(color = Merchant),
  upper = list(continuous = wrap("density", alpha = 0.5), combo = "box_no_facet"),
  lower = list(continuous = wrap("points", alpha = 0.3), combo = wrap("dot_no_facet", alpha = 0.4)),
  columnLabels = c("Customers", "TXN","Annual_Salary")
)

ggpairs(data, columns = c(6,7,10),ggplot2::aes(color = Merchant))


ggpairs(data, columns = c(5,8,13),ggplot2::aes(color = Merchant))

#timeline
ggplot(data, aes(x = Date, y = Spend)) +
geom_point(position = "stack") +
facet_grid(Merchant ~ .)



#faceted histogram, no

ggplot(data, aes (x = Annual_Salary, fill= factor(Spend))) + 
  geom_histogram() +
        facet_grid(Merchant ~ .) +
        theme_classic() +
        theme(legend.position = "bottom") +
        labs(x = "Annual_Salary") +
        labs(fill='Spend') 


### geom point plot

ggplot(data, aes(x = Customers, y = Annual_Salary, colour=Account_Type, size = Spend)) +
geom_point(alpha = 0.3) +
facet_grid(Merchant ~ .)
theme_classic() +
theme(legend.position = "below")

ggplot(data, aes(x = Customers, y = Spend, colour=Account_Type, size = Annual_Salary)) +
geom_point(alpha = 0.3) +
facet_grid(Merchant ~ .)
theme_classic() +
theme(legend.position = "below")

library(dplyr)
library(corrplot)

## corrplot

na.omit(data) %>% select_if(is.numeric) %>% # select(-Date) %>%  cor() %>%  corrplot()
  
# Frequent storesggplot(data) +    geom_boxplot(aes(x = Merchant, y = Spend/Customers)) +
    scale_y_continuous(limits = c(0, 200)) +
    facet_grid(~Year) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    theme(legend.position = "none")
 

    
   # Preferred brandggplot(data) +    geom_boxplot(aes(x=interaction(Account_Type, Region), y = Spend, fill = interaction(Account_Type, Region))) +
    facet_wrap(~Merchant) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    theme(legend.position = "none")
 
    
ggplot(data) +
    geom_boxplot(aes(x=interaction(Account_Type, Region), y = Transactions, fill = interaction(Account_Type, Region))) +
    facet_wrap(~Merchant) +
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1)) +
    theme(legend.position = "none")

## histogram

ggplot(data, aes(x=Spend)) +
geom_histogram(col = 'white', bins=50) +
facet_wrap(~Merchant) +
theme_light() +
scale_x_continuous()


ggplot(data, aes(x=Transactions)) +
geom_histogram(col = 'white') +
facet_wrap(~Merchant) +
theme_light() +
scale_x_continuous()


ggplot(data, aes(x=Annual_Salary)) +
geom_histogram(col = 'white', bins=50) +
facet_wrap(~Merchant) +
theme_light() +
scale_x_continuous()

##

library(MASS)

ggplot(data, aes(x=sqrt(Spend))) +
geom_histogram(col = 'white',bins=40) +
facet_wrap(~Merchant) +
theme_light() +
scale_x_continuous()

ggplot(data, aes(x=log(Spend+1))) +
geom_histogram(col = 'white',bins=50) +
facet_wrap(~Merchant) +
theme_light() +
scale_x_continuous()



ggplot(data, aes(x=sqrt(Transactions))) +
geom_histogram(col = 'white',bins=50) +
facet_wrap(~Merchant) +
theme_light() +
scale_x_continuous()


ggplot(data, aes(x=log(Annual_Salary+1))) +
geom_histogram(col = 'white',bins=50) +
facet_wrap(~Account_Type) +
theme_light() +
scale_x_continuous()

ggplot(data, aes(x=Spend/Customers)) +
geom_histogram(col = 'white',bins=70) +
facet_wrap(~Account_Type) +
theme_light() +
scale_x_continuous(limits = c(0, 500))




### timeline

data %>%
group_by(Merchant, Year, Mon)%>%
summarise(spend_avg = mean(Spend),
		txn_avg = mean(Transactions),
		total = n())


data %>%
group_by(Merchant, Date)%>%
summarise(spend_avg = mean(Spend)) %>%
ggplot(aes(x = Date, y = spend_avg)) +
geom_line() +
facet_grid(~Merchant) +
theme_classic()

### density

data %>%
group_by(Merchant, Date)%>%
summarise(spend_avg = mean(Spend)) %>%
ggplot(aes(x = Date, y = spend_avg)) +
geom_line(aes(x = x), stat = 'density', size = 1,alpha = 1.0) +
facet_grid(~Merchant) +
theme_classic()


ggplot(data, aes(x = Date, y = mean(Spend))) +
geom_line(stat = 'density', size = 1,alpha = 1.0) +
facet_grid(~Merchant) +
theme_classic()

## pairs

pairs(~sqrt(Spend)+ Savings_AC+log(Annual_Salary)+ Account_Type, data=data, main="Simple Scatterplot Matrix")




## Standardizing data data

data2 <- data

str(data2)

data$Mon <- as.factor(data$Mon)
data$Year <- as.factor(data$Year)

#data2 <- data %>% mutate_at(funs(scale(.) %>% as.vector), vars=c("Customers","Spend","Transactions", "Age","Annual_Salary"))

data2 <- data %>% mutate_at(vars(6:10), funs(scale(.) %>% as.vector))

summary(data2)

### splitting the data

## 75% of the sample size
smp_size <- floor(0.75 * nrow(data2))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(data2)), size = smp_size)

train <- data2[train_ind, ]
test <- data2[-train_ind, ]

nrow(train)
nrow(test)




###multi-linear regression

library(rms)
library(caret)


head(data)

sales1 <- lm(Spend ~., data = train)

summary(sales1)
vif(sales1)
print(AIC(sales1))

str(data2)

sales2 <- lm(Spend ~. -Date-Gender-Mon-Customers + 0, data = train)

summary(sales2)
vif(sales2)
print(AIC(sales2))
par(mfrow=c(2,2))
plot(sales2)
#plot(residuals(sales2))



sales3 <- lm(Spend ~ Savings_AC+ Merchant*Account_Type + Age + Transactions+Mon+Year+I(Transactions^2)+Region*Transactions+0, data = train)

summary(sales3)
vif(sales3)
par(mfrow=c(2,2))
plot(sales3)
print(AIC(sales3))


## predictions
pred3 <- predict(sales3, test)
output3 <- cbind(test, pred3)

err3 <- output3$pred3 - output3$Spend
error3 <- err3^2

rss3 <- sum(error3)
rmse3 <- sqrt(mean(error3))

print(rss3)
print(rmse3)

ggplot(output3, aes(x = pred3, y = Spend)) + 
    geom_point() +
    geom_abline(color = "blue")



sales5 <- lm(Spend ~ Account_Type + Age + Transactions+Mon+Year+I(Transactions^2)+Region*Transactions, data = train)

#anova(sales3)
#contrasts(data2$Merchant)

summary(sales5)
vif(sales5)
par(mfrow=c(2,2))
plot(sales5)
print(AIC(sales5))

## predictions
pred5 <- predict(sales5, test)
output5 <- cbind(test, pred5)

err5 <- output5$pred5 - output5$Spend
error5 <- err5^2

rss5 <- sum(error5)
rmse5 <- sqrt(mean(error5))

print(rss5)
print(rmse5)



ggplot(output5, aes(x = pred5, y = Spend)) + 
    geom_point() +
    geom_abline(color = "blue")

#t.test(mse~model, data=cv_pred_error(sales3,sales5))

anova(sales1,sales2)
anova(sales5,sales3)


#data2$Predictions <- predict(sales5)
#ggplot(data2, aes(x = Predictions, y = Spend)) + 
 #   geom_point() +
  #  geom_abline(color = "blue")



sales4 <- lm(Spend ~. -Age-Annual_Salary-Gender-Customers-1, data = data2)

predict(sales4)

summary(sales4)
vif(sales4)

anova(sales4, sales3,sales2)

confint(sales4)

predict(sales4, data.frame(Transactions = c(100,1000,10)), interval = "confidence")


par(mfrow=c(2,2))
plot(predict(sales4), residuals(sales4))
plot(predict(sales4), rstudent(sales4))




# deleting outliers

data2 <- data2[-c(570,833,1080,1162,1527,2104), ]
#570, 833, 1527, 1080
#2104, 1162, 1080
# 780, 1154, 1737



car::outlierTest(sales3)

print(data[c(570,833,1080,1162,1154,780,2104,2111,1134,1098), ])


##

outlierKD <- function(dt, var) {
     var_name <- eval(substitute(var),eval(dt))
     na1 <- sum(is.na(var_name))
     m1 <- mean(var_name, na.rm = T)
     par(mfrow=c(2, 2), oma=c(0,0,3,0))
     boxplot(var_name, main="With outliers")
     hist(var_name, main="With outliers", xlab=NA, ylab=NA)
     outlier <- boxplot.stats(var_name)$out
     mo <- mean(outlier)
     var_name <- ifelse(var_name %in% outlier, NA, var_name)
     boxplot(var_name, main="Without outliers")
     hist(var_name, main="Without outliers", xlab=NA, ylab=NA)
     title("Outlier Check", outer=TRUE)
     na2 <- sum(is.na(var_name))
     cat("Outliers identified:", na2 - na1, "n")
     cat("Propotion (%) of outliers:", round((na2 - na1) / sum(!is.na(var_name))*100, 1), "n")
     cat("Mean of the outliers:", round(mo, 2), "n")
     m2 <- mean(var_name, na.rm = T)
     cat("Mean without removing outliers:", round(m1, 2), "n")
     cat("Mean if we remove outliers:", round(m2, 2), "n")
     response <- readline(prompt="Do you want to remove outliers and to replace with NA? [yes/no]: ")
     if(response == "y" | response == "yes"){
          dt[as.character(substitute(var))] <- invisible(var_name)
          assign(as.character(as.list(match.call())$dt), dt, envir = .GlobalEnv)
          cat("Outliers successfully removed", "n")
          return(invisible(dt))
     } else{
          cat("Nothing changed", "n")
          return(invisible(var_name))
     }
}

outlierKD(data4, Transactions)

data3 <- data
data4 <- data2

##




## scaling

data2 <- data

mc <- mean(data$Customers)

data2$Customers <- (data2$Customers - mc)/(max(data2$Customers)-min(data2$Customers))


ms <- mean(data$Spend)

data2$Spend <- (data2$Spend -ms)/(max(data2$Spend)-min(data2$Spend))

mt <- mean(data$Transactions)

data2$Transactions <- (data2$Transactions - mt)/(max(data2$Transactions) - min(data2$Transactions))

ma <- mean(data$Annual_Salary, na.rm=T)

data2$Annual_Salary <- (data2$Annual_Salary - ma)/(max(data2$Annual_Salary, na.rm=T) - min(data2$Annual_Salary, na.rm=T))

mg <- mean(data$Age)

data2$Age <- (data2$Age - mg)/(max(data2$Age, na.rm=T) - min(data2$Age, na.rm=T))


####


library(dplyr)

summary(data2)
str(data2)

data2 <- data2[complete.cases(data2), ]


data %>%
group_by(Merchant, Year) %>%
summarise(SPC = mean(Spend/Customers),
ATC = mean(Spend/Transactions),
total = n())


data %>%
group_by(Merchant, Savings_AC) %>%
summarise(avg_sp = mean(Spend),
avg_sal = mean(Annual_Salary, na.rm=T),
avg_trx = mean(Transactions),
total = n())


######

data1$lsal <- log(data1$Annual_Salary)
data1$lspend <- log(data1$Spend)

data1 <- data
data1$Annual_Salary <- NULL
data1$Spend <- NULL

summary(data1)

colSums(sapply(data, is.na))

cat("The number of duplicated rows are", nrow(data) - nrow(unique(data)))

####

data2 <- mice(data, m=5, maxit = 50, method = 'pmm', seed = 500)

summary(data)

### computing missing value accuracy is 0.325

library(Hmisc)

#using argImpute
data2 <- aregImpute(~ Annual_Salary + Account_Type + Customers + Spend + Date + Gender + Region + Savings_AC + Merchant + Age +
Transactions, data = data, n.impute = 5)

head(data2)

data2$imputed$Annual_Salary
####


## random forest

data3 <- data2[complete.cases(data2), ]

nrow(data3)
nrow(data2)

### splitting the data

## 75% of the sample size
smp_size <- floor(0.75 * nrow(data3))

## set the seed to make your partition reproducible
set.seed(13)
train_ind <- sample(seq_len(nrow(data3)), size = smp_size)

train1 <- data3[train_ind, ]
test1 <- data3[-train_ind, ]

ncol(train1)
nrow(test1)





library(randomForest)
set.seed(1)
bag.pred <- randomForest(Spend~.,data=train1,mtry=13, importance=TRUE)

bag.pred

yhat.bag <- predict(bag.pred, newdata=test1)

par(mfrow=c(1,1))
plot(yhat.bag, test1$Spend)
abline(0,1)

mean((yhat.bag-test1$Spend)^2)

importance(bag.pred)

rf.pred2 <- randomForest(Spend~.,data=train1,mtry=6, importance=TRUE, ntree=25)

yhat.bag2 <- predict(rf.pred2, newdata=test1)

par(mfrow=c(1,1))
plot(yhat.bag2, test1$Spend)
abline(0,1)

mean((yhat.bag2-test1$Spend)^2)
importance(rf.pred2)
varImpPlot(rf.pred2)


