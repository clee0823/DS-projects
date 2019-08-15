setwd("C:/Users/clfee/Desktop/sales analytics/email")
rm(list=ls())

library(data.table)
library(tidyverse)
library(caret)
library(DMwR)
library(pROC)
library(purrr)
require(dplyr)
require(ggplot2)
library(gridExtra)



open  <- fread('email_opened_table.csv')
tbl   <- fread('email_table.csv')
click <- fread('link_clicked_table.csv')

# data overview
glimpse(open)
glimpse(tbl)
glimpse(click)

# find missing values
# @ comment on missing vales
x <- is.na(tbl)
apply(is.na(tbl), 2, which)

# There are 8226 id that opened but not clicked and 2069 id opend and clicked, 50 id not opend but clicked!


(length(open$email_id) - length(click$email_id))

opened_n_clicked <- intersect(open$email_id,click$email_id)

click_no_open <- sort(setdiff(click$email_id, open$email_id))

# add lable before combine datasets
# @ for open email id, add open column and label 1
# @ for clicked id, add click column and label 1 

open$opend    <- 1
click$clicked <- 1 

# merge 

tot      <- merge(tbl, open, by = 'email_id', all.x = T, all.y = T)
tot      <- merge(tot, click, by = 'email_id', all.x = T, all.y = T)
tot[is.na(tot)] <- 0# fill the missing values
tot$flag        <- as.numeric(tot$clicked>tot$opend)

rm(click, open, tbl,x);gc()

length(unique(tot$email_id)) # check unqie email id


# @ create new var for open & click combination
tot <-tot %>%
   select(colnames(tot)) %>%
   mutate(
      open_click = case_when(
   tot$opend == 1 & tot$clicked == 0 ~ 'onoc',
   tot$opend == 0 & tot$clicked == 1 ~ 'wierd',
   tot$opend == 1 & tot$clicked == 1 ~ 'oandc' ,
   tot$opend == 0 & tot$clicked == 0 ~ 'no_action'
      ))
# @ purchase freuency
summary(tot$user_past_purchases)
hist(tot$user_past_purchases)
tot <-tot %>%
   select(colnames(tot)) %>%
   mutate(
      freq = case_when(
         # bin purchase freq #
         tot$user_past_purchases  == 0 ~ 'inactive',
         tot$user_past_purchases  < 5 & tot$user_past_purchases  > 0 ~ 'active1',
         tot$user_past_purchases  < 10 & tot$user_past_purchases  >= 5 ~ 'active2',
         tot$user_past_purchases  >= 10  ~ 'active3'
      ))

# summary of action 


action.ver     <- data.table(tot %>% group_by(email_version) %>% count(open_click))
action.ver$pct <- 100*(action.ver$n/length(tot[,1])) 

(action.ver)

action.txt     <- data.table(tot %>% group_by(email_text) %>% count(open_click))
action.txt$pct <- 100*(action.txt$n/length(tot[,1])) 

(action.txt)


# data distribution

# @ email version
barplot(prop.table(table(tot$email_version)))
# @ email text
barplot(prop.table(table(tot$email_text)))
# @ action
barplot(prop.table(table(tot$open_click)))


# frequence of action
ggplot(data=tot, aes(x=open_click)) +
   geom_bar(stat="count")

# actions vs email types
p1 <- ggplot(data=tot, aes(x=open_click)) +
      geom_bar(aes(fill=email_text))
p2 <- ggplot(data=tot, aes(x=open_click)) +
      geom_bar(aes(fill=email_version))
p3 <- ggplot(data=tot, aes(x=open_click)) +
      geom_bar(aes(fill=email_text), position = "fill")
p4 <- ggplot(data=tot, aes(x=open_click)) +
      geom_bar(aes(fill=email_version), position = "fill")

grid.arrange(p1, p2, p3, p4, nrow = 2)

# @ click and opened doesnt seem to be defferent in txt and version
# how about the day? is there any synergy afftect btw txt and version

ggplot(data=tot, aes(x=open_click)) +
   geom_bar(aes(fill=user_country))


# need to find the trend for wkday and hour

# variable transformation #
# transform factor to numeric # direct transformation is not working
char.names<-names(tot)
for (c in char.names){
   if(class(tot[[c]])=="character"){
      tot[[c]]<-as.factor(tot[[c]])
      } 
}  
tot$clicked <- as.factor(tot$clicked)

# remove unusal activities
tot  <- data.table(tot)
tot <- tot[tot[,flag==0]]

# train and test data
set.seed(431)
train_sample = sample(nrow(tot), size = nrow(tot)*0.80)
features     = c("user_past_purchases","email_text","email_version","hour","weekday","user_country","freq","clicked")
train_data = tot[train_sample, c("user_past_purchases","email_text","email_version","hour","weekday","user_country","freq","clicked")]
test_data  = tot[-train_sample, c("user_past_purchases","email_text","email_version","hour","weekday","user_country","freq","clicked")]


# model building for imbalance group
ctrl <- trainControl(method = "repeatedcv", 
                     number = 2, 
                     repeats = 2, 
                     verboseIter = T,
                     sampling = "smote")

set.seed(42)
model_rf_smote <- caret::train(clicked ~ .,
                               data = train_data,
                               method = "rf",
                               preProcess = c("scale", "center"),
                               trControl = ctrl)


#
final_smote <- data.frame(actual = test_data$clicked,
                          predict(model_rf_smote, newdata = test_data[,-8], type = "prob"))
final_smote$predict <- ifelse(final_smote$X1 > 0.5, 1, 0)

# confusion metrix
confusionMatrix(as.factor(final_smote$predict), test_data$clicked)
table(final_smote$predict, test_data$clicked)

# calculate auprc

cal_auprc <- function(model, data){
   predictions <- predict(model,data[,-"clicked"], type = "prob") 
   PRcurve(predictions$`1`,data$clicked,
           curve = T)
}
cal_auprc(model_rf_smote, data = test_data)

# immportant variables
imp <- varImp(model_rf_smote)
plot(imp, top = 5)

# plot

library(plotly)
sp <- ggplot(tot, aes(x = user_past_purchases, y= clicked , color = clicked))+ geom_boxplot()
sp + facet_wrap(~ email_text, ncol = 2)
#sp+ facet_grid(rows = (tot$user_past_purchases))
ggplotly()

ggplot(tot, aes(x = user_past_purchases, y = posttest, fill = clicked)) + 
   geom_boxplot()

ggplot(tot, aes(x = sum(user_past_purchases), y = clicked, fill = weekday)) + 
   geom_boxplot()
