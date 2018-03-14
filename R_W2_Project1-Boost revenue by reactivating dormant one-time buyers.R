# R version 3.4.0 (2017-04-21)
install.packages("data.table")
install.packages("sqldf")
install.packages('doMC')
install.packages('glmnet')

path ='~/Downloads'
setwd(path)

library(data.table)
library(sqldf)
library(doMC)
library(glmnet)
options(scipen=999)

customer_table <- fread("customer_table.csv")
order_table <- fread("order_table.csv")
product_table <- fread("product_table.csv")
category_table <- fread("category_table.csv")

#### to change from scientific notation to the actual number
customer_table[,customer_id := as.character(customer_table$customer_id)]
order_table[,customer_id := as.character(order_table$customer_id)]
order_table[,order_id := as.character(order_table$order_id)]
order_table[,product_id := as.character(order_table$product_id)]
product_table[,product_id := as.character(product_table$product_id)]

## find customers who only made one purchase before 2016/12/22
base <- subset(
  order_table[
    order_date<'20161222' & order_amount > 0,
    .(count=.N
      ,order_date=max(order_date)
      ,order_amount=max(order_amount)
      ,product_id=max(product_id)),by=customer_id],
  count==1)

## find customers who only made purchase between 2016/12/22 and 2017/02/22
purchase_again <- sqldf("SELECT customer_id
                        , MAX(order_date) AS latest_orderdate
                        FROM order_table
                        WHERE order_date BETWEEN '20161222' and '20170222'
                        AND order_amount > 0
                        GROUP BY 1");

## find customers who were dormant between 2016/12/22 and 2017/02/22
dormant_3month <- sqldf("SELECT * 
                 FROM base 
                 WHERE customer_id NOT IN 
                 (SELECT customer_id FROM purchase_again);")

## find customers who purchased again between 2017/02/23 and 2017/05/22
purchase_flag <- sqldf("SELECT customer_id
                        , MAX(order_date) AS latest_orderdate
                        FROM order_table
                        WHERE order_date BETWEEN '20170223' and '20170522' 
                        AND order_amount > 0
                        GROUP BY 1");

is_converted <- sqldf("SELECT d.customer_id,
                      CASE WHEN pf.customer_id IS NOT NULL THEN 1
                       ELSE 0 END AS is_converted 
                      FROM dormant_3month d
                      LEFT JOIN purchase_flag pf
                      ON d.customer_id = pf.customer_id
                      ;")

user_features <- sqldf("SELECT ct.*
                      ,ic.is_converted
                     FROM customer_table ct
                     INNER JOIN is_converted ic
                        ON ct.customer_id = ic.customer_id");

### fill all NA's with 0
user_features[is.na(user_features)] <- 0

### remove all categorical variables, for now
user_features$customer_id <- NULL
user_features$country <- NULL
user_features$gender <- NULL
user_features$first_visit_date <- NULL
user_features$last_visit_date <- NULL

train_x <- subset(user_features,select=-c(is_converted))
train_y_categorical <- ifelse(train_y == 1, 'YES', 'NO') 

### let's train the model!
registerDoMC(cores=6)

model_lasso <- cv.glmnet(as.matrix(train_x),train_y_categorical,alpha=1
                         ,family="binomial",type.measure = "auc",parallel = TRUE)
