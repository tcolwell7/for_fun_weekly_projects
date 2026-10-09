# Dummy data to practice transaction style data in R

# R practice script for date event data

options(scipen=999) # turn off scientific numerical notation

`%notin%` <- Negate(`%in%`) # custom function for filtering data

# Specify packages
packages <-
  c("tidyverse","janitor","stringr","data.table",# general data wrangling
    "readxl","readr",
    "lubridate"
  ) 

# Install packages if not already installed
install.packages(setdiff(packages, rownames(installed.packages())))

# Load packages
sapply(packages, require, character.only = TRUE)

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


# load data

transactions <- read_csv("transactions.csv") 
product_events <- read_csv("product_events.csv")
customers <- read_csv("customers.csv")
accounts <- read_csv("accounts.csv")


#' Practice script to go through problems relating to transactional data 
#' for a financial product company
#' using SQL style language and step by step process i.e. long format, step by step, CTEs etc.
#' Set up to do simple stpe by step through problems
#' Explaining logic and why I'm doing specific steps - to then be challenged etc 
#' 

# Q1 --------
# Find every customer who has signed up but has not opened an account. 

# Step 1: Check table schemas for details, data types etc 
# Reason: check contents, pre cursor for any join issues (data type) check event details etc

# i.e. Equiv of SELECT *
customers

# i.e. check distinct row count - incase of dups
length(unique(customers$customer_id))

# i,e, check distinct event type SELECT DISTINCT event_type FROM product_events ...
product_events
unique(product_events$event_type)

# Step 2: Match customer data to event data via LEFT JOIN - will expand table row count (expected)
# Reason: need event data per customer, so can identify accounts who have signup_completed but no account_opened 

# i.e. equiv: SELECT * FROM C LEFT JOIN P ON c.customer_id = p.customer_id 
customers_event <- customers %>%
  left_join(product_events, by = "customer_id")

# I would check data contents and row count incase anything looks off (it doesn't)

# this is a quick check for the filter - equiv of SELECT count(event_type) GROUP_BY event_type)
# logic is - every customer id in table starts with signup_completed - so simply filter where <> "account_opened"

product_events %>% group_by(event_type) %>% tally()


account_openers <- product_events %>%
  filter(event_type == "account_opened") %>%
  distinct(customer_id)

customers_event <- customers %>%
  left_join(product_events, by = "customer_id") %>%
  filter(customer_id %notin% account_openers$customer_id)

# Step 3: find unique list of customer_ids
# simple equiv to select distinct 
customer_id_not_open <- customers_event %>%
  select(customer_id) %>%
  distinct()


# Step 3+: if looking for a reason in this data - look at what other events are present
# Reason: check if there is a trend of people sign up, kyc but no deposit started etc
# which would require a general countof each event id to see a trend or not


customer_id_event_type <- customers_event %>%
  group_by(event_type) %>%
  tally()

# i.e. - again simple aggregation using GROUP BY AND COUNT

# Step 4: check drop out issue with KYC

kyc_dropout <- customers_event %>%
  filter(event_type == "kyc_started",event_status == "abandoned")

# i.e. equiv of simple CTE table for join, then filter that CTE with that logic
# end result full customer table who started kyc but abandoned > which can be used to investigate the issue

# End Q1.

--
  
# Q2 ----------
# For each month, show the number of customers who made:
# at least one successful deposit, 
# the total value of successful deposits, 
# and the average successful deposit value.

# Step 1: investigate customer and transaction data, 
# Reason: data types for joins, transaction type data to check for deposits etc - before calculating figures

# have unique customer id in customers
# have all customer ids with transactions in transactions so only need to use this table 
# unless want wider info for further analysis
# as unique table - can join at the end if needed

glimpse(transactions)
unique(transactions$transaction_type)

# Step 2: filter and aggregate data having looked through/checked data
# Reason: simple step by step process once data structure and contents is known, filter, aggregate count, sum, average

transactions_deposits <- transactions %>%
  filter(transaction_type == "deposit", status == "successful") %>%
  group_by(customer_id) %>%
  summarise(
    deposit_count = length(transaction_id), 
    deposit_total_value = sum(amount, na.rm = TRUE),
    deposit_total_avg   = mean(amount, na.rm = TRUE)
  )

# simple SQL logic.. SELECT COUNT(),SUM(),MEAN(), FROM, FILTER (&), GROUP BY col etc..

# Step 3: Having looked at overall - break data further by month, first need to format data/extract month
# Reason: ensure correct formatting and group by column so can answer specific question..

transactions_deposits_mon <- transactions %>%
  mutate(month = lubridate::month(transaction_date)) %>%
  filter(transaction_type == "deposit", status == "successful") %>%
  group_by(customer_id, month) %>%
  summarise(
    deposit_count = length(transaction_id), 
    deposit_total_value = sum(amount, na.rm = TRUE),
    deposit_total_avg   = mean(amount, na.rm = TRUE)
  )
    
# now we have the data we wanted, but larger and harder to make sense of. 
# a sensible next step to make sense of this is use table 2 - find top ranking customers (i.e. top 10)
# filter monthly data
# or conversely, may wish to look at lowest deposit-ting customers for focused marketing etc.

# Step 4: Find top ranked customers based on total deposit
# Reason: as above ^ - simple arrange and filter based on top N customer ids
# total deposit is more useful than average, unless determining a baseline amount of deposits, as one large deposit skews the figures etc.


transactions_deposits_top <- transactions_deposits %>%
  arrange(desc(deposit_total_value)) %>%
  slice(1:10)

transactions_deposits_mon <- transactions_deposits_mon %>%
  filter(customer_id %in% transactions_deposits_top$customer_id)


# now we have the top 10 customers by total deposits by value - monthly data which would help in any specific search

# SQL equiv would be simple using a CTE or sub-query
# i.e. WITH top_10 AS (SELECT customer_id FROM ORDER BY val LIMIT N) .. SELECT * FROM table WHERE customer_id IN (SELECT customer_id from top_10)


# Step 5: added - find monthly data total deposits, total value and average - per month to highlight trends
# further simple aggregations - based on correct formatting of dates

transactions_deposits_monthly <- transactions %>%
  mutate(date_monthly = floor_date(transaction_date, unit = "month")) %>%
  filter(transaction_type == "deposit", status == "successful") %>%
  group_by(date_monthly) %>%
  summarise(
    deposit_count = length(transaction_id), 
    deposit_total_value = sum(amount, na.rm = TRUE),
    deposit_total_avg   = mean(amount, na.rm = TRUE)
  )

# simple SQL > create CTE with formatted data/filtered data >> simple GROUP BY >> then can arrnage depending what you are looking for.


# Q3 ------
# Find the latest successful transaction for each customer.

# Step 1: investigate data - expectaiotn multi transactions on same dates
# Reason: likely multiple rows per date, look into data, to decide on how to handle this is 1 row per customer required

multi_transactions <- transactions %>%
  arrange(customer_id, transaction_date) %>%
  group_by(customer_id, transaction_date) %>%
  mutate(count = n())

# quick highlights multiple transactions on same dates, i.e. multip deposits
# so end data woud require aggregation
# for this question only interested in successful deposits or withdrawals

# Step 2: aggregate data based on id, date, type - to aggregate daily figures as no timestamp included. 
# Reason: excluding transaction ids as a proxy for date, no time stamps, so can't really find "latest"" transaction

customer_transactions_daily <- transactions %>%
  filter(status == "successful") %>%
  group_by(customer_id, transaction_date, transaction_type) %>%
  summarise(amount = sum(amount, na.rm = TRUE)) %>%
  arrange(customer_id, transaction_date)


# Step 3: Now have agg data - so can use latest date and values using group by

customer_transactions_latest <- customer_transactions_daily %>%
  group_by(customer_id, transaction_type) %>%
  filter(transaction_date == max(transaction_date)) %>%
  ungroup() %>%
  arrange(customer_id, transaction_date)

# SQL is simple CTE and aggregation using max on date column 

# Now we have latest date of successful transaction - 
# data includes multiple types across the same date - so data either needs time stamps or is fine for this purpose.


# Q4: ----------
# For each month, calculate the successful deposit rate: the number of successful deposits divided by the total number of deposit attempts.

# Step 1: investigate data, derived formula etc
# data types, what is required and formula... successful deposit rate = depsoit successful / deposit total 

monthly_deposits <- transactions %>%
  mutate(date_monthly = floor_date(transaction_date, unit = "month")) %>%
  filter(transaction_type == "deposit") %>%
  group_by(date_monthly, status) %>%
  summarise(amount = n())




# Q5: ---------
# Using the transactions table, show monthly successful deposit value and the change from the previous month.

# Step 1: investigate data/convert date column, filter for successful deposits only
# Reason: date type ready for aggregation, select filtered data interested in. 

transactions_deposits <- transactions %>%
  mutate(date_monthly = floor_date(transaction_date, unit = "month")) %>%
  filter(transaction_type == "deposit", status == "successful") 

# Step 2: Simple aggregation on monthly date time - then group/partition over month using lag for previous monthly value
# Reason, multi rows per month for successful deposits, need single row per month

transaciton_deposits_monthly <- transactions_deposits %>%
  group_by(transaction_type, status, date_monthly) %>%
  summarise(monthly_amount = sum(amount), .groups = 'drop') %>%
  arrange(desc(date_monthly)) %>%
  mutate(previous_month_val = lead(monthly_amount, 1)) %>% 
  mutate(monthly_change = monthly_amount - lead(monthly_amount, 1))

# Now have a monthly dataset with previous month chnage + or - for successful deposits for further trend analysis

## SQL equiv: can simple follow same process using CTE for formatted/filtered/grouped monthly data then find the LEAD value

    

# Q6: ----------
# Return each customer’s latest successful deposit, including the date and amount.

# Step 1: investigate data, decide on table / select data i.e. filter successful deposits, end goal is one row per customer
# Reason: simple data prep for next step of finding latest deposit/ordering data. 

latest_customer_deposit <- transactions %>%
  filter(transaction_type == "deposit", status == "successful")

# Step 2: need to group data by customer and order for latest amount
# Reason: transaciton id, highest value as proxy for latest if multipel deposits on same day.

latest_customer_deposit <- latest_customer_deposit %>%
  group_by(customer_id) %>%
  arrange(customer_id,desc(transaction_date), desc(transaction_id)) %>%
  mutate(n = row_number()) %>%
  filter(n == 1)

# SQL equiv:

#' WITH cte_table AS CTE(
#' SELECT
#' *,
#' ROW_NUMBER() OVER
#'   PARTITTION customer_id
#'   ORDER BY ransaction_date DESC, transaction_id DESC
#'   As latest_deposit_row
#' FROM
#' FILTER AND
#' )
#' 
#' SELECT
#' *
#' FROM
#' cte_table
#' FILTER latest_deposit_row = 1
#' 
  

# Q7: --------
#' identify customers whose monthly successful deposit amount increased compared with the previous month.

# Step 1: filter data and format month date for aggregation to month level

customer_successful_deposits <- transactions %>% 
  filter(transaction_type == "deposit", status == "successful") %>%
  mutate(date_monthly = floor_date(transaction_date, unit = "month"))

# Step 2: aggregate data to month before finding monthly varinaces

customer_successful_deposits_monthly <- customer_successful_deposits %>%
  group_by(customer_id, transaction_type, date_monthly) %>%
  summarise(monthly_amount = sum(amount))

# Step 3: Order data by month to find previous month values

customer_successful_deposits_monthly <- customer_successful_deposits_monthly %>%
  group_by(customer_id) %>%
  arrange(date_monthly, .by_group = TRUE) %>%
  mutate(previous_month = monthly_amount - lag(monthly_amount)) %>%
  mutate(monthly_increase = ifelse(previous_month > 0, 1, 0))

#' SQL equiv:
#' 
#' WITH cte_one AS(
#' SELECT * (also DATE FORMATTING SYNTAX)
#' sum(amount)
#' FROM
#' transactions
#' WHERE
#' GROUP BY
#'  same grouping cols)
#' 
#' SELECT
#' *,
#' LAG(monthly_amount) OVER
#' PARTTITION BY customer_id
#' ORDER BY date_monthly
#' AS previous_month,
#' 
#' CASE WHEN monthly_amount - previous_amount > 1, 1
#' ELSE 0
#' END AS monthly_increase
#' FROM cte_one
  
  

