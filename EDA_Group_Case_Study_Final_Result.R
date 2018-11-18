## EDA group Case Study
# members - Pradnya Paithankar, Dr. RAjendra Warke, VAibhav Palkar, Sameer Sinha

library(ggplot2)
library(stringr)
library(lubridate)
library(dplyr)
library(tidyr)
library(scales)
library(corrplot)
options(max.print = 100000)
options(scipen = 999)

## Read Data----------------------

loan <- read.csv("loan.csv", header = TRUE, stringsAsFactors = TRUE, na.strings = "")
View(loan)
str(loan)

## Data cleaning----------------
duplicate_values <- loan[duplicated(loan),] # No duplicate records

# na_values
na_values <- is.na(loan)
summary(na_values)

# Removing all columns with single factors
loan <- loan[,sapply(loan, nlevels)!= 1]

# Removing all columns with single values
loan <- loan[, -as.vector(which(sapply(loan, function(x) length(unique(x))==1)))]
str(loan)


# By looking at structure we found that 3 columns having zero and NA as 2 levels so removing these columns
# Lets check columns with NA and 0 values only
loan <- loan[, !(names(loan) %in% c("collections_12_mths_ex_med", "chargeoff_within_12_mths", "tax_liens"))]
str(loan)

# check unique values in id column
no_of_uniques_id <- unique(loan$id)
length(no_of_uniques_id) # every id is unique

# check unique values in member id column
no_of_uniques_member_id <- unique(loan$member_id)
length(no_of_uniques_member_id) # every member id is unique

# Remove '%' sign from int_rate column
loan$int_rate <- str_replace_all(loan$int_rate, "[%]", "")
loan$int_rate <- as.numeric(loan$int_rate)

# Remove '%' sign from revol_util column
loan$revol_util <- str_replace_all(loan$revol_util, "[%]", "")
loan$revol_util <- as.numeric(loan$revol_util)

# anual income to integer value
loan$annual_inc <- as.integer(loan$annual_inc)

## Preliminary Data analysis--------------------
# Loan status count
# Plot for Loan Status vs Total Count
ggplot(loan, aes(x = loan_status, fill = loan_status)) + geom_bar() + 
  labs(x="Loan Status", y="Count of Record") + ggtitle("Loan Status vs Total Count")

loan_status_fully_paid <- subset(loan, loan$loan_status == "Fully Paid")
nrow(loan_status_fully_paid)
# 32950 candidate who are fully paid there loan

# loan status is current
loan_status_current <- subset(loan, loan$loan_status == "Current")
nrow(loan_status_current)
# 1140 candidate whose tenure of the loan is not yet completed

# loan status is charged off/defaulted
loan_status_charged_off <- subset(loan, loan$loan_status == "Charged Off")
nrow(loan_status_charged_off)
# 5627 candidate are defaulter

# Plot for Loan Status vs Percent by amount
ggplot(loan, aes(x = loan_status, y=loan_amnt/sum(loan_amnt), fill=loan_status)) + geom_col() + 
  scale_y_continuous(labels = percent) + labs(x="Loan Status", y="Percent by amount") + ggtitle("Loan Status vs Percent by amount")

# Lets seggregate the Charged off and fully paid
loan_closed <- subset(loan, loan$loan_status == "Fully Paid" | loan$loan_status == "Charged Off")
nrow(loan_closed)

# Removing columns having having single unique values as it dont have any impact
loan_closed <- loan_closed[, -as.vector(which(sapply(loan_closed, function(x) length(unique(x))==1)))]

## Understanding Impact of different variable's for defaulter loan----------

## Variables having impact
# Impact of term column
ggplot(loan_closed, aes(x=term, fill=loan_status)) + geom_bar(position = "fill") + scale_y_continuous(labels = percent) + 
  labs(x="Term", y="Percent by Count") + ggtitle("Term vs Percent by Count")
# 60 months term has more % of defaulters

# Impact of interest rate
ggplot(loan_closed, aes(x=int_rate, fill=loan_status)) + geom_histogram(binwidth = 1, position = "fill", col="black") + 
  scale_y_continuous(labels = percent) + labs(x="Interest Rate", y="Percent by Count") + ggtitle("Interest Rate vs Percent by Count")
# Higher interest rate's have more % of defaulters

# Impact of grade
ggplot(loan_closed, aes(x=grade, fill=loan_status)) + geom_bar(position = "fill", col="black") + 
  scale_y_continuous(labels = percent) + labs(x="Grade", y="Percent by Count") + ggtitle("Interest Rate vs Percent by Count")
# Lower grades have more % of defaulters

# Relation between grade and interest rate
ggplot(loan_closed, aes(x=grade, y=int_rate)) + geom_point(aes(col=loan_status)) + labs(x="Grade", y="Interest Rate") + ggtitle("Grade vs Interest Rate")
# It confirm that lending rate inversly proportional to grade

# impact of emp_legnth
ggplot(loan_closed, aes(x=emp_length, fill=loan_status)) + geom_bar(position = "fill") + scale_y_continuous(labels = percent) + 
  labs(x="Employment Length", y="Percent by Count") + ggtitle("Employment Length vs Percent by Count") + theme(axis.text.x = element_text(face="bold", size=7, angle=90))
# Employement length mention has more % of defaulters

# Impact of purpose
ggplot(loan_closed, aes(x=purpose, fill=loan_status)) + geom_bar(position = "fill") + 
  scale_y_continuous(labels = percent) + labs(x="Purpose", y="Percent bCount")  + ggtitle("Purpose vs Percent by Count") +
  theme(axis.text.x = element_text(face="bold", size=10, angle=45))
# Few purpose;s like small buisness, debt-consolidation, educational have more % of defaulters

# Impact of address state
# for better analysis plot of count as well as % distribution ploted
ggplot(loan_closed, aes(x=addr_state, fill=loan_status)) + geom_bar(position = "dodge") + 
  labs(x="State", y="Count")  + ggtitle("State vs Count") +
  theme(axis.text.x = element_text(face="bold", size=10))

ggplot(loan_closed, aes(x=addr_state, fill=loan_status)) + geom_bar(position = "fill") + 
  scale_y_continuous(labels = percent) + labs(x="State", y="Percent by Count") + ggtitle("State vs Percent by Count") +
  theme(axis.text.x = element_text(face="bold", size=10))
# CA has high number of defaulters but NE has high % of defaulters

# Impact of recoveries
ggplot(loan_closed, aes(x=recoveries, fill=loan_status)) + geom_histogram()
# zooming this graph for better understanding with defining new data frame less recovery

less_recovery <- subset(loan_closed, loan_closed$loan_status == "Charged Off")
nrow(less_recovery)
ggplot(less_recovery, aes(x=recoveries)) + geom_histogram(binwidth = 1, fill="red") + 
  labs(x="Recoveries", y="Count") + ggtitle("Recoveries vs Count") + coord_cartesian(xlim = c(0, 50))

# Impact of annual income
# for better analysis plot of count as well as % distribution ploted
ggplot(loan_closed, aes(x=annual_inc, fill=loan_status)) + geom_histogram(binwidth = 25000, position = "fill", col="black") + 
  coord_cartesian(xlim = c(0, 300000)) + labs(x="Annual Income", y="Count") + ggtitle("Annual Income vs Count")

ggplot(loan_closed, aes(x=annual_inc, fill=loan_status)) + geom_histogram(binwidth = 25000, position = "dodge", col="black") + 
  coord_cartesian(xlim = c(0, 300000)) + labs(x="Annual Income", y="Count") + ggtitle("Annual Income vs Count")
# lowe income customer's have high number of bank defaulters

# Impact of open account
# for better analysis plot of count as well as % distribution ploted
ggplot(loan_closed, aes(x=open_acc)) + geom_bar(aes(fill=loan_status), position = "dodge", col="black") +
  labs(title="Open Account vs Count", x="Open Account", y="Count")

ggplot(loan_closed, aes(x=open_acc)) + geom_bar(aes(fill=loan_status), position = "fill", col="black") +
  labs(title="Open account vs Count", x="Open Account", y="Count") + scale_y_continuous(labels = percent)
# open account numbers 2,3,4 have more defaulters

## Variables not having significant/visible impact
# box plot for loan status
ggplot(loan_closed, aes(x=loan_status, y=loan_amnt, fill=loan_status)) + geom_boxplot() + 
  labs(x="Loan Status", y="Loan amount") + ggtitle("Loan Status vs Loan Amount")


# Impact of home ownership
ggplot(loan_closed, aes(x=home_ownership, fill=loan_status)) + geom_bar(position = "dodge") + 
  labs(x="Home Ownership", y="Count") + ggtitle("Home Ownership vs Count") + theme(axis.text.x = element_text(face="bold", size=7, angle=90))

# Impact of verification status
ggplot(loan_closed, aes(x=verification_status, fill=loan_status)) + geom_bar(position = "fill") + scale_y_continuous(labels = percent) + 
  labs(x="Verification Status", y="Percent by Count") + ggtitle("Verification Status vs Percent by Count") + theme(axis.text.x = element_text(face="bold", size=7, angle=90)) + 
  theme(axis.text.x = element_text(face="bold", size=12, angle=45))

# Impact of delinq_2yrs
# for better analysis plot of count as well as % distribution ploted
ggplot(loan_closed, aes(x=delinq_2yrs, fill=loan_status)) + geom_bar(position = "dodge") + 
  labs(x="Delinquency 2 Years", y="Count") + ggtitle("Delinquency 2 Years vs Count") + theme(axis.text.x = element_text(face="bold", size=7, angle=90))

ggplot(loan_closed, aes(x=delinq_2yrs, fill=loan_status)) + geom_bar(position = "fill") + scale_y_continuous(labels = percent) +
  labs(x="Delinquency 2 Years", y="Count") + ggtitle("Delinquency 2 Years vs Count") + theme(axis.text.x = element_text(face="bold", size=7, angle=90))


#Bivariate analysis
# Adding derived metric - default flag
loan_closed$default_flag <- sapply(loan_closed$loan_status, FUN = function(x){if(x == "Fully Paid") {0} else{1}})

# TAking out all the numeric columns from the dataframe
col_num <- unlist(lapply(loan_closed, is.numeric))
loan_numeric <- loan_closed[, col_num]

#-------------
# CORRELATION MATRIX
#PLOT : CORRELATION ANALYSIS GRID FOR QUANTITATIVE VARIABLES
# create dataframe with only quantitative variables including default status column as binary variable
#create correlation matrix
correl_calc <- round(cor(loan_numeric,method= "spearman",use="pairwise.complete.obs"),1)
#configure color combination for the plot

col <- colorRampPalette(c("red","white","blue"))(20)
#plot correlation matrix
corrplot(correl_calc,method="color",tl.cex = 0.8,tl.col = "red",col=col,
         number.cex = 0.5,order = "FPC",type="upper",addCoef.col = "white",diag = F)
#--------------








