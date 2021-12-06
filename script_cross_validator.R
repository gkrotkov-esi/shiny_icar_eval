# We will proceed with LOOCV (or k-fold cross validation with k = n)
source("fxns.R")
load_libraries()


# general outline: 
# 1) load data: "cleaned_running_dispatches.csv"
# 2) split into train & test (NOTE THIS HAPPENS IN THE FXNS NOT IN MAIN)
# 3) construct each model, 
# run LOOCV for each model

data <- read_csv("raw data/cleaned_running_dispatches.csv")

#######################
#### Random Forest ####
#######################

# as per the rfrc script, generate an rfrc model and load it.

bot <- load_data(data)
tmp <- extract_rules(bot)
rules <- tmp$rules
colon <- tmp$colon
data <- unstack_rules(rules, colon)
data <- combine_data(bot, data)
# set cmatrix to TRUE to get a confusion matrix
tmp <- rfrc_modeling(data, cmatrix = FALSE)
rfrc_model <- tmp$model
rfrc_error <- tmp$error

#############
#### ARC ####
#############

# the ARC script doesn't have the ability to train, so
# I'll just use the trained model

# NEED TO DEBUG THE arc_modeling FXN!

tmp <- arc_modeling(data)
arc_model <- tmp$model
arc_error <- tmp$error