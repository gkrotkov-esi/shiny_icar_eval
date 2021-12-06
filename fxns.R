# Utility Functions v1.0
# Gabriel Krotkov

#####################
#### convenience ####
#####################

load_libraries <- function(){
  library(tidyverse)
  library(lubridate)
  library(gdata)
  library(caret)
  library(randomForest)
  library(arulesCBA)
  library(arc)
}

##########################
#### legacy functions ####
##########################

# Requires monthyear, corresponds to date range in filename
# Used for the random forest and ARC model scripts WITHOUT root cause analysis.
data_processing <- function(new.bot, monthyear = ""){
  ## create issue id column
  new.bot$id <- seq.int(nrow(new.bot))
  new.bot$monthyear <- monthyear
  
  ## split sets into base and rules for long formatting
  model.df <- new.bot %>%
    select(state, source, sourceCost, rules, monthyear, id)
  model.df.rules <- model.df %>%
    select(id, rules)
  # counts the number of colons
  model.df.rules$colon <- sapply(gregexpr(":", model.df.rules$rules), length)
  # moves that column out of the dataframe
  colon <-  model.df.rules$colon
  # seperates each comment into its own column following the data
  model.df.rules <- data.frame(cbind(model.df.rules,str_split_fixed(model.df.rules$rules, ",", max(model.df.rules$colon))))
  
  ## drop unneeded columns and replace blanks with nas and rename remaining columns
  model.df.rules <-  model.df.rules %>%
    select(-c(rules, colon)) %>%
    mutate_all(na_if,"")
  colnames( model.df.rules) <- c("id", paste0("rule",1:max(colon)))
  ## cast long - events is actually more properly number of unique rules
  model.df.rules.lng <- model.df.rules %>%
    gather(key = "events", value = "times", rule1:paste0("rule",max(colon))) %>%
    drop_na()
  
  ## separate rule from duration as new object (to have the OG to reset to)
  model.df.rules.lng <- model.df.rules.lng %>%
    separate(times, into = c("rule","dur"), sep = ":", extra = "merge")
  
  ## find first alpha character position
  model.df.rules.lng$alpha.pos <- str_locate(model.df.rules.lng$dur, "[a-z]+")[,1]
  ## remove from position of first alpha to end of string
  model.df.rules.lng$units <- str_sub(model.df.rules.lng$dur, start=model.df.rules.lng$alpha.pos, end=length(model.df.rules.lng$dur))
  ## more conditional cleanup, NAs happened where no colon was present
  ## find first number position
  model.df.rules.lng$num.pos <- str_locate(model.df.rules.lng$rule, "[0-9]+")[,1]
  model.df.rules.lng$dur <- if_else(is.na(model.df.rules.lng$dur), str_sub(model.df.rules.lng$rule, start=model.df.rules.lng$num.pos - 1, end = str_length(model.df.rules.lng$rule)), model.df.rules.lng$dur)
  ## rerun unit separation again
  model.df.rules.lng$alpha.pos <- str_locate(model.df.rules.lng$dur, "[a-z]+")[,1]
  model.df.rules.lng$units <- str_sub(model.df.rules.lng$dur, start=model.df.rules.lng$alpha.pos, end=length(model.df.rules.lng$dur))
  ## filter out nas in rules
  model.df.rules.lng <- model.df.rules.lng %>%
    filter(rule != "n/a")
  ## create duration as numeric
  ## find first alpha character position for separate number from hour or minute
  model.df.rules.lng <- model.df.rules.lng %>%
    mutate(dur2=as.numeric(str_sub(model.df.rules.lng$dur, start=1, end=model.df.rules.lng$alpha.pos-1))) %>%
    mutate(dur3=if_else(units=="min", dur2/60, dur2))
  ## merge long set back with base set
  model.merged <- left_join(new.bot, model.df.rules.lng, by="id")
  model.merged <- model.merged %>% 
    filter(rule != "n/a")
  ## select relevant columns
  model.merged1 <- model.merged %>% 
    select(site, state, source, sourceCost, rule, monthyear, id, dur3)
  ## reaggregate rules together by comma; this aggregated model will be used for ml modeling with random forest
  model.agg.new <- model.merged1 %>%
    group_by(state, site, source, id) %>%
    summarize(rules = toString(rule), dur=mean(dur3), source.cost = mean(sourceCost)) %>%
    ungroup() %>%
    select(-id) %>%
    drop_na() %>%
    as.data.frame()
  return(model.agg.new)
}

modeling_random_forest <- function(model.agg.new, date){
  load("D:/ESI/Training Wheels/random forest default.rda")
  ## create column of predicted values from default random forest model
  model.agg.new$prediction <- predict(rf.default, model.agg.new)
  clean <- "D:/ESI/Training Wheels/clean data random forest driver"
  setwd(clean)
  output_filename <- paste("bot resolution rf prediction ", 
                           date, ".csv", sep = "")
  write.csv(model.agg.new, output_filename, row.names=F)
}

#############
#### ARC ####
#############

# start with agg model from end of random forest data processing
modeling_arc <- function(data, date){
  data$id <- seq.int(nrow(data))
  site.id <- data %>% select(id, site)
  data <- data %>% select(-c(site, id)) %>% mutate_if(is.character,as.factor)
  
  # discretize january data and convert to transactions
  data <- discretizeDF(data)
  
  ## create prediction column
  load("D:/ESI/Training Wheels/arc classifier.rda")
  data$prediction <- predict(classifier, data)
  data$id <- seq.int(nrow(data))
  
  ## merge site back in
  complete <- left_join(data, site.id, by="id")
  
  out_file <- paste("both resolution mb prediction ", 
                    date, ".csv", sep = "")
  out_file <- paste(paste(getwd(), "clean data arc driver", sep = "/"), 
                    out_file, sep = "/")
  
  write.csv(complete, out_file, row.names = F)
}

# rewrite of the above function, in the same style as the rewritten rfrc fxns
# takes in data, returns model and its CV error
# ASSUMPTION: "root.cause" is a column in the dataframe and is the class value
# ASSUMPTION: data has been through the data processing fxns below
arc_modeling <- function(data){
  data <- data %>%
    mutate_if(is.character, as.factor)
  data <- discretizeDF(data)
  browser()
  classifier <- cba(data, "root.cause")
  predictions <- predict(classifier, data)
  error <- sum(predictions == data$root.cause) / nrow(data)
  return(list(model = classifier, error = 1 - error))
}

#########################
#### data processing ####
#########################


load_data <- function(raw){
  # new df filtered for observations where review is true
  
  bot <- raw %>%
    mutate(review = str_detect(review, regex("Review*|Reiv*|Reveiw*", 
                                        ignore_case = T))) %>%
    filter(review == T)

  # arrange and add id
  bot <- bot %>%
    mutate(date = mdy(date),
           month = month(date),
           year = year(date),
           monthyear = mdy(paste(month, "/1/", year, sep=""))) %>%
    arrange(monthyear, date, siteCost) %>%
    mutate(id=seq.int(nrow(bot)))
  return(bot)
}
 
extract_rules <- function(bot){
  # split sets into base and rules for long formatting
  model <- bot %>%
    select(state, source, sourceCost, rules, date, monthyear, id)
  
  rules <- model %>%
    select(id, rules)
  
  # create vector of colon counts for unique rule count per observation
  rules$colon <- sapply(gregexpr(":", rules$rules), length)
  colon <-  rules$colon
  rules <- data.frame(cbind(rules,str_split_fixed(rules$rules, 
                                                  ",", max(rules$colon))))
  
  # drop unneeded columns and replace blanks with nas and rename remaining columns
  rules <-  rules %>% 
    select(-c(rules, colon)) %>% 
    mutate_all(na_if,"")
  
  ## name columns from 1:n rules per event
  colnames(rules) <- c("id", paste0("rule",1:max(colon)))
  
  return(list(rules = rules, colon = colon))
}

unstack_rules <- function(rules, colon){
  # separate rule from duration as new object (to have the OG to reset to)
  rules.lng <- rules %>%
    gather(key = "events", value = "times", rule1:paste0("rule",max(colon))) %>%
    drop_na() %>%
    separate(times, into = c("rule","dur"), sep = ":", extra = "merge")
  
  ## 9. find first alpha character position
  rules.lng$alpha.pos <- str_locate(rules.lng$dur, "[a-z]+")[,1]
  
  ## 10. remove from position of first alpha to end of string
  rules.lng$units <- str_sub(rules.lng$dur, start=rules.lng$alpha.pos, end=length(rules.lng$dur))
  
  
  ## 11. find first alpha character position for separate number from hour or minute and duration as numeric
  rules.lng <- rules.lng %>%
    mutate(dur2 = as.numeric(
      str_sub(dur, start=1, end=alpha.pos-1)),
      dur3 = if_else(units=="min", dur2/60,dur2))
  
  return(rules.lng)
}

combine_data <- function(bot, rules.lng){
  # merge long set back with base set and filter remaining n/as out
  model.merged <- left_join(bot, rules.lng, by="id") %>%
    filter(rule != "n/a")
  
  # select relevant columns
  trimmed <- model.merged %>% 
    select(site, state, source, sourceCost, rule, monthyear, id, dur3, root.cause)
  
  
  # reaggregate rules together by comma; this aggregated model will be used for ml modeling with random forest and mb
  aggregated <- trimmed %>%
    group_by(state, site, source, id, root.cause) %>%
    summarize(rules = toString(rule), dur=mean(dur3), source.cost = mean(as.numeric(sourceCost))) %>%
    ungroup() %>%
    select(-id) %>%
    drop_na() %>%
    as.data.frame()
  
  prop.table(table(aggregated$root.cause))
  
  # begin from section above
  # POSSIBLE BUG - apparently nothing is in the workspace? 
  # perhaps this is just a funky bit of how functions work
  gdata::keep(aggregated, sure = T)
  
  # convert root.cause to factor 
  model.data <- aggregated %>% 
    select(-site) %>% 
    mutate(root.cause = str_replace(root.cause, "Coolling", "Cooling"),
           root.cause = as.factor(root.cause))
  return(model.data)
}

###################
#### rfrc fxns ####
###################

# rfrc = "random forest (w/) root cause"

# returns generalization error
# specifically the error of the model returning the correct root cause
# uses 70-30 train-test split and returns recombined model
# cmatrix - if TRUE, output a confusion matrix
rfrc_modeling <- function(data, cmatrix = FALSE){
  idx <- createDataPartition(data$root.cause, p=0.7, list=F)
  train <- data[idx,]
  test <- data[-idx,]
  
  ## run random forest with default settings
  rfrc <- randomForest(root.cause ~ ., data=train)

  ## create column of predicted values from default random forest model
  test <- test %>%
    modelr::add_predictions(rfrc) %>%
    select(pred, root.cause, everything())
  
  # compute error, recombine, and save recombined model
  error <- 1 - (sum(test$pred == test$root.cause) / nrow(test))
  rfrc <- randomForest(root.cause ~ ., data = data)
  save(rfrc, file = "random forest default root cause.rda")
  
  if(cmatrix){
    confusionMatrix(test$pred, test$root.cause)
  }
  
  return(list(model = rfrc, error = error))
}

# k fold cross validation
# returns the combined model and also the CV error
# ISSUE: since some classes have levels with 1 value, k fold cross validation
# causes the calls to randomForest to crash. 
rfrc_modeling_kfold <- function(data, k){
  data$fold <- createFolds(data$root.cause, k = k, list = FALSE)
  errors <- vector(length = k)
  for(i in 1:k){
    train <- data[data$fold == k, ]
    test <- data[data$fold != k, ]
    rfrc <- randomForest(root.cause ~ ., data = train)
    test <- test %>%
      modelr::add_predictions(rfrc) %>%
      select(pred, root.cause, everything())
    errors[i] <- sum(test$pred == test$root.cause) / nrow(test)
  }
  
  # compute combined model and save it locally
  rfrc <- randomForest(root.cause ~ ., data = data)
  save(rfrc, file = "random forest default root cause.rda")
  
  return(list(model = rfrc, error = 1 - mean(errors)))
}
