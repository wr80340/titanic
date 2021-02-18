
# load data and librarys --------------------------------------------------

library(tidyverse)
library(magrittr)
library(mice)
library(doParallel)
library(rpart)
library(caret)

# load data
train = read_csv("C:/Users/User/Desktop/kaggle/titanic/train.csv") %>% dplyr::select(-c(PassengerId, Name))
test = read_csv("C:/Users/User/Desktop/kaggle/titanic/test.csv") %>% dplyr::select(-c(PassengerId, Name))
test_id = read_csv("C:/Users/User/Desktop/kaggle/titanic/test.csv") %>% select(PassengerId)
# Y = Survived 

train %>% head()
train$Survived %<>% as.character()

dim(train)
dim(test)
str(train)

# EDA ---------------------------------------------------------------------

ggplot(train, aes(x = Age, col = Survived))+geom_density(bw = 2)
train$Age %>% table()
test$Age %>% table()
ggplot(train, aes(x = SibSp, col = Survived))+geom_density()
train$SibSp %>% table()
test$SibSp %>% table()
ggplot(train, aes(x = Parch, col = Survived))+geom_density()
train$Parch %>% table()
test$Parch %>% table()
ggplot(train%>% filter(Fare < 100), aes(x = Fare, col = Survived))+geom_density(bw = 2)
train %>% filter(Fare > 200)

table(train$Survived, train$Pclass)
table(train$Survived, train$Sex)
table(train$Survived, train$Embarked)
table(train$Survived, train$Embarked, train$Sex)

train %<>% mutate(is_alone = ifelse(SibSp + Parch == 0, "alone", "acomp"))
test %<>% mutate(is_alone = ifelse(SibSp + Parch == 0, "alone", "acomp"))

train %<>% unite(sex_alone, c(Sex,is_alone), remove = FALSE)
test %<>% unite(sex_alone, c(Sex,is_alone), remove = FALSE) 

train %<>% unite(sex_class, c(Sex,Pclass), remove = FALSE)
test %<>% unite(sex_class, c(Sex,Pclass), remove = FALSE) 

train %<>% mutate(num_follow = SibSp + Parch)
test %<>% mutate(num_follow = SibSp + Parch)

table(train$Survived, train$sex_alone)

train %<>% 
  separate(Cabin, c("Cabin_block1", NA, NA, NA), " ") %>% 
  separate(Cabin_block1, c("Cabin_block1", NA), 1) %>% 
  mutate(Cabin = ifelse(is.na(Cabin_block1), "No Cabin", Cabin_block1)) %>% 
  dplyr::select(-c(Cabin_block1))

test %<>% 
  separate(Cabin, c("Cabin_block1", NA, NA, NA), " ") %>% 
  separate(Cabin_block1, c("Cabin_block1", NA), 1) %>% 
  mutate(Cabin = ifelse(is.na(Cabin_block1), "No Cabin", Cabin_block1)) %>% 
  dplyr::select(-c(Cabin_block1))

table(train$Survived, train$Cabin)

train$Cabin
train %>% head()

vec = grepl(pattern = " ", x = train$Ticket, fixed = TRUE)
train[ifelse(vec == TRUE, FALSE, TRUE), "Ticket"] = paste0("normal ", train$Ticket[ifelse(vec == TRUE, FALSE, TRUE)])
train %<>% separate(Ticket, c("Ticket_type", NA), " ") 
train$Ticket_type %>% table()
vec = grepl(pattern = " ", x = test$Ticket, fixed = TRUE)
test[ifelse(vec == TRUE, FALSE, TRUE), "Ticket"] = paste0("normal ", test$Ticket[ifelse(vec == TRUE, FALSE, TRUE)])
test %<>% separate(Ticket, c("Ticket_type", NA), " ") 
test$Ticket_type %>% table()

table(train$Ticket_type)
table(test$Ticket_type)

vec = grepl(pattern = "STON/", x = train$Ticket_type, fixed = TRUE)
train[vec, "Ticket_type"] = "STON"
vec = grepl(pattern = "STON/", x = test$Ticket_type, fixed = TRUE)
test[vec, "Ticket_type"] = "STON"

vec = grepl(pattern = "SOTON/", x = train$Ticket_type, fixed = TRUE)
train[vec, "Ticket_type"] = "SOTON"
vec = grepl(pattern = "SOTON/", x = test$Ticket_type, fixed = TRUE)
test[vec, "Ticket_type"] = "SOTON"

vec = grepl(pattern = "SC/", x = train$Ticket_type, fixed = TRUE)
train[vec, "Ticket_type"] = "SC"
vec = grepl(pattern = "SC/", x = test$Ticket_type, fixed = TRUE)
test[vec, "Ticket_type"] = "SC"

vec = grepl(pattern = "S.O", x = train$Ticket_type, fixed = TRUE)
train[vec, "Ticket_type"] = "SO"
vec = grepl(pattern = "SO/", x = train$Ticket_type, fixed = TRUE)
train[vec, "Ticket_type"] = "SO"
vec = grepl(pattern = "S.O", x = test$Ticket_type, fixed = TRUE)
test[vec, "Ticket_type"] = "SO"

vec = grepl(pattern = "C.A.", x = train$Ticket_type, fixed = TRUE)
train[vec, "Ticket_type"] = "CA"
vec = grepl(pattern = "C.A.", x = test$Ticket_type, fixed = TRUE)
test[vec, "Ticket_type"] = "CA"

vec = grepl(pattern = "CA.", x = train$Ticket_type, fixed = TRUE)
train[vec, "Ticket_type"] = "CA"
vec = grepl(pattern = "CA.", x = test$Ticket_type, fixed = TRUE)
test[vec, "Ticket_type"] = "CA"

vec = grepl(pattern = "S.", x = train$Ticket_type, fixed = TRUE)
train[vec, "Ticket_type"] = "S"
vec = grepl(pattern = "SCO/W", x = train$Ticket_type, fixed = TRUE)
train[vec, "Ticket_type"] = "S"
vec = grepl(pattern = "S.", x = test$Ticket_type, fixed = TRUE)
test[vec, "Ticket_type"] = "S"

vec = grepl(pattern = "A.", x = train$Ticket_type, fixed = TRUE)
train[vec, "Ticket_type"] = "A"
vec = grepl(pattern = "A.", x = test$Ticket_type, fixed = TRUE)
test[vec, "Ticket_type"] = "A"

vec = grepl(pattern = "A/", x = train$Ticket_type, fixed = TRUE)
train[vec, "Ticket_type"] = "A"
vec = grepl(pattern = "A/", x = test$Ticket_type, fixed = TRUE)
test[vec, "Ticket_type"] = "A"

vec = grepl(pattern = "A4.", x = train$Ticket_type, fixed = TRUE)
train[vec, "Ticket_type"] = "A"
vec = grepl(pattern = "AQ/", x = test$Ticket_type, fixed = TRUE)
test[vec, "Ticket_type"] = "A"

vec = grepl(pattern = "F.C.", x = train$Ticket_type, fixed = TRUE)
train[vec, "Ticket_type"] = "F.C.C"
vec = grepl(pattern = "F.C.", x = test$Ticket_type, fixed = TRUE)
test[vec, "Ticket_type"] = "F.C.C"

vec = grepl(pattern = "/PP", x = train$Ticket_type, fixed = TRUE)
train[vec, "Ticket_type"] = "PP"

vec = grepl(pattern = "W/C", x = train$Ticket_type, fixed = TRUE)
train[vec, "Ticket_type"] = "W./C."
vec = grepl(pattern = "WE/P", x = train$Ticket_type, fixed = TRUE)
train[vec, "Ticket_type"] = "W.E.P."

vec = grepl(pattern = "Fa", x = train$Ticket_type, fixed = TRUE)
train[vec, "Ticket_type"] = "other"
vec = grepl(pattern = "LP", x = test$Ticket_type, fixed = TRUE)
test[vec, "Ticket_type"] = "other"

table(train$Ticket_type)
table(test$Ticket_type)

train$Ticket_type %<>% as.factor()
test$Ticket_type %<>% as.factor()

count_na = sapply(1:dim(train)[2], function(i){sum(is.na(train[,i]))})
# check NA columns
for(i in which(count_na != 0)){
  cat(colnames(train)[i], count_na[i], "\n")
}

count_na = sapply(1:dim(test)[2], function(i){sum(is.na(test[,i]))})
# check NA columns
for(i in which(count_na != 0)){
  cat(colnames(test)[i], count_na[i], "\n")
}

train_imp = mice(data = train, method = "cart", m = 5, maxit = 5) 
test_imp = mice(data = test, method = "cart", m = 5, maxit = 5) 
train_imp = complete(train_imp, action = 1)
test_imp = complete(test_imp, action = 1)

anyNA(train_imp)
anyNA(test_imp)

tree.fit = rpart(Embarked~., train %>% filter(!is.na(Embarked)))
predict(tree.fit, train %>% filter(is.na(Embarked)))
train_imp[which(is.na(train_imp$Embarked)), "Embarked"] = "S"

tcrl = trainControl(method = "cv", number = 5, verboseIter = TRUE)
set.seed(1122)


# XGBoost -----------------------------------------------------------------

grid = expand.grid(eta = seq(0.01,0.03,length.out = 3),
                   max_depth = c(4,6,8),
                   gamma = c(0.6,0.9), 
                   colsample_bytree = c(0.6,0.8), 
                   min_child_weight = c(2,6,10),
                   subsample = c(0.6,0.8), 
                   nrounds = c(1000,2000))

# round : dim(grid) * 5
cl = makePSOCKcluster(7)
registerDoParallel(cl)
xgb.fit = train(Survived ~ .,
                data = train_imp, 
                method = "xgbTree", 
                trControl = tcrl,
                maximize = TRUE,
                tuneGrid = grid,
                verbose = TRUE)
stopCluster(cl)
xgb.fit
xgb.fit$results$Accuracy %>% which.max()
xgb.fit$results[140,]
plot(xgb.fit)

submit = read.csv("C:/Users/User/Desktop/kaggle/titanic/gender_submission.csv")
submit$Survived = predict(xgb.fit, newdata = test_imp)
write.csv(submit, file = "C:/Users/User/Desktop/kaggle/titanic/xgb_submit.csv", row.names = FALSE)

# gbm ---------------------------------------------------------------------

grid = expand.grid(shrinkage = seq(0.1,0.6,length.out = 3), 
                   interaction.depth= c(5,7,9), 
                   n.minobsinnode = c(6,8,10), 
                   n.trees = c(500,1000))
cl = makePSOCKcluster(7)
registerDoParallel(cl)
gbm.fit = train(Survived ~ .,
                data = train_imp, 
                method = "gbm", 
                trControl = tcrl,
                maximize = TRUE,
                tuneGrid = grid,
                verbose = FALSE)
stopCluster(cl)
gbm.fit
plot(gbm.fit,
     nameInStrip = TRUE,
     highlight = TRUE)

submit = read.csv("C:/Users/User/Desktop/kaggle/titanic/gender_submission.csv")
submit$Survived = predict(gbm.fit, newdata = test_imp)
write.csv(submit, file = "C:/Users/User/Desktop/kaggle/titanic/gbm_submit.csv", row.names = FALSE)

# adaboost ----------------------------------------------------------------

grid = expand.grid(nIter = seq(50, 250, by = 50),
                   method=c("Adaboost.M1", "Real adaboost"))
cl = makePSOCKcluster(7)
registerDoParallel(cl)
ada.fit = train(Survived ~ .,
                data = train_imp, 
                method = "adaboost", 
                trControl = tcrl,
                maximize = TRUE,
                # tuneGrid = grid,
                tuneLength = 5,
                verbose = TRUE)
stopCluster(cl)
ada.fit
plot(ada.fit,
     nameInStrip = TRUE,
     highlight = TRUE)

submit = read.csv("C:/Users/User/Desktop/kaggle/titanic/gender_submission.csv")
submit$Survived = predict(ada.fit, newdata = test_imp)
write.csv(submit, file = "C:/Users/User/Desktop/kaggle/titanic/ada_submit.csv", row.names = FALSE)

# random forest -----------------------------------------------------------

grid = expand.grid(nIter = seq(50, 250, by = 50),
                   method=c("Adaboost.M1", "Real adaboost"))
cl = makePSOCKcluster(7)
registerDoParallel(cl)
rf.fit = train(Survived ~ .,
                data = train_imp, 
                method = "ordinalRF", 
                trControl = tcrl,
                maximize = TRUE,
                # tuneGrid = grid,
                tuneLength = 3,
                verbose = TRUE)
stopCluster(cl)
rf.fit
plot(rf.fit,
     nameInStrip = TRUE,
     highlight = TRUE)

submit = read.csv("C:/Users/User/Desktop/kaggle/titanic/gender_submission.csv")
submit$Survived = predict(rf.fit, newdata = test_imp)
write.csv(submit, file = "C:/Users/User/Desktop/kaggle/titanic/rf_submit.csv", row.names = FALSE)
