getwd()
list.files()
install.packages("class")
install.packages("caret")
library(class)
library(caret)
bank <- read.csv("UniversalBank-1.csv")
head(bank)
str(bank)
dim(bank)
str(bank)
# Convert Education to factor
bank$Education <- as.factor(bank$Education)
# Create dummy variables
education_dummies <- model.matrix(~ Education - 1, data = bank)
# Rename columns
colnames(education_dummies) <- c(
"Education_1",
"Education_2",
"Education_3"
)
head(education_dummies)
bank_model <- cbind(
bank,
education_dummies
)
head(bank_model)
bank_model <- bank_model[
,
!(names(bank_model) %in%
c(
"ID",
"ZIP.Code",
"Education"
))
]
names(bank_model)
str(bank_model)
set.seed(12345)
library(caret)
train_index <-
createDataPartition(
bank_model$Personal.Loan,
p = 0.60,
list = FALSE
)
train_data <-
bank_model[train_index, ]
validation_data <-
bank_model[-train_index, ]
dim(train_data)
dim(validation_data)
train_x <-
train_data[
,
!(names(train_data)
%in%
"Personal.Loan")
]
train_y <-
train_data$Personal.Loan
validation_x <-
validation_data[
,
!(names(validation_data)
%in%
"Personal.Loan")
]
validation_y <-
validation_data$Personal.Loan
train_x_scaled <-
scale(train_x)
validation_x_scaled <-
scale(
validation_x,
center =
attr(
train_x_scaled,
"scaled:center"
),
scale =
attr(
train_x_scaled,
"scaled:scale"
)
)
bank$Education <- as.factor(bank$Education)
validation_x_scaled <- scale(...)
validation_x_scaled <- scale(...)
dim(train_data)
dim(validation_data)
dim(train_x_scaled)
dim(validation_x_scaled)
new_customer <- data.frame(
Age = 40,
Experience = 10,
Income = 84,
Family = 2,
CCAvg = 2,
Mortgage = 0,
Securities.Account = 0,
CD.Account = 0,
Online = 1,
CreditCard = 1,
Education_1 = 0,
Education_2 = 1,
Education_3 = 0
)
colnames(train_x)
# Reorder new customer columns to match training predictors
new_customer <- new_customer[, colnames(train_x)]
# Check column order
colnames(new_customer)
# Standardize the new customer using training data scaling
new_customer_scaled <- scale(
new_customer,
center = attr(train_x_scaled, "scaled:center"),
scale = attr(train_x_scaled, "scaled:scale")
)
library(class)
prediction_k1 <- knn(
train = train_x_scaled,
test = new_customer_scaled,
cl = train_y,
k = 1,
prob = TRUE
)
prediction_k1
k_values <- 1:30
accuracy_results <- data.frame(
k = k_values,
validation_accuracy = NA
)
for (i in seq_along(k_values)) {
k <- k_values[i]
validation_pred <- knn(
train = train_x_scaled,
test = validation_x_scaled,
cl = train_y,
k = k
)
cm <- confusionMatrix(
factor(validation_pred, levels = c(0, 1)),
factor(validation_y, levels = c(0, 1)),
positive = "1"
)
accuracy_results$validation_accuracy[i] <- cm$overall["Accuracy"]
}
accuracy_results
best_k <- 3
validation_pred_best <- knn(
train = train_x_scaled,
test = validation_x_scaled,
cl = train_y,
k = best_k
)
confusion_validation <- confusionMatrix(
factor(validation_pred_best, levels = c(0,1)),
factor(validation_y, levels = c(0,1)),
positive = "1"
)
confusion_validation
prediction_best_k <- knn(
train = train_x_scaled,
test = new_customer_scaled,
cl = train_y,
k = 3,
prob = TRUE
)
prediction_best_k
set.seed(12345)
# 50% Training
train_index_2 <- createDataPartition(
bank_model$Personal.Loan,
p = 0.50,
list = FALSE
)
train_data_2 <- bank_model[train_index_2, ]
temp_data <- bank_model[-train_index_2, ]
# Split remaining 50% into 30% validation and 20% test
validation_index_2 <- createDataPartition(
temp_data$Personal.Loan,
p = 0.60,
list = FALSE
)
validation_data_2 <- temp_data[validation_index_2, ]
test_data_2 <- temp_data[-validation_index_2, ]
dim(train_data_2)
dim(validation_data_2)
dim(test_data_2)
train_x_2 <- train_data_2[, !(names(train_data_2) %in% "Personal.Loan")]
train_y_2 <- train_data_2$Personal.Loan
validation_x_2 <- validation_data_2[, !(names(validation_data_2) %in% "Personal.Loan")]
validation_y_2 <- validation_data_2$Personal.Loan
test_x_2 <- test_data_2[, !(names(test_data_2) %in% "Personal.Loan")]
test_y_2 <- test_data_2$Personal.Loan
train_x_2_scaled <- scale(train_x_2)
validation_x_2_scaled <- scale(
validation_x_2,
center = attr(train_x_2_scaled,"scaled:center"),
scale = attr(train_x_2_scaled,"scaled:scale")
)
test_x_2_scaled <- scale(
test_x_2,
center = attr(train_x_2_scaled,"scaled:center"),
scale = attr(train_x_2_scaled,"scaled:scale")
)
best_k <- 3
train_pred_2 <- knn(
train = train_x_2_scaled,
test = train_x_2_scaled,
cl = train_y_2,
k = best_k
)
validation_pred_2 <- knn(
train = train_x_2_scaled,
test = validation_x_2_scaled,
cl = train_y_2,
k = best_k
)
test_pred_2 <- knn(
train = train_x_2_scaled,
test = test_x_2_scaled,
cl = train_y_2,
k = best_k
)
confusion_train_2 <- confusionMatrix(
factor(train_pred_2, levels = c(0,1)),
factor(train_y_2, levels = c(0,1)),
positive = "1"
)
confusion_validation_2 <- confusionMatrix(
factor(validation_pred_2, levels = c(0,1)),
factor(validation_y_2, levels = c(0,1)),
positive = "1"
)
confusion_test_2 <- confusionMatrix(
factor(test_pred_2, levels = c(0,1)),
factor(test_y_2, levels = c(0,1)),
positive = "1"
)
confusion_train_2$overall["Accuracy"]
confusion_validation_2$overall["Accuracy"]
confusion_test_2$overall["Accuracy"]
list.files()
