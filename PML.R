
# Set the working directory
if (!file.exists("PML")) {
    dir.create("PML")
}
setwd("PML")


# Download the raw datasets
if (!file.exists("training.csv") | !file.exists("testing.csv")) {
    url1 <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
    url2 <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
    download.file(url1, destfile = "training.csv")
    download.file(url2, destfile = "testing.csv")
}

# Load the data
trainDT <- read.csv("training.csv", na.strings = c("NA", ""))
testDT <- read.csv("testing.csv", na.strings = c("NA", ""))
dim(trainDT); dim(testDT)

# Remove columns with NA
naNum <- sapply(trainDT, function(x) {sum(is.na(x))})
trainClean <- trainDT[, naNum == 0]

# Remove variables wchich are unimportant to the sensor records
trainFinal <- trainClean[, -(1:7)]
remove(trainDT, trainClean)

# Slice the data with 70% for training and 30% for testing
if(!is.element("caret", installed.packages()[,1])) {
    print("Installing packages")
    install.packages("caret")
}
library(caret)
inTrain <- createDataPartition(y = trainFinal$classe, p = .7, list = F)
training <- trainFinal[inTrain, ]
testing <- trainFinal[-inTrain, ]

# Build a prediction model using Random Forest
if(!is.element("doParallel", installed.packages()[,1])) {
    print("Installing packages")
    install.packages("doParallel")
}
library(doParallel)
registerDoParallel(cores = 3)
set.seed(123456)
mod <- train(classe ~ ., 
              data = training, 
              method = "rf", 
              trControl = trainControl(method = "cv", number = 5))

# Evalute the model with test data
pred <- predict(mod, testing)
confusionMatrix(pred, testing$classe)

if(!is.element("ggplot2", installed.packages()[,1])) {
    print("Installing packages")
    install.packages("ggplot2")
}
testing$predRight <- pred == testing$classe
p <- ggplot(testing, aes(roll_belt, pitch_belt)) + 
    labs(title = "Prediction on the testing dataset")
p + geom_point(aes(color = predRight)) + 
    theme(plot.title = element_text(size = 20, vjust = 2.0)) + 
    theme(axis.title.x = element_text(size = 15, vjust = 0.2)) + 
    theme(axis.title.y = element_text(size = 15, vjust = 1.0)) + 
    theme(axis.text.x = element_text(size = 12, color = "grey60")) + 
    theme(axis.text.y = element_text(size = 12, color = "grey60")) + 
    theme(panel.background = element_blank()) + 
    theme(panel.background = element_rect(color = "black")) + 
    theme(legend.position = c(0.9, 0.9))
 

# Predict the original test data downloaded from the web
answers <- predict(mod, testDT)
answers

# Write the answers into .txt files
pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_", i, ".txt")
        write.table(x[i], file=filename, quote=FALSE, 
                    row.names=FALSE, col.names=FALSE)
    }
}
pml_write_files(answers)

