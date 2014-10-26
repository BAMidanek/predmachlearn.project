
# download files      
      download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
                    "./pml-training.csv")
      download.file("https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", 
                    "./pml-testing.csv")
      
# load files into R
      setwd("C:/Users/midanekb14/Desktop/Learning Data Science/Coursera - Data Products")
      
      training <- read.csv("./pml-training.csv")
      testing <- read.csv("./pml-testing.csv")

# load required libraries
      library(caret)
      library(randomForest)

# create validation and training sets

      set.seed(1234)
      inTrain <- createDataPartition(y=training$classe,
                                     p=0.5,
                                     list=F)
      
      train <- training[inTrain,]
      validation <- training[-inTrain,]

# convert numeric variables to numeric; get rid of variables with majority NAs
      train[,8:159] <- sapply(train[,8:159], as.character)
      train[,8:159] <- sapply(train[,8:159], as.numeric)

      useVars <- names(train[apply(train, 2, function(x) sum(is.na(x))) < 100])
      train1 <- train[,useVars]

# 
      ag <- aggregate(. ~ classe, data = train1[,8:60], FUN = mean, simplify=TRUE)
      ag[,2:53] <- scale(ag[,2:53])
      ag

      
      
      train1_scaled <- cbind(train1[,c(60, 2, 5)], scale(train1[,8:59]))
      train1_scaled <- train1_scaled[, -c(2,3)]
      
      
      beltVars <- grep("belt", names(train1_scaled))
      beltProc <- preProcess(train1_scaled[,beltVars], method="pca", thresh=0.8)
      beltTrainPC <- predict(beltProc, train1_scaled[,beltVars])
      names(beltTrainPC) <- paste("belt_", names(beltTrainPC), sep = "")
      
      armVars <- grep("arm", names(train1_scaled))
      armProc <- preProcess(train1_scaled[,armVars], method="pca", thresh=0.8)
      armTrainPC <- predict(armProc, train1_scaled[,armVars])
      names(armTrainPC) <- paste("arm_", names(armTrainPC), sep = "")
      
      dumbbellVars <- grep("dumbbell", names(train1_scaled))
      dumbbellProc <- preProcess(train1_scaled[,dumbbellVars], method="pca", thresh=0.8)
      dumbbellTrainPC <- predict(dumbbellProc, train1_scaled[,dumbbellVars])
      names(dumbbellTrainPC) <- paste("dumbbell_", names(dumbbellTrainPC), sep = "")
      
      forearmVars <- grep("forearm", names(train1_scaled))
      forearmProc <- preProcess(train1_scaled[,forearmVars], method="pca", thresh=0.8)
      forearmTrainPC <- predict(forearmProc, train1_scaled[,forearmVars])
      names(forearmTrainPC) <- paste("forearm_", names(forearmTrainPC), sep = "")
      
      trainPC <- cbind(classe = train1_scaled[,1], armTrainPC, 
                       beltTrainPC, dumbbellTrainPC, forearmTrainPC)
      
      featurePlot(trainPC[,2:24], trainPC$classe)
      
      
      modelFit <- train(classe ~ ., method = "rpart",
                        preProcess = "pca", data = train1_scaled)
      modfit1 <- confusionMatrix(train1_scaled$classe, predict(modelFit, train1_scaled))
      modfit1
      

      
      
      modelFit2 <- train(classe ~ ., data=trainPC, method = "rpart")
      modfit2 <- confusionMatrix(trainPC$classe, predict(modelFit2, trainPC))
      modfit2
      
      
      modelFit3 <- train(classe ~ ., method = "rpart",
                         data=train1)
      modfit3 <- confusionMatrix(train1$classe, predict(modelFit3, train1))
      modfit3
      

      
      
      modelFit4 <- train(classe ~ ., data=train1_scaled, method = "lda")
      modfit4 <- confusionMatrix(train1_scaled$classe, predict(modelFit4, train1_scaled))
      modfit4

      modelFit5 <- train(classe ~ ., data=train1_scaled, method = "qda")
      modfit5 <- confusionMatrix(train1_scaled$classe, predict(modelFit5, train1_scaled))
      modfit5

      sum(train1_scaled$classe == predict(modelFit5, train1_scaled)) / 
            length(train1_scaled$classe)
      
      sum(validation1_scaled$classe == predict(modelFit5, validation1_scaled)) / 
            length(validation1_scaled$classe)
      

      
      #
      modelFit6 <- train(classe ~ ., data=trainPC, method = "qda")
      modfit6 <- confusionMatrix(trainPC$classe, predict(modelFit6, trainPC))
      modfit6
      

      modelFit7 <- randomForest(classe ~ ., data = train1_scaled, 
                              ntree = 50,
                              importance = TRUE,
                              do.trace=TRUE)
      
      sum(train1_scaled$classe == predict(modelFit7, train1_scaled)) / 
            length(train1_scaled$classe)
      
      sum(validation1_scaled$classe == predict(modelFit7, validation1_scaled)) / 
            length(validation1_scaled$classe)

      plot(modelFit7)
      
      confusionMatrix(train1_scaled$classe, predict(modelFit7, train1_scaled))
      confusionMatrix(validation1_scaled$classe, predict(modelFit7, validation1_scaled))
      
      #
      modelFit8 <- randomForest(classe ~ ., data = trainPC, 
                              ntree = 50,
                              importance = TRUE,
                              do.trace=TRUE)
      
      sum(trainPC$classe == predict(modelFit8, trainPC)) / 
            length(trainPC$classe)
      sum(validationPC$classe == predict(modelFit8, validationPC)) / 
            length(validationPC$classe)
      varImp(modelFit8)

      
     
      
      validation[,8:159] <- sapply(validation[,8:159], as.character)
      validation[,8:159] <- sapply(validation[,8:159], as.numeric)
      validation1 <- validation[,useVars]
      validation1_scaled <- cbind(validation1[,c(60, 2, 5)], scale(validation1[,8:59]))
      validation1_scaled <- validation1_scaled[, -c(2,3)]
            
      beltValidationPC <- predict(beltProc, validation1_scaled[,beltVars])
      armValidationPC <- predict(armProc, validation1_scaled[,armVars])
      dumbbellValidationPC <- predict(dumbbellProc, validation1_scaled[,dumbbellVars])
      forearmValidationPC <- predict(forearmProc, validation1_scaled[,forearmVars])
      
      names(beltValidationPC) <- paste("belt_", names(beltValidationPC), sep = "")
      names(armValidationPC) <- paste("arm_", names(armValidationPC), sep = "")
      names(dumbbellValidationPC) <- paste("dumbbell_", names(dumbbellValidationPC), sep = "")
      names(forearmValidationPC) <- paste("forearm_", names(forearmValidationPC), sep = "")
      
      validationPC <- cbind(classe = validation1_scaled[,1], armValidationPC, 
                       beltValidationPC, dumbbellValidationPC, forearmValidationPC)

      
      