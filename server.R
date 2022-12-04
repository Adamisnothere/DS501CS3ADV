# Load Libraries
library(plyr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(caret)
library(lattice)
# Algorithm Assistance from: https://rpubs.com/shihui17170153/abalone

# Retrieve the data from UCI Archive on Abalone
abalone <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data'
AbaloneColNames = c('Sex', 'Length', 'Diameter', 'Height', 'Whole', 'Shucked', 'Viscera', 'Shell', 'Rings')
dataAbalone <- read.table(abalone, sep = ",", dec = ".", col.names = AbaloneColNames )

# Given is the attribute name, attribute type, the measurement unit and a brief 
# description. The number of rings is the value to predict: either as a continuous value or as a classification problem.
# 
# Name / Data Type / Measurement Unit / Description
# -----------------------------
# Sex / nominal / -- / M, F, and I (infant)
# Length / continuous / mm / Longest shell measurement
# Diameter / continuous / mm / perpendicular to length
# Height / continuous / mm / with meat in shell
# Whole weight / continuous / grams / whole abalone
# Shucked weight / continuous / grams / weight of meat
# Viscera weight / continuous / grams / gut weight (after bleeding)
# Shell weight / continuous / grams / after being dried
# Rings / integer / -- / +1.5 gives the age in years

# Preprocessing

# Remove Duplicate rows
dataAbalone <- dataAbalone[!duplicated(dataAbalone),]

# Check for missing values and clean if necessary
dataAbalone[!complete.cases(dataAbalone),]

# Remove 0 Height values, trim whitespace, add column for weight difference
dataAbalone <- dataAbalone %>%
  mutate_if(is.character, str_trim) %>%
  mutate(WDifference = Whole - (Viscera + Shucked + Shell)) %>%
  subset(Height > 0) %>%
  subset(WDifference > 0)

# Factor Sex column
dataAbalone$Sex <- ordered(dataAbalone$Sex, levels = c("I", "M", "F"), labels = c("Infant", "Male", "Female"))

# classify
levels(dataAbalone$Sex) <- c(levels(dataAbalone$Sex), "Adult")
dataAbaloneC = dataAbalone
dataAbaloneC$Sex[dataAbaloneC$Sex=="Male"] <- "Adult"
dataAbaloneC$Sex[dataAbaloneC$Sex=="Female"] <- "Adult"
dataAbaloneC$Sex <- ordered(dataAbaloneC$Sex, 
                            levels = c("Infant", "Adult"),
                            labels = c("Infant", "Adult"))

# Split into training and testing data
trainingIndex <- createDataPartition(dataAbaloneC$Sex, p = 0.8, list = F)
trainingSet <- dataAbaloneC[trainingIndex,]
testingSet <- dataAbaloneC[-trainingIndex,]
#
# # # Build Model
# fitControl <- trainControl(method = "repeatedcv",
#                            number = 5,
#                            repeats = 5)
# 
# #Support Vector Machine
# SVM = train(Sex ~ .,
#             data= trainingSet,
#             method = "svmLinear",
#             preProcess = c("center","scale"),
#             trControl = fitControl,
#             tuneGrid = expand.grid(C = seq(0, 2, length = 20))
#             )
# # 
# # 
# # # # Predict
# SVM_Prediction = predict(SVM, newdata = testingSet)
# # 
# # # 
# # # # Model Eval
# SVM_Eval = confusionMatrix(SVM_Prediction, testingSet$Sex)


server <- function(input, output) {
  
  # Main data table on home page
  output$dataAb <- DT::renderDataTable(
    DT::datatable({
      dataAbalone
    },
    options = list(lengthMenu=list(c(5,15,20),c('5','15','20')),pageLength=10,
                   initComplete = JS(
                     "function(settings, json) {",
                     "$(this.api().table().header()).css({'background-color': 'moccasin', 'color': '1c1b1b'});",
                     "}"),
                   columnDefs=list(list(className='dt-center',targets="_all"))
    ),
    filter = "top",
    selection = 'multiple',
    style = 'bootstrap',
    class = 'cell-border stripe',
    rownames = FALSE,
    colnames = c('Sex', 'Length', 'Diameter', 'Height', 'Whole', 'Shucked', 'Viscera', 'Shell', 'Rings', 'Weight Diff')
    ))
  
  
  # ScatterPlots based on input$radioSex1
  output$ScatterPlotWeightShucked <- renderPlot({
    if(input$radioSex1 == "All"){
      qplot(data=dataAbalone, x=Whole, y=Shucked, geom=c("point", "smooth"), color= Sex, xlim=c(0,3.5), ylim=c(0,1.5), main = "All Sexes Whole Vs Shucked Weight")
    }else{
      qplot(data= subset(dataAbalone, Sex==input$radioSex1), x=Whole, y=Shucked,geom=c("point", "smooth"), xlim=c(0,3.5), ylim=c(0,1.5), main = paste(input$radioSex1, " Sex Whole Vs Shucked Weight", sep=""))
    }
  })
  
  output$ScatterPlotWeightViscera <- renderPlot({
    if(input$radioSex1 == "All"){
      qplot(data=dataAbalone, x=Whole, y=Viscera,geom=c("point", "smooth"), color= Sex, xlim=c(0,3), ylim=c(0,0.8), main = "All Sexes Whole Vs Viscera Weight")
    }else{
      qplot(data= subset(dataAbalone, Sex==input$radioSex1),  x=Whole, y=Viscera,geom=c("point", "smooth"), xlim=c(0,3), ylim=c(0,0.8), main = paste(input$radioSex1, " Sex Whole Vs Viscera Weight", sep=""))
    }
  })
    
  output$ScatterPlotWeightShell <- renderPlot({
    if(input$radioSex1 == "All"){
      qplot(data=dataAbalone, x=Whole, y=Shell, color= Sex,geom=c("point", "smooth"), xlim=c(0,3), ylim=c(0,1.1), main = "All Sexes Whole Vs Shell Weight")
    }else{
      qplot(data= subset(dataAbalone, Sex==input$radioSex1), geom=c("point", "smooth"), x=Whole, y=Shell, xlim=c(0,3), ylim=c(0,1.1), main = paste(input$radioSex1, " Sex Whole Vs Shell Weight", sep=""))
    }
  })
  
  # Machine Learning Classification
  trainingTestSet = reactive({
    createDataPartition(dataAbaloneC$Sex, p = input$trainingSlider1/100, list = F)
    
  })
  
  
  # reactive for fit control
  fitControl = reactive({
    trainControl(method = "repeatedcv",
                 number = input$fitNumber,
                 repeats = input$fitRepeat)
    
  })
  
  
  # reactive for SVM train and prediction
  Model = reactive({
      train(Sex ~ .,
            data= dataAbaloneC[trainingTestSet(),],
            method = "svmLinear",
            preProcess = c("center","scale"),
            trControl = fitControl(),
            tuneGrid = expand.grid(C = seq(0, input$upperCBound, length = 20)))
  })
  
  Prediction <- reactive({
    SVM_Prediction = predict(Model(), newdata = dataAbaloneC[-trainingTestSet(),])
  })
  
  
  output$modelEvalText = renderText({
    input$submitModel
    isolate({
      testingSet <- dataAbaloneC[-trainingTestSet(),]
      SVM_Eval = confusionMatrix(Prediction(), testingSet$Sex)
      paste("Predictions (Infants, Adults): \n", "    Infant: ",SVM_Eval[[2]][1], SVM_Eval[[2]][2], "\n", "    Adult:  ", SVM_Eval[[2]][3], SVM_Eval[[2]][4], "\n", "Statistics:\n",
            "Accuracy: ", SVM_Eval[[3]][1], "\n",
            "Kappa: ", SVM_Eval[[3]][2], "\n",
            "Accuracy Lower: ", SVM_Eval[[3]][3], "\n",
            "Accuracy Upper: ", SVM_Eval[[3]][4], "\n",
            "Accuracy P Value: ", SVM_Eval[[3]][6], "\n",
            "Mcnemar P Value: ", SVM_Eval[[3]][7], "\n\n",
            "Sensitivity: ", SVM_Eval[[4]][1], "\n",
            "Specificity: ", SVM_Eval[[4]][2], "\n",
            "Pos Pred Value: ", SVM_Eval[[4]][3], "\n",
            "Neg Pred Value: ", SVM_Eval[[4]][4], "\n",
            "Precision: ", SVM_Eval[[4]][5], "\n",
            "Recall: ", SVM_Eval[[4]][6], "\n",
            "P1: ", SVM_Eval[[4]][7], "\n",
            "Prevalence: ", SVM_Eval[[4]][8], "\n",
            "Detection Rate: ", SVM_Eval[[4]][9], "\n",
            "Balanced Accuracy: ", SVM_Eval[[4]][10], "\n",
            "Positive Class: ", SVM_Eval[[1]])
    })
  })
  
  output$modelCostPlot = renderPlot({
    input$submitModel
    isolate({plot(Model())})
  })
  
  
  output$histogramsattr = renderPlot({
    par(mfrow=c(3,3))
    Length<-dataAbalone$Length; hist(Length, col="blue")
    Diameter<-dataAbalone$Diameter; hist(Diameter, col="blue")
    Height<-dataAbalone$Height; hist(Height, col="blue")
    Whole<-dataAbalone$Whole; hist(Whole, col="red")
    Shucked<-dataAbalone$Shucked; hist(Shucked, col="red")
    Viscera<-dataAbalone$Viscera; hist(Viscera, col="red")
    Shell<-dataAbalone$Shell; hist(Shell, col="red")
    Rings<-dataAbalone$Rings; hist(Rings, col="green")
  })
  
  output$barGSex = renderPlot({
    ggplot(data=dataAbalone,aes(x=Sex,fill=Sex)) + geom_bar()
  })
  
  
  output$barSexvsWhole = renderPlot({
    qplot(Sex, Whole, data = dataAbalone, geom = "boxplot", fill = Sex)
  })
  
  output$vioSexvsWhole = renderPlot({
    qplot(Sex, Whole, data = dataAbalone, geom = "violin", fill = Sex)
  })
  
  
}