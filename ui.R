# Import Libraries
library(shiny)
library(shinythemes)
library(DT)

abalone <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data'
AbaloneColNames = c('Sex', 'Length', 'Diameter', 'Height', 'Whole', 'Shucked', 'Viscera', 'Shell', 'Rings')
dataAbalone <- read.table(abalone, sep = ",", dec = ".", col.names = AbaloneColNames )

# Define UI for app that draws a histogram ----
ui <- shinyUI(fluidPage(theme = shinytheme("cosmo"),
  # App title ----
  titlePanel("Abalone Classification Model: Classifying Age of Female and Male Abalones"),
    navbarPage("DS501 CS3",
               
        # Home page
        tabPanel(icon("house"),
          fluidRow(column(img(src = "Green_Abalone.jpg", height = 200, width = 200), width = 2),
                   column(
                     br(),
                     p("Abalone Machine Learning Project", style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
                     p(tags$a(href="https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data", "Dataset Source", style="text-align:justify;color:red;text-decoration: underline;background-color:lavender;padding:15px;border-radius:10px")),
                     br(),
                     width = 8),
                   ),
          tags$style(".fa-database {color:#E87722}"),
          h3(p("Abalone Dataset ",icon("database",lib = "font-awesome"),style="color:black;text-align:center")),
          fluidRow(column(DT::dataTableOutput("dataAb"),width = 12)),
          hr(),
          p("Developed by",br("Adam Veilleux."),style="text-align:center; font-family: times"),
        ),
        
        # Page 1 Graphs
        tabPanel("Graphs and Model",
                 # Sidebar choices
                 sidebarLayout(
                   sidebarPanel(
                     h5(strong("Note: The first load can take several seconds"), style="text-align:center"),
                     hr(),
                     h2("Graphs"),
                    fluidRow(width=3,
                             radioButtons("radioSex1",
                                          h3("Select Sex", style="text-align:center"),
                                          choices= c("All", "Male", "Female", "Infant"),
                                          selected="All")),
                    hr(),
                    h2("Machine Learning Parameters:"),
                    fluidRow(
                      h4("Training Parameters:"),
                      sliderInput("trainingSlider1",
                                  h3("Percent Training Data"),
                                  min = 1,
                                  max = 99,
                                  value = 80)),
                    fluidRow(
                      h4("Fit Parameters Number and Repeats:"),
                      numericInput("fitNumber",
                                   h3("Number of Folds"),
                                   value = 2,
                                   min = 2,
                                   max = 25),
                      numericInput("fitRepeat",
                                   h3("Repeats"),
                                   value = 3,
                                   min = 1,
                                   max = 20),
                      h4("Cost Upper Bound:"),
                      numericInput("upperCBound",
                                   h3("C Upper Bound"),
                                   value = 2,
                                   min=2,
                                   max=5),
                      selectInput("selectMLMethod",
                                  h3("Classification Method:"),
                                  choices = list("SVM" = 1),
                                  selected = 1),
                      h3("Run Model"),
                      actionButton("submitModel", "Submit Model", class= "btn-success")
                    )
                    
                    ),
                 # Main panel plot that changes based on choice
                  mainPanel(
                     fluidRow(
                       h3("Sexes Whole Weights vs Part Weights", style="text-align:center"),
                       column(br(),
                              plotOutput("ScatterPlotWeightShucked"),
                              br(),
                              width = 4,
                              style="border:1px solid black"),
                       column(br(),
                              plotOutput("ScatterPlotWeightViscera"),
                              br(),
                              width = 4,
                              style="border:1px solid black"),
                       column(br(),
                              plotOutput("ScatterPlotWeightShell"),
                              br(),
                              width = 4,
                              style="border:1px solid black")
                       ),
                     br(),
                     hr(),
                     fluidRow(
                       column(width=4,
                         h3("Bar Chart of Sex Counts"),
                         plotOutput("barGSex")),
                       column(width=4,
                         h3("BoxPlot of Sex Vs Whole Weight"),
                         plotOutput("barSexvsWhole")),
                       column(width=4,
                         h3("Violin Plot of Sex Vs Whole Weight"),
                         plotOutput("vioSexvsWhole"))
                     ),
                     hr(),
                     fluidRow(
                       h3("Histograms of Each Dataset Attribute", style="text-align:center"),
                       plotOutput("histogramsattr")
                     ),
                     hr(),
                     fluidRow(
                       h2("Confusion Matrix and Statistics"),
                       verbatimTextOutput("modelEvalText")),
                     h3("Cost Vs Accuracy of Model"),
                       plotOutput("modelCostPlot")

                  )
        )),
        
        
        # Page 2 Algorithm Details Write Up
        tabPanel("Algorithm Details",
                 h2("Support Vector Machine Algorithm Details"),
                 p("SVM or Support Vector Machine is a linear model for classification. 
                 It can solve linear and non-linear problems and will work for 
                 the Abalone Age Classification.The SVM is responsible for finding 
                 the decision boundary to separate different classes and maximize the 
                 margin, where margins are the perpendicular distances between the line 
                 and the dots closest to the line. SVM wants find an ideal line between 
                 the two classes such that each classification is separated between the boundary."),
                 br(),
                 p("I will discuss the SVM algorithm in the case of linearly separable cases and briefly discuss the non-linearly separable cases. SVM wants to find the optimal line with the constraint of needing to correctly classify either class (adult or infant). That is,  findiong separate hyperplanes that classify correctly and picking one that maximizes the margin. The hyperplane is a n-1 dimensional subspace for an n-dimensional space."),
                 br(),
                 p("A Hyperplane is known as the following equation: "),
                 img(src = "eq1.png", height = 50, width = 400),
                 br(),
                 p("The data above the line will be greater than 0 and below the line will be less than 0. Taking an arbitrary line, there are 3 hyperplanes that separate the 2 classifications. There is a main hyperplane and 2 others that will be gradually shifted to meet the smallest margin. To calculate the margin you will take the perpendicular distance from the main hyperplane to the datapoints and find the smallest distance on either side of the main hyperplane."),
                 br(),
                 img(src = "hyperplanes.png", height = 400, width = 400),
                 br(),
                 p("The margin is the distance from the dotted line to the middle hyperplane. In the non-linear case you can use several “tricks” to address this issue. The soft margin and kernel tricks. The soft margin will try to find a line to separate but will allow several misclassified data points. While the kernel trick will try to find a non-linear boundary. The soft margin will have a tolerance that can be used to choose how many misclassifcations are allowed. The kernel trick will create polynomial hyperplanes, sigmoid hyperplanes or even linear depending on what you choose. These two tricks can be combined to create a solution that is both nonlinear and allowing misclassifications."),
                 br(),
                 br(),
                 hr(),
                 p("Source: https://towardsdatascience.com/support-vector-machine-simply-explained-fee28eba5496 ")
                 ),
        
        # Page 3 Data Set and Modeling Methodology
        tabPanel("Data Set/Model Details",
                 h3("Dataset:"),
                 p("The dataset that I choose for this project is from the University of California Irvine Machine Learning Repository, specifically a dataset on a marine snail called the Abalone. This dataset was captured from an original marine life study by Warwick J Nash, Tracy L Sellers, Simon R Talbot, Andrew J Cawthorn and Wes B Ford (1994). The dataset is multivariate in nature including, categorical, integers, and reals data types. The ranges of the characteristics mentioned are in the form of continuous data, integers, and categorical nominal. In total there are 4177 instances of data made up of 9 attributes related to the abalone. "),
                 
                 p("The 9 attributes are as follows in the order of Name, Data Type, Measurement Unit and its description:"),
                 p("1. Sex, Nominal, N/A, Male/Female/ or Infant"),
                 p("2. Length, Continuous, Millimeters, Longest shell measurement"),
                 p("3. Diameter, Continuous, Millimeters, Perpendicular distance to length"),
                 p("4. Height, Continuous, Millimeters, Height with meat in shell"),
                 p("5. Whole Weight, Continuous, Grams, Weight of entire Abalone"),
                 p("6. Shucked Weight, Continuous, Grams, Weight of only meat"),
                 p("7. Viscera Weight, Continuous, Grams, Weight of guts after bleeding"),
                 p("8. Shell Weight, Continuous Grams, Weight of shell after being dried"),
                 p("9. Rings, Integer, N/A, Rings added to 1.5 can tell the age"),
                 p("An additional attribute that I created was the weight difference between the sum of its parts:"),
                 p("1. Weight Diffrerence, Real, Grams, Difference in total weight from sum of its parts"),
                 h3("Modeling Methodology"),
                 p("The machine learning modeling methodology that was used for this application was using the methods from the R library Caret. The caret package (short for Classification And REgression Training) contains functions to streamline the model training process for complex regression and classification problems. To create the model I used the following process: Create training and testing sets, create the resampling method, train the model, used the model and evaluated its performance. To create the training and testing sets, you simply split the given dataset into a percentage of training and testing which can be configured, I used a default of 80% training data. Next, I created the resampling control or fit control using the function trainControl. This will control the method in which is used to sample, in our case repeated cv folds and how many repetitions of the folding will occur, which is configured through the application as well. Next, I trained the model using the function ‘train’ which will tune the model. More specifically, we used the method called support vector machine to train the model, this is described more in depth in the algorithm tab of the application. This is where the trainControl output can be used to change the resampling as well as where the attribute to be trained on is chosen, in our case ‘Sex’. Next, we can use our trained model to make predictions on our testing dataset that was partitioned earlier, using the function predict. This will calculate the predicted class ‘Adult’ or ‘Infant’. Finally, we can evaluate our results and get statistical information on how the model has performed using the confusionMatrix function.")
                 ),
        
        # Page 4 Case Study Write Up
        tabPanel("Case Study 3",
                 h2(strong("DS501 Case Study 3 - R Shiny Application")),
                 h3("Data Collected"),
                 p("The data that I collected is explained primarily in the Data Set/Model Details section but a short summary will be provided: The dataset for this project is from the University of California Irvine Machine Learning Repository, specifically a dataset on a type of marine snail called the Abalone. The dataset is multivariate in nature including, categorical, integers, and reals data types. The ranges of the characteristics mentioned are in the forms of continuous data, integers, and categorical nominal. In total there are 4177 instances of the dataset made up of 9 attributes related to the abalone."),
                 p("The 9 attributes are as follows in the order of Name, Data Type, Measurement Unit and its description:"),
                 p("1. Sex, Nominal, N/A, Male/Female/ or Infant"),
                 p("2. Length, Continuous, Millimeters, Longest shell measurement"),
                 p("3. Diameter, Continuous, Millimeters, Perpendicular distance to length"),
                 p("4. Height, Continuous, Millimeters, Height with meat in shell"),
                 p("5. Whole Weight, Continuous, Grams, Weight of entire Abalone"),
                 p("6. Shucked Weight, Continuous, Grams, Weight of only meat"),
                 p("7. Viscera Weight, Continuous, Grams, Weight of guts after bleeding"),
                 p("8. Shell Weight, Continuous Grams, Weight of shell after being dried"),
                 p("9. Rings, Integer, N/A, Rings added to 1.5 can tell the age"),
                 p("An additional attribute that I created was the weight difference between the sum of its parts:"),
                 p("1. Weight Diffrerence, Real, Grams, Difference in total weight from sum of its parts"),
                 p("These attributes are important because they can be used to classify if for a given sample whether the abalone is an adult or an infant. Additionally, this data can be used to create a training and testing set of data to train a machine learning model, the application of this case study. Furthermore, this data could also be used to predict the the number of rings or the age of a given abalone."),
                 #Why is it important to you?
                 h3("Motivations"),
                 p("The dataset that I chose is interesting to me because I have never heard of the abalone before and did not know it was a type of snail even after I had learned about it. I find it interesting that you can use machine learning to predict not only if the abalone is an adult or infant based on its physical attributes but its age and therefore its number of rings. I also thought it would be interesting to create plots of the abalone’s different attributes and see how different the data was across ages or the range of data that the researchers collected. Finally, I am also interested because this work could potentially help researchers identify attributes of abalones and help with conservation efforts."),
                 h3("How I Analyzed the Data"),
                 p("To analyze the data that was collected from the University of California Irvine Machine Learning Repository on Abalone physical attributes, I used several packages and methods. First, I loaded the Abalone dataset using R’s standard table read, I added labels to the columns to easily identify each attribute. Next, I removed duplicate data row entries and removed whitespaces by using the R library package Dplyr, a package used primarily for data manipulation. I did this to standardize and clean the data so that it is more consistent to parse and use to train the model. Additionally, I created a new attribute that is the difference between the whole weight of the abalone and the sum of its parts to make sure there were no data entry errors as well as check that the heights of the Abalones are greater than 0 for additional data errors. Next, I factored the Sex attribute since it will be the classifying category, this will make it easier to parse and inspect the data. Next, I changed the data rows with Sex as ‘Male’ or ‘Female’ into ‘Adult’ using R standard data frame manipulation. I did this because the model will be trying to classify the Abalone into either an ‘Adult’ or ‘Infant’ and the dataset currently identifies Male or Female Abalones as adults. Next, I created a training and testing set this is user modifiable in that the percentage of the dataset used for training can be adjusted. This was done using the caret R package, a package (short for Classification And REgression Training) which provides a set of functions that attempt to streamline the process for creating predictive models. Specifically, I used the function createDataPartition which simply needs the percentage you will be splitting in. Next, I created the fit control using the repeated CV method. This is using the caret function trainControl which will take the number of folds and repetitions which is also user modifiable. Next, I trained the model using caret function ‘train’, this will train the model using support vector machine methodology using a linear classifier. This function is also modifiable in tha the user can select the ‘Cost’ which will help to identify margins in the support vector machine. Finally, I used the testing data to predict and classify the Abalone using caret function ‘predict’ which will just take the model and testing data as well as evaluate the performance using caret function confusionMatrix which gives statistical information on the predictions."),
                 h3("Data Conclusions"),
                 p("After analyzing the dataset I found the following pieces of information on the different weight differentials of the whole weight of the abalone versus the part weights. I found that for male Abalone’s for Shucked and Viscera, had the greatest maximum weight, whereas the females had the maximum shell weight. This is shown in the Sexes Whole Weights Vs Part Weights in the Graphs and Model section of the shiny application. Furthermore, Male abalones had greater shucked weights than females and Infants. This made my hypothesis lean towards males will have heavier shucked or viscera weights but females will have greater shell weights. Next, I analyzed the differences between the sex’s whole weights and found that while the male abalone may have a highest maximum whole weight the female abalone has a slightly higher average whole weight. This is shown in the box plot and violin plot in the Graphs and Model section of the shiny application. It should also be known that there are several hundred more instances of male abalone in this dataset but I believe it is still a good dataset due to the number of each male and female instances being over 1250. Finally, I created several plots of the histograms of each attribute in the dataset. I found that the average length of an abalone would be in the 0.6 mm range, the average diameter would be in the 0.47 mm range, the average height would be heavily averaged near the 0.15 mm range which suggests that for grown adults their heights do not vary. For the weights, the whole weights were averaged around 0.8 grams, the shucked weights average around 0.25 grams, the Viscera weights average around 0.17 grams, and the shell weights average around 0.26 grams. Furthermore, I analyzed the performance of the Support Vector Machine model, due to the model being in Shiny I provided several tunable parameters that can change the performance of the SVM model. My app allows you to change the percent of the dataset being training data, which can affect how well trained your model is, the more training data you provide, generally, the strong the model will be. I also provided tunable fields for the fit control using repeated cv folds, you are able to change the number of folds and repetitions of the folds which can also affect the accuracy of the model and its resampling. Finally, I allow the Cost parameter of the linear classifier to be manually sampled allowing the user to set the upper bound. The output performance of the predictions on the testing dataset is provided in a confusion matrix in the Graphs and Model section of the shiny application. It shows the predictions made for each class (infants and adults) as well as statistical information, the most important of these would be the accuracy values, precision and specificity. The highest accuracy that I was able to achieve was using a test set percentage of 80, folds of 7, repetitions of 5, and C upper bound of 3 (actual of 2.1), this achieved an accuracy of 0.8366. Finally, I created this Shiny application and included the dataset on the first page of the application."),
                 )
    )
  )
)