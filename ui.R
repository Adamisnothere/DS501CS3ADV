# Import Libraries
library(shiny)
library(shinythemes)
library(DT)

abalone <- 'https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data'
AbaloneColNames = c('Sex', 'Length', 'Diameter', 'Height', 'Whole', 'Shucked', 'Viscera', 'Shell', 'Rings')
dataAbalone <- read.table(abalone, sep = ",", dec = ".", col.names = AbaloneColNames )

# Define UI for app that draws a histogram ----
ui <- shinyUI(fluidPage(theme = shinytheme("cerulean"),
  # App title ----
  titlePanel("Abalone Classification Model: Classifying Age of Female and Male Abalones"),
    navbarPage("DS501 CS3",
               
        # Home page
        tabPanel(icon("house"),
          fluidRow(column(img(src = "Green_Abalone.jpg", height = 200, width = 200), width = 2),
                   column(
                     br(),
                     p("Placeholder", style="text-align:justify;color:black;background-color:lavender;padding:15px;border-radius:10px"),
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
                     h2("Graphs"),
                    fluidRow(width=3,
                             radioButtons("radioSex1",
                                           h3("Select Sex"),
                                           choices= c("All", "Male", "Female", "Infant"),
                                          selected="All")),
                    br(), 
                    br(), 
                    br(),
                    h2("Machine Learning Parameters"),
                    fluidRow(
                      p("Set up training parameters"),
                      p("Percent of data to be training:"),
                      sliderInput("trainingSlider1",
                                  h3("Percent Training Data"),
                                  min = 1,
                                  max = 99,
                                  value = 80)),
                    fluidRow(
                      p("Set up fit parameters number and repeats"),
                      numericInput("fitNumber",
                                   h3("Number for fit"),
                                   value = 10,
                                   min = 1,
                                   max = 25),
                      numericInput("fitRepeat",
                                   h3("Repeat for fit"),
                                   value = 10,
                                   min = 1,
                                   max = 20),
                      p("Set up Classification method"),
                      selectInput("selectMLMethod",
                                  h3("Classification Method"),
                                  choices = list("SVM" = 1),
                                  selected = 1),
                      h3("Run Model"),
                      actionButton("submitModel", "Submit Model", class= "btn-success")
                    )
                    
                    ),
                 # Main panel plot that changes based on choice
                  mainPanel(
                     fluidRow(
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
                     fluidRow(
                       h2("Confusion Matrix and Statistics"),
                       p("Prediction  Infant  Adult"),
                       verbatimTextOutput("modelEvalText")
                     )
                     
                  )
        )),
        
        
        # Page 2 Algorithm Details Write Up
        tabPanel("Algorithm Details",
                 
        ),
        
        # Page 3 Data Set and Modeling Methodology
        tabPanel("Data Set/Model Details",
        ),
        
        # Page 4 Case Study Write Up
        tabPanel("Case Study 3"
        )
    )
  )
)