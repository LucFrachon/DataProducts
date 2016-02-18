library(shiny)
shinyUI(pageWithSidebar(
    headerPanel("Surviving the Titanic"),
    sidebarPanel(width = 3,
                 radioButtons("modClass", "Choose a model class:", 
                                    c("Selection Tree" = "rpart", 
                                      "Generalised Linear Model" = "glm", 
                                      "Boosted Gen. Linear Model" = "glmboost")),
                 sliderInput("param", "Parameter", min = 1., max = 1., 
                             value = 1.),
                 verbatimTextOutput("sliderComment"),
                 h4("Instructions:"),
                 p("This Shiny app is based on the Titanic dataset provided 
                   with R (note that a more elaborate dataset is available 
                   from Kaggle). The version used here has been cleaned up 
                   outside of the app."),
                 p("The goal is to predict whether a passenger was likely to 
                   survive the sinking of the Titanic based on their Sex, Age 
                   and Class."),
                 p("Three learning models are offered: "), em("Selection 
                   Tree, Generalised Linear Model and Boosted GLM."),
                 p("For each model, a parameter can be adjusted using the 
                   slider. The parameter is described under the slider."),
                 p("75% of the dataset is used for training 
                   and 25% as a validation set."),
                 p("The main panel will then show a reminder of the selected 
                   model and parameter, a plot of the predictions applied to the 
                   validation set, the prediction accuracy and the confusion 
                   matrix report."),
                 p("Of course, any tuning done here would need to be tested 
                   against an unseen test dataset."),
                 p("It turns out that it is not so easy to do much better than
                   the basic rule 'Female = Survived', 'Male' = 'Did not 
                   survive' (which corresponds to a Selection Tree with 
                   maxdepth = 1). Have a try!")),
mainPanel(img(src = "Stower_Titanic.jpg", width = 300, height = 200, align = "right"),
              h3("Results:"),
              h4("Learning Algorithm:"),
              verbatimTextOutput("modelInput"),
              h4("Parameter input:"),
              verbatimTextOutput("paramInput"),
              h4("Plot:"),
              plotOutput("plot"),
              h4("Accuracy measure (%) on validation set (25% of observations):"),
              verbatimTextOutput("accuracy"),
              h4("Confusion matrix of prediction:"),
              verbatimTextOutput("confusionMatrix")
              )
    )
)
