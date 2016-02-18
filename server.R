library(shiny); library(rpart); library(ipred)
library(mboost); library(ggplot2); library(caret); library(e1071)
set.seed(1234)

normalised <- function(x){
    preProcObj <- preProcess(x, method = c("center", "scale", "bagImpute"))
    trainPp <- predict(preProcObj, newdata = x)
    trainPp
}

titanic <- read.csv("dataset.csv")
titanic <- transform(titanic, Pclass = as.factor(Pclass), Sex = as.factor(Sex),
                     Survived = as.factor(Survived))
predictors <- subset(titanic, select = c(Pclass, Sex, Age))
outcome <- titanic$Survived
indices <- createDataPartition(titanic$Survived, p = 0.75, 
                                       list = FALSE, times = 1)
X_train <- normalised(predictors[indices, ])
X_test <- normalised(predictors[-indices, ])
Y_train <- outcome[indices]
Y_test <- outcome[-indices]

paramRange <- function(modelClass){
    if (modelClass == "rpart"){
        return(list("maxdepth = max. number of levels in tree", 
                    c(1, 7, 20, 1), "maxdepth"))
    } else if (modelClass == "glm"){
        return(list("epsilon = convergence tolerance", 
                    c(1e-3, 0.01, 0.1, 0.005), "l"))
    } else if (modelClass == "glmboost"){
        return(list("nu = step size", c(1e-3, 1e-2, 0.1, 3e-3),
                    "nu"))
    }
}

fitModel <- function(x, y, modelClass, paramValue){
    if (modelClass == "rpart") {
        fitObj <- rpart(y ~ . , data = x, method = "class", 
                        control = rpart.control(maxdepth = paramValue))
    } else if (modelClass == "glm") {
        fitObj <- glm(y ~ .*. , data = x, family = binomial,
                      control = glm.control(epsilon = paramValue))
    } else if (modelClass == "glmboost") {
        fitObj <- glmboost(y ~ . , data = x, family = Binomial(link="logit"),
                           control = boost_control(nu = paramValue, 
                                                   mstop = 1000))
    }    
    fitObj
}

prediction <- function(fitObj, new.x, modelClass){
    if (modelClass == "rpart") {
        pred <- as.factor(predict(fitObj, newdata = new.x, type = "vector") - 1)
    } else if (modelClass == "glm") {
        pred <- predict(fitObj, newdata = new.x, type = "response")
        pred <- as.factor((pred >= 0.5) * 1.)
    } else if (modelClass == "glmboost") {
        pred <- predict(fitObj, newdata = new.x, type = "class")
    }
    pred
}

plotting <- function(x, y, pred){
    jitterVct <- runif(nrow(x), min = -0.2, max = +0.2)
    x$PclassJitter <- as.numeric(x$Pclass) + jitterVct
    x$correct <- as.factor(pred == y)
    dataset <- cbind(x, y, pred)
    g <- ggplot(dataset, aes(x = PclassJitter, y = Age)) + 
        geom_point(aes(x = PclassJitter, y = Age,
                       colour = as.factor(y),
                       alpha = as.factor(ifelse(dataset$correct == TRUE, 
                                                0.3, 1)),
                       size = 3),
                   show.legend = TRUE) +
        xlab("Passenger Class, jittered to reduce overplotting") +
        scale_x_discrete(breaks = c(1, 2, 3), labels = c("1", "2", "3")) +
        xlim("1", "2", "3") +
        ylab("Age, scaled and centered") +
        guides(size = FALSE) +
        scale_colour_discrete(name = "Survived?", labels = c("No", "Yes")) +
        scale_alpha_discrete(name = "Correctly predicted?", labels = c("Yes", "No")) +
        theme(legend.position = "bottom") + 
        facet_grid(. ~ Sex)
    g  
}

shinyServer(
    function(input, output, session){
        observe({
            min <- paramRange(input$modClass)[[2]][1]
            def <- paramRange(input$modClass)[[2]][2]
            max <- paramRange(input$modClass)[[2]][3]
            step <- paramRange(input$modClass)[[2]][4]
            updateSliderInput(session, "param", value = def, 
                              min = min, max = max, step = step)
        })
        mod <- reactive({mod <- input$modClass})
        output$modelInput <- renderText({input$modClass})
        paramVal <- reactive({input$param})
        output$paramInput <- renderPrint({input$param})
        output$sliderComment <- renderPrint({cat(paramRange(
            input$modClass)[[1]])})
        paramDesc <- reactive({paramRange(input$modClass)[[3]]})
        fit <- reactive({fitModel(X_train, Y_train, mod(), paramVal()[1])})
        pred <- reactive({prediction(fit(), X_test, mod())})
        output$confusionMatrix <- renderPrint({confusionMatrix(pred(), Y_test)})
        output$accuracy <- renderPrint({round(sum(pred() == Y_test) /
                length(Y_test) * 100, 2)})
        output$plot <- renderPlot({plotting(X_test, Y_test, pred())})
})
