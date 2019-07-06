#
# Series of BT2101 demos
#
# @author: Lek Hsiang Hui <lekhsian @ comp.nus.edu.sg>
#


library(shiny)
library(DT)
library(twitteR)
library(ROAuth)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(stringr)

#for svm
library(e1071)

#for decision tree
library(rpart)
library(rpart.plot)

# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage("BT2101 Demos",
               #for linear regression
               tabPanel("Linear Regression",
                  sidebarLayout(
                    #left side bar
                    sidebarPanel(
                      sliderInput("lrN", "Number of points", min = 5, max = 100, value = 50,
                                  step = 1),
                      actionButton("lrGenerate", label = "Plot random points"),
                      h5("Description:"),
                      p("This demo allows you to plot random points (red dots) and see the corresponding regression line. The residual errors are shown as grey lines.")
                    ),
                    #right main panel
                    mainPanel(
                      plotOutput("lrPlot", height = "600px"),
                      h3(textOutput("lrEqn"), align = "center"),
                      h3(textOutput("lrErr"), align = "center"),
                      wellPanel(
                        verbatimTextOutput("lrCode")
                      )
                    )
                  )
               ), 
               
               #for linear regression
               tabPanel("Linear Regression Simulation",
                  sidebarLayout(
                    #left side bar
                    sidebarPanel(
                      textInput("lrsEqnInput", "Enter Equation (e.g. 2*x +3)", value="2*x+3"),
                      actionButton("lrsGenerate", label = "Run Simulation"),
                      h5("Description:"),
                      p("This demo allows you to run regression simulation. Specify an equation in R format(e.g. 2*x+3, x^3+x^2+1) and see how the regression lines (represented using cyan-colored lines) might look like using randomly generated data (after adding some artificial errors). The red line/curve is the actual relationship.")
                    ),
                    #right main panel
                    mainPanel(
                      plotOutput("lrsPlot", height = "600px")
                    )
                  )
               ),
               
               #for logistic regression
               tabPanel("Logistic Regression (Default Dataset)",
                  sidebarLayout(
                    #left side bar
                    sidebarPanel(
                      selectInput("logrP", "Predictor(s):",
                                  c("Balance" = "balance",
                                    "Income1k" = "income1k",
                                    "Student" = "student",
                                    "Balance+Income1k" = "b+i")),
                      actionButton("logrRun", label = "Classify"),
                      h5("Description:"),
                      p("This demo allows you to run logistic regression classification on the Default dataset. The 'default' points classified by the model is denoted with the 'x' symbol. Blue - non-default cases, Orange - default cases.")
                    ),
                    #right main panel
                    mainPanel(
                      plotOutput("logrPlot", height = "600px"),
                      wellPanel(
                        verbatimTextOutput("logrCode")
                      ),
                      #datatable
                      DT::dataTableOutput('logrDT')
                    )
                  )
               ),
               
               #for web mining
               tabPanel("Web Mining (Twitter)",
                    sidebarLayout(
                      #left side bar
                      sidebarPanel(
                        textInput("wmConsumerKey", "Enter Consumer Key (API Key)"),
                        textInput("wmConsumerSecret", "Enter Consumer Secret (API Secret)"),
                        textInput("wmAccessToken", "Enter Access Token"),
                        textInput("wmAccessSecret", "Enter Access Secret"),
                        textInput("wmSearch", "Enter search string"),
                        actionButton("wmRun", label = "Search"),
                        h5("Description:"),
                        p("This demo demonstrates how you can access the Twitter API from the R environment. You need to create a Twitter app and object the 4 keys from your Twitter app. https://apps.twitter.com/")
                      ),
                      #right main panel
                      mainPanel(
                        plotOutput("wmPlot", height = "600px"),
                        #datatable
                        DT::dataTableOutput('wmDT')
                      )
                    )
               ),
               
               #for SVM
               tabPanel("SVM",
                    sidebarLayout(
                      #left side bar
                      sidebarPanel(
                        selectInput("svmClass", "Num of Class:",
                                    c("2-class" = "2",
                                      "3-class" = "3"),
                                    selected = "2"),
                        
                        #depending on the svmClass selection, show the respective panel:
                        conditionalPanel(
                          condition = "input.svmClass == '2'",
                          radioButtons("svmRadio2", 
                                       label=h5("Class"),
                                       choices =  list("Class 1" = 1, "Class 2" = 2),
                                       selected = 1
                          )
                        ),
                        
                        conditionalPanel(
                          condition = "input.svmClass == '3'",
                          radioButtons("svmRadio3", 
                                       label=h5("Class"),
                                       choices =  list("Class 1" = 1, "Class 2" = 2, "Class 3" = 3),
                                       selected = 1
                          )
                        ),
                        #--------------------------------------------
                        
                        selectInput("svmType", "Kernel Type:",
                                    c("linear" = "linear",
                                      "polynomial" = "polynomial",
                                      "radial" = "radial"),
                                    selected = "radial"),
                        actionButton("svmClear", label = "Clear"),
                        h5("Description:"),
                        p("This demo demonstrates how SVM classify the prediction space. Choose the Class and add point by clicking on the plot. You have to first choose a point for each class for this demo to work properly.")
                      ),
                      #right main panel
                      mainPanel(
                        plotOutput("svmPlot", click = "svm_plot_click")
                      )
                    )
               ),
               
               #for decision tree
               tabPanel("Decision Tree",
                    sidebarLayout(
                      #left side bar
                      sidebarPanel(
                        textInput("dtSourceURL", "Source URL (CSV)", value = "https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"),
                        textInput("dtColnames", "Column names", value = "c(\"sex\", \"len\", \"diameter\", \"height\", \"whole_weight\", \"shucked_weight\", \"viscera_weight\", \"shell_weight\", \"rings\")"),
                        actionButton("dtRun", label = "Classify"),
                        h5("Description:"),
                        p("This demo demonstrates how a decision tree is generated from a dataset. You need to the url to the data set (CSV format) and the colnames (in R string vector format)")
                      ),
                      #right main panel
                      mainPanel(
                        plotOutput("dtPlot", height = "600px"),
                        #datatable
                        DT::dataTableOutput('dtDT')
                      )
                    )
               ),
               
               #for K-means
               #taken from http://shiny.rstudio.com/gallery/kmeans-example.html
               tabPanel("K-means",
                        sidebarLayout(
                          #left side bar
                          sidebarPanel(
                            selectInput('xcol', 'X Variable', names(iris)),
                            selectInput('ycol', 'Y Variable', names(iris),
                                        selected=names(iris)[[2]]),
                            numericInput('clusters', 'Cluster count', 3,
                                         min = 1, max = 9)
                          ),
                          #right main panel
                          mainPanel(
                            plotOutput('plot1')
                          )
                        )
               )
      )
)

server <- shinyServer(function(input, output) {
  #for linear regression
  #method to get the data for the linear regression part
  lrData <- eventReactive(input$lrGenerate,{
    #generate a random number between -10 and 10
    xCoeff <- runif(1, -5, 5)
    yIntercept <- runif(1, -5, 5)
    formula <- sprintf("y = %.3f*x + %.3f", xCoeff, yIntercept, sep="")
    
    xVals <- runif(input$lrN, 0, 100)
    
    #y = xCoeff * x + yIntercept + error
    yVals <- xCoeff * xVals + yIntercept
    yVals <- yVals + rnorm(input$lrN, mean=0, sd = 20)
    
    mod1 <- lm(yVals~xVals)
    
    
    #formula column is just to pass the formula to the code block
    #pass on the data to plot (in a list format)
    data <- list(data = data.frame(x = xVals, y = yVals), 
                 formula = formula, 
                 mod = mod1)
    data
  })
  
  #render the linear regression plot
  output$lrPlot <- renderPlot({
    #read the regression details for displaying
    dataVals = lrData()$data
    mod1 = lrData()$mod
    formula = lrData()$formula
    plot(dataVals$x, dataVals$y, col="red", pch=16, xlab="x", ylab="y")
    
    #regression line
    abline(mod1, col="blue", lwd = 2)
    
    #predict values for the Xs
    pre <- predict(mod1)
    
    #add on "residual lines segments"
    segments(dataVals$x, dataVals$y, dataVals$x, pre, col="grey")
  })
  
  #render the linear regression actual relationship label
  output$lrEqn <- renderText({
    as.character(paste("Actual relationship:", lrData()$formula))
  })
  
  #render the linear regression MSE label
  output$lrErr <- renderText({
    mod1 = lrData()$mod
    
    #calculate residuals (e-values)
    res <- residuals(mod1)
    
    #square error
    sqErr = res * res
    
    #mean - square error (MSE)
    #unbiased - so divided by n-2 instead
    mse = sum(sqErr) / (length(res) - 2)
    
    
    paste("MSE(unbiased) :", "\n", mse, sep="")
  })
  
  #render the linear regression model summary
  output$lrCode <- renderPrint({
    mod1 = lrData()$mod
    summary(mod1)
  })
  #-------end linear regression-------
  
  
  #for linear regression simulation
  #delayed reaction
  lrsData <- eventReactive(input$lrsGenerate,{
    input$lrsEqnInput
  })
  
  #render the linear regression plot
  output$lrsPlot <- renderPlot({
    #read the equation
    eqn <- lrsData()
    
    #generate 100 random values (x-coordinates)
    x <- rnorm(100)
    
    #introduce some error in the sample data
    y_resp <- function(x){ eval(parse(text=eqn)) + rnorm(1, sd=10)} 
    
    #no error case
    y_resp_perfect <- function(x){ eval(parse(text=eqn)) }
    
    #corresponding Y (y1 - y values with error)
    y1 <- lapply(x, y_resp)
    y1 <- c(do.call("cbind", y1))
    
    #y values without error
    y_perfect <- lapply(x, y_resp_perfect)
    y_perfect <- c(do.call("cbind", y_perfect))
    
    #create a plot
    plot(x,y1, type="n", xlab="X", ylab="Y")
    
    #plot the formula (real relationship)
    eqn2 = function(x){eval(parse(text=eqn))}
    curve(eqn2, col="red", lwd=2, add = T)    
    
    #run simulation 10 times (embed the regression lines onto the plot)
    #1 simulation result in one cyan line
    for (i in 1:10){
      y <- lapply(x, y_resp)
      y <- c(do.call("cbind", y))
      
      abline(lm(y~x), col="cyan")
    }
  })
  
  #-------end linear regression simulation-------
  

  #for logistic regression
  Default <- read.csv("data/default.csv")
  #subset of 3000 records
  #Default <- Default[sample(1:nrow(Default), 3000, replace=F),]
  
  #prepare the data (convert the fields from character to factor type)
  Default$default = as.factor(Default$default)
  Default$student = as.factor(Default$student)
  
  #create income1k column
  Default[,"income1k"] <- Default[,"income"] / 1000
  Default$income <- NULL
  
  default_len = nrow(Default)
  logrClassification_res = rep(0, default_len)

  
  #delayed reaction
  logrData <- eventReactive(input$logrRun,{
    #variable to store the number of correct and wrong counts
    correct = 0
    wrong = 0
    
    #read in the predictor type
    predictor = input$logrP
    
    #generate the model accordingly
    if (predictor == "balance"){
      defaultModel <- glm(default~balance, data=Default, family=binomial)

      logrClassification_res = predict(defaultModel,data.frame(balance=Default$balance),type="resp")
    }else if (predictor == "income1k"){
      defaultModel <- glm(default~income1k, data=Default, family=binomial)
      
      logrClassification_res = predict(defaultModel,data.frame(income1k=Default$income1k),type="resp")
    }else if (predictor == "student"){
      defaultModel <- glm(default~student, data=Default, family=binomial)
      
      logrClassification_res = predict(defaultModel,data.frame(student=Default$student),type="resp")
    }else if (predictor == "b+i"){
      defaultModel <- glm(default~balance+income1k, data=Default, family=binomial)
      
      logrClassification_res = predict(defaultModel,data.frame(balance=Default$balance, income1k=Default$income1k),type="resp")
    }
    
    #check the classification result and update the correct and wrong counts
    #each row must be classified to be either default or non-default
    #classify based on which value is larger - if P(default) >= 0.5 -> default, else non-default
    for (i in 1:default_len){
      if (logrClassification_res[i] < 0.5){
        logrClassification_res[i] = 1
        
        if (as.numeric(Default$default[i]) == 1){
          correct = correct + 1
        }else{
          wrong = wrong + 1
        }
      }else{
        logrClassification_res[i] = 2
        
        if (as.numeric(Default$default[i]) == 1){
          correct = correct + 1
        }else{
          wrong = wrong + 1
        }
      }
    }

    #generate the accuracy
    accuracy = correct / (correct + wrong)
    
    #create the data to be passed on to other renderXXX functions
    data <- list(predictor = predictor, 
                 accuracy = accuracy, 
                 mod = defaultModel,
                 classification = logrClassification_res)
  })
  
  #print out the accuracy and the model details
  output$logrCode <- renderPrint({
    result <- logrData()
    
    cat(paste("Accuracy:", result$accuracy))

    summary(result$mod)
  })
  
  #render plot
  output$logrPlot <- renderPlot({
    #read in the classification details
    result <- logrData()
    predictor <- result$predictor
    model <- result$mod
    classification_res <- result$classification

    #plot the actual points colored by default type
    #draw the curve of the model
    #add in the classification results (x denotes default case)
    #pch: NA_integer_ -> empty plot symbol
    #     120    -> 'x' symbol
    
    if (predictor == "balance"){
      plot(x=Default$balance, y=as.numeric(Default$default)-1, col=c('blue','orange')[as.numeric(Default$default)], xlab="Balance", ylab="Probability of Default", xlim=c(0, 3000))
      
      curve(predict(model,data.frame(balance=x),type="resp"),add=TRUE)
      
      
      points(Default$balance,fitted(model), pch=c(NA_integer_,120)[classification_res], cex= 1.5)
    }else if (predictor == "income1k"){
      plot(x=Default$income1k, y=as.numeric(Default$default)-1, col=c('blue','orange')[as.numeric(Default$default)], xlab="Income1k", ylab="Probability of Default")
      
      curve(predict(model,data.frame(income1k=x),type="resp"),add=TRUE)
      
      points(Default$income1k,fitted(model), pch=c(NA_integer_,120)[classification_res], cex= 1.5)
    }else if (predictor == "student"){
      #draw with jitter (i.e. introduce a small "error" so that the points can be drawn without overlapping each other until it is only a single point)
      plot(x=jitter(as.numeric(Default$student),factor=0.2), y=as.numeric(Default$default)-1, col=c('blue','orange')[as.numeric(Default$default)], xlab="Student (1 - not a student, 2 - student)", ylab="Probability of Default")
      
    }else if (predictor == "b+i"){
      plot(x=Default$balance, y=Default$income1k, col=c('blue','orange')[as.numeric(Default$default)], pch=19, xlab="Balance", ylab="Income (1000)", xlim=c(0, 3000))
      
      points(Default$balance,Default$income1k, pch=c(NA_integer_,120)[classification_res], cex= 1.5)
    }
  })
  
  #data table 
  output$logrDT <- DT::renderDataTable({
    result <- logrData()
    classification_res <- result$classification
    
    #convert 2 to Yes and 1 to No
    classification_res[classification_res == 2] = 'Yes'
    classification_res[classification_res == 1] = 'No'

    #render the DT datatable
    #%>% is an infix function - piping function (see magrittr library)
    DT::datatable(cbind(Default, classification=classification_res), options = list(pageLength = 10)) %>%
      
      #color default column as bold with blue denoting No and orange denoting Yes
      formatStyle('default', color = styleEqual(c("No", "Yes"), c('blue', 'orange')), fontWeight = 'bold') %>%
      
      #show the column as a bar
      formatStyle('balance', background = styleColorBar(Default$balance, 'steelblue')) %>%
      formatStyle('income1k', background = styleColorBar(Default$income1k, 'steelblue')) %>%
      
      #color classification column as bold with blue denoting No and orange denoting Yes
      formatStyle('classification', color = styleEqual(c("No", "Yes"), c('blue', 'orange')), fontWeight = 'bold')
  })
  
  #-------end logistic regression-------
  
  
  #for web mining
  
  #delayed reaction
  wmData <- eventReactive(input$wmRun,{
    #read in the predictor type
    consumerKey = input$wmConsumerKey
    consumerSecret = input$wmConsumerSecret
    accessToken = input$wmAccessToken
    accessSecret = input$wmAccessSecret
    searchString = input$wmSearch
    
    #execute this before calling any API-related calls
    setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessSecret)
    
    #get 100 tweets with the search term
    data.list <- searchTwitter(searchString, n=100)
    
    #convert it into dataframe
    data.df <- twListToDF(data.list)
    
    
    #create the data to be passed on to other renderXXX functions
    data.df
  })
  
  #render plot
  output$wmPlot <- renderPlot({
    #read in the classification details
    data <- wmData()
    
    #remove any illegal symbols
    usableText=str_replace_all(data$text,"[^[:graph:]]", " ") 
    corpus <- Corpus(VectorSource(usableText))
    
    tdm <- TermDocumentMatrix(corpus, control = list(removePunctuation = TRUE, stopwords = stopwords("english"), removeNumbers = TRUE, tolower = TRUE))
    
    m = as.matrix(tdm)
    
    # get word counts in decreasing order
    word_freqs = sort(rowSums(m), decreasing=TRUE)
    dm = data.frame(word=names(word_freqs), freq=word_freqs)
    
    wordcloud(dm$word, dm$freq, random.order=FALSE, colors=brewer.pal(8, "Dark2"), scale=c(8,.2),min.freq=3)
  })
  
  #data table 
  output$wmDT <- DT::renderDataTable({
    data <- wmData()
    
    #render the DT datatable
    #%>% is an infix function - piping function (see magrittr library)
    DT::datatable(data, options = list(pageLength = 10))
  })
  
  #-------end web mining-------
  
  #for svm
  #have to use reactive values for this variable as it changes dynamically
  svmData <- reactiveValues()
  svmData$df <- data.frame(x1 = numeric(0), x2 = numeric(0), class = numeric(0))

  #update the data accordingly for new data
  observeEvent(input$svm_plot_click, {
    numClass <- input$svmClass
    x1Pt <- input$svm_plot_click$y
    x2Pt <- input$svm_plot_click$x
    
    if (numClass == '2'){
      thisClass <- input$svmRadio2
    }
    else{
      thisClass <- input$svmRadio3
    }
    
    newdf <- data.frame(x1=x1Pt, x2=x2Pt, class=thisClass)
    svmData$df = rbind(svmData$df, newdf)
  })
  
  #clear the data
  observeEvent(input$svmClear, {
    svmData$df <- data.frame(x1 = numeric(0), x2 = numeric(0), class = numeric(0))
  })
  
  #clear the data (when change number of classes)
  observeEvent(input$svmClass, {
    svmData$df <- data.frame(x1 = numeric(0), x2 = numeric(0), class = numeric(0))
  })
  
  output$svmPlot <- renderPlot({
    data <- svmData$df
    type <-input$svmType
    numClass <- input$svmClass
    
    if (nrow(data) == 0){
      #show empty plot for 
      plot(1, type="n", xlim = c(-5,5), ylim = c(-5, 5), xlab="x2", ylab="x1")
    }
    else{
      check = FALSE
      if (numClass == '2'){
        check = (1 %in% data$class) && (2 %in% data$class)
      }
      else{
        check = (1 %in% data$class) && (2 %in% data$class) && (3 %in% data$class)
      }
      
      
      #only do the model generation when we have all 3 classes
      #x %in% y check whether x is in y (similar to is.element)
      if (check){
        data$class <- factor(data$class)
        print(data)
        model <- svm(class ~ ., data = data, kernel = type)
        
        #plot the svm model object
        plot(model, data = data)
      }
      else{
        #plot the points normally
        plot(x=data$x2, y = data$x1, xlim = c(-5,5), ylim = c(-5, 5) , pch=c(15,17,19)[data$class], xlab="x2", ylab="x1")
      }
    }
  })
  #-------end svm-------
  
  
  #for decision tree
  
  #delayed reaction
  decisionTreeData <- eventReactive(input$dtRun,{
    #read in the predictor type
    sourceURL = input$dtSourceURL
    colnames = input$dtColnames
    
    data <- read.csv(sourceURL, header = F)
    colnames(data) <- eval(parse(text=colnames))
    
    data
  })
  
  #render plot
  output$dtPlot <- renderPlot({
    #read in the classification details
    data <- decisionTreeData()
    
    colnames(data)[ncol(data)] <- "y"
    
    #create the decision tree model
    model <- rpart(y~., data)
    
    #plot the decision tree
    rpart.plot(model)
  })
  
  #data table 
  output$dtDT <- DT::renderDataTable({
    data <- decisionTreeData()
    
    DT::datatable(data, options = list(pageLength = 10))
  })
  
  #-------end decision tree-------
  
  #for kmeans
  selectedData <- reactive({
    iris[, c(input$xcol, input$ycol)]
  })
  
  clusters <- reactive({
    kmeans(selectedData(), input$clusters)
  })
  
  output$plot1 <- renderPlot({
    palette(c("#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
              "#FF7F00", "#FFFF33", "#A65628", "#F781BF", "#999999"))
    
    par(mar = c(5.1, 4.1, 0, 1))
    plot(selectedData(),
         col = clusters()$cluster,
         pch = 20, cex = 3)
    points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
  })
  #-------end kmeans-------
  
  #TODO: more algos
})

# Run the application 
shinyApp(ui = ui, server = server)

