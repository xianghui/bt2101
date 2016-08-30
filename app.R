#
# Series of BT2101 demos
#


library(shiny)

# Define UI for application that draws a histogram
ui <- shinyUI(navbarPage("BT2101 Demos",
               #for linear regression
               tabPanel("Linear Regression",
                  sidebarLayout(
                    #left side bar
                    sidebarPanel(
                      sliderInput("lrN", "Number of points", min = 5, max = 100, value = 50,
                                  step = 1),
                      actionButton("lrGenerate", label = "Plot random points")
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
                      actionButton("lrsGenerate", label = "Run Simulation")
                    ),
                    #right main panel
                    mainPanel(
                      plotOutput("lrsPlot", height = "600px"),
                      h6("Red line - actual relationship", align = "center")
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
    data <- list(data = data.frame(x = xVals, y = yVals), 
                 formula = formula, 
                 mod = mod1)
    data
  })
  
  #render the linear regression plot
  output$lrPlot <- renderPlot({
    dataVals = lrData()$data
    mod1 = lrData()$mod
    formula = lrData()$formula
    plot(dataVals$x, dataVals$y, col="red", pch=16, xlab="x", ylab="y")
    
    
    
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
  
  output$lrsEqn <- renderText({
    paste("Actual Equation :", lrsData(), "red line")
  })
  
  #render the linear regression plot
  output$lrsPlot <- renderPlot({
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
    
    #run 10 more simulations
    plot(x,y1, type="n", xlab="X", ylab="Y")
    
    #plot the formula
    eqn2 = function(x){eval(parse(text=eqn))}
    curve(eqn2, col="red", lwd=2, add = T)    
    
    
    for (i in 1:10){
      y <- lapply(x, y_resp)
      y <- c(do.call("cbind", y))
      
      abline(lm(y~x), col="cyan")
    }
  })
  
  #-------end linear regression simulation-------
  
  
  
  #TODO: more algos
})

# Run the application 
shinyApp(ui = ui, server = server)

