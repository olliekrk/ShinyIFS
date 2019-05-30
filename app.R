#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
set.seed(2137)

barnsley_fern_preset <- function(xy){
  f1 <- function(xy) matrix(c(0,0,0,.16), nrow = 2) %*% xy #stem
  f2 <- function(xy) matrix(c(.2,.23,-.26,.22), nrow = 2) %*% xy + c(0,1.6) #left-hand leaflet
  f3 <- function(xy) matrix(c(-.15,.26,.28,.24), nrow = 2) %*% xy + c(0,.44) #right-hand leaflets
  f4 <- function(xy) matrix(c(.85,-.04,.04,.85), nrow = 2) %*% xy + c(0,1.6) #smaller leaflets
  
  results <- c(f1,f2,f3,f4)
  probs <- c(0.01, 0.07,0.07,0.85)
  chosen_i <- sample(1:4, 1, prob=probs)
  
  as.matrix(switch(chosen_i,f1(xy),f2(xy),f3(xy),f4(xy)))
}

# w R sie zapisuje KOLUMNAMI !

heighway_dragon_preset <- function(xy){
  f1 <- function(xy) matrix(c(.5,.5,-.5,.5), nrow = 2) %*% xy
  f2 <- function(xy) matrix(c(-.5,.5,-.5,-.5), nrow = 2) %*% xy + c(1,0)
  
  results <- c(f1,f2)
  probs <- c(.5,.5)
  chosen_i <- sample(1:2, 1, prob=probs)
  
  as.matrix(switch(chosen_i,f1(xy),f2(xy)))
}

sierpinski_gasket_preset <- function(xy){
  f1 <- function(xy) matrix(c(.5,0,0,.5), nrow = 2) %*% xy
  f2 <- function(xy) matrix(c(.5,0,0,.5), nrow = 2) %*% xy + c(.5,0)
  f3 <- function(xy) matrix(c(.5,0,0,.5), nrow = 2) %*% xy + c(0,.5)
  
  results <- c(f1,f2,f3)
  probs <- c(.33,.33,.33)
  chosen_i <- sample(1:3, 1, prob=probs)
  
  as.matrix(switch(chosen_i,f1(xy),f2(xy),f3(xy)))
}

clone_preset <- function(xy){
  f1 <- function(xy) matrix(c(.14,0,.01,.51), nrow = 2) %*% xy + c(-.08, -1.31)
  f2 <- function(xy) matrix(c(.43,-.45,.52,.5), nrow = 2) %*% xy + c(1.49,-.75)
  f3 <- function(xy) matrix(c(.45,.47,-.49,.47), nrow = 2) %*% xy + c(-1.62,-.74)
  f4 <- function(xy) matrix(c(.49,0,0,.51), nrow = 2) %*% xy + c(.02,1.62)
  
  results <- c(f1,f2,f3,f4)
  probs <- c(.1, .35,.35,.2)
  chosen_i <- sample(1:4, 1, prob=probs)
  
  as.matrix(switch(chosen_i,f1(xy),f2(xy),f3(xy),f4(xy)))
}

tree_preset <- function(xy){
  f1 <- function(xy) matrix(c(.01,0,0,.45), nrow = 2) %*% xy
  f2 <- function(xy) matrix(c(-.01,0,0,-.45), nrow = 2) %*% xy + c(0,.4)
  f3 <- function(xy) matrix(c(.42,.42,-.42,.42), nrow = 2) %*% xy + c(0,.4)
  f4 <- function(xy) matrix(c(.42,-.42,.42,.42), nrow = 2) %*% xy + c(0,.4)
  
  results <- c(f1,f2,f3,f4)
  probs <- c(.25,.25,.25,.25)
  chosen_i <- sample(1:4, 1, prob=probs)
  
  as.matrix(switch(chosen_i,f1(xy),f2(xy),f3(xy),f4(xy)))
}

mandelbrot_preset <- function(xy){
  f1 <- function(xy) matrix(c(.202,-.689,-.805,-.342), nrow = 2) %*% xy + c(-.0373, -.653)
  f2 <- function(xy) matrix(c(.138,-.502,.665,-.222), nrow = 2) %*% xy + c(.66,-.277)
  
  results <- c(f1,f2)
  probs <- c(.5,.5)
  chosen_i <- sample(1:2, 1, prob=probs)
  
  as.matrix(switch(chosen_i,f1(xy),f2(xy)))
}

spiral_preset <- function(xy){
  f1 <- function(xy) matrix(c(.787879,.242424,-.424242,.859848), nrow = 2) %*% xy + c(1.758647, 1.40865)
  f2 <- function(xy) matrix(c(-.121212,.151515,.257576,.053030), nrow = 2) %*% xy + c(-6.721654,1.377236)
  f3 <- function(xy) matrix(c(.181818,.090909,-.136364,.181818), nrow = 2) %*% xy + c(6.086107,1.568035)
  
  results <- c(f1,f2,f3)
  probs <- c(.9,.05,.05)
  chosen_i <- sample(1:3, 1, prob=probs)
  
  as.matrix(switch(chosen_i,f1(xy),f2(xy),f3(xy)))
}

custom_preset <- function(xy, a, b, c, d, e, f){
  f1 <- function(xy) matrix(c(a,b,c,d), nrow = 2) %*% xy + c(e,f)
  as.matrix(f1(xy))
}

ifs_generate <- function(preset_fun, iterations, points){
  results <- data.frame(matrix(ncol=4, nrow=0))
  
  # Initial xs and ys
  xys <- rbind(seq(0,1,by=(1.0/points)),0)
  
  for (pt in 1:ncol(xys)){
    xy <- as.vector(xys[,pt])
    
    init_row <- c(pt, 0, xy[1], xy[2])
    results <- rbind(results, init_row)
    
    for (iter in 1:iterations){
      next_xys <- preset_fun(xy)
      
      for (next_xy in 1:ncol(next_xys)){
        next_x <- next_xys[1,next_xy]
        next_y <- next_xys[2,next_xy]
        
        # Add iteration step
        new_row <- c(pt, iter, next_x, next_y)
        results <- rbind(results, new_row)
      }
      
      xy <- as.vector(next_xys[,ncol(next_xys)])
    }
  }
  colnames(results) <- c("point_no", "iteration_no", "x", "y")
  results
}

# Define UI for application
ui <- fluidPage(
  
  # Application title
  titlePanel("Iterated Function System Fractals"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(width=4,
                 selectInput(inputId = "preset",
                             label = "Choose a preset:",
                             choices = c(
                               "Barnsley's Fern",
                               "Clone leaf",
                               "Tree",
                               "Mandelbrot-like",
                               "Heighway's Dragon",
                               "Sierpinski's Gasket",
                               "Spiral",
                               "Custom")
                 ),
                 sliderInput("a",
                             "Parameter a:",
                             min = -1,
                             max = 1,
                             value = -.6,
                             step = 0.01),
                 sliderInput("b",
                             "Parameter b:",
                             min = -1,
                             max = 1,
                             value = -.1,
                             step = 0.01),
                 sliderInput("c",
                             "Parameter c:",
                             min = -1,
                             max = 1,
                             value = .1,
                             step = 0.01),
                 sliderInput("d",
                             "Parameter d:",
                             min = -1,
                             max = 1,
                             value = .6,
                             step = 0.01),
                 sliderInput("e",
                             "Parameter e:",
                             min = -1,
                             max = 1,
                             value = -.8,
                             step = 0.01),
                 sliderInput("f",
                             "Parameter f:",
                             min = -1,
                             max = 1,
                             value = -.2,
                             step = 0.01),
                 numericInput("iterations",
                              label = "Number of interations:",
                              min = 0,
                              value = 1000,
                              step = 100),
                 numericInput("points",
                              label = "Number of points:",
                              min = 1,
                              value = 10,
                              step = 1)
    ),
    
    # Show a plot of the generated fractal
    mainPanel(
      fluidRow(class = "text-center",
               column(12, align = "center",
                      h3(textOutput("description", container= span)),
                      h6(textOutput("x_equation")),
                      h6(textOutput("y_equation")),
                      actionButton("generate", "Generate fractal"))
      ),
      
      plotOutput("distPlot")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # For fractals presets
  presetInput <- reactive({
    paste("Chosen parameters preset: ", input$preset)
  })
  
  output$x_equation <- renderText({
    paste("x' = ", input$a, "x + ", input$b, "y + ", input$e, sep="")
  })
  
  output$y_equation <- renderText({
    paste("y' = ", input$c, "x + ", input$d, "y + ", input$f, sep="")
  })
  
  output$description <- renderText({
    presetInput()
  })
  
  observeEvent(input$generate, {
    chosen_preset <- function(xy) {custom_preset(xy,input$a,input$b,input$c,input$d,input$e,input$f)}
    
    switch(input$preset,
           "Heighway's Dragon"={chosen_preset <- heighway_dragon_preset},
           "Barnsley's Fern"={chosen_preset <- barnsley_fern_preset},
           "Sierpinski's Gasket"={chosen_preset <- sierpinski_gasket_preset},
           "Clone leaf"={chosen_preset <- clone_preset},
           "Tree"={chosen_preset <- tree_preset},
           "Mandelbrot-like"={chosen_preset<-mandelbrot_preset},
           "Spiral" = {chosen_preset<-spiral_preset},
           {})
    
    results <- ifs_generate(chosen_preset,input$iterations, input$points)

    n_colors <- input$points +1
    rbPalette <- colorRampPalette(c('red','blue'))
    results$color <- rbPalette(n_colors)[as.numeric(cut(results$y, breaks = n_colors))]
    
    output$distPlot<- renderPlot({
      plot(results$x,
           results$y,
           asp = 1,
           axes = FALSE,
           type = "p",
           xlab="",
           ylab="",
           pch=20,
           cex=0.1,
           bty="n",
           col=results$color)}, height = 1000, width = 1000)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

