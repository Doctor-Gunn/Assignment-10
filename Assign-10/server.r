server <- function(input, output){
  
  Preds <- reactive({
    generatePreds(
      Petal.Width = input$Petal.Width
      ,Petal.Length = input$Petal.Length
      ,Sepal.Width = input$Sepal.Width
      ,Sepal.Length = input$Sepal.Length
    )
  })
  
  output$pred_table <- DT::renderDataTable({
    Preds() %>%
      datatable() %>%
      formatPercentage(columns = 'preds', digits = 2)
  })
  
  output$plot <- renderPlot({ggplot(data=iris, aes(x = Sepal.Length, y = Sepal.Width)) 
    plot + geom_point(aes(color=Species)) +
      xlab("Sepal Length") +  ylab("Sepal Width") +
      geom_vline(aes(xintercept = mean(Sepal.Length)), color = "red", linetype = "dashed") +
      geom_hline(aes(yintercept = mean(Sepal.Width)), color = "red", linetype = "dashed") +
      ggtitle("Sepal Length-Width")
  })
  
  output$density <- renderPlot({ggplot(data=iris, aes(x=Sepal.Width, fill=Species))
    density + geom_density(stat="density", alpha=I(0.2)) +
      xlab("Sepal Width") +  ylab("Density") + ggtitle("Density Plots")
  })
  
}

