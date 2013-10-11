# source("dataprep.R") #
# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  datasetInput<- reactive({
    switch(input$dataset,
           "Physical"=physical,
           "Cognitive"=cognitive,
           "Emotional"=emotional)
  })
#   
#   # Generate an HTML table view of the data
#   output$table <- renderTable({
#   # fpmShort is used to create the table of values for the tabset "Table"
#   patterns[which(patterns$Rotation==input$rotation & 
#                      dsFORp$Kappa==input$kappa &
#                      dsFORp$Oblique==input$oblique),]
#     
  })