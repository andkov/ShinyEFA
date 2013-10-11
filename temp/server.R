# source("dataprep.R") # 
# 
# # Reads the matrix containing solutions for all rotations and values of kappa
# patterns<-read.csv("data/fpm.csv") # short form
# # Transforms dataset into a long format to be used in ggplot
# dsFORp <- reshape2::melt(patterns, id.vars=c("Oblique","Rotation","Kappa","Varname"))  ## id.vars declares MEASURED variables (as opposed to RESPONSE variable)
# dsFORp <- plyr::rename(dsFORp, replace=c(variable="factor",value="loading"))
# dsFORp$positive <- dsFORp$loading >= 0 # is factor loading positive? color coded in ggplot
# dsFORp$loading<-abs(as.numeric(dsFORp$loading)) # Long form

# Define server logic for random distribution application
shinyServer(function(input, output) {
   datasetInput<- reactive({
     switch(input$dataset,
            "Physical"=physical,
            "Cognitive"=cognitive,
            "Emotional"=emotional)
   })
  
   selectedRotation <- reactive({switch(input$rotation,
                       "Unrotated" = "none",
                       "Varimax" = "varimax",
                       "Promax" = "promax",
                       "Quartimax" = "quartimax",
                       "Quartimin" = "quartimin",
                       "Crawford-Ferguson"="CF")
                         
  selectKappa <- reactive(function(){
    return(input$kappa)
  })
 
  selectOblique <-reactive(function(){
    return(input$oblique)
  })
                                
  selectNfactors <- reactive(function(){
    return(input$nfactors)
  })                               
    })

   output$plot <- renderPlot({
#      # # fpmLong is used to produce the graph of factor loadings
     fpmLong<-dsFORp[which(dsFORp$Rotation==input$rotation & 
                             dsFORp$Kappa==input$kappa &
                             dsFORp$Oblique==input$oblique),]
#      
     # The colors for negative and positve values of factor loadings
     colors<- c("darksalmon" ,"lightskyblue")
     title<-"Basic Title"

    
     p<-ggplot(fpmLong, aes(x=factor, y=loading, fill=positive))+
       ggtitle(title)+ 
       geom_bar(stat="identity")+
       scale_fill_manual(values=colors)+
       scale_y_continuous(limits=c(0,1))+
       theme(axis.text.x =element_text(angle=0,hjust=.5))+
       facet_grid(Varname~.)
     print(p)
     
  })
  

  # Generate an HTML table view of the data
  output$table <- renderTable({
    # fpmShort is used to create the table of values for the tabset "Table"
    
    patterns[which(patterns$Rotation==input$rotation & 
                     dsFORp$Kappa==input$kappa &
                     dsFORp$Oblique==input$oblique),]
    
    })
   output$plot <- RenderPlot({
     
   })
})