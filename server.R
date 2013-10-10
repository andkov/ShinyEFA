library(shiny)
library(datasets)
library(ggplot2) # load ggplot


patterns<-read.csv("./data/fpm.csv")
dsFORp <- reshape2::melt(patterns, id.vars=c("Oblique","Rotation","Kappa","Varname"))  ## id.vars declares MEASURED variables (as opposed to RESPONSE variable)
dsFORp <- plyr::rename(dsFORp, replace=c(variable="factor",value="loading"))
dsFORp$positive <- dsFORp$loading >= 0 # positive value?
dsFORp$loading<-abs(as.numeric(dsFORp$loading))

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
   datasetInput <- reactive({  
    rotation <- switch(input$rotation,
                       "Unrotated" = "none",
                       "Varimax" = "varimax",
                       "Promax" = "promax",
                       "Quartimax" = "quartimax",
                       "Quartimin" = "quartimin",
                       "Crawford-Ferguson"="cf")
})
###
   output$plot <- renderPlot({
#    dsLong<-patterns[which(Rotation==input$rotation & Oblique==input$oblique )]
   
     colors<- c("darksalmon" ,"lightskyblue")
     title<-"Basic Title"
     
     fpmShort<-patterns[which(fpm$Rotation==input$rotation),]
     fpm<-dsFORp[which(fpm$Rotation==input$rotation),]
    
     p<-ggplot(dsFORp, aes(x=factor, y=loading, fill=positive))+
       ggtitle(title)+ 
       geom_bar(stat="identity")+
       scale_fill_manual(values=colors)+
       scale_y_continuous(limits=c(0,1))+
       theme(axis.text.x =element_text(angle=0,hjust=.5))+
       facet_grid(Varname~.)
     p
     
  })
  

  # Generate an HTML table view of the data
  output$table <- renderTable({
    data.frame(datasetInput())
  })
})