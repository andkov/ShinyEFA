library(shiny)
pattern<-read.csv("shinyapp/fpm.csv")

# Define server logic for random distribution application
shinyServer(function(input, output) {
  
  # Reactive expression to generate the requested distribution. This is 
  # called whenever the inputs change. The renderers defined 
  # below then all use the value computed from this expression
  data <- reactive({  
    dist <- switch(input$dist,
                   norm = rnorm,
                   unif = runif,
                   lnorm = rlnorm,
                   exp = rexp,
                   rnorm)
    
    dist(input$n)
  })
  kappa <- input$kappa
  # Generate a plot of the data. Also uses the inputs to build the 
  # plot label. Note that the dependencies on both the inputs and
  # the 'data' reactive expression are both tracked, and all expressions 
  # are called in the sequence implied by the dependency graph
  output$plot <- renderPlot({
   #####                     #####
    colors<- c("darksalmon" ,"lightskyblue")
    title<- paste0("Shiny pattern")
    ylims<-c(0,3)           # max for eigenvalue plot 
    width<-450              # width of pattern in pixels
    height<-900             # height of pattern in pixels
    width2<-300            # width of scree in pixels
    height2<-200            # height of scree in pixels
    
    dsLong <- reshape2::melt(pattern, id.vars=c("Varname","kappa"))  ## id.vars declares MEASURED variables (as opposed to RESPONSE variable)
    dsLong <- plyr::rename(dsLong, replace=c(variable="factor",value="loading"))
    dsFORp<-dsLong
    dsFORp$positive <- dsFORp$loading >= 0 # positive value?
    dsFORp$loading<-abs(dsFORp$loading)
    head(dsFORp,20)
    str(dsFORp)
    
    dsFORp<-dsFORp[which(dsFORp$kappa==kappa),] # make kappa= dynamic
    
    pT<-ggplot(dsFORp, aes(x=factor, y=loading, fill=positive))+
      ggtitle(title)+ 
      geom_bar(stat="identity")+
      scale_fill_manual(values=colors)+
      scale_y_continuous(limits=c(0,1))+
      theme(axis.text.x =element_text(angle=0,hjust=1))+
        facet_grid(Varname ~.)
  
  })
  
  # Generate a summary of the data
  output$summary <- renderPrint({
    summary(data())
  })
  
  # Generate an HTML table view of the data
  output$table <- renderTable({
    data.frame(x=data())
  })
})