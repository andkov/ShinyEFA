library(shiny)
library(ggplot2)

#Declare factor variables so they  have nice labels. Since this doesn't rely on any user inputs,
#   we can do this once at startup and then use the value throughout the lifetime of the application

pathInput <- file.path(getwd(), "Data/FakeData.csv")
ds <- read.csv(file=pathInput, stringsAsFactors=F)
ds$Gender <- factor(ds$Gender, levels=1:2, labels=c("Male", "Female"))
ds$MaritalStatus <- factor(ds$MaritalStatus, levels=1:7, labels=c("Unmarried", "Married", "Engaged", "Dating", "Single", "DivorcedOnce", "DivorcedMultiple"))

# Define server logic required to plot various variables against mpg
shinyServer(function(input, output) {

  DV <- "Development"
  IV1 <- reactive(function() {
    paste(input$Variable)
  })
  GenderValue <- reactive(function() {
    return( input$Gender )
  })  
  AgeValue <- reactive(function() {
    return( input$Age )
  })
  MaritalStatusValue <- reactive(function() {
    return( input$MaritalStatus )
  })
  ChildCountValue <- reactive(function() {
    return( input$ChildCount )
  })
  IncomeValue <- reactive(function() {
    return( input$Income )
  })  
  DevelopmentValue <- reactive(function() {
    return( input$Development )
  })
  RowCountValue <- reactive(function() {
    return( input$RowCount )
  })
  
  
  output$DatasetPath <- reactiveText(function() {
    paste("Dataset:", pathInput)
  })
  output$IVLabel <- reactiveText(function() {
    paste("Predictor Variable:", IV1())
  })
  
  output$View <- reactiveTable(function() {
    head(ds, n=RowCountValue())
  })
  output$AnovaTable <- reactiveTable(function() {
    anova(SummarizeModel())
  })
  output$ModelSummary <- reactivePrint(function() {
    summary(SummarizeModel())
  })
   
  AnalyzeModel <- function( ) {
    modelForumula <- paste0("Development ~ 1 + ",  IV1())
    return( lm(modelForumula, data=ds) )
  }
  
  Prediction <- function( ) {
    dsNew <- data.frame(
      SubjectID=0,
      Gender=GenderValue(),
      Age=AgeValue(), 
      MaritalStatus=MaritalStatusValue(),
      ChildCount=ChildCountValue(),
      Income=IncomeValue(),
      Development=DevelopmentValue()
    )
    m <- AnalyzeModel()
    prediction <- predict(m, newdata=dsNew, interval="prediction")
    return( c(prediction[1, "lwr"], prediction[1, "upr"]) )
  }
  
  SummarizeModel <- function( ) {
    m <- AnalyzeModel()
    return( m ) 
  }
  
  
  output$Bounds <- reactiveText(function() {
    pred <- Prediction()
    paste0("Most patients with your value of ", IV1(), " fall between ", round(pred[1]), " and ", round(pred[2]), " points on the Development scale.")
  })
  
  output$GGPlot <- reactivePlot(function() {

    pred <- Prediction()
    g <- ggplot(data=ds, aes_string(x=IV1(), y=DV)) 
    if( IV1()=="Gender") {
      g <- g %+% aes_string(colour=IV1(), fill=IV1())
      #   g <- g + stat_smooth(formula=y ~ 1 + x , geom="smooth", method="lm", se=TRUE, aes(group=Gender))
      g <- g + geom_violin(alpha=.1)
      if( input$ShowModel ) {
        g <- g + annotate(geom="errorbar", x=GenderValue(), ymin=pred[1], ymax=pred[2], color="purple", size=2, width=.2)
      }
      g <- g + geom_point(shape=1, position=position_jitter(w=0.1, h=0.1), alpha=.5)
      g <- g + theme(legend.position="none")
      if( input$ShowSubject ) {
        g <- g + annotate(geom="point", x=GenderValue(), y=DevelopmentValue(), size=5, colour="green3")#, shape=4)
      }  
    }
    if( IV1()=="Age") {
      if( input$ShowModel ) {
        g <- g + annotate(geom="errorbar", x=AgeValue(), ymin=pred[1], ymax=pred[2], color="purple", size=2, width=2)
      } 
      g <- g + geom_point(shape=1, alpha=.5)
      
      #   if( input$ShowModel ) {
      #     g <- g + stat_smooth(formula=y ~ 1 + x + I(x^2), geom="smooth", method="lm", se=TRUE)
      #   }
      
      if( input$ShowSubject ) {
        g <- g + annotate(geom="point", x=AgeValue(), y=DevelopmentValue(), size=5, colour="green3")#, shape=4)
      }
    }
    if( IV1()=="MaritalStatus") {
      g <- g %+% aes_string(colour=IV1(), fill=IV1())
      if( input$ShowModel ) {
        g <- g + annotate(geom="errorbar", x=MaritalStatusValue(), ymin=pred[1], ymax=pred[2], color="purple", size=2, width=.2)
      }
      #g <- g + geom_violin(alpha=.1)
      g <- g + geom_boxplot(alpha=.1, outlier.size=0)
      g <- g + geom_point(shape=1, position=position_jitter(w=0.1, h=0.1), alpha=.5)
      g <- g + theme(legend.position = "none")
      if( input$ShowSubject ) {
        g <- g + annotate(geom="point", x=MaritalStatusValue(), y=DevelopmentValue(), size=5, colour="green3")#, shape=4)
      }
    }
    if( IV1()=="ChildCount") {
      if( input$ShowModel ) {
        g <- g + annotate(geom="errorbar", x=ChildCountValue(), ymin=pred[1], ymax=pred[2], color="purple", size=2, width=.5)
      } 
      g <- g + geom_point(shape=1, position=position_jitter(w=0.1, h=0.1), alpha=.5)
      #   g <- g + stat_smooth(formula=y ~ 1 + x + I(x^2), geom="smooth", method="lm", se=TRUE)
      if( input$ShowSubject ) {
        g <- g + annotate(geom="point", x=ChildCountValue(), y=DevelopmentValue(), size=5, colour="green3")#, shape=4)
      }
    }
    if( IV1()=="Income") {
      if( input$ShowModel ) {
        g <- g + annotate(geom="errorbar", x=IncomeValue(), ymin=pred[1], ymax=pred[2], color="purple", size=2, width=5000)
      }   
      g <- g + geom_point(shape=1, alpha=.5)
      #   g <- g + stat_smooth(formula=y ~ 1 + x + I(x^2), geom="smooth", method="lm", se=TRUE)
      if( input$ShowSubject ) {
        g <- g + annotate(geom="point", x=IncomeValue(), y=DevelopmentValue(), size=5, colour="green3")#, shape=4)
      }
    }
    g <- g + scale_colour_brewer(palette="Set1")
    g <- g + scale_fill_brewer(palette="Set1")
    
    
    
    
    print(g)
  })
})
