rm(list=ls(all=TRUE))

library(shiny)
# setwd("C:/Users/kovalav/Documents/GitHub/ShinyEFA")
# setwd("D:/Users/Will/Documents/GitHub/ShinyEFA") #Will's Development Box
# setwd("~/ShinyEFA") #Will's Experimental Shiny server at home: http://lucky1304:8100/
server <- (Sys.info()["nodename"] == "Lucky1304")
shiny::runApp('shinyApp', launch.browser=!server)

# shiny::runApp("ShinyApp")
