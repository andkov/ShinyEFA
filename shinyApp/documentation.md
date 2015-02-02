<!-- Don't forget to update these two files simultaneously: /README.md and /shinyApp/documentation.md -->

ShinyEFA is a web application created with [R](http://cran.rstudio.com/) and [Shiny](http://www.rstudio.com/shiny/). It is created and maintained by [Andrey Koval](http://www.statcanvas.net) (Vanderbuilt University) and [Will Beasley](http://www.linkedin.com/profile/view?id=48089881&trk=nav_responsive_tab_profile) (University of Oklahoma Health Sciences Center). 

ShinyEFA uses datasets from the [psych package](http://cran.r-project.org/web/packages/psych/psych.pdf) by William Revelle and [gradient projection algorithms](http://www.stat.ucla.edu/research/gpa/) by Bernaards and Jennrich. The original factor pattern matrices are obtained from an unrotated solution of the [factanal function](http://rss.acs.unt.edu/Rdoc/library/stats/html/factanal.html) in the stats package. Advanced factor functions by [James Steiger](www.statpower.net) are used for RMSEA diagnostic and [corrgram](http://www.datavis.ca/papers/corrgram.pdf) package by [Michael Friendly](http://www.datavis.ca/) from [York University](http://qm.info.yorku.ca/)

The application is hosted on RStudio's Glimmer server [http://glimmer.rstudio.com/wibeasley/ShinyEFA/](http://glimmer.rstudio.com/wibeasley/ShinyEFA/), and the public GitHub code repository is available at [https://github.com/kavnoff/ShinyEFA/](https://github.com/kavnoff/ShinyEFA/).  Questions and comments are welcomed at Andrey's gmail account (koval.andrey).



*Selected References*
 * Revelle, W. (2013) psych: Procedures for Personality and Psychological Research, Northwestern University, Evanston, Illinois, USA, [http://CRAN.R-project.org/package=psych](http://CRAN.R-project.org/package=psych), Version = 1.3.10.
 * Bernaards, C. A., & Jennrich, R. I. (2005). Gradient projection algorithms and software for arbitrary rotation criteria in factor analysis. Educational and Psychological Measurement, 65(5), 676-696.
 * Browne, M. W. (2001). An overview of analytic rotation in exploratory factor analysis. Multivariate Behavioral Research, 36(1), 111-150.
 * Crawford, C. B., & Ferguson, G. A. (1970). A general rotation criterion and its use in orthogonal rotation. Psychometrika, 35(3), 321-332. 
 * Jennrich, R. I. (2004). Rotation to simple loadings using component loss functions: The orthogonal case. Psychometrika, 69(2), 257-273.
