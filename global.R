library(shinyjs)
library(shinyhelper)
library(shiny)
library(DT)
library(shinyWidgets)
library(ggplot2)
library(shinydashboard)
library(xml2)
library(plotly)
library(shinyBS)
library(shinycssloaders)
library(formattable)
library(dplyr)
library(tools)
library(httr)
library(gghighlight)
library(dplyr)
library(writexl)
library(plotly)
library(tinytex) # formattable dependency
library(jsonlite)
library(scales)
library(doBy)
library(readxl)
library(stringr)
library(htmltools)
library(heatmaply)
library(xml2)
library(dendextend)
library(hrbrthemes)
library(openxlsx)
library(rlang)
library(seqinr)


#load main dataset
# dset <- read.csv("DSET_ALL2.csv")
dset <- read.csv("DSET_ALL3.csv")

#dset=dset[,-1]
heat_test <- read.csv("HEAT_DSET.csv")

# page 1
dset_PTs <- read.delim("PTs.txt",header=TRUE)
vars<-dset_PTs$PT_TERM

# page 2
dset_SUBs <- read.csv("SUBs.csv")
vars2<-dset_SUBs$INAME

#page 4
l1 <- read.delim("l1.txt", header=TRUE)$c1
l2 <- read.delim("l2.txt", header=TRUE)$c2
l3 <- read.delim("l3.txt", header=TRUE)$c3
l4 <- read.delim("l4.txt", header=TRUE)$c4

# collapse other boxes when a new one is opened
collapseInput <- function(inputId, boxId) {
  tags$script(
    gsub("%s", boxId,
         "$('#%s').closest('.box').on('hidden.bs.collapse', function () {
        if('%s' === 'box1a') col_1a = true;
        else if('%s' === 'box1b') col_1b = true;
        else if('%s' === 'box1c') col_1c = true;
      })
      
      
      $('#%s').closest('.box').on('shown.bs.collapse', function () {
        if('%s' === 'box1a') col_1a = false;
        else if('%s' === 'box1b') col_1b = false;
        else if('%s' === 'box1c') col_1c = false;
        if(!col_1a){
          if('%s' != 'box1a'){
              $('#box1a').closest('.box').find('[data-widget=collapse]').click(); 
              col_1a = true;
            }
        }
        if(!col_1b){
          if('%s' != 'box1b'){
              $('#box1b').closest('.box').find('[data-widget=collapse]').click(); 
              col_1b = true;
            }
        }
        if(!col_1c){
          if('%s' != 'box1c'){
              $('#box1c').closest('.box').find('[data-widget=collapse]').click(); 
              col_1c = true;
            }
        }
      })
      ")
  )
}

plotHeight <- 800
used <- new.env(hash = TRUE)  # map for used pt terms
mp <- new.env(hash=TRUE)
for(i in 1:nrow(dset)){
  mp[[dset$INAME[i]]][[dset$PT_TERM[i]]]=dset$PRR[i]
}
