
                                            # BUAN6357 ASSIGNMENT-1 (Shiny)

# Name: Nithin S Nair
#Last updated: June 19, 2019
#--------------------------------------------------------------------------------------------------------------#

#RELATIVE WORKING DIRECTORY
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

##  IMPORTING REQUIRED PACKAGES
#Packages will be installed if not available in the device

packages <- c("dplyr","tidyverse","tidyr","DT","shiny","ggplot2","ggmap")
packages_new <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(packages_new)) install.packages(packages_new)

library(dplyr)
library(tidyverse)
library(tidyr)
library(DT)
library(shiny)
library(ggplot2)
library(ggmap)


# Reading Data
collision.df <- read.csv("traffic-collision-data-from-2010-to-present.csv", stringsAsFactors = F)


## Extracting Latitudes and Longitudes from Location

collision.df <- tidyr::separate(data=collision.df,
                                col=Location,
                                into=c("Latitude", "Longitude"),
                                sep=",",
                                remove=FALSE)

collision.df$Latitude <- stringr::str_replace_all(collision.df$Latitude, "\\{'latitude': '", "")
collision.df$Latitude <- stringr::str_replace_all(collision.df$Latitude, "'", "")
collision.df$Longitude <- stringr::str_replace_all(collision.df$Longitude, " 'longitude': '", "")
collision.df$Longitude <- stringr::str_replace_all(collision.df$Longitude, "'", "")

collision.df$Latitude <- as.numeric(collision.df$Latitude)
collision.df$Longitude <- as.numeric(collision.df$Longitude)


## Extracting Month and Year from Date Occurred

collision.df <- tidyr::separate(data=collision.df,
                                col=Date.Occurred,
                                into=c("Year","Month"),
                                sep="-",
                                remove=FALSE)
collision.df$Year <- as.numeric(collision.df$Year)
collision.df$Month <- as.numeric(collision.df$Month)

## Filtering Values of Year 2018
collision.df <- collision.df %>% filter(Year == 2018)


# Extract Hour of the Accident
collision.df$Hour <- as.numeric(collision.df$Time.Occurred)%/%100

#--------------------------------------------------------------------------------------------------------------#


# Define UI for application

ui <- fluidPage(
  # App title
  titlePanel("Vehicle Collisions in Los Angeles"),
 
   # Sidebar layout with input and output definitions
  sidebarLayout(
    
    # Sidebar panel for inputs
    sidebarPanel(
      
      sliderInput(inputId="Mon",label = "Month of the Year (Select 0 to see for the whole year)",value = 0,min = 0,max = 12)
      
),
    
    # Outputs
mainPanel(
  tabsetPanel(type = "tabs",id = "tabselected",
              tabPanel("Bar Chart", value=1, plotOutput(outputId = "BarPlot", height=500),textOutput(outputId = "frequency")),
              tabPanel("Location of Collisions in Map",value=2, plotOutput(outputId = "maplocation", height=500)),
              tabPanel("Heat Map", value=3, plotOutput(outputId = "Heatmap", height=500))
    )
  )
 )
)


# Define Server

server <- function(input, output) {
  
# Create Barplot
  
  output$BarPlot <- renderPlot({
    filtered.df <- collision.df
    if (input$Mon >=1) {
      filtered.df <- filtered.df %>%filter(filtered.df$Month == input$Mon)
    }
    ggplot(data=filtered.df, aes(x=filtered.df$Hour)) +
      geom_bar(aes(y = (..count..))) + theme_minimal()+xlab("Hour of the Day")+ylab("Frequency")+
      ggtitle("Frequency of Collisions by the Time of the Day")
  })
  
# Create text output stating the top frequency
  output$frequency <- renderText({
    filtered.df <- collision.df
    if (input$Mon >=1) {
      filtered.df <- filtered.df %>%filter(filtered.df$Month == input$Mon)
    }
    histo <- hist(filtered.df$Hour, breaks=24, freq=TRUE)
    Bin <- histo$breaks[which.max(histo$counts)]
    #Top <- max(histo$counts)
    Top <- round(max(histo$counts)*100/length(filtered.df$Hour),2)
    paste0("The highest number of collisions occur in the hour ",Bin+1," and its Percentage Frequency is ", Top)
  })


 
# Rendering Map

  filtered2.df <- collision.df%>%filter(Latitude!=0, Longitude!=0)
    output$maplocation <- renderPlot({
    if (input$Mon >=1) {
      filtered2.df <- filtered2.df %>%filter(filtered2.df$Month == input$Mon)
    }
  maploc<- qmplot(Longitude, Latitude, data=filtered2.df, geom = "blank",
                    zoom = 11, maptype = "terrain", darken = .5)+
        ggtitle("Map Locations of Collisions")
  maploc+geom_point(aes(Longitude,Latitude),data = filtered2.df, size =0.1, color ="red")
  })
  
    
# Rendering Heatmap
  
  output$Heatmap <- renderPlot({
    filtered.df <- collision.df
    if (input$Mon >=1) {
      filtered.df <- filtered.df %>%filter(filtered.df$Month == input$Mon)
    }
    ggplot(filtered.df, aes(x = filtered.df$Area.Name, y = filtered.df$Hour))+
      geom_bin2d()+
      scale_fill_gradient(low="white", high="red")+
      ggtitle("Heatmap of Locations of Collisions") +
      labs(x="Area Name of Collision",y="Hour of the day") 
  })
}
  

# Creating a Shiny app object
shinyApp(ui = ui, server = server)

#--------------------------------------------------------------------------------------------------------------#
