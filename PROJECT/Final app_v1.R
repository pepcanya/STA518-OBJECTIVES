college <- read.csv("college.csv")
#country <- read.csv("by_country15_16.csv")
intlalls<-read.csv("by_country20_21.csv")
gender<-read.csv("Gender.csv")
ethnicity<-read.csv("by_ethnicity.csv")
state<-read.csv("state_19.csv")
intlall<-read.csv("by_country20_21.csv", stringsAsFactors = FALSE)


# Load R packages
library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(IRdisplay)
library(gganimate)
library(googlesheets4)
library(tidyverse)
library(gifski)
library(plotly)


# Define UI
ui <- fluidPage(theme = shinytheme("cerulean"),
                navbarPage(
                  #theme = "cerulean",  # <--- To use a theme,
                  "Graduate Student Data Visualization",
                  tabPanel("College and Departments",
                           sidebarPanel(
                             tags$h3("Input:"),
                             selectInput("year", "Choose Year", choices = unique(college$Year)),
                             h4("Table of Department vs Student Count of each choosen year: "),
                             tableOutput("duration_table"),
                           ), # sidebarPanel
                           mainPanel(
                             h4("Distribution of students of different departments for each year: "),
                             imageOutput("college_data"),
                             h4("   "),
                             h4("   "),
                             h4("   "),
                             h4("   "),
                             h4("   "),
                             h4("Horizontal Barchart of Total Student for each Department of all Six years: "),
                             plotOutput("barplot_collegesum"),
                             #h4("Histogram: "),
                             #plotOutput("hist"),
                             ) # mainPanel
                           
                  ), # Navbar 1, tabPanel 1
                       tabPanel("Gender", 
                           h4("Gender Visualizations of all Six years of data"),
                           sidebarPanel(
                             h3("Doughnut of Gender Distribution of all students for Six years "),
                            # tags$h3("Choose Year:"),
                            # selectInput("g_year", "Choose Year", choices = unique(gender$Year)),
                            # selectInput("year", "Choose Year", choices = unique(college$Year)),
                             plotOutput("gender_pie")
                           
                             
                             ),
                           
                           mainPanel(
                             h3("Stacked Barplot of Gender Distribution of all students for Six years "),
                             plotOutput("gender_stack"),
                             
                            #imageOutput("gender_data")
                           )
                           ),
                  tabPanel("Ethnicity",
                           h4("Ehnicity Visualizations of all years"),
                           sidebarPanel(
                             h4("Pie Chart of all ethnicities for all years"),
                             plotOutput("ethnicity_pie")
                           ),
                           
                           mainPanel(
                             h4("Line Chart of all ethnicities for each year"),
                             # plotOutput("ethnicity"),
                             imageOutput("ethnicity_data")
                             
                             
                           )
                  )
                  ,
                  tabPanel("Countries",
                           sidebarPanel(
                             tags$h3("Input:"),
                             selectInput("year_country", "Choose Year", choices = unique(intlalls$Year)),
                             tableOutput("country_table"),
                             
                           ), # sidebarPanel
                           mainPanel(
                             h4("2d Visualizations"),
                             plotOutput("country_2d"),
                             h4("3d Visualizations"),
                             plotOutput("country_3d"),
                           
                           )
                      ),
                  
                 tabPanel("States", 
                             sidebarPanel(
                               h4("State Visualizations")
                               ),
                               
                             mainPanel(
                               plotOutput("state_data")
                             
                             
                             ))
                ) # navbarPage
) # fluidPage


# Define server function  
server <- function(input, output) {
  
#Departments-------------------------------------------------------------
  
  output$college_data <-renderImage({
   outfile <- tempfile(fileext='.gif')
  p= ggplot(college, aes( y = data, x = college, fill=college))+geom_bar(stat='identity')+ theme_bw() +transition_states(Year, transition_length = 2,state_length = 1) +labs(title = 'Year: {closest_state}',subtitle  = "Departments Visualization of Graduate Student from 2015 to 2020")
    
  anim_save("outfile.gif", animate(p,height=350,width=800,fps=20,duration=20,end_pause=60,res=120))
  list(src = "outfile.gif",
      contentType = 'image/gif',
     width = 800,
    height = 400
         # alt = "This is alternate text"
  )}, deleteFile = TRUE)
  
 
  
  #output$barplot_allyears <-renderPlot({barplot(college$data)})
  
  output$barplot_collegesum <- renderPlot({
    
    
    df <- college %>% group_by(college) %>% summarize(data = sum(data))
    g <- ggplot(df, aes( y = data, x = college,fill=college))
    p<- g + geom_bar(stat = "sum")
    p + coord_flip()
  
  })
  
  #output$hist <-renderPlot({
  #  x    <- college$data
  #  x    <- na.omit(x)
  #  hist(x, breaks = 10, col = "#75AADB", border = "black",
   #      xlab = "Data",
    #     main = "Histogram of college")
   #})
  
  output$duration_table <- renderTable({
    
    df<- college %>% filter(Year==input$year)
    df 
  })
 
  
#Country-----------------------------------------------------------------
  
  output$country_table <- renderTable({
   intlall<- select(intlall, Country, Total,Year)
    
    df<- intlall %>% filter(Year==input$year)
    df 
  })
  
  
  output$country_2d <-renderPlot({
    library(ggmap)
    intlall<-read.csv("by_country20_21.csv", stringsAsFactors = FALSE)
    intlall<- intlall %>% filter(Year==input$year_country) #%>% group_by(Country) %>% summarize(Total = sum(Total))
    world_map=map_data("world")
    world_map=merge(world_map,intlall,by.x="region",by.y="Country")
    world_map=world_map[order(world_map$group, world_map$order),]
    ggplot(world_map, aes(x=long, y=lat, group=group)) +geom_polygon(aes(fill=Total), color="red")+coord_map("mercator")
    
  })
  
  output$country_3d <-renderPlot({
    library(ggmap)
    intlall<-read.csv("by_country20_21.csv", stringsAsFactors = FALSE)
    intlall<- intlall %>% filter(Year==input$year_country) #%>% group_by(Country) %>% summarize(Total = sum(Total))
    world_map=map_data("world")
    world_map=merge(world_map,intlall,by.x="region",by.y="Country")
    world_map=world_map[order(world_map$group, world_map$order),]
    ggplot(world_map, aes(x=long, y=lat, group=group)) +geom_polygon(aes(fill=Total), color="black")+coord_map("ortho", orientation = c(20,50,10))
  })
  
#State------------------------------------------------------------------------------  
  output$state_data <-renderPlot({
    library(usmap)
    library(ggplot2)
    
    plot_usmap(data = statepop, values = "pop_2015", color = "red") + 
      scale_fill_continuous(name = "Population (2015)", label = scales::comma) + 
      theme(legend.position = "right")
  })

  #ethnicity-----------------------------------------------------------------------------
 
  output$ethnicity_pie<-renderPlot({
    library(ggplot2)
    ggplot(ethnicity, aes(x="", y=Total, fill=Ethnicity)) +
    geom_bar(stat="identity", width=1, color="white") +
    coord_polar("y", start=0) +
    theme_void()
    
  })
  
  
   output$ethnicity <-renderPlot({
    
   g <- ggplot(ethnicity, aes( y = Total, x = Year,color=Ethnicity))
  g + geom_line()+theme_bw()
    
  })
  
   output$ethnicity_data <-renderImage({
    outfile <- tempfile(fileext='.gif')
    p= ggplot(ethnicity, aes( y = Total, x = Year, fill=Ethnicity,color=Ethnicity))+
       geom_line(stat='identity')+ theme_bw() + transition_reveal(Year)
       #transition_states(Year, transition_length = 2,state_length = 1) +
       #labs(title = 'Year: {closest_state}',subtitle  = "Gender Visualization of Graduate Student from 2015 to 2020")
    
    anim_save("outfile.gif", animate(p,height=400,width=800,fps=20,duration=20,end_pause=60,res=120))
    list(src = "outfile.gif",
         contentType = 'image/gif'
         # width = 400,
         # height = 300,
         # alt = "This is alternate text"
    )}, deleteFile = TRUE)
  
  
    
#Gender-----------------------------------------------------
   
  # output$gender_pie<-renderPlot({
     
   #  df<- gender %>% filter(Year==input$year)
    # hsize <- 4
     #df <- df %>% 
      # mutate(x = hsize)
     
     #ggplot(df, aes(x = hsize, y = Total, fill = Sex)) +geom_col() + coord_polar(theta = "y") +      xlim(c(0.2, hsize + 0.5))
     
  # })
   
   
   output$gender_pie<-renderPlot({
   library(ggplot2)
   hsize <- 8
   gender <- gender %>% mutate(x = hsize)
   ggplot(gender, aes(x = hsize, y = Total, fill = Sex)) +geom_col() + coord_polar(theta = "y") + xlim(c(0.2, hsize + 0.5))
   
   })
   
   
   
   
   output$gender_stack <-renderPlot({
      ggplot(gender, aes( y = Total, x = Sex, fill=Sex))+geom_bar(stat='identity')
     ggplot(gender, aes(fill=Sex, y=Total, x=Year)) + 
       geom_bar(position="dodge", stat="identity")
       })
   
   
  #output$gender_data <-renderImage({
   # outfile <- tempfile(fileext='.gif')
   #p= ggplot(gender, aes( y = Total, x = Sex, fill=Sex))+geom_bar(stat='identity')+ theme_bw() +transition_states(Year, transition_length = 2,state_length = 1) +labs(title = 'Year: {closest_state}',subtitle  = "Gender Visualization of Graduate Student from 2015 to 2020")
    
   #anim_save("outfile.gif", animate(p))
   #list(src = "outfile.gif",
    #    contentType = 'image/gif'
        # width = 400,
        # height = 300,
        # alt = "This is alternate text"
   #)}, deleteFile = TRUE)
 
 

}

# Create Shiny object
shinyApp(ui = ui, server = server)