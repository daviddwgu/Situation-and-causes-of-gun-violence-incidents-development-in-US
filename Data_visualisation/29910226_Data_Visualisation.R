#load library
require(shiny)
require(maps)
require(leaflet)
require(ggplot2)

#set work directory
#setwd("C:/Users/David/Desktop/FIT5147_A3/Data_visualisation")

#load q1 dataset
q1 <- read.csv('q1.csv')
q1[,'count'] <- 1

#load first type of q2 data and make the state name to lower for map data matching
q2_1 <- read.csv('q2_1.csv')
q2_1$state_1 <- tolower(q2_1$state)

#load second type of q2 data and make the state name to lower for map data 
# matching, also make the unemployment rate ready to show in the line chart.
q2_2 <- read.csv('q2_2.csv')
q2_2$state_1 <- tolower(q2_2$state)
q2_2[,'unemployment'] <- q2_2[,'unemployment']*600

#load the q3 data, and make the state name to lower for map data matching. 
q3 <- read.csv('q3.csv')
q3$state_1 <- tolower(q3$state)

#set up the map data, and make the name of state properly
mapstates <-map('state',fill=TRUE, plot=FALSE)
splitename <- strsplit(mapstates$names, ':')
firstpartnames <- lapply(splitename, function(x) x[1])
mapstates$names <- firstpartnames


#ui design
ui<-shinyUI(
  navbarPage("My Application",
             
             #first page which is used to describe the project
             tabPanel("About",
                      #title of page
                      titlePanel("Situation and causes of gun violence 
                                 incidents development in US"),
                      mainPanel(h3('Project Overview'),
                                p("The gun incidents in US is a very hot topic, 
                                  there is data set recording more than 230k gun 
                                  incidents happened in US from 2013 to 2018. We
                                  want to know that what causes the gun incidents
                                  and how does gun incidents develop.The following 
                                  is the 3 question need to ask."),
                                h4('Q1'),
                                p('Q1. How are the age group and gender involving 
                                in gun violence incidents?'), 
                                h4('Q2'),
                                p('Q2. What is the number gun violence incidents 
                                develop in each state by years? Does trend align
                                with unemployment rate?'), 
                                h4('Q3'),
                                p('Q3. What the major causes of gun violence in 
                                  each state?')
                                ),
                      ),
             
             #second page used to display the q1
             tabPanel(
               "Q1",
               #title of page
               titlePanel(h3('Q1. How are the age group and gender involving in gun
                          violence incidents? ')),
               
               mainPanel(
                 #arrange two groups of selection buttons
                 fluidRow(
                 splitLayout(cellWidths = c("90%", "100%"), 
                             radioButtons("agegroup", label = "Age Group", choices = 
                                            list("Overall" = 1, "Child" = 2, 'Teen'=3,
                                                 'Adult'=4,'Elder'=5)),
                             radioButtons("gender", label = "Gender", choices = 
                                            list("Overall" = 1, "Female" = 2,'Male' = 3)))),
                 #arrange two plots for the q1
                 fluidRow(
                 splitLayout(cellWidths = c("70%", "95%"), 
                             plotOutput("Q1_1_plot"),plotOutput("Q1_2_plot"))),
                  #user guide for the q1 plot in the second page
                  p('User Guide: Choose the gender and age group on the top, the 
                            plots will change as selection'))
             ),
             
             #third page used to display the q2
             tabPanel("Q2",
                      #title of page
                      titlePanel(h3('Q2. What is the number gun violence incidents 
                                develop in each state by years? Does trend align
                                with unemployment rate?')),
                      mainPanel(
                        #arrange the drop down selection box for the map 
                        selectInput("static", "Static:",
                                    c("Count" = 1,
                                      "Kill or injured" = 2,
                                      "Unemployment rate" = 3)),
                        #arrange two plots for the q2
                        fluidRow(
                          splitLayout(cellWidths = c("70%", "85%"), 
                                      leafletOutput("Q2_1_plot"), plotOutput("Q2_2_plot"))),
                        #user guide for the q2 plot in the third page
                        p('User Guide: '),
                        p('- Select the type of statisic in the top left drop down selection box will change the map on the left'),
                        p('- For map in the left side, Darker the color, higher the value.'),
                        p('- Mouse over the state of map will show the name of state.'),
                        p('- Click on the state on the map will show the line plot on the right for that state.'),
                        p('- For right side line plot, the left side y axis is for number of cases and number of kill or injured, the right side y axis is for unemployment rate.')
                        )
                      ),
             # fourth page used to display the q3
             tabPanel("Q3",
                      #title of page
                      titlePanel(h3('Q3. What the major causes of gun violence in 
                                  each state?')),
                      mainPanel(
                        # drop down selection box for the selection fo reason
                        selectInput("reason", "Reason: ",
                                    unique(q3$reason)),
                        #arrange two plots for the q3
                        fluidRow(
                          splitLayout(cellWidths = c("65%", "80%"), 
                                      leafletOutput("Q3_1_plot"), plotOutput("Q3_2_plot"))),
                        #user guide for the q3 plots in the fourth page
                        p('User Guide: '),
                        p('- Select the reason in the top left drop down selection box will change the map on the left'),
                        p('- For map in the left side, Darker the color, higher the value.'),
                        p('- Mouse over the state of map will show the information of gun incident caused by that reason in that state.'),
                        p('- Click on the state on the map will show the Bar graph of top 5 reason gun incident on the right for that state.')
                        )
                      )
  )
)

#server function
server <- function(input, output, session) {
  #first plot for q1 
  output$Q1_1_plot <- renderPlot({
    # base on the input select of age group, change the data and title of plot for certain age group
    if (input$agegroup == 1){
      temp_data<- aggregate(count ~ gender, FUN = sum, data =q1)
      temp_title <- 'The Bar graph of number of people involving in gun incidents'
    }
    else if (input$agegroup == 2){
      temp_data <- aggregate(count ~ gender, FUN = sum, data =q1[q1$age_group == "Child 0-11", ])
      temp_title <- 'The Bar graph of number of people involving in gun incidents (Child)'
    }
    else if (input$agegroup == 3){
      temp_data <- aggregate(count ~ gender, FUN = sum, data =q1[q1$age_group == "Teen 12-17", ])
      temp_title <- 'The Bar graph of number of people involving in gun incidents (Teenager)'
    }
    else if (input$agegroup == 4){
      temp_data <- aggregate(count ~ gender, FUN = sum, data =q1[q1$age_group == "Adult 18+", ])
      temp_title <- 'The Bar graph of number of people involving in gun incidents (Adult)'
    }
    else if (input$agegroup == 5){
      temp_data <- aggregate(count ~ gender, FUN = sum, data =q1[q1$age_group == "Elder 65+", ])
      temp_title <- 'The Bar graph of number of people involving in gun incidents (Elder)'
    }
    #bar plot for certain age group
    ggplot(data=temp_data, aes(x=gender, y=count, fill=gender)) + geom_bar(stat="identity") + ggtitle(temp_title)+geom_text(aes(label=count), vjust=1.6, color="white", size=3.5)+theme_minimal()
  })
  
  #second plot for q1
  output$Q1_2_plot <- renderPlot({ 
    if (input$gender == 1){
      temp_data <- q1
      temp_title <- 'Histogram of age of people involving in the gun incidents'
    }
    else if(input$gender == 2){
      temp_data <- q1[q1$gender == "Female", ]
      temp_title <- 'Histogram of age of people involving in the gun incidents (Female)'
    }
    else if(input$gender == 3){
      temp_data <- q1[q1$gender == "Male", ]
      temp_title <- 'Histogram of age of people involving in the gun incidents (Male)'
    }
    ggplot(data=temp_data, aes(temp_data$age)) + geom_histogram(bins=30, col='red',fill="steelblue")+ ggtitle(temp_title) + labs(x="Age", y="Count") + stat_bin(aes(y=..count.., label=..count..), geom="text", vjust=-.5)+ theme_minimal()
    })
  
  output$Q2_1_plot <- renderLeaflet({
    if (input$static == 1){
      rates <- q2_1$count[match(mapstates$names,q2_1$state_1)]
      cpal <- colorNumeric('Reds', rates)
    }
    else if (input$static == 2){
      rates <- q2_1$kill[match(mapstates$names,q2_1$state_1)]
      cpal <- colorNumeric('Reds', rates)
    }
    else if (input$static == 3){
      rates <- q2_1$unemployment[match(mapstates$names,q2_1$state_1)]
      cpal <- colorNumeric('Reds', rates)
    }
    state_label <- q2_1$state[match(mapstates$names, q2_1$state_1)]
    leaflet(mapstates) %>%
      addTiles() %>%
      addPolygons(
        stroke = TRUE,
        smoothFactor = 0.6,
        fillOpacity = 1,
        color = "black", 
        opacity = 0.03,
        fillColor = ~cpal(rates),
        label=state_label,
        layerId = state_label
      )
  })
  
  output$Q2_2_plot <- renderPlot({
    if (is.null(input$Q2_1_plot_shape_click[1])){
      temp_state <- 'District of Columbia'
    }
    else{
      temp_state <- input$Q2_1_plot_shape_click[1]
    }
    temp_data <- q2_2[q2_2$state == temp_state, ]
    ggplot(temp_data) +
      ggtitle(temp_state)+
      geom_line(aes(x=year, y=count, color='count'))+
      geom_line(aes(x=year, y=killed_injured, color='killed or injured'))+
      geom_line(aes(x=year, y=unemployment, color='unemployment_rate'))+
      scale_color_manual(name = "Y", 
                         values = c("count" = "blue", "unemployment_rate" = "red", 'killed or injured' = 'green')) +
      scale_y_continuous(limits=c(0, 6000), 
                         sec.axis = sec_axis(~ . *1/600, 
                                             name = "unemployment"))
  })
  
  output$Q3_1_plot <- renderLeaflet({
    temp_data <- q3[q3$reason == input$reason, ]
    rates <- temp_data$count[match(mapstates$names, temp_data$state_1)]
    state_label <- temp_data$label[match(mapstates$names, temp_data$state_1)]
    state_name <- temp_data$state[match(mapstates$names, temp_data$state_1)]
    cpal <- colorNumeric('Reds', rates)
    leaflet() %>%
      addTiles() %>%
      addPolygons(
        data = mapstates,
        stroke = TRUE,
        smoothFactor = 0.6,
        fillOpacity = 1,
        color = "black", 
        opacity = 0.03,
        fillColor = ~cpal(rates),
        label=state_label,
        layerId = state_name
      )
  })
  
  output$Q3_2_plot <- renderPlot({
    if (is.null(input$Q3_1_plot_shape_click$id)){
      temp_state <- 'District of Columbia'
    }
    else{
      temp_state <- input$Q3_1_plot_shape_click$id
    }
    temp_data_2 <- q3[q3$state == temp_state, ]
    ggplot(data=temp_data_2, aes(x=reason, y=count, fill=reason)) + 
      ggtitle(temp_state)+
      geom_bar(stat="identity") +
      geom_text(aes(label=count), vjust=1.6, color="white", size=3.5)+ 
      theme(axis.text.x=element_blank()) 
  })
}

options(warn=-1)
shinyApp(ui, server)