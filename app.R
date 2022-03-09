library(shiny)
library(shinydashboard)
library(tidyverse)
library(ggplot2)
library(plotly)
library(datasets)

states <- map_data("state")
arrests <- USArrests
names(arrests) <- tolower(names(arrests))
arrests$region <- tolower(rownames(USArrests))

choro <- merge(states, arrests, sort = FALSE, by = "region")
choro <- choro[order(choro$order), ]

region.lab.data <- states %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

ui <- dashboardPage(
  dashboardHeader(title="Exploring the US Arrests data with R Shiny Dashboard", titleWidth = 650),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dataset", tabName = "data"),
      menuItem("Scatter Plot", tabName = "sp"),
      menuItem("State wise trend", tabName = "trend"),
      menuItem("Choropleth Map of crimes", tabName = "map")
    )),
  dashboardBody(
    tabItems(
      #first page
      tabItem("data",
              h2("USA Arrests dataset"),
              box(dataTableOutput("dataT"), width= 500)
      ),
      #second page
      tabItem("sp",
              h2("Scatter Plot of Murder with respect to Assault and Rape"),
              box(plotlyOutput("scatter_plot"), width = 500)
      ),
      tabItem("trend",
              h2("State wise Arrests for Murder"),
              box(plotlyOutput("bar1"), width = 500),
              h2("State wise Arrests for Rape"),
              box(plotlyOutput("bar2"), width = 500)
      ),
      #fourth page
      tabItem("map",
              h2("Choropleth map of Assualt"),
              box(plotOutput("map_plot1"), width = 500),
              h2("Choropleth map of Murder"),
              box(plotOutput("map_plot2"), width = 500),
              h2("Choropleth map of Rape"),
              box(plotOutput("map_plot3"), width = 500)
      )
    )
    
  )
)

server <- function(input, output) {
  
  output$dataT <- renderDataTable(USArrests)
  
  output$map_plot1 <- renderPlot({
    
    ggplot(choro, aes(long, lat)) +
      geom_polygon(aes(group = group, fill = assault))+geom_text(aes(label = region), data = region.lab.data,size = 2, hjust = 0.5)
  })
  
  output$map_plot2 <- renderPlot({
    ggplot(choro,aes(long, lat)) +
      geom_polygon(aes(group = group, fill = murder),color = "black")+scale_fill_gradient(low = "yellow", high = "Red")+geom_text(aes(label = region), data = region.lab.data,size = 2, hjust = 0.5)
  })
  output$map_plot3 <- renderPlot({
    ggplot(choro, aes(long, lat)) +
      geom_polygon(aes(group = group, fill = rape),color="white")+scale_fill_gradient(low = "blue", high = "red")+geom_text(aes(label = region), data = region.lab.data,size = 2, hjust = 0.5)
  })
  
  output$bar1 <- renderPlotly({
    choro %>% 
      plot_ly() %>% 
      add_bars(x=~region, y=~murder) %>% 
      layout(title = "Statewise Arrests for Murder",
             xaxis = list(title = "State"),
             yaxis = list(title = "Murder") )
  })
  
  output$bar2 <- renderPlotly({
    choro %>% 
      plot_ly() %>% 
      add_bars(x=~region, y=~rape) %>% 
      layout(title = "Statewise Arrests for Rape",
             xaxis = list(title = "State"),
             yaxis = list(title = "Rape") )
  })
  
  
  output$scatter_plot <- renderPlotly({
    us1 <- plot_ly(USArrests, 
                   x= ~ Murder,
                   y= ~ Assault,
                   name = 'Assault',
                   type = 'scatter'
    ) %>% layout(title = "Murder Vs Assault",
                 xaxis = list(title="Murder"),
                 yaxis = list(title = "Assault"))
    us2 <- plot_ly(USArrests, 
                   x= ~ Murder,
                   y= ~ Rape,
                   name = 'Rape',
                   type = 'scatter'
    ) %>% layout(title = "Murder Vs Rape",
                 xaxis = list(title="Murder"),
                 yaxis = list(title = "Rape"))
    us_new <- subplot(list(us1,us2),
                      nrows = 2, 
                      shareX = TRUE,
                      titleX = FALSE)  %>%
      rangeslider() %>%
      layout(hovermode = "x")
    
    us_new <-  us_new %>% layout(title = "Assault|Rape Vs Murder",
                                 xaxis = list(title="Murder"),
                                 yaxis = list(title = "Assault and Rape "))
  })
}

shinyApp(ui, server)
