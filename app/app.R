library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(DT)

library(tidyverse)
library(imager)

#################################################################
load("dt.Rdata")
EDA1 <- qplot(data=dt,x=Rating,fill=Genre,bins=30) +
    labs(title="Ratings of Top 50 Comedy TV Shows",
         y="Count")
EDA2 <- ggplot(dt,aes(x=Rank,y=Rating)) +
    geom_point(aes(size=Votes,color=Runtime)) +
    labs(title="Ratings of Top 50 Comedy TV Shows",
         x="Rank")

wc <- load.image("wc.jpg")
load("ff.Rdata")
sen_com <- ff %>%
    ggplot(aes(x=Word,y=sentiment,fill=method)) +
    geom_col() +
    theme(axis.text.x=element_text(angle=45,size=9)) +
    facet_wrap(~method,ncol=1,scales="free_y")

load("Y_plot.Rdata")
load("df_lines.Rdata")
YS_rating <- Y_plot %>% 
    ggplot(aes(x=rn,y=rating)) +
    geom_hline(data = tibble(y = 7:10), 
               aes(yintercept = y),
               color = '#D3D3D3') +
    geom_segment(aes(y = avg_rating, 
                     yend = avg_rating, 
                     x = start, 
                     xend = end,
                     color = season,
                     color = after_scale(colorspace::darken(color, .1))), 
                 lwd = 2.5) +
    geom_segment(aes(y = rating, 
                     yend = avg_rating,
                     x = rn, 
                     xend = rn,
                     color = season,
                     color = after_scale(colorspace::darken(color, .3)))) +
    geom_segment(data = df_lines, 
                 aes(x = end, 
                     xend = lag_rn,
                     y = s_avg,
                     yend = lag_rating,
                     color = season,
                     color = after_scale(colorspace::darken(color, .3))),
                 lwd = .7) +
    geom_point(aes(color = season, 
                   color = after_scale(colorspace::darken(color, .3))),
               size = 3) +
    geom_label(aes(label = glue::glue('Season {season}'), 
                   x = median,
                   y = 9.3,
                   color = season,
                   color = after_scale(colorspace::darken(color, .3))), 
               label.padding = unit(.3, "lines"),
               label.r = unit(.25, "lines"),
               label.size = .8,
               size = 5) +
    scale_x_continuous(expand = c(.005, .005)) + 
    scale_y_continuous(breaks = seq(7, 9.4, by = .5), 
                       limits = c(7,9.6), 
                       sec.axis = dup_axis(name = NULL)) + 
    scale_color_brewer(palette = 'Set3') + 
    labs(title = '"Young Sheldon" Rating',
         y = 'IMDb Rating',
         x = 'Episode') + 
    guides(color = FALSE) + 
    theme(plot.title = element_text(size = rel(2), hjust = 0.5),
          plot.subtitle = element_text(size = rel(1.2), hjust = 0.5)) 

#################################################################
ui <- dashboardPage(
    dashboardHeader(title = "IMDb"),
    
    dashboardSidebar(  
        sidebarMenu(
            menuItem("Top 50 Comedies", tabName = "Comedies", icon = icon("dashboard")),
            menuItem("EDA", tabName = "EDA", icon = icon("dashboard")),
            menuItem("Comedies Description", tabName = "Des", icon = icon("dashboard")),
            menuItem("Young sheldon", tabName = "YS", icon = icon("dashboard")))),
    
    dashboardBody(
        shinyDashboardThemes(
            theme = "blue_gradient"
        ),
        tabItems(
            tabItem(tabName = "Comedies",
                    DTOutput("table")),
            
            tabItem(tabName = "EDA",
                    box(plotOutput("EDA1")),
                    box(plotOutput("EDA2"))),
            
            tabItem(tabName = "Des",
                    box(plotOutput("wc")),
                    box(plotOutput("sen_com"))),
            
            tabItem(tabName = "YS",
                    textOutput("YS1"),
                    plotOutput("YS2"),
                    textOutput("YS3"))
        )
    )
)

#################################################################
server <- function(input, output) {
    output$table <-renderDT({datatable(dt)})
    
    output$EDA1 <- renderPlot({EDA1})
    output$EDA2 <- renderPlot({EDA2})
    
    output$wc <- renderPlot({plot(wc,xlim = c(1,width(wc)),ylim = c(height(wc),1))})
    output$sen_com <- renderPlot({sen_com})
    
    output$YS1 <- renderText({"Young Sheldon is a spin-off of The Big Bang Theory. It mainly tells a series of stories about Sheldon living in Texas with his family in his childhood."})
    output$YS2 <- renderPlot({YS_rating})
    output$YS3 <- renderText({"The horizontal lines in the plot is the average rating of each season, while the vertical lines represent the distance to the seasons' average rating."})
}

#################################################################
shinyApp(ui = ui, server = server)
