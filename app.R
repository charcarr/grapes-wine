#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

## dependencies
library(shiny)
library(dplyr)
library(readr)
library(ggplot2)
library(tm)
library(wordcloud)
library(RColorBrewer)
library(forcats)
library(DT)
library(plotly)
library(shinythemes)

wines <- read_csv("winemag-data-130k-v2.csv") %>% 
  filter(is.na(price) == FALSE) %>% 
  filter(is.na(points) == FALSE) %>% 
  filter(is.na(country) == FALSE) %>% 
  mutate(country = as.factor(country))

nobel_grapes <- c("Chardonnay", "Sauvignon Blanc", "Riesling", "Cabernet Sauvignon", 
                  "Pinot Noir", "Merlot","Syrah")
white <- c("Chardonnay", "Sauvignon Blanc", "Riesling") 

my_custom_stopwords <- c("wine",
                         "flavor",
                         "flavors",
                         "flavour",
                         "flavours",
                         "palate",
                         "chardonnay",
                         "riesling",
                         "cabernet",
                         "pinot",
                         "merlot",
                         "sauvignon",
                         "syrah",
                         "fruit",
                         stopwords("english"))

makeWordCloud <- function(documents, col_palette) {
  corpus = Corpus(VectorSource(tolower(documents)))
  corpus = tm_map(corpus, removePunctuation)
  corpus = tm_map(corpus, removeWords, my_custom_stopwords )
  
  frequencies = DocumentTermMatrix(corpus)
  word_frequencies = as.data.frame(as.matrix(frequencies))
  
  words <- colnames(word_frequencies)
  freq <- colSums(word_frequencies)
  wordcloud(words, freq,
            min.freq=sort(freq, decreasing=TRUE)[[40]],
            colors=brewer.pal(8, col_palette),
            random.color= FALSE ,random.order=FALSE) 
}

wine_countries<-wines %>% 
  filter(variety %in% nobel_grapes) %>% 
  group_by(country) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n)) 

# make colour palette 
country_vec <-unique(wine_countries$country)
qual_col_pals = brewer.pal.info[brewer.pal.info$category == 'qual',]
col_vector = unlist(mapply(brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))
names(col_vector) <- country_vec

############################################################################################

## define UI
ui <- fluidPage(theme = shinytheme("yeti"),
                
                # application title
                titlePanel("Noble Grapes"),
                br(),
                "Please select a grape",
                br(),
                
                #user inputs
                sidebarLayout(
                  sidebarPanel(
                    selectInput("varietals", "Grapes",
                                choices = nobel_grapes),
                    br()
                  ),
                  
                  
                mainPanel(
                  tabsetPanel(type = "tabs",
                                
                        tabPanel("Flavour Profile", plotOutput("word_cloud"), br(),
                                 actionButton("button", "View sample description"),br(),br(), textOutput("nText")),
                                
                        tabPanel("Growing Region", br(), br(), br(), br(), plotOutput("bar"), br(), plotOutput("bar2")),
                                
                        tabPanel("Price", br(), br(), br(),"Price vs. Quality Relationship" ,br(), 
                                 plotlyOutput("scatter"), br(),
                                 checkboxInput("show_table", "show data", value = FALSE), br(),  
                                 dataTableOutput("test"))
                    )
                  )
                )
)


## define server
server <- function(input, output, session) {
  
  # make word cloud
  output$word_cloud <- renderPlot({
    grape <-  wines %>% filter(variety == input$varietals)
    if (input$varietals %in% white) {
      makeWordCloud(grape[["description"]][1:50], "YlGn")
    }
    else {
      makeWordCloud(grape[["description"]][1:50], "YlOrRd")
    }
    
  })
  
  ## make text output
  ntext <- eventReactive(input$button, {
    input$n
    grape <- wines %>% filter(variety == input$varietals)
    paste(grape$description[sample(1:nrow(grape), 1)])
  })
  output$nText <- renderText({
    ntext()
  })
  
  
  # make bar plot quantity
  output$bar <- renderPlot({
    grape <- wines %>%  
      filter(variety == input$varietals) %>% 
      group_by(province, country) %>% 
      summarise(count = n()) %>% 
      arrange(desc(count)) %>% 
      head(6) %>% 
      droplevels()
    
    ggplot(grape, aes(x = fct_reorder(province, count, max), y = count)) +
      geom_col(aes(fill = country), alpha = 0.8)+
      coord_flip() +
      scale_fill_manual(name = "country", values = col_vector) +
      labs(x = "region", title = "Top 6 Regions:  Quantity") +
      theme_minimal()
  })
  
  # make bar plot quality
  output$bar2 <- renderPlot({
    grape <- wines %>%  
      filter(variety == input$varietals) %>% 
      group_by(province, country) %>% 
      summarise(avg_rating = mean(points), sd = sd(points)) %>% 
      arrange(desc(avg_rating)) %>% 
      head(6)
    
    ggplot(grape, aes(x = fct_reorder(province, avg_rating, max), y = avg_rating, color = country)) +
      geom_point(size = 5) +
      coord_flip()  +
      scale_color_manual(name = "country", values = col_vector) +
      labs(x = "region", y = "average rating", title = "Top 6 Regions: Average Quality") + 
      theme_minimal()
  })
  
  # make price - points plot 
  output$scatter <- renderPlotly({
    
    grape_plus <-wines %>%  filter(variety == input$varietals, price < 300) %>% 
      arrange(desc(points)) %>% 
      head(500)
    
    # set color for white or reds
    if (input$varietals %in% white) {
      point_col = "palegreen3"
    } else {
      point_col = "tomato2"
    }
    
    plot<-ggplot(grape_plus) +
      geom_jitter(color = point_col, aes(x= points, y = price,
                                         alpha = 0.7, text=sprintf("Points: %s <br> Price: $%s<br>Location: %s, %s",
                                                                   points, price, country, region_1))) + 
      theme_minimal() + 
      labs(y = "price")
    
    ggplotly(plot, tooltip = "text")
  })
  
  # make price - points data table
  output$test <- DT::renderDataTable({
    
    if (input$show_table == FALSE) {
      return(NULL)
    } else {
      # Return entries based on user input
      grape <- wines %>%  
        filter(variety == input$varietals, price < 300) %>% 
        arrange(desc(points)) %>% 
        select(points, price, winery, "region" = region_1 ,province, country) %>% 
        head(500)
    }
    
  }, 
  # data table options
  options = list(
    pageLength = 5
  ),
  # Even more options!
  rownames = TRUE,
  selection = list(
    mode = "single", target = "column",
    selected = 0
  ),
  style = "bootstrap",
  class = "table-bordered table-responsive"
  )
}

## run application
shinyApp(ui = ui, server = server)
