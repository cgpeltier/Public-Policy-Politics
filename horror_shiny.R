## Shiny app for JMF network analysis

library(shiny)
library(networkD3)
library(tidyverse)
library(here)
library(shinyWidgets)
library(lubridate)
library(janitor)
library(tidytext)
library(plotly)

customjs2 <- '
function(el,x) { 
    var link = d3.selectAll(".link")
    var node = d3.selectAll(".node")

    var options = { opacity: 1,
                    clickTextSize: 10,
                    opacityNoHover: 0.1,
                    radiusCalculation: "Math.sqrt(d.nodesize)+6"
                  }

    var unfocusDivisor = 4;

    var links = HTMLWidgets.dataframeToD3(x.links);
    var linkedByIndex = {};
    

    links.forEach(function(d) {
      linkedByIndex[d.source + "," + d.target] = 1;
      linkedByIndex[d.target + "," + d.source] = 1;
    });

    function neighboring(a, b) {
      return linkedByIndex[a.index + "," + b.index];
    }

    function nodeSize(d) {
            if(options.nodesize){
                    return eval(options.radiusCalculation);
            }else{
                    return 6}
    }

    function mouseover(d) {
      var unfocusDivisor = 4;

      link.transition().duration(200)
        .style("opacity", function(l) { return d != l.source && d != l.target ? +options.opacity / unfocusDivisor : +options.opacity });

      node.transition().duration(200)
        .style("opacity", function(o) { return d.index == o.index || neighboring(d, o) ? +options.opacity : +options.opacity / unfocusDivisor; });

      d3.select(this).select("circle").transition()
        .duration(750)
        .attr("r", function(d){return nodeSize(d)+5;});

      node.select("text").transition()
        .duration(750)
        .attr("x", 13)
        .style("stroke-width", ".5px")
        .style("font", 24 + "px ")
        .style("opacity", function(o) { return d.index == o.index || neighboring(d, o) ? 1 : 0; });
        
    }

    function mouseout() {
      node.style("opacity", +options.opacity);
      link.style("opacity", +options.opacity);

      d3.select(this).select("circle").transition()
        .duration(750)
        .attr("r", function(d){return nodeSize(d);});
      node.select("text").transition()
        .duration(1250)
        .attr("x", 0)
        .style("font", options.fontSize + "px ")
        .style("opacity", 0);
    }

  
      var svg = d3.select(el).select("svg");
      var mouseout = d3.selectAll(".node").on("mouseout");
      
      
      function mouseout_clicked(d) {
        node.style("opacity", +options.opacity);
        link.style("opacity", +options.opacity);
    
        d3.select(this).select("circle").transition()
          .duration(750)
          .attr("r", function(d){return nodeSize(d);});
        d3.select(this).select("text").transition()
          .duration(1250)
          .attr("x", 0)
          .style("font", options.fontSize + "px ");
      }
      
      function onclick(d) {
        if (d3.select(this).on("mouseout") == mouseout) {
          d3.select(this).on("mouseout", mouseout_clicked);
        } else {
          d3.select(this).on("mouseout", mouseout);
        }
        
        node.select("text").transition()
        .duration(750)
        .attr("x", 13)
        .style("stroke-width", ".5px")
        .style("font", 24 + "px ")
        .style("opacity", function(o) { return d.index == o.index || neighboring(d, o) ? 1 : 0; });
        
      }
      
      
    
      d3.selectAll(".node").on("click", onclick);
    
    
      
      

}


'


## Data
horror3 <- read_csv("horror3.csv")
horror_sim <- read_csv("horror_sim.csv") 


horror_words <- read_csv("horror_words.csv")


word_filter <- c("ghost|vampire|existential|haunted|alien|zomb|demon|devil|cosmic|monster|witch|
                 psycholog|giallo|undead|virus|pandemic|werewolf|sci-fi|occult|supernatural|possession|
                 spirit|disease|nature|curse|myster|suspens|documentary|relig|undead")

horror_words <- horror_words %>%
    filter(str_detect(word, word_filter),
           !str_detect(word, "illegal|rape")) 


## UI 
ui <- fluidPage(
  
  tabsetPanel(

      tabPanel("Find Movies",
               sidebarPanel(
                 pickerInput(inputId = "keywords_choice",
                             label = "Keywords",
                             choices = horror_words %>% arrange(word) %>% pull(word) %>% unique(),
                             options = list(`actions-box` = TRUE,
                                            "max-options" == "false",
                                            `live-search` = TRUE),
                             multiple = TRUE,
                             selected = horror_words %>% filter(str_detect(word, "ghost")) %>% arrange(word) %>% pull(word) %>% unique()),
                 

               ),
               mainPanel(
                 textOutput(outputId = "explanation"),
                 plotlyOutput(outputId = "rating_plot")

               ),


      ),
      
      tabPanel("Movie Recommendations",

               sidebarLayout(
                 sidebarPanel(

                   pickerInput(inputId = "movie_title",
                               label = "Movie",
                               choices = horror3 %>% arrange(desc(imdb_rating)) %>% pull(title) %>% unique(),
                               options = list(`actions-box` = TRUE,
                                              "max-options" == "false",
                                              `live-search` = TRUE),
                               multiple = TRUE,
                               selected = horror3 %>% arrange(desc(imdb_rating)) %>% slice(1:10) %>% pull(title) %>% unique())
                   # dateRangeInput(inputId = "date",
                   #                label = "Event Date",
                   #                start = min(df$date_reported),
                   #                end = max(df$date_reported))
                 ),

                 mainPanel(
                   textOutput("explanation2"),
                   forceNetworkOutput(outputId = "net"),
                   plotOutput(outputId = "similar_plot",
                              height = "600px")

                 ),

               ),


      )
  
  )
  
)



server <- function(input, output, session) {
  
  
  horror_filtered <- reactive({

    horror3 %>%
      filter(title %in% input$movie_title)

  })


  horror_sim_filtered <- reactive({

    horror_sim %>%
      filter(title1 %in% input$movie_title) 

  })
  
  
  horror_select <- reactive({

    keyword_vec <- paste(input$keywords_choice, collapse = "|")

    horror3 %>%
      filter(str_detect(keywords, keyword_vec))

  })

  

  #   
  #   word_similarities <- function(movie) {
  #     tf_idf <- horror_words %>%
  #       left_join(horror3 %>% select(id, title)) %>%
  #       group_by(title) %>%
  #       mutate(normalized = tf_idf / sqrt(sum(tf_idf ^ 2))) %>%
  #       ungroup()
  #     
  #     word_combinations <- tf_idf %>%
  #       filter(title == movie) %>%
  #       select(-title) %>%
  #       inner_join(tf_idf, by = "word", suffix = c("_movie", "_similar")) %>%
  #       filter(title != movie) %>%
  #       mutate(contribution = normalized_movie * normalized_similar) %>%
  #       arrange(desc(contribution)) %>%
  #       select(title, word, tf_idf_movie, tf_idf_similar, contribution)
  #     
  #     word_combinations %>%
  #       filter(title %in% head(horror_sim %>% filter(title1 == movie) %>% pull(title2))) %>%
  #       mutate(title = reorder(title, -contribution, sum),
  #              word = reorder_within(word, contribution, title)) %>%
  #       group_by(title) %>%
  #       top_n(10, contribution) %>%
  #       ungroup() %>%
  #       mutate(word = reorder_within(word, contribution, title)) %>%
  #       ggplot(aes(word, contribution, fill = title)) +
  #       geom_col(show.legend = FALSE) +
  #       scale_x_reordered() +
  #       facet_wrap(~ title, scales = "free_y") +
  #       coord_flip() +
  #       labs(x = "",
  #            y = "Contribution to similarity score") +
  #       theme_light() +
  #       theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) 
  #   }
  #   
    

  
  nodes <- reactive({

    horror_sim_filtered() %>%
      {if(is.null(input$movie_title)) horror_sim_filtered() else horror_sim_filtered() %>% filter(title1 %in% input$movie_title)} %>%
      select(label = title1) %>%
      bind_rows(horror_sim_filtered() %>%
                  {if(is.null(input$movie_title)) horror_sim_filtered() %>% select(label = title2) else horror_sim_filtered() %>% filter(title1 %in% input$movie_title) %>% group_by(title1) %>% arrange(desc(similarity)) %>% top_n(n = 10, wt = similarity) %>% ungroup() %>% select(label = title2)}) %>%
      distinct() %>%
      rowid_to_column("id") %>%
      mutate(id = id-1) 
  })


  edges <- reactive({

    horror_sim_filtered() %>%
      {if(is.null(input$movie_title)) horror_sim_filtered() %>% group_by(title1) %>% top_n(n = 10, wt = similarity) else horror_sim_filtered() %>% filter(title1 %in% input$movie_title) %>% group_by(title1) %>% arrange(desc(similarity)) %>% top_n(n = 10, wt = similarity) %>% ungroup()} %>%
      select(title1, title2, similarity) %>%
      ungroup() %>%
      left_join(nodes() %>% rename(from = id), by = c("title1" = "label")) %>%
      left_join(nodes() %>% rename(to = id), by = c("title2" = "label")) %>%
      mutate(value2 = 1) %>%
      select(from, to, title1, title2, similarity, value2)

  })



  most_similar <- reactive({

    horror_sim_filtered() %>%
      group_by(title1) %>%
      top_n(n = 10, wt = similarity) %>%
      select(title1, title2, similarity, imdb_rating1, imdb_rating2) %>%
      ungroup() %>%
      arrange(desc(imdb_rating1))

  })

  
  
  
## Render outputs
  
  output$net <- renderForceNetwork({

    htmlwidgets::onRender(forceNetwork(Links = edges(),
                                       Nodes = nodes(),
                                       Source = "from",
                                       Target = "to",
                                       NodeID = "label",
                                       Group = "id",
                                       Value = "similarity",
                                       opacity = 1,
                                       fontSize = 12,
                                       arrows = TRUE,
                                       zoom = TRUE), customjs2)

  })
  
  
  output$similar_plot <- renderPlot({

    most_similar() %>%
      mutate(title1 = factor(title1, levels = unique(horror_filtered() %>%
                                                        select(title, imdb_rating) %>%
                                                        distinct() %>%
                                                        arrange(desc(imdb_rating)) %>%
                                                        pull(title))),
             title2 = reorder_within(title2, similarity, title1)
                                     ) %>%
      ggplot(aes(similarity, title2, fill = imdb_rating2)) +
      geom_col() +
      labs(y = "", fill = "IMDB Rating", x = "") +
      theme_classic() +
      scale_y_reordered() +
      facet_wrap(~title1, scales = "free", ncol = 2)

  })
  
  
  output$rating_plot <- renderPlotly({

      p <- horror_select() %>%
          filter(year > 1920) %>%
          mutate(metacritic_score = round(metacritic_score,0)) %>%
          ggplot(aes(year, imdb_rating, label = title, size = gross_world,
                     alpha = 0.4, color = metacritic_score)) +
          geom_point() +
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
          guides(alpha = FALSE, color = FALSE) +
          labs(x = "Year", y = "IMDB Rating") +
          theme_classic()
      
      ggplotly(p, tooltip = c("Title" = "title", 
                              "metacritic_score", 
                              "imdb_rating", 
                              "gross_world"))


  })

  
  
  output$explanation <- renderText({
    
    print("Select IMDB keyword(s) to find horror movies. Plot displays all horror movies from 1920-Present, sized by global revenue and colored by Metacritic score.")
    
  })
  
  output$explanation2 <- renderText({
    
    print("Network chart displays the top 10 most similar movies based on IMDB keywords. Bar charts display tf-idf cosine similarity scores.")
    
  })
  
  
}

shinyApp(ui = ui, server = server)












