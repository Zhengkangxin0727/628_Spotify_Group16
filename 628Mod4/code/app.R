
library(shiny)
library(wordcloud2)
library(ggplot2)
library(dplyr)
library(readr)
library(entropy)


lda_df <- read_csv('Copy of lda_df.csv') 
bayes_df <- read_csv('Copy of bayes_df.csv') 


combined_df <- full_join(lda_df, bayes_df, by = "Episode ID") %>%
  select(-ends_with(".y")) %>%
  rename_with(~ gsub("\\.x$", "", .), ends_with(".x"))

combined_metrics <- combined_df %>%
  mutate(
    LDA_Max = apply(select(., starts_with("Topic_")), 1, max),
    LDA_Entropy = apply(select(., starts_with("Topic_")), 1, function(x) entropy::entropy(x)),
    Bayes_Max = apply(select(., ends_with("_Score")), 1, max),
    Bayes_Mean = rowMeans(select(., ends_with("_Score")))
  ) %>%
  filter(complete.cases(.)) # Remove rows with missing values
topic_labels <- c(
  "0" = "Politics & Election",
  "1" = "Business & Success",
  "2" = "Sports & NFL",
  "3" = "Health & Wellness",
  "4" = "Personal Finance",
  "5" = "Investment & Tax",
  "6" = "Gaming & Sports Media",
  "7" = "Podcasts & Subscriptions",
  "8" = "Sleep & Relaxation",
  "9" = "Promotions & Discounts",
  "10" = "AI & Technology",
  "11" = "True Crime & Stories"
)
# K-means Clustering
set.seed(123)
cluster_data <- combined_metrics %>% select(PCA1, PCA2) %>% na.omit()
kmeans_result <- kmeans(cluster_data, centers = 4)
combined_metrics$Cluster <- as.factor(kmeans_result$cluster)

ui <- fluidPage(
  tags$head(tags$style(HTML("
        body { 
            background-color: #222222; 
            color: #ddd; 
            font-family: Arial, sans-serif; 
        }
        h4 { 
            color: #1DB954; 
            font-weight: bold; 
            text-align: center; 
        }
        .panel { 
            background-color: #333333; 
            border-radius: 10px; 
            padding: 15px; 
            box-shadow: 0 4px 8px rgba(0,0,0,0.3);
        }
        .plot-container {
            padding: 10px;
            background-color: #333333; 
            border-radius: 10px;
        }
        .episode-details {
            padding: 15px;
            background-color: #333333;
            color: #ddd;
            border-radius: 10px;
            box-shadow: 0px 4px 8px rgba(0,0,0,0.5);
        }
        .contact-info {
            background-color: #333333;
            color: #ddd;
            padding: 10px;
            border-radius: 10px;
            margin-top: 10px;
        }
        .nearest-episodes {
            color: #ddd;
            font-size: 14px;
            line-height: 1.5;
        }
    "))),
  
  # 标题
  titlePanel(tags$h1("Podcast Analysis Dashboard", style = "color:#1DB954; text-align: center;")),
  
  # 布局
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "episode", 
        "Choose Episode Name:",
        choices = unique(combined_metrics$`Episode Name`)
      ),
      
      div(
        tags$h4("Contact Information", style = "color: #1DB954; text-align: center;"),
        tags$p("Created by Group 16: "),
        tags$p('Tianyu Yao/Kangxin Zheng'),
        tags$p("For any questions or feedback, please contact:"),
        tags$p('tyao39@wisc.edu'),
        tags$p('kzheng59@wisc.edu'),
        class = "contact-info"
      ),
      style = "background-color: #1DB954; border-radius: 10px; color: white; padding: 15px;"
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Episode Information",
                 fluidRow(
                   column(12, tags$h4("Word Cloud"), 
                          div(wordcloud2Output("wordcloud"), class = "panel")),
                   column(12, tags$h4("Topic-Based Clustering"), 
                          div(plotOutput("topic_pca"), class = "plot-container"))
                 )
        ),
        tabPanel("Nearest Episodes",
                 fluidRow(
                   column(12, tags$h4("Nearest Episodes PCA"), 
                          div(plotOutput("nearest_pca"), class = "plot-container")),
                   column(12, tags$h4("Top 10 Nearest Episodes"), 
                          div(uiOutput("episode_details"), class = "episode-details nearest-episodes"))
                 )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  get_episode_text <- function(episode_name, data) {
    text <- data %>% filter(`Episode Name` == episode_name) %>% pull(Description)
    return(text)
  }
  output$selected_topic <- renderText({
    req(input$episode) 
    selected_episode <- combined_metrics %>% filter(`Episode Name` == input$episode)
    
    if (nrow(selected_episode) > 0) {
      topic_index <- as.character(selected_episode$Dominant_Topic)  
      topic_label <- topic_labels[topic_index]  
      paste("Selected Topic:", topic_label)
    } else {
      "No topic available."
    }
  })
  
  output$wordcloud <- renderWordcloud2({
    episode_text <- get_episode_text(input$episode, combined_metrics)
    words <- unlist(strsplit(episode_text, "\\s+"))
    word_freq <- as.data.frame(table(words))
    wordcloud2(word_freq,color = "random-light",backgroundColor = "#333333")
  })
  
  output$topic_pca <- renderPlot({
    # 选中 episode 的数据
    selected_row <- combined_metrics %>% filter(`Episode Name` == input$episode)
    
    ggplot(combined_metrics, aes(x = PCA1, y = PCA2, color = factor(Dominant_Topic))) +
      geom_point(alpha = 0.7) +
      # 标记选中的 episode
      geom_point(data = selected_row, aes(x = PCA1, y = PCA2), 
                 color = "red", size = 5, shape = 21, fill = "yellow") +
      geom_label(data = selected_row, aes(x = PCA1, y = PCA2, 
                                          label = `Episode Name`), color = "white", fill = "black", 
                 fontface = "bold", label.padding = unit(0.2, "lines"), 
                 vjust = -1.2, hjust = 0.5) +
      labs( x = "PCA1", y = "PCA2", color = "Topic") +
      scale_color_manual(values = RColorBrewer::brewer.pal(12, "Set3"), 
                         labels = topic_labels) +
      theme(
        panel.background = element_rect(fill = "gray20", color = NA),
        plot.background = element_rect(fill = "gray20", color = NA),
        panel.grid.major = element_line(color = "gray30"),
        panel.grid.minor = element_line(color = "gray25"),
        axis.text = element_text(color = "white", size = 12),
        axis.title = element_text(color = "white", size = 14, face = "bold"),
        legend.background = element_rect(fill = "gray20"),
        legend.text = element_text(color = "white"),
        legend.title = element_text(color = "#66CDAA", face = "bold")
      )
  })
  
  output$episode_details <- renderUI({
    # 获取选中 episode 的信息
    selected_row <- combined_metrics %>% filter(`Episode Name` == input$episode)
    topic_name <- topic_labels[as.character(selected_row$Dominant_Topic)]
    
    details <- paste0(
      "<div style='background-color:#333333; color:#00FF00; padding:10px; border-radius:10px; 
                     box-shadow: 0 4px 8px rgba(0,0,0,0.5);'>",
      "<h4>Selected Episode Information</h4>",
      "<p><strong>Episode Name:</strong> ", selected_row$`Episode Name`, "</p>",
      "<p><strong>Show Name:</strong> ", selected_row$`Show Name`, "</p>",
      "<p><strong>Topic:</strong> ", topic_name, "</p>",
      "</div>"
    )
    
    HTML(details)
  })
  
  
  
  
  nearest_episodes <- reactive({
    req(input$episode) # Ensure episode is selected
    selected_row <- combined_metrics %>% filter(`Episode Name` == input$episode)
    
    combined_metrics %>%
      filter(`Episode Name` != input$episode) %>%
      mutate(Distance = sqrt((PCA1 - selected_row$PCA1)^2 + (PCA2 - selected_row$PCA2)^2)) %>%
      arrange(Distance) %>%
      head(10)
  })
  
  
  
  output$nearest_pca <- renderPlot({
    selected_row <- combined_metrics %>% filter(`Episode Name` == input$episode)
    nearest_episodes <- combined_metrics %>%
      filter(`Episode Name` != input$episode) %>%
      mutate(Distance = sqrt((PCA1 - selected_row$PCA1)^2 + (PCA2 - selected_row$PCA2)^2)) %>%
      arrange(Distance) %>%
      head(10)
    ggplot() +
      geom_point(data = nearest_episodes, aes(x = PCA1, y = PCA2, color = factor(Dominant_Topic)), size = 4) +
      geom_point(data = selected_row, aes(x = PCA1, y = PCA2), color = "red", size = 5) +
      geom_label(data = nearest_episodes, 
                 aes(x = PCA1, y = PCA2, label = `Episode Name`), 
                 vjust = -1.2, hjust = 0.5, size = 3, color = "white", fill = "black", 
                 fontface = "bold", label.padding = unit(0.15, "lines"), check_overlap = TRUE) +
      
      scale_color_manual(values = RColorBrewer::brewer.pal(12, "Set3"),
                         labels = topic_labels) +
      labs(title = "Nearest Episodes", 
           x = "PCA1", y = "PCA2", color = "Topic") +
      theme_minimal(base_size = 14) + 
      theme(
        panel.background = element_rect(fill = "gray20", color = "gray20"), 
        plot.background = element_rect(fill = "gray20", color = "gray20"),  
        panel.grid.major = element_line(color = "gray30"),               
        panel.grid.minor = element_line(color = "gray20"),               
        axis.text = element_text(color = "white"),                     
        axis.title = element_text(color = "white"),                      
        plot.title = element_text(color = "white", hjust = 0.5),         
        legend.background = element_rect(fill = "gray20", color = NA),    
        legend.text = element_text(color = "white"),                    
        legend.title = element_text(color = "white")                     
      )
  })
  
  output$episode_details <- renderUI({
    if (input$episode == "") return(NULL)
    
    selected_row <- combined_metrics %>% filter(`Episode Name` == input$episode)
    
    nearest_episodes <- combined_metrics %>%
      filter(`Episode Name` != input$episode) %>%
      mutate(Distance = sqrt((PCA1 - selected_row$PCA1)^2 + (PCA2 - selected_row$PCA2)^2)) %>%
      arrange(Distance) %>%
      head(10)
    
    details <- paste0(
      "<h4 style='margin-bottom:10px;'>Top 10 Nearest Episodes</h4>",
      "<ul style='list-style-type: none; padding: 0;'>",
      paste(
        "<li style='margin-bottom: 10px;'>
         <strong>Episode Name:</strong> ", nearest_episodes$`Episode Name`, "<br>
         <strong>Show Name:</strong> ", nearest_episodes$`Show Name`, "<br>
         <strong>Topic:</strong> ", topic_labels[as.character(nearest_episodes$Dominant_Topic)],
        "</li>", collapse = ""),
      "</ul>"
    )
    
    HTML(details) 
  })
  
  
}

shinyApp(ui, server)
