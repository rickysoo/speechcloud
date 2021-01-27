library(shiny)
library(shinythemes)
library(wordcloud2)
library(colourpicker)
library(tm)
library(stringr)
library(dplyr)
library(ggplot2)
library(highcharter)
library(syuzhet)
library(visNetwork)
library(RWeka)

shapes <- sort(c('circle', 'diamond', 'triangle-forward', 'triangle', 'pentagon', 'star'))

ui <- fluidPage(
  tags$head(includeHTML('google-analytics.html')),
  
  theme = shinytheme('united'),
  titlePanel('Speech Cloud'),
  p('Make a word cloud and analyze your speech.'),
  hr(),
  
  sidebarLayout(
    sidebarPanel(
      uiOutput('speeches'),
      
      textAreaInput('own', 'Paste your speech here', rows = 5),
      
      sliderInput('num', 'Use number of words', min = 10, max = 300, value = 100),
      
      colourInput('col', 'Choose background color', value = '#EEEEEE'),
      
      selectInput(
        'shape',
        label = 'Choose shape',
        choices = setNames(shapes, str_to_title(shapes)),
        selected = 'circle'
      ),
      
      sliderInput('size', 'Font size', min = 0.5, max = 1.5, value = 0.8, step = 0.1),
      
      hr(),
      a('Made by Ricky Soo | Feedback welcomed', href = 'https://github.com/rickysoo', target = '_blank')
    ),
    
    mainPanel(
      tabsetPanel(
        id = 'tabs',
        
        tabPanel(
          title = 'Word Cloud',
          
          br(),
          p('The word cloud shows the most spoken words in the speech. The bigger a word is, the more it is used.'),
          wordcloud2Output('cloud', width = '100%'),
          
          tags$head(
            tags$style(HTML('#regenerate {background-color:#004165}'))
          ),
          
          br(),
          fluidRow(
            column(12, div(actionButton('regenerate', 'Get Another One'), style = "text-align: center"))
          )
        ),
        tabPanel(
          title = 'Words',
          
          br(),
          highchartOutput('words_plot'),
          
          br(),
          highchartOutput('words_combination_plot')
        ),
        tabPanel(
          title = 'Connections',
          
          br(),
          p('Words are connected when they are often spoken near to each other.'),
          
          br(),
          fluidRow(
            column(6, sliderInput('topwords', 'Show number of most spoken words', min = 1, max = 10, value = 5)),
            column(6, sliderInput('connection', 'Minimum connection required', min = 0.2, max = 1, value = 0.5, step = 0.1))
          ),
          
          br(),
          visNetworkOutput('words_connections')
        ),
        tabPanel(
          title = 'Sentiments',
          
          br(),
          p('Sentiment analysis shows the positivity and negativity in the speech. A value above 0 is positive. A value below 0 is negative.'),
          
          br(),
          highchartOutput('sentiments_plot'),
          hr(),
          
          h3('Analysis by Sentences'),
          tableOutput('sentiments_table'),
          
          p(a('Sentiment analysis is based on the Syuzhet R package by Matthew L. Jockers', href = 'https://github.com/mjockers/syuzhet', target = '_blank'))
        ),
        tabPanel(
          title = 'Emotions',
          
          br(),
          p('Emotion analysis shows the emotions based on words used in the sentences of the speech.'),
          
          br(),
          highchartOutput('emotions_plot'),
          hr(),
          
          h3('Analysis by Sentences'),
          tableOutput('emotions_table'),
          
          p(a('Emotion analysis is based on the NRC Emotion Lexicon by Saif Mohammad', href = 'https://saifmohammad.com/WebPages/NRC-Emotion-Lexicon.htm', target = '_blank'))
        ),
        tabPanel(
          title = 'Watch Video',
          
          h3(textOutput('video_title')),
          tags$style(HTML('.video-container {position: relative; width: 100%; padding-bottom: 56.25%;}')),
          tags$style(HTML('.video {position: absolute; top: 0; left: 0; width: 100%; height: 100%; border: 0;}')),
          htmlOutput('video'),
          
          br(),
          p(a('Video source: World Championship of Public Speaking', href = 'https://www.youtube.com/watch?v=7Tev43VNRIc&list=PLZfLuUohfwTYDFDiFFFIl47hyPYe7Z6Xi', target = '_blank'))
        )
      )
    )
  )
)

server <- function(input, output, session) {
  values <- reactiveValues(
    source = 'speech',
    df = data.frame()
  )   
  
  load_speeches <- reactive({
    read.csv('Speeches.csv') %>%
      arrange(Title)
  })
  
  get_video_info <- function(file, col) {
    speeches <- load_speeches() %>%
      filter(File == file)
    
    speeches[[col]]
  }
  
  load_corpus <- function(stopword = TRUE) {
    if (values$source == 'speech') {
      sentences <- extract_sentences()
    }
    else if (values$source == 'own') {
      sentences <- get_sentences(input$own)
    }
    else {
      return(NULL)
    }
    
    corpus <- VCorpus(VectorSource(sentences))
    corpus <- tm_map(corpus, content_transformer(tolower))
    corpus <- tm_map(corpus, removePunctuation)
    corpus <- tm_map(corpus, removeNumbers)
    corpus <- tm_map(corpus, stripWhitespace)
    
    if (stopword) {
      corpus <- tm_map(corpus, removeWords, stopwords('english'))
    }
    # corpus <- tm_map(corpus, stemDocument)
    # corpus <- tm_map(corpus, lemmatize_strings)
    corpus <- tm_map(corpus, PlainTextDocument)
    corpus
  }
  
  load_dtm <- function(tokenize = FALSE, stopword = TRUE) {
    corpus <- load_corpus(stopword = stopword)
    
    if (tokenize) {
      DocumentTermMatrix(
        corpus,
        control = list(tokenize = tokenizer)
      )
    }
    else {
      DocumentTermMatrix(corpus)  
    }
  }
  
  tokenizer <- function(x) {
    NGramTokenizer(x, Weka_control(min = 2, max = 5))
  }
  
  load_df <- reactive({
    dtm <- load_dtm()
    corpus_matrix <- as.matrix(dtm)
    
    words <- sort(colSums(corpus_matrix), decreasing = TRUE)
    df <- data.frame(word = names(words), freq = as.numeric(words))
    
    values$df <- head(df, n = input$num)
    values$df
  })  
  
  generate_wordcloud <- reactive({
    df <- load_df()
    
    if (nrow(df) == 0) {
      return(NULL)
    }
    
    wordcloud2(data = df, backgroundColor = input$col, shape = input$shape, size = input$size, shuffle = TRUE)
  })
  
  output$speeches <- renderUI({
    speeches <- load_speeches()
    
    selectInput(
      inputId = 'speech',
      label = 'See an example',
      choices = setNames(speeches$File, paste0(speeches$Title, ' - ', speeches$Speaker)),
      selected = 'dananjaya_hettiarachchi.txt'
    )
  })
  
  output$cloud <- renderWordcloud2({
    if (is.null(input$speech) | is.null(values$df)) {
      return(NULL)
    }
    
    print('Rendering word cloud...')
    generate_wordcloud()
  })
  
  output$words_plot <- renderHighchart({
    df <- values$df %>%
      head(10)
    
    if (nrow(df) == 0) {
      return(NULL)
    }
    
    df %>%
      hchart(type = 'bar', hcaes(x = reorder(word, freq), y = freq, color = word)) %>%
      hc_title(text = '10 Most Spoken Words', align = 'center', style = list(fontWeight = 'bold')) %>%
      hc_xAxis(title = list(text = 'Words')) %>%
      hc_yAxis(title = list(text = 'Count')) %>%
      hc_add_theme(hc_theme_ffx()) %>%
      hc_tooltip(shared = TRUE, pointFormat = '{point.y}')
    
    # ggplot(data = df, aes(x = reorder(word, freq), y = freq, fill = word)) +
    #   geom_bar(stat = 'identity') +
    #   scale_y_continuous(breaks = 0:max(df$freq)) +
    #   coord_flip() +
    #   labs(
    #     title = '10 Most Spoken Words',
    #     x = 'Words',
    #     y = 'Count'
    #   ) +
    #   theme_minimal() +
    #   theme(
    #     plot.title = element_text(face = 'bold', size = 14, hjust = 0.5),
    #     axis.title = element_text(face = 'bold'),
    #     plot.background = element_blank(),
    #     panel.background = element_blank(),
    #     panel.grid.major.x = element_blank(),
    #     panel.grid.minor.x = element_blank(),
    #     panel.grid.major.y = element_blank(),
    #     panel.grid.minor.y = element_blank(),
    #     legend.position = 'None'
    #   )
  })
  
  output$words_combination_plot <- renderHighchart({
    dtm <- load_dtm(tokenize = TRUE, stopword = FALSE)
    ngram_freq <- sort(colSums(as.matrix(dtm)), decreasing = TRUE)
    
    df <- data.frame(
      word = names(ngram_freq),
      freq = ngram_freq
    ) %>%
      head(10)
    
    if (nrow(df) == 0) {
      return(NULL)
    }
    
    df %>%
      hchart(type = 'bar', hcaes(x = reorder(word, freq), y = freq, color = word)) %>%
      hc_title(text = '10 Most Spoken Word Combinations', align = 'center', style = list(fontWeight = 'bold')) %>%
      hc_xAxis(title = list(text = 'Words')) %>%
      hc_yAxis(title = list(text = 'Count')) %>%
      hc_add_theme(hc_theme_ffx()) %>%
      hc_tooltip(shared = TRUE, pointFormat = '{point.y}')
    
    # ggplot(data = df, aes(x = reorder(word, freq), y = freq, fill = word)) +
    #   geom_bar(stat = 'identity') +
    #   scale_y_continuous(breaks = 0:max(df$freq)) +
    #   coord_flip() +
    #   labs(
    #     title = '10 Most Spoken Word Combinations',
    #     x = 'Words',
    #     y = 'Count'
    #   ) +
    #   theme_minimal() +
    #   theme(
    #     plot.title = element_text(face = 'bold', size = 14, hjust = 0.5),
    #     axis.title = element_text(face = 'bold'),
    #     plot.background = element_blank(),
    #     panel.background = element_blank(),
    #     panel.grid.major.x = element_blank(),
    #     panel.grid.minor.x = element_blank(),
    #     panel.grid.major.y = element_blank(),
    #     panel.grid.minor.y = element_blank(),
    #     legend.position = 'None'
    #   )
  })
  
  output$words_connections <- renderVisNetwork({
    df <- values$df %>%
      head(input$topwords)
    
    if (nrow(df) == 0) {
      return(NULL)
    }
    
    nodes <- data.frame(
      id = df$word,
      label = df$word,
      value = df$freq,
      title = paste('Word =', df$word, '<br>Occurrences =', df$freq),
      group = 'Top'
    )
    
    edges <- data.frame()
    
    dtm <- load_dtm()
    dtm_matrix <- as.matrix(dtm)
    
    for (word in df$word) {
      associates <- findAssocs(dtm, word, corlimit = input$connection)[[1]]
      associate_words <- names(associates)
      
      if (length(associate_words) == 0) {
        next
      }
      
      for (associate_word in associate_words) {
        `%notin%` <- Negate(`%in%`)
        
        associate_freq <- sum(dtm_matrix[, associate_word])
        associate_corr <- associates[associate_word]
        
        if (associate_freq <= 2) {
          next
        }
        
        if (associate_word %notin% nodes$id) {
          node <- data.frame(
            id = associate_word,
            label = associate_word,
            value = associate_freq,
            title = paste('Word =', associate_word, '<br>Occurrences =', associate_freq),
            group = 'Connected'
          )
          
          nodes <- rbind(nodes, node)
        }
        
        edge <- data.frame(
          from = word,
          to = associate_word,
          value = associate_corr,
          title = paste('Connection =', associate_corr),
          color = 'lightgreen'
        )
        
        edges <- rbind(edges, edge)
      }
    }
    
    visNetwork(nodes, edges, width = '100%', height = 500, main = 'Most Spoken Words and The Connected Words') %>%
      visGroups(groupname = 'Top', color = 'red') %>%
      visGroups(groupname = 'Connected', color = 'blue') %>%
      visEdges('to')
  })
  
  extract_sentences <- reactive({
    if (values$source == 'speech') {
      text <- readChar(input$speech, file.info(input$speech)$size)
    }
    else if (values$source == 'own') {
      text <- input$own
    }
    
    get_sentences(text)
  })
  
  extract_sentiments <- reactive({
    sentences <- extract_sentences()
    sentiments <- get_sentiment(sentences, method = 'syuzhet')
    
    data.frame(
      'ID' = 1:length(sentences),
      'Sentence' = sentences,
      'Sentiment' = sentiments
    )
  })    
  
  get_speech_sentiment <- reactive({
    sentences <- extract_sentences()
    speech <- paste(sentences, collapse = ' ')
    get_sentiment(speech, method = 'syuzhet')
  })    
  
  output$sentiments_plot <- renderHighchart({
    df <- extract_sentiments()
    speech_sentiment <- get_speech_sentiment()
    
    df %>%
      hchart(type = 'line', hcaes(x = ID, y = Sentiment)) %>%
      hc_title(text = 'Change of Sentiments in Speech', align = 'center', style = list(fontWeight = 'bold')) %>%
      hc_xAxis(
        title = list(text = 'Sentences')
      ) %>%
      hc_yAxis(
        plotLines = list(
          list(
            color = 'gray',
            width = 2,
            value = 0
          )
        ),
        title = list(text = 'Sentiment')
      ) %>%
      hc_annotations(
        list(
          labels = list(
            list(point = list(x = 1, y = max(df$Sentiment), xAxis = 0, yAxis = 0), text = paste('Speech Sentiment =', speech_sentiment))
          )
        )
      ) %>%
      hc_add_theme(hc_theme_ffx()) %>%
      hc_tooltip(shared = TRUE, headerFormat = 'Sentence {point.x} <br>Sentiment = ', pointFormat = '{point.y}')
    
    # ggplot(data = df, aes(x = ID, y = Sentiment)) +
    #   geom_line(color = '#004165', size = 1) +
    #   geom_hline(yintercept = 0, colour = 'gray', size = 1) +
    #   labs(
    #     title = 'Change of Sentiments in Speech',
    #     x = 'Sentences',
    #     y = 'Sentiment'
    #   ) +
    #   annotate(
    #     'text', 
    #     label = paste0('Speech Sentiment = ', speech_sentiment),
    #     x = 1,
    #     y = max(df$Sentiment),
    #     color = '#004165',
    #     size = 5,
    #     hjust = 0
    #   ) +
    #   theme_minimal() +
    #   theme(
    #     plot.title = element_text(face = 'bold', size = 14, hjust = 0.5),
    #     axis.title = element_text(face = 'bold'),
    #     plot.background = element_blank(),
    #     panel.background = element_blank(),
    #     panel.grid.major.x = element_blank(),
    #     panel.grid.minor.x = element_blank(),
    #     panel.grid.major.y = element_blank(),
    #     panel.grid.minor.y = element_blank(),
    #     legend.position = 'None'
    #   )
  })
  
  output$sentiments_table <- renderTable(
    {
      extract_sentiments()
    },
    
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    colnames = TRUE,
    rownames = FALSE
  )
  
  extract_emotions <- reactive({
    sentences <- extract_sentences()
    
    emotions <- get_nrc_sentiment(sentences)
    emotions <- emotions[1:8]
    colnames(emotions) <- str_to_title(colnames(emotions))
    
    emotions <- emotions %>%
      mutate_if(is.numeric, as.integer)
    
    emotions
  })    
  
  output$emotions_plot <- renderHighchart({
    df <- extract_emotions()        
    
    df <- data.frame(t(df))
    df <- data.frame(rowSums(df))
    names(df)[1] = 'Count'
    df <- cbind('Sentiment' = rownames(df), df)
    rownames(df) <- NULL
    
    df %>%
      hchart(type = 'bar', hcaes(x = Sentiment, y = Count, color = Sentiment)) %>%
      hc_title(text = 'Emotions Used in Speech', align = 'center', style = list(fontWeight = 'bold')) %>%
      hc_xAxis(title = list(text = 'Emotions')) %>%
      hc_yAxis(title = list(text = 'Count')) %>%
      hc_add_theme(hc_theme_ffx()) %>%
      hc_tooltip(shared = TRUE, pointFormat = '{point.y}')
    
    # ggplot(data = df, aes(x = reorder(Sentiment, desc(Sentiment)), y = Count, fill = Sentiment)) +
    #   geom_bar(stat = 'identity') +
    #   scale_y_continuous(breaks = 0:max(df$Count)) +
    #   coord_flip() +
    #   labs(
    #     title = 'Emotions Used in Sentences in Speech',
    #     x = 'Emotions',
    #     y = 'Count'
    #   ) +
    #   theme_minimal() +
    #   theme(
    #     plot.title = element_text(face = 'bold', size = 14, hjust = 0.5),
    #     axis.title = element_text(face = 'bold'),
    #     plot.background = element_blank(),
    #     panel.background = element_blank(),
    #     panel.grid.major.x = element_blank(),
    #     panel.grid.minor.x = element_blank(),
    #     panel.grid.major.y = element_blank(),
    #     panel.grid.minor.y = element_blank(),
    #     legend.position = 'None'
    #   )
  })
  
  get_emotion_words <- function(emotions) {
    emotion_words <- vector()
    
    for (i in 1:length(emotions)) {
      if (emotions[i] > 0) {
        emotion_words <- c(emotion_words, names(emotions)[i])
      }
    }
    
    paste(emotion_words, collapse = ', ')
  }
  
  output$emotions_table <- renderTable(
    {
      sentences <- extract_sentences()
      emotions <- extract_emotions()
      emotion_words <- apply(emotions, 1, get_emotion_words)
      
      data.frame(
        'ID' = 1:length(sentences),
        'Sentence' = sentences,
        'Emotions' = emotion_words
      )            
    },
    
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    colnames = TRUE,
    rownames = FALSE
  )
  
  output$video_title <- renderText({
    if (is.null(input$speech) | values$source != 'speech') {
      return('No video for your speech')
    }
    
    title <- get_video_info(input$speech, 'Title')
    speaker <- get_video_info(input$speech, 'Speaker')
    
    paste0(title, ' - ', speaker)
  })
  
  output$video <- renderText({
    if (is.null(input$speech) | values$source != 'speech') {
      return(NULL)
    }
    
    video <- get_video_info(input$speech, 'Video')
    
    HTML(paste0('<div class="video-container"><iframe class="video" src="https://www.youtube.com/embed/', video, '" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe></div>'))
  })
  
  observeEvent(input$own, {
    values$source <- 'own'
    values$df <- load_df()
    hideTab(inputId = 'tabs', target = 'Watch Video')
  })
  
  observeEvent(input$speech, {
    values$source <- 'speech'
    values$df <- load_df()
    showTab(inputId = 'tabs', target = 'Watch Video')
  })
  
  observeEvent(input$regenerate, {
    temp <- values$source
    values$source <- ''
    values$source <- temp
    
    generate_wordcloud()
  })
}

shinyApp(ui, server)