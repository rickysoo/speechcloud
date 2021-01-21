library(shiny)
library(shinythemes)
library(wordcloud2)
library(colourpicker)
library(tm)
library(stringr)
# library(webshot)
# library(htmlwidgets)
# library(magick)
library(dplyr)
library(ggplot2)
library(syuzhet)

# shapes <- sort(c('circle', 'cardioid', 'diamond', 'triangle-forward', 'triangle', 'pentagon', 'star'))

ui <- fluidPage(
    theme = shinytheme('united'),
    titlePanel('Speech Cloud'),
    p('Make a word cloud and analyze your speech.'),
    # hr(),
    
    sidebarLayout(
        sidebarPanel(
            uiOutput('speeches'),

            textAreaInput('own', 'Paste your speech here', rows = 7),
            
            sliderInput('num', 'Use number of words', min = 10, max = 300, value = 100),
            
            colourInput('col', 'Choose background color', value = '#EEEEEE'),
            
            # selectInput(
            #     'shape',
            #     label = 'Choose shape',
            #     choices = setNames(shapes, str_to_title(shapes)),
            #     selected = 'circle'
            # ),
            
            hr(),
            a('Made by Ricky Soo | Feedback welcomed', href = 'https://github.com/rickysoo', target = '_blank')
        ),
        
        mainPanel(
            tabsetPanel(
                tabPanel(
                    'Word Cloud',
                    
                    br(),
                    p('The word cloud shows the most spoken words in the speech. The bigger a word is, the more frequently it is used.'),
                    wordcloud2Output('cloud', width = '100%'),
                    
                    tags$head(
                        # tags$style(HTML('#regenerate, #download {background-color:#004165}'))
                        tags$style(HTML('#regenerate {background-color:#004165}'))
                    ),
                    
                    br(),
                    fluidRow(
                        column(12, div(actionButton('regenerate', 'Get Another One'), style = "text-align: center"))
                        # column(6, div(downloadButton('download', 'Download'), style = "text-align: left")))
                    )
                ),
                tabPanel(
                    'Word List',
                    
                    br(),
                    p('Here are the top 10 words used in the speech. Do you see any pattern?'),
                    plotOutput('words')                    
                ),
                tabPanel(
                    'Sentiments',
                    
                    br(),
                    p('Sentiment analysis shows the positivity and negativity in the speech. A value above 0 is positive. A value below 0 is negative.'),
                    br(),
                    
                    plotOutput('sentiments_plot'),
                    hr(),
                    
                    h3('Break It Down to Sentences'),
                    tableOutput('sentiments_table')
                ),
                tabPanel(
                    'Watch Video',
                    
                    h3(textOutput('video_title')),
                    tags$style(HTML('.video-container {position: relative; width: 100%; padding-bottom: 56.25%;}')),
                    tags$style(HTML('.video {position: absolute; top: 0; left: 0; width: 100%; height: 100%; border: 0;}')),
                    htmlOutput('video'),
                    a('Video source: Toastmasters World Champions', href = 'https://www.youtube.com/watch?v=7Tev43VNRIc&list=PLZfLuUohfwTYDFDiFFFIl47hyPYe7Z6Xi', target = '_blank')
                )
            )
        )
    )
)

server <- function(input, output, session) {
    values <- reactiveValues(
        source = 'speech',
        # corpus = NULL,
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
    
    load_df <- reactive({
        if (values$source == 'speech') {
            data <- readLines(input$speech)
        }
        else if (values$source == 'own') {
            data <- input$own
        }
        else {
            return(NULL)
        }
        
        if(is.character(data)) {
            corpus <- Corpus(VectorSource(data))
            corpus <- tm_map(corpus, tolower)
            corpus <- tm_map(corpus, removePunctuation)
            corpus <- tm_map(corpus, removeNumbers)
            corpus <- tm_map(corpus, removeWords, stopwords('english'))
            corpus <- tm_map(corpus, stripWhitespace)
            
            # corpus_frame <- data.frame(text=unlist(sapply(corpus, `[`, "content")), stringsAsFactors=F)
            # values$corpus <- corpus
            # print(corpus)
            # View(corpus_frame)
            
            tdm <- as.matrix(TermDocumentMatrix(corpus))
            data <- sort(rowSums(tdm), decreasing = TRUE)
            df <- data.frame(word = names(data), freq = as.numeric(data))
        }
        
        values$df <- head(df, n = input$num)
        values$df
    })  
    
    generate_wordcloud <- reactive({
        df <- load_df()
        
        if (nrow(df) == 0) {
            return(NULL)
        }
        
        wordcloud2(data = df, backgroundColor = input$col, shape = 'circle', size = 0.6, shuffle = TRUE)
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
    
    output$words <- renderPlot({
        df <- values$df %>%
            head(10)
        
        if (nrow(df) == 0) {
            return(NULL)
        }
        
        ggplot(data = df, aes(x = reorder(word, freq), y = freq, fill = word)) +
            geom_bar(stat = 'identity') +
            scale_y_continuous(breaks = 0:max(df$freq)) +
            coord_flip() +
            labs(
                title = 'Top 10 Words in Speech',
                x = 'Words',
                y = 'Count'
            ) +
            theme_minimal() +
            theme(
                plot.title = element_text(face = 'bold', size = 14, hjust = 0.5),
                axis.title = element_text(face = 'bold'),
                plot.background = element_blank(),
                panel.background = element_blank(),
                panel.grid.major.y = element_blank(),
                legend.position = 'None'
            )
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
    
    output$sentiments_plot <- renderPlot({
        df <- extract_sentiments()
        speech_sentiment <- round(mean(df$Sentiment), 2)
        print(speech_sentiment)
        
        ggplot(data = df, aes(x = ID, y = Sentiment)) +
            # geom_line(aes(color = '#CD202C')) +
            # geom_line(aes(color = '#F2DF74')) +
            geom_line(aes(color = 'blue')) +
            geom_hline(yintercept = 0, colour = 'gray', size = 1) +
            labs(
                title = 'Change of Sentiments in Speech',
                x = 'Sentences',
                y = 'Sentiment'
            ) +
            annotate(
                'text', 
                label = paste0('Overall Sentiment = ', speech_sentiment), 
                x = 1,
                y = max(df$Sentiment),
                color = '#004165',
                size = 5,
                hjust = 0
            ) +
            theme_minimal() +
            theme(
                plot.title = element_text(face = 'bold', size = 14, hjust = 0.5),
                axis.title = element_text(face = 'bold'),
                plot.background = element_blank(),
                panel.background = element_blank(),
                panel.grid.major.y = element_blank(),
                legend.position = 'None'
            )
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
        # HTML(paste0('<iframe width="560" height="315" src="https://www.youtube.com/embed/', video, '" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>'))
    })
    
    observeEvent(input$own, {
        values$source <- 'own'
        values$df <- load_df()
    })
    
    observeEvent(input$speech, {
        values$source <- 'speech'
        values$df <- load_df()
    })
    
    observeEvent(input$regenerate, {
        temp <- values$source
        values$source <- ''
        values$source <- temp
        
        generate_wordcloud()
    })
    
    # output$download <- downloadHandler(
    #     filename = 'wordcloud.png',
    #     
    #     content = function(file) {
    #         saveWidget(w, 'tmp.html', selfcontained = FALSE)
    #         webshot('tmp.html', 'wordcloud.png', delay = 2, vwidth = 480, vheight = 480)
    #     },
    #     
    #     contentType = 'image/png'
    # )    
}

shinyApp(ui, server)