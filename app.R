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

# shapes <- sort(c('circle', 'cardioid', 'diamond', 'triangle-forward', 'triangle', 'pentagon', 'star'))

ui <- fluidPage(
    theme = shinytheme('united'),
    titlePanel('Speech Cloud'),
    p('Make a word cloud for your speech.'),
    hr(),
    
    sidebarLayout(
        sidebarPanel(
            selectInput(
                'speech',
                label = 'See an example',
                choices = c(
                        'An Unbelievable Story - Aaron Beverly' = 'aaron_beverly.txt',
                        'Changed by a Tyre - Pres Vasilev' = 'pres_vasilev.txt',
                        'I See Something - Danajaya Hettiarachchi' = 'dananjaya_hettiarachchi.txt',
                        'Outsmart, Outlast - Darren Tay' = 'darren_tay.txt',
                        'Pull Less, Bend More - Manoj Vasudevan' = 'manoj_vasudevan.txt',
                        'Still Standing - Ramona Smith' = 'ramona_smith.txt',
                        'The Power of Words - Mohammed Qahtani' = 'mohammed_qahtani.txt'
                    ),
                selected = 'dananjaya_hettiarachchi.txt'
            ),
            
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
                    'Cloud',
                    wordcloud2Output('cloud', width = '100%'),
                    
                    tags$head(
                        # tags$style(HTML('#regenerate, #download {background-color:#004165}'))
                        tags$style(HTML('#regenerate {background-color:#004165}'))
                    ),
                    
                    hr(),
                    fluidRow(
                        column(12, div(actionButton('regenerate', 'Try Another One'), style = "text-align: center"))
                        # column(6, div(downloadButton('download', 'Download'), style = "text-align: left")))
                    )
                ),
                tabPanel(
                    'Words',
                    plotOutput('words')                    
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

    # create_wordcloud <- function(data, num_words = 100, background = 'white', shape = 'circle') {
    #     if(is.character(data)) {
    #         corpus <- Corpus(VectorSource(data))
    #         corpus <- tm_map(corpus, tolower)
    #         corpus <- tm_map(corpus, removePunctuation)
    #         corpus <- tm_map(corpus, removeNumbers)
    #         corpus <- tm_map(corpus, removeWords, stopwords('english'))
    #         
    #         tdm <- as.matrix(TermDocumentMatrix(corpus))
    #         data <- sort(rowSums(tdm), decreasing = TRUE)
    #         df <- data.frame(word = names(data), freq = as.numeric(data))
    #     }
    #     
    #     if (!is.numeric(num_words) || num_words < 3) {
    #         num_words <- 3
    #     }
    #     
    #     values$df <- head(df, n = num_words)
    # 
    #     if (nrow(values$df) == 0) {
    #         return(NULL)
    #     }
    #     
    #     wordcloud2(data = values$df, backgroundColor = background, shape = shape, size = 0.6, shuffle = TRUE)
    # }

    load_df <- reactive({
        if (values$source == 'speech') {
            data <- readLines(input$speech)
        }
        else if (values$source == 'own') {
            data <- input$own
        }
        else {
            return()
        }
        
        if(is.character(data)) {
            corpus <- Corpus(VectorSource(data))
            corpus <- tm_map(corpus, tolower)
            corpus <- tm_map(corpus, removePunctuation)
            corpus <- tm_map(corpus, removeNumbers)
            corpus <- tm_map(corpus, removeWords, stopwords('english'))
            
            tdm <- as.matrix(TermDocumentMatrix(corpus))
            data <- sort(rowSums(tdm), decreasing = TRUE)
            df <- data.frame(word = names(data), freq = as.numeric(data))
        }
        
        if (!is.numeric(input$num) || input$num < 3) {
            num_words <- 3
        }
        
        values$df <- head(df, n = input$num)
        
        if (nrow(values$df) == 0) {
            return(NULL)
        }
        
        values$df
    })  
    
    generate_wordcloud <- reactive({
        # if (values$source == 'speech') {
        #     data <- readLines(input$speech)
        # }
        # else if (values$source == 'own') {
        #     data <- input$own
        # }
        # else {
        #     return()
        # }
        # 
        # if(is.character(data)) {
        #     corpus <- Corpus(VectorSource(data))
        #     corpus <- tm_map(corpus, tolower)
        #     corpus <- tm_map(corpus, removePunctuation)
        #     corpus <- tm_map(corpus, removeNumbers)
        #     corpus <- tm_map(corpus, removeWords, stopwords('english'))
        #     
        #     tdm <- as.matrix(TermDocumentMatrix(corpus))
        #     data <- sort(rowSums(tdm), decreasing = TRUE)
        #     df <- data.frame(word = names(data), freq = as.numeric(data))
        # }
        # 
        # if (!is.numeric(input$num) || input$num < 3) {
        #     num_words <- 3
        # }
        # 
        # values$df <- head(df, n = input$num)
        # 
        # if (nrow(values$df) == 0) {
        #     return(NULL)
        # }
        
        wordcloud2(data = load_df(), backgroundColor = input$col, shape = 'circle', size = 0.6, shuffle = TRUE)
        
        # create_wordcloud(
        #     data,
        #     num_words = input$num,
        #     background = input$col
        # )
    })
    
    output$cloud <- renderWordcloud2({
        if (input$speech == '') {
            return()
        }
        print('rendering...')
        generate_wordcloud()
    })
    
    output$words <- renderPlot({
        df <- values$df %>%
            head(10)

        ggplot(data = df, aes(x = reorder(word, freq), y = freq, fill = word)) +
            geom_bar(stat = 'identity') +
            scale_y_continuous(breaks = 0:max(df$freq)) +
            coord_flip() +
            labs(
                title = 'Top 10 Words in Speech',
                x = 'Words',
                y = 'Count'
            ) +
            theme(
                plot.title = element_text(face = 'bold', size = 14, hjust = 0.5),
                axis.title = element_text(face = 'bold'),
                plot.background = element_blank(),
                panel.background = element_blank(),
                # panel.grid.major.y = element_line(color = 'grey'),
                panel.grid.major.y = element_blank(),
                legend.position = 'None'
            )
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