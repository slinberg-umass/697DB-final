#THIS IS A DEMO APP FOR SHOWING SENTIMENTS AND MAP IN TWEETS

library(shiny)
#library(rCharts)
library(lubridate)
library(highcharter)
library(leaflet)
library(quanteda)
library(quanteda.textplots)
library(dplyr)
library(shinyjs)

load("data/located_tweets_senti.rda")
located_tweets_senti$lat <- located_tweets_senti$lat_google
located_tweets_senti$lng <- located_tweets_senti$lng_google

# DRY. Only type sentiment names once. Use them everywhere else.
sentiment_names <- c("anger", "anticipation", "fear", "disgust", "joy", 
                     "sadness", "surprise", "trust", "positive", "negative")

# Dynamically create the sentiment checkboxes with information about the 
# value ranges for each column. This is a lot more cumbersome than it should be.
# Just assigning directly to a list doesn't evaluate the left-hand expression.
sentiment_choices = list()
for (sn in sentiment_names) {
    # create pairs like "anger (0-5)" <- "anger" for each sentiment
    sentiment_choices[[sprintf("%s (%d-%d)", sn,
                       min(located_tweets_senti[[sn]]),
                       max(located_tweets_senti[[sn]])
                       )]] <- sn
}
# get the highest overall sentiment for the max value for the control slider
highest_sentiment <- max(located_tweets_senti[sentiment_names])

# Make user icons for all the tweets at the global scope.
usericon <- makeIcon(
    iconUrl = located_tweets_senti$profile_image_url,
    iconWidth = 15,
    iconHeight = 15
)

# Make a parallel array of empty icons.
# Easiest way to do this is to create an empty column and make another set of
# "icons" "based" on that.

located_tweets_senti$empty_column <- NA
emptyicon <- makeIcon(
    iconUrl = located_tweets_senti$empty_column,
    iconWidth = 15,
    iconHeight = 15
)


# UI
ui <- fluidPage(
    
    # Use the shinyjs library for more advanced UI control
    # See below, and https://stackoverflow.com/a/55161883/13603796
    shinyjs::useShinyjs(),
    
    titlePanel("Who is #vaccinated"),
    
    p(
        class = "text-muted",
        paste(
            "A look at tweets from",
            nrow(located_tweets_senti),
            "geolocated twitter profiles mentioning the #vaccinated tag in June 2021"
        )
    ),
    
    helpText(
        "See",
        a("discussion", href = "#discussion"),
        "at bottom. Source code at ",
        a("github", href = "https://github.com/slinberg-umass/697DB-final", .noWS = c("outside")),
        "."
    ), 
    
    hr(), 

    p(
        "Choose the number of tweets to work with (",
        strong("Caution", .noWS = c("outside")),
        ": higher values slow down processing):",
    ),
    
    sliderInput(
        "tweet_count",
        label = "Number of tweets:",
        min = 100,
        value = 1000,
        max = nrow(located_tweets_senti),
        width = "100%",
        step = 100
    ),
    checkboxInput("disable_twitter_icons", "RECOMMENDED: disable twitter icons over 2,000 results", T, width="100%"),
    

    wellPanel(
        p(strong("Optional:"),
          "select one or more sentiments to filter by", ),
        
        checkboxGroupInput(
            "type",
            # label = (helpText(h5("sentiment type"))),
            label = NULL,
            
            choices = sentiment_choices,
            # selected = "trust",
            inline = T
        ),
        
        # Suppress minor ticks between integers;
        # H/T https://stackoverflow.com/a/44474596/13603796
        tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
        sliderInput(
            "senti_threshold",
            label = "Sentiment threshold:",
            min = 0,
            value = 1,
            max = highest_sentiment,
            width = "30%",
        ),
        helpText(
            p("Higher numbers are stronger; 0 means no sentiment")
        ),
        
    ),
    
    
    hr(), 
    
    fluidRow(
        column(4,
               wellPanel(
                   sliderInput(
                       "slider2",
                       h3("min. retweet count for geo-mapping"),
                       min = 0,
                       max = 100,
                       value = 30
                   ),
                   
                   p(
                       class = "text-muted",
                       paste(
                           "Use the slider to adjust the data displayed based on retweet minimums."
                       )
                   ),
                   
               ),),
        column(8,
               textOutput("mymap_min_count"),
               leafletOutput("mymap"),)
        
    ),
    
    hr(),
    
    fluidRow(column(
        4,
        wellPanel(
            radioButtons(
                "radio",
                label = h3("Show in wordcloud"),
                choices = list(
                    "#hashtags only" = 1,
                    "non-#hashtags only" = 2,
                    "everything" = 3
                ),
                selected = 1
            ),
            
            p(
                class = "text-muted",
                paste(
                    "Specify whether to show hashtags, non-hashtags, or both in the wordcloud"
                )
            ),
            sliderInput(
                "slider1",
                h3("min. retweet count for wordcloud"),
                min = 0,
                max = 100,
                value = 30
            ),
        )
    ),
    column(
        8,
        textOutput("wordcloud_min_count"),
        plotOutput("wordcloud"),
    )),
    
    hr(),
    h2("Disussion", id="discussion"),
    markdown("This section contains working notes and details for the application.
    ### Purpose and scope
    The purpose of this application is to explore a set of Twitter data, based
    on the presence of a hashtag, looking at location data, sentiment, and 
    accompanying words in wordcloud form.
    
    I chose the hashtag \"#vaccinated\", as it is germane at the time of this
    application's creation (June 2021), to look at attitudes around vaccination
    for COVID-19 in the early months of its availability.
    
    Wordclouds may be examined with or without hashtags; while it can be 
    informative to see which other hashtags accompany the main hashtag, it is
    also interesting to look at standard word usage in this context.
    
    ### Data collection and methods
    
    I requested 50,000 tweets through the Twitter API; the API delivered about
    40,000. As the API credentials were shared, I did not push harder to 
    retreive more data and risk triggering limits or shutdowns. I then ran the
    40,000 tweets through Google's API for turning text descriptions of location
    (from each tweet's user's profile), and the 26,000 or so locatable tweets
    formed the dataset for this application. The tweets represent a random 
    sample spread across approximately 10 days, from June 8 to June 18, as
    served by the API.
    
    ### Observations
    
    The general tone of Twitter discussion around the `#vaccinated` hashtag is
    positive. Setting the `positive` threshold to `3` with around 10,000 tweets
    shown creates a dense cluster of tweets in the United States; changing it
    to a `negative` threshold of `3` shows none (that were retweeted the
    default minumum of 30 times). 
    
    To find the most negative single tweet, with a rating of 7, reduce the
    minimum retweet count for the map to 0, check `negative` and set the
    threshold to 7, and drag the tweet count slider all the way to the right
    to show them all. It shows one person in western Canada who is very upset.
    
    By contrast, set the same results to `positive`, and observe the clusters
    in North America (towards the coasts, interestingly), northern Europe and
    India.
    
    ### Critical analysis
    
    Although there are some interesting insights to be gleaned from various
    explorations of the sentiments of the tweets we looked at, it's difficult
    to avoid noticing first that it is very difficult to do accurate sentiment
    detection on tweet-sized texts. The single tweet with an `anger` rating of
    `4`, for instance, reads:
    
    >Good news of the day... Partner had his 2nd AZ jab at weekend and been totally fine (after feeling bloody awful with first one!). Can't wait to get my 2nd one now (also an AZ 35-40ish). #AZ #vaccinated #vaccine #AstraZeneca
    
    Furthermore, the emphasis on retweets can work both for and against the 
    judgment of sentiment. Is a retweet itself an expression of sentiment? Is an 
    original expression of sentiment a stronger indicator than a retweet? The 
    application in its current form does not facilitate the perusal of only
    original tweets, for example, although this would not be difficult to add.
    
    Doing larger-scale analysis of sentiment on Twitter would probably be more
    effective without visual presentation of individual tweets on a map; if the
    scope were expanded to hundreds of thousands or millions of tweets, it's 
    obvious that the application would be overwhelmed. This is an interesting
    way to do some initial exploration, but has obvious limits if applied at 
    scale.
    
    ### Future directions
    There are a couple of technical limitations of this application that I would
    like to address:
    
    1. I would prefer to detect an empty `dfm` when the search terms are too 
    restrictive, and present a cleaner explanation than the red-text error 
    currently shown;
    1. There is an issue with switching between profile-based icons and null 
    icons; the `leaflet` function doesn't switch them, perhaps due to internal 
    caching?
    
             ")
    
)

filter_tweets <- function(df, ..., tweet_count = 100, input_types = c(), threshold = 0) {
    
    set.seed(12345)
    tweets <- slice_sample(df, n = tweet_count)
    
    # for each sentiment name
    for (sn in sentiment_names) {
        # if the corresponding checkbox is checked
        if (sn %in% input_types) {
            # filter tweets by that column name greater than the input threshold
            tweets <- tweets[tweets[[sn]] >= threshold,]
        }
    }

    return(tweets)
}

# SERVER
server <- function(input, output) {
    
    filter_tweets_reactive <- reactive({
        tweets <-
            filter_tweets(
                df = located_tweets_senti,
                tweet_count = input$tweet_count,
                input_types = input$type,
                threshold = input$senti_threshold
            )
    })

    # XXX this works, but the `leaflet` call doesn't seem to use the result.
    usericon_reactive <- reactive({
        # print(paste("Calling reactive usericon, control value is", input$disable_twitter_icons))
        return(ifelse(input$disable_twitter_icons == T, emptyicon, usericon))
    })
    
    # Enable or disable the sentiment threshold slider depending on whether
    # any sentiment filters are checked.
    # https://stackoverflow.com/a/55161883/13603796
    observeEvent(input$type, {
        if (length(input$type) > 0) {
            shinyjs::enable("senti_threshold")
        } else{
            shinyjs::disable("senti_threshold")
        }
    },
    # ignoreNull = F needed to fire event when last checkbox is unchecked
    ignoreNULL = F)
    
    output$wordcloud <- renderPlot({

        tweets <- filter_tweets_reactive()
        
        dfm <-
            dfm(
                tweets[tweets$retweet_count >= input$slider1,]$text,
                remove = c(
                    stopwords("english"),
                    remove_numbers = TRUE,
                    remove_symbols = TRUE,
                    remove_punct = TRUE
                )
            )
        if (input$radio == 1) {
            dfm <- dfm_select(dfm, pattern = ("#*"))
        } else if (input$radio == 2) {
            dfm <- dfm_select(dfm,
                              pattern = ("#*"),
                              selection = "remove")
        }
        set.seed(12345)
        textplot_wordcloud(
            dfm,
            min_size = 1.5,
            min_count = 10,
            max_words = 100,
            color = rev(RColorBrewer::brewer.pal(10, "RdBu"))
        )
    })
    
    output$wordcloud_min_count <- renderText({
        paste0(
            "Wordcloud for ",
            input$tweet_count,
            " #vaccinated tweets with at least ",
            input$slider1,
            " retweet",
            ifelse(input$slider1 == 1, "", "s"),
            switch(input$radio,
                   "1" = ", hashtags only",
                   "2" = ", non-hashtags only")
        )
    })

    output$mymap <- renderLeaflet({
        
        tweets <- filter_tweets_reactive()
        
        print("Calling renderleaflet")
        leaflet(data = tweets[tweets$retweet_count >= input$slider2,]) %>%
            addTiles() %>%
            setView(lng = -98.35,
                    lat = 39.50,
                    zoom = 2) %>%
            addMarkers(
                lng = ~ lng,
                lat = ~ lat,
                popup = ~ as.character(text),
                # XXX this isn't working
                icon = usericon_reactive()
            ) %>%
            addProviderTiles("Stamen.TonerLite") %>%  #more layers:http://leaflet-extras.github.io/leaflet-providers/preview/
            addCircleMarkers(stroke = FALSE, fillOpacity = 0.5)
    })
    
    output$mymap_min_count <- renderText({
        paste0(
            "Geolocaton map for ",
            input$tweet_count,
            " #vaccinated tweets with at least ",
            input$slider2,
            " retweet",
            ifelse(input$slider2 == 1, "", "s")
        )
    })
    
}

# Run the application
shinyApp(ui = ui, server = server)
