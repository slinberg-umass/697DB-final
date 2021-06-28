# 697DB-final

There are two main code files in this repository:

## get_and_prepare_tweets.Rmd

This is the code that uses the Twitter and Google Geocoding APIs to fetch and
prepare data for the Shiny app. It produces a few stages of intermediary files
along the way to the final data file called `located_tweets_senti.rda`, which
contains the variable `located_tweets_senti`, a data frame with all of the
Twitter columns, plus sentiment analysis columns and latitude and longitude
returned by Google for textual location data scraped from the Twitter users'
bios.

It requires a `config.yml` file with your API credentials, in the following
format:

```yml
default:
  # Used to create the twitter token
  twitter_app : "yourvalue"
  twitter_consumer_key : "yourvalue"
  twitter_consumer_secret : "yourvalue"
  twitter_access_token : "yourvalue"
  twitter_access_secret : "yourvalue"
  
  # google API
  google_geocoding_apikey : "yourvalue"
```

The directory "data" will be created if it doesn't exist, and files are written
to it. `data/located_tweets_senti.rda` is loaded by app.R.

## app.R

This is the Shiny application. It loads the `data/located_tweets_senti.rda` file
created by `get_and_prepare_tweets.Rmd` above.

You can see it running at https://stevelinberg.shinyapps.io/697DB_final_project/.

Steve Linberg  
https://slinberg.net  
steve@slinberg.net  

