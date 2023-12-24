library(rvest)
library(httr)
library(stringr)
library(dplyr)

# Function to get word frequency from Google Ngrams
get_ngram_frequency <- function(word) {
    Sys.sleep(runif(1, min = 1, max = 5)) # Rate limiting: random delay between 1 and 5 seconds
    
    base_url <- "https://books.google.com/ngrams/graph"
    user_agent <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3"
    
    response <- GET(url = base_url, query = list(content = word, year_start = 1800, year_end = 2019, corpus = 15, smoothing = 3),
                    user_agent(user_agent))
    
    webpage <- read_html(response)
    script_text <- html_text(html_nodes(webpage, 'script'))
    frequency_data <- str_match(script_text, paste0(word, "\", \\[\\[\\d+, (\\[.*\\])\\]\\]\\];"))
    
    if (is.na(frequency_data[1, 2])) {
        return(NA)
    } else {
        # Extracting the last year's data
        yearly_data <- str_extract_all(frequency_data[1, 2], "\\[(\\d+), (0\\.\\d+)\\]")[[1]]
        last_year_data <- yearly_data[length(yearly_data)]
        freq <- as.numeric(str_match(last_year_data, "\\[(\\d+), (0\\.\\d+)\\]")[,3])
        
        return(freq)
    }
}

# Example usage
word <- "example" # Replace with your word
frequency <- get_ngram_frequency(word)
print(frequency)

