Notes from programmable search engine May 1 2025

Validation tests:
  https://docs.google.com/document/d/1SA16StFKjXZpWbEkQOxNnCQ_ngrPKohcf605rfsladA/edit?tab=t.0

older code that didnt specify english or us for the servers and didnt disable personalization

OLD VERSION #determine search results per month

```{r}


# Function to count search results by date
count_results_by_date <- function(api_key, search_engine_id, site, search_term, date_ranges) {
  #creates empty dataframe that stores search result counts
  results_count <- data.frame(
    date_range = character(),
    result_count = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (date_range in date_ranges) {
    # Building the search query
    query <- paste0('site:', site, ' "', search_term, '" after:', date_range[1], ' before:', date_range[2])
    
    # Make the API request
    response <- GET(
      "https://www.googleapis.com/customsearch/v1",
      query = list(
        key = api_key,
        cx = search_engine_id,
        q = query,
        num = 1  # requests only 1 result -only need the count, not actual results
      )
    )
    
    # Parse the response
    # Checks if the API request was successful (status code 200)
    if (status_code(response) == 200) {
      results <- fromJSON(rawToChar(response$content))
      
      # Extracts the total result count 
      total_results <- as.numeric(results$searchInformation$totalResults)
      
      # Add to dataframe
      new_row <- data.frame(
        date_range = paste(date_range[1], "to", date_range[2]),
        result_count = total_results,
        stringsAsFactors = FALSE
      )
      
      results_count <- rbind(results_count, new_row)
      
      message("Query: ", query)
      message("Results: ", total_results)
    } else {
      message("Error for date range ", date_range[1], " to ", date_range[2], 
              ": Status code ", status_code(response))
    }
    
    # One second pause to avoid hitting rate limits with API query
    Sys.sleep(1.5)
  }
  #Returns the completed data frame containing all date ranges and their corresponding result counts
  return(results_count)
}

# Define your parameters
site <- "www.dailywire.com"
search_term <- "kamala harris"

# Define date ranges to check (monthly intervals)
date_ranges <- list(
  c("2024-07-01", "2024-07-31"),
  c("2024-08-01", "2024-08-31"),
  c("2024-09-01", "2024-09-30"),
  c("2024-10-01", "2024-10-31"),
  c("2024-11-01", "2024-11-05")
)

# If you want to see how results are distributed within September
# september_ranges <- list(
#   c("2024-09-01", "2024-09-10"),
#   c("2024-09-11", "2024-09-20"),
#   c("2024-09-21", "2024-09-30")
# )

# If you want to see how results are distributed within a month

# august_ranges <- list(
#   c("2024-08-01", "2024-08-15"),
#   c("2024-08-16", "2024-08-31")
# )

# october_ranges <- list(
#   c("2024-10-01", "2024-10-10"),
#   c("2024-10-11", "2024-10-20"),
#   c("2024-10-21", "2024-10-31")
# )

```

Older code no us eng or personalization

ORIGINAL # Extract articles by date
```{r}
# Function to search using Google Custom Search API with pagination
google_search_all <- function(api_key, search_engine_id) {
  base_url <- "https://www.googleapis.com/customsearch/v1"
  all_results <- data.frame(
    url = character(),
    title = character(),
    snippet = character(),
    stringsAsFactors = FALSE
  )
  
  # The exact search query
  #Change the dates here
  full_query <- 'site:www.dailywire.com "kamala harris" after:2024-08-01 before:2024-08-10'
  
  # Google only allows up to 100 pages 
  for(start_index in seq(1, 300, 10)) {
    # Add delay to avoid hitting rate limits
    Sys.sleep(1)
    
    response <- GET(
      base_url,
      query = list(
        key = api_key,
        cx = search_engine_id,
        q = full_query,
        start = start_index
      )
    )
    
    results <- fromJSON(rawToChar(response$content))
    
    # Check if we have items in the response
    if(!is.null(results$items)) {
      page_df <- data.frame(
        url = results$items$link,
        title = results$items$title,
        snippet = results$items$snippet,
        stringsAsFactors = FALSE
      )
      
      all_results <- rbind(all_results, page_df)
      
      # Print progress
      cat("Retrieved results", start_index, "to", start_index + nrow(page_df) - 1, "\n")
    } else {
      # If no more results, break the loop
      break
    }
  }
  
  return(all_results)
}


# Get all results
all_urls_df <- google_search_all(api_key, search_engine_id)

# Remove any duplicates
august_10_urls_df <- unique(all_urls_df)

# Print total number of results
cat("\nTotal unique articles found:", nrow(all_urls_df), "\n")

# Save to CSV
#write.csv(sept_urls_df, "sept_kamala_harris_all_articles.csv", row.names = FALSE)
```


Older query without the parameters broken down



works # Query to fetch article URLs by date
```{r}
google_search_all <- function(api_key, search_engine_id) {
  base_url <- "https://www.googleapis.com/customsearch/v1"
  all_results <- data.frame(
    url = character(),
    title = character(),
    snippet = character(),
    stringsAsFactors = FALSE
  )
  
  # The exact search query
  #Change the dates here
  full_query <- 'site:www.dailywire.com "kamala harris" after:2024-08-01 before:2024-08-10'
  
  # Google only allows up to 100 pages 
  for(start_index in seq(1, 300, 10)) {
    # Add delay to avoid hitting rate limits
    Sys.sleep(1)
    
    response <- GET(
      base_url,
      query = list(
        key = api_key,
        cx = search_engine_id,
        q = full_query,
        start = start_index,
        personalization = "false",  # Disable personalization
        gl = "us",                  # Set country to US
        hl = "en"                   # Set language to English
      )
    )
    
    results <- fromJSON(rawToChar(response$content))
    
    # Check if we have items in the response
    if(!is.null(results$items)) {
      page_df <- data.frame(
        url = results$items$link,
        title = results$items$title,
        snippet = results$items$snippet,
        stringsAsFactors = FALSE
      )
      
      all_results <- rbind(all_results, page_df)
      
      # Print progress
      cat("Retrieved results", start_index, "to", start_index + nrow(page_df) - 1, "\n")
    } else {
      # If no more results, break the loop
      break
    }
  }
  
  return(all_results)
}
# Get all results
all_urls_df <- google_search_all(api_key, search_engine_id)
# Remove any duplicates
clean_urls_df <- unique(all_urls_df)
# Print total number of results
cat("\nTotal unique articles found:", nrow(clean_urls_df), "\n")
```



experiment to serve up batches of 100 articles
Experiment
```{r}
library(dplyr)
library(lubridate)
library(httr)
library(jsonlite)

# Completely rewritten function to retrieve all articles with dates
get_all_articles_with_dates <- function(api_key, search_engine_id, site, search_term, start_date, end_date) {
  base_url <- "https://www.googleapis.com/customsearch/v1"
  
  # Initialize empty dataframe to store results
  all_results <- data.frame(
    url = character(),
    title = character(),
    snippet = character(),
    date = as.Date(character()),
    stringsAsFactors = FALSE
  )
  
  # The exact search query
  full_query <- paste0('site:', site, ' "', search_term, '" after:', start_date, ' before:', end_date)
  
  # Google only allows up to 100 pages (10 results per page, max start index is 991)
  for(start_index in seq(1, 991, 10)) {
    # Add delay to avoid hitting rate limits
    Sys.sleep(1.5)
    
    cat("Requesting results starting at position", start_index, "\n")
    
    # Make the API request
    response <- GET(
      base_url,
      query = list(
        key = api_key,
        cx = search_engine_id,
        q = full_query,
        start = start_index,
        personalization = "false",
        gl = "us",
        hl = "en"
      )
    )
    
    # Check if response is valid
    if (status_code(response) != 200) {
      cat("Error: HTTP status", status_code(response), "\n")
      break
    }
    
    # Parse JSON content carefully
    content_text <- rawToChar(response$content)
    results <- tryCatch({
      fromJSON(content_text)
    }, error = function(e) {
      cat("Error parsing JSON:", e$message, "\n")
      return(NULL)
    })
    
    # Check if we have results
    if(is.null(results) || !("items" %in% names(results))) {
      cat("No items found for this page or end of results reached.\n")
      break
    }
    
    # Extract basic information
    urls <- sapply(results$items, function(x) x$link)
    titles <- sapply(results$items, function(x) x$title)
    snippets <- sapply(results$items, function(x) x$snippet)
    
    # Create a data frame for this batch
    batch_df <- data.frame(
      url = urls,
      title = titles,
      snippet = snippets,
      date = as.Date(NA),  # Initialize dates as NA
      stringsAsFactors = FALSE
    )
    
    # Now let's try to extract dates carefully for each result
    for (i in 1:nrow(batch_df)) {
      # Initialize with NA
      date_value <- as.Date(NA)
      
      # Make sure we don't go out of bounds
      if (i <= length(results$items)) {
        current_item <- results$items[[i]]
        
        # Using regex to extract dates from snippets (most reliable method)
        date_pattern <- "(\\d{4}[-/]\\d{2}[-/]\\d{2})"
        date_match <- regexpr(date_pattern, snippets[i])
        
        if (date_match > 0) {
          date_str <- substr(snippets[i], date_match, 
                             date_match + attr(date_match, "match.length") - 1)
          tryCatch({
            date_value <- as.Date(date_str)
          }, error = function(e) {
            # Keep as NA if conversion fails
          })
        }
      }
      
      # Update the date in our dataframe
      batch_df$date[i] <- date_value
    }
    
    # Add this batch to our results
    all_results <- rbind(all_results, batch_df)
    
    # Print progress
    cat("Retrieved", nrow(batch_df), "results. Total so far:", nrow(all_results), "\n")
    
    # If we have fewer than 10 results or we've reached our limit, we're done
    if (nrow(batch_df) < 10 || nrow(all_results) >= 1000) {
      cat("Reached end of results or limit.\n")
      break
    }
  }
  
  # Count how many articles have NA dates
  na_count <- sum(is.na(all_results$date))
  cat(na_count, "out of", nrow(all_results), "articles have no extractable date.\n")
  
  # If we have no dates at all, we'll use the order as a proxy for chronology
  if (na_count == nrow(all_results)) {
    cat("No dates could be extracted. Using result order as a proxy for chronology.\n")
    # Assign fake dates just for ordering purposes
    all_results$date <- as.Date(start_date) + seq(0, nrow(all_results) - 1)
  } else {
    # Sort by date (NA values will go to the end)
    all_results <- all_results[order(all_results$date), ]
  }
  
  return(all_results)
}

# Function to analyze articles in batches of 99
analyze_articles_in_batches <- function(articles_df, batch_size = 99) {
  # Ensure we have data to work with
  if(nrow(articles_df) == 0) {
    stop("No articles were found")
  }
  
  # Calculate number of batches
  num_batches <- ceiling(nrow(articles_df) / batch_size)
  
  # Create batch summary dataframe
  batch_summary <- data.frame(
    batch_num = integer(),
    start_date = as.Date(character()),
    end_date = as.Date(character()),
    start_index = integer(),
    end_index = integer(),
    num_articles = integer(),
    stringsAsFactors = FALSE
  )
  
  # Process each batch
  for(i in 1:num_batches) {
    start_idx <- (i-1) * batch_size + 1
    end_idx <- min(i * batch_size, nrow(articles_df))
    
    batch_data <- articles_df[start_idx:end_idx, ]
    
    # Get date range for this batch
    batch_dates <- batch_data$date[!is.na(batch_data$date)]
    if (length(batch_dates) > 0) {
      min_date <- min(batch_dates)
      max_date <- max(batch_dates)
    } else {
      # If no valid dates in this batch, use NA
      min_date <- as.Date(NA)
      max_date <- as.Date(NA)
    }
    
    # Summarize batch
    new_row <- data.frame(
      batch_num = i,
      start_date = min_date,
      end_date = max_date,
      start_index = start_idx,
      end_index = end_idx,
      num_articles = nrow(batch_data),
      stringsAsFactors = FALSE
    )
    
    batch_summary <- rbind(batch_summary, new_row)
  }
  
  return(batch_summary)
}

# Example usage
site <- "www.dailywire.com"
search_term <- "kamala harris"
start_date <- "2024-07-01"
end_date <- "2024-11-05"

# Get all articles - just call this function with your API key and search engine ID
articles_df <- get_all_articles_with_dates(api_key, search_engine_id, site, search_term, start_date, end_date)

# Example of how you would call the batch analysis
batch_summary <- analyze_articles_in_batches(articles_df)
# print(batch_summary)
```



```{r}
# Create a visualization of the batches
library(ggplot2)

ggplot(batch_summary, aes(x = factor(batch_num), y = num_articles)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = paste0(format(start_date, "%b %d"), " - ", format(end_date, "%b %d"))), 
            angle = 90, hjust = -0.1, size = 3) +
  labs(
    title = "DailyWire Articles Mentioning 'Kamala Harris' by Batch of 99",
    x = "Batch Number",
    y = "Number of Articles"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5)
  ) +
  coord_cartesian(ylim = c(0, 110))  # Make room for labels
```



