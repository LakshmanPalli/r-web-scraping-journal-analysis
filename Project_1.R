#### Task 1: Set Up Your R Environment

## installation and loading the necessary libs

install.packages("rvest")
install.packages("httr")
install.packages("xml2")
install.packages("ggplot2")

library(rvest)
library(httr)
library(xml2)
library(ggplot2)




#### Task 2: Scraping Article Data

## Extracting Title, Authors, Correspondence Author, Correspondence Author's Email, Publish Date, Abstract, Keywords, from all the journals given a year

# Define the function to extract all articles
extract_all_articles <- function(year) {
  # Construct the URL based on the input year
  volume <- year - 2005
  url <- paste0("https://neuraldevelopment.biomedcentral.com/articles?query=&volume=", volume, "&searchType=&tab=keyword")
  
  # Read the HTML content of the page
  page <- read_html(url)
  
  # Extract all href attributes
  href_values <- page %>% html_nodes("a[data-test='title-link']") %>% html_attr("href")
  
  # Construct absolute URLs
  absolute_urls <- paste0("https://neuraldevelopment.biomedcentral.com", href_values)
  
  # Initialize lists to store extracted data
  titles <- authors <- correspondence_authors <- correspondence_emails <- publish_dates <- abstracts <- keywords <- list()
  
  # Loop through each article URL and extract data
  for (article_url in absolute_urls) {
    # Read the HTML content of the article page
    article_page <- read_html(article_url)
    
    # Extract title
    title <- article_page %>% html_node("h1.c-article-title") %>% html_text(trim = TRUE)
    titles <- c(titles, title)
    
    # Extract author names
    author_names <- article_page %>% html_nodes("ul.c-article-author-list a[data-test='author-name']") %>% html_text(trim = TRUE) %>% paste(collapse = ", ")
    authors <- c(authors, author_names)
    
    # Extract corresponding author's name
    corresponding_author_name <- article_page %>% html_node("p#corresponding-author-list a") %>% html_text(trim = TRUE)
    correspondence_authors <- c(correspondence_authors, corresponding_author_name)
    
    # Extract corresponding author's email address
    corresponding_author_email <- article_page %>% html_node("p#corresponding-author-list a") %>% html_attr("href") %>% gsub("mailto:", "", .)
    correspondence_emails <- c(correspondence_emails, corresponding_author_email)
    
    # Extract the publication date
    publication_date <- article_page %>% html_node("time") %>% html_attr("datetime")
    publish_dates <- c(publish_dates, publication_date)
    
    # Extract abstract content
    abstract <- article_page %>% html_node("section[aria-labelledby='Abs1'] p") %>% html_text(trim = TRUE)
    abstracts <- c(abstracts, abstract)
    
    # Extract keyword elements
    keyword_elements <- article_page %>% html_nodes("ul.c-article-subject-list a")
    
    # Extract keywords if keyword elements are found, otherwise set to NA
    if (length(keyword_elements) > 0) {
      keywords_text <- keyword_elements %>% html_text(trim = TRUE) %>% paste(collapse = ", ")
      keywords <- c(keywords, keywords_text)
    } else {
      keywords <- c(keywords, NA)
    }
  }
  
  # Combine extracted data into a data frame
  articles_df <- data.frame(
    Title = titles,
    Authors = authors,
    Correspondence_Author = correspondence_authors,
    Correspondence_Email = correspondence_emails,
    Publish_Date = publish_dates,
    Abstract = abstracts,
    Keywords = keywords,
    stringsAsFactors = FALSE
  )
  
  return(articles_df)
}

# Call the function with the input year
year <- 2024                                  ## year: user input!! {please select any year from 2006 to 2024}
article_data <- extract_all_articles(year)



#### Task 3: Data Cleaning and Preprocessing

## Removal of white spaces, and format handling

# Function to clean and preprocess article data
clean_and_preprocess_data <- function(articles_df) {
  # Remove leading and trailing whitespace from the title
  if (!is.null(articles_df$Title)) {
    articles_df$Title <- trimws(articles_df$Title)
  }
  
  # Remove any HTML tags from the abstract
  if (!is.null(articles_df$Abstract)) {
    articles_df$Abstract <- gsub("<.*?>", "", articles_df$Abstract)
  }
  
  # Remove leading and trailing whitespace from the abstract
  if (!is.null(articles_df$Abstract)) {
    articles_df$Abstract <- trimws(articles_df$Abstract)
  }
  
  # Remove any leading or trailing whitespace from author names
  if (!is.null(articles_df$Authors)) {
    articles_df$Authors <- trimws(articles_df$Authors)
  }
  
  # Remove any empty strings from author names
  if (!is.null(articles_df$Authors)) {
    articles_df$Authors <- articles_df$Authors[articles_df$Authors != ""]
  }
  
  # Remove leading and trailing whitespace from the corresponding author's name and email
  if (!is.null(articles_df$Correspondence_Author)) {
    articles_df$Correspondence_Author <- trimws(articles_df$Correspondence_Author)
  }
  if (!is.null(articles_df$Correspondence_Email)) {
    articles_df$Correspondence_Email <- trimws(articles_df$Correspondence_Email)
  }
  
  # ToDo: Add cleaning and preprocessing for other fields as needed
  
  # Return the cleaned and preprocessed data frame
  return(articles_df)
}


# Function to extract all articles with data cleaning and preprocessing
extract_all_articles_cleaned <- function(year) {
  # Extract articles
  articles_df <- extract_all_articles(year)
  
  # Clean and preprocess the extracted data
  cleaned_articles_df <- clean_and_preprocess_data(articles_df)
  
  # Return the cleaned and preprocessed data frame
  return(cleaned_articles_df)
}

# Call the function with the input year
year <- 2024                             ## year: user input!! {please select any year from 2006 to 2024}
article_data_cleaned <- extract_all_articles_cleaned(year)





#### Task 4: Data Analysis and Visualization

## Two plots a) keyword cloud b) papers published in each year 

# a) The no. of keywords(y) across each keyword(x) in each journal

url <- "https://neuraldevelopment.biomedcentral.com/articles/10.1186/s13064-024-00180-8" ## should provide the link manually {please provide journal link}
page <- read_html(url)

# Initialize an empty list to store counts for each keyword
keyword_counts <- list()


# Extract keyword elements
keyword_elements <- page %>% html_nodes("ul.c-article-subject-list a")

# Extract keywords if keyword elements are found, otherwise set to NA
keywords <- if (length(keyword_elements) > 0) keyword_elements %>% html_text() else NA
  
article_content <- page %>% html_nodes("article") %>% html_text()

# Iterate over each keyword
for (keyword in keywords) {
  # Find count of occurrences of the keyword within the article content
  count <- length(gregexpr(keyword, article_content, ignore.case = TRUE)[[1]])
  
  # Store the count for the keyword
  keyword_counts[[keyword]] <- count
}

# Convert the list of counts to a data frame for easier manipulation
keyword_counts_df <- data.frame(keyword = names(keyword_counts),
                                frequency = unlist(keyword_counts),
                                stringsAsFactors = FALSE)


# Sort the data frame by frequency in descending order
keyword_counts_df <- keyword_counts_df[order(keyword_counts_df$frequency, decreasing = TRUE),]

# Print the keyword counts data frame
print(keyword_counts_df)

# Plotting
ggplot(keyword_counts_df, aes(x = keyword, y = frequency)) + # specify the data and the x and y variables
  geom_col() + # draw the bars
  labs(title = "Keywords & Keyword Count", x = "Keyword", y = "Frequency") # add title and labels


# b) The no.of papers published(y) across year(X) in this topic

# Function to extract the number of papers published in a given year
publish_papers_count <- function(year) {
  volume <- year - 2005
  url <- paste0("https://neuraldevelopment.biomedcentral.com/articles?query=&volume=",volume,"&searchType=&tab=keyword")
  page <- read_html(url)
  
  # Extract the number of search results
  result_count <- page %>%
    html_node("h2[data-test='result-title'] strong[data-test='search-results-count']") %>%
    html_text() %>%
    as.numeric()
  
  return(result_count)
}


# Range of years to analyze
start_year <- 2006
end_year <- 2024

# Initialize vectors to store year and paper counts
years <- start_year:end_year
paper_counts <- numeric(length(years))


# Loop through each year to get the paper count
for (i in 1:length(years)) {
  paper_counts[i] <- publish_papers_count(years[i])
}

# Create a data frame
papers_df <-
  data.frame(year = years, papers_published = paper_counts)

# Plotting
ggplot(papers_df, aes(x = year, y = papers_published)) +
  geom_line() +
  geom_point() +
  scale_x_continuous(breaks = years, labels = years) +  # Display every year on x-axis
  scale_y_continuous(breaks = seq(0, max(paper_counts), by = 5)) +  # Adjust y-axis to range at intervals of 5 units
  labs(title = "Number of Papers Published in Neural Development, Each Year (2006-2024)",
       x = "Year",
       y = "Number of Papers Published")




