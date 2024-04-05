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
library(tidyverse)




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
  titles <- authors <- correspondence_authors <- correspondence_emails <- publish_dates <- abstracts <- keywords <- contents <- c()
  # Loop through each article URL and extract data
  for (article_url in absolute_urls) {
    # Read the HTML content of the article page
    article_page <- read_html(article_url)
    
    # Extract title
    title <- article_page %>% html_node("h1.c-article-title") %>% html_text(trim = TRUE)
    titles <- c(titles, title)
    # Extract Text
    content <- article_page %>%  html_nodes("article") %>% html_text()
    contents <- c(contents,content)
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
    Keywords =keywords,
    Contents = contents,
    stringsAsFactors = FALSE
  )
  
  return(articles_df)
}

# Call the function with the input year
year <- 2024                                  ## year: user input!! {please select any year from 2006 to 2024}
article_data <- extract_all_articles(year)

# Task 3 Cleaning- 
# Loads all Article data into a list from 2006 to 2024
article_list <-lapply(2024:2006,
                      function(x){
                        extract_all_articles(x)
                      })

# renames every element with corresponding year
names(article_list) <- as.character(2024:2006)

#Combines list of dataframes and creates publish year
article_final <- bind_rows(article_list) %>% 
  mutate(Publish_Date=ymd(Publish_Date),
         Publish_Year=year(Publish_Date))

write_csv(article_final %>% select(-Contents),"Neural_Development_articles.csv")
Neural_develop <- read_csv("Neural_Development_articles.csv")
# Fixing Keyword Column
article_finallong <- article_final %>% separate_longer_delim(Keywords,delim = ", ") %>% 
  replace_na(list(Keywords="No keywords"))


#### Task 4: Data Analysis and Visualization

## Two plots a) keyword cloud b) papers published in each year 

# a) The no. of keywords(y) across each keyword(x) in each journal

articles_year <- article_finallong %>%
  filter(Publish_Year==2024)

create_keyword_count <-function(data,keywords){
  keyword_count <- c()
  for(i in 1:length(keywords)){
    keyword_count[i]<-str_count(data[["Contents"]][i],paste0("(?i)",keywords[i]))
  }
  keyword_count
}

articles_year$Keyword_Count<-create_keyword_count(articles_year,articles_year$Keywords)

articles_year %>% 
  ggplot(aes(x=reorder(Keywords,Keyword_Count),y=Keyword_Count,fill=Title)) +
  geom_bar(stat = "identity") + theme_classic()+ 
  labs(title = "Key Word Appearances in Neural Development Articles (2024)",
       x="Key words",y="Appearances")+ coord_flip()


# b) The no.of papers published(y) across year(X) in this topic

num_of_articles <- article_final %>% group_by(Publish_Year) %>%
  summarise(Publish_Amount=n())
num_of_articles %>%
  ggplot(aes(x=Publish_Year,y=Publish_Amount)) + geom_line()+
  geom_smooth(method="loess")+
  theme_classic() + labs(title = "Number of Articles Published in Neural Development (2006-2024)",
                         x="Year Published",
                         y="Number of Articles") 

