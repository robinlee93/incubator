# challenge 2
urls <- c()
urls <- for(i in 1:12){
  
}

  https://s3.amazonaws.com/tripdata/201501-citibike-tripdata.zip

library(rvest)
base.url <- read_html("https://s3.amazonaws.com/tripdata/index.html")
data <- base.url %>% 
  html_nodes("#a")  %>%
  html_attr('href')

# choose again, specify hyperlink
links <- base.url %>%
  html_nodes("#system-data li a") %>%
  html_attr("href")

# i want trip data
trip_links <- links[1:27]

trip_links
data
