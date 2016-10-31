# input: Open Secrets webpage
# output: a small-multiple graph that shows the political contribution 
# by interest group and by receipient party over the last 20 years

# Robin Lee
# 10/30/2016
library(ggplot2)
library(dplyr)
library(grid)
library(gridExtra)
agri_url <- "https://www.opensecrets.org/industries/totals.php?cycle=2016&ind=A"
comm_electr_url <- "https://www.opensecrets.org/industries/totals.php?ind=B"
constr_url <- "https://www.opensecrets.org/industries/totals.php?ind=C"
defense_url <- "https://www.opensecrets.org/industries/totals.php?ind=D"
energy_url <- "https://www.opensecrets.org/industries/totals.php?ind=E"
fin_url <- "https://www.opensecrets.org/industries/totals.php?ind=F"
health_url <- "https://www.opensecrets.org/industries/totals.php?ind=H"
law_lobby_url <- "https://www.opensecrets.org/industries/totals.php?ind=K"
# this group seems arbitrary grouped together ?

labor_url <- "https://www.opensecrets.org/industries/totals.php?cycle=2016&ind=P"
trans_url <- "https://www.opensecrets.org/industries/totals.php?ind=M"
misc_biz_url <- "https://www.opensecrets.org/industries/totals.php?ind=N"
single_issue_url <- "https://www.opensecrets.org/industries/totals.php?ind=Q"


int_urls <- mget(ls(pattern = "url$"))

# graph time series, facet_wrap, but how to sort them? well, we'll figure it out.

# a function that takes in url for an industry, and ouputs a claned data frame for me to plot

grab_interest_table <- function(int_url){
  library(rvest)
  html <- read_html(int_url)
  data <- html_nodes(html, "#profileLeftColumn > div:nth-child(5) > table") %>% html_table()
  data <- data[[1]]
  data <- data[!(data$`Election Cycle` == "Total" | data$`Election Cycle` == "2016"), ]
  
  data$`Total Contributions` <- gsub("[[:punct:]]", "", data$`Total Contributions`) %>% as.integer()
  return(data)
}

tables <- lapply(int_urls, grab_interest_table)
plot_int_group <- function(table){
  library(ggplot2)
  p <- ggplot(table, aes(`Election Cycle`, `Total Contributions`/1e6, group = 1))+
    geom_line()+
    ylab("Contribution in Millions")+
    xlab("Election Cycle (every two year)")+
    scale_y_continuous()+
    scale_x_discrete(breaks = c(1990,2000,2010))+
    theme(axis.line = element_line(colour = "black"),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          panel.background = element_blank())
  
  return(p)
}

plots <- lapply(tables, plot_int_group)

str(plots)
labels <- list("Agriculture", "Communication & \n Electronics", "Construction", "Defense",
               "Energy & \n Natural Resources", "Finance, Insurance & \n Real Estate", "Healthcare", "Labor Union",
               "Lawyers & \n Lobbyists", "Miscellaneous \n Businesses", "Single Issue", "Transportation")
add_label <- function(plot, label){
  plot + ggtitle(label)
}


n <- length(labels)

for(i in 1:n){
  plots[[i]] <- add_label(plots[[i]], labels[[i]])
  
}

for(i in 1:length(tables)){
  tables[[i]]$group <- labels[[i]]
}


rbinded <- plyr::rbind.fill(tables)

# str(rbinded)

names(rbinded) <- c("cycle", "total_contri", "contri_from_ind", "contri_from_pac", "soft_outside",
                    "to_dem", "to_rep", "percent_dem", "percent_rep", "group")

rbinded$cycle <- as.numeric(rbinded$cycle)

rm_punct_to_num <- function(vec){
  vec <- gsub("[[:punct:]]", "", vec) %>% as.numeric()
  return(vec)
}

rbinded$contri_from_ind <- rm_punct_to_num(rbinded$contri_from_ind)
rbinded$contri_from_pac <- rm_punct_to_num(rbinded$contri_from_pac)
rbinded$soft_outside <- rm_punct_to_num(rbinded$soft_outside)
rbinded$to_dem <- rm_punct_to_num(rbinded$to_dem)
rbinded$to_rep <- rm_punct_to_num(rbinded$to_rep)


sum_int <- rbinded %>% group_by(group) %>%
  summarize(sum = sum(as.numeric(total_contri)))

rbinded <- rbinded %>% left_join(sum_int)

rbinded$group_sorted <- factor(rbinded$group, levels = unique(rbinded[order(rbinded$sum, decreasing = TRUE), "group"]))

rbinded$grid_y <- ifelse(as.numeric(rbinded$group_sorted) <= 4, 1, 
                         ifelse(as.numeric(rbinded$group_sorted) <= 8, 2, 3))


rbinded$grid_x <- ifelse(as.numeric(rbinded$group_sorted)%%4 == 1, 1, 
                         ifelse(as.numeric(rbinded$group_sorted)%%4 == 2, 2,
                                ifelse(as.numeric(rbinded$group_sorted)%%4 == 3, 3, 4)))


p_split <- ggplot(rbinded)+
  geom_line(aes(cycle, to_rep/1e6, group = 1), colour = "red")+
  geom_line(aes(cycle, to_dem/1e6, group = 1), colour = "blue")+
  facet_grid( grid_y ~ grid_x, scales = "free_y")+
  xlab("Election Cycle")+
  ylab("Contribution in Millions")+
  scale_x_continuous(breaks = c(1990,2000,2010))+
  theme(
    axis.line.x = element_line(), 
    axis.line.y = element_line(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    strip.text.y = element_blank(), 
    strip.text.x = element_blank(), 
    panel.margin = unit(0.1, "lines"))

text_df_2 <- data.frame( x= rep(2003, 12), 
                         y = rep(c(320, 98, 62), each = 4),
                         lab = levels(rbinded$group_sorted), 
                         grid_x = rep(1:4, 3), 
                         grid_y = rep(1:3, each = 4))

p_split_text <- p_split + geom_text(aes(x = x, y = y, label = lab),
                                    inherit.aes = FALSE,
                                    data = text_df_2, size = 4)

grid.arrange(p_split_text, 
             top = textGrob("Where do political contributions come from?", gp = gpar(fontsize = 18)),
             bottom = textGrob(
               "Political contributions from different interests groups to the Republican \n party and the Democratic party between 1989 and 2014. The data are \n collected every two-year election cycle. Thus, the up represents the \n cycle with presidential election, while the down represents off-cycle. Panels \n are sorted by the total contribution from each interest group. The graph shows \n that labor union contributions lean towards Democrats more than other interest groups. \n Construction, agriculture and transportation lean towards the Republicans", gp = gpar(fontsize = 11)))

library(bigrquery)
library(tidyverse)
library(httr)
# state code to state dictionary
state_code <- read_delim("http://www2.census.gov/geo/docs/reference/state.txt", 
                         delim = '|')

# calculate amount by recipient state

projectid = 'data-science-147220'
q1 = 'SELECT upper(recipient_state) as recipient_state, sum(amount) total_amount
FROM [data-science-147220:dime.contrib_2012] 
group by 1 '
query_exec(q1, projectid)

total_receive <- query_exec(q1, projectid)

total_receive$recipient_state

data(state)
state_abb_50 = state.abb
state_abb_50
not_in <- total_receive %>% filter(!recipient_state %in% state_code$STUSAB)
not_in$recipient_state
sum(not_in$total_amount, na.rm = TRUE)


matched_state <- total_receive %>% filter(recipient_state %in% state_code$STUSAB)
sum(matched_state$total_amount, na.rm = TRUE)


total_receive$recipient_state

# population by state
api_base_url <- 'http://api.census.gov/data/2010/sf1?'
http://api.census.gov/data/2010/sf1?get=P0080001&for=state

var = 'P0080001'
geo = 'state'
api_long_url <- paste(api_base_url, 'get=' , var, '&for=', geo, sep = '')
us_pop <- api_long_url

r <- GET(api_long_url)
r
pop_data <- content(r)
pop_value <- pop_data[2:53]
pop_values_df <- data.frame(matrix(unlist(pop_value), ncol = 2, byrow = TRUE), stringsAsFactors = FALSE)
names(pop_values_df) <- unlist(pop_data[[1]])
names(pop_values_df)[1] <- 'total_population'
pop_values_df_enriched <- pop_values_df %>% left_join(state_code, by = c('state' = 'STATE'))
str(pop_values_df_enriched)

amount_pop <- pop_values_df_enriched %>% 
  left_join(total_receive, by = c('STUSAB' = 'recipient_state'))
amount_pop <- amount_pop %>% 
  rename(receive_amount = total_amount) %>%
  mutate(receive_amount = as.numeric(receive_amount), 
         total_population = as.numeric(total_population))

p1 <- ggplot(data = amount_pop, aes(total_population/1e6, receive_amount/1e6, label = STUSAB))+
  geom_text()+
  labs(title = 'Amount of Disclosed Political Contributions in 2012 Election Cycle',
       x = 'Recipient State Population (Millions)', 
       y = 'Total Amount (Millions)')

p2 <- ggplot(data = amount_pop %>% filter(!STUSAB %in% c('FL', 'DC', 'CA', 'TX', 'NY')), 
             aes(total_population/1e6, receive_amount/1e6, label = STUSAB)) +
  geom_text()+
  labs(title = 'Excluding FL, DC, CA, TX, NY',
       x = 'Recipient State Population (Millions)', 
       y = 'Total Amount (Millions)')

library(grid)
library(gridExtra)

grid.arrange(p1, p2, nrow = 1, 
             top = textGrob('Where does the money go?', gp = gpar(fontsize = 18)),
             bottom = textGrob('Florida and DC receive extra money relative to their population size. States such as Virginia and Ohio receive more money probably due to being a swing state.', gp = gpar(fontsize = 13)))
