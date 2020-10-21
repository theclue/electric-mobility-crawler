#!/usr/bin/env Rscript

##########################
# Requirements and Setup #
##########################
suppressMessages(library("rprojroot"))

if(!interactive()) setwd(find_root_file(criterion = is_git_root))

source(find_root_file("data-raw", "setup.R", criterion = is_git_root))

########################
# CRAWLING
##

# Forum Home
forum.home <- read_html(paste(proxycrawl.endpoint, proxycrawl.token, "&url=", URLencode(forum.home.url, reserved = TRUE), sep = ""))

# Forums List
forum.containers <- forum.home %>% html_nodes(xpath = "//ul[contains(@class, 'topiclist forums')]")

all.forums <- do.call(plyr::rbind.fill, lapply(forum.containers, function(f){
  
  category <- f %>% html_nodes(xpath = "parent::node()/ul[contains(@class, 'topiclist')]/li[contains(@class, 'header')]") %>% html_node("a") %>% html_text()
  
  if(length(category)==0) category <- NA
  
  data.frame(
    name = f %>% html_nodes(xpath="node()/*/dt/*/a[not(contains(@class, 'subforum'))]") %>% html_text(),
    url = f %>% html_nodes(xpath="node()/*/dt/*/a[not(contains(@class, 'subforum'))]") %>% html_attr("href") %>% gsub("(.*)\\?sid=.*", "\\1", .),
    topics = as.integer(f %>% html_nodes(xpath="node()/*/dd[contains(@class, 'topics')]") %>% html_text %>% gsub("([0-9]+)\\sArgomenti", "\\1", .)),
    messages = as.integer(f %>% html_nodes(xpath="node()/*/dd[contains(@class, 'posts')]") %>% html_text %>% gsub("([0-9]+)\\sMessaggi", "\\1", .)),
    # has.subforums = (function(sf){
    #   if(length(sf %>% html_nodes(xpath="node()/*/dt/a[contains(@class, 'subforum')]"))) return(TRUE)
    #   return(FALSE)
    #   })(f),
    stringsAsFactors = FALSE) %>%
    mutate(category = category) 
})) %>%
  mutate(proxy.url = paste(proxycrawl.endpoint, proxycrawl.token, "&url=", URLencode(url, reserved = TRUE), sep = ""))

# un.forum <- all.forums[11,]
# 
# un.forum.home <- read_html(un.forum$url)
# 
# un.forum.container <- un.forum.home %>%
#   html_nodes(xpath="//div[text()[contains(.,'Argomenti attivi')]]") %>%
#   html_nodes(xpath="ancestor::div[contains(@class,'inner')]") %>%
#   html_nodes(xpath="ul[contains(@class, 'topics')]/li")
# 
# un.forum.df <- data.frame(
#   title = un.forum.container %>% html_nodes(xpath="dl/dt/div[contains(@class, 'list-inner')]/a") %>% html_text(),
#   author = un.forum.container %>% html_nodes(xpath="dl/dt/div[contains(@class, 'list-inner')]/div[contains(@class, 'responsive-hide')]/a[contains(@class, 'username')]") %>% html_text(),
#   date = un.forum.container %>% html_nodes(xpath="dl/dt//div[contains(@class, 'list-inner')]/div[contains(@class, 'responsive-hide')]/a[contains(@class, 'username')]/parent::node()") %>% html_text() %>%
#     gsub("oggi", format(Sys.Date(), "%d/%m/%Y"), .) %>%
#     gsub("ieri", format(Sys.Date()-1, "%d/%m/%Y"), .) %>%
#     gsub(".*\\s([0-9]{1,2}\\/[0-9]{1,2}\\/201[0-9]).*", "\\1", .,  perl = TRUE),
#   time = un.forum.container %>% html_nodes(xpath="dl/dt//div[contains(@class, 'list-inner')]/div[contains(@class, 'responsive-hide')]/a[contains(@class, 'username')]/parent::node()") %>% html_text() %>% gsub(".*\\s([0-9]{1,2}:[0-9]{1,2}).*", "\\1", .),
#   replies = as.integer(un.forum.container %>% html_nodes(xpath="dl/dd[contains(@class, 'posts')]") %>% html_text() %>% gsub("([0-9]+)\\sRisposte", "\\1", .)),
#   views = as.integer(un.forum.container %>% html_nodes(xpath="dl/dd[contains(@class, 'views')]") %>% html_text() %>% gsub("([0-9]+)\\sVisite", "\\1", .)),
#   url = un.forum.container %>% html_nodes(xpath="dl/dt/div/a") %>% html_attr("href"),
#   stringsAsFactors = FALSE) %>%
#   mutate(datetime = as_datetime(gsub("\\(n|t)", "", paste(date, time)))) %>%
#   select(-date, -time)

all.rooms <- do.call(plyr::rbind.fill, lapply(all.forums$url, function(x) {
  forum.indexing(x, proxycrawl.endpoint, proxycrawl.token)
                 }))

forums.total <- all.forums
