suppressMessages(library("dplyr"))
suppressMessages(library("rvest"))
suppressMessages(library("lubridate"))
suppressMessages(library("futile.logger"))

proxycrawl.token <- Sys.getenv("PROXYCRAWL_TOKEN")
proxycrawl.endpoint <- "https://api.proxycrawl.com/?token="

forum.home.url <- "https://www.forumelettrico.it/forum/"

forum.indexing <- function(url, proxycrawl.endpoint, proxycrawl.token){
  
  parent.url <- url
  
  flog.info(sprintf("Indexing %s", url))
  
  proxy.url <- paste(proxycrawl.endpoint, proxycrawl.token, "&url=", URLencode(url, reserved = TRUE), sep = "")
  
  proxy.url <- url
  
  current.file <- file.path(".", "data", gsub("https:\\/\\/www\\.forumelettrico\\.it\\/forum\\/(.+\\.html)", "\\1", url))
  
  if(file.exists(current.file)) {
    un.forum.home <- read_html(file(current.file))
  } else {
    un.forum.home <- read_html(proxy.url)
    write_xml(un.forum.home, current.file)
  }
  
  un.forum.subforums.container <- un.forum.home %>%
    html_nodes(xpath="//div[text()[contains(.,'Forum')]]") %>%
    html_nodes(xpath="ancestor::div[contains(@class,'inner')]") %>%
    html_nodes(xpath="ul[contains(@class, 'forums')]/li")
  
  if(length(un.forum.subforums.container)==0) return(NULL)
  
  un.forum.subforums.df <- data.frame(
    title = un.forum.subforums.container %>% html_nodes(xpath="dl/dt/div[contains(@class, 'list-inner')]/a[contains(@class, 'forumtitle')]") %>% html_text(),
    url = un.forum.subforums.container %>% html_nodes(xpath="dl/dt/div[contains(@class, 'list-inner')]/a[contains(@class, 'forumtitle')]") %>% html_attr("href") %>% gsub("(.*)\\?sid=.*", "\\1", .),
    stringsAsFactors = FALSE) %>%
    mutate(parent.url = parent.url)
  
  return(plyr::rbind.fill(
    un.forum.subforums.df,
    do.call(plyr::rbind.fill, lapply(un.forum.subforums.df$url, function(u){
      forum.indexing(u, proxycrawl.endpoint, proxycrawl.token)
    }))))
  
}
