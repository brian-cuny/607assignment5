library(tidyverse)
library(magrittr)
library(jsonlite)
library(yaml)
library(XML)

Format.Raw.Data <- function(df){
  df %>%
    separate('book_number', c('book_number', 'series_length'), sep=' of ') %>%
    map_df(~str_replace_all(., '^(The|A) (.*)','\\2, \\1')) %>%
    arrange(title)
}


# json --------------------------------------------------------------------
json.raw.data <- fromJSON('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\607\\607assignment5\\authors.json')[[1]]

json.data.frame <- json.raw.data %>%
  Format.Raw.Data()


# yaml --------------------------------------------------------------------
yaml.raw.data <- read_yaml('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\607\\607assignment5\\authors.yaml')[[1]]

yaml.names <- yaml.raw.data[[1]] %>% 
  names() %>% 
  unique()

yaml.data.frame <- yaml.raw.data %>%
  unlist(recursive=F) %>%
  matrix(ncol=yaml.names %>% length(), byrow=T) %>%
  as.data.frame() %>%
  setNames(yaml.names) %>%
  Format.Raw.Data()


# html --------------------------------------------------------------------
Html.Trimmer <- function(value){
  value %>%
    xmlValue() %>%
    str_trim()
}

html.raw.data <- htmlParse('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\607\\607assignment5\\authors.html')

html.names <- html.raw.data %>%
  xpathSApply('//table//thead//td', xmlGetAttr, 'id')

html.data.frame <- html.raw.data %>%
  xpathSApply('//table//tbody//td', Html.Trimmer) %>% 
  matrix(ncol=html.names %>% length(), byrow=T) %>%
  as.data.frame() %>%
  setNames(html.names) %>%
  Format.Raw.Data()


# xml ---------------------------------------------------------------------

Xml.Processor <- function(value){
  ifelse(value %>% xmlSize() > 1,
    value %>% xmlChildren() %>% 
             map(. %>% 
                   xmlValue()
                 ) %>% 
             unlist() %>% 
             list(),
    value %>% xmlValue
  )
}

xml.raw.data <- xmlParse('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\607\\607assignment5\\authors.xml')

xml.names <- xml.raw.data %>%
  xpathSApply('//book/*', xmlName) %>%
  unique()

xml.data.frame <- xml.raw.data %>%
  xpathSApply('//book/*', Xml.Processor) %>% 
  matrix(ncol=xml.names %>% length(), byrow=T) %>%
  as.data.frame() %>%
  setNames(xml.names) %>%
  Format.Raw.Data()



