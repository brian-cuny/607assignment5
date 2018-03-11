library(tidyverse)
library(magrittr)
library(jsonlite)
library(yaml)
library(XML)

Format.Raw.Data = function(df){
  df %>%
    separate('book_number', c('book_number', 'series_length'), sep=' of ') %>%
    map_df(~str_replace_all(., '^(The )([[:print:]]+)','\\2, The'))
}


# json --------------------------------------------------------------------
json.raw.data <- fromJSON('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\607\\607assignment5\\authors.json')[[1]]

json.data.frame <- json.raw.data %>%
  Format.Raw.Data()


# yaml --------------------------------------------------------------------
yaml.raw.data <- read_yaml('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\607\\607assignment5\\authors.yaml')[[1]]

yaml.data.frame <- yaml.raw.data %>%
  unlist(recursive=F) %>%
  matrix(ncol=6, byrow=T) %>%
  as.data.frame() %>%
  setNames(c('title', 'authors', 'series', 'book_number', 'release_year', 'pages')) %>%
  Format.Raw.Data()


# html --------------------------------------------------------------------
Html.Trimmer = function(value){
  value %>%
    xmlValue() %>%
    str_trim()
}

html.raw.data <- htmlParse('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\607\\607assignment5\\authors.html')

html.data.frame <- html.raw.data %>%
  xpathSApply('//table//tbody//td', Html.Trimmer) %>% 
  matrix(ncol=6, byrow=T) %>%
  as.data.frame() %>%
  setNames(html.raw.data %>%
             xpathSApply('//table//thead//td', xmlGetAttr, 'id')
           ) %>%
  Format.Raw.Data()


# xml ---------------------------------------------------------------------

#authors names is messed up
xml.raw.data <- xmlParse('C:\\Users\\Brian\\Desktop\\GradClasses\\Spring18\\607\\607assignment5\\authors.xml')


xml.data.frame <- xml.raw.data %>%
  xpathSApply('//book//*[not(name()="author")]', xmlValue) %>% 
  matrix(ncol=6, byrow=T) %>%
  as.data.frame() %>%
  setNames(xml.raw.data %>%
           xpathSApply('//book//*[not(name()="author")]', xmlName) %>%
           .[1:6]
          ) %>%
  Format.Raw.Data()





