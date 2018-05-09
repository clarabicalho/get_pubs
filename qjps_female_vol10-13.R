# Author: Clara Bicalho
# Description: Scrape article titles and abstract of female authors in QJPS from Jan 1, 2015
# Wed May  9 14:01:04 2018 ------------------------------

library(XML)
library(rvest)
library(gender)
library(genderdata)

#read QJPS page
web <- read_html("https://www.nowpublishers.com/QJPS")
test <- web %>% html_nodes("a") %>% html_attrs()

# clean up
test <- test[-which(lapply(test, length) > 1)]
test <- lapply(test, function(x) grep(pattern = "Search", x, value = TRUE, invert = TRUE))
test <- test[lapply(test,length)>0]
start <- which(test == "/article/Details/QJPS-16095")
end <- which(test == "/article/Details/QJPS-13137")
test <- test[start:end]

#retrieve article info
base <- "https://www.nowpublishers.com"
arts <- lapply(test, function(i){
  page <- read_html(paste0(base, i))
  title <- page %>% html_nodes("h2") %>% html_text() %>% .[2]
  link <- paste0(base, i)
  abstract <- page %>% html_nodes("p") %>% html_text()  %>% .[2]
  return(list(title = title, link = link, abstract = abstract))
  })
articles <- bind_rows(arts)

#retrieve author info
author_info <- web %>% html_nodes(".search-result") %>% html_text()
a <- lapply(author_info, function(a){
  a <- gsub("  ", "", a, fixed = TRUE)
  a <- gsub("[\r\n]+", "%", a)
  a <- stringr::str_split_fixed(a, "%", 7)
  return(list(issue = paste0(a[[1]],a[[2]]), title = a[[3]], authors = paste0(a[[5]], a[[6]])))
})

author <- bind_rows(a)
dat <- left_join(articles, author, by = "title")

#delete editor's notes
dat <- dat[dat$abstract!="",]
dat[is.na(dat$authors),]

#retrieve 1 missing author
missing <- grep(pattern = "Senate Gate-Keeping", author$title, fixed = TRUE)
dat$authors[is.na(dat$authors)] <- trimws(author$authors[missing])

#correct 1 abstract
correct <- which(dat$abstract=="This is published under the terms of CC-BY.")
dat$abstract[correct] <- read_html(dat$link[correct]) %>% html_nodes("p") %>% html_text() %>% .[3]

#split authors names separate rows
author_split <- str_split(dat$authors, "[|]", simplify = TRUE)
dat$author_1 <- trimws(author_split[,1])
dat$author_2 <- trimws(author_split[,2])
dat$author_3 <- trimws(author_split[,3])
dat$author_4 <- trimws(author_split[,4])

dat <- dat %>% gather(author, ind_author, -c(title:authors))
dat <- dat[dat$ind_author!="",]

dat$name <- word(dat$ind_author, 1)
dat$name <- sub("^\\s+", "", dat$name)

gender <- gender(dat$name, years = c(1970, 2012),
                 method = c("ssa", "ipums", "napp","kantrowitz", "genderize", "demo"),
                 countries = c("United States", "Canada", "United Kingdom", "Denmark", "Iceland", "Norway", "Sweden"))

dat <- left_join(dat, gender[,c("name", "gender","proportion_female")], by = "name")

#correct gender / impute missing gender
# dat[dat$proportion_female < 0.9 & dat$gender=="female",] %>% View()
dat$gender[dat$ind_author == "Jean Guillaume Forand"] <- "male"

dat$ind_author[is.na(dat$gender)]
fnames <- c("C. Christine Fair ", "Kimuli Kasara", "Gilat Levy ", " Rocío Titiunik")
dat$gender[dat$ind_author %in% fnames] <- "female"

#identify gender-related abstracts
dat$gender_related <- grepl("women|gender|female", dat$abstract)

#prepare to export
dat <- dat %>% subset(gender == "female" | gender_related==TRUE)

dat <- dat %>% dplyr::rename(author_order = author,
                             first_name = name)

dat$last_name <- str_replace(dat$ind_author, dat$first_name, "")
dat$first_name <- trimws(dat$first_name, "both")
dat$last_name <- trimws(dat$last_name, "both")

dat$journal <- "QJPS"

dat <- dat %>% select(last_name, first_name, journal, title, link, abstract, issue)
dat <- dat[!duplicated(dat),]
dat <- dat %>% arrange(title)

#fix encoding issues
dat$abstract <- gsub("\n", "", dat$abstract, fixed = TRUE)
dat$abstract <- gsub("\u0092", "'", dat$abstract, fixed = TRUE)

con <- file("~/Downloads/qjps_female_vol10-13.csv", encoding="UTF-8")
write.csv(dat,file=con)
