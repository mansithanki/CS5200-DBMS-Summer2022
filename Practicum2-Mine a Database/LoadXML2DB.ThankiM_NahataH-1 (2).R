# Author 1: Mansi Pravin Thanki (thanki.m@northeastern.edu) 
# Author 2:Hardik Nahata (nahata.h@northeastern.edu)
# Assignment: Practicum II (Mine Database) - Part 1
# Course: CS5200
# Date: 08/12/2022

library(RSQLite)
library(XML)
library(DBI)
library(knitr)
library(dplyr)
library(sqldf)
#install.packages("XML")
library(XML)
library(xml2)
library(tidyr)


xmlfile <- "./pubmed-tfm-xml/pubmed22n0001-tf.xml"
#driver_data = read_xml(xmlfile) %>% as_list()
#driver_data
xmlDOM <- xmlParse(xmlfile)
r <- xmlRoot(xmlDOM)

# dataframe initialization for author 
author <- data.frame(authorid = vector(mode = "integer"),
                     firstName = vector(mode = "character"),
                     lastName = vector(mode = "character"),
                     initials = vector(mode = "character"),
                     stringsAsFactors = F
)

# dataframe initialization for Articles 
Articles <- data.frame(pmid = vector(mode = "integer"),
                       language = vector(mode = "character"),
                       title = vector(mode = "character"),
                       stringsAsFactors = F
)

# dataframe initialization for author_article_junction 
author_article_junction <- data.frame(authorid = vector(mode = "integer"),
                                      pmid = vector(mode = "integer"),
                                      stringsAsFactors = F
)

# dataframe initialization for journal 
journal <- data.frame(issn = vector(mode = "integer"),
                      title = vector(mode = "character"),
                      stringsAsFactors = F
)

# dataframe initialization for article_Journal_Junction (as shown in chalk talk video by Prof Martin)
article_Journal_Junction <- data.frame(  pmid = vector(mode = "integer"), 
                                         issn = vector(mode = "integer"), 
                                         publication_day = vector(mode = "integer"),
                                         publication_month = vector(mode = "character"),
                                         publication_year = vector(mode = "integer"),
                                         issn_type = vector(mode = "character"),
                                         stringsAsFactors = F
)


  numSize <- xmlSize(r)
  auth_counter = 0
  for(i in 1:numSize){
  pubmedarticle <- r[[i]]
  article <- pubmedarticle[[1]]
  language <- xmlValue(article[[2]])
  lang_var <- article[[3]]
  if(lang_var=="Language"){
    article_title <- xmlValue(article[[4]])
    list_of_authors <- article[[5]]
  }
  else {
    article_title <- xmlValue(article[[3]])
    list_of_authors <- article[[4]]
  }
  
  journals <- article[[1]]
  if (xmlName(journals[[1]]) == "ISSN"){
  issn_root <- journals[[1]]
  issnType <- xmlAttrs(issn_root)
  issn <- xmlValue(journals[[1]])
  journal_title <- xmlValue(journals[[3]])
  issoAbbr <- xmlValue(journals[[4]])
  
  
  pubmed_id <- xmlAttrs(pubmedarticle)
  journal_IssueDetails <- journals[[2]]
  
  # volume tag was not present in some cases so handled the case for it:
  if(xmlName(journal_IssueDetails[[1]]) == "Volume")
  {
    pubDate <- journal_IssueDetails[[3]]
  }
  else {
    pubDate <- journal_IssueDetails[[2]]
  }
  
  Year <- xmlValue(pubDate[[1]])
  Month <- " "
  Day <- 1
  
  if(!is.null(pubDate[[1]])){
  # some records did not have Year, Month, Day tags. Instead they had MedlineDate. Used substr to process
  if (xmlName(pubDate[[1]]) == "MedlineDate"){
     Year <- substr(xmlValue(pubDate[[1]]),0,5)
     Month <- substr(xmlValue(pubDate[[1]]),6,8)
   }
  else {
    Year <- xmlValue(pubDate[[1]])
    Month <- "Jan"
    Day <- 1
  }
    }

  if(!is.null(pubDate[[2]])){
    Month <- xmlValue(pubDate[[2]])
  }


  if(!is.null(pubDate[[3]])){
    Day <- xmlValue(pubDate[[3]])
  }
  
  # Loading data into Author and author_article_junction tables
  counter = 1
  while (!is.null(list_of_authors[[counter]])) {
    authors = list_of_authors[[counter]]
    lastName <- xmlValue(authors[[1]])
    
    # there was presence of records where forename was not available
    if(!is.null(authors[[2]])){
      foreName <- xmlValue(authors[[2]])
    }
    else{
      foreName <- " "
    }
    #print(lastName)
    
    #print(author[author$lastName == lastName & author$firstName == foreName, ]$authorid)
    if((lastName %in% author$lastName & foreName %in% author$firstName )){
      if(foreName %in% author[author$lastName == lastName, ]$firstName ) {
        #print('if')
        #print(author[author$lastName == lastName & author$firstName == foreName, ]$authorid)
        idValue = author[author$lastName == lastName & author$firstName == foreName, ]$authorid
        rno = nrow(author_article_junction) + 1
        author_article_junction[rno,]$authorid <- idValue[!is.na(idValue)]
        author_article_junction[rno,]$pmid <- pubmed_id

      }
    }
    
    # if author is a new entry:
    else{
      #print('else')
      auth_counter = auth_counter + 1
      idx = nrow(author) + 1
      author[idx,]$authorid <- auth_counter
      author[idx,]$lastName <- lastName
      author[idx,]$firstName <- foreName
      author[idx,]$initials <- xmlValue(authors[[3]])

      rno = nrow(author_article_junction) + 1
      author_article_junction[rno,]$authorid <- auth_counter # assigning id
      author_article_junction[rno,]$pmid <- pubmed_id # assigning pmid
    }

    counter = counter+1
  }
  }
  
  ############## loading data for journal data frame ###################
  if(!(issn %in% journal$issn)) {
    index <- nrow(journal) + 1
    journal[index,]$issn  <- issn
    journal[index,]$title  <- journal_title
  }
  
  ############## loading data for articles data frame ###################
  Articles[i,]$pmid <- pubmed_id
  Articles[i,]$language <- language
  Articles[i,]$title <- article_title

 
############## loading data for article_Journal_Junction data frame ###################
  article_Journal_Junction[i,]$issn  <- issn
  article_Journal_Junction[i,]$pmid  <- pubmed_id
  article_Journal_Junction[i,]$publication_year  <- Year
  article_Journal_Junction[i,]$publication_month  <- Month
  article_Journal_Junction[i,]$publication_day  <- Day
  article_Journal_Junction[i,]$issn_type <- issnType
}


######################### LOADIND DATA INTO SQLITE ##################################
dbfile = "pubmed.db"
dbcon <- dbConnect(RSQLite::SQLite(), dbfile)

dbExecute(dbcon, "PRAGMA foreign_keys = ON")
dbExecute(dbcon,'DROP TABLE IF EXISTS author_article_junction')
dbExecute(dbcon,'DROP TABLE IF EXISTS article_Journal_Junction')
dbExecute(dbcon,'DROP TABLE IF EXISTS Authors')
dbExecute(dbcon,'DROP TABLE IF EXISTS Journals')
dbExecute(dbcon,'DROP TABLE IF EXISTS Articles')

################################ CREATING TABLES ############################################

dbExecute(dbcon,'CREATE TABLE Authors(
  authorid INT PRIMARY KEY,
  firstName TEXT,
  lastName TEXT,
  Initials TEXT
)
')

dbExecute(dbcon,'CREATE TABLE Articles(
  pmid INT PRIMARY KEY,
  language TEXT,
  title TEXT
)')

dbExecute(dbcon,'CREATE TABLE Journals(
issn TEXT PRIMARY KEY,
title TEXT
)
')

dbExecute(dbcon,'CREATE TABLE author_article_junction(
  pmid INT,
  authorid INT,
  PRIMARY KEY(pmid,authorid),
  FOREIGN KEY(pmid) REFERENCES Articles(pmid),
  FOREIGN KEY(authorid) REFERENCES Authors(authorid)
)')

dbExecute(dbcon,'CREATE TABLE article_Journal_Junction(
  pmid INT,
  issn TEXT,
  publication_day TEXT,
  publication_month TEXT,
  publication_year TEXT,
  issn_type TEXT,
  
  PRIMARY KEY(pmid,issn),
  FOREIGN KEY(pmid) REFERENCES Articles(pmid),
  FOREIGN KEY(issn) REFERENCES Journals(issn)
)')


################# Loading Data into Database Tables using dbWriteTable ################

dbWriteTable(dbcon, "Authors", author, overwrite = T)
dbGetQuery(dbcon, 'select * from Authors')

dbWriteTable(dbcon, "Articles", Articles, overwrite = T)
dbGetQuery(dbcon, 'select * from Articles')

dbWriteTable(dbcon, "Journals", journal, overwrite = T)
dbGetQuery(dbcon, 'select * from Journals')

dbWriteTable(dbcon, "article_Journal_Junction", article_Journal_Junction, overwrite = T)
dbGetQuery(dbcon, 'select * from article_Journal_Junction')

dbWriteTable(dbcon, "author_article_junction", author_article_junction, overwrite = T)
dbGetQuery(dbcon, 'select * from author_article_junction')


# disconnecting database
dbDisconnect(dbcon)



