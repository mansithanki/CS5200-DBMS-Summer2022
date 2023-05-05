# Author 1: Mansi Pravin Thanki (thanki.m@northeastern.edu) 
# Author 2:Hardik Nahata (nahata.h@northeastern.edu)
# Assignment: Practicum II (Mine Database) - Part 2
# Course: CS5200
# Date: 08/12/2022

#rm(list = ls())
library(DBI)
library(RMySQL)
library(sqldf)
con<-dbConnect(MySQL(),host="localhost",port=3306,dbname='pubmed',user="root",password="root12345")

dbfile1 = "pubmed.db"
dbcon1 <- dbConnect(RSQLite::SQLite(), dbfile1)

###################### dropping table if they exist ###################

dbExecute(con,'DROP TABLE IF EXISTS author_facts')
dbExecute(con,'DROP TABLE IF EXISTS journal_facts')

#################### CREATING AUTHOR FACTS TABLE ############################
dbExecute(con,'CREATE TABLE author_facts(
  authorid INT PRIMARY KEY,
  firstName TEXT,
  lastName TEXT,
  number_of_articles INT,
  number_of_co_authors INT 
)
')

################### CREATING JOURNAL FACTS TABLE ############################

dbExecute(con,'CREATE TABLE journal_facts(
  title VARCHAR(200) PRIMARY KEY,
  publication_qtr TEXT,
  publication_month TEXT,
  publication_year TEXT,
  number_of_articles_per_year INT,
  number_of_articles_per_month INT,
  number_of_articles_per_qtr INT
)')


################### Loading data into Author facts dataframe ##########################

author_fact_temp <- dbSendQuery(dbcon1, "SELECT t1.authorid, t3.firstName as firstName, t3.lastName as lastName, count(distinct t1.pmid) as number_of_articles, count(distinct t2.authorid) as number_of_co_authors
FROM author_article_junction AS t1
INNER JOIN author_article_junction AS t2 ON t1.pmid = t2.pmid and t1.authorid <> t2.authorid 
inner join Authors as t3 on  t1.authorid=t3.authorid
group by t1.authorid
")
author_fact_temp_df = fetch(author_fact_temp)
author_fact_temp_df # contains loaded data

########################## Inserting data  into Author facts table #######################

dbSendQuery(con, "SET GLOBAL local_infile = true")
dbWriteTable(con, "author_facts", author_fact_temp_df, overwrite = F,append = T, row.names=F)
dbGetQuery(dbcon1, 'select * from author_facts')

############################################################################################




########################## Loading data into Journal facts dataframe #########################

jfresult_df = dbSendQuery(dbcon1,'select 
j.title,
SUBSTR(aj.publication_year,0,5) publication_year,
aj.publication_month,
case when aj.publication_month in ("Jan","Feb","Mar") then "Q1"
when aj.publication_month in ("Apr","May","Jun") then "Q2"
when aj.publication_month in ("Jul","Aug","Sep") then "Q3"
when aj.publication_month in ("Oct","Nov","Dec") then "Q4" end publication_qtr,
count(aj.pmid) over(partition by j.title, aj.publication_year) number_of_articles_per_year,
count(aj.pmid) over(partition by j.title, aj.publication_month) number_of_articles_per_month,
count(aj.pmid) over(partition by j.title, aj.publication_year ,case when aj.publication_month in ("Jan","Feb","Mar") then "Q1"
when aj.publication_month in ("Apr","May","Jun") then "Q2"
when aj.publication_month in ("Jul","Aug","Sep") then "Q3"
when aj.publication_month in ("Oct","Nov","Dec") then "Q4" end) number_of_articles_per_qtr
from journals j 
join article_Journal_Junction aj 
on j.issn = aj.issn 
')

journalfacts_result_df = fetch(jfresult_df)
journalfacts_result_df # contains the loaded data



############################################################################################


########################## Inserting data  into Journal facts table #######################

dbWriteTable(con, "journal_facts", journalfacts_result_df, overwrite = F,append = T,row.names=F)
dbGetQuery(con, 'select * from journal_facts')

############################################################################################


dbDisconnect(con)
dbDisconnect(dbcon1)
