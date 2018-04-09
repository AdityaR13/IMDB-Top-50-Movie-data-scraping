library(rvest)
# Website
url <- 'http://www.imdb.com/search/title?groups=top_250&sort=user_rating&view=advanced&page=1&ref_=adv_prv'

# Reading the html code into R
webpage <- read_html(url)



#Using CSS selectors to scrap the rankings section
rank_data_html <- html_nodes(webpage,'.text-primary')

#Converting the ranking data to text
rank_data <- html_text(rank_data_html)

#Data-Preprocessing: Converting rankings to numerical
rank_data<-as.numeric(rank_data)

head(rank_data)


#Using CSS selectors to scrap the title section
title_data_html <- html_nodes(webpage,'.lister-item-header a')

#Converting the title data to text
title_data <- html_text(title_data_html)

#Let's have a look at the title
head(title_data)

# Using CSS selectors to scrap the description section
description_data_html <- html_nodes(webpage,'.ratings-bar+ .text-muted')

#Converting the description data to text
description_data <- html_text(description_data_html)

#Data-Preprocessing: removing '\n'
description_data<-gsub("\n","",description_data)

#Let's have a look at the description data
head(description_data)

# Using CSS selectors to scrap the rating
rating_data_html <- html_nodes(webpage,'.ratings-imdb-rating strong')

#Converting the description data to text
rating_data <- html_text(rating_data_html)

#Data-Preprocessing: Converting ratings to numerical
rating_data<-as.numeric(rating_data)

#Let's have a look at the description data
head(rating_data)
length(rating_data)

# Using CSS selectors to scrap the runtime
runtime_data_html <- html_nodes(webpage,'.text-muted .runtime')

#Converting the description data to text
runtime_data <- html_text(runtime_data_html)

#Data-Preprocessing: removing mins
runtime_data<-gsub(" min","",runtime_data)

#Data-Preprocessing: Converting rankings to numerical
runtime_data<-as.numeric(runtime_data)


#Let's have a look at the description data
head(runtime_data)
length(runtime_data)

# Using CSS selectors to scrap the genre
genre_data_html <- html_nodes(webpage,'.genre')

#Converting the genre data to text
genre_data <- html_text(genre_data_html)

#Data-Preprocessing: removing \n
genre_data<-gsub("\n","",genre_data)

#taking only the first genre of each movie
genre_data<-gsub(",.*","",genre_data)

#Convering each genre from text to factor
genre_data<-as.factor(genre_data)

#Let's have a look at the genre data
head(genre_data)
length(genre_data)

#Using CSS selectors to scrap the votes section
votes_data_html <- html_nodes(webpage,'.sort-num_votes-visible span:nth-child(2)')

#Converting the votes data to text
votes_data <- html_text(votes_data_html)

#Let's have a look at the votes data
head(votes_data)

#Data-Preprocessing: removing commas
votes_data<-gsub(",","",votes_data)

#Data-Preprocessing: converting votes to numerical
votes_data<-as.numeric(votes_data)

#Let's have another look at the votes data
head(votes_data)
length(votes_data)


#Using CSS selectors to scrap the directors section
directors_data_html <- html_nodes(webpage, '.text-muted+ p a:nth-child(1)')

#Converting the directors data to text
directors_data <- html_text(directors_data_html)

#Data-Preprocessing: converting directors data into factors
directors_data<-as.factor(directors_data)

#Let's have a look at the directors data
head(directors_data)
length(directors_data)


#Using CSS selectors to scrap the stars section
stars_data_html <- html_nodes(webpage, '.lister-item-content .ghost+ a')

#Converting the stars data to text
stars_data <- html_text(stars_data_html)

#Data-Preprocessing: converting actors data into factors
starts_data<-as.factor(stars_data)

#Let's have a look at the stars data
head(stars_data)
length(stars_data)

# Using CSS selectors to scrap the metascore section
metascore_data_html <- html_nodes(webpage,'.favorable')

#Converting the metascore data to text.
metascore_data <- html_text(metascore_data_html)

#Let's have a look at the metascore data
head(metascore_data)

#Data-Preprocessing: removing extra space in metascore
metascore_data<-gsub(" ","",metascore_data)

#Lets check the length of metascore data
length(metascore_data)

head(metascore_data)

View(metascore_data)

for (i in c(29,30,32,47,48)){
  
  a<-metascore_data[1:(i-1)]
  
  b<-metascore_data[i:length(metascore_data)]
  
  metascore_data<-append(a,list("NA"))
  
  metascore_data<-append(metascore_data,b)
  
}

metascore_data <- unlist(metascore_data, use.names=FALSE)
#Data-Preprocessing: metascore to numerical
metascore_data<-as.numeric(metascore_data)

#Lets check the length of metascore data
length(metascore_data)



# head of metascore
head(metascore_data)


#Using CSS selectors to scrap the gross revenue section
gross_data_html <- html_nodes(webpage,'.ghost~ .text-muted+ span')

#Converting the gross revenue data to text
gross_data <- html_text(gross_data_html)

##Data-Preprocessing: removing '$' and 'M' signs
# gross_data<-substring(gross_data,2,6)     # won't work here because values are in 4,5,6 digit range

gross_data <- gsub("M","",gross_data)

gross_data<-substring(gross_data,2)
View(gross_data)

#Filling missing entries with NA
for (i in c(9)){
  
  a<-gross_data[1:(i-1)]
  
  b<-gross_data[i:length(gross_data)]
  
  gross_data<-append(a,list("NA"))
  
  gross_data<-append(gross_data,b)
  
}

gross_data <- unlist(gross_data, use.names=FALSE)


#Data-Preprocessing: converting gross to numerical
gross_data<-as.numeric(gross_data)

length(gross_data)

#Let's have a look at the votes data
head(gross_data)

#Using CSS selectors to scrap the year section
year_data_html <- html_nodes(webpage, '.text-muted.unbold')

#Converting the stars data to text
year_data <- html_text(year_data_html)

year_data[33] <- "(2017)"
#Data-Preprocessing: removing '()'
year_data<-substring(year_data,2,5)


summary(year_data)
View(year_data)
head(year_data)

#Data-Preprocessing: converting year data into numericals
year_data<-as.numeric(year_data)

#Let's have a look at the year data
head(year_data)


#Combining all the lists to form a data frame
movies_df<-data.frame(Rank = rank_data, Title = title_data,
                      Description = description_data, Runtime = runtime_data,
                      Genre = genre_data, Rating = rating_data,
                      Metascore = metascore_data, Votes = votes_data,
                      Gross_Earning_in_Mil = gross_data,
                     Director = directors_data, Actor = stars_data, Year = year_data)
write.csv(movies_df,file = "IMDB Top 50.csv", row.names=FALSE)


library(lattice)
library(car)
scatterplot(x =movies_df$Gross_Earning_in_Mil, y=movies_df$Votes) 

library('ggplot2')

qplot(data = movies_df,Runtime,fill = Genre,bins = 30)
