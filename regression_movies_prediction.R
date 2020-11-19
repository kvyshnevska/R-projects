library(lubridate)
library(tidyr)
library(dplyr)
library(Amelia)
library(caTools)
library(rworldmap)
library(tidyverse)
library(caret)
library(stringr)
library(DataExplorer)
library(fastDummies)
library(forcats)
library(purrr)
library(scales)
library(patchwork)
library(ggthemes)
library(lattice)
library(xgboost) 
library(e1071)
library(car)
library(doSNOW) 
library(ipred)
library(glmnet)
library(chron)
library(naniar)
library(corrplot)
library(data.table)
library(mice)
library(sjmisc)
library(estimatr)
library(randomForest)
library(Metrics)

###################################################################
#Loading data
###################################################################
movies_train<- read.csv("C:/Users/krist/Desktop/datasets/movies_train.csv",header=TRUE,na.strings=c("","NA","N/A","#N/A","[]",stringsAsFactors = TRUE))
movies_test<- read.csv("C:/Users/krist/Desktop/datasets/movies_test.csv",header=TRUE,na.strings=c("","NA","N/A","#N/A","[]",stringsAsFactors = TRUE))

#combining test and train to deal with missing values, perform feature engineering
movies<- bind_rows(movies_train, movies_test)

#removing homepage,imdb,poster variables, which are URL
movies<-dplyr::select(movies, -imdb_id, -original_title, -overview,-title, -poster_path,-status)

#updating column name
names(movies)[1]<-"id"

#removing outliers after running regression and checking Cook's distance plot
movies <- dplyr::filter(movies, movies$id!= 3)
movies <- dplyr::filter(movies, movies$id!= 16)
movies <- dplyr::filter(movies, movies$id!= 34)
movies <- dplyr::filter(movies, movies$id!= 40)
movies <- dplyr::filter(movies, movies$id!= 59)
movies <- dplyr::filter(movies, movies$id!= 105)
movies <- dplyr::filter(movies, movies$id!= 117)
movies <- dplyr::filter(movies, movies$id!= 150)
movies <- dplyr::filter(movies, movies$id!= 151)
movies <- dplyr::filter(movies, movies$id!= 225)
movies <- dplyr::filter(movies, movies$id!= 243)
movies <- dplyr::filter(movies, movies$id!= 264)
movies <- dplyr::filter(movies, movies$id!= 270)
movies <- dplyr::filter(movies, movies$id!= 281)
movies <- dplyr::filter(movies, movies$id!= 335)
movies <- dplyr::filter(movies, movies$id!= 348)
movies <- dplyr::filter(movies, movies$id!= 388)
movies <- dplyr::filter(movies, movies$id!= 391)
movies <- dplyr::filter(movies, movies$id!= 461)
movies <- dplyr::filter(movies, movies$id!= 470) 
movies <- dplyr::filter(movies, movies$id!= 449)
movies <- dplyr::filter(movies, movies$id!= 513)
movies <- dplyr::filter(movies, movies$id!= 519)
movies <- dplyr::filter(movies, movies$id!= 580)
movies <- dplyr::filter(movies, movies$id!= 640)
movies <- dplyr::filter(movies, movies$id!= 645)
movies <- dplyr::filter(movies, movies$id!= 665)
movies <- dplyr::filter(movies, movies$id!= 666)
movies <- dplyr::filter(movies, movies$id!= 685)

movies <- dplyr::filter(movies, movies$id!= 696)
movies <- dplyr::filter(movies, movies$id!= 797)
movies <- dplyr::filter(movies, movies$id!= 818)
movies <- dplyr::filter(movies, movies$id!= 828)
movies <- dplyr::filter(movies, movies$id!= 844)
movies <- dplyr::filter(movies, movies$id!= 850)
movies <- dplyr::filter(movies, movies$id!= 887)
movies <- dplyr::filter(movies, movies$id!= 911)
movies <- dplyr::filter(movies, movies$id!= 929)
movies <- dplyr::filter(movies, movies$id!= 1008)
movies <- dplyr::filter(movies, movies$id!= 1021)
movies <- dplyr::filter(movies, movies$id!= 1059)
movies <- dplyr::filter(movies, movies$id!= 1139)
movies <- dplyr::filter(movies, movies$id!= 1142)
movies <- dplyr::filter(movies, movies$id!= 1162)
movies <- dplyr::filter(movies, movies$id!= 1188)
movies <- dplyr::filter(movies, movies$id!= 1191)
movies <- dplyr::filter(movies, movies$id!= 1199)
movies <- dplyr::filter(movies, movies$id!= 1212)
movies <- dplyr::filter(movies, movies$id!= 1231)
movies <- dplyr::filter(movies, movies$id!= 1241)
movies <- dplyr::filter(movies, movies$id!= 1255)
movies <- dplyr::filter(movies, movies$id!= 1256)
movies <- dplyr::filter(movies, movies$id!= 1263)
movies <- dplyr::filter(movies, movies$id!= 1310)
movies <- dplyr::filter(movies, movies$id!= 1347)
movies <- dplyr::filter(movies, movies$id!= 1355)
movies <- dplyr::filter(movies, movies$id!= 1472)
movies <- dplyr::filter(movies, movies$id!= 1480)
movies <- dplyr::filter(movies, movies$id!= 1484)
movies <- dplyr::filter(movies, movies$id!= 1542)
movies <- dplyr::filter(movies, movies$id!= 1582)

movies <- dplyr::filter(movies, movies$id!= 1670)
movies <- dplyr::filter(movies, movies$id!= 1691)
movies <- dplyr::filter(movies, movies$id!= 1696)
movies <- dplyr::filter(movies, movies$id!= 1755)
movies <- dplyr::filter(movies, movies$id!= 1756)
movies <- dplyr::filter(movies, movies$id!= 1763)
movies <- dplyr::filter(movies, movies$id!= 1772)
movies <- dplyr::filter(movies, movies$id!= 1801)
movies <- dplyr::filter(movies, movies$id!= 1804)
movies <- dplyr::filter(movies, movies$id!= 1875)
movies <- dplyr::filter(movies, movies$id!= 1918)
movies <- dplyr::filter(movies, movies$id!= 1924)
movies <- dplyr::filter(movies, movies$id!= 1949)
movies <- dplyr::filter(movies, movies$id!= 1965)
movies <- dplyr::filter(movies, movies$id!= 2011)
movies <- dplyr::filter(movies, movies$id!= 2033)

movies <- dplyr::filter(movies, movies$id!= 2118)
movies <- dplyr::filter(movies, movies$id!= 2127)
movies <- dplyr::filter(movies, movies$id!= 2151)
movies <- dplyr::filter(movies, movies$id!= 2175)
movies <- dplyr::filter(movies, movies$id!= 2202)
movies <- dplyr::filter(movies, movies$id!= 2238)
movies <- dplyr::filter(movies, movies$id!= 2252)
movies <- dplyr::filter(movies, movies$id!= 2264)
movies <- dplyr::filter(movies, movies$id!= 2311)
movies <- dplyr::filter(movies, movies$id!= 2314)
movies <- dplyr::filter(movies, movies$id!= 2324)
movies <- dplyr::filter(movies, movies$id!= 2327)
movies <- dplyr::filter(movies, movies$id!= 2339)
movies <- dplyr::filter(movies, movies$id!= 2343)
movies <- dplyr::filter(movies, movies$id!= 2369)
movies <- dplyr::filter(movies, movies$id!= 2384)
movies <- dplyr::filter(movies, movies$id!= 2385)
movies <- dplyr::filter(movies, movies$id!= 2400)
movies <- dplyr::filter(movies, movies$id!= 1842)


#converting Release date to date format
movies[is.na(movies$release_date),]$release_date <- "01/01/00"
movies$release_date<-as.Date(chron(format(as.Date(movies$release_date, "%m/%d/%y"), "%m/%d/%y")))
movies$release_year<-year(movies$release_date)
movies$relase_month<-month(movies$release_date)

#checking missing data per variable
missing_data_summary_table <- as.data.frame(miss_var_summary(movies))
missing_data_summary_table%>%filter(pct_miss>0)


#converting text in columns to 1/0 to indicate the presence of data for modeling
movies$Keywords<-ifelse(is.na(movies$Keywords),0,1)
movies$belongs_to_collection<-ifelse(is.na(movies$belongs_to_collection),0,1)
movies$tagline<-ifelse(is.na(movies$tagline),0,1)
movies$homepage<-ifelse(is.na(movies$homepage),0,1)

#exploring and transforming response variable
ggplot(movies,aes(revenue))+geom_histogram(fill="steel blue")+
  ggtitle("Distribution of revenue")+
  theme_classic()

ggplot(movies,aes(log(revenue)))+geom_histogram(fill="steel blue")+
  ggtitle("Distribution of Log Revenue")+
  theme_classic()

#movies that belong to collection have higher revenue
ggplot(movies, aes(x = factor(belongs_to_collection), y = log(revenue))) + 
  geom_jitter(alpha = .2, color = 'steelblue') +
  geom_boxplot(alpha = .1) +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(x = 'Belongs to a Collection', y = 'Log(Revenue)') +
  ggtitle("Log(Revenue) vs belongs to collection")
theme_classic()

#movies that have Keywords to collection have higher revenue 
ggplot(movies, aes(x = factor(Keywords), y = log(revenue))) + 
  geom_jitter(alpha = .2, color = 'steelblue') +
  geom_boxplot(alpha = .1) +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(x = 'Keywords', y = 'Log(Revenue)') +
  ggtitle("Log(Revenue) vs Keywords")
theme_classic() 

ggplot(movies, aes(x = factor(tagline), y = log(revenue))) + 
  geom_jitter(alpha = .2, color = 'steelblue') +
  geom_boxplot(alpha = .1) +
  scale_x_discrete(labels = c("No", "Yes")) +
  labs(x = 'Keywords', y = 'Log(Revenue)') +
  ggtitle("Log(Revenue) vs Keywords")
theme_classic() 

#correlation exploration
#grabbing only numeric columns
num.cols<-sapply(movies,is.numeric)

#filter all numeric columns for correlation
cor.data<-cor(movies[,num.cols],use="pairwise.complete.obs")
cor.data

#visualizing correlation between numeric variables. only numeric columns can be used!
corrplot(cor.data,method="color")

#working with production countries
n = 0
for (i in movies$production_countries) {
  n = n + 1
  movies$production_countries[n] <- str_match_all(i, "'name': '(.*?)'")
  movies$production_countries[[n]] <- movies$production_countries[[n]][,2]
}

Asia <- c('Afghanistan','Bahrain','Bangladesh','Bhutan','Brunei','Cambodia','China','East Timor','Hong Kong S.A.R.',
          'India','Indonesia','Iran','Iraq','Japan','Jordan','Kazakhstan','Korea No Mans Area','Kuwait','Kyrgyzstan',
          'Laos','Lebanon','Macau S.A.R','Malaysia','Maldives','Mongolia','Myanmar','Nepal','North Korea','Oman',
          'Pakistan','Philippines','Qatar','Saudi Arabia','Siachen Glacier','Singapore','South Korea','Sri Lanka',
          'Syria','Taiwan','Tajikistan','Thailand','Turkmenistan','United Arab Emirates','Uzbekistan','Vietnam','Yemen')

North.America <- c('Canada','United States of America','Saint Pierre and Miquelon',
                   'United States Minor Outlying Islands','US Naval Base Guantanamo Bay')

Europe <- c('Akrotiri Sovereign Base Area','Aland','Albania','Andorra','Armenia','Austria',
            'Azerbaijan','Baykonur Cosmodrome','Belarus','Belgium','Bosnia and Herzegovina',
            'British Indian Ocean Territory','Bulgaria','Croatia','Cyprus','Cyprus No Mans Area',
            'Czech Republic','Denmark','Dhekelia Sovereign Base Area','Estonia','Faroe Islands',
            'Finland','France','Gaza','Georgia','Germany','Gibraltar','Greece','Greenland','Guernsey',
            'Hungary','Iceland','Ireland','Isle of Man','Israel','Italy','Jersey','Kosovo','Latvia',
            'Liechtenstein','Lithuania','Luxembourg','Macedonia','Malta','Moldova','Monaco','Montenegro','Netherlands',
            'Northern Cyprus','Norway','Poland','Portugal','Republic of Serbia','Romania','Russia','Saint Barthelemy',
            'Saint Martin','San Marino','Sint Maarten','Slovakia','Slovenia','South Georgia and South Sandwich Islands',
            'Spain','Sweden','Switzerland','Turkey','Ukraine','United Kingdom','Vatican','West Bank')

Latin.and.South.America <- c('Anguilla','Antigua and Barbuda','Argentina','Aruba','Barbados',
                             'Belize','Bermuda','Bolivia','Brazil','British Virgin Islands','Cayman Islands','Chile',
                             'Colombia','Costa Rica','Cuba','Curacao','Dominica','Dominican Republic','Ecuador','El Salvador',
                             'Falkland Islands','Grenada','Guatemala','Guyana','Haiti','Honduras','Jamaica','Mexico','Montserrat',
                             'Nicaragua','Panama','Paraguay','Peru','Puerto Rico','Saint Kitts and Nevis','Saint Lucia',
                             'Saint Vincent and the Grenadines','Suriname','The Bahamas','Trinidad and Tobago','Turks and Caicos Islands',
                             'United States Virgin Islands','Uruguay','Venezuela','French Guiana')

Africa<-c('Algeria','Angola','Benin','Botswana','Burkina Faso','Burundi','Cameroon','Cape Verde',
          'Central African Republic','Chad','Comoros','Democratic Republic of the Congo','Djibouti',
          'Egypt','Equatorial Guinea','Eritrea','Ethiopia','Gabon','Gambia','Ghana','Guinea','Guinea Bissau',
          'Ivory Coast','Kenya','Lesotho','Liberia','Libya','Madagascar','Malawi','Mali','Mauritania',
          'Mauritius','Morocco','Mozambique','Namibia','Niger','Nigeria','Republic of the Congo','Rwanda',
          'Saint Helena','Sao Tome and Principe','Senegal','Seychelles','Sierra Leone','Somalia','Somaliland',
          'South Africa','South Sudan','Sudan','Swaziland','Togo','Tunisia','Uganda','United Republic of Tanzania',
          'Western Sahara','Zambia','Zimbabwe')

Oceania<-c('American Samoa','Ashmore and Cartier Islands','Australia','Cook Islands','Coral Sea Islands',
           'Federated States of Micronesia','Fiji','French Polynesia','Guam','Indian Ocean Territories',
           'Kiribati','Marshall Islands','Nauru','New Caledonia','New Zealand','Niue','Norfolk Island',
           'Northern Mariana Islands','Palau','Papua New Guinea','Pitcairn Islands','Samoa',
           'Solomon Islands','Tonga','Tuvalu','Vanuatu','Wallis and Futuna')

country_group<-function(country) {
  
  if(country %in% Asia) {
    return("Asia")
  }else if (country %in% North.America) {
    return ("North America")
  } else if (country %in% Europe) {
    return ("Europe")
  } else if (country %in% Latin.and.South.America) {
    return("Latin America")
  } else if(country %in% Oceania) {
    return ("Oceania")
  }else if(country %in% Africa) {
    return ("Africa")
  }else {
    return("Other")
  }
}
movies$production_countries<-sapply(movies$production_countries,country_group)

#renaming column
setnames(movies, "production_countries", "production_region")

#revenue by region
ggplot(movies,aes(y=log(revenue),x=reorder(production_region,log(revenue),na.rm=TRUE),col=production_region))+
  geom_boxplot(alpha=0.4)+
  labs(x = 'Regions', y = 'Log(Revenue)')+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("Log(Revenue) by region")


#I will apply the same approach to genres as to production_countries

n=0
for (i in movies$genres) {
  n = n + 1
  movies$genres[n] <- str_match_all(i, "'name': '(.*?)'")
  movies$genres[[n]] <- movies$genres[[n]][,2]
}

Action<-c("Action")
Adventure<-c("Adventure")
Animation<-c("Animation")
Comedy<-c("Comedy")
Crime<-c("Crime")
Documentary<-c("Documentary")
Drama<-c("Drama")
Family<-c("Family")
Fantasy<-c("Fantasy")
Foreign<-c("Foreign")
History<-c("History")
Horror<-c("Horror")
Music<-c("Music")
Mystery<-c("Mystery")
Romance<-c("Romance")
Science_Fiction<-c("Science Fiction")
Thriller<-c("Thriller")
War<-c("War")
Western<-c("Western")

genre_group<-function(genre) {
  
  if(genre %in% Action) {
    return("Action")
  }else if (genre %in% Adventure) {
    return ("Adventure")
  } else if (genre %in% Animation) {
    return ("Animation")
  } else if (genre %in% Comedy) {
    return("Comedy")
  } else if(genre %in% Documentary) {
    return ("Documentary")
  }else if(genre %in% Drama) {
    return ("Drama")
  }else if(genre %in% Family) {
    return ("Family")
  }else if(genre %in% Fantasy) {
    return ("Fantasy")
  }else if(genre %in% Foreign) {
    return ("Foreign")  
  }else if(genre %in% History) {
    return ("History")
  }else if(genre %in% Horror) {
    return ("Horror")
  }else if(genre %in% Music) {
    return ("Music") 
  }else if(genre %in% Mystery) {
    return ("Mystery") 
  }else if(genre %in% Romance) {
    return ("Romance")   
  }else if(genre %in% Science_Fiction) {
    return ("Science Fiction") 
  }else if(genre %in% Thriller) {
    return ("Thriller")
  }else if(genre %in% War) {
    return ("War") 
  }else if(genre %in% Western) {
    return ("Western")  
  }else {
    return("Other")
  }
}

movies$genres<-sapply(movies$genres,genre_group)

#removing release date 
movies<- dplyr::select(movies,-release_date)

#density plot by genre
densityplot(~log(budget)|genres, data=movies, color='black', group=genres)

#high level view on revenue by genre
GenresRevenue<-movies %>% group_by(genres) %>% dplyr::summarise(TotalRev = sum(as.numeric(revenue), na.rm = TRUE)) 
GenresRevenue %>% arrange(desc(TotalRev))
ggplot(GenresRevenue, aes(genres,TotalRev, size=.9)) + geom_jitter()+ facet_wrap(~genres)+
  ggtitle("Revenue by genre")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

ggplot(movies,aes(y=log(revenue),x=genres,color=factor(genres)))+geom_jitter(alpha=0.4)+
  ggtitle("Revenue by genre")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


#working with original languages
#85% of training dataset has EN as original lang so I'll introduce binary variable to handle it
original_language_unique <- count(movies, vars = "original_language")
movies$original_language<-ifelse(movies$original_language=="en",1,0)

#working with spoken languages
n = 0
for (i in movies$spoken_languages) {
  n = n + 1
  movies$spoken_languages[n] <- str_match_all(i, "'iso_639_1': '(.*?)'")
  movies$spoken_languages[[n]] <- movies$spoken_languages[[n]][,2]
}
spokenlang_list <- foreach(x = seq_along(movies$spoken_languages),.combine = rbind, .multicombine=TRUE,
                           .inorder=FALSE) %do%{
                             newdataframe <- setNames(do.call(data.frame, movies$spoken_languages[x]), "spokenlang")
                           }

#looked at frequency distribution and selected the most frequent languages
spokenlang_list_unique <- count(spokenlang_list, vars = "spokenlang")
spokenlang_list_unique

movies$spoken_languages <- foreach(x = seq_along(movies$spoken_languages)) %do% {
  ifelse("en" %in% movies$spoken_languages[[x]], movies$spoken_languages[x] <- 1, 
         ifelse("fr" %in% movies$spoken_languages[[x]], movies$spoken_languages[x] <- 1,
                ifelse("de" %in% movies$spoken_languages[[x]], movies$spoken_languages[x] <- 1,
                       ifelse("es" %in% movies$spoken_languages[[x]], movies$spoken_languages[x] <- 1,
                              ifelse("it" %in% movies$spoken_languages[[x]], movies$spoken_languages[x] <- 1,
                                     ifelse("ru" %in% movies$spoken_languages[[x]], movies$spoken_languages[x] <- 1,
                                            ifelse("ja" %in% movies$spoken_languages[[x]], movies$spoken_languages[x] <- 1,
                                                   ifelse("ar" %in% movies$spoken_languages[[x]], movies$spoken_languages[x] <- 1,
                                                          ifelse("zh" %in% movies$spoken_languages[[x]], movies$spoken_languages[x] <- 1,
                                                                 movies$spoken_languages[x] <- 0)))))))))
}

any(is.na(movies$spoken_languages))


#creating cast and crew count variables
movies$cast_count <- str_count(movies$cast, "'name':\\s'")
movies$crew_count <- str_count(movies$crew, "'name':\\s'")

movies<-dplyr::select(movies,-crew,-cast)

#missing values are substituted with median
movies[is.na(movies$crew_count),]$crew_count <- median(movies$crew_count, na.rm = TRUE)
movies[is.na(movies$cast_count),]$cast_count <- median(movies$cast_count, na.rm = TRUE)

ggplot(movies,aes(y=log(revenue),x=crew_count))+geom_jitter(col="steel blue",alpha=0.4)+
  ggtitle("Log revenue by total crew")


#checking missing data per variable
missing_data_summary_table <- as.data.frame(miss_var_summary(movies))
missing_data_summary_table%>%filter(pct_miss>0)

#substituting missing value with median
movies$runtime<-tidyr::replace_na(movies$runtime,median(movies$runtime,na.rm=TRUE))
any(is.na(movies$runtime))


#working with production companies
movies$prodcomp_count <- str_count(movies$production_companies, pattern = "\\'name\\'")

# replace NA number by median
movies$prodcomp_count[is.na(movies$prodcomp_count)] <- median(movies$prodcomp_count, na.rm = TRUE)

n = 0
for (i in movies$production_companies) {
  n = n + 1
  movies$production_companies[n] <- str_match_all(i, "'name': '(.*?)'")
  movies$production_companies[[n]] <- movies$production_companies[[n]][,2]
}

prodcomp_list <- foreach(x = seq_along(movies$production_companies),.combine = rbind, .multicombine=TRUE,
                         .inorder=FALSE) %do%{
                           newdataframe <- setNames(do.call(data.frame, movies$production_companies[x]), "prodcompany")
                         }


prodcomp_list_unique <- count(prodcomp_list, vars = "prodcompany")
prodcomp_list_unique$percentage<-(prodcomp_list_unique$freq/sum(prodcomp_list_unique$freq))*100
#sorting by percentage
prodcomp_list_unique<-prodcomp_list_unique[order(prodcomp_list_unique$percentage, decreasing = TRUE),] 

movies$production_companies <- foreach(x = seq_along(movies$production_companies)) %do% {
  ifelse("Warner Bros." %in% movies$production_companies[[x]], movies$production_companies[x] <- 1,
         ifelse("Universal Pictures" %in% movies$production_companies[[x]], movies$production_companies[x] <- 1,
                ifelse("Paramount Pictures" %in% movies$production_companies[[x]], movies$production_companies[x] <- 1,
                       ifelse("Twentieth Century Fox Film Corporation" %in% movies$production_companies[[x]], movies$production_companies[x] <- 1,
                              ifelse("Columbia Pictures" %in% movies$production_companies[[x]], movies$production_companies[x] <- 1,
                                     ifelse("Metro-Goldwyn-Mayer (MGM)" %in% movies$production_companies[[x]], movies$production_companies[x] <- 1,
                                            ifelse("New Line Cinema" %in% movies$production_companies[[x]], movies$production_companies[x] <- 1,
                                                   ifelse("Walt Disney Pictures" %in% movies$production_companies[[x]], movies$production_companies[x] <- 1,
                                                          movies$production_companies[x] <- 0 ))))))))
}

any(is.na(movies$production_companies))
movies$production_companies<-unlist(movies$production_companies)


ggplot(movies,aes(y=log(revenue),x=factor(production_companies)))+geom_boxplot()+
  geom_jitter(alpha = .2, color = 'steelblue')+
  geom_boxplot(alpha = .1)+
  scale_x_discrete(labels = c("Other", "Major players"))+
  labs(x = 'Production', y = 'Log(Revenue)') +
  ggtitle("Log(Revenue) by production company")+
  theme_classic() 


densityplot(as.numeric(movies$runtime))

movies_clean<-movies

#converting 0 budget to NA
movies_clean$budget_missing<-ifelse(movies_clean$budget==0,0,1)
movies_clean$budget<-ifelse(movies_clean$budget==0,NA,movies_clean$budget)

missing_data_summary_table <- as.data.frame(miss_var_summary(movies_clean))
missing_data_summary_table%>%filter(pct_miss>0)


movies_clean$spoken_languages<-unlist(movies_clean$spoken_languages)
movies_clean$belongs_to_collection<-unlist(movies_clean$belongs_to_collection)
movies_clean$genres<-unlist(movies_clean$genres)
movies_clean$budget<-as.numeric(movies_clean$budget)

#setting up NA revenue to 0 to avoid imputation
movies_clean$revenue<-ifelse(is.na(movies_clean$revenue),0,movies_clean$revenue)


imputed_data <- mice(movies_clean, m = 10, maxit = 25, method = "cart")

df<-merge_imputations(movies_clean,imputed_data)

movies4<-cbind(movies_clean,df)

for (i in colnames(df)) {
  movies4[[i]] <- df[[i]]
}

#dropping last column: budget duplicate
movies4<-movies4[1:(length(movies4)-1)]

#no missing data left
missing_data_summary_table <- as.data.frame(miss_var_summary(movies4))
missing_data_summary_table%>%filter(pct_miss>0)


#########################################################
#exploring data after cleaning and feature engineering
########################################################
hist(movies4$revenue)
hist(log(movies4$revenue))

#movies that belong to collection have higher median revenue but we have to keep in mind
#that 79% of data was missing for this variable
ggplot(movies4,aes(as.factor(belongs_to_collection),log(revenue)))+geom_boxplot(alpha=0.5,col="steel blue")+
  ggtitle("Belong to collection vs log_revenue")

#we can see that the most profitable genres are Action,Adventure,Comedy,Documentary
ggplot(movies4,aes(y=log(revenue),x=log(budget),colour=genres))+geom_jitter(alpha=0.5)

boxplot(movies4$revenue)$out
ggplot(movies4,aes(log(revenue)))+geom_histogram(fill="steel blue")+
  theme_classic()+ggtitle("Log revenue distribution")#longer right tail but it's in scope outliers --> don't remove

#removing observations with 1000000 to make it closer to normal distribution
movies5<-filter(movies4,revenue==0|revenue>100000)

ggplot(movies5,aes(log(revenue)))+geom_histogram()

ggplot(movies5,aes(log(revenue)))+geom_histogram()#longer right tail but it's in scope outliers --> don't remove

movies5$spoken_languages<-unlist(movies5$spoken_languages)
movies5$belongs_to_collection<-unlist(movies5$belongs_to_collection)
movies5$homepage<-unlist(movies5$homepage)

###########################################################
#Modeling
###########################################################

#Train and test split
#80% of data -->train from sample
data_train<-subset(movies5, id<=1940 & movies5$revenue!=0)

#20% of data -->test from sample
data_test<-subset(movies5,id>=1941 & movies5$revenue!=0)

data_prediction<-subset(movies5,revenue==0)

#1
mod_all<-lm(log(revenue)~.,data_train)  
summary(mod_all)
prediction<-exp(predict(mod_all,data_test))
percent.errors.log <- abs((data_test$revenue-prediction)/data_test$revenue)*100
mean(percent.errors.log) 

#2
mod<-lm(log(revenue)~log(budget)+original_language+popularity+production_region+relase_month+release_year:budget+
          budget:popularity+release_year:popularity+relase_month:popularity+popularity*runtime+
          Keywords:tagline+runtime+spoken_languages+production_companies+
          production_companies:log(budget)+Keywords+
          belongs_to_collection+tagline+homepage+production_region+cast_count+
          crew_count+genres+prodcomp_count*log(budget)+prodcomp_count+budget_missing,data_train)

summary(mod)
vif(mod)

res<-residuals(mod)
plot(res)
plot(density(resid(mod)))

par(mfrow=c(1,4))
plot(mod)

cooksd <- cooks.distance(mod)
influential <- as.numeric(names(cooksd)[(cooksd > 4*mean(cooksd, na.rm=T))])
plot(cooksd, pch="*", cex=2)
abline(h = 4*mean(cooksd, na.rm=T), col="red") 
text(x=1:length(cooksd)+1, y=cooksd, labels=ifelse(cooksd>4*mean(cooksd, na.rm=T),names(cooksd),""), col="red")
influential

prediction<-exp(predict(mod,data_test))
percent.errors.log <- abs((data_test$revenue-prediction)/data_test$revenue)*100
mean(percent.errors.log)

#checking for heteroskedasticity
#H0 is that there is no heteroskedasticity
ncvTest(mod) #reject H0 --> our data is heteroskedastic

mod_robust<-lm_robust(log(revenue)~log(budget)+original_language+popularity+production_region+relase_month+release_year:budget+
                        budget:popularity+release_year:popularity+relase_month:popularity+popularity*runtime+
                        Keywords:tagline+runtime+spoken_languages+production_companies+
                        production_companies:log(budget)+Keywords+
                        belongs_to_collection+tagline+homepage+production_region+cast_count+
                        crew_count+genres+prodcomp_count*log(budget)+prodcomp_count+budget_missing,data_train,se_type="HC2")
summary(mod_robust)

prediction<-exp(predict(mod_robust,data_test))
percent.errors.log <- abs((data_test$revenue-prediction)/data_test$revenue)*100
mean(percent.errors.log) 

#3              
randomForest_model<-randomForest(log(revenue)~log(budget)+original_language+popularity+production_region+relase_month+release_year:budget+
                                   budget:popularity+release_year:popularity+relase_month:popularity+popularity*runtime+
                                   Keywords:tagline+runtime+spoken_languages+production_companies+
                                   production_companies:log(budget)+Keywords+
                                   belongs_to_collection+tagline+homepage+production_region+cast_count+
                                   crew_count+genres+prodcomp_count*log(budget)+prodcomp_count+budget_missing,importance=TRUE,data=data_train)

randomforest.prediction<-exp(predict(randomForest_model,data_test))
percent.errors.log <- abs((data_test$revenue-randomforest.prediction)/data_test$revenue)*100
mean(percent.errors.log)


####################################################
###LASSO
####################################################
y<-log(data_train$revenue)
X<-model.matrix(id~log(budget)+original_language+popularity+production_region+relase_month+
                  release_year:budget+budget:popularity+release_year:popularity+
                  relase_month:popularity+popularity*runtime+
                  Keywords:tagline+runtime+spoken_languages+production_companies+
                  production_companies:log(budget)+Keywords+
                  belongs_to_collection+tagline+homepage+production_region+cast_count+
                  crew_count+genres+budget^2+crew_count+genres+cast_count:popularity+
                  cast_count:log(budget)+prodcomp_count*log(budget)+prodcomp_count+budget_missing,movies5)[,-1]

X<-cbind(movies5$id,X)

X.training<-subset(X,X[,1]<=1940)
X.testing<-subset(X,(X[,1]>=1941 & X[,1]<=3000))
X.prediction<-subset(X,X[,1]>=3001)


#LASSO (alpha=1)
lasso.fit<-glmnet(x = X.training, y = y, alpha = 1)
plot(lasso.fit, xvar = "lambda")

crossval <-  cv.glmnet(x = X.training, y = y, alpha = 1) #create cross-validation data
plot(crossval)

penalty.lasso <- crossval$lambda.min #determine optimal penalty parameter, lambda
lasso.opt.fit <-glmnet(x = X.training, y = y, alpha = 1, lambda = penalty.lasso) #estimate the model with the optimal penalty
coef(lasso.opt.fit) #resultant model coefficients

# predicting the performance on the testing set
lasso.testing <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.testing))
mean(abs(lasso.testing-data_test$revenue)/data_test$revenue*100) #calculate and display MAPE


#testing on a data set that we know values of
results<-cbind(lasso.testing,data_test$revenue)
colnames(results)<-c("predicted","actual")
results<-as.data.frame(results)



rmsle(results$predicted,results$actual)
rmsle <- function(y_true, y_pred)
  sqrt(mean((log1p(results$actual) - log1p(results$predicted))^2))


#lasso prediction
predicted.prices.log.i.lasso <- exp(predict(lasso.opt.fit, s = penalty.lasso, newx =X.prediction))
lasso<-as.data.frame(cbind(X.prediction[,1],predicted.prices.log.i.lasso))
setnames(lasso, "V1", "id")
setnames(lasso,"1","revenue")


##############################################################
#Much better result in Kaggle competition was achieved using Caret package. Code below

#Caret package
##############################################################
movies_train<- read.csv("C:/Users/krist/Desktop/datasets/movies_train.csv",header=TRUE,na.strings=c("","NA","N/A","#N/A","[]",stringsAsFactors = TRUE))
movies_test<- read.csv("C:/Users/krist/Desktop/datasets/movies_test.csv",header=TRUE,na.strings=c("","NA","N/A","#N/A","[]",stringsAsFactors = TRUE))
#combining test and train to deal with missing values
movies<- bind_rows(movies_train, movies_test)

#removing homepage,imdb,poster variables, which are URL
movies<-dplyr::select(movies, -imdb_id, -original_title, -overview,-title, -poster_path,-status)

#updating column name
names(movies)[1]<-"id"


#converting Release date to date format
movies[is.na(movies$release_date),]$release_date <- "01/01/00"
movies$release_date<-as.Date(chron(format(as.Date(movies$release_date, "%m/%d/%y"), "%m/%d/%y")))
movies$release_year<-year(movies$release_date)
movies$relase_month<-month(movies$release_date)


#checking missing data per variable
missing_data_summary_table <- as.data.frame(miss_var_summary(movies))
missing_data_summary_table%>%filter(pct_miss>0)


#converting text in columns to 1/0 to indicate the presence of data for modeling
movies$Keywords<-ifelse(is.na(movies$Keywords),0,1)
movies$belongs_to_collection<-ifelse(is.na(movies$belongs_to_collection),0,1)
movies$tagline<-ifelse(is.na(movies$tagline),0,1)
movies$homepage<-ifelse(is.na(movies$homepage),0,1)


#working with production countries

n = 0
for (i in movies$production_countries) {
  n = n + 1
  movies$production_countries[n] <- str_match_all(i, "'name': '(.*?)'")
  movies$production_countries[[n]] <- movies$production_countries[[n]][,2]
}

Asia <- c('Afghanistan','Bahrain','Bangladesh','Bhutan','Brunei','Cambodia','China','East Timor','Hong Kong S.A.R.',
          'India','Indonesia','Iran','Iraq','Japan','Jordan','Kazakhstan','Korea No Mans Area','Kuwait','Kyrgyzstan',
          'Laos','Lebanon','Macau S.A.R','Malaysia','Maldives','Mongolia','Myanmar','Nepal','North Korea','Oman',
          'Pakistan','Philippines','Qatar','Saudi Arabia','Siachen Glacier','Singapore','South Korea','Sri Lanka',
          'Syria','Taiwan','Tajikistan','Thailand','Turkmenistan','United Arab Emirates','Uzbekistan','Vietnam','Yemen')

North.America <- c('Canada','United States of America','Saint Pierre and Miquelon',
                   'United States Minor Outlying Islands','US Naval Base Guantanamo Bay')

Europe <- c('Akrotiri Sovereign Base Area','Aland','Albania','Andorra','Armenia','Austria',
            'Azerbaijan','Baykonur Cosmodrome','Belarus','Belgium','Bosnia and Herzegovina',
            'British Indian Ocean Territory','Bulgaria','Croatia','Cyprus','Cyprus No Mans Area',
            'Czech Republic','Denmark','Dhekelia Sovereign Base Area','Estonia','Faroe Islands',
            'Finland','France','Gaza','Georgia','Germany','Gibraltar','Greece','Greenland','Guernsey',
            'Hungary','Iceland','Ireland','Isle of Man','Israel','Italy','Jersey','Kosovo','Latvia',
            'Liechtenstein','Lithuania','Luxembourg','Macedonia','Malta','Moldova','Monaco','Montenegro','Netherlands',
            'Northern Cyprus','Norway','Poland','Portugal','Republic of Serbia','Romania','Russia','Saint Barthelemy',
            'Saint Martin','San Marino','Sint Maarten','Slovakia','Slovenia','South Georgia and South Sandwich Islands',
            'Spain','Sweden','Switzerland','Turkey','Ukraine','United Kingdom','Vatican','West Bank')

Latin.and.South.America <- c('Anguilla','Antigua and Barbuda','Argentina','Aruba','Barbados',
                             'Belize','Bermuda','Bolivia','Brazil','British Virgin Islands','Cayman Islands','Chile',
                             'Colombia','Costa Rica','Cuba','Curacao','Dominica','Dominican Republic','Ecuador','El Salvador',
                             'Falkland Islands','Grenada','Guatemala','Guyana','Haiti','Honduras','Jamaica','Mexico','Montserrat',
                             'Nicaragua','Panama','Paraguay','Peru','Puerto Rico','Saint Kitts and Nevis','Saint Lucia',
                             'Saint Vincent and the Grenadines','Suriname','The Bahamas','Trinidad and Tobago','Turks and Caicos Islands',
                             'United States Virgin Islands','Uruguay','Venezuela','French Guiana')

Africa<-c('Algeria','Angola','Benin','Botswana','Burkina Faso','Burundi','Cameroon','Cape Verde',
          'Central African Republic','Chad','Comoros','Democratic Republic of the Congo','Djibouti',
          'Egypt','Equatorial Guinea','Eritrea','Ethiopia','Gabon','Gambia','Ghana','Guinea','Guinea Bissau',
          'Ivory Coast','Kenya','Lesotho','Liberia','Libya','Madagascar','Malawi','Mali','Mauritania',
          'Mauritius','Morocco','Mozambique','Namibia','Niger','Nigeria','Republic of the Congo','Rwanda',
          'Saint Helena','Sao Tome and Principe','Senegal','Seychelles','Sierra Leone','Somalia','Somaliland',
          'South Africa','South Sudan','Sudan','Swaziland','Togo','Tunisia','Uganda','United Republic of Tanzania',
          'Western Sahara','Zambia','Zimbabwe')

Oceania<-c('American Samoa','Ashmore and Cartier Islands','Australia','Cook Islands','Coral Sea Islands',
           'Federated States of Micronesia','Fiji','French Polynesia','Guam','Indian Ocean Territories',
           'Kiribati','Marshall Islands','Nauru','New Caledonia','New Zealand','Niue','Norfolk Island',
           'Northern Mariana Islands','Palau','Papua New Guinea','Pitcairn Islands','Samoa',
           'Solomon Islands','Tonga','Tuvalu','Vanuatu','Wallis and Futuna')

country_group<-function(country) {
  
  if(country %in% Asia) {
    return("Asia")
  }else if (country %in% North.America) {
    return ("North America")
  } else if (country %in% Europe) {
    return ("Europe")
  } else if (country %in% Latin.and.South.America) {
    return("Latin America")
  } else if(country %in% Oceania) {
    return ("Oceania")
  }else if(country %in% Africa) {
    return ("Africa")
  }else {
    return("Other")
  }
}
movies$production_countries<-sapply(movies$production_countries,country_group)

#renaming column
setnames(movies, "production_countries", "production_region")


#I will apply the same approach to genres as to production_countries 
n=0
for (i in movies$genres) {
  n = n + 1
  movies$genres[n] <- str_match_all(i, "'name': '(.*?)'")
  movies$genres[[n]] <- movies$genres[[n]][,2]
}

Action<-c("Action")
Adventure<-c("Adventure")
Animation<-c("Animation")
Comedy<-c("Comedy")
Crime<-c("Crime")
Documentary<-c("Documentary")
Drama<-c("Drama")
Family<-c("Family")
Fantasy<-c("Fantasy")
Foreign<-c("Foreign")
History<-c("History")
Horror<-c("Horror")
Music<-c("Music")
Mystery<-c("Mystery")
Romance<-c("Romance")
`Science Fiction`<-c("Science_Fiction")
Thriller<-c("Thriller")
War<-c("War")
Western<-c("Western")

genre_group<-function(genre) {
  
  if(genre %in% Action) {
    return("Action")
  }else if (genre %in% Adventure) {
    return ("Adventure")
  } else if (genre %in% Animation) {
    return ("Animation")
  } else if (genre %in% Comedy) {
    return("Comedy")
  } else if(genre %in% Documentary) {
    return ("Documentary")
  }else if(genre %in% Drama) {
    return ("Drama")
  }else if(genre %in% Family) {
    return ("Family")
  }else if(genre %in% Fantasy) {
    return ("Fantasy")
  }else if(genre %in% Foreign) {
    return ("Foreign")  
  }else if(genre %in% History) {
    return ("History")
  }else if(genre %in% Horror) {
    return ("Horror")
  }else if(genre %in% Music) {
    return ("Music") 
  }else if(genre %in% Mystery) {
    return ("Mystery") 
  }else if(genre %in% Romance) {
    return ("Romance")   
  }else if(genre %in%  `Science Fiction`) {
    return ("Science_Fiction") 
  }else if(genre %in% Thriller) {
    return ("Thriller")
  }else if(genre %in% War) {
    return ("War") 
  }else if(genre %in% Western) {
    return ("Western")  
  }else {
    return("Other")
  }
}

movies$genres<-sapply(movies$genres,genre_group)


#working with original languages
#85% of training dataset has EN as original lang so I'll introduce binary variable to handle it
original_language_unique <- count(movies, vars = "original_language")
movies$original_language<-ifelse(movies$original_language=="en",1,0)

#working with spoken languages
n = 0
for (i in movies$spoken_languages) {
  n = n + 1
  movies$spoken_languages[n] <- str_match_all(i, "'iso_639_1': '(.*?)'")
  movies$spoken_languages[[n]] <- movies$spoken_languages[[n]][,2]
}
spokenlang_list <- foreach(x = seq_along(movies$spoken_languages),.combine = rbind, .multicombine=TRUE,
                           .inorder=FALSE) %do%{
                             newdataframe <- setNames(do.call(data.frame, movies$spoken_languages[x]), "spokenlang")
                           }

#looked at frequency distribution and selected the most frequent languages
spokenlang_list_unique <- count(spokenlang_list, vars = "spokenlang")
spokenlang_list_unique

movies$spoken_languages <- foreach(x = seq_along(movies$spoken_languages)) %do% {
  ifelse("en" %in% movies$spoken_languages[[x]], movies$spoken_languages[x] <- 1, 
         ifelse("fr" %in% movies$spoken_languages[[x]], movies$spoken_languages[x] <- 1,
                ifelse("de" %in% movies$spoken_languages[[x]], movies$spoken_languages[x] <- 1,
                       ifelse("es" %in% movies$spoken_languages[[x]], movies$spoken_languages[x] <- 1,
                              ifelse("it" %in% movies$spoken_languages[[x]], movies$spoken_languages[x] <- 1,
                                     ifelse("ru" %in% movies$spoken_languages[[x]], movies$spoken_languages[x] <- 1,
                                            ifelse("ja" %in% movies$spoken_languages[[x]], movies$spoken_languages[x] <- 1,
                                                   ifelse("ar" %in% movies$spoken_languages[[x]], movies$spoken_languages[x] <- 1,
                                                          ifelse("zh" %in% movies$spoken_languages[[x]], movies$spoken_languages[x] <- 1,
                                                                 movies$spoken_languages[x] <- 0)))))))))
}


movies$cast_count <- str_count(movies$cast, "'name':\\s'")
movies$crew_count <- str_count(movies$crew, "'name':\\s'")

movies<-dplyr::select(movies,-crew,-cast)

#missing values are substituted with median
movies[is.na(movies$crew_count),]$crew_count <- median(movies$crew_count, na.rm = TRUE)
movies[is.na(movies$cast_count),]$cast_count <- median(movies$cast_count, na.rm = TRUE)


#checking missing data per variable
missing_data_summary_table <- as.data.frame(miss_var_summary(movies))
missing_data_summary_table%>%filter(pct_miss>0)

#substituting missing value with median
movies$runtime<-tidyr::replace_na(movies$runtime,median(movies$runtime,na.rm=TRUE))
any(is.na(movies$runtime))


#working with production companies
movies$prodcomp_count <- str_count(movies$production_companies, pattern = "\\'name\\'")

# replace NA number by median
movies$prodcomp_count[is.na(movies$prodcomp_count)] <- median(movies$prodcomp_count, na.rm = TRUE)

n = 0
for (i in movies$production_companies) {
  n = n + 1
  movies$production_companies[n] <- str_match_all(i, "'name': '(.*?)'")
  movies$production_companies[[n]] <- movies$production_companies[[n]][,2]
}

prodcomp_list <- foreach(x = seq_along(movies$production_companies),.combine = rbind, .multicombine=TRUE,
                         .inorder=FALSE) %do%{
                           newdataframe <- setNames(do.call(data.frame, movies$production_companies[x]), "prodcompany")
                         }


prodcomp_list_unique <- count(prodcomp_list, vars = "prodcompany")
prodcomp_list_unique$percentage<-(prodcomp_list_unique$freq/sum(prodcomp_list_unique$freq))*100
#sorting by percentage
prodcomp_list_unique<-prodcomp_list_unique[order(prodcomp_list_unique$percentage, decreasing = TRUE),] 

movies$production_companies <- foreach(x = seq_along(movies$production_companies)) %do% {
  ifelse("Warner Bros." %in% movies$production_companies[[x]], movies$production_companies[x] <- 1,
         ifelse("Universal Pictures" %in% movies$production_companies[[x]], movies$production_companies[x] <- 1,
                ifelse("Paramount Pictures" %in% movies$production_companies[[x]], movies$production_companies[x] <- 1,
                       ifelse("Twentieth Century Fox Film Corporation" %in% movies$production_companies[[x]], movies$production_companies[x] <- 1,
                              ifelse("Columbia Pictures" %in% movies$production_companies[[x]], movies$production_companies[x] <- 1,
                                     ifelse("Metro-Goldwyn-Mayer (MGM)" %in% movies$production_companies[[x]], movies$production_companies[x] <- 1,
                                            ifelse("New Line Cinema" %in% movies$production_companies[[x]], movies$production_companies[x] <- 1,
                                                   ifelse("Walt Disney Pictures" %in% movies$production_companies[[x]], movies$production_companies[x] <- 1,
                                                          movies$production_companies[x] <- 0 ))))))))
}

any(is.na(movies$production_companies))
movies$production_companies<-unlist(movies$production_companies)

movies_clean<-movies

#converting 0 budget to NA
movies_clean$budget_missing<-ifelse(movies_clean$budget==0,0,1)
movies_clean$budget<-ifelse(movies_clean$budget==0,NA,movies_clean$budget)

missing_data_summary_table <- as.data.frame(miss_var_summary(movies_clean))
missing_data_summary_table%>%filter(pct_miss>0)


movies_clean$spoken_languages<-unlist(movies_clean$spoken_languages)
movies_clean$belongs_to_collection<-unlist(movies_clean$belongs_to_collection)
movies_clean$genres<-unlist(movies_clean$genres)
movies_clean$budget<-as.numeric(movies_clean$budget)


#setting up NA revenue to 0 to avoid imputation
movies_clean$revenue<-ifelse(is.na(movies_clean$revenue),0,movies_clean$revenue)

#removing id column from data frame
movies_clean<-dplyr::select(movies_clean,-id)
str(movies_clean)

#converting to factors
movies_clean$genres<-as.factor(movies_clean$genres)
movies_clean$belongs_to_collection<-as.factor(movies_clean$belongs_to_collection)
movies_clean$homepage<-as.factor(movies_clean$homepage)
movies_clean$original_language<-as.factor(movies_clean$original_language)
movies_clean$production_companies<-as.factor(movies_clean$production_companies)
movies_clean$production_region<-as.factor(movies_clean$production_region)
movies_clean$spoken_languages<-as.factor(movies_clean$spoken_languages)
movies_clean$tagline<-as.factor(movies_clean$tagline)
movies_clean$Keywords<-as.factor(movies_clean$Keywords)
movies_clean$budget_missing<-as.factor(movies_clean$budget_missing)

#unlisting factors
movies_clean$spoken_languages<-unlist(movies_clean$spoken_languages)
movies_clean$belongs_to_collection<-unlist(movies_clean$belongs_to_collection)
movies_clean$genres<-unlist(movies_clean$genres)
movies_clean$budget<-as.numeric(movies_clean$budget)
movies_clean$homepage<-as.numeric(movies_clean$homepage)
movies_clean$original_language<-as.numeric(movies_clean$original_language)
movies_clean$production_companies<-as.numeric(movies_clean$production_companies)
movies_clean$production_region<-as.numeric(movies_clean$production_region)
movies_clean$tagline<-as.numeric(movies_clean$tagline)
movies_clean$Keywords<-as.numeric(movies_clean$Keywords)
movies_clean$budget_missing<-as.numeric(movies_clean$budget_missing)


# First, transform all feature to dummy variables
#converting all my variables to numeric,except revenue which is our response variable
dummy.vars <- dummyVars(~ ., data = movies_clean[, -14]) 
train.dummy <- predict(dummy.vars, movies_clean[, -14])

# Imputation
pre.process <- preProcess(train.dummy, method = "bagImpute")

imputed.data <- predict(pre.process, train.dummy)

movies_clean$budget <- imputed.data[, 3] #feeding imputed age back to our model (overwriting original values with imputed)


#no missing data left
missing_data_summary_table <- as.data.frame(miss_var_summary(movies_clean))
missing_data_summary_table%>%filter(pct_miss>0)

movies4<-movies_clean

#splitting back 2 datasets
movies_train<-dplyr::filter(movies4,revenue!=0)
movies_predict<-dplyr::filter(movies4,revenue==0)


indexes <- createDataPartition(movies_train$revenue,
                               times = 1,
                               p = 0.7,        
                               list = FALSE)   
movies.train <- movies_train[indexes,]
movies.test <- movies_train[-indexes,]

train.control <- trainControl(method = "repeatedcv",
                              number = 10,
                              repeats = 3,
                              search = "grid") 

tune.grid <- expand.grid(eta = c(0.05, 0.075, 0.1),
                         nrounds = c(50, 75, 100),
                         max_depth = 6:8,
                         min_child_weight = c(2.0, 2.25, 2.5),
                         colsample_bytree = c(0.3, 0.4, 0.5),
                         gamma = 0,
                         subsample = 1)

cl <- makeCluster(4, type = "SOCK") 

# Register cluster so that caret will know to train in parallel.
registerDoSNOW(cl)

# Train the xgboost model using 10-fold CV repeated 3 times 
# and a hyperparameter grid search to train the optimal model.
caret.cv<-train(log(revenue)~.,
                data = movies.train,
                method = "glmnet",  #"xgbTree"
                #tuneGrid = tune.grid,
                trControl = train.control)

stopCluster(cl)
caret.cv

preds <- exp(predict(caret.cv, movies.test))

pred<-exp(predict(caret.cv,movies_predict))




