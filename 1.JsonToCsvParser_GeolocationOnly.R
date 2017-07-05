rm(list=ls())

library(jsonlite)
library(data.table)
library(tidyverse)
library(stringr)
options('max.print'=9999999)
options(encoding="UTF-8")

directory <- "/Users/Macbook/Documents/Work/CST/1)Livingstone/24Hours/jsons/" #the directory where your json files are, dont forget to add last /
file.names<- list.files(directory)
file.names
number.of.files <- length(file.names)
out <- NULL

for (i in 1:5) {
     file.path <- paste(directory,file.names[i],sep = "")
     # file.path <- "/Users/Macbook/Documents/Work/CST/1)Livingstone/24Hours/jsons/tweets2016-04-28-09-00.jsonl"
     temp <- fromJSON(sprintf("[%s]", paste(readLines(file.path), collapse=",")), flatten = T)
     temp1 <- temp[temp$lang=="en",] # sorts only tweets in English
     
     temp2 <- temp1 %>% 
          select(text,created_at,id_str,geo.coordinates) #select columns
     
     temp3 <- temp2[ (map_lgl(temp2$geo.coordinates, is.null)==FALSE),] #select only the tweets with geolocations

     print(i)
     # print(temp3$geo.coordinates)

     # possibleError <-  tryCatch (
     #      (temp3$geo.coordinates %>% unnest()),
     #      error= function (e) e
     # )
     # if (inherits(possibleError, "error")) next
     
     temp.coor <- temp3$geo.coordinates %>% 
                         as.character() %>% 
                         str_sub(., 3, -2) %>% 
                         str_split_fixed(., ", ", 2 ) %>% 
                         as.data.frame() %>% 
                         rename (lat=V1, long=V2)
     
     temp4 <- as.data.table(cbind(temp3,temp.coor))

     out <- rbind(out, temp4)
     # pattern.UK <- "\\bLondon\\b|\\bUK\\b|\\bU.K\\b|\\bU.K.\\b|\\bUnited Kingdom\\b|\\England\\b|\\bBritain\\b|\\bWales\\b|\\bNorthern Ireland\\b|\\bScotland\\b"
     # temp5 <- temp1[ grepl(pattern.UK,temp1$user$location,ignore.case = TRUE) | grepl(pattern.UK,temp1$user$description,ignore.case = TRUE) | temp1$user$time_zone=="London"| temp1$user$time_zone=='Edinburgh',] # working, struggled with grepl

}

# pattern.UK <- "\\bLondon\\b|\\bUK\\b|\\bU.K\\b|\\bU.K.\\b|\\bUnited Kingdom\\b|\\England\\b|\\bBritain\\b|\\bWales\\b|\\bNorthern Ireland\\b|\\bScotland\\b"
# temp5 <- temp1[ grepl(pattern.UK,temp1$user$location,ignore.case = TRUE) | grepl(pattern.UK,temp1$user$description,ignore.case = TRUE) | temp1$user$time_zone=="London"| temp1$user$time_zone=='Edinburgh',] # working, struggled with grepl

out <- out %>% 
     select(-geo.coordinates)
write.csv(out, "/Users/Macbook/Desktop/coordinates.csv", row.names=FALSE)


library(ggplot2)
library(maps)
library(ggthemes)


world <- ggplot() +
     borders("UK", colour = "gray85", fill = "gray80") + # check uk
     theme_map() 

map <- UK +
     geom_point(aes(x = lon, y = lat, size = 2),
                data = out, 
                colour = 'purple', alpha = .5) +
     # scale_size_continuous(range = c(1, 8), 
     #                     breaks = c(250, 500, 750, 1000)) +
     # labs(size = 'Followers')
     

    
  # # Main Tweet 
  #    data.temp.tweet <- temp5[,c("id_str","text","created_at","timestamp_ms" )]#still needs renaming
  #    data.temp.tweet <- rename(data.temp.tweet, c( "id_str"="tweet.id.str","text"="tweet.text.str",
  #                                                  "created_at"="tweet.time.str", "timestamp_ms"="timestamp.str" ))
  # 
  # 
  #    user <- as.data.frame(temp5$user) # creating a dataframe for the user
  #    data.temp.user <-  as.data.frame(user[, c("id_str","screen_name","name","verified", "followers_count", "friends_count", "statuses_count", 
  #                                              "description", "location", "time_zone")])
  #    data.temp.user <- rename(data.temp.user, c("id_str"='user.id.str', "screen_name"='user.handle.str', "name"='user.name.str', "verified"='user.verified', 
  #                                               "followers_count"='user.followers', "friends_count"= 'user.following', "statuses_count"='user.status.count', 
  #                                               "description"='user.description,str', "location"='user.location.str', "time_zone"='user.timezone'))
  # 
  #    # Selecting fields from Retweeted tweets (originals)
  #    
  #    retweet <- as.data.frame(temp5$retweeted_status)
  #    data.temp.retweeted <- retweet[,c("id_str","text","created_at","favorite_count","retweet_count" )]#still needs renaming
  #    data.temp.retweeted <- rename(data.temp.retweeted, c( "id_str"="retweeted.id.str","text"="retweeted.text.str",
  #                                                          "created_at"="retweeted.time.str", "favorite_count"="retweeted.favorite.count", 
  #                                                          "retweet_count"= "retweeted.retweet.count" ))
  #    # Selecting fields from retweeted user
  #    retweeted.user <- retweet$user # make retweeted user fields a dataset and then select required fields
  #    data.temp.retweeted.user <-  as.data.frame(retweeted.user[, c("id_str","screen_name","name","verified", "followers_count", "friends_count", "statuses_count", 
  #                                                                  "description", "location", "time_zone")])
  #    data.temp.retweeted.user <- rename(data.temp.retweeted.user, c("id_str"='retweeted.user.id.str', "screen_name"='retweeted.user.handle.str', "name"='retweeted.user.name.str', 
  #                                                                   "verified"='retweeted.user.verified', 
  #                                                                   "followers_count"='retweeted.user.followers', "friends_count"= 'retweeted.user.following', "statuses_count"='retweeted.user.status.count', 
  #                                                                   "description"='retweeted.user.description,str', "location"='retweeted.user.location.str', "time_zone"='retweeted.user.timezone'))
  #    # Selecting fields if quoted
  #    quoted <- as.data.frame(temp5$quoted_status)
  #    data.temp.quoted <- quoted[,c("id_str","text","created_at","favorite_count","retweet_count" )]#still needs renaming
  #    data.temp.quoted <- rename(data.temp.quoted, c( "id_str"="quoted.id.str","text"="quoted.text.str",
  #                                                    "created_at"="quoted.time.str", "favorite_count"="quoted.favorite.count", 
  #                                                    "retweet_count"= "quoted.retweet.count" ))
  #    # Seleting fields from quoted user
  #    
  #    quoted.user <- as.data.frame(temp5$quoted_status$user)
  #    data.temp.quoted.user <-  as.data.frame(quoted.user[, c("id_str","screen_name","name","verified", "followers_count", "friends_count", "statuses_count", 
  #                                                            "description", "location", "time_zone")])
  #    
  #    data.temp.quoted.user <- rename(data.temp.quoted.user, c("id_str"='quoted.user.id.str', "screen_name"='quoted.user.handle.str', "name"='quoted.user.name.str', "verified"='quoted.user.verified', 
  #                                                             "followers_count"='quoted.user.followers', "friends_count"= 'quoted.user.following', "statuses_count"='quoted.user.status.count', 
  #                                                             "description"='quoted.user.description,str', "location"='quoted.user.location.str', "time_zone"='quoted.user.timezone'))
  #    # Selecting fiedls form in reply to
  #    
  #    data.temp.reply <- temp5[, c("in_reply_to_status_id_str",'in_reply_to_user_id_str','in_reply_to_screen_name')]
  #    data.temp.reply <- rename(data.temp.reply, c("in_reply_to_status_id_str" = "in.reply.to.status.id.str", 
  #                                                 'in_reply_to_user_id_str'= 'in.reply.to.user.id.str', 'in_reply_to_screen_name'='in.reply.to.screen.name'))
  # 
  #    temp.binded <- cbind(data.temp.tweet,data.temp.user,data.temp.retweeted,data.temp.retweeted.user,data.temp.quoted, data.temp.quoted.user, data.temp.reply) #binding to form the ultimate dataset
  #    temp.binded <- temp.binded %>% tidyr::drop_na(tweet.id.str)
  #    sum(is.na(temp.binded$tweet.id.str))
  #    write.csv(temp.binded, paste(file.path, ".csv", sep=""), fileEncoding = "UTF-8")

