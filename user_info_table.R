library(easyr)
library(stringr)

#this function parses the raw txt file of chat history from WA into a usable format
parse_wa_chat <- function(x){
  
  chat <- read.txt(x) #read in txt file from path provided
  
  #each notification or message starts with the date on a new line, the time and -
  date <- str_extract_all(chat, pattern= "\\d{1,2}/\\d{2}/\\d{4} \\d{2}:\\d{2} - ") %>%
    unlist() %>% str_extract(pattern="\\d{1,2}/\\d{2}/\\d{4}") %>% as.Date(format="%d/%m/%Y")
  #extract the notifications or messages associated with each timestamp
  message_notification <- str_split_1(chat, pattern="\\n\\d{1,2}/\\d{2}/\\d{4} \\d{2}:\\d{2} - ") %>%
    str_split(pattern=":", n=2)
  names(message_notification) <- date
  #extract only messages (which are associated with a username first followed by ":") - they are entries in the list with two parts
  #extract only notifications
  is_notification <- sapply(message_notification, function(x) ifelse(length(x) == 1, T, F))
  notifications <- message_notification[is_notification] %>% unlist()
  messages <- message_notification[!is_notification] 
  message.dates <- names(messages)
  messages <- matrix(unlist(messages), nrow=length(messages), ncol=2, byrow=TRUE)
  messages <- data.frame(date=message.dates, user=messages[,1], message=messages[,2])
  messages <- messages[-1,] 
  
  #identify which users changed their numbers to a new number. 
  #Annoyingly WA doesn't give a notification if a user joined with a number and then created a WA user name for themselves
  #Neither does it tell you if a user changed their username, but I think past usernames are automatically updated in the chat history to the most recent alias
  number_change1 <- str_match(notifications, pattern=".(.+) is gewijzigd naar (.+)")
  number_change2 <- str_match(notifications, pattern=".(.+) heeft een nieuw nummer")
  number_change1 <- data.frame(date=names(notifications), from=str_trim(number_change1[,2]), to=str_trim(number_change1[,3])) %>% na.omit()
  number_change2 <- data.frame(date=names(notifications), from=str_trim(number_change2[,2])) %>% na.omit()
  number_change2$to <- rep(NA, nrow(number_change2))
  
  number_change <- rbind(number_change1, number_change2)
  
  return(list(notifications=notifications, messages=messages, number_change=number_change))
}

#this is a function to find the last alias of anyone who changed phone numbers or usernames multiple times
find_last_alias <- function(x){
  
  number_change <- x$number_change
  number_change$date <- as.Date(number_change$date)
  number_change <- number_change[order(number_change$date),]
  
  number_change$last.alias <- rep(NA, nrow(number_change))
  
  for(i in 1:nrow(number_change)){
    
    original.alias <- number_change$from[i]
    mr.last.alias <- number_change$to[i]
    
    if(is.na(mr.last.alias)) next
    
    for(j in i:nrow(number_change)){
      
      if(number_change$from[j] == mr.last.alias) mr.last.alias <- number_change$to[j]
      if(is.na(mr.last.alias)) break
    }
    
    number_change$last.alias[i] <- mr.last.alias
  }
  
  number_change <- data.frame(user=number_change$from, last.alias=number_change$last.alias)
  return(number_change)
}


#this function generates a table from the parsed data with some stats about each user
#note that some people changed their phone numbers or usernames so date.joined may be NA
generate_user_info_table <- function(x){

  list <- parse_wa_chat(x)
  notifications <- list$notifications
  messages <- list$messages
  number_change <- find_last_alias(list) %>% na.omit()
  
  #first we will make a list of all users who have left or been removed since the creation of the group and the date they last left
  left <- str_match(notifications, pattern=".(.+) heeft de groep verlaten")
  left <- data.frame(date=names(notifications), user=str_trim(left[,2])) %>% na.omit()
  left <- merge(left, number_change, by="user", all.x=TRUE)
  for(i in 1:nrow(left)) ifelse(!is.na(left[i,'last.alias']), left[i,'user'] <- left[i,'last.alias'], next) 
  idx <- duplicated(left$user, fromLast = TRUE)
  last_left <- left[!idx,]
  
  removed <- str_match(notifications, pattern=".+ (heeft|hebt) (.+) verwijderd")
  removed <- data.frame(date=names(notifications), user=str_trim(removed[,3])) %>% na.omit()
  removed <- merge(removed, number_change, by="user", all.x=TRUE)
  for(i in 1:nrow(removed)) ifelse(!is.na(removed[i,'last.alias']), removed[i,'user'] <- removed[i,'last.alias'], next) 
  idx <- duplicated(removed$user, fromLast=TRUE)
  last_removed <- removed[!idx,]
  
  last_left <- rbind(last_left, last_removed)
  idx <- duplicated(last_left$user, fromLast=TRUE)
  last_left <- last_left[!idx,]
  last_left <- last_left[,1:2]
  
  #second we will make a list of all users by when they last joined or were added
  joined <- str_match(notifications, pattern=".(.+) is deelnemer geworden")
  joined <- data.frame(date=names(notifications), user=str_trim(joined[,2])) %>% na.omit() 
  joined <- merge(joined, number_change, by="user", all.x=TRUE)
  for(i in 1:nrow(joined)) ifelse(!is.na(joined[i,'last.alias']), joined[i,'user'] <- joined[i,'last.alias'], next) 
  idx <- duplicated(joined$user)
  first_joined <- joined[!idx,]
  
  added <- str_match(notifications, pattern=".+ (heeft|hebt) (.+) toegevoegd")
  added <- data.frame(date=names(notifications), user=str_trim(added[,3])) %>% na.omit()
  added <- merge(added, number_change, by="user", all.x=TRUE)
  for(i in 1:nrow(added)) ifelse(!is.na(added[i,'last.alias']), added[i,'user'] <- added[i,'last.alias'], next) 
  idx <- duplicated(added$user)
  first_added <- added[!idx,]
  
  first_joined <- rbind(first_joined, first_added)
  idx <- duplicated(first_joined$user)
  first_joined <- first_joined[!idx,]
  first_joined <- first_joined[,1:2]
  
  #when was the last message users sent?
  messages <- merge(messages, number_change, by="user", all.x=TRUE)
  for(i in 1:nrow(messages)) ifelse(!is.na(messages[i,'last.alias']), messages[i,'user'] <- messages[i,'last.alias'], next) 
  idx <- duplicated(messages$user, fromLast=TRUE)
  last_message <- messages[!idx,]
  last_message <- last_message[,1:3]
  colnames(last_message) <- c("user","date.last.message","message")
  
  #how many messages did each user send?
  messages_by_user <- table(messages$user) %>% data.frame()
  colnames(messages_by_user) <- c("user","total.messages")
  
  #put these together
  user_info_table <- merge(first_joined, last_left, by="user", all=TRUE, suffixes=c(".joined",".left"))
  user_info_table <- merge(user_info_table, last_message, by="user", all=TRUE)
  user_info_table <- merge(user_info_table, messages_by_user, by="user", all=TRUE)
  
  user_info_table$date.joined <- as.Date(user_info_table$date.joined)
  user_info_table$date.left <- as.Date(user_info_table$date.left)
  user_info_table$date.last.message <- as.Date(user_info_table$date.last.message)
  
  return(user_info_table)
  
}


