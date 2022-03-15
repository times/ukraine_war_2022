###############################################################
# Title: Russian protests collected by Ovd-info 
# Author: Ademola Bello
# Source: Ovd-info - https://ovdinfo.org/
###############################################################

# Main section ------------------------------------------------------------

# Read in old data
trans_ref <- readRDS("data/city_name_translations.rds")
original_scrape <- readRDS("data/russia_protests.rds")

# Scrape new data if available
url_list <- c("#INSERT URL HERE")

store <- list()

#Loop through the URLS 
for(url in url_list){
  print(url)
  
  ovd_city <- read_html(url)%>%
    html_nodes("h2")%>%
    html_text()%>%
    .[. != "Помочь проекту"]
  
  ovd_text <- read_html(url)%>%
    html_nodes("p strong")%>%
    html_text()
  
  url_date <- str_extract(url, "[0-9]+/[0-9]+/[0-9]+")
  
  
  df <- data.frame(ovd_city)%>%
    cbind(data.frame(ovd_text))%>%
    mutate(numbers = str_extract(ovd_text, "[0-9]+"),
           date = url_date)
  
  store[[url]] <- df
  
}

# Turn into a dataframe
new_data <- data.table::rbindlist(store)

# Combine new data with original data
new_table <- original_scrape%>%
  rbind(new_data) #If there is no new data then comment this out

saveRDS(new_table, "data/russia_protests.rds")

# Join to english names reference
final_table <- new_table%>% #If there is now new data then comment this out
  left_join(trans_ref, by = "ovd_city") #Check all the cities have English name column

# Data for map
summary_protests <- final_table%>%
  group_by(ovd_city, english_name)%>%
  summarise(total = sum(as.numeric(numbers)))

dw_data_to_chart(summary_protests, chart_id = "Qf43z")

# Data for column chart
over_time <- final_table%>%
  group_by(date)%>%
  summarise(total = sum(as.numeric(numbers)))%>%
  mutate(date = as.Date(date,format = "%Y/%m/%d"))

dw_data_to_chart(over_time, "8mIvb")

# Initial scrape of the data ----------------------------------------------

# Data generated here is saved in the RDS files

# url1 <- "https://ovd.news/news/2022/03/13/spiski-zaderzhannyh-v-svyazi-s-akciyami-protiv-voyny-s-ukrainoy-13-marta-2022-goda"
# 
# get_url <- read_html(url1)%>%
#   html_nodes("a")%>%
#   html_attr("href")%>%
#   enframe()%>%
#   filter(grepl("protiv-voyny-s-ukrainoy-", value))%>%
#   mutate(value = paste0("https://ovd.news", value))
# 
# url_list <- get_url$value
# url_list[13] <- url1
# 
# store <- list()
# for(url in url_list){
#   print(url)
#   
#   ovd_city <- read_html(url)%>%
#     html_nodes("h2")%>%
#     html_text()%>%
#     .[. != "Помочь проекту"]
#   
#   ovd_text <- read_html(url)%>%
#     html_nodes("p strong")%>%
#     html_text()
#   
#   url_date <- str_extract(url, "[0-9]+/[0-9]+/[0-9]+")
#   
#   
#   df <- data.frame(ovd_city)%>%
#     cbind(data.frame(ovd_text))%>%
#     mutate(numbers = str_extract(ovd_text, "[0-9]+"),
#            date = url_date)
#   
#   store[[url]] <- df
#   
# }
# 
# test <- data.table::rbindlist(store)
# 
# test.2 <- test%>%
#   group_by(ovd_city)%>%
#   summarise(n = n())
# 
# sheet_write(ss="1ozDZF6r-FTErOogEIwcrJUbyi5NcdoXFUB3ulfYVfE4", test.2)
# 
# trans_ref <- read_sheet(ss="1ozDZF6r-FTErOogEIwcrJUbyi5NcdoXFUB3ulfYVfE4", sheet = "rus_eng_translate")
# 
# saveRDS(trans_ref%>%select(ovd_city, english_name), "code/ukraine_war_2022/city_name_translations.rds")
# saveRDS(test, "code/ukraine_war_2022/russia_protests_upto_15mar.rds")

