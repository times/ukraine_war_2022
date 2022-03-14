###############################################################
# Title: Ukrainian refugees by country of destination
# Author: Anna Lombardi
# Source: UNHCR - http://data2.unhcr.org/en/situations/ukraine
###############################################################


library(pacman)
p_load ('rjson', 'jsonlite','plyr', 'tidyr', 'dplyr', 'ggplot2', 'googlesheets4', 'DatawRappr')


# 1. Get the data
data_raw <- fromJSON('https://data2.unhcr.org/population/get/sublocation?widget_id=283408&sv_id=54&population_group=5459,5460&forcesublocation=0&fromDate=1900-01-01')
data <- data_raw$data 


# 2. Clean the data: unnest your list --> select relevant columns --> rename
final <- data%>%
  select(1,4,5,11,18)%>%
  dplyr::rename(country=c(1), lon=c(2), lat=c(3), date=c(4), refugees=c(5))

stand_first <- paste0("Around ", round(sum(as.numeric(final$refugees))/1000000,1),
                      " million refugees have fled Ukraine since Russia’s invasion")

final.2 <- final%>%
  filter(!country %in% c("Russian Federation", "Other European countries"))

# 3. Update DW chart
dw_data_to_chart(final.2, chart_id = "Ss76u")
# Tweak stand if needed
dw_edit_chart(chart_id = "Ss76u", intro = stand_first, annotate = paste("Data as of",gsub(" "," ",format(Sys.Date(),"%b %d"))))
dw_publish_chart(chart_id = "Ss76u")


# ---------------------------------------------------------------------------------------------
# NOTE: annotations for Russia and EU have to be tweaked manually in DW. Here the data you need
# ---------------------------------------------------------------------------------------------
notes <- final %>%
  select(1,5)%>%
  dplyr::filter(country %in% c('Russian Federation', 'Other European countries'))


