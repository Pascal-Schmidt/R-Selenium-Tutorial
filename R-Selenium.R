##################
### Example #1 ###
##################

# load libraries
library(RSelenium) 
library(tidyverse)

# connect to chrome driver
driver <- RSelenium::rsDriver(browser = "chrome",
                              chromever =
                                system2(command = "wmic",
                                        args = 'datafile where name="C:\\\\Program Files (x86)\\\\Google\\\\Chrome\\\\Application\\\\chrome.exe" get Version /value',
                                        stdout = TRUE,
                                        stderr = TRUE) %>%
                                stringr::str_extract(pattern = "(?<=Version=)\\d+\\.\\d+\\.\\d+\\.") %>%
                                magrittr::extract(!is.na(.)) %>%
                                stringr::str_replace_all(pattern = "\\.",
                                                         replacement = "\\\\.") %>%
                                paste0("^",  .) %>%
                                stringr::str_subset(string =
                                                      binman::list_versions(appname = "chromedriver") %>%
                                                      dplyr::last()) %>%
                                as.numeric_version() %>%
                                max() %>%
                                as.character())

remote_driver <- driver[["client"]] 
remote_driver$navigate("https://www.latlong.net/convert-address-to-lat-long.html")

# get coordinates
address_element <- remote_driver$findElement(using = 'class', value = 'width70')
address_element$sendKeysToElement(list("Lombard Street, San Francisco"))
button_element <- remote_driver$findElement(using = 'class', value = "button")
button_element$clickElement()
out <- remote_driver$findElement(using = "class", value="coordinatetxt") 
lat_long <- out$getElementText()

street_names <- c("Lombard Street, San Francisco", 
                  "Santa Monica Boulevard", 
                  "Bourbon Street, New Orleans", 
                  "Fifth Avenue, New York", 
                  "Richards Street, Vancouver")

get_lat_lon <- function(street_names) {
  remote_driver$navigate("https://www.latlong.net/convert-address-to-lat-long.html")
  final <- c()
  for(i in 1:length(street_names)) {
    
    remote_driver$refresh()
    Sys.sleep(1)
    
    address_element <- remote_driver$findElement(using = 'class', value = 'width70')
    
    address_element$sendKeysToElement(list(street_names[i]))
    button_element <- remote_driver$findElement(using = 'class', value = "button")
    
    button_element$clickElement()
    Sys.sleep(3)
    
    out <- remote_driver$findElement(using = "class", value = "coordinatetxt")
    output <- out$getElementText()
    final <- c(final, output)
    
  }
  
  return(final)
}


vector_out <- get_lat_lon(street_names)

data.frame(street_names, purrr::flatten_chr(vector_out)) %>%
  dplyr::mutate(., vector_out = stringr::str_remove_all(vector_out, "\\(|\\)")) %>%
  tidyr::separate(., vector_out, into = c("latitude", "longitude"), sep = ",")


##################
### Example #2 ###
##################

url <- "https://www.canadapost.ca/cpo/mc/personal/postalcode/fpc.jsf" 
remote_driver$navigate(url)

address_element <- remote_driver$findElement(using = 'id', value = 'addressComplete') 
address_element$sendKeysToElement(list("413 Seymour Street Vancouver"))
address_element$sendKeysToElement(list(" "))

button_element <- remote_driver$findElement(using = 'id', value = 'searchFpc') 
button_element$clickElement()

output <- remote_driver$findElement(using = "id", value="HeaderAddressLabel") 
output <- output$getElementText()

unlist(output) %>% 
  stringr::str_sub(., start = -7, end = -1)
