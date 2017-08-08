library(rvest)
library(stringr)
library(readxl)
library(leaflet)
library(dplyr)
library(tidyr)
library(plotly)
library(pryr)

axisfont <- list(family = "sans-serif",size = 7,color = "black")
titlefont <- list(family = "sans-serif",size = 9,color = "black")
titlefont1 <- list(family = "sans-serif",size = 10,color = "black")

#read downloaded file
senators <- read.csv("commons.csv")
senators <- senators[,c(1:6)]

#read the mapdata file
mapdata <- read.csv("mapdata.csv")
#read tax Data
taxdata <- read.csv("01110004-eng.csv",na.strings = c("x" , ".." ))
colnames(taxdata) <- c("ref_date","Geo","Region","DON","Vector","co","value")
###########################################################33
total <- taxdata %>% filter(DON=="Number of taxfilers"
                            & ((!grepl(',', Geo)))) %>% select(c(ref_date,Geo,value))
taxdata1 <- taxdata %>% filter((DON=="Percentage of taxfilers aged 0 to 24 years" |
                                  DON=="Percentage of taxfilers aged 25 to 44 years" |
                                  DON=="Percentage of taxfilers aged 45 to 64 years" |
                                  DON=="Percentage of taxfilers aged 65 years and over")
                               & ((!grepl(',', Geo)))) %>% left_join(total,by=c("ref_date","Geo")) %>% 
  mutate(Number=(value.x/100)*value.y) %>% select(-c(value.y))
colnames(taxdata1) <- c("ref_date","Geo","Region","DON","Vector","co","value","Number")
###########################################################33
total1 <- taxdata %>% filter(DON=="Number of persons with total income"
                            & ((!grepl(',', Geo)))) %>% select(c(ref_date,Geo,value))
taxdata2 <- taxdata %>% filter((DON=="Percentage of persons with total income $15,000 and over" |
                                  DON=="Percentage of persons with total income $25,000 and over" |
                                  DON=="Percentage of persons with total income $35,000 and over" |
                                  DON=="Percentage of persons with total income $50,000 and over"|
                                  DON=="Percentage of persons with total income $75,000 and over"|
                                  DON=="Percentage of persons with total income $100,000 and over")
                        & ((!grepl(',', Geo)))) %>% left_join(total1,by=c("ref_date","Geo")) %>% 
mutate(Number=(value.x/100)*value.y) %>% select(-c(value.y))
colnames(taxdata2) <- c("ref_date","Geo","Region","DON","Vector","co","value","Number")


###########################################################33

total2 <- taxdata %>% filter(DON=="Number of taxfilers"
                             & ((!grepl(',', Geo)))) %>% select(c(ref_date,Geo,value))
taxdata3 <- taxdata %>% filter((DON=="Percentage of taxfilers, female" |
                                  DON=="Percentage of taxfilers, married")
                          & ((!grepl(',', Geo)))) %>% left_join(total2,by=c("ref_date","Geo")) %>% 
mutate(Number=(value.x/100)*value.y) %>% select(-c(Vector,co,value.y))
colnames(taxdata3) <- c("ref_date","Geo","Region","DON","value","Number")
###########################################################33

#Joining both the files
senators <- senators %>% arrange(Province...Territory,Constituency)
mapdata <- mapdata %>% arrange(PRNAME,FEDENAME)
mapdata <- mapdata %>% filter(FEDENAME!="Sturgeon River--Parkland")
newfile <- cbind(senators,mapdata)
newfile$name <- str_trim(paste(newfile$Honorific.Title,newfile$First.Name,newfile$Last.Name,sep=" "))
#newfile$name <- str_trim(paste(newfile$First.Name,newfile$Last.Name,sep=" "))
newfile <- newfile[,c(14,4,5,6,8,9)]
colnames(newfile) <- c("Name","Constituency","PrTr","Party","lng","lat")
newfile$color <- ifelse(newfile$Party=="Conservative","blue",ifelse(newfile$Party=="Liberal","red",ifelse(newfile$Party=="NDP","orange",ifelse(newfile$Party=="Green Party","green",ifelse(newfile$Party=="Independent","yellow","lime")))))

#Mp Image path 
url <- "https://www.ourcommons.ca/Parliamentarians/en/members?page=1"
webpage <- read_html(url)
temp_text <- webpage %>% html_nodes(xpath='//html/body/div[2]/div/div[2]/div[1]/ul/li[2]/span') %>% html_text() %>% strsplit(" ")
itemsPerPage = as.integer(tail(temp_text[[1]],3)[1])
TotalItems = as.integer(tail(temp_text[[1]],3)[3])
Pages = ceiling(TotalItems/itemsPerPage)
MP_details = data.frame(FullName=NA,ImageSource=NA)[numeric(0), ]
#Browse through each pages
for(page in 1:Pages){
  new_url = gsub("1", page , url)
  webpage <- read_html(new_url)
  img_path<- webpage %>% html_nodes(xpath='//html/body/div[2]/div/div[3]/ul/li') %>% html_nodes("div") %>% html_nodes("a") %>% html_nodes("img") %>% html_attr("src")
  fullname <- webpage %>% html_nodes(xpath='/html/body/div[2]/div/div[3]/ul/li') %>% html_nodes("div") %>% html_nodes("a") %>% html_nodes("div") %>% html_text() 
  fullname <- gsub("\\s+"," ",str_trim(gsub("[\r\n\t]", "",fullname)))
  temp = data.frame(FullName=fullname,ImageSource=img_path)
  MP_details <- rbind(MP_details,temp)
  MP_details$FullName <- as.character(MP_details$FullName)
}

#Arrange the file alphabetically
MP_details <- MP_details %>% arrange(FullName) %>% select(ImageSource)
newfile <- newfile %>% arrange(Name)
newfile = cbind(newfile,MP_details)