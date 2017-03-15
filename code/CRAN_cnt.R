#https://cran.r-project.org/web/packages/available_packages_by_date.html
#https://cran.r-project.org/web/packages/available_packages_by_name.html
#http://blog.revolutionanalytics.com/2016/04/cran-package-growth.html
#
#Update with this: https://www.r-bloggers.com/scraping-cran-with-rvest/?utm_source=feedburner&utm_medium=email&utm_campaign=Feed%3A+RBloggers+%28R+bloggers%29
#aand with this https://rpubs.com/Radcliffe/superbowl
#
library(rvest)
#library(magrittr)
library(ggplot2)
#library(plotly)
library(dplyr)
library(readr)
library(stringr)
#library(zoo)
library(lubridate)

# Scrapes CRAN archives to determine the number of packages per release

# Create a list of pages to scrape, including both archive and current
# use this for testing http://www.regexpal.com/ , http://www.regexr.com/
# href=\\"(.*?)\/ gives href=\"2.4/ then can parse
# 
urls <- list(archive = "https://cran-archive.r-project.org/bin/windows/contrib/",
            active = "https://cran.r-project.org/bin/windows/contrib/")

#Get the archive data
htmlText <- read_lines(urls[[1]])
#Examine the htmlText and note that the first line of interest is line 12-25
htmlText <- htmlText[12:25]
#Make a data frame
htmlText <- as.data.frame(htmlText)

#Get a useful piece of the text (href=\"1.7/).
tmpPT1 <- as.data.frame(str_locate(htmlText$htmlText, "href="))
tmpPT2 <- as.data.frame(str_locate(htmlText$htmlText, "</a>"))
htmlText <- mutate(htmlText, version=str_trim(str_sub(htmlText$htmlText, tmpPT1$end + 2, tmpPT2$start-8)))
htmlText <- mutate(htmlText, nchar=nchar(htmlText$version))

htmlText$version[htmlText$nchar==5] <- str_sub(htmlText$version[htmlText$nchar==5], 1, 4)

rm("tmpPT1", "tmpPT2")

folderURLs <- as.data.frame(matrix(nrow = nrow(htmlText), ncol = 1))

for(x in 1:nrow(folderURLs)){
     folderURLs[x,1] <- paste0(urls[[1]], htmlText[x,2])
}

folderHMTL <-  read_lines(folderURLs[1,1])
folderHMTL <- folderHMTL[13:length(folderHMTL)-3]
#Need to get package name and date
tmpPT1 <- as.data.frame(str_locate(htmlText$htmlText, "href="))
tmpPT2 <- as.data.frame(str_locate(htmlText$htmlText, "</a>"))
tmpPT1 <- as.data.frame(str_locate(htmlText$htmlText, "align=\"right\">"))

#############################
readByDate <- read_lines("https://cran.r-project.org/web/packages/available_packages_by_date.html")
readByDate <- readByDate[18:length(readByDate)-3]
readByDate <- as.data.frame(readByDate)

tmpBool <- str_detect(readByDate$readByDate, "<tr> <td>")

readByDate <- data_frame(readByDate[tmpBool,])
names(readByDate) <- c("rawHTML")
readByDate$rawHTML <- as.character(readByDate$rawHTML)

cntDatesDF <- data.frame(matrix(nrow=0, ncol=1))
colnames(cntDatesDF) <- c("Date")

#Need the dates for each package.  Then we can count how many by date
for(x in 1:nrow(readByDate)){
     cntDatesDF[x,1] <- substr(readByDate[x,1],11, 20)
}
cntDatesDF$Date <- ymd(cntDatesDF$Date)
cntDatesDF <- arrange(cntDatesDF, Date)
head(cntDatesDF,1)
tail(cntDatesDF,1)
nrow(cntDatesDF)


#############################
#Different way to get table - WORKS!
pkgs <- "https://cran.r-project.org/web/packages/available_packages_by_date.html"
pkgs_raw <- read_html(pkgs) %>% 
     html_nodes("table") %>% 
     .[[1]] %>%
     html_table()

#################################

url <- "https://cran.r-project.org/web/packages/available_packages_by_date.html"

page <- read_html(url)
page %>%
     html_node("table") %>%
     html_table() %>%
     mutate(count = rev(1:nrow(.))) %>%
     mutate(Date = as.Date(Date)) %>%
     mutate(Month = format(Date, format="%Y-%m")) %>%
     group_by(Month) %>%
     summarise(published = min(count)) %>%
     mutate(Date = as.Date(as.yearmon(Month))) -> pkgs

margins = list(l = 100, r = 100, b = 100, t = 100, pad = 4)

pkgs %>%
     plot_ly(x=pkgs$Date, y=pkgs$published, name="Published packages") %>%
     layout(title = "CRAN packages published ever since.", margin = margins)

#####################################
