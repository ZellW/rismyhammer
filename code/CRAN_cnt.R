#https://cran.r-project.org/web/packages/available_packages_by_date.html
#https://cran.r-project.org/web/packages/available_packages_by_name.html
#http://blog.revolutionanalytics.com/2016/04/cran-package-growth.html

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

#Need the dates for each package.  The we can count how many by date
for(x in 1:nrow(readByDate)){
     cntDatesDF[x,1] <- substr(readByDate[x,1],11, 20)
}
cntDatesDF$Date <- ymd(cntDatesDF$Date)
cntDatesDF <- arrange(cntDatesDF, Date)
head(cntDatesDF,1)
tail(cntDatesDF,1)
nrow(cntDatesDF)







#############################

#-----
extract_url <- function() {
     url <- list(archive = "https://cran-archive.r-project.org/bin/windows/contrib/",
          active = "https://cran.r-project.org/bin/windows/contrib/")
     
     get_urls <- function(url) {
          txt <- readLines(url)
          idx <- grep("\\d.\\d+/", txt)
          txt[idx]
          versions <- gsub(".*?>(\\d.\\d+(/)).*", "\\1", txt[idx])
          versions
          paste0(url, versions)
     }
     z <- lapply(url, get_urls)
     unname(unlist(z))
}




# Given a CRAN URL, extract the number of packages and date
extract_pkg_info <- function(url) {
     extract_date <- function(txt, fun = max) {
          txt <- txt[ - grep("[(STATUS)|(PACKAGES)](.gz)*", txt)]
          pkgs <- grep(".zip", txt)
          txt <- txt[pkgs]
          ptn <- ".*?>(\\d{4}-\\d{2}-\\d{2}).*"
          idx <- grep(ptn, txt)
          date <- gsub(ptn, "\\1", txt[idx])
          date <- as.Date(date, format = "%Y-%m-%d")
          match.fun(fun)(date)
     }
     
     message(url)
     txt <- readLines(url)
     count <- length(grep(".zip", txt))
     # sum(grepl(".zip", txt))
     
     # head(txt)
     data.frame(
          version = basename(url),
          date = extract_date(txt),
          pkgs = count
     )
}


# Get the list of CRAN URLs
CRAN_urls <- extract_url()
CRAN_urls

# Extract package information
pkgs <- lapply(CRAN_urls, extract_pkg_info)
pkgs <- do.call(rbind, pkgs)
head(pkgs)
tail(pkgs)

pkgs <- head(pkgs, -2) # Remove r-devel and r-future

# Extract major release information
major_releases <- pkgs[grep("\\.0", pkgs$version),]

#
library(ggplot2)
p <- ggplot(pkgs, aes(x = date, y = pkgs)) +
     geom_smooth() +
     geom_point() +
     geom_rug(colour = "grey50") +
     geom_vline(data = major_releases,
                aes(xintercept = as.numeric(date)),
                colour = "grey80") +
     geom_text(data = major_releases,
               aes(label = paste("Version", version), y = 8000),
               angle = 90,
               colour = "red",
               hjust = 1, vjust = -1) +
     geom_text(data = pkgs,
               aes(label = version, y = 50),
               colour = "blue") +
     theme_minimal(16) +
     ggtitle("Number of CRAN packages per R version") +
     xlab(NULL) +
     ylab(NULL)

print(p)


          
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
