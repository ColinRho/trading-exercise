library(data.table)
library(dplyr)
library(magrittr)



fileName <- "data/2018_FS_03_PL_Consolidated_20190521.txt"
conn <- file(fileName, open = "r")
linn <- readLines(conn)

colNames <- linn[1] %>% strsplit(., "\t") %>% unlist() %>% t()
colNames[13] <- "X1"
colNames[15] <- "X2"
colNames[16] <- "X3"


dat <- data.frame(matrix(vector(), nrow = 0, ncol = length(colNames)), stringsAsFactors = FALSE)


for (i in 2:length(linn)) {
  
  row <- linn[i] %>% strsplit(., "\t") %>% unlist() %>% t()
  
  if(row[11] %in% c("ifrs_Revenue", "dart_OperatingIncomeLoss")) {
  
    if(length(row) != 18) {length(row) = 18 ; row[18] <- "NA"}
    
    dat <- rbind(dat, data.frame(matrix(row, nrow=1, ncol=18), stringsAsFactors = FALSE))
    
  }
  
}

close(conn)

colnames(dat) <- colNames

dat <- dat %>% select(-c("X1","X2","X3")) # remove empty columns
dat2 <- dat %>% select(c(2,3,4,5,6,11,13,14,15))

dat2$당기 <- round(gsub(pattern = ",", "", dat$당기) %>% as.numeric() / 10^6)
dat2$전기 <- round(gsub(pattern = ",", "", dat$전기) %>% as.numeric() / 10^6)
dat2$전전기 <- round(gsub(pattern = ",", "", dat$전전기) %>% as.numeric() / 10^6)
dat2$종목코드 <- gsub(pattern = "[[:punct:]]", "", dat$종목코드)

dat2 <- reshape(dat2,
                idvar = c("종목코드", "회사명", "시장구분", "업종", "업종명"),
                timevar = "항목코드",
                direction = "wide")


write.csv(x = dat2, file = "output.csv")
 



# naver 금융 시가총액 크롤링

kor_marketcap_scrape = function(code) {
  
  # NAVER Finance URL
  naverFin_url = "https://finance.naver.com/item/main.nhn?code="
  
  request_url = paste0(naverFin_url, code)
  

  req = httr::GET(request_url, encoding = 'UTF-8')
  
  
  market_cap <- content(req) %>%
    html_nodes('.tab_con1') %>%
    html_nodes('.first') %>%
    html_node('em') %>%
    gsub('\\s+', '', .) %>%
    gsub('조','',.) %>% 
    readr::parse_number()
    
  
  return(market_cap)
  
  }



cap = c()

for(i in 1:nrow(dat2)) {
  
  cap[i] = kor_marketcap_scrape(dat2$종목코드[i])
  cat(".")
  if((i %% 100) == 0) cat('\n')
  if(i == nrow(dat2)) cat ('\n')
  
}


dat3 = data.frame(dat2, 시가총액 = cap)

write.csv(x = dat3, file = 'output.csv')

