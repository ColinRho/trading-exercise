library(httr)
library(rvest)
library(dplyr)
library(RSelenium)
library(magrittr)


# RSelenium setting


ch = wdman::chrome(port = 4445L)
remDr = remoteDriver(port = 4445L, browserName = 'chrome')
remDr$open()


# Dart URL

start_search = function() {
    
    # 기업 개황 URL
    remDr$navigate("http://dart.fss.or.kr/dsae001/main.do")
    
    firstpage = remDr$getPageSource()[[1]]
    
    # 전체 업종 리스트 
    biztype_list = data.frame(type = firstpage %>% 
                                  read_html() %>%
                                  html_nodes('#typesOfBusiness') %>% 
                                  html_nodes('option') %>%
                                  html_text(trim = TRUE),
                              value = firstpage %>% 
                                  read_html() %>%
                                  html_nodes('#typesOfBusiness') %>% 
                                  html_nodes('option') %>%
                                  html_attr('value')
    )
    
    # 제조업 only
    
    biztype_list %<>% filter(., grepl('제조업', type))
    
    # 업종 select
    repeat{
      
        print(biztype_list)
      
        bizvalue = readline(prompt = "업종 코드를 선택하세요: ")
        
        if (!(bizvalue %in% biztype_list$value)) {
            cat("유효하지 않은 값입니다.", '\n')
            Sys.time(1)
        }
        
        if (bizvalue %in% biztype_list$value) break
        
    }
    
    bizType = remDr$findElement(using = "xpath",
                              paste0('//*[@id="typesOfBusiness"]/option[@value="', bizvalue, '"]'))
    bizType$clickElement()
    
    Sys.sleep(1)
    
    
    
    # 법인유형 select
    repeat{
        
        cat('법인유형 Option', '\n',
            'all: 전체', '\n',
            'P: 유가증권시장', '\n',
            'A: 코스닥시장', '\n',
            'N: 코넥스시장', '\n',
            'E: 기타법인', '\n'
        )
        corpvalue = readline(prompt = "법인유형을 선택하세요: ")
        
        if (!(corpvalue %in% c('all', 'P', 'A', 'N', 'E'))) {
            cat("유효하지 않은 값입니다.", '\n')
            Sys.time(1)
        }
        if (corpvalue %in% c('all', 'P', 'A', 'N', 'E')) break
      
    }
    
    corpType = remDr$findElement(using = "xpath",
                               paste0('//*[@id="corporationType"]/option[@value="', corpvalue, '"]'))
    corpType$clickElement()
    
    Sys.sleep(1)
    
    # 검색 실행 
    searchBtn = remDr$findElement(using = "class name", value = "btn3")
    searchBtn$clickElement()
    
    Sys.sleep(1)
    
    
    # 검색 결과 페이지 수 as search_result
    resultpage = remDr$getPageSource()[[1]]
  
    resultpage %>% 
        read_html() %>%
        html_node(., '.page_info') %>%
        html_text(., trim=TRUE) %>%
        gsub("1/", "", .) %>%
        strsplit(., "\\] \\[") -> number_extract
    
    number_extract <- number_extract[[1]] %>% gsub('\\D', '', .)
    
    search_result <<- list(
        numofpages = number_extract[1] %>% as.integer(),
        numofcomps = number_extract[2] %>% as.integer()
    )
    
    cat('총 ', search_result$numofcomps, '개 업체 검색됨', '\n')
    
}

####################



# 검색 결과에서 데이터 테이블 추출
get_table = function() {
  
    resultpage = remDr$getPageSource()[[1]]
    
    # crp_cd 추출
    resultpage %>% 
        read_html() %>%
        html_node(., '.list') %>%
        html_nodes('a') %>%
        html_attr('onclick') %>% 
        gsub("[^0-9]", "",.) %>% 
        substring(., 2) -> crp_cd
    
    # 회사명 추출
    resultpage %>% 
        read_html() %>%
        html_node(., '.list') %>%
        html_nodes('a') %>% 
        html_text() -> comp_name
    
    res = data.frame(comp_name, crp_cd)
    
    return(res)
  
}



  
## loop
do_loop = function(numofpages) {
    

    # 1페이지 이상인 일반적 loop
    
    if(numofpages != 1) {
        
        # 빈 리스트 생성
        dat_list = list()
        
        # 다음으로 넘겨야할 섹션 수
        pages = ceiling(search_result$numofpages / 10)
        
        for(i in 1:pages) {
            
            
            # 각 섹션의 첫페이지 
            p = 1 
            dat_list[[p + (i-1) * 10]] = get_table()
            
            # 마지막이 아닌 섹션
            if(i != pages) {
                
                max_p = 10
                
                for(p in 2:max_p) {
                    
                    selectPage = remDr$findElement('xpath', paste0('//*[@id="listContents"]/div[2]/input[@alt="', p + (i-1) * 10, '"]'))
                    selectPage$clickElement()
                    Sys.sleep(3)
                    
                    dat_list[[p + (i-1) * 10]] = get_table()
                    
                }
                
                
                # 10번째 이후 섹션 넘기기
                nextPage = remDr$findElement('xpath', '//*[@id="listContents"]/div[2]/input[@alt="다음"]')
                nextPage$clickElement()
                Sys.sleep(3)
                
            }
            
            
            # 마지막 섹션
            if(i == pages) {
                
                max_p = search_result$numofpages %% 10
                
                for(p in 2:max_p) {
                    
                    selectPage = remDr$findElement('xpath', paste0('//*[@id="listContents"]/div[2]/input[@alt="', p + (i-1) * 10, '"]'))
                    selectPage$clickElement()
                    Sys.sleep(3)
                    
                    dat_list[[p + (i-1) * 10]] = get_table()
                    
                }
                
            }
            
        }
        
        # list 결합 및 공백 제거
        dat_list %>%
            rbind_list(.) %>%
            filter(comp_name != '') ->> dat
        
    }
    
    # 1페이지 only
    
    if(numofpages == 1) {
        
        get_table() %>% 
            filter(comp_name != '') ->> dat
        
    }
    
    return(dat)
}


start_search()

do_loop(numofpages = search_result$numofpages)







