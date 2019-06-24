library(dplyr)
library(stringr)
library(httr)
library(rvest)
library(jsonlite)


# searching API

# ADDRESS


### GET general company information
comp_info = function(crp_cd) {
    
    company_request = GET(url = "http://dart.fss.or.kr/api/company.json?",
                          query = list(
                              auth = "ada75db1ec2ec6363f9661dac9cd23ba28199415",
                              crp_cd = crp_cd
                          ))
    
    company_request %>% 
        content(., as = 'text', encoding = 'UTF-8') %>%
        fromJSON(., flatten = TRUE) %>% 
        data.frame(row.names = c('value')) %>% 
        t()
    
}


### GET the list of submitted reports from 2018


get_rcp_no = function(crp_cd) {
    
    
    # API 요청
    search_request = GET(url = "http://dart.fss.or.kr/api/search.json?",
                         query = list(
                             auth = "ada75db1ec2ec6363f9661dac9cd23ba28199415",
                             crp_cd = crp_cd,
                             start_dt = '20180101'
                         ))
    
    # 요청 결과
    search_request %>% 
        content() %>%
        fromJSON() -> json_result
    
    report_list = json_result$list
    
    
    # 사업보고서 혹은 (연결)감사보고서 
    res = report_list %>% 
        filter(str_detect(rpt_nm, '사업보고서|감사보고서')) %>%
        select(rcp_no, rpt_nm) 
    
    
    # 해당 보고서가 존재하지 않는 경우
    
    # 우선순위: 사업보고서 > 연결감사보고서 > 감사보고서
    
    if(res$rpt_nm %>% str_detect('사업보고서')) {
        
        res %<>% filter(str_detect(rpt_nm, '사업보고서')) %>% slice(1)
        
    } 
    
    else if(res$rpt_nm %>%str_detect('연결감사보고서')) {
        
        res %<>% filter(str_detect(rpt_nm, '연결감사보고서')) %>% slice(1)
        
    } 
    
    else if(res$rpt_nm %>% str_detect('감사보고서')) {
        
        res %<>% filter(str_detect(rpt_nm, '감사보고서')) %>% slice(1)
        
    }
    
    else {
        
        
    }
    
    
    
}









##### SEARCH input variables #####

# crp_cd : 6 or 8 digits of company
# end_dt : date submitted
# start_dt : 
# fin_rpt : find the latest report (Y or N, deault = N)
# dsp_tp : 정기공시(A), 주요사항보고(B), 발행공시(C), 지분공시(D), 기타공시(E)
#           외부감사관련(F), 펀드공시(G), 자산유동화(H), 거래소공시(I), 공정위공시(J)
# bsn_tp : 정기공시(5개), 주요사항보고(3개), 발행공시(11개) 


# ......

##########################


# URL generation
crp_cd <- "00185912" 
dsp_tp <- "A"


search_call <-paste(search_url, "auth=", authKey, "&crp_cd=", crp_cd, "&start_dt=20190101", "&bsn_tp=F001", sep = "")
search_resp <- fromJSON(search_call)


rcp_number <- search_resp$list$rcp_no

report_url = paste('http://dart.fss.or.kr/dsaf001/main.do?rcpNo=', rcp_number, sep = "")


report <- GET(report_url)

req <- read_html(report) %>% html_node(xpath = '//*[@id="north"]/div[2]/ul/li[1]/a')

req %<>% html_attr('onclick') %>% stringr::str_split(., ' ')
dcm_number <- req[[1]][2] %>% readr::parse_number()


query_base <- list(
  rcp_no = rcp_number,
  dcm_no = dcm_number
)
down_excel <- POST('http://dart.fss.or.kr/pdf/download/excel.do',
                   query = query_base)

writeBin(content(down_excel, "raw"),
         paste0(crp_cd, "_", rcp_number[1], "xls"))



df = readxl::read_excel(paste0(crp_cd, "_", rcp_number[1], "xls"), sheet = 2)




#################




rcp_number = "20190313000081"

report_url = paste0('http://dart.fss.or.kr/dsaf001/main.do?rcpNo=', rcp_number)
report <- GET(report_url)

report %>% read_html() %>%
  html_nodes('#ifrm') %>% html_attr('table')


report %>% read_html() %>%
  html_nodes(xpath = '//*[@id="ifrm"]') %>% html_attr('src')




####################




