#======================================================================
# example 1 : download.file
#=======================================================================

# remove old environment
rm(list = ls())

# load package

library(rvest)
library(dplyr)
library(stringr)
library(stringi)

# link url

url<-"https://www.intercambioidiomasonline.com/english/multiple-choice-cloze/"

# load all link

url %>% 
  read_html() %>% 
  html_nodes("a") %>% 
  html_attr("href") -> all_links

# keep only useful links

all_links[str_detect(all_links, pattern = "upload")] %>% 
  matrix(ncol = 1) %>% 
  as.data.frame()-> multiple_choice_links

n <- str_length("https://www.intercambioidiomasonline.com/wp-content/uploads/2020/05/")

# add file name and destination
multiple_choice_links %>% 
  rename("Urls" = V1) %>% 
  mutate(Name_file = str_sub(Urls, start = n + 1, end = -1)) %>% 
  mutate(Destinations = str_c("./multiple_choice/", Name_file))-> df_links

write.csv2(df_links,"./multiple_choice_links.csv")

# df_links %>%
#   Map(function(u, d) download.file(u, d, mode="wb"), Urls, Destinations)
  
# download files  
for (i in 1:45) {
  download.file(df_links$Urls[i], df_links$Destinations[i], mode = "wb")
}

#============================================================================
# Example2. Phrasal.verb
#============================================================================

rm(list = ls())


url <- "https://www.englishrevealed.co.uk/advanced_vocabulary.php"  

url %>% 
  read_html() %>% 
  html_element("body > div.container.wrapper > div.content.content-padding.sector-grey > div:nth-child(4) > div:nth-child(2) > div > div.panel-body") %>% 
  html_nodes("a") %>% 
  html_attr("href") %>% 
  matrix(ncol = 1) %>% 
  as.data.frame() %>% 
  mutate(V1 = str_c("https://www.englishrevealed.co.uk/",V1))-> all_links2

index1 <- c()
index2 <- c()

for (i in 1:29) {
  all_links2$V1[i] %>% 
    read_html() %>% 
    html_elements("#change") %>% 
    html_text() -> qs 
  if (length(qs) != 0){
     append(index1,i) -> index1}
    else {append(index2,i)->index2}
} 

df_space <- data.frame()

for (i in index1) {
  all_links2$V1[i] %>% 
    read_html() %>% 
    html_elements("#description > div > div > table") %>% 
    html_table() %>% 
    .[[1]] -> table_i
  bind_rows(df_space, table_i)-> df_space
}

for (i in index2) {
  all_links2$V1[i] %>% 
    read_html() %>% 
    html_elements("#login-modal > div > div > table") %>% 
    html_table() %>% 
    .[[1]] -> table_i
  bind_rows(df_space, table_i)-> df_space
}

write.csv2(df_space,"./Phrasal_verbs_idioms.csv")
df_space %>% 
  mutate(X3 = case_when(is.na(X3) ~ X1, TRUE ~ X3)) %>% 
  select(2,3) %>% 
  arrange(X2)-> phrasal_verb
names(phrasal_verb) <- c("Phrasal_verb", "Meaning")

write.csv2(phrasal_verb, "./Phrasal_verbs.csv") 
 
#==========================================================================================


url <- "https://vietnamnet.vn/bien-dong-diem-chuan-dai-hoc-kinh-te-quoc-dan-4-nam-qua-2042615.html"
url %>% 
  read_html() %>% 
  html_elements("#maincontent > div > div:nth-child(8) > table > tbody") %>% 
  html_table(header = TRUE) %>% 
  .[[1]] -> table_dc

write.csv2(table_dc,"./diemchuanKTQD.csv")

names(table_dc) <- c("STT","cod", "name", "combine", "grade_2021", "grade_2020",
                     "grade_2019","grade_2018") 
table_dc %>% 
  filter(!str_detect(cod,"EP")) %>% 
  filter(!str_detect(grade_2018,"-")&!str_detect(grade_2019,"-")) %>% 
  mutate(avg = (as.numeric(grade_2021) + as.numeric(grade_2020) + as.numeric(grade_2019) + as.numeric(grade_2018))/4)-> table_dc2

#==================================================================================
# example 3: papers about neu
#==================================================================================
rm(list = ls())


css_field <- "body > div.w-1140.px-20.pt-20.bg-white.mx-auto > div:nth-child(7) > div.w-760.mr-40 > div.breadcrumb-box.flex-wrap > div.breadcrumb-box__link > p > a"

css_date <- "body > div.w-1140.px-20.pt-20.bg-white.mx-auto > div:nth-child(7) > div.w-760.mr-40 > div.breadcrumb-box.flex-wrap > div.breadcrumb-box__time"
 
css_title <- "body > div.w-1140.px-20.pt-20.bg-white.mx-auto > div:nth-child(7) > div.w-760.mr-40 > div.newsFeatureBox > div.newsFeature__header > h1"

css_main <-"body > div.w-1140.px-20.pt-20.bg-white.mx-auto > div:nth-child(7) > div.w-760.mr-40 > div.newsFeatureBox > div.newsFeature__main > div.newFeature__main-textBold"  

read_infor <-function(A,B){
    A %>% 
      read_html() %>% 
      html_element(B) %>% 
      html_text() %>% 
      str_remove_all("  ") -> content
    return(content)
  }

url <- "https://vietnamnet.vn/tim-kiem-p1?q=kinh%20t%E1%BA%BF%20qu%E1%BB%91c%20d%C3%A2n"
url_part1 <- "https://vietnamnet.vn/tim-kiem-p"
url_part2 <- "?q=kinh%20t%E1%BA%BF%20qu%E1%BB%91c%20d%C3%A2n"

page_links <- data.frame(str_c(url_part1, 1:5, url_part2))
css_page <-"body > div.w-1140.px-20.pb-20.pt-30.bg-white.mx-auto.main-content > div > div.mt-30.mb-20.main-result"

get_links <- function(n,css){
  page_links <-str_c(url_part1, n, url_part2)
  page_links %>% 
    read_html() %>% 
    html_element(css_page) %>% 
    html_nodes("a") %>% 
    html_attr("href") %>% 
    matrix(ncol = 1) %>% 
    as.data.frame() %>% 
    filter(str_detect(V1,"https")) %>% 
    distinct() -> ktqd_links 
  return(ktqd_links)
}



df_links <- data.frame()
for (i in 1:5) {
  get_links(i, css_page)-> df_links1
  bind_rows(df_links, df_links1) -> df_links
}

paper <- data.frame()

for (i in 1:100) {
    data.frame(Date = read_infor(df_links$V1[i], css_date),
               Field = read_infor(df_links$V1[i], css_field), 
               Title = read_infor(df_links$V1[i], css_title), 
               Main_content = read_infor(df_links$V1[i], css_main), 
               URL = df_links$V1[i]) -> KTQD
    bind_rows(paper, KTQD) -> paper
}

write.csv2(paper, "./paper_ktqd.csv")

paper %>% 
  mutate(Date = str_sub(Date,start = 4, end = 13)) %>%
  mutate(URL = df_links$V1) %>% 
  filter(! is.na(Date)) %>% 
  distinct() %>% 
  mutate(Date = as.Date(Date,format = "%d/%m/%Y")) %>% 
  arrange(-Date)-> paper_date_edit 

write.csv2(paper_date_edit,"./paper_ktqd_arrange.csv")

