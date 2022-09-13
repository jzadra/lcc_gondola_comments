require(tidyverse)
require(pdftools)
#require(magick)
require(tidytext)
require(viridis)
require(wordcloud)
library(fs)


options(tibble.print_max = 60, tibble.print_min = 50) #if there are more than n rows, print only the first m rows. 

files <- dir_ls("pdfs", glob = "*.pdf")

raw_cm <- dir_map("pdfs", fun = pdf_text) 

#cm <- pdf_text("pdfs/LCC_FEIS_32B_Comments_00001-03086.pdf")

cm <- raw_cm %>% map(function(x) {
  unlist(x) %>% 
    enframe(name = "page", value = "comment") %>% 
    slice(-1)
}) %>% bind_rows()

parsed <- cm %>% 
  mutate(comment_num = str_extract(comment, "^COMMENT \\#:\\s*\\d{1,5}")) %>% 
  mutate(comment_num = str_remove(comment_num, "^COMMENT \\#:\\s*")) %>% 
  mutate(comment_num = as.numeric(comment_num)) %>% 
  #mutate(comment = str_remove_all(comment, "\\n\\n\\s{3}Sept 2022\\s*Page\\s\\d*[A-Z]?-\\d\\s*Little Cottonwood Canyon Final EIS\\n")) %>% 
  mutate(comment = str_remove_all(comment, "Sept 2022.*Little Cottonwood Canyon Final EIS")) %>% 
  fill(comment_num, .direction = "down") %>% 
  group_by(comment_num) %>% 
  summarize(comment = paste(comment, collapse = " ")) %>% 
  ungroup() %>% 
  separate(comment, into = c("cnum", "date", "source", "name", "text"), sep = "DATE:|\nSOURCE:|\nNAME:|\nCOMMENT:") %>% 
  mutate(across(everything(), ~str_squish(.))) %>% 
    select(-cnum)

codes <- parsed %>% 
  mutate(codes = str_extract_all(text, "\\(32\\.\\d*\\w\\.?\\d?\\w?\\)")) %>% 
  select(comment_num, date, source, codes) %>% 
  unnest(codes)

write_csv(parsed, "comments table.csv")
write_csv(codes, "comment codes.csv")
