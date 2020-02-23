library(dplyr)
library(stringr)
library(tidyr)
library(tidyverse)
library(readr)

feb14 <- read_csv("feb14.csv")

working_set <- feb14 %>% slice(-1) 

#Replace QID mappings

for(search in QIDmapping$QID_time){
  searchval <- paste("QID",search, sep="")
  index <- which(search == QIDmapping$QID_time)
  working_set <- working_set %>% rename_at(vars(starts_with(searchval)), 
                                               funs(str_replace(., searchval, QIDmapping$Question[index])))
}

for(search in QIDmapping$QID_var){
  searchval <- paste("QID",search, sep="")
  index <- which(search == QIDmapping$QID_var)
  working_set <- working_set %>% rename_at(vars(starts_with(searchval)), 
                                               funs(str_replace(., searchval, QIDmapping$Question[index])))
}


#Remove submissions that used mouse and keyboard
working_set <- working_set %>% filter(N1_FIRST_CLICK == 0) 


#Get useful data
trunc_set <- working_set %>% dplyr::select(duration, finished,mTurkCode,QID2257_OS,QID2257_RESOLUTION,
         P1_PAGE_SUBMIT,P1_distance,P1_scrolls,
         P2_PAGE_SUBMIT,P2_distance,P2_scrolls,
         P3_PAGE_SUBMIT,P3_distance,P3_scrolls,
         condition, order,
         N1_PAGE_SUBMIT,N1_distance,N1_scrolls,
         N2_PAGE_SUBMIT,N2_distance,N2_scrolls,
         N3_PAGE_SUBMIT,N3_distance,N3_scrolls,
         N4_PAGE_SUBMIT,N4_distance,N4_scrolls,
         N5_PAGE_SUBMIT,N5_distance,N5_scrolls,
         N6_PAGE_SUBMIT,N6_distance,N6_scrolls,
         R1_PAGE_SUBMIT,R1_distance,R1_scrolls,
         R2_PAGE_SUBMIT,R2_distance,R2_scrolls,
         R3_PAGE_SUBMIT,R3_distance,R3_scrolls,
         R4_PAGE_SUBMIT,R4_distance,R4_scrolls,
         R5_PAGE_SUBMIT,R5_distance,R5_scrolls,
         R6_PAGE_SUBMIT,R6_distance,R6_scrolls,
         S1_PAGE_SUBMIT,S1_distance,S1_scrolls,
         S2_PAGE_SUBMIT,S2_distance,S2_scrolls,
         S3_PAGE_SUBMIT,S3_distance,S3_scrolls,
         S4_PAGE_SUBMIT,S4_distance,S4_scrolls,
         S5_PAGE_SUBMIT,S5_distance,S5_scrolls,
         S6_PAGE_SUBMIT,S6_distance,S6_scrolls
         )

#Validate with mTurk
merged_set <- merge(trunc_set, compiled_batches, by.x = "mTurkCode", by.y = "Answer.surveycode", all.x = TRUE)

#Remove submissions not strictly validated with mTurk
#trunc_set %>% filter(!(mTurkCode %in% merged_set$mTurkCode[is.na(merged_set$HITId)]))


#Get analysis set
analysis_set <- trunc_set %>% dplyr::select(mTurkCode, condition, order,
                                     N1_PAGE_SUBMIT,N1_distance,N1_scrolls,
                                     N2_PAGE_SUBMIT,N2_distance,N2_scrolls,
                                     N3_PAGE_SUBMIT,N3_distance,N3_scrolls,
                                     N4_PAGE_SUBMIT,N4_distance,N4_scrolls,
                                     N5_PAGE_SUBMIT,N5_distance,N5_scrolls,
                                     N6_PAGE_SUBMIT,N6_distance,N6_scrolls,
                                     R1_PAGE_SUBMIT,R1_distance,R1_scrolls,
                                     R2_PAGE_SUBMIT,R2_distance,R2_scrolls,
                                     R3_PAGE_SUBMIT,R3_distance,R3_scrolls,
                                     R4_PAGE_SUBMIT,R4_distance,R4_scrolls,
                                     R5_PAGE_SUBMIT,R5_distance,R5_scrolls,
                                     R6_PAGE_SUBMIT,R6_distance,R6_scrolls,
                                     S1_PAGE_SUBMIT,S1_distance,S1_scrolls,
                                     S2_PAGE_SUBMIT,S2_distance,S2_scrolls,
                                     S3_PAGE_SUBMIT,S3_distance,S3_scrolls,
                                     S4_PAGE_SUBMIT,S4_distance,S4_scrolls,
                                     S5_PAGE_SUBMIT,S5_distance,S5_scrolls,
                                     S6_PAGE_SUBMIT,S6_distance,S6_scrolls)



#Convert to long form
df <- analysis_set %>% 
  pivot_longer(cols = -(1:3), names_to = "measure", values_to ="dv") 
df <- df %>% mutate(condition = factor(condition, levels = c("npm","pm"), labels=c("No Metaphor","Metaphor") ),
                                texture = factor(substr(measure, 1, 1), levels = c("R","N","S"), labels = c("Rough", "Normal","Smooth")),
                                question = substr(measure, 1, 2),
                                measure = substring(measure, 4))
df <- df %>% spread(measure, dv) %>% mutate(distance = as.numeric(distance),
                                                        scrolls = as.numeric(scrolls),
                                                        PAGE_SUBMIT = as.numeric(PAGE_SUBMIT)) %>%
  dplyr::rename(RT = PAGE_SUBMIT, metaphor = condition)




#Get order mappings
order_working_set <- working_set %>% 
  dplyr::select(mTurkCode, FL_201_DO,FL_199_DO,FL_172_DO,FL_247_DO,FL_261_DO,FL_233_DO,FL_292_DO,
         FL_306_DO,FL_278_DO,FL_351_DO,FL_337_DO,FL_323_DO,FL_396_DO,FL_368_DO,
         FL_382_DO,FL_441_DO,FL_413_DO,FL_427_DO)

#Create order lookup table
order_set <- data.frame()
for(i in 1:nrow(order_working_set)){
  chunk <- order_working_set[i,] %>% dplyr::select_if(~sum(!is.na(.)) > 0) 
  chunk <- chunk[,order(as.character(chunk[1,]))]
  colnames(chunk) <- c("mTurkCode", "N_order","R_order","S_order")
  order_set <- rbind(order_set, chunk)
}
order_set <- order_set %>% separate(N_order,sep="\\|",into=c("1N","2N","3N","4N","5N","6N")) %>% 
  separate(R_order,sep="\\|",into=c("1R","2R","3R","4R","5R","6R")) %>%
  separate(S_order,sep="\\|",into=c("1S","2S","3S","4S","5S","6S")) %>% 
  mutate_if(is.character, str_replace_all, pattern = '([A-Z]).*(\\d)', replacement = '\\1\\2')

#Match order to trial
for (i in 1:nrow(df)) {
  matching_row <- order_set[as.character(order_set$mTurkCode) == as.character(df[i,1]), ]
  question_to_match <- as.character(df[i,"question"])
  df[i,"trial"] <- as.numeric(substr(colnames(matching_row)[which(matching_row == question_to_match)], 1,1))
}

saveRDS(df, file = "./cleaned.rds")

