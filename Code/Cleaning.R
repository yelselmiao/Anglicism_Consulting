# This R script is used for pre-cleaning 

pre_clean <- function(df) {
  data <- raw_data %>%
    dplyr::select(c(2:36, 53, 55, 57, 60, 61, 68)) %>%
    rename(
      p1_1 = Q4_1,
      # part 1: 15 sentences
      p1_2 = Q5_1,
      p1_3 = Q6_1,
      p1_4 = Q7_1,
      p1_5 = Q8_1,
      p1_6 = Q9_1,
      p1_7 = Q10_1,
      p1_8 = Q11_1,
      p1_9 = Q12_1,
      p1_10 = Q13_1,
      p1_11 = Q14_1,
      p1_12 = Q15_1,
      p1_13 = Q16_1,
      p1_14 = Q17_1,
      p1_15 = Q18_1,
      # part 2: 10 questions
      p2_1_choice = Q20,
      p2_1_other = Q20_3_TEXT,
      p2_2_choice = Q21,
      p2_2_other = Q21_3_TEXT,
      p2_3_choice = Q22,
      p2_3_other = Q22_3_TEXT,
      p2_4_choice = Q23,
      p2_4_other = Q23_3_TEXT,
      p2_5_choice = Q24,
      p2_5_other = Q24_3_TEXT,
      p2_6_choice = Q25,
      p2_6_other = Q25_3_TEXT,
      p2_7_choice = Q26,
      p2_7_other = Q26_3_TEXT,
      p2_8_choice = Q27,
      p2_8_other = Q27_3_TEXT,
      p2_9_choice = Q28,
      p2_9_other = Q28_3_TEXT,
      p2_10_choice = Q29,
      p2_10_other = Q29_3_TEXT,
      gender = Q45,
      age = Q46,
      grew_up = `...57`,
      live = `...60`,
      edu_level = Q50,
      eng_pro = Q51
    ) %>%
    dplyr::select(-c(seq(17, 35, 2))) %>%
    mutate_at(c(1:15, 27), as.numeric) %>%
    mutate_if(is.character, as.factor)
  
  return(data)
}


choice_sorter <- function(inno_use, trad_use, col, new_col_name) {
  sorted_ans <- col %>%
    as.data.frame() %>%
    mutate(sorted = case_when(
      (str_detect(col, inno_use) &
         !str_detect(col, trad_use)) ~ 'innovative',
      (str_detect(col, trad_use) &
         !str_detect(col, inno_use)) ~ 'traditional',
      (str_detect(col, trad_use) &
         str_detect(col, inno_use)) ~ 'both',
      (!str_detect(col, trad_use) &
         !str_detect(col, inno_use)) ~ 'neither'
    ))
  sorted_ans <- sorted_ans %>%
    select(sorted)
  names(sorted_ans)[names(sorted_ans) == 'sorted'] <- new_col_name
  return(sorted_ans)
}


inno_encoder <- function(df) {
  df[df == 'traditional'] = 0
  df[df == 'neither'] = 0
  df[df == 'both'] = 1
  df[df == 'innovative'] = 1
  
  return(df)
}







