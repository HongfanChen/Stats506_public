## Stats 506, F20
## Week 6 - activities
## Author(s): Hongfan Chen, chenhf@umich.edu
## Updated: October 13, 2020 - Last modified date

library(tidyverse)
# Part1 - Data Prep------------------------------------------------------------
demo = read_delim('./nhanes_demo.csv', delim = ',')
ohxden = read_delim('nhanes_ohxden.csv', delim =',' )
demodata = ohxden %>% 
  select(SEQN, OHDDESTS) %>% 
  left_join(demo, by = "SEQN") %>%
  select(id = SEQN, gender = RIAGENDR, age = RIDAGEYR, EDU = DMDEDUC2,
         exam_status = RIDSTATR, ohx_status = OHDDESTS) %>%
  mutate(under_20 = ifelse(age < 20, TRUE, FALSE),
         college = ifelse(under_20 == "TRUE" | !(EDU == 3 | EDU == 4),
                          "No college/<20","some college/college graduate"),
         ohx = ifelse(exam_status == 2 | ohx_status == 1, "complete", "mising")
  ) %>%
  filter(exam_status == 2)

# Part2 - Construct a table / marginal tests-----------------------------------
cap = paste0(
  '**Demotable.**'
)
demotable = demodata %>%
  select(under_20, gender, college) %>%
  summary() %>%
  knitr::kable( digits = 1, caption = cap, format = 'html' ) %>%
  kableExtra::kable_styling(full_width = TRUE)