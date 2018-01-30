
# Merge manual edits performed after first cleaning script

## Load cleaned data from file if not in memory
load('data/orphan_indications/r1/temp/1-clean_indications.RData')

## Load manual brand name edits
brand_name_edits <- read_csv('data/orphan_indications/r1/brand_names_edits_01-19-18.csv')

## Merge name edits into main indication data
orphan_inds <-
  orphan_inds %>% 
  left_join(
    brand_name_edits %>% 
      select(temp_id, brand_name)
    ,by = 'temp_id'
    ) %>%
  mutate(brand_name =
           replace(brand_name.x,
                   is.na(brand_name.x),
                   brand_name.y[is.na(brand_name.x)]
                   )
         ) %>% 
  select(temp_id, brand_name, everything(),
         -brand_name.x, -brand_name.y, -temp_id
         ) 

## Recode variables
orphan_inds <-
  orphan_inds %>%
  arrange(brand_name, designation_date) %>% 
  mutate(
    ind_id                   = 1:n()
    ,designation_date         = ymd(designation_date)
    ,marketing_approval_date = mdy(marketing_approval_date)
    ,exclusivity_end_date    = mdy(exclusivity_end_date, quiet = TRUE) #NAs in this column
  ) %>% 
  left_join(
    orphan_inds %>% 
      select(brand_name) %>% 
      distinct() %>% 
      arrange(brand_name) %>% 
      mutate(drug_id = 1:n())
    , by = 'brand_name'
  ) %>% 
  select(ind_id, drug_id, everything())

## Count distinct orphan drugs
## The IMS Report indicates that 449 drugs were approved as of February 2017.
orphan_inds %>% 
  dplyr::filter(marketing_approval_date < '2017-03-01') %>% 
  select(brand_name) %>% 
  distinct() %>% 
  nrow()


## Save data to handoff to next document
save(list = 'orphan_inds', file = 'data/orphan_indications/r1/temp/2-clean_indications.RData')


