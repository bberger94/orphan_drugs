# Load and clean data from second round of manual edits

## Load data
inds <- read_excel('data/orphan_indications/r2/clean_inds_all_01-30-18.xlsx', sheet = 1)

## Clean dates
inds <- 
  inds %>% 
  mutate(
    approval_date = date(approval_date),
    orphan_designation_date = date(orphan_designation_date),
    orphan_exclusivity_end_date = date(orphan_exclusivity_end_date)
    ) 

## Reassign IDs to drugs and indications
inds <- 
  inds %>% 
  select(-drug_id, -ind_id) %>% 
  left_join(
    inds %>% 
      distinct(brand_name) %>% 
      arrange(brand_name) %>% 
      mutate(drug_id = 1:n())
    ,by = 'brand_name'
  ) %>% 
  arrange(brand_name, approval_date) %>% 
  mutate(ind_id = 1:n()) %>% 
  select(drug_id, ind_id, everything())

## Write to file
save(inds, file = 'data/orphan_indications/r2/r2_clean_inds.RData')
  