# Count non-orphan indications for each orphan drug

## After writing this data to file, 
## export it to the OLDW, clean the brand names, 
## and calculate total spend for each drug w/ a nonorphan ind

## Load indication data
load('data/orphan_indications/r2/r2_clean_inds.RData')

## Limit data to indications approved before 2017.
ind_data <- 
  inds %>% 
  dplyr::filter(!is.na(approval_date)) %>% 
  dplyr::filter(approval_date < '2017-01-01')

## Identify which orphan drugs have nonorphan inds (and how many)
drug_data <- 
  ind_data %>%
  group_by(drug_id, brand_name, generic_name) %>% 
  summarize(
    n_orphan_ind = sum(orphan_indication),
    n_nonorphan_ind = sum(1-orphan_indication),
    n_ind = n()
  ) %>% 
  left_join(
    ind_data %>%
      group_by(drug_id) %>% 
      summarize(first_approval_date = min(approval_date))
  ) %>% 
  arrange(drug_id)

## Write to file
output_dir <- 'data/drugs/topselling'
dir.create(output_dir, showWarnings = F)
write_csv(
  drug_data, 
  path = paste0(output_dir,'/orphan_names.csv')
  )