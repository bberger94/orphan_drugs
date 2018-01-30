
# Export orphan indications to csv for a 2nd round of manual edits on google sheets

## Load cleaned data from file if not in memory
load('data/orphan_indications/r1/temp/2-clean_indications.RData')

## Recode orphan as binary and rename variables
inds_to_export <- 
  orphan_inds %>% 
    mutate(
      orphan_indication = as.numeric(orphan_drug_status == 'Designated/Approved')
    ) %>% 
    select(
      drug_id,
      ind_id,
      brand_name,
      generic_name,
      indication = approved_indication,
      approval_date = marketing_approval_date,
      orphan_indication,
      orphan_designation_date = designation_date,
      orphan_designation = designation,
      orphan_exclusivity_end_date = exclusivity_end_date
    ) 
  
## Write to csv
write_csv(inds_to_export, 'data/orphan_indications/r1/cleaned_orphan_indications_01-21-18.csv')


