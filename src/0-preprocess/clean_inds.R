
# Clean orphan drug indication data

## Import orphan indications
orphan_inds <-
  read_excel('data/orphan_indications/orphan_indications_extract_01-18-18.xlsx') 


## Initial cleaning steps:
### 1. Rename variables with underscores rather than spaces
names(orphan_inds) <- 
  names(orphan_inds) %>% 
  tolower() %>% 
  gsub(x = ., pattern = ' ', replacement = '_')

### 2. Code NAs 
orphan_inds <- 
  orphan_inds %>% 
  mutate(trade_name = replace(trade_name, trade_name == 'n/a', NA)) 
  
## 2. Lower case all drug names
orphan_inds <- 
  orphan_inds %>% 
  mutate_at(.vars = vars(generic_name, trade_name), funs(tolower)) %>%
  arrange(trade_name) 


## Make manual changes
orphan_inds <-
  orphan_inds %>%
  mutate(
    
    generic_name = replace(generic_name, generic_name == 'dabrafenib and trametinib', 'trametinib and dabrafenib')
    
    ,trade_name   = replace(trade_name, generic_name == 'trametinib and dabrafenib', 'mekinist and tafinlar')
    
    ,generic_name = replace(generic_name, trade_name == 'actimmune', 'interferon gamma-1b')
    ,generic_name = replace(generic_name, trade_name == 'bendeka', 'bendamustine for 50ml admixture')
    ,generic_name = replace(generic_name, trade_name == 'doxil', 'doxorubicin liposome')
    ,generic_name = replace(generic_name, trade_name == 'genotropin', 'somatropin')
    ,generic_name = replace(generic_name, trade_name == 'jakafi', 'ruxolitinib phosphate')
    ,generic_name = replace(generic_name, trade_name == 'gleevec', 'imatinib mesylate')
    ,generic_name = replace(generic_name, trade_name == 'humatrope', 'somatropin')
    ,generic_name = replace(generic_name, trade_name == 'norditropin', 'somatropin')
    ,generic_name = replace(generic_name, trade_name == 'novantrone', 'mitoxantrone')
    ,generic_name = replace(generic_name, trade_name == 'photofrin', 'porfimer sodium')
    ,generic_name = replace(generic_name, trade_name == 'photrexa viscous', 'riboflavin ophthalmic solution ultraviolet-a (uva) irradiation')
    ,generic_name = replace(generic_name, trade_name == 'salagen', 'pilocarpine')
    ,generic_name = replace(generic_name, trade_name == 'nutropin', 'somatropin')
    ,generic_name = replace(generic_name, trade_name == 'somatuline depot', 'lanreotide acetate')
    ,generic_name = replace(generic_name, trade_name == 'thyrogen', 'thyrotropin alfa')
   
    
    ,ind_id = 1:nrow(orphan_inds) 
    
  ) %>% 
  select(ind_id, everything())



## Determine which trade names link to multiple DISTINCT generic names
## There should be none if the manual changes worked
n_distinct_generic_name <-
  orphan_inds %>%
  dplyr::filter(!is.na(trade_name)) %>%
  group_by(trade_name) %>%
  distinct(generic_name) %>%
  count()

generic_multiples <- 
  orphan_inds %>% 
  left_join(n_distinct_generic_name, by = 'trade_name') %>% 
  select(ind_id, trade_name, everything()) %>% 
  dplyr::filter(n > 1)


## Assert no generic multiples
if(nrow(generic_multiples) > 0) View(generic_multiples)
assert <- function(x) stopifnot(x)
assert(nrow(generic_multiples) == 0)

## Print whether we were successful
if(nrow(generic_multiples) == 0) print('Hooray, no drug brand names with multiple generic names!')


## Count distinct orphan drugs
## The IMS Report indicates that 449 drugs were approved as of February 2017.
orphan_inds %>% 
  dplyr::filter(mdy(marketing_approval_date) < '2017-03-01') %>% 
  select(trade_name) %>% 
  distinct() %>% 
  dplyr::filter(!is.na(trade_name)) %>% 
  count()

orphan_inds %>% 
  dplyr::filter(mdy(marketing_approval_date) < '2017-03-01') %>% 
  select(generic_name) %>% 
  distinct() %>%
  dplyr::filter(!is.na(generic_name)) %>% 
  count()


## Export a csv to perform manual edits
dir.create('data/orphan_indications', showWarnings = F)
write_csv(orphan_inds, 'data/orphan_indications/indications_to_edit_01-18-18.csv')


## Now import manual edits







