
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

### 2. Code NAs (and rename trade_name -> brand_name)
orphan_inds <- 
  orphan_inds %>% 
  mutate(brand_name = replace(trade_name, trade_name == 'n/a', NA)) %>% 
  select(generic_name, brand_name, everything(), -trade_name)
  
## 3. Lower case all drug names
orphan_inds <- 
  orphan_inds %>% 
  mutate_at(.vars = vars(generic_name, brand_name), funs(tolower)) 

## 4. Replace 'and's with semicolons
orphan_inds <-
  orphan_inds %>% 
  mutate(brand_name =
           gsub(x = brand_name,
                pattern = ' and ',
                replace = '; ')) %>% 
  mutate(brand_name =
           gsub(x = brand_name,
                pattern = ', ',
                replace = '; ')) 

## Make manual changes
orphan_inds <-
  orphan_inds %>%
  mutate(
    generic_name = replace(generic_name, generic_name == 'dabrafenib and trametinib', 'trametinib and dabrafenib')
    ,brand_name   = replace(brand_name, generic_name == 'trametinib and dabrafenib', 'mekinist; tafinlar')
    ,brand_name   = replace(brand_name, brand_name == '1. myozyme 2. lumizyme', 'myozyme; lumizyme')
  ) %>% 
  arrange(brand_name) %>% 
  mutate(
    generic_name = replace(generic_name, brand_name == 'actimmune', 'interferon gamma-1b')
    ,generic_name = replace(generic_name, brand_name == 'bendeka', 'bendamustine for 50ml admixture')
    ,generic_name = replace(generic_name, brand_name == 'doxil', 'doxorubicin liposome')
    ,generic_name = replace(generic_name, brand_name == 'genotropin', 'somatropin')
    ,generic_name = replace(generic_name, brand_name == 'jakafi', 'ruxolitinib phosphate')
    ,generic_name = replace(generic_name, brand_name == 'gleevec', 'imatinib mesylate')
    ,generic_name = replace(generic_name, brand_name == 'humatrope', 'somatropin')
    ,generic_name = replace(generic_name, brand_name == 'norditropin', 'somatropin')
    ,generic_name = replace(generic_name, brand_name == 'novantrone', 'mitoxantrone')
    ,generic_name = replace(generic_name, brand_name == 'photofrin', 'porfimer sodium')
    ,generic_name = replace(generic_name, brand_name == 'photrexa viscous', 'riboflavin ophthalmic solution ultraviolet-a (uva) irradiation')
    ,generic_name = replace(generic_name, brand_name == 'salagen', 'pilocarpine')
    ,generic_name = replace(generic_name, brand_name == 'nutropin', 'somatropin')
    ,generic_name = replace(generic_name, brand_name == 'somatuline depot', 'lanreotide acetate')
    ,generic_name = replace(generic_name, brand_name == 'thyrogen', 'thyrotropin alfa')
    
    ,temp_id = 1:nrow(orphan_inds) 
    
  ) %>% 
  select(temp_id, everything())



## Determine which brand names link to multiple DISTINCT generic names
## There should be none if the manual changes worked
n_distinct_generic_name <-
  orphan_inds %>%
  dplyr::filter(!is.na(brand_name)) %>%
  group_by(brand_name) %>%
  distinct(generic_name) %>%
  count()

generic_multiples <- 
  orphan_inds %>% 
  left_join(n_distinct_generic_name, by = 'brand_name') %>% 
  select(temp_id, brand_name, everything()) %>% 
  dplyr::filter(n > 1)


## Assert no generic multiples
if(nrow(generic_multiples) > 0) View(generic_multiples)
assert <- function(x) stopifnot(x)
assert(nrow(generic_multiples) == 0)

## Print whether we were successful
if(nrow(generic_multiples) == 0) print('Hooray, no drug brand names with multiple generic names!')


## Export a csv to fill in blank brand names
dir.create('data/orphan_indications', showWarnings = F)
dir.create('data/temp', showWarnings = F)

orphan_inds %>% 
  select(temp_id, brand_name, generic_name, ends_with('date')) %>% 
  dplyr::filter(is.na(brand_name)) %>% 
  write_excel_csv('data/orphan_indications/brand_names_to_edit.csv')

## Save data to handoff to next document
save(list = 'orphan_inds', file = 'data/temp/1-clean_indications.RData')



