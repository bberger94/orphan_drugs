
# Clean orphan drug indication data


## Import orphan indications
orphan_inds <-
  read_excel('data/orphan_indications/orphan_indications_extract_01-18-18.xlsx') 

## Initial cleaning step:
## 1. Code NAs
## 2. Lower case all drug names
orphan_inds <- 
  orphan_inds %>% 
  mutate(`Trade Name` = replace(`Trade Name`, `Trade Name` == 'n/a', NA)) %>% 
  mutate_at(.vars = vars(`Generic Name`, `Trade Name`), funs(tolower)) %>%
  arrange(`Trade Name`) 


## Make manual changes
orphan_inds <-
  orphan_inds %>%
  mutate(
    
    `Generic Name` = replace(`Generic Name`, `Generic Name` == "dabrafenib and trametinib", "trametinib and dabrafenib")
    
    ,`Trade Name`   = replace(`Trade Name`, `Generic Name` == "trametinib and dabrafenib", "mekinist and tafinlar")
    
    ,`Generic Name` = replace(`Generic Name`, `Trade Name` == "actimmune", "interferon gamma-1b")
    ,`Generic Name` = replace(`Generic Name`, `Trade Name` == "bendeka", "bendamustine for 50ml admixture")
    ,`Generic Name` = replace(`Generic Name`, `Trade Name` == "doxil", "doxorubicin liposome")
    ,`Generic Name` = replace(`Generic Name`, `Trade Name` == "genotropin", "somatropin")
    ,`Generic Name` = replace(`Generic Name`, `Trade Name` == "jakafi", "ruxolitinib phosphate")
    ,`Generic Name` = replace(`Generic Name`, `Trade Name` == "gleevec", "imatinib mesylate")
    ,`Generic Name` = replace(`Generic Name`, `Trade Name` == "humatrope", "somatropin")
    ,`Generic Name` = replace(`Generic Name`, `Trade Name` == "norditropin", "somatropin")
    ,`Generic Name` = replace(`Generic Name`, `Trade Name` == "novantrone", "mitoxantrone")
    ,`Generic Name` = replace(`Generic Name`, `Trade Name` == "photofrin", "porfimer sodium")
    ,`Generic Name` = replace(`Generic Name`, `Trade Name` == "photrexa viscous", "riboflavin ophthalmic solution ultraviolet-a (uva) irradiation")
    ,`Generic Name` = replace(`Generic Name`, `Trade Name` == "salagen", "pilocarpine")
    ,`Generic Name` = replace(`Generic Name`, `Trade Name` == "nutropin", "somatropin")
    ,`Generic Name` = replace(`Generic Name`, `Trade Name` == "somatuline depot", "lanreotide acetate")
    ,`Generic Name` = replace(`Generic Name`, `Trade Name` == "thyrogen", "thyrotropin alfa")
   
    
    ,ind_id = 1:nrow(orphan_inds) 
    
  ) %>% 
  select(ind_id, everything())



## Determine which trade names link to multiple DISTINCT generic names
## There should be none if the manual changes worked
n_distinct_generic_name <-
  orphan_inds %>%
  filter(!is.na(`Trade Name`)) %>%
  group_by(`Trade Name`) %>%
  distinct(`Generic Name`) %>%
  count()

generic_multiples <- 
  orphan_inds %>% 
  left_join(n_distinct_generic_name, by = "Trade Name") %>% 
  select(`Trade Name`, everything()) %>% 
  filter(n > 1)


## Assert no generic multiples
if(nrow(generic_multiples) > 0) View(generic_multiples)
assert <- function(x) stopifnot(x)
assert(nrow(generic_multiples) == 0)

## Print whether we were successful
if(nrow(generic_multiples) == 0) print("Hooray, no drug brand names with multiple generic names!")


## Count distinct orphan drugs
## The IMS Report indicates that 449 drugs were approved as of February 2017.
orphan_inds %>% 
  filter(mdy(`Marketing Approval Date`) < '2017-03-01') %>% 
  select(`Trade Name`) %>% 
  distinct() %>% 
  filter(!is.na(`Trade Name`)) %>% 
  count()

orphan_inds %>% 
  filter(mdy(`Marketing Approval Date`) < '2017-03-01') %>% 
  select(`Generic Name`) %>% 
  distinct() %>%
  filter(!is.na(`Generic Name`)) %>% 
  count()


## Export a csv to perform manual edits
dir.create('data/orphan_indications', showWarnings = F)
write_csv(orphan_inds, 'data/orphan_indications/indications_to_edit_01-18-18.csv')


## Now import manual edits







