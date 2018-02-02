# Calculate top selling orphan drugs using data from Optum

## Load indication data
orphan_drug_data <- read_csv('data/drugs/topselling/clean_names.csv')
orphan_drug_data <- 
  orphan_drug_data %>% 
  select(everything(), -brand_name, brand_name = brand_name_clean)

## Load Optum sales data
sales_data <- read_delim('data/sales/DRUG_SPEND_CENSORED_2018-02-02.CSV', delim = '|') 
sales_data <- 
  sales_data %>% 
  select(brand_name = BRAND_NAME, n_patients = N_PATIENTS, sales = COST_TOTAL) %>% 
  mutate(brand_name = tolower(brand_name))
  
## Total up sales by orphan drug ID
library(fuzzyjoin)

joined <- 
  sales_data %>% 
  regex_right_join(orphan_drug_data, by = c('brand_name' = 'brand_name')) 

total_sales <- 
  joined %>% 
  select(drug_id, brand_name = brand_name.y, sales) %>% 
  mutate(sales = coalesce(sales, 0)) %>% 
  group_by(drug_id, brand_name) %>% 
  summarize(sales_mil = sum(sales)/1000000) %>% 
  arrange(-sales_mil)

## Write total sales to file (of orphan drugs with non-orphan indications [OWNOI])
write_csv(total_sales, 'data/drugs/topselling/total_sales_ownoi.csv')


