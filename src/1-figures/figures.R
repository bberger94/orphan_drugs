# Attempt to replicate figures from IMS report

## Load packages
library(ggplot2)
library(ggthemes)

## Load cleaned data
load('data/indications/r2/r2_clean_inds.RData')

## Limit data to indications approved before 2017.
data <- inds %>% dplyr::filter(approval_date < '2017-01-01')


## Count orphans with non-orphan indications
## This figure is analagous to Figure 8 in IMS report
exhibit8_data <- 
  data %>%
    group_by(drug_id) %>% 
    summarize(orphan_only = min(orphan_indication)) %>% 
    mutate(
      orphan_only = ifelse(orphan_only == 1, 'Orphan Only', 'Orphan with non-orphan indication(s)')
    ) %>% 
    count(orphan_only)

exhibit8_data %>% 
    ggplot(aes('', y = n, fill = orphan_only)) +
      geom_bar(width = 1, stat = "identity") +
      coord_polar('y', start = 0, direction = -1) +
      scale_fill_manual(values = c('#00A1D0', '#E88939')) +
      ggtitle('Orphan Drug Approval by Approval Indication') +
      xlab(label = '') + ylab(label = '') + 
      guides(fill=guide_legend(title=NULL)) +
      theme_fivethirtyeight()
ggsave('figures/figure_8.png', width = 8.5, height = 10, scale = .8, dpi = 300)



## Further categorize orphans with non-orphan indications by sequence of approval
## This figure is analagous to Figure 9 in IMS report

exhibit9_data <- 
  data %>%
  distinct(drug_id) %>% 
  left_join(
    data %>%
      group_by(drug_id) %>% 
      dplyr::filter(approval_date == min(approval_date)) %>% 
      summarize(
        first_orphan = max(orphan_indication),
        first_nonorphan = max(1 - orphan_indication)
      ) 
  ) %>% 
  left_join(
    data %>%
      group_by(drug_id) %>% 
      summarize(
        orphan_only = min(orphan_indication)
      )
  ) %>% 
  mutate(
    approval_seq = ifelse(orphan_only == 1, 'Orphan Only',
                          ifelse(first_orphan == 1 & first_nonorphan == 1, 'Orphan/Non-Orphan Simultaneous',
                          ifelse(first_orphan == 1 & first_nonorphan == 0, 'Orphan Indication First',
                          ifelse(first_orphan == 0 & first_nonorphan == 1, 'Non-Orphan Indication First',
                                'unknown')        
                                 )))
  ) %>% 
  count(approval_seq)

exhibit9_data %>%
  ggplot(aes(x = reorder(approval_seq, c(2,3,4,1)), y = n)) +  
  geom_col(fill = '#18476d') + coord_flip() +
  geom_text(aes(label = n),  hjust = 1.5,  color="white", size=4) +
  ggtitle(label = 'Number of Orphan Drugs Stratified by Approval Sequence') +
  xlab(label = '') + ylab(label = '') +
  theme_fivethirtyeight()
ggsave('figures/figure_9.png', width = 13, height = 7.5, scale = .8, dpi = 300)




    