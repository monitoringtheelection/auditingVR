## Setup libraries =============================================================
library(tidyverse)
library(voterdiffR)

## Initialize blocks and summary dataframe =====================================
init <- block_init(
  varlist, 
  firstname = "szNameFirst", 
  gender = "gender",
  address_partial = c("sHouseNum", "szStreetName"),
  address_full = "szSitusAddress",
  exceptions = list(c("gender", "szNameLast"))
)

## Load the initial dataframe and assess reduction ratio =======================
### We can ignore warning on rowwise data structure being stripped
load(paste0(path_clean, clean_prefix, date_df[1, ]$date_label, ".Rda"))
prematch_output <- suppressWarnings(
  block_prematch(df, init$blocks, init$summ_df)
)
summ_df <- prematch_output$summ_df %>%
  ### If we are using phones or emails, always block by gender
  ### Because families tend to share them
  filter(
    comparison < (nrow(df) * threshold_prop) &
      !(gender == 0 & szNameFirst == 0 & szPhone == 1) & 
      !(gender == 0 & szNameFirst == 0 & szEmailAddress == 1)
  ) %>%
  ### Blocks with no pairs have no use
  filter(comparison > 0) %>% 
  mutate(row = row_number()) %>%
  select(row, everything())

## Prepare to match block-by-block =============================================
fastprep_output <- block_fastprep(df, prematch_output, summ_df)
assert_that(
  sum(
    fastprep_output$fastprep %>% 
      map(~ nrow(inner_join(.x$mismatch_A, .x$mismatch_B))) %>%
      unlist()
  ) == 0
)

date_df <- snapshot_list()
dups <- list(
  by_snapshot = vrdedup(
    threshold = cutoff,
    summ_final = summ_final,
    date_df = date_df,
    varlist = vl_dedup, 
    path_clean = path_clean,
    path_dedup = path_dedup,
    id_var = vl$id[1],
    dedup_prefix = dedup_prefix,
    clean_prefix = clean_prefix,
    extra_vars = "gender",
    exist_files = TRUE
  )
)

dups$by_type <- dups_type(
  dups$by_snapshot, date_df, path_clean, path_dedup,
  clean_prefix, id_var = vl$id[1]
)
save(dups_final, file = paste0(path_dedup, "dups_type.Rda"))
