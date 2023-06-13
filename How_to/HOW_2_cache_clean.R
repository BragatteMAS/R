## Libraries
pacman::p_load(vroom, tidyverse, janitor, stringr,lubridate)

## read data
# cache20 <- vroom("data/comb/combined_cache_20.tsv")
# cacheFleury <- vroom("data/comb/combined_cache_fleury.tsv")
# cache20new <-  vroom("data/comb/combined_cache_new.tsv")
# combined <- vroom("data/combined.tsv")
combined_cache <- vroom("data/combined_cache.tsv")
combined_cache_clean <- vroom("data/combined_cache.tsv")

## merge data
files <-
    list.files(path = "data/",
               pattern = "*.tsv",
               full.names = TRUE)

## replace NA for NT in _test_result columns
data <- files %>%
    map_df(~ read_tsv(.)) %>%
    mutate_at(vars(ends_with("_test_result")), replace_na, "NT")

## adjusts
### column patient_id
data <- data %>%
    relocate(patient_id, .before = 4)

## unify files *.tsv
files <-
    list.files(path = "data/",
               pattern = "*.tsv",
               full.names = TRUE)

## replace...
data <- data %>%
    mutate_all( ~ str_replace_all(
        .,
        c(
            "Not detected" = "ND",
            "Detected" = "Det",
            "Positive" = "Pos",
            "Negative" = "Neg"
        )
    ))

## Data column geneS change key values
combined_cache <- combined_cache %>%
    mutate(geneS_detection = str_replace_all(geneS_detection, c("Pos" = "Det",
                                                                "Neg" = "ND")))

combined_cache <- combined_cache %>%
    mutate(lab_id = replace(lab_id, lab_id == "DB Molecular", "DB Mol"))


## Deduplicate
### Identify duplicated lines
dupes <- combined_cache %>%
    get_dupes()
### Remove duplicated lines
combined_cache <- combined_cache %>%
    anti_join(dupes, by = colnames(combined_cache))

### Verify duplicated rows in column sample_id
duplicated_rows <-
    combined_cache[duplicated(combined_cache$sample_id),]
duplicated_ids <-
    combined_cache$sample_id[duplicated(combined_cache$sample_id)]

### reducce 40 to 16 character in column sample_id
combined_cache16 <- combined_cache %>%
    mutate(sample_id = substr(sample_id, 1, 16))

### number of rows without 16 characters
sum(nchar(combined_cache$sample_id) != 16, na.rm = TRUE)
sum(nchar(combined_cache16$sample_id) != 16, na.rm = TRUE)

### Identify duplicated lines
dupes16 <- combined_cache16 %>%
    get_dupes()
### Remove duplicated lines
combined_cache_clean <- combined_cache16 %>%
    anti_join(dupes16, by = colnames(combined_cache16))

### only columns with "_test_result" add NT, considering the column test_kit in rows with the word "test", exceto a coluna SC2_test_result
combined_cache <- combined_cache_clean %>%
    mutate(across(-SC2_test_result & ends_with("_test_result"), 
                  ~ ifelse(str_detect(test_kit, "^test_"), ifelse(is.na(.) | . == "", "NT", .), ""),
                  .names = "{.col}"))

## verify errors DB_Mol
filtered_data <- combined_cache %>%
    filter(!(SC2_test_result %in% c("NT", "Pos", "Neg")))
### diff labs errors
distinct_lab_ids <- filtered_data %>%
distinct(lab_id)
### labs and results
unique_values <- filtered_data %>%
    group_by(lab_id) %>%
    summarise(unique_SC2_test_result = list(unique(SC2_test_result)))
unique_values <- unique_values %>%
    tidyr::unnest(unique_SC2_test_result)

### replace errors
combined_cache <- combined_cache %>%
    mutate(SC2_test_result = case_when(
        SC2_test_result == "NÃ£o detectado" ~ "Neg",
        SC2_test_result == "NÃ£o Detectado" ~ "Neg",
        SC2_test_result == "NÃÂÃÂÃÂÃÂ£o Detectado" ~ "Neg",
        SC2_test_result == "Detectado" ~ "Pos",
        SC2_test_result == "Det" ~ "Pos",
        SC2_test_result == "Not tested" ~ "NT",
        is.na(SC2_test_result) ~ "NT",
        TRUE ~ SC2_test_result
    ))

## remove columns in state_code com IG
combined_cache <- combined_cache %>%
    filter(state_code != "IG")

## last date labs
last_date <- combined_cache %>%
    filter(lab_id == "FLEURY") %>%
    summarise(last_epiweek = max(epiweek, na.rm = TRUE))
## wrong date
future_dates_df <- combined_cache %>%
    filter(epiweek > Sys.Date())
## correct date
# combined_cache <- combined_cache %>%
#     mutate(epiweek = ifelse(epiweek == as.Date("2023-06-03"), ymd("2023-03-06"), epiweek))


## Save .tsv
write_tsv(combined_cache, "data/combined_cache_clean.tsv", na = "")
