library(tidyverse)
library(dplyr)
library(tidyr)
library(purrr)
library(data.table)
library(readxl)
library(readr)
library(stringr)
library(bit64)
library(ggplot2)
library(tibble)
library(eulerr)
library(ggforce)
library(countrycode)
options(scipen = 999)


# OpenAlex march 2025 version upload
openalex_journals <- read.csv("~/Desktop/Local.Journals/local_journals_OA2503_journals_functions.csv")

# data mining
openalex_journals$issn <- gsub(",", "; ", openalex_journals$issn)
openalex_journals$issn <- gsub("\\[|\\]", "", openalex_journals$issn)
openalex_journals$issn <- gsub('"', "", openalex_journals$issn)

# remove duplicates and store second issn code in issn_l 
openalex_journals <- openalex_journals %>% mutate(issn_cleaned = str_split(issn, "; "),
                                           issn_l = if_else(issn_l %in% unlist(issn_cleaned), NA_character_, issn_l)) %>%
                                           select(-issn_cleaned)

openalex_journals <- openalex_journals %>% mutate(issn_split = str_split(issn, "; "),
                                           issn_l = if_else(lengths(issn_split) > 1, map_chr(issn_split, ~ .x[2]), issn_l),
                                           issn = map_chr(issn_split, ~ .x[1])) %>%
                                           select(-issn_split)

# replace the subfield, field and domain codes with the corresponding tags from OpenAlex topics
openalex_topics <- readxl::read_excel("~/Desktop/Local.Journals/OAtopics.xlsx")
subfield_lookup <- openalex_topics %>% select(subfield_id, subfield_name) %>% distinct()
field_lookup <- openalex_topics %>% select(field_id, field_name) %>% distinct()
domain_lookup <- openalex_topics %>% select(domain_id, domain_name) %>% distinct()

replace_codes_with_tags <- function(code_column, lookup_table, code_name, tag_name) {
                           sapply(code_column, function(codes) {
                           code_list <- strsplit(codes, "; ")[[1]]
                           tags <- sapply(code_list, function(code) {
                           match_code <- lookup_table %>% filter(!!sym(code_name) == code)
                           if (nrow(match_code) > 0) match_code[[tag_name]] else NA})
                           paste(tags, collapse = "; ")})}

openalex_journals$subfield <- replace_codes_with_tags(openalex_journals$subfield, subfield_lookup, "subfield_id", "subfield_name")
openalex_journals$field <- replace_codes_with_tags(openalex_journals$field, field_lookup, "field_id", "field_name")
openalex_journals$domain <- replace_codes_with_tags(openalex_journals$domain, domain_lookup, "domain_id", "domain_name")


## languages enhancement
# MJL language data upload and mining (label = English)
mjl_language <- read.csv("~/Desktop/Local.Journals/languages/MJL.csv") %>% select(issn, eissn, Languages) %>%
                                                                           rename(mjl_lang = Languages)
mjl_language <- mjl_language %>% distinct(issn, eissn, mjl_lang, .keep_all = TRUE) %>%
                                 filter(mjl_lang != "" & !is.na(mjl_lang))
mjl_language <- mjl_language %>% mutate(eissn = if_else(eissn %in% issn, NA_character_, eissn))
mjl_language <- mjl_language %>% mutate(issn = na_if(trimws(issn), ""))

# Scopus language data upload and mining (label = ENG)
scopus_language <- readxl::read_excel("~/Desktop/Local.Journals/languages/Scopus.xlsx") %>% select(issn, eissn, language) %>%
                                                                                            rename(scopus_lang = language)
scopus_language$issn <- sub("^(.{4})(.{4})$", "\\1-\\2", scopus_language$issn)
scopus_language$eissn <- sub("^(.{4})(.{4})$", "\\1-\\2", scopus_language$eissn)
scopus_language <- scopus_language %>% distinct(issn, eissn, scopus_lang, .keep_all = TRUE) %>%
                                       filter(scopus_lang != "" & !is.na(scopus_lang))

# DOAJ language data upload and mining (label = English)
doaj_language <- read.csv("~/Desktop/Local.Journals/languages/DOAJ.csv") %>% select(issn, eissn, language) %>%
                                                                             rename(doaj_lang = language)
doaj_language$issn <- ifelse(!is.na(doaj_language$issn) & doaj_language$issn != "", 
                             str_pad(doaj_language$issn, width = 8, side = "left", pad = "0"), 
                             doaj_language$issn)
doaj_language$eissn <- ifelse(!is.na(doaj_language$eissn) & doaj_language$eissn != "", 
                              str_pad(doaj_language$eissn, width = 8, side = "left", pad = "0"), 
                              doaj_language$eissn)
doaj_language$issn <- sub("^(.{4})(.{4})$", "\\1-\\2", doaj_language$issn)
doaj_language$eissn <- sub("^(.{4})(.{4})$", "\\1-\\2", doaj_language$eissn)
doaj_language <- doaj_language %>% distinct(issn, eissn, doaj_lang, .keep_all = TRUE) %>%
                                   filter(doaj_lang != "" & !is.na(doaj_lang))
doaj_language <- doaj_language %>% mutate(issn = na_if(trimws(issn), ""))

# add unique identifiers to each dataframe
openalex_journals <- openalex_journals %>% mutate(OA_ID = paste0("OA", row_number())) %>%
                                           relocate(OA_ID)
mjl_language <- mjl_language %>% mutate(MJL_ID = paste0("MJL", row_number())) %>%
                                 relocate(MJL_ID)
scopus_language <- scopus_language %>% mutate(SCOP_ID = paste0("SCOP", row_number())) %>%
                                       relocate(SCOP_ID)
doaj_language <- doaj_language %>% mutate(DOAJ_ID = paste0("DOAJ", row_number())) %>%
                                   relocate(DOAJ_ID)

# create variable to unify all ISSN codes per dataframe
openalex_journals$OA_issn <- apply(openalex_journals[, c("issn", "issn_l")], 1, function(x) {
                             unique_values <- unique(na.omit(x))
                             paste(unique_values, collapse = ";")})
mjl_language$MJL_issn <- apply(mjl_language[, c("issn", "eissn")], 1, function(x) {
                         unique_values <- unique(na.omit(x))
                         paste(unique_values, collapse = ";")})
scopus_language$SCOP_issn <- apply(scopus_language[, c("issn", "eissn")], 1, function(x) {
                             unique_values <- unique(na.omit(x))
                             paste(unique_values, collapse = ";")})
doaj_language$DOAJ_issn <- apply(doaj_language[, c("issn", "eissn")], 1, function(x) {
                           unique_values <- unique(na.omit(x))
                           paste(unique_values, collapse = ";")})

# prepare dataframes for matching
openalex_journals_match <- subset(openalex_journals, select = c("OA_ID", "OA_issn"))
openalex_journals_match <- openalex_journals_match %>% mutate(OA_issn = strsplit(as.character(OA_issn), ";")) %>%
                                                       unnest(OA_issn) %>%
                                                       mutate(OA_issn = gsub("\\s+", "", OA_issn)) %>%
                                                       filter(OA_issn != "") %>%
                                                       distinct(OA_ID, OA_issn)

mjl_language_match <- subset(mjl_language, select = c("MJL_ID", "MJL_issn"))
mjl_language_match <- mjl_language_match %>% mutate(MJL_issn = strsplit(as.character(MJL_issn), ";")) %>%
                                             unnest(MJL_issn) %>%
                                             mutate(MJL_issn = gsub("\\s+", "", MJL_issn)) %>%
                                             filter(MJL_issn != "") %>%
                                             distinct(MJL_ID, MJL_issn)

scopus_language_match <- subset(scopus_language, select = c("SCOP_ID", "SCOP_issn"))
scopus_language_match <- scopus_language_match %>% mutate(SCOP_issn = strsplit(as.character(SCOP_issn), ";")) %>%
                                                   unnest(SCOP_issn) %>%
                                                   mutate(SCOP_issn = gsub("\\s+", "", SCOP_issn)) %>%
                                                   filter(SCOP_issn != "") %>%
                                                   distinct(SCOP_ID, SCOP_issn)

doaj_language_match <- subset(doaj_language, select = c("DOAJ_ID", "DOAJ_issn"))
doaj_language_match <- doaj_language_match %>% mutate(DOAJ_issn = strsplit(as.character(DOAJ_issn), ";")) %>%
                                               unnest(DOAJ_issn) %>%
                                               mutate(DOAJ_issn = gsub("\\s+", "", DOAJ_issn)) %>%
                                               filter(DOAJ_issn != "") %>%
                                               distinct(DOAJ_ID, DOAJ_issn)

# match all dataframes by the journals' ISSN codes to OpenAlex
ddff_ISSNs_match <- openalex_journals_match %>% left_join(mjl_language_match, by = c("OA_issn" = "MJL_issn"), relationship = "many-to-many") %>%
                                                left_join(scopus_language_match, by = c("OA_issn" = "SCOP_issn"), relationship = "many-to-many") %>%
                                                left_join(doaj_language_match, by = c("OA_issn" = "DOAJ_issn"), relationship = "many-to-many") %>%
                                                select(OA_ID, MJL_ID, SCOP_ID, DOAJ_ID, OA_issn) %>%
                                                rename(issn = OA_issn)

# remove ISSN code variable, duplicated rows, cases with only one ID, and group rows by the OpenAlex ID
ddff_ISSNs_match <- subset(ddff_ISSNs_match, select = c("OA_ID", "MJL_ID", "SCOP_ID", "DOAJ_ID"))
ddff_ISSNs_match <- ddff_ISSNs_match %>% distinct()
ddff_ISSNs_match <- ddff_ISSNs_match[rowSums(!is.na(ddff_ISSNs_match)) > 1, ]
ddff_ISSNs_match <- ddff_ISSNs_match %>% group_by(OA_ID) %>%
                                         summarise(across(everything(), ~ unique(na.omit(.))[1]), .groups = "drop")

# merge the language variables from MJL, Scopus and DOAJ into OpenAlex journals
openalex_journals <- openalex_journals %>% left_join(ddff_ISSNs_match, by = c("OA_ID" = "OA_ID"))
openalex_journals <- openalex_journals %>% left_join(mjl_language %>% select(MJL_ID, mjl_lang), by = "MJL_ID") %>%
                                           left_join(scopus_language %>% select(SCOP_ID, scopus_lang), by = "SCOP_ID") %>%
                                           left_join(doaj_language %>% select(DOAJ_ID, doaj_lang), by = "DOAJ_ID")
openalex_journals <- openalex_journals %>% select(-OA_issn, -MJL_ID, -SCOP_ID, -DOAJ_ID)

# create a new variable to identify if a journal publishes in English (= English in MJL, ENG in Scopus, English in DOAJ and en in OpenAlex)
openalex_journals <- openalex_journals %>% mutate(english_lang = case_when(str_detect(scopus_lang %||% "", "\\bENG\\b") |
                                                                             str_detect(mjl_lang %||% "", "\\bEnglish\\b") |
                                                                             str_detect(doaj_lang %||% "", "\\bEnglish\\b") |
                                                                             str_detect(language %||% "", "\\ben\\b") ~ 1,
                                                                           !is.na(scopus_lang) | !is.na(mjl_lang) | !is.na(doaj_lang) | !is.na(language) ~ 0,
                                                                           TRUE ~ NA_real_))


## mainstream indexing identification
# MJL journals upload and data mining
mjl_journals <- read.csv("~/Desktop/Local.Journals/languages/MJL.csv") %>% select(issn, eissn)
mjl_issns <- unique(c(mjl_journals$issn, mjl_journals$eissn))
mjl_issns <- mjl_issns[!is.na(mjl_issns) & mjl_issns != ""]

# Scopus journals upload
scopus_journals <- readxl::read_excel("~/Desktop/Local.Journals/languages/Scopus.xlsx") %>% select(issn, eissn)
scopus_issns <- unique(c(scopus_journals$issn, scopus_journals$eissn))
scopus_issns <- scopus_issns[!is.na(scopus_issns) & scopus_issns != ""]

# label journals in openalex_journals that match any of those MJL or Scopus ISSNs
indexed_issns <- unique(c(mjl_issns, scopus_issns))
openalex_journals <- openalex_journals %>% mutate(wos_scopus_index = if_else(issn %in% indexed_issns | issn_l %in% indexed_issns,
                                                                             1, 0, missing = NA_real_))

# classification of OpenAlex journals into mainstream and non-mainstream
mainstream_journals <- openalex_journals %>% filter(wos_scopus_index == 1)
non_mainstream_journals <- openalex_journals %>% anti_join(mainstream_journals)


## identification of knowledge bridging journals within all of the OpenAlex journals
# read references files and split into 20 dataframes for processing
references_files <- list.files(path = "~/Desktop/Local.Journals/references", pattern = "local_journals_OA2503_references_local_variable_.*", full.names = TRUE)
num_parts <- 20  
num_files <- length(references_files)
chunk_size <- ceiling(num_files / num_parts)
for (i in 1:num_parts) {chunk_files <- references_files[((i - 1) * chunk_size + 1):min(i * chunk_size, num_files)]
                        chunk_files <- chunk_files[!is.na(chunk_files)]
                        chunk_data <- rbindlist(lapply(chunk_files, fread), fill = TRUE)
                        assign(paste0("references_part_", i), chunk_data, envir = .GlobalEnv)
                        rm(chunk_data)
                        gc()}

# compute the refs_count and refs_total variables recurrently per each dataframe partition (references_part_1 until references_part_20)
references_part_1 <- references_part_1 %>% group_by(journal_id, journal_name, country) %>%
                                           mutate(refs_count = n()) %>%
                                           ungroup()
references_part_1 <- within(references_part_1, rm(article_id, reference_id))
references_part_1 <- references_part_1 %>% distinct()
references_part_1 <- references_part_1 %>% group_by(journal_id, journal_name) %>%
                                           mutate(refs_total = sum(refs_count, na.rm = TRUE)) %>%
                                           ungroup()

# merge all parts together without loosing rows
references <- rbind(references_part_1, references_part_2, references_part_3, references_part_4, references_part_5, references_part_6,
                    references_part_7, references_part_8, references_part_9, references_part_10, references_part_11, references_part_12,
                    references_part_13, references_part_14, references_part_15, references_part_16, references_part_17, references_part_18,
                    references_part_19, references_part_20, fill = TRUE)

# compute variables refs_count, refs_total and refs_prop per unique combination of journal and its most referenced country
references <- references %>% group_by(journal_id, journal_name, country) %>%
                             summarise(refs_count = sum(refs_count, na.rm = TRUE), .groups = "drop")
references <- references %>% filter(!(journal_id == 1 & journal_name == "TRUE" & country == "TRUE" & refs_count == 1))
references <- references %>% group_by(journal_id, journal_name) %>%
                             mutate(refs_total = sum(refs_count, na.rm = TRUE)) %>%
                             ungroup()
references <- references %>% group_by(journal_id, journal_name) %>%
                             filter(refs_count == max(refs_count)) %>%
                             ungroup()
references <- references %>% mutate(refs_prop = round(refs_count / refs_total, 2))

# incorporate these refs variables to all of the OpenAlex journals dataframe
openalex_journals <- openalex_journals %>% left_join(references %>%
                                                     mutate(journal_id = as.numeric(journal_id)) %>%
                                                     select(journal_id, country, refs_count, refs_total, refs_prop) %>%
                                                     rename(refs_country = country), by = "journal_id")
openalex_journals <- openalex_journals %>% distinct()

# read citations files
citations <- list.files(path = "~/Desktop/Local.Journals/citations", pattern = "local_journals_OA2503_citations_local_variable_.*", full.names = TRUE)
citations <- rbindlist(lapply(citations, fread, sep = ","), fill = TRUE)

# compute variables cits_count, cits_total and cits_prop per unique combination of journal and its most citing country
citations <- citations %>% group_by(journal_id, journal_name, country) %>%
                           mutate(cits_count = n()) %>%
                           ungroup()
citations <- within(citations, rm(article_id, citing_work_id))
citations <- citations %>% distinct()
citations <- citations %>% group_by(journal_id, journal_name) %>%
                           mutate(cits_total = sum(cits_count, na.rm = TRUE)) %>%
                           ungroup()
citations <- citations %>% group_by(journal_id, journal_name) %>%
                           filter(cits_count == max(cits_count)) %>%
                           ungroup()
citations <- citations %>% mutate(cits_prop = round(cits_count / cits_total, 2))

# incorporate these cits variables to all of the OpenAlex journals dataframe
openalex_journals <- openalex_journals %>% left_join(citations %>%
                                                     mutate(journal_id = as.numeric(journal_id)) %>%
                                                     select(journal_id, country, cits_count, cits_total, cits_prop) %>%
                                                     rename(cits_country = country), by = c("journal_id"))

# compute descriptive measures with variables refs_prop and cits_prop per unique OpenAlex journal combination
print(quantile(openalex_journals %>% distinct(journal_id, .keep_all = TRUE) %>%
                                              pull(refs_prop), probs = 0.75, na.rm = TRUE))

print(quantile(openalex_journals %>% distinct(journal_id, .keep_all = TRUE) %>%
                                              pull(cits_prop), probs = 0.75, na.rm = TRUE))

# label journals in openalex_journals as knowledge bridging and/or gap filling
openalex_journals <- openalex_journals %>% mutate(knowledge_bridging = if_else(refs_prop < 0.42 & cits_prop >= 0.86, 1, 0, missing = NA_real_))
openalex_journals <- openalex_journals %>% mutate(gap_filling = if_else(refs_prop >= 0.42 & cits_prop >= 0.86, 1, 0, missing = NA_real_))

# classification of OpenAlex journals into knowledge bridging and gap filling
knowledge_bridging_journals <- openalex_journals %>% filter(knowledge_bridging == 1)
gap_filling_journals <- openalex_journals %>% filter(gap_filling == 1)


## figure 2
my_colors <- list(mainstream = "#D53E4F", non_mainstream = "#66C2A5", knowledge_bridging = "#7B02A8", gap_filling = "#FCA50A")

# function to compute the overlap areas and plot accordingly
draw_2set_venn <- function(set1_size, set2_size, intersection_size) {r1 <- sqrt(set1_size / pi)
                                                                     r2 <- sqrt(set2_size / pi)
                                                                     overlap_area <- function(d, r1, r2) {if(d >= r1 + r2) return(0)
                                                                       if(d <= abs(r1 - r2)) return(pi * min(r1,r2)^2)
                                                                       acos((d^2 + r1^2 - r2^2)/(2*d*r1))*r1^2 +
                                                                         acos((d^2 + r2^2 - r1^2)/(2*d*r2))*r2^2 -
                                                                         0.5*sqrt((-d+r1+r2)*(d+r1-r2)*(d-r1+r2)*(d+r1+r2))}
                                                                     d <- uniroot(function(d) overlap_area(d, r1, r2) - intersection_size,
                                                                                  lower = 0, upper = r1 + r2)$root
                                                                     circles <- data.frame(x = c(0, d), y = c(0, 0), r = c(r1, r2), set = factor(c("Set 1", "Set 2")))
                                                                     p <- ggplot(circles) +
                                                                       geom_circle(aes(x0 = x, y0 = y, r = r, fill = set), alpha = 0.7, color = "grey20") +
                                                                       coord_fixed() +
                                                                       theme_minimal() +
                                                                       scale_fill_manual(values = c("Set 1" = my_colors[["non_mainstream"]], "Set 2" = my_colors[["knowledge_bridging"]])) +
                                                                       theme(legend.position = "none", axis.text = element_blank(), axis.title = element_blank(), axis.ticks = element_blank(), panel.grid = element_blank())
                                                                     return(p)}

# set sizes
mainstream_size <- 17771
non_mainstream_size <- 41459
knowledge_bridging_size <- 6192
gap_filling_size <- 7219

# intersections
ms_kb_intersection <- 316
ms_gf_intersection <- 141
nm_kb_intersection <- 5876
nm_gf_intersection <- 7078

# final drawing of the Venn diagrams
figure2 <- draw_2set_venn(set1_size = non_mainstream_size,
                          set2_size = knowledge_bridging_size,
                          intersection_size = nm_kb_intersection)
ggsave("~/Desktop/journals_communication_functions/figures/figure2_nm_kb.png", figure2, width = 6, height = 6, dpi = 300)


## figure 3
# construction of articles, references and citations vectors for all subsets: mainstream, non-mainstream, knowledge bridging and gap filling journals
# bring all article data from 2023 OpenAlex publications
articles <- list.files(path = "~/Desktop/journals_communication_functions/articles", pattern = "local_journals_OA2503_journals_and_articles_functions_.*", full.names = TRUE)
articles <- rbindlist(lapply(articles, fread), fill = TRUE)

# replace the subfield codes with the corresponding tags from OpenAlex topics
articles <- articles %>% left_join(openalex_topics %>% 
                         select(subfield_id, subfield_name) %>% 
                         distinct(), by = c("subfield" = "subfield_id")) %>%
                         mutate(subfield = subfield_name) %>%
                         select(-subfield_name)

# subset those articles into mainstream, non-mainstream, knowledge bridging and gap filling journals groups
mainstream_articles <- articles %>% mutate(journal_id = as.character(journal_id)) %>%
                                    semi_join(mainstream_journals %>% mutate(journal_id = as.character(journal_id)),
                                              by = "journal_id")

non_mainstream_articles <- articles %>% mutate(journal_id = as.character(journal_id)) %>%
                                        semi_join(non_mainstream_journals %>% mutate(journal_id = as.character(journal_id)),
                                        by = "journal_id")

knowledge_bridging_articles <- articles %>% mutate(journal_id = as.character(journal_id)) %>%
                                            semi_join(knowledge_bridging_journals %>% mutate(journal_id = as.character(journal_id)),
                                            by = "journal_id")

gap_filling_articles <- articles %>% mutate(journal_id = as.character(journal_id)) %>%
                                     semi_join(gap_filling_journals %>% mutate(journal_id = as.character(journal_id)),
                                     by = "journal_id")

# subset the references linked to knowledge bridging and gap filling articles
references_files <- list.files(path = "~/Desktop/Local.Journals/references", pattern = "local_journals_OA2503_references_local_variable_.*", full.names = TRUE)
num_parts <- 20  
num_files <- length(references_files)
chunk_size <- ceiling(num_files / num_parts)
              for (i in 1:num_parts) {chunk_files <- references_files[((i - 1) * chunk_size + 1):min(i * chunk_size, num_files)]
chunk_files <- chunk_files[!is.na(chunk_files)]
chunk_data <- rbindlist(lapply(chunk_files, fread), fill = TRUE)
              assign(paste0("references_part_", i), chunk_data, envir = .GlobalEnv)
              rm(chunk_data)
              gc()}

# keep only those references linked to knowledge bridging and gap filling articles ids (references_part_1 until references_part_20)
references_part_1kb <- references_part_1 %>% semi_join(knowledge_bridging_articles, by = "article_id")
references_part_1gf <- references_part_1 %>% semi_join(gap_filling_articles, by = "article_id")

# merge all parts together without loosing rows
knowledge_bridging_references <- rbind(references_part_1kb, references_part_2kb, references_part_3kb, references_part_4kb, references_part_5kb, references_part_6kb,
                                       references_part_7kb, references_part_8kb, references_part_9kb, references_part_10kb, references_part_11kb, references_part_12kb,
                                       references_part_13kb, references_part_14kb, references_part_15kb, references_part_16kb, references_part_17kb, references_part_18kb,
                                       references_part_19kb, references_part_20kb, fill = TRUE)

gap_filling_references <- rbind(references_part_1gf, references_part_2gf, references_part_3gf, references_part_4gf, references_part_5gf, references_part_6gf,
                                references_part_7gf, references_part_8gf, references_part_9gf, references_part_10gf, references_part_11gf, references_part_12gf,
                                references_part_13gf, references_part_14gf, references_part_15gf, references_part_16gf, references_part_17gf, references_part_18gf,
                                references_part_19gf, references_part_20gf, fill = TRUE)

# remove duplicated ids in reference_id
knowledge_bridging_references <- knowledge_bridging_references %>% distinct(reference_id, .keep_all = TRUE)
gap_filling_references <- gap_filling_references %>% distinct(reference_id, .keep_all = TRUE)

# subset the citations linked to knowledge bridging and gap filling articles
citations_files <- list.files(path = "~/Desktop/Local.Journals/citations", pattern = "local_journals_OA2503_citations_local_variable_.*", full.names = TRUE)
citations_files <- rbindlist(lapply(citations_files, fread, sep = ","), fill = TRUE)

# keep only those citations linked to knowledge bridging and gap filling articles ids
knowledge_bridging_citations <- citations_files %>% semi_join(knowledge_bridging_articles, by = "article_id")
gap_filling_citations <- citations_files %>% semi_join(gap_filling_articles, by = "article_id")

# remove duplicated ids in citing_work_id
knowledge_bridging_citations <- knowledge_bridging_citations %>% distinct(citing_work_id, .keep_all = TRUE)
gap_filling_citations <- gap_filling_citations %>% distinct(citing_work_id, .keep_all = TRUE)

# download knowledge bridging and gap filling references and citations files to look for their subfields in OpenAlex BigQuery
#write.csv(knowledge_bridging_references[, "reference_id", with = FALSE], "~/Desktop/journals_communication_functions/references/knowledge_bridging_references_ids.csv", row.names = FALSE)
#write.csv(gap_filling_references[, "reference_id", with = FALSE], "~/Desktop/journals_communication_functions/references/gap_filling_references_ids.csv", row.names = FALSE)
#write.csv(knowledge_bridging_citations[, "citing_work_id", with = FALSE], "~/Desktop/journals_communication_functions/citations/knowledge_bridging_citations_ids.csv", row.names = FALSE)
#write.csv(gap_filling_citations[, "citing_work_id", with = FALSE], "~/Desktop/journals_communication_functions/citations/gap_filling_citations_ids.csv", row.names = FALSE)

# incorporate the found subfields into knowledge bridging and gap filling references and citations dataframes, one per row to match the other dfs format and replacing the codes for tags
references_subfields_kb <- read_csv("~/Desktop/journals_communication_functions/references/knowledge_bridging_references_subfields.csv")
references_subfields_kb <- references_subfields_kb %>% mutate(reference_id = as.integer64(reference_id))

references_subfields_gf <- read_csv("~/Desktop/journals_communication_functions/references/gap_filling_references_subfields.csv")
references_subfields_gf <- references_subfields_gf %>% mutate(reference_id = as.integer64(reference_id))

knowledge_bridging_references <- knowledge_bridging_references %>% select(-country) %>%
                                                                   left_join(references_subfields_kb, by = "reference_id")

gap_filling_references <- gap_filling_references %>% select(-country) %>%
                                                     left_join(references_subfields_gf, by = "reference_id")

knowledge_bridging_references <- knowledge_bridging_references %>% left_join(openalex_topics %>% 
                                                                   select(subfield_id, subfield_name) %>% 
                                                                   distinct(), by = c("subfield" = "subfield_id")) %>%
                                                                   mutate(subfield = subfield_name) %>%
                                                                   select(-subfield_name)

gap_filling_references <- gap_filling_references %>% left_join(openalex_topics %>% 
                                                     select(subfield_id, subfield_name) %>% 
                                                     distinct(), by = c("subfield" = "subfield_id")) %>%
                                                     mutate(subfield = subfield_name) %>%
                                                     select(-subfield_name)

citations_subfields_kb <- read_csv("~/Desktop/journals_communication_functions/citations/knowledge_bridging_citations_subfields.csv")
citations_subfields_kb <- citations_subfields_kb %>% mutate(citing_work_id = as.integer64(citing_work_id))

citations_subfields_gf <- read_csv("~/Desktop/journals_communication_functions/citations/gap_filling_citations_subfields.csv")
citations_subfields_gf <- citations_subfields_gf %>% mutate(citing_work_id = as.integer64(citing_work_id))

knowledge_bridging_citations <- knowledge_bridging_citations %>% select(-country) %>%
                                                                 left_join(citations_subfields_kb, by = "citing_work_id")

gap_filling_citations <- gap_filling_citations %>% select(-country) %>%
                                                   left_join(citations_subfields_gf, by = "citing_work_id")

knowledge_bridging_citations <- knowledge_bridging_citations %>% left_join(openalex_topics %>% 
                                                                 select(subfield_id, subfield_name) %>% 
                                                                 distinct(), by = c("subfield" = "subfield_id")) %>%
                                                                 mutate(subfield = subfield_name) %>%
                                                                 select(-subfield_name)

gap_filling_citations <- gap_filling_citations %>% left_join(openalex_topics %>% 
                                                   select(subfield_id, subfield_name) %>% 
                                                   distinct(), by = c("subfield" = "subfield_id")) %>%
                                                   mutate(subfield = subfield_name) %>%
                                                   select(-subfield_name)


## incorporate references and citations data to mainstream and non-mainstream articles
# download mainstream and non-mainstream files with article IDs to look for their references and citations in OpenAlex BigQuery, as well as associated subfields
#write.csv(unique(mainstream_articles[, .(article_id)]), "~/Desktop/journals_communication_functions/mainstream_articles_ids.csv", row.names = FALSE)
#write.csv(unique(non_mainstream_articles[, .(article_id)]), "~/Desktop/journals_communication_functions/non_mainstream_articles_ids.csv", row.names = FALSE)


## build vectors for all dfs (mainstream_articles, mainstream_references, mainstream_citations, non-mainstream_articles, non-mainstream_references, non-mainstream_citations, knowledge_bridging_articles, knowledge_bridging_references, knowledge_bridging_citations, gap_filling_articles, gap_filling_references and gap_filling_citations). In mainstream and non-mainstream references and citations, some steps were conducted in BigQuery due to the amount of data available
mainstream_articles_vector <- mainstream_articles %>% filter(!is.na(subfield)) %>%
                                                      count(subfield) %>%
                                                      arrange(subfield)
mainstream_articles_vec <- mainstream_articles_vector$n
names(mainstream_articles_vec) <- mainstream_articles_vector$subfield

mainstream_references_vector <- read_csv("~/Desktop/journals_communication_functions/references/mainstream_references_subfields.csv")
mainstream_references_vec <- mainstream_references_vector$n
names(mainstream_references_vec) <- mainstream_references_vector$subfield

mainstream_citations_vector <- read_csv("~/Desktop/journals_communication_functions/citations/mainstream_citations_subfields.csv")
mainstream_citations_vec <- mainstream_citations_vector$n
names(mainstream_citations_vec) <- mainstream_citations_vector$subfield

non_mainstream_articles_vector <- non_mainstream_articles %>% filter(!is.na(subfield)) %>%
                                                              count(subfield) %>%
                                                              arrange(subfield)
non_mainstream_articles_vec <- non_mainstream_articles_vector$n
names(non_mainstream_articles_vec) <- non_mainstream_articles_vector$subfield

non_mainstream_references_vector <- read_csv("~/Desktop/journals_communication_functions/references/non_mainstream_references_subfields.csv")
non_mainstream_references_vec <- non_mainstream_references_vector$n
names(non_mainstream_references_vec) <- non_mainstream_references_vector$subfield

non_mainstream_citations_vector <- read_csv("~/Desktop/journals_communication_functions/citations/non_mainstream_citations_subfields.csv")
non_mainstream_citations_vec <- non_mainstream_citations_vector$n
names(non_mainstream_citations_vec) <- non_mainstream_citations_vector$subfield

knowledge_bridging_articles_vector <- knowledge_bridging_articles %>% filter(!is.na(subfield)) %>%
                                                                      count(subfield) %>%
                                                                      arrange(subfield)
knowledge_bridging_articles_vec <- knowledge_bridging_articles_vector$n
names(knowledge_bridging_articles_vec) <- knowledge_bridging_articles_vector$subfield

knowledge_bridging_references_vector <- knowledge_bridging_references %>% filter(!is.na(subfield)) %>%
                                                                          count(subfield) %>%
                                                                          arrange(subfield)
knowledge_bridging_references_vec <- knowledge_bridging_references_vector$n
names(knowledge_bridging_references_vec) <- knowledge_bridging_references_vector$subfield

knowledge_bridging_citations_vector <- knowledge_bridging_citations %>% filter(!is.na(subfield)) %>%
                                                                        count(subfield) %>%
                                                                        arrange(subfield)
knowledge_bridging_citations_vec <- knowledge_bridging_citations_vector$n
names(knowledge_bridging_citations_vec) <- knowledge_bridging_citations_vector$subfield

gap_filling_articles_vector <- gap_filling_articles %>% filter(!is.na(subfield)) %>%
                                                        count(subfield) %>%
                                                        arrange(subfield)
gap_filling_articles_vec <- gap_filling_articles_vector$n
names(gap_filling_articles_vec) <- gap_filling_articles_vector$subfield

gap_filling_references_vector <- gap_filling_references %>% filter(!is.na(subfield)) %>%
                                                            count(subfield) %>%
                                                            arrange(subfield)
gap_filling_references_vec <- gap_filling_references_vector$n
names(gap_filling_references_vec) <- gap_filling_references_vector$subfield

gap_filling_citations_vector <- gap_filling_citations %>% filter(!is.na(subfield)) %>%
                                                          count(subfield) %>%
                                                          arrange(subfield)
gap_filling_citations_vec <- gap_filling_citations_vector$n
names(gap_filling_citations_vec) <- gap_filling_citations_vector$subfield

# complete and align vectors to the mainstream
mainstream_subfields <- names(mainstream_articles_vec)
align_to_mainstream <- function(vec, mainstream_names) {aligned <- numeric(length(mainstream_names))
                        names(aligned) <- mainstream_names
                        aligned[names(vec)] <- vec[names(vec) %in% mainstream_names]
                        aligned}

mainstream_references_vec <- align_to_mainstream(mainstream_references_vec, mainstream_subfields)
mainstream_citations_vec <- align_to_mainstream(mainstream_citations_vec, mainstream_subfields)
non_mainstream_articles_vec <- align_to_mainstream(non_mainstream_articles_vec, mainstream_subfields)
non_mainstream_references_vec <- align_to_mainstream(non_mainstream_references_vec, mainstream_subfields)
non_mainstream_citations_vec <- align_to_mainstream(non_mainstream_citations_vec, mainstream_subfields)
knowledge_bridging_articles_vec <- align_to_mainstream(knowledge_bridging_articles_vec, mainstream_subfields)
knowledge_bridging_references_vec <- align_to_mainstream(knowledge_bridging_references_vec, mainstream_subfields)
knowledge_bridging_citations_vec <- align_to_mainstream(knowledge_bridging_citations_vec, mainstream_subfields)
gap_filling_articles_vec <- align_to_mainstream(gap_filling_articles_vec, mainstream_subfields)
gap_filling_references_vec <- align_to_mainstream(gap_filling_references_vec, mainstream_subfields)
gap_filling_citations_vec <- align_to_mainstream(gap_filling_citations_vec, mainstream_subfields)

# normalize all vectors so that the comparisons are not dataframe size dependent
normalize_vec <- function(x) {if (sum(x) == 0) return(x)
                 x / sum(x)}

mainstream_articles_vec <- normalize_vec(mainstream_articles_vec)
mainstream_references_vec <- normalize_vec(mainstream_references_vec)
mainstream_citations_vec <- normalize_vec(mainstream_citations_vec)
non_mainstream_articles_vec <- normalize_vec(non_mainstream_articles_vec)
non_mainstream_references_vec <- normalize_vec(non_mainstream_references_vec)
non_mainstream_citations_vec <- normalize_vec(non_mainstream_citations_vec)
knowledge_bridging_articles_vec <- normalize_vec(knowledge_bridging_articles_vec)
knowledge_bridging_references_vec <- normalize_vec(knowledge_bridging_references_vec)
knowledge_bridging_citations_vec <- normalize_vec(knowledge_bridging_citations_vec)
gap_filling_articles_vec <- normalize_vec(gap_filling_articles_vec)
gap_filling_references_vec <- normalize_vec(gap_filling_references_vec)
gap_filling_citations_vec <- normalize_vec(gap_filling_citations_vec)

# compute cosine similarity
cosine_sim <- function(x, y) {sum(x * y) / (sqrt(sum(x^2)) * sqrt(sum(y^2)))}

mainstream_references_cos <- cosine_sim(mainstream_articles_vec, mainstream_references_vec)
mainstream_citations_cos <- cosine_sim(mainstream_articles_vec, mainstream_citations_vec)
non_mainstream_articles_cos <- cosine_sim(mainstream_articles_vec, non_mainstream_articles_vec)
non_mainstream_references_cos <- cosine_sim(mainstream_articles_vec, non_mainstream_references_vec)
non_mainstream_citations_cos <- cosine_sim(mainstream_articles_vec, non_mainstream_citations_vec)
knowledge_bridging_articles_cos <- cosine_sim(mainstream_articles_vec, knowledge_bridging_articles_vec)
knowledge_bridging_references_cos <- cosine_sim(mainstream_articles_vec, knowledge_bridging_references_vec)
knowledge_bridging_citations_cos <- cosine_sim(mainstream_articles_vec, knowledge_bridging_citations_vec)
gap_filling_articles_cos <- cosine_sim(mainstream_articles_vec, gap_filling_articles_vec)
gap_filling_references_cos <- cosine_sim(mainstream_articles_vec, gap_filling_references_vec)
gap_filling_citations_cos <- cosine_sim(mainstream_articles_vec, gap_filling_citations_vec)

# final drawing of the vectors to represent the cosine similarity of all subsets with respect to the mainstream
figure3 <- tibble(group = c("Mainstream references (cos 0.98)", "Mainstream citations (cos 0.95)", "Non-mainstream references (cos 0.80)", "Knowledge bridging references (cos 0.80)", "Non-mainstream citations (cos 0.71)", "Non-mainstream articles (cos 0.58)", "Knowledge bridging articles (cos 0.54)", "Knowledge bridging citations (cos 0.50)", "Gap-filling references (cos 0.30)", "Gap-filling articles (cos 0.21)", "Gap-filling citations (cos 0.19)"),
                  cosine = c(0.9826949, 0.9598726, 0.81, 0.80, 0.7129414, 0.5780729, 0.5401474, 0.5038888, 0.3046356, 0.2113309, 0.1924647),
                  angle_rad = acos(cosine)) %>%
                  mutate(group = fct_relevel(group, "Gap-filling citations (cos 0.19)", "Gap-filling articles (cos 0.21)", "Gap-filling references (cos 0.30)", "Knowledge bridging citations (cos 0.50)", "Knowledge bridging articles (cos 0.54)", "Non-mainstream articles (cos 0.58)", "Non-mainstream citations (cos 0.71)", "Knowledge bridging references (cos 0.80)", "Non-mainstream references (cos 0.80)", "Mainstream citations (cos 0.95)", "Mainstream references (cos 0.98)"))
arc <- tibble(t = seq(0, pi/2, length.out = 200),
              x = 0.95 * cos(t),
              y = 0.95 * sin(t))

ggplot(figure3) +
  geom_path(data = arc, aes(x = x, y = y), color = "grey80", linewidth = 0.4, linetype = "dashed") +
  geom_segment(aes(x = 0, y = 0,
                   xend = cos(angle_rad),
                   yend = sin(angle_rad),
                   color = group),
               arrow = arrow(length = unit(0.3, "cm")),
               linewidth = 0.8) +
  geom_segment(aes(x = 0, y = 0, xend = 1, yend = 0), color = "grey40", linewidth = 0.5) +
  geom_segment(aes(x = 0, y = 0, xend = 0, yend = 1), color = "grey40", linewidth = 0.5) +
  annotate("text", x = 1.05, y = -0.04, label = "0°", size = 3, color = "grey40") +
  annotate("text", x = 0.5, y = -0.04, label = "Mainstream articles", size = 2.5, color = "grey40") +
  annotate("text", x = -0.04, y = 1.07, label = "90°", size = 3, color = "grey40") +
  coord_fixed(xlim = c(-0.05, 1.2), ylim = c(-0.05, 1.2)) +
  scale_color_manual(values = c("Mainstream references (cos 0.98)" = "#8C1220",
                                "Mainstream citations (cos 0.95)" = "#EE8C96",
                                "Non-mainstream references (cos 0.80)" = "#2E8B6E",
                                "Knowledge bridging references (cos 0.80)" = "#5C0190",
                                "Non-mainstream citations (cos 0.71)" = "#B3E2D2",
                                "Non-mainstream articles (cos 0.58)" = "#66C2A5",
                                "Knowledge bridging articles (cos 0.54)" = "#7B02A8",
                                "Knowledge bridging citations (cos 0.50)" = "#B34FD4",
                                "Gap-filling references (cos 0.30)" = "#B87100",
                                "Gap-filling articles (cos 0.21)" = "#FCA50A",
                                "Gap-filling citations (cos 0.19)" = "#FDD06D")) +
  labs(color = NULL) +
  theme_minimal() +
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.5, "cm"))
ggsave("~/Desktop/journals_communication_functions/figures/figure3.png", width = 6, height = 4, dpi = 300)


## figure 4A
# plot scatterplot at the journal level splitting them between mainstream and non-mainstream cases
ggplot(openalex_journals, aes(x = cits_prop, y = refs_prop, group = journal_id)) +
  geom_point(aes(color = factor(wos_scopus_index)),
             alpha = 0.6, size = 1.5) +
  geom_hline(yintercept = 0.42, linetype = "dashed", color = "grey0", linewidth = 0.6) +
  geom_vline(xintercept = 0.86, linetype = "dashed", color = "grey0", linewidth = 0.6) +
  scale_x_continuous(breaks = c(0.00, 0.25, 0.50, 0.75, 0.86, 1.00),
                     labels = c("0.00", "0.25", "0.50", "0.75", "3ºQ", "1.00")) +
  scale_y_continuous(breaks = c(0.00, 0.25, 0.42, 0.50, 0.75, 1.00),
                     labels = c("0.00", "0.25", "3ºQ", "0.50", "0.75", "1.00")) +
  scale_color_manual(values = c("1" = "#D53E4F",
                                "0" = "#66C2A5"),
                     labels = c("1" = "Mainstream",
                                "0" = "Non-mainstream"),
                     breaks = c("1", "0")) +
  labs(x = "Citation share", y = "Reference share", color = "Indexing status") +
  theme_minimal() +
  theme(legend.position = "right")
ggsave("~/Desktop/journals_communication_functions/figures/figure4A.png", width = 6, height = 4, dpi = 300)

## figure 4B
# plor scatterplot starting from articles df in order to compute the most predominant domain per journal. Each dot is a journal and the colors correspond to each domain
figure4B <- articles %>%
  filter(!is.na(domain)) %>%
  count(journal_id, domain) %>%
  slice_max(n, by = journal_id, with_ties = FALSE) %>%
  mutate(domain = case_when(domain == 1 ~ "Life Sciences",
                            domain == 2 ~ "Social Sciences",
                            domain == 3 ~ "Physical Sciences",
                            domain == 4 ~ "Health Sciences"),
         journal_id = as.character(journal_id)) %>%
  select(journal_id, domain) %>%
  inner_join(openalex_journals %>%
               mutate(journal_id = as.character(journal_id)) %>%
               select(journal_id, cits_prop, refs_prop, wos_scopus_index),
             by = "journal_id") %>%
  distinct(journal_id, .keep_all = TRUE)

ggplot(figure4B, aes(x = cits_prop, y = refs_prop, group = journal_id)) +
  geom_point(aes(color = domain),
             alpha = 0.6, size = 1.5) +
  geom_hline(yintercept = 0.42, linetype = "dashed", color = "grey0", linewidth = 0.6) +
  geom_vline(xintercept = 0.86, linetype = "dashed", color = "grey0", linewidth = 0.6) +
  scale_x_continuous(breaks = c(0.00, 0.25, 0.50, 0.75, 0.86, 1.00),
                     labels = c("0.00", "0.25", "0.50", "0.75", "3ºQ", "1.00")) +
  scale_y_continuous(breaks = c(0.00, 0.25, 0.42, 0.50, 0.75, 1.00),
                     labels = c("0.00", "0.25", "3ºQ", "0.50", "0.75", "1.00")) +
  scale_color_manual(values = c("Life Sciences"     = "#3DBFA8",
                                "Social Sciences"   = "#F5D130",
                                "Physical Sciences" = "#154F8A",
                                "Health Sciences"   = "#F24CAB")) +
  labs(x = "Citation share", y = "Reference share", color = "Domain") +
  theme_minimal() +
  theme(legend.position = "right")
ggsave("~/Desktop/journals_communication_functions/figures/figure4B.png", width = 6, height = 4, dpi = 300)
