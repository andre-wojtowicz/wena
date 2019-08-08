library(checkpoint)

options("checkpoint.mranUrl" = "file:///") # fast checkpoint load
checkpoint(snapshotDate    = "2019-04-15", # default for MRO 3.5.3
           scanForPackages = FALSE,
           verbose         = FALSE)

suppressPackageStartupMessages({
library(git2r)
library(dplyr)
library(fuzzyjoin)
library(readr)
library(shiny.i18n)
library(tibble)
library(tidyr)
})

tr_s  = Translator$new(translation_json_path = "translation-site.json")
tr_rg = Translator$new(translation_json_path = "translation-research-groups.json")
tr_d  = Translator$new(translation_json_path = "translation-disciplines.json")
tr_ra = Translator$new(translation_json_path = "translation-research-areas.json")

primary_data_language    = "pl"
secondary_data_languages = setdiff(tr_s$languages, c(primary_data_language, "id"))
all_data_languages       = setdiff(tr_s$languages, "id")

tr_rg_recode_df = tr_rg$translations %>% rownames_to_column(tr_rg$languages[1])
tr_d_recode_df  = tr_d$translations  %>% rownames_to_column(tr_d$languages[1])
tr_ra_recode_df = tr_ra$translations %>% rownames_to_column(tr_ra$languages[1]) %>%
                                         mutate(id = rownames(tr_ra$translations))

HTML_RECODE = c("TRUE" = "&#10004;", "FALSE" = "&#128473;", "NA" = "")

data_info = read_csv("data/info.csv",
                     col_types = cols(
                        database = col_character(),
                        date     = col_date(format = "%Y-%m-%d")
                    ))

pbn_articles = read_csv("data/pbn-articles.csv",
                        col_types = cols(
                            title                       = col_character(),
                            `system-identifier`         = col_character(),
                            `publication-date`          = col_integer(),
                            doi                         = col_character(),
                            `is-conference`             = col_logical(),
                            `journal-title`             = col_character(),
                            `journal-id-pbn`            = col_character(),
                            `journal-id-issn`           = col_character(),
                            `journal-id-eissn`          = col_character(),
                            `journal-ministerial-list`  = col_character(),
                            `journal-points`            = col_integer(),
                            `author-given-names`        = col_character(),
                            `author-family-name`        = col_character(),
                            `author-id-system`          = col_character(),
                            `author-id-pbn`             = col_integer(),
                            `author-id-orcid`           = col_character(),
                            `author-affiliated-to-unit` = col_logical(),
                            `author-employed-in-unit`   = col_logical()
                        ))

pbn_chapters = read_csv("data/pbn-chapters.csv",
                        col_types = cols(
                            title                       = col_character(),
                            `system-identifier`         = col_character(),
                            `publication-date`          = col_integer(),
                            doi                         = col_character(),
                            `is-conference`             = col_logical(),
                            `book-title`                = col_character(),
                            `author-given-names`        = col_character(),
                            `author-family-name`        = col_character(),
                            `author-id-system`          = col_character(),
                            `author-id-pbn`             = col_integer(),
                            `author-id-orcid`           = col_character(),
                            `author-affiliated-to-unit` = col_logical(),
                            `author-employed-in-unit`   = col_logical()
                        ))
pbn_books = read_csv("data/pbn-books.csv",
                     col_types = cols(
                        title                       = col_character(),
                        `system-identifier`         = col_character(),
                        `publication-date`          = col_integer(),
                        doi                         = col_character(),
                        `is-conference`             = col_logical(),
                        `author-given-names`        = col_character(),
                        `author-family-name`        = col_character(),
                        `author-id-system`          = col_character(),
                        `author-id-pbn`             = col_integer(),
                        `author-id-orcid`           = col_character(),
                        `author-affiliated-to-unit` = col_logical(),
                        `author-employed-in-unit`   = col_logical(),
                        `is-author`                 = col_logical(),
                        `is-editor`                 = col_logical()
                    ))

faculty_members = read_csv("data/faculty-members.csv",
                           col_types = cols(
                                Author             = col_character(),
                                `Name variants`    = col_character(),
                                Affiliation        = col_character(),
                                `Level 1`          = col_character(),
                                `Level 2`          = col_character(),
                                `Level 3`          = col_character(),
                                `Scopus Author ID` = col_double(),
                                `PBN ID`           = col_integer()
                            )) %>%
                select(-Affiliation) %>%
                rename(Faculty    = `Level 1`,
                       Institute  = `Level 2`,
                       Department = `Level 3`) %>%
                mutate(Discipline = case_when(
                        startsWith(Institute, "Matematyka")  ~ "matematyka",
                        startsWith(Institute, "Informatyka") ~ "informatyka",
                        TRUE ~ Institute)) %>%
                mutate(id = row_number()) %>%
                select(id, Author:Discipline)

work_join = function(pbn_works, faculty_members)
{
    # stage 1: PBN ID

    tmp = pbn_works %>%
          left_join(filter(faculty_members, !is.na(`PBN ID`)),
                    by = c("author-id-pbn" = "PBN ID")) %>%
          select(colnames(pbn_works), id)

    stage_1    = tmp %>% filter(!is.na(id))
    to_stage_2 = tmp %>% filter(is.na(id)) %>% select(-id)

    if (nrow(to_stage_2) == 0)
        return(list(joined_works = left_join(stage_1, faculty_members,
                                             by = "id")))

    # stage 2: valid author's name order

    tmp = to_stage_2 %>%
          mutate(tmp_rev_name = paste0(`author-family-name`, ", ",
                                       `author-given-names`)) %>%
          left_join(faculty_members,
                    by = c("tmp_rev_name" = "Author")) %>%
          select(colnames(pbn_works), id)

    stage_2    = tmp %>% filter(!is.na(id))
    to_stage_3 = tmp %>% filter(is.na(id)) %>% select(-id)

    if (nrow(to_stage_3) == 0)
        return(list(joined_works = left_join(bind_rows(stage_1, stage_2),
                                             faculty_members,
                         by = "id")))

    # stage 3: reverse author's name order

    tmp = to_stage_3 %>%
          mutate(tmp_rev_name = paste0(`author-given-names`, ", ",
                                       `author-family-name`)) %>%
          left_join(faculty_members,
                    by = c("tmp_rev_name" = "Author")) %>%
          select(colnames(pbn_works), id)

    stage_3    = tmp %>% filter(!is.na(id))
    to_stage_4 = tmp %>% filter(is.na(id)) %>% select(-id)

    if (nrow(to_stage_4) == 0)
        return(list(joined_works = left_join(bind_rows(stage_1, stage_2,
                                                       stage_3),
                                             faculty_members,
                         by = "id")))

    # stage 4: valid author's name order with Hamming max 2 distance
    # (still better as separate step than replacing step 2 since fuzzy join
    # is quite slow)

    tmp = to_stage_4 %>%
          mutate(tmp_rev_name = paste0(`author-family-name`, ", ",
                                       `author-given-names`)) %>%
          stringdist_left_join(faculty_members,
                               by = c("tmp_rev_name" = "Author"),
                               method = "hamming",
                               max_dist = 2) %>%
          select(colnames(pbn_works), id)

    stage_4    = tmp %>% filter(!is.na(id))
    to_stage_5 = tmp %>% filter(is.na(id)) %>% select(-id)

    list(joined_works = left_join(bind_rows(stage_1, stage_2, stage_3, stage_4),
                   faculty_members,
                   by = "id"),
         leftovers = to_stage_5)
}

pbn_articles = work_join(pbn_articles, faculty_members)$joined_works
pbn_chapters = work_join(pbn_chapters, faculty_members)$joined_works
pbn_books    = work_join(pbn_books, faculty_members)$joined_works

pbn_min_year = min(c(pbn_articles$`publication-date`,
                     pbn_chapters$`publication-date`,
                     pbn_books$`publication-date`))

pbn_vb_authors = bind_rows(
    pbn_articles %>% select(id, `author-affiliated-to-unit`),
    pbn_chapters %>% select(id, `author-affiliated-to-unit`),
    pbn_books %>% select(id, `author-affiliated-to-unit`)) %>%
    filter(`author-affiliated-to-unit` == TRUE) %>%
    select(id) %>% unique() %>% nrow()

pbn_vb_articles =
    pbn_articles %>%
    filter(`author-affiliated-to-unit` == TRUE) %>%
    select(`system-identifier`) %>%
    unique() %>% nrow()

pbn_vb_chapters =
    pbn_chapters %>%
    filter(`author-affiliated-to-unit` == TRUE) %>%
    select(`system-identifier`) %>%
    unique() %>% nrow()

pbn_vb_books    =
    pbn_books %>%
    filter(`author-affiliated-to-unit` == TRUE) %>%
    select(`system-identifier`) %>%
    unique() %>% nrow()

pbn_vb_points   =
    pbn_articles %>%
    filter(!is.na(`journal-points`)) %>%
    filter(`author-affiliated-to-unit` == TRUE) %>%
    select(`system-identifier`, `journal-points`) %>%
    unique() %>% select(`journal-points`) %>% pull() %>% sum()

pbn_journals_stats =
    pbn_articles %>%
    filter(!is.na(`journal-ministerial-list`)) %>%
    select(`system-identifier`, `journal-ministerial-list`) %>%
    unique() %>%
    group_by(`journal-ministerial-list`) %>%
    summarise(count = n())

pbn_articles_list = sapply(all_data_languages, function(lang)
{
    tr_rg_recode_vec = bind_cols(
        tr_rg_recode_df %>% select(primary_data_language),
        tr_rg_recode_df %>% select(lang)
    ) %>% deframe()

    tr_d_recode_vec = bind_cols(
        tr_d_recode_df %>% select(primary_data_language),
        tr_d_recode_df %>% select(lang)
    ) %>% deframe()

    pbn_articles %>%
        select(`system-identifier`, title, doi, Author, `journal-title`,
               `journal-ministerial-list`, `journal-points`, Faculty, Institute,
               Department, Discipline, `publication-date`,
               `author-affiliated-to-unit`, `author-employed-in-unit`,
               `is-conference`) %>%
        arrange(title) %>%
        mutate(Faculty    = recode(Faculty,    !!!tr_rg_recode_vec),
               Institute  = recode(Institute,  !!!tr_rg_recode_vec),
               Department = recode(Department, !!!tr_rg_recode_vec),
               Discipline = recode(Discipline, !!!tr_d_recode_vec),
               title = ifelse(!is.na(doi),
                              paste0('<a href="https://dx.doi.org/',
                                     doi,
                                     '" target="_blank">',
                                     title,
                                     '</a>'),
                              title)) %>%
        rename(!!tr_s$translations["dt-title", lang]                    := title,
               !!tr_s$translations["dt-author", lang]                   := Author,
               !!tr_s$translations["dt-research-group", lang]           := Department,
               !!tr_s$translations["dt-journal-title", lang]            := `journal-title`,
               !!tr_s$translations["dt-journal-ministerial-list", lang] := `journal-ministerial-list`,
               !!tr_s$translations["dt-points-short", lang]             := `journal-points`,
               !!tr_s$translations["dt-discipline", lang]               := Discipline,
               !!tr_s$translations["dt-year", lang]                     := `publication-date`) %>%
        mutate(!!tr_s$translations["dt-author-affiliated-short", lang]  :=
                   recode(as.character(`author-affiliated-to-unit`), !!!HTML_RECODE),
               !!tr_s$translations["dt-author-employed-short", lang] :=
                   recode(as.character(`author-employed-in-unit`), !!!HTML_RECODE),
               !!tr_s$translations["dt-is-conference-short", lang] :=
                   recode(as.character(`is-conference`), !!!HTML_RECODE)) %>%
        select(-doi)
},
simplify = FALSE)

pbn_articles_years = fivenum(pbn_articles$`publication-date`)[c(1, 5)]
pbn_articles_points = c(0, max(pbn_articles$`journal-points`, na.rm = TRUE))

pbn_chapters_list = sapply(all_data_languages, function(lang)
{
    tr_rg_recode_vec = bind_cols(
        tr_rg_recode_df %>% select(primary_data_language),
        tr_rg_recode_df %>% select(lang)
    ) %>% deframe()

    tr_d_recode_vec = bind_cols(
        tr_d_recode_df %>% select(primary_data_language),
        tr_d_recode_df %>% select(lang)
    ) %>% deframe()

    pbn_chapters %>%
        select(`system-identifier`, title, doi, Author, Faculty, Institute,
               Department, Discipline, `publication-date`,
               `author-affiliated-to-unit`, `author-employed-in-unit`,
               `is-conference`) %>%
        arrange(title) %>%
        mutate(Faculty    = recode(Faculty,    !!!tr_rg_recode_vec),
               Institute  = recode(Institute,  !!!tr_rg_recode_vec),
               Department = recode(Department, !!!tr_rg_recode_vec),
               Discipline = recode(Discipline, !!!tr_d_recode_vec),
               title = ifelse(!is.na(doi),
                              paste0('<a href="https://dx.doi.org/',
                                     doi,
                                     '" target="_blank">',
                                     title,
                                     '</a>'),
                              title)) %>%
        rename(!!tr_s$translations["dt-title", lang]                   := title,
               !!tr_s$translations["dt-author", lang]                  := Author,
               !!tr_s$translations["dt-research-group", lang]          := Department,
               !!tr_s$translations["dt-discipline", lang]              := Discipline,
               !!tr_s$translations["dt-year", lang]                    := `publication-date`) %>%
        mutate(!!tr_s$translations["dt-author-affiliated-short", lang] :=
                   recode(as.character(`author-affiliated-to-unit`), !!!HTML_RECODE),
               !!tr_s$translations["dt-author-employed-short", lang] :=
                   recode(as.character(`author-employed-in-unit`), !!!HTML_RECODE),
               !!tr_s$translations["dt-is-conference-short", lang] :=
                   recode(as.character(`is-conference`), !!!HTML_RECODE)) %>%
        select(-doi)
},
simplify = FALSE)

pbn_chapters_years = fivenum(pbn_chapters$`publication-date`)[c(1, 5)]

pbn_books_list = sapply(all_data_languages, function(lang)
{
    tr_rg_recode_vec = bind_cols(
        tr_rg_recode_df %>% select(primary_data_language),
        tr_rg_recode_df %>% select(lang)
    ) %>% deframe()

    tr_d_recode_vec = bind_cols(
        tr_d_recode_df %>% select(primary_data_language),
        tr_d_recode_df %>% select(lang)
    ) %>% deframe()

    pbn_books %>%
    select(`system-identifier`, title, doi, Author, Faculty, Institute,
           Department, Discipline, `publication-date`,
           `author-affiliated-to-unit`, `author-employed-in-unit`,
           `is-author`, `is-editor`, `is-conference`) %>%
    arrange(title) %>%
    mutate(Faculty    = recode(Faculty,    !!!tr_rg_recode_vec),
           Institute  = recode(Institute,  !!!tr_rg_recode_vec),
           Department = recode(Department, !!!tr_rg_recode_vec),
           Discipline = recode(Discipline, !!!tr_d_recode_vec),
           title = ifelse(!is.na(doi),
                          paste0('<a href="https://dx.doi.org/',
                                 doi,
                                 '" target="_blank">',
                                 title,
                                 '</a>'),
                          title)) %>%
    rename(!!tr_s$translations["dt-title", lang]                   := title,
           !!tr_s$translations["dt-author", lang]                  := Author,
           !!tr_s$translations["dt-research-group", lang]          := Department,
           !!tr_s$translations["dt-discipline", lang]              := Discipline,
           !!tr_s$translations["dt-year", lang]                    := `publication-date`) %>%
    mutate(!!tr_s$translations["dt-author-affiliated-short", lang] :=
               recode(as.character(`author-affiliated-to-unit`), !!!HTML_RECODE),
           !!tr_s$translations["dt-author-employed-short", lang] :=
               recode(as.character(`author-employed-in-unit`), !!!HTML_RECODE),
           !!tr_s$translations["dt-author-short", lang] :=
               recode(as.character(`is-author`), !!!HTML_RECODE),
           !!tr_s$translations["dt-editor-short", lang] :=
               recode(as.character(`is-editor`), !!!HTML_RECODE),
           !!tr_s$translations["dt-is-conference-short", lang] :=
               recode(as.character(`is-conference`), !!!HTML_RECODE)) %>%
    select(-doi)
},
simplify = FALSE)

pbn_books_years = fivenum(pbn_books$`publication-date`)[c(1, 5)]

scival_profiles = faculty_members %>%
    select(Author, Department, `Scopus Author ID`) %>%
    arrange(Author) %>%
    mutate(`Scopus Author ID` =
               ifelse(!is.na(`Scopus Author ID`),
                      paste0('<a href="https://www.scopus.com/authid/detail.uri?authorId=',
                             `Scopus Author ID`,
                             '" target="_blank">',
                             `Scopus Author ID`,
                             '</a>'),
                      NA))

scival_profiles_list = sapply(all_data_languages, function(lang)
{
    tr_rg_recode_vec = bind_cols(
        tr_rg_recode_df %>% select(primary_data_language),
        tr_rg_recode_df %>% select(lang)
    ) %>% deframe()

    scival_profiles %>%
        mutate(Department = recode(Department, !!!tr_rg_recode_vec)) %>%
        rename(!!tr_s$translations["dt-author", lang]         := Author,
               !!tr_s$translations["dt-research-group", lang] := Department,
               !!tr_s$translations["dt-scopus-id", lang]      := `Scopus Author ID`)
},
simplify = FALSE)

scival_stats_research_groups =
    read_csv("data/scival-stats-research-groups.csv",
             col_types = cols(
                Name               = col_character(),
                `Scholarly Output` = col_integer(),
                `Citation Count`   = col_integer()
             )) %>%
    left_join(faculty_members %>%
                select(Department, Discipline) %>%
                unique(),
              by = c("Name" = "Department")) %>%
    select(Name, Discipline, `Scholarly Output`, `Citation Count`)

scival_stats_research_groups_list = sapply(all_data_languages, function(lang)
{
    tr_rg_recode_vec = bind_cols(
        tr_rg_recode_df %>% select(primary_data_language),
        tr_rg_recode_df %>% select(lang)
    ) %>% deframe()

    tr_d_recode_vec = bind_cols(
        tr_d_recode_df %>% select(primary_data_language),
        tr_d_recode_df %>% select(lang)
    ) %>% deframe()

    scival_stats_research_groups %>%
        mutate(Name       = recode(Name,       !!!tr_rg_recode_vec),
               Discipline = recode(Discipline, !!!tr_d_recode_vec)) %>%
        arrange(factor(Name, levels =
                           c(unname(tr_rg_recode_vec)[1:3],
                             sort(tail(unname(tr_rg_recode_vec), -3))
                           ))) %>%
        rename(!!tr_s$translations["dt-research-group", lang]  := Name,
               !!tr_s$translations["dt-discipline", lang]      := Discipline,
               !!tr_s$translations["dt-publications", lang]    := `Scholarly Output`,
               !!tr_s$translations["dt-citations", lang]       := `Citation Count`) %>%
        mutate(text = paste0(
            "<b>",
            !!as.name(quo_name(tr_s$translations["dt-research-group", lang])),
            "</b><br>",
            tr_s$translations["dt-citations", lang],
            ": <b>",
            !!as.name(quo_name(tr_s$translations["dt-citations", lang])),
            "</b><br>",
            tr_s$translations["dt-publications", lang],
            ": <b>",
            !!as.name(quo_name(tr_s$translations["dt-publications", lang])),
            "</b>"))
},
simplify = FALSE)

scival_stats_researchers =
    read_csv("data/scival-stats-researchers.csv",
             col_types = cols(
                    Name               = col_character(),
                    Publications       = col_integer(),
                    Citations          = col_integer(),
                    `h-index`          = col_integer(),
                    `Scopus Author ID` = col_double()
             )) %>%
    left_join(faculty_members %>%
              select(`Scopus Author ID`, Discipline),
              by = c("Scopus Author ID")) %>%
    select(Name, Discipline, Publications, Citations, `h-index`) %>%
    arrange(desc(Citations))

scival_stats_researchers_list = sapply(all_data_languages, function(lang)
{
    tr_rg_recode_vec = bind_cols(
        tr_rg_recode_df %>% select(primary_data_language),
        tr_rg_recode_df %>% select(lang)
    ) %>% deframe()

    tr_d_recode_vec = bind_cols(
        tr_d_recode_df %>% select(primary_data_language),
        tr_d_recode_df %>% select(lang)
    ) %>% deframe()

    scival_stats_researchers %>%
        mutate(Discipline = recode(Discipline, !!!tr_d_recode_vec)) %>%
        rename(!!tr_s$translations["dt-author", lang]       := Name,
               !!tr_s$translations["dt-discipline", lang]   := Discipline,
               !!tr_s$translations["dt-publications", lang] := Publications,
               !!tr_s$translations["dt-citations", lang]    := Citations,
               !!tr_s$translations["dt-h-index", lang]      := `h-index`) %>%
        mutate(text = paste0(
            "<b>",
            !!as.name(quo_name(tr_s$translations["dt-author", lang])),
            "</b><br>",
            tr_s$translations["dt-citations", lang],
            ": <b>",
            !!as.name(quo_name(tr_s$translations["dt-citations", lang])),
            "</b><br>",
            tr_s$translations["dt-publications", lang],
            ": <b>",
            !!as.name(quo_name(tr_s$translations["dt-publications", lang])),
            "</b>"))
},
simplify = FALSE)

git_last_commit = tryCatch({
    format(as.POSIXct(commits(repository(), n = 1)[[1]]$author$when),
           tz="Europe/Warsaw",
           usetz = TRUE)},
    error = function(e) NULL)

rdat = list(
    data_info                    = data_info,
    pbn_min_year                 = pbn_min_year,
    pbn_vb_authors               = pbn_vb_authors,
    pbn_vb_articles              = pbn_vb_articles,
    pbn_vb_chapters              = pbn_vb_chapters,
    pbn_vb_books                 = pbn_vb_books,
    pbn_vb_points                = pbn_vb_points,
    pbn_journals_stats           = pbn_journals_stats,
    pbn_articles                 = pbn_articles_list,
    pbn_chapters                 = pbn_chapters_list,
    pbn_books                    = pbn_books_list,
    pbn_articles_points          = pbn_articles_points,
    pbn_articles_years           = pbn_articles_years,
    pbn_chapters_years           = pbn_chapters_years,
    pbn_books_years              = pbn_books_years,
    scival_stats_research_groups = scival_stats_research_groups_list,
    scival_stats_researchers     = scival_stats_researchers_list,
    scival_profiles              = scival_profiles_list,
    git_last_commit              = git_last_commit
)

rm(list = setdiff(ls(), "rdat"))

cat("Data loaded.", sep = "\n")
