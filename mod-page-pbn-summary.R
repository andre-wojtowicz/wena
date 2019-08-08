mod_page_pbn_summary_ui = function(id)
{
    ns = NS(id)
    fluidPage(
        uiOutput(ns("vb_header")),
        uiOutput(ns("vb_subheader"), style="text-align: center;"),
        fluidRow(
            valueBoxOutput(ns("vb_pbn_articles")),
            valueBoxOutput(ns("vb_pbn_chapters")),
            valueBoxOutput(ns("vb_pbn_books")),
            valueBoxOutput(ns("vb_pbn_journal_stats_A")),
            valueBoxOutput(ns("vb_pbn_journal_stats_B")),
            valueBoxOutput(ns("vb_pbn_journal_stats_C")),
            valueBoxOutput(ns("vb_pbn_points")),
            valueBoxOutput(ns("vb_pbn_authors")),
            valueBoxOutput(ns("vb_pbn_date"))
        )
    )
}

mod_page_pbn_summary_logic = function(input, output, session, i18n)
{
    output$vb_header = renderUI({
        tagList(
            h1(tags$i(span(class="logo logo-pbn")),
               paste(i18n$t("pbn-summary-page-title-1"),
                     rdat$pbn_min_year,
                     i18n$t("pbn-summary-page-title-2"))),
            hr()
        )
    })

    output$vb_subheader = renderUI({
        tagList(
            h3(i18n$t("pbn-summary-page-title-3")),
            hr()
        )
    })

    output$vb_pbn_articles = renderValueBox({
        valueBox(
            value    = rdat$pbn_vb_articles,
            subtitle = i18n$t("pbn-summary-vb-articles"),
            icon     = icon("file", lib = "glyphicon"),
            color    = "aqua"
        )
    })

    output$vb_pbn_chapters = renderValueBox({
        valueBox(
            value    = rdat$pbn_vb_chapters,
            subtitle = i18n$t("pbn-summary-vb-chapters"),
            icon     = icon("duplicate", lib = "glyphicon"),
            color    = "blue"
        )
    })

    output$vb_pbn_books = renderValueBox({
        valueBox(
            value    = rdat$pbn_vb_books,
            subtitle =i18n$t("pbn-summary-vb-books"),
            icon     = icon("book", lib = "glyphicon"),
            color    = "light-blue"
        )
    })

    output$vb_pbn_points = renderValueBox({
        valueBox(
            value    = rdat$pbn_vb_points,
            subtitle = i18n$t("pbn-summary-vb-points"),
            icon     = icon("star"),
            color    = "maroon"
        )
    })

    output$vb_pbn_authors = renderValueBox({
        valueBox(
            value    = rdat$pbn_vb_authors,
            subtitle = i18n$t("pbn-summary-vb-authors"),
            icon     = icon("users"),
            color    = "orange"
        )
    })

    output$vb_pbn_date = renderValueBox({
        valueBox(
            value    = rdat$data_info %>%
                       filter(database == "PBN") %>%
                       select(date) %>%
                       pull,
            subtitle = i18n$t("pbn-summary-vb-pbn-state"),
            icon     = icon("calendar"),
            color    = "purple"
        )
    })

    output$vb_pbn_journal_stats_A = renderValueBox({
        valueBox(
            value    = rdat$pbn_journals_stats %>%
                       filter(`journal-ministerial-list` == "A") %>%
                       select(count) %>%
                       pull(),
            subtitle = i18n$t("pbn-summary-vb-pbn-journal-stats-a"),
            icon     = icon("star"),
            color    = "green"
        )
    })

    output$vb_pbn_journal_stats_B = renderValueBox({
        valueBox(
            value    = rdat$pbn_journals_stats %>%
                       filter(`journal-ministerial-list` == "B") %>%
                       select(count) %>%
                       pull(),
            subtitle = i18n$t("pbn-summary-vb-pbn-journal-stats-b"),
            icon     = icon("star"),
            color    = "olive"
        )
    })

    output$vb_pbn_journal_stats_C = renderValueBox({
        valueBox(
            value    = rdat$pbn_journals_stats %>%
                       filter(`journal-ministerial-list` == "C") %>%
                       select(count) %>%
                       pull(),
            subtitle = i18n$t("pbn-summary-vb-pbn-journal-stats-c"),
            icon     = icon("star"),
            color    = "teal"
        )
    })
}
