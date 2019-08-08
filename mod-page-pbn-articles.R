mod_page_pbn_articles_ui = function(id)
{
    ns = NS(id)

    fluidPage(
        h2(textOutput(ns("page_title"))),
        fluidRow(
            box(width = 12,
                fluidRow(
                    column(4, uiOutput(ns("disciplines_rb"))),
                    column(4, uiOutput(ns("journal_lists_cb"))),
                    column(4, uiOutput(ns("points_sl")))
                ),
                fluidRow(
                    column(2, uiOutput(ns("affiliation_cb"))),
                    column(2, uiOutput(ns("employment_cb"))),
                    column(4, uiOutput(ns("conference_proceedings_cb"))),
                    column(4, uiOutput(ns("years_sl")))
                )
            )
        ),
        tabsetPanel(type = "tabs",
            tabPanel(textOutput(ns("tab_name_research_group")),
                fluidRow(
                    box(width = 12,
                        uiOutput(ns("rg_statistic_si"), style = "display: inline-block;"),
                        withSpinner(plotlyOutput(ns("research_groups_plot"),
                                                 height = "auto", width = "99%")),
                        textOutput(ns("note_multiple_authors_1"))
                    )
                )
            ),
            tabPanel(textOutput(ns("tab_name_persons")),
                fluidRow(
                    box(width = 12,
                        uiOutput(ns("pe_statistic_si"), style = "display: inline-block;"),
                        withSpinner(plotlyOutput(ns("persons_plot"),
                                                 height = "auto", width = "99%")),
                        textOutput(ns("note_multiple_authors_2"))
                    )
                )
            ),
            tabPanel(textOutput(ns("tab_name_points")),
                fluidRow(
                    box(width = 12,
                        withSpinner(plotlyOutput(ns("points_plot"),
                                                 height = "auto", width = "99%")),
                        textOutput(ns("note_multiple_authors_3"))
                    )
                )
            ),
            tabPanel(textOutput(ns("tab_name_list")),
                fluidRow(
                    box(dataTableOutput(ns("list_dt")), width = 12)
                )
            )
        )
    )
}

mod_page_pbn_articles_logic = function(input, output, session, i18n)
{
    output$page_title = renderText({
        i18n$t("tabname-pbn-articles")
    })

    output$tab_name_research_group = renderText({
        i18n$t("top-box-research-groups")
    })

    output$tab_name_persons = renderText({
        i18n$t("top-box-persons")
    })

    output$tab_name_points = renderText({
        i18n$t("top-box-points")
    })

    output$tab_name_list = renderText({
        i18n$t("top-box-list")
    })

    output$note_multiple_authors_1 = renderText({
        i18n$t("common-note-multiple-authors")
    })

    output$note_multiple_authors_2 = renderText({
        i18n$t("common-note-multiple-authors")
    })

    output$note_multiple_authors_3 = renderText({
        i18n$t("common-note-multiple-authors")
    })

    output$disciplines_rb = renderUI(
    {
        prettyRadioButtons(
            session$ns("pbn_articles_disciplines_rb"),
            i18n$t("top-box-disciplines"),
            setNames(c("all", "math", "cs"),
                     c(i18n$t("top-box-all-disciplines"),
                       i18n$t("top-box-mathematics"),
                       i18n$t("top-box-computer-science"))
            ),
            inline = TRUE,
            status = "danger"
        )
    })

    output$journal_lists_cb = renderUI(
    {
        prettyCheckboxGroup(
            session$ns("pbn_articles_journal_lists_cb"),
            i18n$t("label-mshe-list"),
            choiceNames = c(i18n$t("top-box-journal-list-a"),
                            i18n$t("top-box-journal-list-b"),
                            i18n$t("top-box-journal-list-c"),
                            i18n$t("top-box-journal-list-na")),
            choiceValues = c("A", "B", "C", "na"),
            selected     = c("A", "B", "C", "na"),
            inline = TRUE,
            status = "danger"
        )
    })

    output$affiliation_cb = renderUI(
    {
        prettyCheckbox(session$ns("pbn_articles_affiliation_cb"),
                      label = i18n$t("top-box-only-affiliated"),
                      value = TRUE,
                      status = "danger")
    })

    output$employment_cb = renderUI(
    {
        prettyCheckbox(session$ns("pbn_articles_employment_cb"),
                      label = i18n$t("top-box-employed workers"),
                      value = FALSE,
                      status = "danger")
    })

    output$conference_proceedings_cb = renderUI(
    {
        prettyCheckbox(session$ns("pbn_articles_conference_proceedings_cb"),
                      label = i18n$t("top-box-conference-proceedings"),
                      value = FALSE,
                      status = "danger")
    })

    output$points_sl = renderUI(
    {
        sliderInput(session$ns("pbn_articles_points_sl"),
                    NULL,
                    min = rdat$pbn_articles_points[1],
                    max = rdat$pbn_articles_points[2],
                    value = c(rdat$pbn_articles_points[1],
                              rdat$pbn_articles_points[2]),
                    step = 1,
                    post = i18n$t("top-box-points-short"),
                    sep = "")
    })

    output$years_sl = renderUI(
    {
        sliderInput(session$ns("pbn_articles_years_sl"),
                    NULL,
                    min = rdat$pbn_articles_years[1],
                    max = rdat$pbn_articles_years[2],
                    value = c(rdat$pbn_articles_years[1],
                              rdat$pbn_articles_years[2]),
                    step = 1,
                    sep = "")
    })

    output$years_sl = renderUI(
    {
        sliderInput(session$ns("pbn_articles_years_sl"),
                    NULL,
                    min = rdat$pbn_articles_years[1],
                    max = rdat$pbn_articles_years[2],
                    value = c(rdat$pbn_articles_years[1],
                              rdat$pbn_articles_years[2]),
                    step = 1,
                    sep = "")
    })

    output$rg_statistic_si = renderUI(
    {
        selectInput(session$ns("pbn_articles_rg_statistic_si"),
                    NULL,
                    setNames(c("n", "sum", "mean", "median", "max", "min"),
                             c(i18n$t("stats-no-of-articles"),
                               i18n$t("stats-points-sum"),
                               i18n$t("stats-points-mean"),
                               i18n$t("stats-points-median"),
                               i18n$t("stats-points-max"),
                               i18n$t("stats-points-min"))),
                    width = "160px")
    })

    output$pe_statistic_si = renderUI(
    {
        selectInput(session$ns("pbn_articles_pe_statistic_si"),
                    NULL,
                    setNames(c("n", "sum", "mean", "median", "max", "min"),
                             c(i18n$t("stats-no-of-articles"),
                               i18n$t("stats-points-sum"),
                               i18n$t("stats-points-mean"),
                               i18n$t("stats-points-median"),
                               i18n$t("stats-points-max"),
                               i18n$t("stats-points-min"))),
                    width = "160px")
    })

    empty_plotly = function(df)
    {
        p = ggplot() +
            annotate("text", x = 0, y = 0, size = 5,
                     label = i18n$t("common-no-data-to-plot")) +
            theme_void()

        ggplotly(p)
    }

    # https://stackoverflow.com/a/39877048
    int_breaks = function(x)
        unique(floor(pretty(seq(0, (max(x) + 1) * 1.1))))

    observeEvent(
    {
        input$pbn_articles_disciplines_rb
        input$pbn_articles_points_sl
        input$pbn_articles_years_sl
        input$pbn_articles_journal_lists_cb
        input$pbn_articles_affiliation_cb
        input$pbn_articles_employment_cb
        input$pbn_articles_conference_proceedings_cb
    },
    {
        df = rdat$pbn_articles[[i18n$translation_language]]
        df_disc = sort(unique(df[[i18n$t("dt-discipline")]]))

        journal_list_filter = input$pbn_articles_journal_lists_cb
        journal_list_na = FALSE
        if ("na" %in% journal_list_filter)
        {
            journal_list_filter = setdiff(journal_list_filter, "na")
            journal_list_na = TRUE
        }

        df = switch(input$pbn_articles_disciplines_rb,
                   "all" = df,
                   "math" = filter(df, !!as.name(quo_name(i18n$t("dt-discipline"))) ==
                                       i18n$t("discipline-mathematics")),
                   "cs"   = filter(df, !!as.name(quo_name(i18n$t("dt-discipline"))) ==
                                       i18n$t("discipline-computer-science"))) %>%
            filter(
                between(!!as.name(quo_name(i18n$t("dt-year"))),
                       input$pbn_articles_years_sl[1],
                       input$pbn_articles_years_sl[2]) &
                (between(!!as.name(quo_name(i18n$t("dt-points-short"))),
                        input$pbn_articles_points_sl[1],
                        input$pbn_articles_points_sl[2]) |
                    {if (input$pbn_articles_points_sl[1] == 0)
                        is.na(!!as.name(quo_name(i18n$t("dt-points-short"))))
                    else FALSE}) &
                (!!as.name(quo_name(i18n$t("dt-journal-ministerial-list"))) %in%
                    journal_list_filter |
                    {if (journal_list_na)
                        is.na(!!as.name(quo_name(i18n$t("dt-journal-ministerial-list"))))
                    else FALSE}) &
                { if (input$pbn_articles_affiliation_cb)
                    `author-affiliated-to-unit` == TRUE else TRUE } &
                { if (input$pbn_articles_employment_cb)
                    `author-employed-in-unit` == TRUE else TRUE } &
                { if (input$pbn_articles_conference_proceedings_cb)
                    `is-conference` == TRUE else TRUE }
            )

        output$research_groups_plot = renderPlotly(
        {
            if (is.null(input$pbn_articles_rg_statistic_si))
                return()

            if (nrow(df) == 0)
                return(empty_plotly(df))

            df_rg = {
                if (input$pbn_articles_disciplines_rb == "all") {
                    df %>%
                    select(`system-identifier`, Faculty,
                           !!as.name(quo_name(i18n$t("dt-points-short")))) %>%
                    unique() %>%
                    rename(Name = Faculty) %>%
                    mutate(RF   = i18n$t("group-faculty"),
                           ord = 1)
                } else {
                    NULL } } %>%
                bind_rows(df %>% select(`system-identifier`, Institute,
                                        !!as.name(quo_name(i18n$t("dt-points-short"))),
                                        !!as.name(quo_name(i18n$t("dt-discipline")))) %>%
                                 unique() %>%
                                 rename(Name = Institute,
                                        RF = !!as.name(quo_name(i18n$t("dt-discipline")))) %>%
                                 mutate(RF  = i18n$t("group-faculty"),
                                        ord = 2)) %>%
                bind_rows(df %>% select(`system-identifier`,
                                        !!as.name(quo_name(i18n$t("dt-research-group"))),
                                        !!as.name(quo_name(i18n$t("dt-points-short"))),
                                        !!as.name(quo_name(i18n$t("dt-discipline")))) %>%
                                 unique() %>%
                                 rename(Name = !!as.name(quo_name(i18n$t("dt-research-group"))),
                                        RF = !!as.name(quo_name(i18n$t("dt-discipline")))) %>%
                                 mutate(ord = 3)) %>%
                group_by(Name, RF, ord) %>%
                rename(Points := !!as.name(quo_name(i18n$t("dt-points-short")))) %>%
                mutate(Points = ifelse(is.na(Points), 0, Points)) %>%
                summarise(Stat_n      := n(),
                          Stat_sum    := sum(Points),
                          Stat_mean   := round(mean(Points), 1),
                          Stat_median := median(Points),
                          Stat_min    := min(Points),
                          Stat_max    := max(Points)
                          ) %>%
                arrange(!!as.name(quo_name(paste0("Stat_", input$pbn_articles_rg_statistic_si))),
                        desc(ord)) %>%
                ungroup() %>%
                mutate(RF = factor(RF, levels = c(i18n$t("group-faculty"),
                                                  sort(unique(df[[i18n$t("dt-discipline")]])))),
                       Name = factor(Name, levels = Name))

            p = ggplot(df_rg, aes(Name, !!as.name(quo_name(paste0("Stat_", input$pbn_articles_rg_statistic_si))),
                                  fill = RF)) +
                geom_col() +
                coord_flip() +
                labs(x = NULL, y = NULL) +
                theme_classic() +
                theme(legend.title = element_blank()) +
                geom_text(aes(label = !!as.name(quo_name(paste0("Stat_", input$pbn_articles_rg_statistic_si))),
                              y = !!as.name(quo_name(paste0("Stat_", input$pbn_articles_rg_statistic_si))) +
                                  max(df_rg[[paste0("Stat_", input$pbn_articles_rg_statistic_si)]])*0.01),
                          position = position_dodge(0.9),
                          hjust = 0) +
                scale_y_continuous(expand = c(0, 0),
                                   limits = c(0, max(df_rg[[paste0("Stat_",
                                                                   input$pbn_articles_rg_statistic_si)]])*1.1),
                                   breaks = int_breaks) +
                scale_fill_manual(values =
                    setNames(brewer.pal(5, "Set1")[1:(length(df_disc)+1)],
                             c(i18n$t("group-faculty"), df_disc)))

            ggplotly(p, tooltip = "none",
                     height = 200 + 20 * nrow(df_rg)) %>%
                style(textposition = "right")
        })

        output$persons_plot = renderPlotly(
        {
            if (is.null(input$pbn_articles_pe_statistic_si))
                return()

            if (nrow(df) == 0)
                return(empty_plotly(df))

            df_pe = df %>%
                select(`system-identifier`,
                       !!as.name(quo_name(i18n$t("dt-author"))),
                       !!as.name(quo_name(i18n$t("dt-discipline"))),
                       !!as.name(quo_name(i18n$t("dt-points-short")))) %>%
                rename(Name = !!as.name(quo_name(i18n$t("dt-author"))),
                       RF = !!as.name(quo_name(i18n$t("dt-discipline"))),
                       Points = !!as.name(quo_name(i18n$t("dt-points-short")))) %>%
                group_by(Name, RF) %>%
                mutate(Points = ifelse(is.na(Points), 0, Points)) %>%
                summarise(Stat_n      := n(),
                          Stat_sum    := sum(Points),
                          Stat_mean   := round(mean(Points), 1),
                          Stat_median := median(Points),
                          Stat_min    := min(Points),
                          Stat_max    := max(Points)
                          ) %>%
                arrange(!!as.name(quo_name(paste0("Stat_", input$pbn_articles_pe_statistic_si))), Name) %>%
                ungroup() %>%
                mutate(RF = factor(RF, levels = sort(unique(df[[i18n$t("dt-discipline")]]))),
                       Name = factor(Name, levels = Name))

            p = ggplot(df_pe, aes(Name, !!as.name(quo_name(paste0("Stat_", input$pbn_articles_pe_statistic_si))),
                                  fill = RF)) +
                geom_col() +
                coord_flip() +
                labs(x = NULL, y = NULL) +
                theme_classic() +
                theme(legend.title = element_blank()) +
                geom_text(aes(label = !!as.name(quo_name(paste0("Stat_", input$pbn_articles_pe_statistic_si))),
                              y = !!as.name(quo_name(paste0("Stat_", input$pbn_articles_pe_statistic_si))) +
                                  max(df_pe[[paste0("Stat_", input$pbn_articles_pe_statistic_si)]])*0.01),
                          position = position_dodge(0.9),
                          hjust = 0) +
                scale_y_continuous(expand = c(0, 0),
                                   limits = c(0, max(df_pe[[paste0("Stat_",
                                                                   input$pbn_articles_pe_statistic_si)]])*1.1),
                                   breaks = int_breaks) +
                scale_fill_manual(values =
                                      setNames(brewer.pal(5, "Set1")[2:(length(df_disc)+1)],
                                               df_disc))

            ggplotly(p, tooltip = "none",
                     height = 200 + 20 * nrow(df_pe)) %>%
                style(textposition = "right")
        })

        output$points_plot = renderPlotly(
        {
            if (nrow(df) == 0)
                return(empty_plotly(df))

            df_pts = {
                if (input$pbn_articles_disciplines_rb == "all") {
                    df %>%
                    select(`system-identifier`, Faculty, !!as.name(quo_name(i18n$t("dt-points-short")))) %>%
                    unique() %>%
                    rename(Name = Faculty) %>%
                    mutate(RF   = i18n$t("group-faculty"),
                           ord = 1)
                } else {
                    NULL } } %>%
                bind_rows(df %>% select(`system-identifier`, Institute,
                                        !!as.name(quo_name(i18n$t("dt-points-short"))),
                                        !!as.name(quo_name(i18n$t("dt-discipline")))) %>%
                                 unique() %>%
                                 rename(Name = Institute,
                                        RF = !!as.name(quo_name(i18n$t("dt-discipline")))) %>%
                                 mutate(ord = 2))  %>%
                mutate(!!as.name(quo_name(i18n$t("dt-points-short"))) :=
                           ifelse(!!as.name(quo_name(i18n$t("dt-points-short"))) < 15,
                                        "< 15",
                                  !!as.name(quo_name(i18n$t("dt-points-short"))))) %>%
                rename(Points = !!as.name(quo_name(i18n$t("dt-points-short")))) %>%
                group_by(RF, Points) %>%
                summarise(NoArt = n()) %>%
                ungroup() %>%
                mutate(Points := ifelse(is.na(Points),
                                        i18n$t("plot-journal-points-na"),
                                        Points)) %>%
                mutate(RF = factor(RF, levels = c(i18n$t("group-faculty"),
                                                  sort(unique(df[[i18n$t("dt-discipline")]])))),
                       Points = factor(Points, levels = c(i18n$t("plot-journal-points-na"),
                                                          "< 15",
                                                          unique(suppressWarnings(as.integer(Points))))))

            p = ggplot(df_pts, aes(Points,
                                   NoArt,
                                   fill = RF,
                                   text = NoArt)) +
                geom_bar(stat = "identity", position = "dodge") +
                theme_classic() +
                labs(x = i18n$t("top-box-points"),
                     y = i18n$t("tabname-pbn-articles")) +
                theme(legend.title = element_blank()) +
                scale_y_continuous(expand=c(0, 0)) +
                scale_fill_manual(values =
                    setNames(brewer.pal(5, "Set1")[1:(length(df_disc)+1)],
                             c(i18n$t("group-faculty"), df_disc)))

            ggplotly(p, tooltip = "text")
        })

        output$list_dt = renderDataTable(
        {
            df_l = df %>%
                select(-c(!!as.name(quo_name(i18n$t("dt-discipline"))),
                          `system-identifier`, `author-affiliated-to-unit`,
                          `author-employed-in-unit`,`is-conference`,
                          Faculty, Institute))

            datatable(df_l,
                      extensions = 'FixedHeader',
                      options = list(pageLength   = -1,
                                     lengthChange = FALSE,
                                     paginate     = FALSE,
                                     fixedHeader  = TRUE),
                      escape = FALSE,
                      selection = 'none')  %>%
            formatStyle(i18n$t("dt-author"), "white-space" = "nowrap") %>%
            formatStyle(i18n$t("dt-research-group"), fontSize = "8pt") %>%
            formatStyle(c(4:5, 7:10), 'text-align' = 'center')
        })
    })
}
