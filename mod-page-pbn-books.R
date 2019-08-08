mod_page_pbn_books_ui = function(id)
{
    ns = NS(id)

    fluidPage(
        h2(textOutput(ns("page_title"))),
        fluidRow(
            box(width = 12,
                fluidRow(
                    column(8, uiOutput(ns("disciplines_rb"))),
                    column(4, uiOutput(ns("years_sl")))
                ),
                fluidRow(
                    column(2, uiOutput(ns("affiliation_cb"))),
                    column(2, uiOutput(ns("employment_cb"))),
                    column(8, uiOutput(ns("conference_proceedings_cb")))
                ),
                fluidRow(
                    column(2, uiOutput(ns("authors_cb"))),
                    column(8, uiOutput(ns("editors_cb")))
                )
            )
        ),
        tabsetPanel(type = "tabs",
            tabPanel(textOutput(ns("tab_name_research_group")),
                fluidRow(
                    box(width = 12,
                        withSpinner(plotlyOutput(ns("research_groups_plot"),
                                                 height = "auto", width = "99%")),
                        textOutput(ns("note_multiple_authors_1"))
                    )
                )
            ),
            tabPanel(textOutput(ns("tab_name_persons")),
                fluidRow(
                    box(width = 12,
                        withSpinner(plotlyOutput(ns("persons_plot"),
                                                 height = "auto", width = "99%")),
                        textOutput(ns("note_multiple_authors_2"))
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

mod_page_pbn_books_logic = function(input, output, session, i18n)
{
    output$page_title = renderText({
        i18n$t("tabname-pbn-books")
    })

    output$tab_name_research_group = renderText({
        i18n$t("top-box-research-groups")
    })

    output$tab_name_persons = renderText({
        i18n$t("top-box-persons")
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

    output$disciplines_rb = renderUI(
    {
        prettyRadioButtons(
            session$ns("pbn_books_disciplines_rb"),
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

    output$affiliation_cb = renderUI(
    {
        prettyCheckbox(session$ns("pbn_books_affiliation_cb"),
                       label = i18n$t("top-box-only-affiliated"),
                       value = TRUE,
                       status = "danger")
    })

    output$employment_cb = renderUI(
    {
        prettyCheckbox(session$ns("pbn_books_employment_cb"),
                       label = i18n$t("top-box-employed workers"),
                       value = FALSE,
                       status = "danger")
    })

    output$conference_proceedings_cb = renderUI(
    {
        prettyCheckbox(session$ns("pbn_books_conference_proceedings_cb"),
                       label = i18n$t("top-box-conference-proceedings"),
                       value = FALSE,
                       status = "danger")
    })

    output$authors_cb = renderUI(
    {
        prettyCheckbox(session$ns("pbn_books_authors_cb"),
                       label = i18n$t("top-box-authors"),
                       value = FALSE,
                       status = "danger")
    })

    output$editors_cb = renderUI(
    {
        prettyCheckbox(session$ns("pbn_books_editors_cb"),
                       label = i18n$t("top-box-editors"),
                       value = FALSE,
                       status = "danger")
    })

    output$years_sl = renderUI(
    {
        sliderInput(session$ns("pbn_books_years_sl"),
                    NULL,
                    min = rdat$pbn_books_years[1],
                    max = rdat$pbn_books_years[2],
                    value = c(rdat$pbn_books_years[1],
                              rdat$pbn_books_years[2]),
                    step = 1,
                    sep = "")
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
        input$pbn_books_disciplines_rb
        input$pbn_books_years_sl
        input$pbn_books_affiliation_cb
        input$pbn_books_employment_cb
        input$pbn_books_authors_cb
        input$pbn_books_editors_cb
        input$pbn_books_conference_proceedings_cb
    },
    {
        df = rdat$pbn_books[[i18n$translation_language]]
        df_disc = sort(unique(df[[i18n$t("dt-discipline")]]))

        df = switch(input$pbn_books_disciplines_rb,
                   "all" = df,
                   "math" = filter(df, !!as.name(quo_name(i18n$t("dt-discipline"))) ==
                                       i18n$t("discipline-mathematics")),
                   "cs"   = filter(df, !!as.name(quo_name(i18n$t("dt-discipline"))) ==
                                       i18n$t("discipline-computer-science"))) %>%
            filter(between(!!as.name(quo_name(i18n$t("dt-year"))),
                           input$pbn_books_years_sl[1],
                           input$pbn_books_years_sl[2])) %>%
            { if (input$pbn_books_affiliation_cb)
                filter(., `author-affiliated-to-unit` == TRUE) else . } %>%
            { if (input$pbn_books_employment_cb)
                filter(., `author-employed-in-unit` == TRUE) else . } %>%
            { if (input$pbn_books_authors_cb)
                filter(., `is-author` == TRUE) else . } %>%
            { if (input$pbn_books_editors_cb)
                filter(., `is-editor` == TRUE) else . } %>%
            { if (input$pbn_books_conference_proceedings_cb)
                filter(., `is-conference` == TRUE) else . }

        output$research_groups_plot = renderPlotly(
        {
            if (nrow(df) == 0)
                return(empty_plotly(df))

            df_rg = {
                if (input$pbn_books_disciplines_rb == "all") {
                    df %>%
                        select(`system-identifier`, Faculty) %>%
                        unique() %>%
                        rename(Name = Faculty) %>%
                        mutate(RF   = i18n$t("group-faculty"),
                               ord = 1)
                } else {
                    NULL } } %>%
                bind_rows(df %>% select(`system-identifier`, Institute,
                                        !!as.name(quo_name(i18n$t("dt-discipline")))) %>%
                                 unique() %>%
                                 rename(Name = Institute,
                                        RF = !!as.name(quo_name(i18n$t("dt-discipline")))) %>%
                                 mutate(RF  = i18n$t("group-faculty"),
                                        ord = 2)) %>%
                bind_rows(df %>% select(`system-identifier`,
                                        !!as.name(quo_name(i18n$t("dt-research-group"))),
                                        !!as.name(quo_name(i18n$t("dt-discipline")))) %>%
                                 unique() %>%
                                 rename(Name = !!as.name(quo_name(i18n$t("dt-research-group"))),
                                        RF = !!as.name(quo_name(i18n$t("dt-discipline")))) %>%
                                 mutate(ord = 3)) %>%
                group_by(Name, RF, ord) %>%
                summarise(!!as.name(quo_name(i18n$t("tabname-pbn-books"))) := n()) %>%
                arrange(!!as.name(quo_name(i18n$t("tabname-pbn-books"))), desc(ord)) %>%
                ungroup() %>%
                mutate(RF = factor(RF, levels = c(i18n$t("group-faculty"),
                                                  sort(unique(df[[i18n$t("dt-discipline")]])))),
                       Name = factor(Name, levels = Name))

            p = ggplot(df_rg, aes(Name, !!as.name(quo_name(i18n$t("tabname-pbn-books"))),
                                  fill = RF)) +
                geom_col() +
                coord_flip() +
                labs(x = NULL) +
                theme_classic() +
                theme(legend.title = element_blank()) +
                geom_text(aes(label = !!as.name(quo_name(i18n$t("tabname-pbn-books"))),
                              y = !!as.name(quo_name(i18n$t("tabname-pbn-books"))) +
                                  max(df_rg[[i18n$t("tabname-pbn-books")]])*0.01),
                          position = position_dodge(0.9),
                          hjust = 0) +
                scale_y_continuous(expand = c(0, 0),
                                   limits = c(0, max(df_rg[[i18n$t("tabname-pbn-books")]])*1.1),
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
            if (nrow(df) == 0)
                return(empty_plotly(df))

            df_pe = df %>%
                select(`system-identifier`,
                       !!as.name(quo_name(i18n$t("dt-author"))),
                       !!as.name(quo_name(i18n$t("dt-discipline")))) %>%
                rename(Name = !!as.name(quo_name(i18n$t("dt-author"))),
                       RF = !!as.name(quo_name(i18n$t("dt-discipline")))) %>%
                group_by(Name, RF) %>%
                summarise(!!as.name(quo_name(i18n$t("tabname-pbn-books"))) := n()) %>%
                arrange(!!as.name(quo_name(i18n$t("tabname-pbn-books"))), Name) %>%
                ungroup() %>%
                mutate(RF = factor(RF, levels = sort(unique(df[[i18n$t("dt-discipline")]]))),
                       Name = factor(Name, levels = Name))

            p = ggplot(df_pe, aes(Name, !!as.name(quo_name(i18n$t("tabname-pbn-books"))),
                                  fill = RF)) +
                geom_col() +
                coord_flip() +
                labs(x = NULL) +
                theme_classic() +
                theme(legend.title = element_blank()) +
                geom_text(aes(label = !!as.name(quo_name(i18n$t("tabname-pbn-books"))),
                              y = !!as.name(quo_name(i18n$t("tabname-pbn-books"))) +
                                  max(df_pe[[i18n$t("tabname-pbn-books")]])*0.01),
                          position = position_dodge(0.9),
                          hjust = 0) +
                scale_y_continuous(expand = c(0, 0),
                                   limits = c(0, max(df_pe[[i18n$t("tabname-pbn-books")]])*1.1),
                                   breaks = int_breaks) +
                scale_fill_manual(values =
                                      setNames(brewer.pal(5, "Set1")[2:(length(df_disc)+1)],
                                               df_disc))

            ggplotly(p, tooltip = "none",
                     height = 200 + 20 * nrow(df_pe)) %>%
                style(textposition = "right")
        })

        output$list_dt = renderDataTable(
        {
            df_l = df %>%
                select(-c(!!as.name(quo_name(i18n$t("dt-discipline"))),
                          `system-identifier`, `author-affiliated-to-unit`,
                          `author-employed-in-unit`, `is-author`, `is-editor`,
                          `is-conference`, Faculty, Institute))

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
            formatStyle(4:9, 'text-align' = 'center')
        })
    })
}
