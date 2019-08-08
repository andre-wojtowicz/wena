mod_page_scival_visualizations_ui = function(id)
{
    ns = NS(id)

    fluidPage(
        h2(textOutput(ns("page_title"))),
        fluidRow(
            box(width = 12,
                column(9, uiOutput(ns("disciplines_rb"))),
                column(3, span(textOutput(ns("scopus_db_snapshot"), inline = TRUE),
                               span(rdat$data_info %>%
                                     filter(database == "Scopus") %>%
                                     select(date) %>%
                                     pull)),
                       style = "text-align: right;")
            )
        ),
        tabsetPanel(type = "tabs",
            tabPanel(textOutput(ns("tab_name_research_group")),
                fluidRow(
                    box(width = 12,
                        withSpinner(plotlyOutput(ns("research_groups_plot"), height = "800px")),
                        dataTableOutput(ns("research_groups_dt")),
                        textOutput(ns("note_multiple_authors")),
                        textOutput(ns("note_self_citations_rg"))
                    )
                )
            ),
            tabPanel(textOutput(ns("tab_name_persons")),
                fluidRow(
                    box(withSpinner(plotlyOutput(ns("persons_plot"), height = "700px")),
                        uiOutput(ns("persons_plot_legend"),
                                 style = "text-align: center; margin-top: 10px;")
                    ),
                    box(dataTableOutput(ns("persons_dt")),
                        textOutput(ns("note_self_citations_pe"))
                    )
                )
            )
        )
    )
}

mod_page_scival_visualizations_logic = function(input, output, session, i18n)
{
    output$page_title = renderText({
        i18n$t("scival-visualizations-page-title")
    })

    output$scopus_db_snapshot = renderText({
        i18n$t("common-scopus-db-snapshot")
    })

    output$tab_name_research_group = renderText({
        i18n$t("top-box-research-groups")
    })

    output$tab_name_persons = renderText({
        i18n$t("top-box-persons")
    })

    output$note_multiple_authors = renderText({
        i18n$t("common-note-multiple-authors")
    })

    output$note_self_citations_rg = renderText({
        i18n$t("common-self-citations")
    })

    output$note_self_citations_pe = renderText({
        i18n$t("common-self-citations")
    })

    output$disciplines_rb = renderUI(
    {
        prettyRadioButtons(
            session$ns("scival_visualizations_disciplines_rb"),
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

    observeEvent(input$scival_visualizations_disciplines_rb,
    {
        df_rg = rdat$scival_stats_research_groups[[i18n$translation_language]]

        df_rg = switch(input$scival_visualizations_disciplines_rb,
                      "all" = df_rg,
                      "math" = filter(df_rg, !!as.name(quo_name(i18n$t("dt-discipline"))) ==
                                               i18n$t("discipline-mathematics")),
                      "cs"   = filter(df_rg, !!as.name(quo_name(i18n$t("dt-discipline"))) ==
                                               i18n$t("discipline-computer-science")))

        output$research_groups_plot = renderPlotly(
        {
            p = ggplot(df_rg, aes(text = text)) +
                geom_point(
                    aes(x     = !!as.name(quo_name(i18n$t("dt-citations"))),
                        y     = !!as.name(quo_name(i18n$t("dt-publications"))),
                        color = !!as.name(quo_name(i18n$t("dt-research-group"))),
                        shape = !!as.name(quo_name(i18n$t("dt-research-group")))
                    ),
                    size = 3
                ) +
                scale_shape_manual(values = seq(0, 25)) +
                theme(legend.title = element_blank())

            ggplotly(p, tooltip = "text")
        })

        output$research_groups_dt = renderDataTable(
        {
            datatable(df_rg %>% select(-text),
                      extensions = 'FixedHeader',
                      options = list(pageLength   = -1,
                                     lengthChange = FALSE,
                                     info         = FALSE,
                                     paginate     = FALSE,
                                     fixedHeader  = TRUE,
                                     searching    = FALSE),
                      selection = "none")
        })

        df_pe = rdat$scival_stats_researchers[[i18n$translation_language]]

        df_pe = switch(input$scival_visualizations_disciplines_rb,
                      "all" = df_pe,
                      "math" = filter(df_pe, !!as.name(quo_name(i18n$t("dt-discipline"))) ==
                                               i18n$t("discipline-mathematics")),
                      "cs"   = filter(df_pe, !!as.name(quo_name(i18n$t("dt-discipline"))) ==
                                               i18n$t("discipline-computer-science")))

        output$persons_plot = renderPlotly(
        {
            if (is.null(input$persons_dt_rows_selected))
                return()

            df_pe_filtered = df_pe %>%
                mutate(Alpha = ifelse(row_number() %in% input$persons_dt_rows_selected,
                                      1, 0.5),
                       Label = ifelse(row_number() %in% input$persons_dt_rows_selected,
                                      !!as.name(quo_name(i18n$t("dt-author"))), ""))

            p = ggplot(df_pe_filtered, aes(text = text)) +
            geom_point(aes(x     = !!as.name(quo_name(i18n$t("dt-citations"))),
                           y     = !!as.name(quo_name(i18n$t("dt-publications"))),
                           size  = !!as.name(quo_name(i18n$t("dt-h-index"))),
                           alpha = Alpha,
                           color = !!as.name(quo_name(i18n$t("dt-discipline"))))) +
            geom_text(aes(x     = !!as.name(quo_name(i18n$t("dt-citations"))),
                          y     = !!as.name(quo_name(i18n$t("dt-publications"))),
                          label = Label),
                      nudge_x = 0.75,
                      nudge_y = 0.75,
                      size    = 2.5) +
            scale_x_continuous(limits = c(0, max(df_pe_filtered[[i18n$t("dt-citations")]]) + 20)) +
            theme(legend.position = "none") +
            scale_color_manual(values = setNames(
                c(brewer.pal(5, "Set1")[5], brewer.pal(5, "Set1")[2]),
                c(i18n$t("discipline-computer-science"), i18n$t("discipline-mathematics"))))


            ggplotly(p, tooltip = "text") %>% style(textposition = "right")
        })

        output$persons_dt = renderDataTable(
        {
            datatable(df_pe %>% select(-text),
                      options = list(pageLength = 15),
                      selection = list(mode     = "multiple",
                                       selected = 1:5,
                                       target   = "row"))
        })
    })

    output$persons_plot_legend = renderUI(
    {
        span(span(HTML(
                  paste(span(HTML("&#9679"),
                             style = paste0("font-size: large; color: ",
                                            brewer.pal(5, "Set1")[2])),
                       i18n$t("discipline-mathematics"),
                       "&nbsp;&nbsp;",
                       span(HTML("&#9679"),
                            style = paste0("font-size: large; color: ",
                                           brewer.pal(5, "Set1")[5])),
                       i18n$t("discipline-computer-science"))
                 )),
            br(),
            span(span(HTML("&#9679 "), style = "font-size: small"),
                 span(HTML("&#9679 "), style = "font-size: large"),
                 span(HTML("&#9679 "), style = "font-size: x-large"),
                 i18n$t("common-h-index"))
        )
    })
}
