mod_page_scival_profiles_ui = function(id)
{
    ns = NS(id)
    uiOutput(ns("out"))
}

mod_page_scival_profiles_logic = function(input, output, session, i18n)
{
    output$out = renderUI({
        dt = datatable(rdat$scival_profiles[[i18n$translation_language]],
                       extensions = 'FixedHeader',
                       escape     = FALSE,
                       selection  = 'none',
                       options    = list(pageLength   = -1,
                                         lengthChange = FALSE,
                                         info         = FALSE,
                                         paginate     = FALSE,
                                         fixedHeader  = TRUE))

        tagList(h2(i18n$t("scival-profiles-page-title")),
                fluidRow(box(renderDataTable({dt}), width = 12))
        )
    })
}

