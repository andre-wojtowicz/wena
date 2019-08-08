mod_page_scival_reports_ui = function(id)
{
    ns = NS(id)
    uiOutput(ns("out"))
}

mod_page_scival_reports_logic = function(input, output, session, i18n)
{
    output$out = renderUI({
        tagList(h1(tags$i(span(class="logo logo-scival")),
                paste(i18n$t("scival-reports-page-title"))),
                h2("2015-2018"),
                p(paste(i18n$t("common-scopus-db-snapshot"),
                        rdat$data_info %>%
                            filter(database == "Scopus") %>%
                            select(date) %>%
                            pull)),
                tags$ul(
                    tags$li(a(i18n$t("scival-reports-amu-m-cs"),
                              href="/raports/scival-uamwmi-matinf-2015-2018.pdf")),
                    tags$li(a(i18n$t("scival-reports-amu-m"),
                              href="/raports/scival-uamwmi-mat-2015-2018.pdf")),
                    tags$li(a(i18n$t("scival-reports-amu-cs"),
                              href="/raports/scival-uamwmi-inf-2015-2018.pdf")),
                    tags$li(a(i18n$t("scival-reports-put-fcs"),
                              href="/raports/scival-ppwi-2015-2018.pdf")),
                    tags$li(a(i18n$t("scival-reports-put-fcs-amu-fmcs"),
                              href="/raports/scival-ppwi-uamwmi-cs-2015-2018.pdf")),
                    tags$li(a(i18n$t("scival-reports-put-fcs-ics-amu-fmcs-cs"),
                              href="/raports/scival-ppwiii-uamwmiinf-cs-2015-2018.pdf"))
                )
        )
    })
}

