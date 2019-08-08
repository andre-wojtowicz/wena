mod_page_site_information_ui = function(id)
{
    ns = NS(id)
    uiOutput(ns("out"))
}

mod_page_site_information_logic = function(input, output, session, i18n)
{
    output$out = renderUI({
        div(style="max-width: 700px; text-align: justify",
             h2(i18n$t("site-information-page-title")),
             p(HTML(paste0(
                 i18n$t("site-information-text-1"),
                 tags$i(i18n$t("site-information-text-2")),
                 i18n$t("site-information-text-3"),
                 tags$a(i18n$t("site-information-text-4"),
                        href="https://pbn.nauka.gov.pl/pbn-report-web/"),
                 i18n$t("site-information-text-5"),
                 tags$a(i18n$t("site-information-text-6"), href="https://www.scival.com/"),
                 i18n$t("site-information-text-7")))),
             tags$ul(
                 tags$li(i18n$t("site-information-text-8")),
                 tags$li(i18n$t("site-information-text-9"))
             ),
             p(i18n$t("site-information-text-10")),
             p(i18n$t("site-information-text-11")),
             p(i18n$t("site-information-text-12")),
             p(i18n$t("site-information-text-13"))
        )
    })
}




