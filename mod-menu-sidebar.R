mod_menu_sidebar_ui = function(id)
{
    ns = NS(id)
    sidebarMenuOutput(ns("out"))
}

mod_menu_sidebar_logic = function(input, output, session, i18n)
{
    output$out = renderMenu(
    {
        sidebarMenu(
            id = "sidebar_menu_tabs",
            menuItem(i18n$t("tabname-pbn"),
                     icon = tags$i(span(class = "icon icon-pbn")),
                     startExpanded = TRUE,
                     menuSubItem(i18n$t("tabname-pbn-summary"),
                                 tabName = "tab_pbn_summary"),
                     menuSubItem(i18n$t("tabname-pbn-articles"),
                                 tabName = "tab_pbn_articles"),
                     menuSubItem(i18n$t("tabname-pbn-chapters"),
                                 tabName = "tab_pbn_chapters"),
                     menuSubItem(i18n$t("tabname-pbn-books"),
                                 tabName = "tab_pbn_books")
            ),
            menuItem(i18n$t("tabname-scival"),
                     icon = tags$i(span(class="icon icon-scival")),
                     startExpanded = TRUE,
                     menuSubItem(i18n$t("tabname-scival-reports"),
                                 tabName = "tab_scival_reports"),
                     menuSubItem(i18n$t("tabname-scival-visualizations"),
                                 tabName = "tab_scival_visualizations"),
                     menuSubItem(i18n$t("tabname-scival-profiles"),
                                 tabName = "tab_scival_profiles")
            ),
            menuItem(i18n$t("tabname-site-information"),
                     icon = tags$i(span(class="icon icon-info")),
                     tabName = "tab_site_information"),
            div(span(i18n$t("sidebar-git-lastupdate")),
                br(),
                span(rdat$git_last_commit), class = "last-update"),
            div(span(a(i18n$t("sidebar-git-host"),
                       href = "https://github.com/andre-wojtowicz/wena",
                       target = "about_blank")),
                class = "last-update")
        )
    })
}
