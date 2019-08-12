dashboardPage(
    title = "WENA",
    skin = "red",
    # hide header and sidebar by default  (DOMContentLoaded event bug)
    dashboardHeader(title = textOutput("header_title"),
                    titleWidth = 275,
                    mod_menu_language_ui("menu_language"),
                    disable = TRUE
    ),
    dashboardSidebar(
        width = 275,
        mod_menu_sidebar_ui("menu_sidebar"),
        disable = TRUE
    ),
    dashboardBody(
        use_waiter(),
        show_waiter_on_load(spin_folding_cube()),
        useShinyjs(),
        includeCSS("www/styles.css"),
        includeScript("www/scripts.js"),
        tabItems(
            tabItem("tab_pbn_summary",           mod_page_pbn_summary_ui("page_pbn_summary")),
            tabItem("tab_pbn_articles",          mod_page_pbn_articles_ui("page_pbn_articles")),
            tabItem("tab_pbn_chapters",          mod_page_pbn_chapters_ui("page_pbn_chapters")),
            tabItem("tab_pbn_books",             mod_page_pbn_books_ui("page_pbn_books")),
            tabItem("tab_scival_reports",        mod_page_scival_reports_ui("page_scival_reports")),
            tabItem("tab_scival_visualizations", mod_page_scival_visualizations_ui("page_scival_visualizations")),
            tabItem("tab_scival_profiles",       mod_page_scival_profiles_ui("page_scival_profiles")),
            tabItem("tab_site_information",      mod_page_site_information_ui("page_site_information"))
        )
    )
)
