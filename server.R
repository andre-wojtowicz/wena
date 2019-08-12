shinyServer(function(input, output, session)
{
    # language config-----------------------------------------------------------

    i18n = Translator$new(translation_json_path = "translation-site.json")
    i18n$set_translation_language(lang_default)
    options(DT.options = list(language = list(url = i18n$t("dt-language-url"))))

    runjs("get_browser_lang();")

    observeEvent(input$browser_lang,
    {
        val = NULL
        for (x in setdiff(i18n$languages, "id"))
        {
            if (startsWith(input$browser_lang, x))
            {
                val = x
                break
            }
        }
        if (is.null(val))
            val = lang_fallback

        i18n$set_translation_language(val)
        options(DT.options = list(language = list(url = i18n$t("dt-language-url"))))

        output$header_title = renderText(
        {
            i18n$t("page-title")
        })

        runjs(paste0("update_page_title('WENA − ",
                     i18n$t("page-title"),
                     "')")
        )

        callModule(mod_menu_language_logic, "menu_language", i18n)
        callModule(mod_menu_sidebar_logic, "menu_sidebar", i18n)

        # show content when page has loaded

        shinyjs::runjs("document.getElementsByClassName('main-header')[0].style.display = 'inherit';")
        shinyjs::runjs("document.getElementsByClassName('sidebar-toggle')[0].style.display = 'inherit';")
        shinyjs::removeClass(selector = "body", class = "sidebar-collapse")

        hide_waiter()
    })

    # language menu - click for language change --------------------------------

    sapply(setdiff(i18n$languages, "id"), function(x)
    {
        onclick(paste0("lang_", x), { set_cookie_and_update_lang(x) })
    })

    set_cookie_and_update_lang = function(val)
    {
        show_waiter(spin_folding_cube()) # hide content
        runjs(paste0("set_cookie('", val, "')"))
        i18n$set_translation_language(val)
        options(DT.options = list(language = list(url = i18n$t("dt-language-url"))))
        session$reload() # reload everything
    }

    # pages --------------------------------------------------------------------

    module_once_call = vector()

    observeEvent(input$sidebar_menu_tabs, ignoreNULL = TRUE, ignoreInit = TRUE,
    {
        if (input$sidebar_menu_tabs %in% module_once_call)
        {
            return()
        } else {
            module_once_call <<- c(module_once_call, input$sidebar_menu_tabs)
        }

        module_fun_name = sub("^tab_(.*)$", "mod_page_\\1_logic",
                              input$sidebar_menu_tabs)
        module_id = sub("^tab_(.*)$", "page_\\1", input$sidebar_menu_tabs)

        callModule(match.fun(module_fun_name), module_id, i18n)
    })
})
