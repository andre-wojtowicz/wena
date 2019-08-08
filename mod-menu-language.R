mod_menu_language_ui = function(id)
{
    ns = NS(id)
    dropdownMenuOutput(ns("out"))
}

mod_menu_language_logic = function(input, output, session, i18n)
{
    output$out = renderMenu(
    {
        possible_langs = setdiff(i18n$languages, c("id", i18n$translation_language))

        lang_list = lapply(possible_langs, function(x)
        {
            tags$li(a(href = "#", id = paste0("lang_", x),
                      tags$i(span(class = paste0("lang-icon lang-icon-", x))),
                      i18n$translations["language-name", x]))
        })

        current_language_icon = tags$i(span(class = paste0("lang-icon lang-icon-",
                                                           i18n$translation_language)))

        tags$li(class = "dropdown notifications-menu",
                style = "width: auto",
                a(href = "#",
                  class = "dropdown-toggle",
                  `data-toggle` = "dropdown",
                  current_language_icon),
                tags$ul(class = "dropdown-menu",
                        tags$li(tags$ul(class = "menu", lang_list))
                )
        )
    })
}
