// https://stackoverflow.com/a/15724300
function get_cookie(name)
{
    var value = "; " + document.cookie;
    var parts = value.split("; " + name + "=");
    if (parts.length == 2) return parts.pop().split(";").shift();
}

function set_cookie(val)
{
    var d = new Date();
    d.setTime(d.getTime() + (2*365*24*60*60*1000));
    var expires = 'expires='+ d.toUTCString();
    document.cookie = 'lang=' + val + ';' + expires + ';path=/';
}

function get_browser_lang()
{
    var user_lang = window.navigator.userLanguage || window.navigator.language;
    var cookie_lang = get_cookie('lang');
    Shiny.onInputChange('browser_lang', cookie_lang || user_lang);
}

function update_page_title(val)
{
    document.title = val;
}
