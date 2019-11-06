#' Baixa decisões de segunda instância do TRF2
#'
#' @param livre busca livre
#' @param aspas TRUE para colocar a busca entre aspas
#' @param data_inicial formato "dd/mm/aaaa"
#' @param data_final formato "dd/mm/aaaa"
#' @param diretorio Default para atual.
#'
#' @return Baixa os htmls nos diretório especificado
#' @export
#'
#' @examples
#' \dontrun{
#' baixar_cjsg_trf2(
#'   livre = "agência nacional", aspas = TRUE,
#'   data_inicial = "10/07/2019", data_final = "31/07/2019"
#' )
#' }
baixar_cjsg_trf2 <- function(livre = "", aspas = FALSE, data_inicial = NULL,data_final = NULL, diretorio = "."){

if (aspas ==  TRUE){

  livre <- deparse(livre)

}

df <-inicial_final(data_inicial,data_final) %>%
  dplyr::mutate_all(lubridate::dmy)

purrr::walk2(df$data_inicial,df$data_final,~{


periodo <- paste0("inmeta:DataDecisao:daterange:",.x,"..",.y)
start <- "0"

url_parseada <-
  structure(
    list(
      scheme = "https",
      hostname = "www10.trf2.jus.br",
      port = NULL,
      path = "consultas/",
      query = NULL,
      params = NULL,
      fragment = NULL,
      username = NULL,
      password = NULL
    ),
    class = "url"
  )

query <-
  list(
    proxystylesheet = "v2_index",
    getfields = "*",
    entqr = "3",
    lr = "lang_pt",
    ie = "UTF-8",
    oe = "UTF-8",
    requiredfields = "(-sin_proces_sigilo_judici:s).(-sin_sigilo_judici:s)",
    sort = "date:D:S:d1",
    entsp = "a",
    adv = "1",
    base = "JP-TRF",
    ulang = "",
    access = "p",
    entqrm = "0",
    wc = "200",
    wc_mc = "0",
    ud = "1",
    client = "v2_index",
    filter = "0",
    as_q = periodo,
    q = livre,
    start = start,
    site = "v2_jurisprudencia"
  )


url_parseada$query <- query
url <- httr::build_url(url_parseada)

#class(url) <- "url"

paginas <- httr::RETRY("GET",url) %>%
           httr::content() %>%
           xml2::xml_find_all("//ul[@id='attr_1']//span[@class='dn-attr-c']") %>%
           xml2::xml_text(trim=TRUE) %>%
           stringr::str_extract("\\d+") %>%
           as.numeric() %>%
           sum() %>%
           seq(0,.,1)

purrr::walk(paginas,~{

url_parseada$query$start <- .x
url <- httr::build_url(url_parseada)

arquivo <- paste0("_pagina_", .x, ".html")

httr::RETRY("GET", url, httr::timeout(30),
            httr::write_disk(file.path(diretorio, Sys.time() %>%
                                         stringr::str_replace_all("\\D+", "_") %>%
                                         stringr::str_replace("$", arquivo))))



})
})
}
