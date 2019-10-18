#' Lê htmls baixados por baixar_cjsg_trf2
#'
#' @param diretorio Default para o atual
#'
#' @return tibble com informações processuais
#' @export
#'
#' @examples
#' \dontrun{
#' ler_cjsg_trf2(diretorio = ".")
#' }
ler_cjsg_trf2_2 <- function(diretorio = "."){

arquivos <- list.files(path = diretorio, pattern = ".html", full.names = TRUE)

purrr::map_dfr(arquivos, ~ {

doc <- xml2::read_html(.x)

processo <- doc %>%
    xml2::xml_find_all("//span[@class='number_link']") %>%
    xml2::xml_text(trim=T)

texto_completo <- doc %>%
     xml2::xml_find_all("//span[1][@class='text_ementa']") %>%
     xml2::xml_text(trim=T)

ementa <- doc %>%
  xml2::xml_find_all("//span[2][@class='text_ementa']") %>%
  xml2::xml_text(trim=T)

classe <- doc %>%
  xml2::xml_find_all("//p[span[contains(.,'Classe')]]") %>%
  xml2::xml_text(trim=T)


orgao_julgador <- doc %>%
  xml2::xml_find_all("//span[@class='gsa-orgJulg']") %>%
  xml2::xml_text(trim=T)

relator <- doc %>%
  xml2::xml_find_all("//span[@class='valor gsa-relator']") %>%
  xml2::xml_text(trim=T)

data_decisao <- doc %>%
  xml2::xml_find_all("//li/span[contains(.,'Data de decis')]/following-sibling::span") %>%
  xml2::xml_text(trim=T)

data_disponibilizacao <- doc %>%
  xml2::xml_find_all("//li/span[contains(.,'Data de disponibiliza')]/following-sibling::span") %>%
  xml2::xml_text(trim=T)

tibble::tibble(processo,ementa,texto_completo,classe,orgao_julgador,relator,data_decisao,data_disponibilizacao) %>%
  dplyr::mutate(classe = stringr::str_remove(classe,"(?i)classe:"),
                data_decisao = lubridate::dmy(data_decisao),
                data_disponibilizacao = lubridate::dmy(data_disponibilizacao)) %>%
  tidyr::separate(processo,c("processo","trf2_processo"),sep = " \\(") %>%
  dplyr::mutate(trf2_processo = stringr::str_remove(trf2_processo,"\\)") %>%
                  stringr::str_remove("\\s?TRF2\\s?")) %>%
  dplyr::mutate_if(is.character,stringr::str_squish)
})
}

