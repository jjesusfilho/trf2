#' Lê metadados do resultado de baixar_cjsg_trf2
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Se arquivos for NULL, indicar diretório.
#'
#' @return Tibble com informações processuais
#' @export
#'
#' @examples
#' \dontrun{
#' ler_cjsg_trf2(diretorio = ".")
#' }
ler_cjsg_trf2 <- function(arquivos = NULL, diretorio = "."){

if (is.null(arquivos)){

  arquivos <- list.files("diretorio","html",full.names=TRUE)

}

purrr::map_dfr(arquivos,purrr::possibly(~{

x <- xml2::read_html(.x)

processo <- x %>%
xml2::xml_find_all("//*[@class='number_link']") %>%
xml2::xml_text()

classe <- x %>%
xml2::xml_find_all("//*[@class='recurso'][1]/p") %>%
xml2::xml_text() %>%
  stringr::str_remove("Classe: ")

data_decisao <- x %>%
xml2::xml_find_all("//*[@class='data-relator']//li[@class='espacado dleft'][1]") %>%
xml2::xml_text()  %>%
stringr::str_remove("(\\p{L}|\\s)+") %>%
readr::parse_date(format = "%d/%m/%Y")

data_disponibilizacao <- x %>%
xml2::xml_find_all("//*[@class='data-relator']//li[@class='espacado dleft'][2]/span[@class='valor']") %>%
xml2::xml_text()  %>%
readr::parse_date(format = "%d/%m/%Y")

relator <- x %>%
xml2::xml_find_all("//*[@class='data-relator']//li[@class='espacado']/span[@class='valor gsa-relator']") %>%
xml2::xml_text()

ementa1 <- x %>%
xml2::xml_find_all("//*[@class='doc-text']/span[@class='text_ementa'][1]") %>%
xml2::xml_text()

ementa2 <- x %>%
xml2::xml_find_all("//*[@class='ementa display-none']") %>%
xml2::xml_text()

ementa <- stringr::str_c(ementa1,ementa2,sep=" ")

orgao_julgador <- x %>%
xml2::xml_find_all("//*[@class='recurso'][2]/p") %>%
xml2::xml_text() %>%
stringr::str_remove(".rg.o\\sjulgador:\\s")

link_julgado <- x %>%
xml2::xml_find_all("//*[@class='tipo_doc']//a[contains(.,'Inteiro teor')]") %>%
xml2::xml_attr("href")

tibble::tibble(processo, classe,orgao_julgador,relator,data_decisao,data_disponibilizacao, ementa,link_julgado) %>%
tidyr::separate(processo,c("processo","trf2_processo"),sep = " \\(") %>%
  dplyr::mutate(trf2_processo = stringr::str_remove(trf2_processo,"\\)") %>%
                  stringr::str_remove("\\s?TRF2\\s?"))
},NULL))

}


