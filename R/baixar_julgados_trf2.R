#' Baixar inteiro teor do acórdão
#'
#' @param urls As urls podem ser obtidas da tibble lida
#'     por meio da função ler_cjsg_trf2
#' @param diretorio atual se não informado
#'
#'
#' @return baixa pdfs no diretorio indicado
#' @export
#'
#' @examples
#' \dontrun{
#' url <- "http://jurisprudencia.trf2.jus.br/sm/download?name=apolo-inteiro-teor&id=2019,08,02,00084163220184020000_2257151.pdf"
#' baixar_julgados_trf2(url)
#' }
baixar_julgados_trf2<-function(urls, diretorio = "."){


  purrr::walk(urls,purrr::possibly(~{

  processo <- stringr::str_extract(.x,"\\d{20}")

  arquivo <- stringr::str_c(diretorio,
                            "/",
                            stringr::str_replace_all(Sys.time(),"\\D+","_"),
                            "_julgado_processo_",
                            processo,
                            ".pdf")

  httr::GET(.x,httr::write_disk(arquivo,overwrite=TRUE))

  },NULL))


}
