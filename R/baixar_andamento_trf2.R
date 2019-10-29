#' Baixar movimentação processual
#'
#' @param codigo_processo Código do processo
#' @param diretorio Se não informado, atual
#'
#' @return html
#' @export
#'
#' @examples
#' \dontrun{
#' baixar_andamento_tf2(codigo_processo = 964285, diretorio = ".")
#' }
baixar_andamento_trf2 <- function(codigo_processo = NULL , diretorio = "."){

  purrr::walk(codigo_processo,purrr::possibly(~{

    url<- paste0("http://portal.trf2.jus.br/portal/consulta/resinfomov2.asp?CodDoc=",.x)

    arquivo <- stringr::str_replace_all(Sys.time(),"\\D","_") %>%
               paste0(diretorio,"/",.,"_andamento_",.x,".html")

    httr::GET(url,httr::write_disk(arquivo,overwrite = TRUE))

  },NULL))


}

