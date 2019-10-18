#' Lê os julgados baixados com baixar_julgados_trf2
#'
#' @param arquivos Vetor de arquivos
#' @param diretorio Se arquivos for NULL, informar diretório
#'
#' @return tibble com número do processo e julgado
#' @export
#'
#' @examples
#' \dontrun{
#' julgados <- ler_julgados_trf2(diretorio = "agencia/julgados")
#' }
#'
ler_julgados_trf2 <- function(arquivos = NULL, diretorio = "."){

  if (is.null(arquivos)){

    arquivos <- list.files(diretorio,pattern="pdf$",full.names = TRUE)

  }


  purrr::map_dfr(arquivos,purrr::possibly(~{

    processo <- stringr::str_extract(.x,"\\d{20}")

    julgado <- pdftools::pdf_text(.x) %>%
               stringr::str_c(collapse="\n\n\n") %>%
               stringr::str_trim()


    tibble::tibble(processo, julgado)

  },NULL))

}
