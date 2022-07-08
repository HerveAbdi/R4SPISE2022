#' @title An empty function
#' @description As empty as can be
#' @param toto (NULL) a parameter
#' @export
#' @return Rien
#' @author Hervé Abdi
#' @rdname empty
#' @export
empty <- function(toto = NULL){
    if (is.null(toto)){ toto <- "Toto est un beau Bo toto"} # Titi est partito
    print(toto)
}
