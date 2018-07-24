#' loadAllChunksCache
#'
#' @param fileContainingChunks The path to your Rmd file
#' @param pathForCache The path to the cache if not by default
#'
#' @return messages with the name of the loaded chunks
#' @export
#'
#' @examples
#' \dontrun{
#' loadAllChunksCache("input.Rmd")
#' }
loadAllChunksCache <- function(fileContainingChunks,
                               pathForCache = 
                                   paste0(sub(".Rmd", "",
                                              fileContainingChunks),
                                          "_cache/",
                                          names(which(sapply(c(
                                              "html", "pdf", "word"),
                                              function(x)dir.exists(paste0(sub(
                                                  ".Rmd", "",
                                                  fileContainingChunks),
                                                  "_cache/", x, "/")))))[1],
                                          "/")){
    lignesDeNomDeChunk <- grep("^```{r", readLines(fileContainingChunks),
                               perl = TRUE, value = TRUE)
    nomsDeChunks <- sapply(strsplit(
        sapply(strsplit(
            sapply(strsplit(lignesDeNomDeChunk, "{r ", perl = TRUE),
                   function(x) x[[2]]),
            ","), function(y) y[[1]]),
        "}"), function(z) z[[1]])
    chunkFiles <- unlist(lapply(1:length(nomsDeChunks), function(i){
        fichier <- grep(".RData", list.files(path = pathForCache,
                                             pattern = paste0("^",
                                                              nomsDeChunks[i],
                                                              "_"),
                                             full.names = TRUE), value = TRUE)
        if (length(fichier) >= 1) {
            names(fichier) <- nomsDeChunks[i]
            return(fichier)
        }
    }))
    for (j in chunkFiles) {
        message(j)
        get(load(j, .GlobalEnv))
    }
}