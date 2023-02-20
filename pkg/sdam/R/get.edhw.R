get.edhw <-
function (file = NULL, hd_nr, ...) 
{
    if (is.null(file) == FALSE) {
        smpl <- rjson::fromJSON(file = file)
    }
    else if (is.null(file) == TRUE) {
        smpl <- list()
        for (i in hd_nr) {
            smpl[[length(smpl) + 1L]] <- try(get.edh(hd_nr = i, 
                ...))
        }
        rm(i)
    }
    ifelse(missing(hd_nr) == FALSE && isTRUE(length(hd_nr) == 
        1) == TRUE, return(smpl[[1]]), return(smpl))
}
