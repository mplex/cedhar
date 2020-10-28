
## 
## WRAPPER FUNCTION get.edhw() to perform several queries from the EDH database API
## (CC BY-SA 4.0) Antonio Rivero Ostoic, jaro@cas.au.dk 
##
## version 0.2.0 (27-04-2020)
##
## PARAMETER
## hd_nr (HD-No of inscription)


get.edhw <-
function (hd_nr, ...) 
{
    smpl <- list()
    for (i in hd_nr) {
        smpl[[length(smpl) + 1L]] <- try(get.edh(hd_nr = i, ...))
    }
    return(smpl)
}
