
## 
## FUNCTION plot.map() to plot cartographical maps of the Roman Empire and the Mediterranean Basin
## (CC BY-SA 4.0) Antonio Rivero Ostoic, jaro@cas.au.dk 
##
## version 0.2.2 (17-04-2024)
##
## OPTIONAL PARAMETERS
##
## x       (char or vector, province or region acronym)
## type    (if x=NULL, type of c. map: plain, roman provinces, senatorial-imperial, tetrarchy, Mediterranean sea)
## settl   (optional and logical, display settlements?)
## roads   (optional and logical, display roads?)
## shipr   (optional and logical, display shipping routes?)
## main    (plot's title)
## compass (optional and logical, display compass?)
## cap     (only province or region, logical, display caption?)
## date    (only province or region, logical, display est date in caption?)
## name    (only province or region, logical, display name?)
## fsize   (title's font size)
## fcol    (title's font color)
## fsize2  (only province or region, date's font size)
## fcol2   (only province or region, date's font color)
## xd      (only province or region, x positioning for date)
## yd      (only province or region, y positioning for date)
## new     (optional, new plot as superimposed graphic?)
##


plot.map <-
function (x = NULL, type = c("plain", "rp", "si", "tetra", "med"), 
    settl, roads, shipr, main, compass, cap, date, name, fsize, 
    fcol, fsize2, fcol2, xd, yd, new, ...) 
{
    if (is.null(x) == TRUE) {
        if (!(exists("retn"))) {
            utils::data("retn", package = "sdam", envir = environment())
            retn <- get("retn", envir = environment())
        }
        else {
            invisible(NA)
        }
        if (missing(new) == FALSE && isTRUE(new == TRUE) == TRUE) {
            invisible(plot.new())
        }
        else {
            invisible(NA)
        }
        grid::grid.newpage()
        switch(match.arg(type), plain = {
            grid::grid.draw(x = grImport2::pictureGrob(picture = eval(parse(text = paste("retn$eucoast", 
                "[[1]]", sep = "")))))
        }, rp = {
            grid::grid.draw(x = grImport2::pictureGrob(picture = eval(parse(text = paste("retn$rpcoast", 
                "[[1]]", sep = "")))))
        }, si = {
            grid::grid.draw(x = grImport2::pictureGrob(picture = eval(parse(text = paste("retn$rpsi", 
                "[[1]]", sep = "")))))
        }, tetra = {
            grid::grid.draw(x = grImport2::pictureGrob(picture = eval(parse(text = paste("retn$rptetra", 
                "[[1]]", sep = "")))))
        }, med = {
            grid::grid.draw(x = grImport2::pictureGrob(picture = eval(parse(text = paste("retn$med", 
                "[[1]]", sep = "")))))
        })
        if (match.arg(type) != "med") {
            if (missing(roads) == FALSE && isTRUE(roads == TRUE) == 
                TRUE) {
                grid::grid.draw(x = grImport2::pictureGrob(picture = eval(parse(text = paste("retn$rds", 
                  "[[1]]", sep = "")))))
            }
            else {
                invisible(NA)
            }
            if (missing(shipr) == FALSE && isTRUE(shipr == TRUE) == 
                TRUE) {
                grid::grid.draw(x = grImport2::pictureGrob(picture = eval(parse(text = paste("retn$srs", 
                  "[[1]]", sep = "")))))
            }
            else {
                invisible(NA)
            }
            if (missing(settl) == FALSE && isTRUE(settl == TRUE) == 
                TRUE) {
                grid::grid.draw(x = grImport2::pictureGrob(picture = eval(parse(text = paste("retn$nds", 
                  "[[1]]", sep = "")))))
            }
            else {
                invisible(NA)
            }
        }
    }
    else {
        if (!(exists("rpmp"))) {
            utils::data("rpmp", package = "sdam", envir = environment())
            rpmp <- get("rpmp", envir = environment())
        }
        else {
            invisible(NA)
        }
        if (isTRUE(x %in% names(rpmp) == FALSE)) {
            message("Could not find \"x\"")
            opt <- options(show.error.messages = FALSE)
            on.exit(options(opt))
            stop()
        }
        else {
            invisible(NA)
        }
        grid::grid.newpage()
        grid::grid.draw(x = grImport2::pictureGrob(picture = eval(parse(text = paste(paste("rpmp", 
            x, sep = "$"), "[[1]]", sep = "")))))
        if ((missing(cap) == FALSE && isTRUE(cap == TRUE) == 
            TRUE) || (missing(date) == FALSE && isTRUE(date == 
            TRUE) == TRUE)) {
            if (!(exists("rpmcd"))) {
                utils::data("rpmcd", package = "sdam", envir = environment())
                rpmcd <- get("rpmcd", envir = environment())
                ifelse(missing(fsize2) == TRUE, fsize2 <- 8, 
                  invisible(NA))
                ifelse(missing(fcol2) == TRUE, fcol2 <- "#808080", 
                  invisible(NA))
            }
            else {
                invisible(NA)
            }
        }
        if (missing(cap) == FALSE && isTRUE(cap == FALSE) == 
            TRUE) {
            if (missing(date) == FALSE && isTRUE(date == TRUE) == 
                TRUE) {
                ifelse(missing(xd) == TRUE, xd <- 0.5, xd <- xd/10)
                ifelse(missing(yd) == TRUE, yd <- 0.1, yd <- yd/10)
                est <- eval(parse(text = paste(paste("rpmcd[[", 
                  which(names(rpmcd) %in% x), "]]", sep = ""), 
                  "provd", sep = "$")))
                grid::grid.text(paste("est.", est), x = xd, y = yd, 
                  gp = grid::gpar(fontsize = fsize2, col = fcol2))
            }
            else {
                invisible(NA)
            }
        }
        else {
            grid::pushViewport(grid::viewport(x = 1.165, y = -0.702, 
                w = 1.25, h = 1.25, just = c("center", "bottom")))
            grid::grid.draw(x = grImport2::pictureGrob(picture = eval(parse(text = paste(paste("rpmcd", 
                x, sep = "$"), "[[1]]", sep = "")))))
            grid::popViewport()
            if (missing(date) == FALSE && isTRUE(date == TRUE) == 
                TRUE) {
                ifelse(missing(xd) == TRUE, xd <- 0.7, xd <- xd/10)
                ifelse(missing(yd) == TRUE, yd <- 0.3, yd <- yd/10)
                est <- eval(parse(text = paste(paste("rpmcd[[", 
                  which(names(rpmcd) %in% x), "]]", sep = ""), 
                  "provd", sep = "$")))
                grid::grid.text(paste("est.", est), x = xd, y = yd, 
                  gp = grid::gpar(fontsize = fsize2, col = fcol2))
            }
            else {
                invisible(NA)
            }
        }
        if (missing(main) == TRUE) {
            if (missing(name) == FALSE && isTRUE(name == FALSE) == 
                TRUE) {
                invisible(NA)
            }
            else {
                ifelse(missing(fsize) == TRUE, fsize <- 20, invisible(NA))
                ifelse(missing(fcol) == TRUE, fcol <- "grey", 
                  invisible(NA))
                grid::grid.text(eval(parse(text = paste(paste("rpmp[[", 
                  which(names(rpmp) %in% x), "]]", sep = ""), 
                  "provn", sep = "$"))), x = 0.5, y = 0.966, 
                  gp = grid::gpar(fontsize = fsize, col = fcol))
            }
        }
        else {
            invisible(NA)
        }
        if (isTRUE(length(x) > 1) == TRUE) {
            for (k in seq_len(length(x) - 1)) {
                grid::grid.newpage()
                grid::grid.draw(x = grImport2::pictureGrob(picture = eval(parse(text = paste(paste("rpmp", 
                  x[k], sep = "$"), "[[1]]", sep = "")))))
                if (missing(cap) == FALSE && isTRUE(cap == FALSE) == 
                  TRUE) {
                  invisible(NA)
                }
                else {
                  grid::pushViewport(grid::viewport(x = 1.165, 
                    y = -0.7, w = 1.25, h = 1.25, just = c("center", 
                      "bottom")))
                  grid::grid.draw(x = grImport2::pictureGrob(picture = eval(parse(text = paste(paste("rpmcd", 
                    paste("c", x[k], sep = ""), sep = "$"), "[[1]]", 
                    sep = "")))))
                  grid::popViewport()
                  if (missing(date) == FALSE && isTRUE(date == 
                    TRUE) == TRUE) {
                    est <- eval(parse(text = paste(paste("rpmcd[[", 
                      which(names(rpmcd) %in% paste("c", x[k], 
                        sep = "")), "]]", sep = ""), "provd", 
                      sep = "$")))
                    grid::grid.text(paste("est.", est), x = 0.68, 
                      y = 0.3, gp = grid::gpar(fontsize = fsize2, 
                        col = fcol2))
                  }
                  else {
                    invisible(NA)
                  }
                }
                if (missing(name) == FALSE && isTRUE(name == 
                  FALSE) == TRUE) {
                  invisible(NA)
                }
                else {
                  grid::grid.text(eval(parse(text = paste(paste("rpmp[[", 
                    which(names(rpmp) %in% x[k]), "]]", sep = ""), 
                    "provn", sep = "$"))), x = 0.5, y = 0.966, 
                    gp = grid::gpar(fontsize = fsize, col = fcol))
                }
            }
            rm(k)
        }
        else {
            invisible(NA)
        }
    }
    if (missing(main) == TRUE) {
        invisible(NA)
    }
    else {
        ifelse(missing(fsize) == TRUE, fsize <- 15, invisible(NA))
        ifelse(missing(fcol) == TRUE, fcol <- "black", invisible(NA))
        if (is.null(x) == TRUE) {
            grid::grid.text(main, x = 0.5, y = 0.06, gp = grid::gpar(fontsize = fsize, 
                col = fcol))
        }
        else {
            grid::grid.text(main, x = 0.5, y = 0.966, gp = grid::gpar(fontsize = fsize, 
                col = fcol))
        }
    }
    if (missing(compass) == FALSE && isTRUE(compass == TRUE) == 
        TRUE) {
        utils::data("retn", package = "sdam", envir = environment())
        retn <- get("retn", envir = environment())
        grid::pushViewport(grid::viewport(x = 0.8, y = 0.85, 
            width = 0.11, height = 0.11, just = c("center", "bottom")))
        grid::grid.draw(x = grImport2::pictureGrob(picture = eval(parse(text = paste("retn$compass", 
            "[[1]]", sep = "")))))
        if ((missing(cap) == FALSE && isTRUE(cap == TRUE) == 
            TRUE) && (is.null(x) == FALSE)) {
            grid::pushViewport(grid::viewport(x = 1.75, y = -5.5, 
                w = 0.4, h = 0.4, just = c("center", "bottom")))
            grid::grid.draw(x = grImport2::pictureGrob(picture = eval(parse(text = paste("retn$compass", 
                "[[1]]", sep = "")))))
        }
        else {
            invisible(NA)
        }
    }
    else {
        invisible(NA)
    }
}
