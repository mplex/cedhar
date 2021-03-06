
## 
## FUNCTION request() to get data API from a given server
## (CC BY-SA 4.0) Antonio Rivero Ostoic, jaro@cas.au.dk 
##
## Aimed to interact first with DEiC's sciencedata.dk
## version 0.1 (19-01-2020)
##
## Parameters
## file (object under 'method')
## URL (protocol and domain of the url)
## method (the http "verb" for the object)
##        "GET" (list)
##        "POST" (place)
##        "PUT" (update)
##        "DELETE" (cancel)
## authenticate (logical, use basic authentication?)
## path (optional, add path to the url)
## 
## Additional parameters:
## cred (vector for username and password credentials)
## subdomain (optional, add subdomain to the url)
## ... (extra parameters if required)


request <-
function (file, URL = "https://sciencedata.dk", method = c("GET", 
    "POST", "PUT", "DELETE"), authenticate = TRUE, cred = NULL, 
    path = "/files", subdomain = NULL, ...) 
{
    require("httr")
    ifelse(is.null(subdomain) == FALSE, URL <- gsub("//", paste0("//", 
        subdomain, sep = "."), URL), NA)
    ifelse(isTRUE(strsplit(path, "")[[1]][1] != "/") == TRUE, 
        path <- paste0("/", path, sep = ""), NA)
    ifelse(isTRUE(strsplit(path, "")[[1]][length(strsplit(path, 
        "")[[1]])] != "/") == TRUE, path <- paste0(path, "/", 
        sep = ""), NA)
    URL <- paste0(URL, path, "/", sep = "")
    if (isTRUE(authenticate) == TRUE && is.null(cred) == TRUE) {
        getLoginDetails <- function() {
    # http://r.789695.n4.nabble.com/tkentry-that-exits-after-RETURN-tt854721.html
            require("tcltk")
            tt <- tktoplevel()
            tkwm.title(tt, "login credentials")
            Name <- tclVar("username")
            Password <- tclVar("password")
            entry.Name <- tkentry(tt, width = "25", textvariable = Name)
            entry.Password <- tkentry(tt, width = "25", show = "-", 
                textvariable = Password)
            tkgrid(tklabel(tt, text = "Enter your login details."))
            tkgrid(entry.Name)
            tkgrid(entry.Password)
            OnOK <- function() {
                tkdestroy(tt)
            }
            OK.but <- tkbutton(tt, text = " OK ", command = OnOK)
            tkbind(entry.Password, "<Return>", OnOK)
            tkgrid(OK.but)
            tkfocus(tt)
            tkwait.window(tt)
            invisible(c(loginID = tclvalue(Name), password = tclvalue(Password)))
        }
        cred <- getLoginDetails()
    }
    else {
        cred <- cred
    }
    if (match.arg(method) == "GET") {
        ifelse(is.null(cred) == TRUE, resp <- GET(paste0(URL, 
            file)), resp <- GET(paste0(URL, file), authenticate(as.vector(cred[1]), 
            as.vector(cred[2]))))
        return(noquote(content(resp, "text")))
    }
    else if (match.arg(method) == "PUT") {
        FILE <- upload_file(file)
        if (is.null(cred) == TRUE) {
            PUT(paste0(URL, strsplit(file, "/")[[1]][length(strsplit(file, 
                "/")[[1]])]))
        }
        else {
            PUT(paste0(URL, strsplit(file, "/")[[1]][length(strsplit(file, 
                "/")[[1]])]), authenticate(as.vector(cred[1]), 
                as.vector(cred[2])), body = FILE, config(followlocation = 0L))
        }
    }
    else if (match.arg(method) == "POST") {
        stop("Method \"POST\" is no implemented in \"sciencedata.dk\"")
    }
    else if (match.arg(method) == "DELETE") {
        ifelse(is.null(cred) == TRUE, DELETE(paste0(URL, strsplit(file, 
            "/")[[1]][length(strsplit(file, "/")[[1]])])), DELETE(paste0(URL, 
            strsplit(file, "/")[[1]][length(strsplit(file, "/")[[1]])]), 
            authenticate(as.vector(cred[1]), as.vector(cred[2])), 
            add_headers(Accept = "")))
    }
}
