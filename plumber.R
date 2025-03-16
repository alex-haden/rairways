#    https://www.rplumber.io/

library(plumber)

blocked_ips <- c("1.2.3.4", "2.3.4.5", "3.4.5.6", "4.5.6.7")

#* @apiTitle Raiways API
#* @apiDescription This is the primary API for Rairways
#* @apiContact list(name = "API Support", email = "contact@rplumberapi.com")
#* @apiVersion 1.0.0

#* @plumber 
function(pr) {
  pr %>% 
    pr_mount("/api", pr("backend.R")) %>%
    pr_mount("/", pr("frontend.R"))
}

#* Block banned IP addresses - this should be replaced by network firewall
#* @filter ip-blocking
function(req, res) {
  if (req$REMOTE_ADDR %in% blocked_ips) {
    res$status <- 403
    return(jsonlite::unbox("Your IP has been blocked"))
  }
  forward()
}