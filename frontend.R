library(plumber)

#* Render the main page: index.html
#* @filter frontend_router
function(req) {
  # Check if the requested asset exists in the dist directory
  file_path <- paste0("./dist", req$PATH_INFO)
  if (!file.exists(file_path)) {
    # If not, fallback to index.html for the frontend to handle the route
    req$PATH_INFO <- "/index.html"
  }
  forward()
}

#* @assets ./dist /
list()