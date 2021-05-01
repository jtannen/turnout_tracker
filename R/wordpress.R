library(httr)

wp_config <- rjson::fromJSON(file="../wordpress_config.json")

upload_media <- function(
  path, 
  slug=NA, 
  overwrite=TRUE, 
  allow_filename_change=FALSE,
  verbose=FALSE
){
  filename <- function(path){
    res <- strsplit(path, "/")[[1]]
    res[length(res)]
  }
  orig_filename <- filename(path)
  
  if(is.na(slug)){
    slug <- orig_filename
  }
  
  if(overwrite){
    g <- GET(
      sprintf("https://sixtysixwards.com/wp-json/wp/v2/media/?slug=%s", slug),
      authenticate(wp_config$username, wp_config$password)
    )
    
    if(length(content(g)) > 0){
      if(length(content(g)) > 1) stop("Too many slug matches. Something's wrong.")
      replace_id <- content(g)[[1]]$id
      if(verbose) print(sprintf("Deleting post %s.", replace_id))
      d <- DELETE(
        sprintf("https://sixtysixwards.com/wp-json/wp/v2/media/%s?force=true", replace_id),
        authenticate(wp_config$username, wp_config$password)
      )
      if(d$status_code != 200){
        warning("Delete Failed!")
        return(d)
      }
    } else {
      if(verbose) print(sprintf("No duplicate slugs found."))
    }
  }
  
  if(verbose) print(sprintf("Posting."))
  
  r <- POST(
    "https://sixtysixwards.com/wp-json/wp/v2/media/",
    authenticate(wp_config$username, wp_config$password),
    body = list(file = upload_file(path), slug=slug)
  )
  
  cr <- content(r)
  
  if(!allow_filename_change){
    new_filename <- filename(cr$source_url)
    if(orig_filename != new_filename){
      print(cr)
      stop(sprintf("File name changed. Old: %s, New: %s", orig_filename, new_filename))
    }
  }
  
  if(r$status != 201){
    print(cr)
    stop("Upload failed.")
  }
  
  if(verbose) print(sprintf("Uploaded post %s.", cr$id))
  return(r)
}

