library(testthat)

test_folder <- "C:/Users/Jonathan Tannen/Dropbox/sixty_six/posts/turnout_tracker/tracker_v0/tests/"

test_files <- list.files(
  pattern = "^test_",
  path = test_folder,
  full.names = TRUE
)

for(file in test_files){
  source(file, chdir=TRUE)
}
