# Create the exact, selectively downloaded GLC package used by the browser lesson.
# Run from the project root: Rscript --vanilla scripts/prepare_masterclass_glc.R
validation_library <- Sys.getenv("MASTERCLASS_R_LIBRARY")
if (nzchar(validation_library)) .libPaths(c(validation_library, .libPaths()))
library(glcdp)

repository <- "tscnlab/melidos-iztech-glc-dataset"
revision <- "abc456bdb418e6250b9c973813a190dc062c6e76"
dataset_id <- "MELIDOS_IZTECH_S011"
file_groups <- paste0(dataset_id, c(":14", ":15", ":17"))
package <- glc_open(repository, ref = revision)
staging <- tempfile("masterclass-glc-")
dir.create(staging)
download <- glc_download(
  package, dest_dir = staging, include = "all",
  dataset_id = dataset_id, file_group = file_groups
)
stopifnot(sum(grepl("^data/files/", download$path)) == 3L)
for (name in c("LICENSE", "README.md")) {
  download.file(paste0("https://raw.githubusercontent.com/", repository,
                       "/", revision, "/", name),
                file.path(staging, name), mode = "wb", quiet = TRUE)
}
local_package <- glc_open(staging)
print(local_package)
print(glc_files(local_package, file_group = file_groups))

dir.create("data/masterclass", recursive = TRUE, showWarnings = FALSE)
archive <- normalizePath("data/masterclass", mustWork = TRUE)
archive <- file.path(archive, "melidos-iztech-teaching.zip")
if (file.exists(archive)) unlink(archive)
previous_directory <- setwd(staging)
utils::zip(archive, list.files(".", recursive = TRUE, all.files = TRUE), flags = "-q")
setwd(previous_directory)
dir.create("outputs/masterclass", recursive = TRUE, showWarnings = FALSE)
writeLines(c(
  "Command: Rscript --vanilla scripts/prepare_masterclass_glc.R",
  paste("Repository:", repository), paste("Commit:", revision),
  paste("Dataset:", dataset_id),
  paste("Archive:", archive), paste("MD5:", tools::md5sum(archive)),
  "Metadata and schemas are unchanged; complete chest-light, sleep-diary, and wear-log files are included.",
  capture.output(print(download, n = Inf, width = Inf)),
  "", capture.output(sessionInfo())
), "outputs/masterclass/glc-package-build.txt")
message("Created ", archive)
