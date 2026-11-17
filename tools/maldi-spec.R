# Package specification for MALDIcellassay for pak.
#
# A branch or tag can be used as a git ref by pak. A commit hash cannot, so a
# commit is installed through a git clone of exactly that commit (`git::`).
# Note: the GitHub zipball URL does not work on Windows (long paths) and the
# archive tarball download is not followed by pak, so `git::` is the reliable
# variant.
#
# Used by installer/scripts/install-packages.R and tools/install-maldi-from-lock.R

maldiCellAssaySpec <- function(ref = "") {
  ref <- trimws(as.character(ref))

  if (grepl("^[0-9a-fA-F]{7,40}$", ref)) {
    paste0("git::https://github.com/CeMOS-Mannheim/MALDIcellassay@", ref)
  } else if (nzchar(ref)) {
    paste0("CeMOS-Mannheim/MALDIcellassay@", ref)
  } else {
    "CeMOS-Mannheim/MALDIcellassay"
  }
}
