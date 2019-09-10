source("setup.R")

## Upload the .7z files in advance: now unzip ==================================
file_list <- list.files(pattern = ".7z", path = "7z", all.files = TRUE)
pw <- readline(prompt = "Enter the password: ")
for (file in file_list) {
  sys_command <- paste0("7z ", "x ./7z/", file, " -p", pw, " -o./7z/")
  system(sys_command)
}

## Clean the snapshots before matching. Stored as .fst format ==================
date_df <- snapshot_list()
data(oc_colclasses)
clean_snapshot(
  date_df = date_df,
  col_classes = oc_colclasses,
  varnames = vl$all,
  date = vl$date,
  num = vl$num
)

## Match with relevant variables. If pre-run matches exist, use them. ==========
report <- vrmatch(
  date_df,
  varnames = vl$mat,
  varnames_str = setdiff(vl$mat, union(vl$num, vl$date)),
  varnames_num = intersect(vl$mat, union(vl$num, vl$date)),
  file_type = ".fst",
  vars_change = c(vl$mat[1:3], vl$addr2[1], vl$id[1], "szPartyName"),
  exist_files = TRUE
)

save(report, file = paste0("report_", format(Sys.Date(), "%y%m%d"), ".Rda"))

