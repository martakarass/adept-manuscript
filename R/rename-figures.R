

fnames.old.0 <- c(
  "intro_3d_1d_acc",
  "intro_translation_and_scaling",
  "results_stride_duration_bubble_plot",
  "validation_consistency_with_manual_segmentation_1",
  "results_individual_strides_for_three_participants"
)
fnames.old <- paste0(fnames.old.0, ".eps")


fnames.old.path <- "/Users/martakaras/Dropbox/_PROJECTS/R/adept-manuscript/figures-subset-eps"
fnames.new.path <- "/Users/martakaras/Dropbox/_PROJECTS/R/adept-manuscript/figures-subset-eps-renamed"

for (i in 1:length(fnames.old)){
  fname.old.i <- fnames.old[i]
  fpath.old.i <- file.path(fnames.old.path, fname.old.i)
  fname.new.i <- paste0("Fig", i, ".eps")
  fpath.new.i <- file.path(fnames.new.path, fname.new.i)
  file.copy(from = fpath.old.i, to = fpath.new.i, overwrite = TRUE)
}


