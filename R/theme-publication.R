
#' Generates `ggplot2` theme used for generating figures in the manuscript. 
#' 
#' Utilizes HanjoStudy's code available on GitHub: 
#' https://github.com/HanjoStudy/quotidieR/blob/master/R/theme_publication.R
#' (commit 1f9c329 on Jun 25, 2018; accessed on May 30, 2019) 
#' with very minor modifications to the original version. 
#' 
#' 
#' Permission is hereby granted, free of charge, to any person obtaining a copy
#' of this software and associated documentation files (the "Software"), to deal
#' in the Software without restriction, including without limitation the rights
#' to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
#' copies of the Software, and to permit persons to whom the Software is
#' furnished to do so, subject to the following conditions:
#' 
#' The above copyright notice and this permission notice shall be included in all
#' copies or substantial portions of the Software.
#' 
#' THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
#' IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
#' FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
#' AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
#' LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#' OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
#' SOFTWARE.
#' 
theme_Publication <- function(base_size = 13, base_family = "Helvetica") {
  theme_bw(
    base_size = base_size,
    base_family = base_family
    ) + 
  theme(
    panel.spacing.x = unit(0.5, "cm"),
    panel.background = element_rect(colour = NA),
    plot.background = element_rect(colour = NA),
    panel.border = element_rect(colour = NA),
    axis.title = element_text(face = "bold",size = rel(1)),
    axis.title.y = element_text(angle = 90, vjust = 2),
    axis.title.x = element_text(vjust = -0.2),
    axis.text = element_text(),
    axis.line = element_line(colour = "black"),
    axis.ticks = element_line(),
    panel.grid.major = element_line(colour = "#f0f0f0"),
    panel.grid.minor = element_blank(),
    legend.key = element_rect(colour = NA),
    legend.position = "bottom",
    legend.direction = "horizontal",
    legend.key.size= unit(0.2, "cm"),
    legend.spacing = unit(0, "cm"),
    legend.title = element_text(face = "italic"),
    plot.margin = unit(c(10, 5, 5, 5), "mm"),
    strip.background = element_rect(colour = "#f0f0f0", fill = "#f0f0f0"),
    strip.text = element_text(face = "bold")
  )
}