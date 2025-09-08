# Install and load necessary packages
if (!require(latex2exp)) install.packages("latex2exp")
library(latex2exp)

# Read evaluation results
eval_list <- readRDS("increasing_corr_eval_list.rds")

# Define the correlations we are interested in
corr_seq <- c(0, -0.8)

# Use only one color (green) for all plots
col <- "#69a35e"  # Green color for all points
pch <- 16  # Small point type for all plots

# Define TH1 and TH2
TH1 <- 2 * (50000^(-0.25))  # TH1
TH2 <- 2 * (50000^(-0.04))  # TH2

# Define ix_inter matrix
ix_inter <- rbind(c(1, 5), c(2, 6), c(7, 8), c(8, 9), c(10, 11), c(12, 13))

# Plot settings
pdf(file="signal_strength_plots_with_TH1_TH2.pdf", width=16, height=8)

# Define the layout with tighter spacing
layout(matrix(c(1, 2), nrow=1, byrow=TRUE), heights=c(1), widths=c(1.05, 0.9))  # Reduce spacing by adjusting widths

# First row: Signal Strength plots for different correlations
for (corr_index in 1:length(corr_seq)) {
  corr <- corr_seq[corr_index]

  file_name <- paste0("inter_strength_gen_corr_", corr, ".rds")
  # Read the RDS file
  data <- readRDS(file_name)
  data <- data[order(data[, 3]), ]  # Order by signal strength (score)

  # Create a logical vector to find matching rows
  mark_idx <- apply(data, 1, function(row) any(apply(ix_inter, 1, function(x) all(row[1:2] == x))))

  # Labels and titles
  ylab <- if (corr == 0) expression(abs(gamma[j]^M)) else ""  # Y-label only for the first plot
  xlab <- "Index"  # X-axis label
  main_title <- latex2exp::TeX(paste0("$\\rho=$", corr))  # Title in LaTeX format

  # Set margins, further increase the left margin for the first plot
  par(mar = c(5, if (corr == 0) 12 else 6, 5, 1), mgp = c(5, 2.3, 0))  # Increased left margin for the first plot

  plot(1, type = "n", xlab = "", ylab = "", xlim = c(1, nrow(data)),
       ylim = c(min(data[,3]), max(data[,3])), cex.axis = 4, cex.lab = 4,
       xaxt = "n", yaxt = "n", font.lab = 2, main = main_title, cex.main = 3.8, font.main = 1)
  box(lwd = 3)

  # Add customized axis labels
  if (corr == 0) {
    mtext(side = 2, line = 5, ylab, cex = 3.4)  # Adjust line number to move label outward
  }
  mtext(side = 1, line = 3, xlab, cex = 3.3)

  # Plot green points for all data
  points(1:nrow(data), data[, 3], col = col, pch = pch, cex = 2, lwd = 3)

  # Highlight the matching points in red
  points(which(mark_idx), data[mark_idx, 3], col = "red", pch = pch, cex = 1.8, lwd = 3)

  # Custom axis ticks (suppressing x-axis tick values)
  axis(2, las = 0, cex.axis = 3, padj = 0.5)

  # Add horizontal line for TH1 on the first plot and TH2 on the second plot
  # Add horizontal line for TH1 on the first plot and TH2 on the second plot
  if (corr_index == 1) {
    abline(h = TH1, col = "blue", lwd = 4, lty = 2)  # Add horizontal line for TH1
    text(x = nrow(data) * 0.48, y = TH1, latex2exp::TeX("$c=2, \\kappa = 0.25$"), pos = 3, cex = 3)  # Move text to the left for TH1
  } else if (corr_index == 2) {
    abline(h = TH2, col = "blue", lwd = 4, lty = 2)  # Add horizontal line for TH2
    text(x = nrow(data) * 0.48, y = TH2 - 0.02, latex2exp::TeX("$c=2, \\kappa = 0.04$"), pos = 1, cex = 3)  # Move text to the left for TH2
  }
}

dev.off()

