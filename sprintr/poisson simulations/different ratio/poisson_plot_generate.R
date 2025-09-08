# Install and load necessary packages
if (!require(latex2exp)) install.packages("latex2exp")
library(latex2exp)

# Define the structures. methods and ratios
structures <- c("hier_strong", "hier_weak", "mix", "anti")
methods <- c("sprinter", "MEL", "APL", "SIS")
ratios <- 5

# Initialize a nested list to store results
results <- setNames(lapply(structures, function(x) list(dev = list())), structures)

# Loop through the structures, methods, and eval list
for (structure in structures) {
  # Load the dataset for the current structure
  eval.list <- readRDS(paste0(structure, "_eval_list.rds"))

  for (method in methods) {
    dev_method_name <- paste("dev", method, sep = "_")

    for (ratio in 1:ratios) {
      results[[structure]]$dev[[method]] <- append(results[[structure]]$dev[[method]], mean(eval.list[[ratio]][[dev_method_name]]))
    }
  }
}

# Set colors, point characters, and line types
col <- c("#FF0000", "#5077b3", "#8a6aa4", "pink")
pch <- c(6, 4, 5, 2)
lty <- c(1, 2, 2, 2)

# Define the PDF output
pdf(file="poisson_plot.pdf", width=30, height=9.8)

# Define the layout
layout(matrix(c(1:4, 5,5,5,5), nrow=2, byrow=TRUE),
       heights=c(0.95, 0.22),
       widths=c(0.99, 0.9, 0.9, 0.9))

# Generate plots for deviance for each structure
for (structure in structures) {
  # Retrieve data for the current structure
  data_all <- results[[structure]][["dev"]]

  # Determine y-range dynamically based on data
  yrange <- c(min(unlist(data_all)), max(unlist(data_all)))

  # Set plot parameters
  if (structure == "hier_weak") {
    main <- "Weak hierarchical"
  } else if (structure == "hier_strong") {
    main <- "Strong hierarchical"
  } else if (structure == "mix") {
    main <- "Mixed"
  } else if (structure == "anti") {
    main <- "Anti-hierarchical"
  }

  xlab <- TeX('\\MIR$')
  ylab <- ifelse(structure == "hier_strong", "Deviance", "")

  # Load the ratios for the current structure
  ratios <- c(0.1, 0.4, 0.7, 1, 1.3)

  # Adjust margins and fonts
  if (structure == "hier_strong") {
    par(mar = c(7, 10, 5, 2), mgp = c(5.8, 2.3, 0))
    plot(1, type="n", xlab = "", ylab="", xlim=c(min(ratios), max(ratios)), ylim=yrange, cex.axis=5, cex.lab=5,
         main=main, cex.main=5, font.main=1, xaxt="n", yaxt="n", font.lab=2)
    box(lwd=3)
    mtext(side=2, line=6, ylab, cex=3.4)
    mtext(side=1, line=8, xlab, cex=3.3)
    axis(1, at=ratios, labels=as.character(ratios), cex.axis=4.5, padj=0.2)
    axis(2, las=0, cex.axis=4.2, padj=0.5)
  } else {
    par(mar = c(7, 4, 5.5, 2), mgp = c(5.8, 2.3, 0))
    plot(1, type="n", xlab="", ylab="", xlim=c(min(ratios), max(ratios)), ylim=yrange, cex.axis=5,
         cex.lab=5, main=main, cex.main=5, font.main=1,  xaxt="n", yaxt="n", font.lab=2)
    box(lwd=3)
    mtext(side=1, line=8, xlab, cex=3.3)
    axis(1, at=ratios, labels=as.character(ratios), cex.axis=4.5, padj=0.2)
    axis(2, las=0, cex.axis=4.2, padj=0.5)
  }

  # Give plot
  for (i in 1:4) {
    points(ratios, data_all[[i]], col=col[i], pch=pch[i], cex=4, lwd=3)
    lines(ratios, data_all[[i]], col=col[i], cex=4, lwd=5, lty=lty[i])
  }
}


# Add legend
par(mar=c(1,1,1,1))
plot.new()
legend("center", legend=methods,
       text.width=c(0.11, 0.073, 0.073, 0.073),
       col=col, lty=lty, lwd=c(5,5,5,5), pch=pch, cex=5,
       bty="n", ncol=4)

# Close the PDF device
dev.off()
