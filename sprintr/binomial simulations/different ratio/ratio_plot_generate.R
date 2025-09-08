# Install and load necessary packages
if (!require(latex2exp)) install.packages("latex2exp")
library(latex2exp)

# Define the structures, methods and ratios
structures <- c("hier_strong", "hier_weak", "mix", "anti")
methods <- c("SIS", "APL", "MEL", "glinternet", "hierNet", "hierFabs", "RAMP", "sprinter")
ratios <- 5

# Initialize a nested list to store results
results <- setNames(lapply(structures, function(x) list(dev = list(), auc = list())), structures)

# Loop through the structures, methods, and eval list
for (structure in structures) {
  # Dynamically load the dataset for the current structure
  eval.list <- readRDS(paste0(structure, "_eval_list.rds"))

  for (method in methods) {
    dev_method_name <- paste("dev", method, sep = "_")
    auc_method_name <- paste("auc", method, sep = "_")

    for (ratio in 1:ratios) {
      results[[structure]]$dev[[method]] <- append(results[[structure]]$dev[[method]], mean(eval.list[[ratio]][[dev_method_name]]))
      results[[structure]]$auc[[method]] <- append(results[[structure]]$auc[[method]], mean(eval.list[[ratio]][[auc_method_name]]))
    }
  }
}


# Set colors, point characters, and line types
col <- c("pink", "#8a6aa4", "#5077b3", "#69a35e", "#6d6c6e", "#FFD700", "#ec8c3c", "#FF0000")
pch <- c(2, 5, 4, 1, 3, 5, 6, 6)
lty <- c(2, 2, 2, 2, 2, 2, 2, 1)

# Define the PDF output
pdf(file = paste0("logistic_ratio_plot.pdf"), width=30, height=16.5)

# Define the layout for two rows and four columns of subplots
layout(matrix(c(1:4, 5:8, 9,9,9,9), nrow=3, byrow=TRUE),
       heights=c(0.85, 0.95, 0.22),
       widths=c(0.99, 0.9, 0.9, 0.9))

# Generate plots for deviance and AUC for each structure
for (metric in c("dev", "auc")) {
  for (structure in structures) {
    # Retrieve data for the current structure and metric
    data_all <- results[[structure]][[metric]]

    # Load the ratios for the current structure
    ratios <- c(0.1, 0.4, 0.7, 1, 1.3)

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

    xlab <- ifelse(metric == "auc", TeX('\\MIR$'), "")
    ylab <- ifelse(metric == "dev" & structure == "hier_strong", "Deviance",
                   ifelse(metric == "auc" & structure == "hier_strong", "AUC", ""))

    # Set margins and fonts
    if (metric == "dev") {
      if (structure == "hier_strong") {
        par(mar = c(0, 10, 5, 2), mgp = c(5.8, 2.3, 0))
        plot(1, type="n", ylab="", xlim=c(min(ratios), max(ratios)), ylim=yrange, cex.axis=5, cex.lab=5,
             main=main, cex.main=5, font.main=1, xaxt="n", yaxt="n", font.lab=2)
        box(lwd=3)
        mtext(side=2, line=6, ylab, cex=3.4)
        axis(2, las=0, cex.axis=4.2, padj=0.5)
      } else {
        par(mar = c(0, 4, 5.5, 2), mgp = c(5.8, 2.3, 0))
        plot(1, type="n", xlim=c(min(ratios), max(ratios)), ylim=yrange, cex.axis=5,
             main=main, cex.main=5, font.main=1, xaxt="n", yaxt="n", font.lab=2)
        box(lwd=3)
        axis(2, las=0, cex.axis=4.2, padj=0.5)
      }
    } else if (metric == "auc") {
      if (structure == "hier_strong") {
        par(mar = c(7, 10, 3.5, 2), mgp = c(5.8, 2.3, 0))
        plot(1, type="n", xlab="", ylab="", xlim=c(min(ratios), max(ratios)), ylim=yrange, cex.axis=5,
             cex.lab=5, xaxt="n", yaxt="n", font.lab=2)
        box(lwd=3)
        mtext(side=2, line=6, ylab, cex=3.4)
        mtext(side=1, line=8, xlab, cex=3.3)
        axis(1, at=ratios, labels=as.character(ratios), cex.axis=4.5, padj=0.2)
        axis(2, las=0, cex.axis=4.2, padj=0.5)
      } else {
        par(mar = c(7, 4, 3.5, 2), mgp = c(5.8, 2.3, 0))
        plot(1, type="n", xlab="", xlim=c(min(ratios), max(ratios)), ylim=yrange, cex.axis=5,
             cex.lab=5, xaxt="n", yaxt="n", font.lab=2)
        box(lwd=3)
        mtext(side=1, line=8, xlab, cex=3.3)
        axis(1, at=ratios, labels=as.character(ratios), cex.axis=4.5, padj=0.2)
        axis(2, las=0, cex.axis=4.2, padj=0.5)
      }
    }

    # Plot the data
    for (i in 1:length(methods)) {
      points(ratios, data_all[[i]], col=col[i], pch=pch[i], cex=4, lwd=3)
      lines(ratios, data_all[[i]], col=col[i], cex=4, lwd=5, lty=lty[i])
    }
  }
}

# Add legend
par(mar=c(1,1,1,1))
plot.new()
legend("center", legend = c("sprinter", "MEL", "APL", "SIS", "glinternet", "hierNet", "HierFabs", "RAMP"),
       text.width = c(0.088, 0.058, 0.058, 0.057, 0.098, 0.083, 0.092, 0.02),
       col = c("#FF0000", "#5077b3", "#8a6aa4", "pink", "#69a35e", "#6d6c6e", "#FFD700", "#ec8c3c"),
       lty = c(1, 2, 2, 2, 2, 2, 2, 2), lwd=5, pch = c(6, 4, 5, 2, 1, 3, 5, 6),
       cex=5, bty="n", ncol=8, x.intersp = 0.2)

# Close the PDF device
dev.off()

