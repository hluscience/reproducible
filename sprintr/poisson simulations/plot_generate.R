# Define the structures. methods and ratios
structures <- c("hier", "mix", "anti")
methods <- c("sprinter", "MEL", "SIS", "APL")
ratios <- 6

# Initialize a nested list to store results
results <- setNames(lapply(structures, function(x) list(dev = list(), auc = list())), structures)

# Loop through the structures, methods, and eval list
for (structure in structures) {
  # Load the dataset for the current structure
  eval.list <- readRDS(paste(structure, "eval list", sep = " "))
  
  for (metric in metrics) {
    dev_metric_name <- paste("dev", metric, sep = "_")
    
    for (ratio in 1:ratios) {
      results[[structure]]$dev[[metric]] <- append(results[[structure]]$dev[[metric]], mean(eval.list[[ratio]][[dev_metric_name]]))
    }
  }
}


# Set colors, point characters, and line types
col <- c("#FF0000", "#5077b3", "pink", "#8a6aa4")
lty <- c(1, 4, 2, 5)
pch <- c(1, 4, 2, 5) 

# Define the PDF output
pdf(file="poisson_plot.pdf", width=24, height=9) 

# Define the layout
layout(matrix(c(1:3, 4,4,4), nrow=2, byrow=TRUE), 
       heights=c(0.95, 0.1), 
       widths=c(0.99, 0.9, 0.9))

# Generate plots for deviance for each structure
for (structure in c("hier", "mix", "anti")) {
  # Retrieve data for the current structure
  data_all <- results[[structure]][["dev"]]
  
  # Determine y-range dynamically based on data
  yrange <- c(min(unlist(data_all)), max(unlist(data_all)))
  
  # Set plot parameters
  main <- structure
  xlab <- TeX('\\beta{*}$')
  ylab <- ifelse(structure == "hier", "Deviance", "")
  beta <- c(1, 1.25, 1.5, 1.75, 2, 2.25)
  
  # Adjust margins and fonts
  if (structure == "hier") {
    par(mar = c(7, 9.8, 5.5, 2), mgp = c(5.8, 2.3, 0))
    plot(1, type="n", xlab = "", ylab="", xlim=c(1,2.25), ylim=yrange, cex.axis=3.3, cex.lab=4, 
         main=main, cex.main=5, font.main=1, xaxt="n", yaxt="n", font.lab=1.9)
    box(lwd=3)
    mtext(side=2, line=5, ylab, cex=3.4)
    mtext(side=1, line=7, xlab, cex=3.3) 
    axis(1, at=beta, labels=as.character(beta), cex.axis=3.8)
    axis(2, las=0, cex.axis=3.8, padj=0.5)
  } else {
    par(mar = c(7, 4, 5.5, 2), mgp = c(5.8, 2.3, 0))
    plot(1, type="n", xlab="", ylab="", xlim=c(1,2.25), ylim=yrange, cex.axis=3.3, 
         cex.lab=4, main=main, cex.main=5, font.main=1,  xaxt="n", yaxt="n", font.lab=1.9)
    box(lwd=3)
    mtext(side=1, line=7, xlab, cex=3.3) 
    axis(1, at=beta, labels=as.character(beta), cex.axis=3.8)
    axis(2, las=0, cex.axis=3.8, padj=0.5)
  }

  # Give plot
  for (i in 1:4) {
    points(beta, data_all[[i]], col=col[i], pch=pch[i], cex=4, lwd=3)
    lines(beta, data_all[[i]], col=col[i], cex=4, lwd=6, lty=lty[i])
  }
}


# Add legend
par(mar=c(1,1,1,1))
plot.new()
legend("center", legend=metrics,
       text.width=c(0.11, 0.073, 0.073, 0.073), 
       col=col, lty=lty, lwd=c(6,6,6,6), pch=pch, cex=4.5, 
       bty="n", ncol=4)

# Close the PDF device
dev.off()