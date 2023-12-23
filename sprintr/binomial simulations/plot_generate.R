# Define the structures, methods and ratios
structures <- c("hier", "mix", "anti")
methods <- c("sprinter", "APL", "glinternet", "RAMP", "MEL", "hierNet", "SIS")
ratios <- 5

# Initialize a nested list to store results
results <- setNames(lapply(structures, function(x) list(dev = list(), auc = list())), structures)

# Loop through the structures, methods, and eval list
for (structure in structures) {
  # Dynamically load the dataset for the current structure
  eval.list <- readRDS(paste(structure, "eval list", sep = " "))
  
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
col <- c("#FF0000", "#8a6aa4", "#69a35e", "#ec8c3c","#5077b3", "#6d6c6e", "pink")
pch <- c(1, 5, 2, 6, 4, 3, 2)
lty <- c(1, 5, 2, 6, 4, 3, 2)

# Define the PDF output
pdf(file="logistic_plot.pdf", width=24, height=16) 

# Define the layout for two rows and three columns of subplots
layout(matrix(c(1:3, 4:6, 7,7,7), nrow=3, byrow=TRUE), 
       heights=c(0.85, 0.95, 0.1), 
       widths=c(0.99, 0.9, 0.9))

# Generate plots for deviance and AUC for each structure
for (metric in c("dev", "auc")) {
  for (structure in c("hier", "mix", "anti")) {
    # Retrieve data for the current structure and metric
    data_all <- results[[structure]][[metric]]
    
    # Determine y-range dynamically based on data
    yrange <- c(min(unlist(data_all)), max(unlist(data_all)))
    
    # Set plot parameters
    main <- ifelse(metric == "dev", structure, "")
    xlab <- ifelse(metric == "auc", TeX('\\beta{*}$'), "")
    ylab <- ifelse(metric == "dev" & structure == "hier", "Deviance", 
                   ifelse(metric == "auc" & structure == "hier", "AUC", ""))
    beta <- c(1, 1.5, 2, 2.5, 3)
      
    # Adjust margins and fonts
    if (metric == "dev") {
      if (structure == "hier") {
        par(mar = c(0, 9.8, 5.5, 2), mgp = c(5.8, 2.3, 0))
        plot(1, type="n", ylab="", xlim=c(1,3), ylim=yrange, cex.axis=3.3, cex.lab=4, 
             main=main, cex.main=5, font.main=1, xaxt="n", yaxt="n", font.lab=1.9)
        box(lwd=3)
        mtext(side=2, line=5, ylab, cex=3.4)
        axis(2, las=0, cex.axis=3.8, padj=0.5)
      } else {
        par(mar = c(0, 4, 5.5, 2), mgp = c(5.8, 2.3, 0))
        plot(1, type="n", xlim=c(1,3), ylim=yrange, cex.axis=3.3, 
             main=main, cex.main=5, font.main=1, xaxt="n", yaxt="n", font.lab=1.9)
        box(lwd=3)
        axis(2, las=0, cex.axis=3.8, padj=0.5)
      }
    } else if (metric == "auc") {
      if (structure == "hier") {
        par(mar = c(7, 9.8, 3.5, 2), mgp = c(5.8, 2.3, 0))
        plot(1, type="n", xlab="", ylab="", xlim=c(1,3), ylim=yrange, cex.axis=3.3, 
             cex.lab=4, xaxt="n", yaxt="n", font.lab=1.9)
        box(lwd=3)
        mtext(side=2, line=5, ylab, cex=3.4)
        mtext(side=1, line=7, xlab, cex=3.3)
        axis(1, at=beta, labels=as.character(beta), cex.axis=3.8)
        axis(2, las=0, cex.axis=3.8, padj=0.5)
      } else {
        par(mar = c(7, 4, 3.5, 2), mgp = c(5.8, 2.3, 0))
        plot(1, type="n", xlab="", xlim=c(1,3), ylim=yrange, cex.axis=3.3, 
             cex.lab=4, xaxt="n", yaxt="n", font.lab=1.9)
        box(lwd=3)
        mtext(side=1, line=7, xlab, cex=3.3)
        axis(1, at=beta, labels=as.character(beta), cex.axis=3.8)
        axis(2, las=0, cex.axis=3.8, padj=0.5)
      }
    }
    # Give plot
    for (i in 1:7) {
      points(beta, data_all[[i]], col=col[i], pch=pch[i], cex=4, lwd=3)
      lines(beta, data_all[[i]], col=col[i], cex=4, lwd=6, lty=lty[i])
    }
  }
}

# Add legend
par(mar=c(1,1,1,1))
plot.new()
legend("center", legend=methods,
       text.width=c(0, 0.074, 0.11, 0.087, 0.076, 0.088, 0.076), 
       col=col, lty=lty, lwd=c(6,6,6,6,6,6,6), pch=pch, cex=4.5, 
       bty="n", ncol=7)

# Close the PDF device
dev.off()

