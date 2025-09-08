# Install and load necessary packages
if (!require(latex2exp)) install.packages("latex2exp")
library(latex2exp)

# Read evaluation results
eval_list <- readRDS("eval_list4.rds")

# Define the methods and their corresponding eta values
methods <- c("sprinter", "oracle")
p_seq <- c(30, 50, 70, 100, 150)
num_methods <- length(methods)

# Initialize lists to store results
method_time <- vector("list", num_methods)
method_TPR <- vector("list", num_methods)
method_FPR <- vector("list", num_methods)

# Function to extract and summarize results
extract_results <- function(eval_list, measure) {
  res <- numeric(length(p_seq))
  for (i in 1:length(p_seq)) {
    res[i] <- mean(eval_list[[i]][[measure]], na.rm = TRUE)
  }
  return(res)
}

# Extract and summarize results for each method
for (m in 1:num_methods) {
  method_name <- methods[m]
  method_time[[m]] <- extract_results(eval_list, paste0("time_", method_name))
  method_TPR[[m]] <- extract_results(eval_list, paste0("TPR_", method_name))
  method_FPR[[m]] <- extract_results(eval_list, paste0("FPR_", method_name))
}

# Colors and symbols for plotting
col <- c("#FF0000", "#1f77b4")
pch <- c(6, 1)
lty <- c(1, 2)

# Plot settings
pdf(file="comparison_with_oracle.pdf", width=20, height=11.5)

# Define the layout for two rows and two columns of subplots
layout(matrix(c(1, 2, 3, 3), nrow=2, byrow=TRUE),
       heights=c(0.95, 0.22),
       widths=c(1, 1))

# First row: Time plots
main <- "Time Comparison"
ylab <- "Time (s)"
xlab <- "p"
par(mar=c(7,10,5,2), mgp=c(5.8,2.3,0))
plot(1, type="n", xlab="", ylab="", xlim=c(min(p_seq), max(p_seq)),
     ylim=c(min(unlist(method_time)), max(unlist(method_time))), cex.axis=5,
     cex.lab=5, xaxt="n", yaxt="n", font.lab=2, main="Mixed (MIR=0.5)", cex.main=4.5, font.main=1)
box(lwd=3)
mtext(side=2, line=6, ylab, cex=3.8)
mtext(side=1, line=7, xlab, cex=4)
for (i in 1:num_methods) {
  points(p_seq, method_time[[i]], col=col[i], pch=pch[i], cex=4, lwd=3)
  lines(p_seq, method_time[[i]], col=col[i], cex=4, lwd=5, lty=lty[i])
}
axis(1, at=p_seq, labels=as.character(p_seq), cex.axis=4, padj=0.25)
axis(2, las=0, cex.axis=4.2, padj=0.5)

# Second column: TPR plots
ylab <- "Interaction Selection TPR"
par(mar=c(7,10,5,2), mgp=c(5.8,2.3,0))
plot(1, type="n", xlab="", ylab="", xlim=c(min(p_seq), max(p_seq)),
     ylim=c(min(unlist(method_TPR)), max(unlist(method_TPR))),cex.axis=5,
     cex.lab=5, xaxt="n", yaxt="n", font.lab=2, main="Mixed (MIR=0.5)", cex.main=4.5, font.main=1)
box(lwd=3)
mtext(side=2, line=5, ylab, cex=3.8)
mtext(side=1, line=7, xlab, cex=4)
for (i in 1:num_methods) {
  points(p_seq, method_TPR[[i]], col=col[i], pch=pch[i], cex=4, lwd=3)
  lines(p_seq, method_TPR[[i]], col=col[i], cex=4, lwd=5, lty=lty[i])
}
axis(1, at=p_seq, labels=as.character(p_seq), cex.axis=4, padj=0.25)
axis(2, las=0, cex.axis=4.2, padj=0.5)

# Plot legend
par(mar=c(1,1,1,1))
plot.new()
legend("center", legend=methods,
       text.width=c(0.2, 0.15),
       col=col, lty=lty, lwd=5, pch=pch, cex=5, # Adjust cex and lwd as needed
       bty="n", ncol=2, x.intersp=0.2) # Set ncol to 2 for one row
dev.off()
