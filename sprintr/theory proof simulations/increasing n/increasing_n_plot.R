# Install and load necessary packages
if (!require(latex2exp)) install.packages("latex2exp")
library(latex2exp)

# Read evaluation results
eval_list <- readRDS("eval_list.rds")

# Define the methods and their corresponding eta values
methods <- c("sprinter")
n_seq <- c(100, 200, 500, 1000, 2000, 5000)
num_methods <- length(methods)

# Initialize lists to store results
method_misspec <- vector("list", num_methods)
method_TPR <- vector("list", num_methods)
method_dev <- vector("list", num_methods) # Added for dev

# Function to extract and summarize results
extract_results <- function(eval_list, measure) {
  res <- numeric(length(n_seq))
  for (i in 1:length(n_seq)) {
    res[i] <- mean(eval_list[[i]][[measure]], na.rm = TRUE)
  }
  return(res)
}

# Extract and summarize results for each method
for (m in 1:num_methods) {
  method_name <- methods[m]
  method_misspec[[m]] <- extract_results(eval_list, paste0("misspec_", method_name))
  method_TPR[[m]] <- extract_results(eval_list, paste0("TPR_", method_name))
  method_dev[[m]] <- extract_results(eval_list, paste0("dev_", method_name)) # Extract dev results
}

# Colors and symbols for plotting
col <- c("#FF0000")
pch <- c(6)
lty <- c(1)

# Plot settings
pdf(file="plot_increasing_n_add_dev.pdf", width=25, height=8.8)

# Define the layout for two rows and two columns of subplots
layout(matrix(c(1:3, 4, 4, 4), nrow=2, byrow=TRUE),
       heights=c(0.95, 0.08),
       widths=c(1, 1, 1))

# First row, first column: Misspecification Error plots
main <- "Misspecification Error Comparison"
ylab <- "Step 1 Misspecification Error"
xlab <- "n"
par(mar=c(7,10,5,2), mgp=c(5.8,2.3,0))
plot(1, type = "n", xlab = "", ylab = "", xlim = c(min(n_seq), max(n_seq)),
     ylim = c(0, max(unlist(method_misspec))), cex.axis = 5,
     cex.lab = 5, xaxt = "n", yaxt = "n", font.lab=2, main="Mixed (MIR=0.5)", cex.main=4.5, font.main=1)
box(lwd = 3)
mtext(side=2, line=6, ylab, cex=3)
mtext(side=1, line=7, xlab, cex=3.5)
for (i in 1:num_methods) {
  points(n_seq, method_misspec[[i]], col = col[i], pch = pch[i], cex = 4, lwd = 3)
  lines(n_seq, method_misspec[[i]], col = col[i], cex = 4, lwd = 5, lty = lty[i])
}
axis(1, at = n_seq, labels = as.character(n_seq), cex.axis = 4, padj = 0.25)
axis(2, las = 0, cex.axis = 4.2, padj = 0.5)

# First row, second column: TPR plots
ylab <- "Step 2 Interaction Selection TPR"
par(mar=c(7,10,5,2), mgp=c(5.8,2.3,0))
plot(1, type = "n", xlab = "", ylab = "", xlim = c(min(n_seq), max(n_seq)),
     ylim = c(min(unlist(method_TPR)), 1), cex.axis = 5,
     cex.lab=5, xaxt="n", yaxt="n", font.lab=2, main="Mixed (MIR=0.5)", cex.main=4.5, font.main=1)
box(lwd = 3)
mtext(side=2, line=6, ylab, cex=3)
mtext(side=1, line=7, xlab, cex=3.5)
for (i in 1:num_methods) {
  points(n_seq, method_TPR[[i]], col = col[i], pch = pch[i], cex = 4, lwd = 3)
  lines(n_seq, method_TPR[[i]], col = col[i], cex = 4, lwd = 5, lty = lty[i])
}
axis(1, at=n_seq, labels=as.character(n_seq), cex.axis=4, padj=0.25)
axis(2, las=0, cex.axis=4.2, padj=0.5)

# Third subplot: dev plots
ylab <- "Deviance"
par(mar=c(7,10,5,2), mgp=c(5.8,2.3,0))
plot(1, type="n", xlab="", ylab="", xlim=c(min(n_seq), max(n_seq)),
     ylim=c(min(unlist(method_dev)), max(unlist(method_dev))),cex.axis=5,
     cex.lab=5, xaxt="n", yaxt="n", font.lab=2, main="Mixed (MIR=0.5)", cex.main=4.5, font.main=1)
box(lwd=3)
mtext(side=2, line=6, ylab, cex=3)
mtext(side=1, line=7, xlab, cex=3.5)
for (i in 1:num_methods) {
  points(n_seq, method_dev[[i]], col=col[i], pch=pch[i], cex=4, lwd=3)
  lines(n_seq, method_dev[[i]], col=col[i], cex=4, lwd=5, lty=lty[i])
}
axis(1, at=n_seq, labels=as.character(n_seq), cex.axis=4, padj=0.25)
axis(2, las=0, cex.axis=4.2, padj=0.5)

dev.off()
