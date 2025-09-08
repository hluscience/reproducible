# Load the necessary files
eval_list <- readRDS("eval_list.rds")
eval_list_add <- readRDS("eval_list_add0.1.rds")

# Function to insert APL_0.1 and reorder the entries
insert_and_reorder <- function(eval_list, eval_list_add) {
  for (i in seq_along(eval_list)) {
    # Insert APL_0.1 data from eval_list_add
    eval_list[[i]]$time_APL_0.1 <- eval_list_add[[i]]$time_APL_0.1
    eval_list[[i]]$dev_APL_0.1 <- eval_list_add[[i]]$dev_APL_0.1
    eval_list[[i]]$auc_APL_0.1 <- eval_list_add[[i]]$auc_APL_0.1

    # Reorder the keys
    eval_list[[i]] <- list(
      time_sprinter = eval_list[[i]]$time_sprinter,
      dev_sprinter = eval_list[[i]]$dev_sprinter,
      auc_sprinter = eval_list[[i]]$auc_sprinter,
      time_glinternet = eval_list[[i]]$time_glinternet,
      dev_glinternet = eval_list[[i]]$dev_glinternet,
      auc_glinternet = eval_list[[i]]$auc_glinternet,
      time_APL_0.3 = eval_list[[i]]$time_APL_0.3,
      dev_APL_0.3 = eval_list[[i]]$dev_APL_0.3,
      auc_APL_0.3 = eval_list[[i]]$auc_APL_0.3,
      time_APL_0.5 = eval_list[[i]]$time_APL_0.5,
      dev_APL_0.5 = eval_list[[i]]$dev_APL_0.5,
      auc_APL_0.5 = eval_list[[i]]$auc_APL_0.5,
      time_APL_0.8 = eval_list[[i]]$time_APL_0.8,
      dev_APL_0.8 = eval_list[[i]]$dev_APL_0.8,
      auc_APL_0.8 = eval_list[[i]]$auc_APL_0.8,
      time_APL_1 = eval_list[[i]]$time_APL_1,
      dev_APL_1 = eval_list[[i]]$dev_APL_1,
      auc_APL_1 = eval_list[[i]]$auc_APL_1,
      time_APL_0.1 = eval_list[[i]]$time_APL_0.1,
      dev_APL_0.1 = eval_list[[i]]$dev_APL_0.1,
      auc_APL_0.1 = eval_list[[i]]$auc_APL_0.1
    )
  }
  return(eval_list)
}

# Apply the function to insert APL_0.1 and reorder the list
new_eval_list <- insert_and_reorder(eval_list, eval_list_add)

# Save the reordered eval_list
saveRDS(new_eval_list, "new_eval_list.rds")


# Install and load necessary packages
if (!require(latex2exp)) install.packages("latex2exp")
library(latex2exp)

# Read evaluation results
eval_list <- readRDS("new_eval_list.rds")

# Define the methods and their corresponding eta values
methods <- c("sprinter", "glinternet", "APL_0.3", "APL_0.5", "APL_0.8", "APL_1", "APL_0.1")
p_seq <- c(150, 500, 1000, 1500)
num_methods <- length(methods)

# Initialize lists to store results
method_time <- vector("list", num_methods)
method_dev <- vector("list", num_methods)
method_auc <- vector("list", num_methods)

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
  method_dev[[m]] <- extract_results(eval_list, paste0("dev_", method_name))
  method_auc[[m]] <- extract_results(eval_list, paste0("auc_", method_name))
}

# Colors and symbols for plotting
col <- c("#FF0000", "#69a35e", "#A2B6D0", "#FFB6C1", "#c27aa8", "#8a6aa4", "chocolate1")
pch <- c(6, 1, 2, 3, 4, 5, 6)
lty <- c(1, 2, 2, 2, 2, 2, 2)

# Plot settings
pdf(file="comparison_with_APL.pdf", width=25, height=9.8)

# Define the layout for two rows and three columns of subplots
layout(matrix(c(1:3, 4, 4, 4), nrow=2, byrow=TRUE),
       heights=c(0.95, 0.22),
       widths=c(1, 1, 1))

# First row: Time plots
main <- "Time Comparison"
ylab <- "Time (s)"
xlab <- "p"
par(mar=c(7,10,5,2), mgp=c(5.8,2.3,0))
plot(1, type="n", xlab="", ylab="", xlim=c(min(p_seq), max(p_seq)),
     ylim=c(min(unlist(method_time)), max(unlist(method_time))), cex.axis=5,
     cex.lab=5, xaxt="n", yaxt="n", font.lab=2, main="Mixed (MIR=1)", cex.main=5, font.main=1)
box(lwd=3)
mtext(side=2, line=6, ylab, cex=3.4)
mtext(side=1, line=7, xlab, cex=3.3)
for (i in 1:num_methods) {
  points(p_seq, method_time[[i]], col=col[i], pch=pch[i], cex=4, lwd=3)
  lines(p_seq, method_time[[i]], col=col[i], cex=4, lwd=5, lty=lty[i])
}
axis(1, at=p_seq, labels=as.character(p_seq), cex.axis=4.5, padj=0.25)
axis(2, las=0, cex.axis=4.2, padj=0.5)

# Second column: Deviance plots
ylab <- "Deviance"
par(mar=c(7,10,5,2), mgp=c(5.8,2.3,0))
plot(1, type="n", xlab="", ylab="", xlim=c(min(p_seq), max(p_seq)),
     ylim=c(min(unlist(method_dev)), max(unlist(method_dev))), cex.axis=5,
     cex.lab=5, xaxt="n", yaxt="n", font.lab=2, main="Mixed (MIR=1)", cex.main=5, font.main=1)
box(lwd=3)
mtext(side=2, line=5, ylab, cex=3.4)
mtext(side=1, line=7, xlab, cex=3.3)
for (i in 1:num_methods) {
  points(p_seq, method_dev[[i]], col=col[i], pch=pch[i], cex=4, lwd=3)
  lines(p_seq, method_dev[[i]], col=col[i], cex=4, lwd=5, lty=lty[i])
}
axis(1, at=p_seq, labels=as.character(p_seq), cex.axis=4.5, padj=0.25)
axis(2, las=0, cex.axis=4.2, padj=0.5)

# Third column: AUC plots
ylab <- "AUC"
par(mar=c(7,10,5,2), mgp=c(5.8,2.3,0))
plot(1, type="n", xlab="", ylab="", xlim=c(min(p_seq), max(p_seq)),
     ylim=c(min(unlist(method_auc)), max(unlist(method_auc))), cex.axis=5,
     cex.lab=5, xaxt="n", yaxt="n", font.lab=2, main="Mixed (MIR=1)", cex.main=5, font.main=1)
box(lwd=3)
mtext(side=2, line=5, ylab, cex=3.4)
mtext(side=1, line=7, xlab, cex=3.3)
for (i in 1:num_methods) {
  points(p_seq, method_auc[[i]], col=col[i], pch=pch[i], cex=4, lwd=3)
  lines(p_seq, method_auc[[i]], col=col[i], cex=4, lwd=5, lty=lty[i])
}
axis(1, at=p_seq, labels=as.character(p_seq), cex.axis=4.5, padj=0.25)
axis(2, las=0, cex.axis=4.2, padj=0.5)

# Define the legend labels
legend_labels <- c("sprinter", "glinternet", expression(APL(eta==0.1)), expression(APL(eta==0.3)), expression(APL(eta==0.5)), expression(APL(eta==0.8)), "APL")

# Plot legend
par(mar=c(1,1,1,1))
plot.new()
legend("bottom", legend=legend_labels,
       col=c("#FF0000", "#69a35e", "chocolate1", "#A2B6D0", "#FFB6C1", "#c27aa8", "#8a6aa4"),
       lty=c(1, 2, 2, 2, 2, 2, 2), lwd=5, pch=c(6, 1, 6, 2, 3, 4, 5), cex=4.5, # Adjust cex and lwd as needed
       bty="n", ncol=7, text.width = c(0.083, 0.097, 0.132, 0.131, 0.131, 0.131, 0.03), x.intersp = -0.03)
dev.off()

