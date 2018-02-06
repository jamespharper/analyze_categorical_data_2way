# Title: Analyze Categorical Data, Two Way
# Desription: Analyze a categorical dataset in pairs of variables using
#             frequency tables and mosaic plots with chi squared, p-values and
#             Cramer's v to test for statistical significance
# Author: James Harper, PE, ENV SP
# Date Created: February 4, 2018
# -----------------------------------------------------------------------------


###############################################################################
###############################################################################
########## CODE NOT WORKING (YET) #############################################
###############################################################################
###############################################################################

library(rio)

data = import("example_dataset_categorical.xlsx")

print("Running two-way tests...")

# Create permutations of metrics to test for association
metrics_2way = permutations(n = length(data), r = 2, v = 1:length(data), repeats.allowed = FALSE)
if (var_interest[[1]] != 0) {
  temp = list()
  for (i in 1:length(var_interest)) {
    if (length(temp) == 0) {
      temp = metrics_2way[metrics_2way[,1] == var_interest[[i]],]
    } else {
      temp = rbind(temp, metrics_2way[metrics_2way[,1] == var_interest[[i]],])
    }
  }
  metrics_2way = temp
  # print(metrics_2way)
}

# Run 2-way categorical analyses
for (num in 1:length(metrics_2way[,1])) {
  print(paste(metrics_2way[num,1],"_",metrics_2way[num,2]))
  categorical_analysis_2way(data, metrics_2way[num,1], metrics_2way[num,2])
}



for (num1 in 1:length(data)) {
  print(num1)
  metric1 = num1
  metric2 = num2
  
    # Also used for stratified one-way categorical analysis
    
    # Create temporary vectors from data
    A = data[metric1][[1]]
    B = data[metric2][[1]]
    
    # Start sending text output to dump file
    file1 = file(paste(getwd(),"/Output/dump.txt", sep = ""))
    sink(file1, append = TRUE)
    sink(file1, append = TRUE, type = "message")
    
    # Perform categorical analyses
    freqs = table(A, B)                            # Create frequency table for stats
    # print(freqs)
    # freqs = freqs[order(-freqs[,1]),]              # Sort table by frequency in first column
    # print(freqs)
    # print(fisher.test(freqs))                      # Fisher Exact test
    chisq_cramv = assocstats(freqs)                # Calculate chi squared and Cramer's V
    
    # Stop sending text output to dump file
    sink()
    sink(type = "message")
    
    # Create file name and plot name variables that includes p-value and Cramer's V
    p_value = round(chisq_cramv$chisq_tests[2,3], digits = 3)
    chisqd = round(chisq_cramv$chisq_tests[2,1], digits = 3)
    cramer_v = round(chisq_cramv$cramer, digits = 3)
    name = paste("freqs_2way_", p_value, "_", chisqd, "_", cramer_v, "_", names(data)[[metric1]], "_", names(data)[[metric2]], sep = "")
    plot_name = paste(names(data)[[metric1]], "_", names(data)[[metric2]], "_", p_value, "_", chisqd, "_", cramer_v, sep = "")
    
    # Start sending text output to text file in a given folder based on p_values
    if (is.nan(p_value)) {
      
      # Create output folder
      folder = create_folder(subfolder = "p is NaN")
      
      # Start sending text output to text file in folder
      file1 = file(paste(folder, "/", name, ".txt", sep = ""))
      sink(file1, append = TRUE)
      sink(file1, append = TRUE, type = "message")
      
    } else if (p_value > 0.05) {
      
      # Create output folder
      folder = create_folder(subfolder = "p above 0.05")
      
      # Start sending text output to text file in folder
      file1 = file(paste(folder, "/", name, ".txt", sep = ""))
      sink(file1, append = TRUE)
      sink(file1, append = TRUE, type = "message")
      
    } else {
      
      # Create output folder
      folder = create_folder(subfolder = "")
      
      # Start sending text output to text file in folder
      file1 = file(paste(folder, "/", name, ".txt", sep = ""))
      sink(file1, append = TRUE)
      sink(file1, append = TRUE, type = "message")
      
    }
    
    # Add title to text file
    print(paste("A = ", names(data)[[metric1]], sep = ""))
    print(paste("B = ", names(data)[[metric2]], sep = ""))
    
    # Print results of analyses to text file
    print(CrossTable(A, B))
    # print(ftable(freqs))
    print(summary(freqs))
    print(chisq_cramv)
    
    # Stop sending text output to file
    sink()
    sink(type = "message")
    
    # Start saving plot to PDF in a given folder based on p_values
    if (is.nan(p_value)) {
      folder = create_folder(subfolder = "p is NaN")       # Create output folder
      pdf(paste(folder, "/", name, ".pdf", sep = ""))      # Start saving plot to PDF in folder
    } else if (p_value > 0.05) {
      folder = create_folder(subfolder = "p above 0.05")   # Create output folder
      pdf(paste(folder, "/", name, ".pdf", sep = ""))      # Start saving plot to PDF in folder
    } else {
      folder = create_folder(subfolder = "")               # Create output folder
      pdf(paste(folder, "/", name, ".pdf", sep = ""))      # Start saving plot to PDF in folder
    }
    
    # Generate categorical analysis plots
    if (length(freqs[,1]) < 50) {
      mosaic(freqs, shade = TRUE, legend = TRUE, main = plot_name)          # Mosaic plot
    } else {
      mosaic(freqs, shade = TRUE, legend = TRUE, main = plot_name)          # Mosaic plot
      # barplot(table(B,A), main=name, legend = colnames(freqs))       # Stacked bar plot with legend
    }
    
    # Stop sending plot output to file
    dev.off()
    closeAllConnections()
    
  }
  
  
}
print("One-way tests completed.")
