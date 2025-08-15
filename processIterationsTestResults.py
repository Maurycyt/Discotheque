#!/usr/bin/env python

import numpy as np
import io
import matplotlib.pyplot as plt

with open('iterationsTestResultsSimplified', 'r') as f:
    data = f.read()

# Use a dictionary to store the results, with (i, j) as keys
results = {}
max_i, max_j = 0, 0

# --- Process Data ---
# Use a dictionary to store the results for the table
results = {}
max_i, max_j = 0, 0

# Lists to store data for plotting
plot_x_values = []
plot_y_values = []

# The rest of the script remains the same
lines = data.strip().split('\n\n')
for block in lines:
    # Skip empty blocks that might result from extra newlines
    if not block.strip():
        continue
    lines_in_block = block.strip().split('\n')
    i, j = map(int, lines_in_block[0].split())
    n = int(lines_in_block[1])

    # Calculate the value using the simplified formula: i*j - log2(n)
    value = i * j - np.log2(n)
    results[(i, j)] = value

    # Store values for the plot
    plot_x_values.append(i * j)
    plot_y_values.append(value)

    # Keep track of the maximum indices to format the table
    if i > max_i:
        max_i = i
    if j > max_j:
        max_j = j

# --- Generate LaTeX Table ---
# The column specifier is dynamically created based on the number of columns
col_spec = "c" * (max_j + 1)
latex_table = f"\\begin{{tabular}}{{{col_spec}}}\n"
latex_table += "\\hline\n"

# Create the header row for j indices
header = ["i/j"] + [str(j_idx) for j_idx in range(1, max_j + 1)]
latex_table += " & ".join(header) + " \\\\\n"
latex_table += "\\hline\n"

# Create each data row
for i in range(1, max_i + 1):
    row = [str(i)]
    for j in range(1, max_j + 1):
        if (i, j) in results:
            row.append(f"{results[(i, j)]:.3f}")
        else:
            row.append("")
    latex_table += " & ".join(row) + " \\\\\n"

latex_table += "\\hline\n"
latex_table += "\\end{tabular}"

# Print the final LaTeX table to the console
print("## LaTeX Table")
print("Copy and paste the code below into your LaTeX document.")
print("-" * 20)
print(latex_table)
print("-" * 20)

# --- Generate Plot ---
print("\nGenerating plot...")
plt.figure(figsize=(10, 6)) # Create a figure

# Create a scatter plot
plt.scatter(plot_x_values, plot_y_values, label="Data points", color='royalblue', alpha=0.8)

# Add titles and labels for clarity
plt.title("Logarithm Difference vs. i * j", fontsize=16)
plt.xlabel("Value of i * j", fontsize=12)
# Using LaTeX formatting for the y-axis label
plt.ylabel(r"$\log_2\left(\frac{2^{i \cdot j}}{n}\right)$", fontsize=14)

# Add a grid for better readability
plt.grid(True, linestyle='--', alpha=0.6)
plt.legend()
plt.tight_layout() # Adjust layout to ensure everything fits

# Save the plot as an SVG file
plot_filename = "log_difference_plot.svg"
plt.savefig(plot_filename, format="svg")

print(f"Plot successfully saved as '{plot_filename}'")

# Convert lists to numpy arrays for easier calculations
x_data = np.array(plot_x_values)
y_data = np.array(plot_y_values)


# --- Quadratic Regression ---
# Perform a polynomial fit of degree 2 (quadratic)
# This finds the coefficients [a, b, c] for the equation y = ax^2 + bx + c
# that minimize the squared error (least squares method).
coefficients = np.polyfit(x_data, y_data, 2)
a, b, c = coefficients

print("--- Quadratic Fit Results ---")
print(f"The best-fitting quadratic function is: y = {a:.4f}x^2 + {b:.4f}x + {c:.4f}")
print("-" * 30)


# --- Plotting the Results ---
print("\nGenerating plot with the fitted curve...")

# Create a set of x-values for the smooth curve
x_fit = np.linspace(min(x_data), max(x_data), 400)
# Calculate the corresponding y-values using the found coefficients
y_fit = a * x_fit**2 + b * x_fit + c

# Create the plot
plt.figure(figsize=(10, 6))

# Plot the original data points
plt.scatter(x_data, y_data, label="Original Data Points", color='royalblue', alpha=0.8, zorder=5)

# Plot the fitted quadratic curve
plt.plot(x_fit, y_fit, label=f"Fit: y = {a:.2f}x² + {b:.2f}x + {c:.2f}", color='crimson', linewidth=2)

# Add titles and labels for academic purposes
plt.title("Quadratic Regression of Logarithm Difference", fontsize=16)
plt.xlabel("Value of i * j", fontsize=12)
plt.ylabel(r"$\log_2\left(\frac{2^{i \cdot j}}{n}\right)$", fontsize=14)
plt.grid(True, linestyle='--', alpha=0.6)
plt.legend(fontsize=12)
plt.tight_layout()

# Save the final plot as an SVG file
plot_filename = "quadratic_fit_plot.svg"
plt.savefig(plot_filename, format="svg")

print(f"Plot successfully saved as '{plot_filename}'")
