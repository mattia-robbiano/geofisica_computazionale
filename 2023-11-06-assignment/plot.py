#python code to plot the data in the file temperature_latitudine.txt and 2d plot of temperature_filtrate.txt and temperature_mediate.txt
import matplotlib.pyplot as plt
from mpl_toolkits.mplot3d import Axes3D
import numpy as np

# Define the file path
FILEPATH = 'temperature_latitudine.txt'
matrix = np.loadtxt('temperature_filtrate.txt')

#---------------------------------------------------------

# Initialize lists to store x and y values
x_values = []
y_values = []

# Read the data from the file
with open(FILEPATH, 'r', encoding='utf-8') as file:
    for line in file:
        x, y = map(float, line.split())
        x_values.append(x)
        y_values.append(y)

# Create a scatter plot
plt.scatter(x_values, y_values, label="Temperature")

# Customize the plot
plt.title("Temperature vs. Latitude")
plt.xlabel("latitude")
plt.ylabel("temperature")
plt.legend()
plt.grid(True)

plt.savefig('temperature.png')

#---------------------------------------------------------
# Get dimensions of the matrix
matrix_size_x, matrix_size_y = matrix.shape

# Generate x and y values
x = np.linspace(0, matrix_size_y - 1, matrix_size_y)  
y = np.linspace(0, matrix_size_x - 1, matrix_size_x)

# Create meshgrid for x and y
x, y = np.meshgrid(x, y)

# Create a 3D plot
fig = plt.figure()
ax = fig.add_subplot(111, projection='3d')

# Plot the surface
ax.plot_surface(x, y, matrix, cmap='viridis')

# Set labels
ax.set_xlabel('X-axis')
ax.set_ylabel('Y-axis')
ax.set_zlabel('Z-axis')

# Save the plot with different rotations
for angle in range(0, 360, 10):
    ax.view_init(elev=20, azim=angle)
    plt.savefig(f'./immagini/output_plot_{angle}.png')