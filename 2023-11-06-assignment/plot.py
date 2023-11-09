import matplotlib.pyplot as plt

# Define the file path
FILEPATH = 'temperature_latitudine.txt'  # Replace with your file path

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

# Save the plot as a PNG image
plt.savefig('temperature.png')
