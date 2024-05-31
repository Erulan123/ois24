# Generate sample data
set.seed(42)  # For reproducible random data
x <- rnorm(100)
y <- rnorm(100, mean=5)
z <- rnorm(100, mean=10)

# Prepare for the 3D plot
zlim <- range(c(x, y, z))  # Set the limits for the plot
zlen <- zlim[2] - zlim[1] + 1
color.palette <- terrain.colors(zlen)  # Get a palette of colors
col <- color.palette[z - zlim[1] + 1]  # Assign colors based on the z value

# Plotting the 3D scatter plot
plot3d <- function(x, y, z, col, main = "3D Scatter Plot") {
  # Set up the perspective plot
  persp(x = seq(min(x), max(x), length.out = 2),
        y = seq(min(y), max(y), length.out = 2),
        z = matrix(c(min(z), max(z), min(z), max(z)), ncol = 2),
        col = "white", # Hide the default surface
        theta = 30, phi = 30,
        xlim = range(x), ylim = range(y), zlim = range(z),
        xlab = "X Axis", ylab = "Y Axis", zlab = "Z Axis",
        main = main,
        border = NA, # No borders around the base plot
        box = TRUE)  # Draw a box around the plot
  
  # Add points to the plot
  points(trans3d(x, y, z, perspMatrix(seq(min(x), max(x), length.out = 2),
                                      seq(min(y), max(y), length.out = 2),
                                      zlim, theta = 30, phi = 30)),
         col = col, pch = 16)
}

# Call the function to create the plot
plot3d(x, y, z, col)