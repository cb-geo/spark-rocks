#!/usr/bin/python
import sys
import numpy as np
import math

# This file generates a cube with specified side lengths and generates the joints
# necessary to get the requested number of blocks.

# First command line argument:  Output file name
# Second command line argument: Number of blocks desired

# Process command line inputs
outputFile = str(sys.argv[1])
nBlocks = float(sys.argv[2])

numberBlocks = int(nBlocks)

divisions = int(math.ceil(numberBlocks**(1/3.0)))
dl = 1.0
# dl = float(sideLength/divisions)
sideLength = divisions * dl

# Generate faces for cube
faces = np.empty([6,6], dtype=float)
faces[0] = np.array([1, 0, 0, sideLength, 0, 0])
faces[1] = np.array([0, 1, 0, sideLength, 0, 0])
faces[2] = np.array([0, 0, 1, sideLength, 0, 0])
faces[3] = np.array([-1, 0, 0, 0, 0, 0])
faces[4] = np.array([0, -1, 0, 0, 0, 0])
faces[5] = np.array([0, 0, -1, 0, 0, 0])

# Generate joints
joints_x = np.empty([divisions-1, 11], dtype=float)
joints_y = np.empty([divisions-1, 11], dtype=float)
joints_z = np.empty([divisions-1, 11], dtype=float)

for i in range(0, divisions-1):
    joints_x[i] = np.array([1, 0, 0, i*dl + dl, i*dl + dl, sideLength/2.0, sideLength/2.0, 0, 0, 0, 0])
    joints_y[i] = np.array([0, 1, 0, i*dl + dl, sideLength/2.0, i*dl + dl, sideLength/2.0, 0, 0, 0, 0]) 
    joints_z[i] = np.array([0, 0, 1, i*dl + dl, sideLength/2.0, sideLength/2.0, i*dl + dl, 0, 0, 0, 0]) 

# Write faces to output file
f = open(outputFile,'w')

for i in range(0, faces.shape[0]):
    for j in range(0, faces.shape[1]):
        f.write(str(faces[i][j])+" ")
    f.write("\n")

# Write symbol to show transition to joint data (Yes, this is silly... we can change it if you like!)
f.write("%\n")

# Write joints to output file
for i in range(0, joints_x.shape[0]):
    for j in range(0, joints_x.shape[1]):
        f.write(str(joints_x[i][j])+" ")
    f.write("\n")

for i in range(0, joints_y.shape[0]):
    for j in range(0, joints_y.shape[1]):
        f.write(str(joints_y[i][j])+" ")
    f.write("\n")

for i in range(0, joints_z.shape[0]):
    for j in range(0, joints_z.shape[1]):
        f.write(str(joints_z[i][j])+" ")
    f.write("\n")

f.close

print "The number of blocks generated: ", divisions**3
