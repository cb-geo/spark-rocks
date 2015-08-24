#!/usr/bin/python
import numpy as np
import sys
from Joint import Joint
import math

# Reads input text file containing data on rock volume and joints
# Outputs text file that can be read by sparkRocks

# Process command line arguments:
inputFile = open(str(sys.argv[1]), 'r')
outputFile = str(sys.argv[2])

# Read input file
data = [line.rstrip('\n') for line in inputFile]

# Read bounds for bounding box: 
#     First three numbers are lower left corner coordinates, 
#     last three numbers are upper right corner coordinates
boundingBox = np.array([float(coordinate) for coordinate in data[0].split(' ')])

# Global origin for block
origin = np.array([float(coordinate) for coordinate in data[1].split(' ')])

facesData = np.empty([0, 5], dtype = float)
jointsData = np.empty([0, 3], dtype = float)

# Read in block face and joint data:
#     Data for block faces specified as follows: Strike, dip, coordinates for point in face
#     Data for joints specified as follows: Strike, dip, joint spacing

block = True
for i in range(2, len(data)):
    if (data[i] == ''): 
        block = False
    elif (block == True):
        facesData = np.append(facesData, 
                          [np.array([float(point) for point in data[i].split(' ')])], axis = 0)
    else:
        jointsData = np.append(jointsData, 
                           [np.array([float(point) for point in data[i].split(' ')])], axis = 0)

faces = np.empty(0, dtype = object)
masterJoints = np.empty(0, dtype = object)
joints = np.empty(0, dtype = object)

# Generate joints that represent the bounds for the rock volume
for face in facesData:
    center = np.array([face[2], face[3], face[4]])
    faces = np.append(faces, Joint(origin, center, face[0], face[1]))

# Generate "master joint set". These will be copied based on joint spacing
for joint in jointsData:
    # localOrigin = np.array([boundingBox[0], boundingBox[1], boundingBox[2]])
    masterJoints = np.append(masterJoints, 
                             Joint(origin, origin, joint[0], joint[1], joint[2]))

# Create complete joint set from master joints
for joint in masterJoints:
    cutoff = origin
    count = 1.0
    while (((cutoff[0] < boundingBox[3]) and (cutoff[0] > boundingBox[0])) and
           ((cutoff[1] < boundingBox[4]) and (cutoff[1] > boundingBox[1])) and
           ((cutoff[2] < boundingBox[5]) and (cutoff[2] > boundingBox[2]))):
        # Increment center location based on joint spacing
        centerIncrement = count * joint.spacing * joint.normalVec
        count = count + 1.0
        joints = np.append(joints,
                           Joint(joint.localOrigin,
                                 np.array([joint.center[0] + centerIncrement[0], 
                                           joint.center[1] + centerIncrement[1],
                                           joint.center[2] + centerIncrement[2]]),
                                 joint.strike, joint.dip, joint.spacing))
        cutoff = np.array([joint.center[0] + centerIncrement[0], 
                           joint.center[1] + centerIncrement[1],
                           joint.center[2] + centerIncrement[2]])

    cutoff = origin
    count = 1.0
    while (((cutoff[0] < boundingBox[3]) and (cutoff[0] > boundingBox[0])) and
           ((cutoff[1] < boundingBox[4]) and (cutoff[1] > boundingBox[1])) and
           ((cutoff[2] < boundingBox[5]) and (cutoff[2] > boundingBox[2]))):
        # Increment center location based on joint spacing
        centerIncrement = count * joint.spacing * joint.normalVec
        count = count + 1.0
        joints = np.append(joints,
                           Joint(joint.localOrigin,
                                 np.array([joint.center[0] - centerIncrement[0], 
                                           joint.center[1] - centerIncrement[1],
                                           joint.center[2] - centerIncrement[2]]),
                                 joint.strike, joint.dip, joint.spacing))
        cutoff = np.array([joint.center[0] - centerIncrement[0], 
                           joint.center[1] - centerIncrement[1],
                           joint.center[2] - centerIncrement[2]])

# Write face data to output file:
f = open(outputFile, 'w')

for face in faces:
    # f.write("%.12f %.12f %.12f %.12f %.12f %.12f\n" % (face.normalVec[0], face.normalVec[1], face.normalVec[2], 
    #                                face.distance, face.phi, face.cohesion))
    f.write(str(round(face.normalVec[0], 1))+" "+str(round(face.normalVec[1], 1))+" "+str(round(face.normalVec[2], 1))+" "+
            str(round(face.distance, 1))+" "+str(round(face.phi, 1))+" "+str(round(face.cohesion, 1))+" \n")

# Write symbol to show transition to joint data (Yes, this is silly...)
f.write("%\n")

# Write joint data to output file
for joint in joints:
    # f.write("%.12f %.12f %.12f %.12f %.12f %.12f %.12f %.12f %.12f %.12f %.12f\n" %
    #         (joint.normalVec[0], joint.normalVec[1], joint.normalVec[2], 
    #          joint.localOrigin[0], joint.localOrigin[1], joint.localOrigin[2],
    #          joint.center[0], joint.center[1], joint.center[2],
    #          joint.phi, joint.cohesion))
    f.write(str(round(joint.normalVec[0], 1))+" "+str(round(joint.normalVec[1], 1))+" "+str(round(joint.normalVec[2], 1))+" "+
            str(round(joint.localOrigin[0], 1))+" "+str(round(joint.localOrigin[1], 1))+" "+str(round(joint.localOrigin[2], 1))+
            " "+str(round(joint.center[0], 1))+" "+str(round(joint.center[1], 1))+" "+str(round(joint.center[2], 1))+" "+
            str(round(joint.phi, 1))+" "+str(round(joint.cohesion, 1))+" \n")

f.close

print "Input for sparkRocks written to: ", outputFile
