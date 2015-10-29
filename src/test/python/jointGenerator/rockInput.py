#!/usr/bin/python
import numpy as np
import sys
from joint import Joint
import math

####################################################################################################
# Reads input text file containing data on rock volume and joints and
# outputs text file that can be read by SparkRocks. Currently, this assumes
# all input joints are persistent.
# 
# Usage: ./rockInput.py <inputFile> <outputFile>
#   
#         Where:
#         <inputFile> is the name of the input file containing relevant data
#         <outputFile> is the name of file to write results to
#
# INPUT FILE FORMAT:
# Format of input file should be as follows:
# Line1: Coordinates of lowerleft and upper right corners of bounding box than
#        encapsulates entire rock volume. The lower left corner is specified first
#        followed by the upper right as follows:
#        x_l y_l z_l x_r y_r z_r
# 
# Line2: Global origin specified as follows:
#        x_o y_o z_o
#        The global origin should be within bounding box - NOT ON BOUNDARY - and be as close as 
#        possible to lower left corner of bounding box. (This is pretty arbitrary for now, but 
#        will be changes later to be more general)
#
# Data that specifies the bounding surfaces(faces) is provided in Line3 and onward, as necessary
# The data for each face is specified on a new line as follows:
#     strike dip x_p y_p z_p
#     x_p, y_p and z_p represent the coordinates of a point on the face
# 
# Once all the bounding faces have been specified, the beginning of the joint data is indicated
# by an empty line. Following the empty line, the joint data is specified as follows:
#     strike dip jointSpacing
# 
# These joints represent the "master joints". The joint set will be generated based on the strike,
# dip and joint spacing for these joints starting from lower left corner of the bounding box.
#
# OUTPUT FILE FORMAT
# The output file contains the bounding faces and joint data in a format that is understood by 
# SparkRocks. 
# The first portion of the file specifies the bounding faces as follows:
#     a b c d phi cohesion
#     <a, b, c> represent the components of the normal vector to the face plane.
#     phi and cohesion are the friction angle and cohesion along the face plane respectively
# 
# The transition from the bounding surfaces date to the joint data is indicated by a line 
# containing "%" only. The lines following this specifiy the joint data as follows:
#     a b c x_o y_o z_o x_c y_c z_c phi cohesion
#     <a, b, c> represent the components of the normal vector to the joint plane
#     x_o, y_o and z_o represent the local origin for the joint
#     x_c, y_c and z_c represent the center of the joint in the joint plane
#     phi and cohesion are the friction angle and cohesion along the joint plane respectively
####################################################################################################

# Process command line arguments:
if len(sys.argv) != 3:
    print "Usage: {} <inputFile> <outputFile>".format(sys.argv[0])
    sys.exit(1)
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
    elif (block):
        facesData = np.append(facesData, [np.array([float(point) for point in data[i].split(' ')])], axis = 0)
    else:
        jointsData = np.append(jointsData, [np.array([float(point) for point in data[i].split(' ')])], axis = 0)

faces = np.empty(0, dtype = object)
masterJoints = np.empty(0, dtype = object)
joints = np.empty(0, dtype = object)

# Generate joints that represent the bounds for the rock volume
for face in facesData:
    center = np.array([face[2], face[3], face[4]])
    faces = np.append(faces, Joint(origin, center, face[0], face[1], boundingBox,
                                   spacing = None, phi = 0.0, cohesion = 0.0, bound = True))

# Generate "master joint set". These will be copied based on joint spacing
for joint in jointsData:
    # center of master joints at lower left corner of bounding box
    center = np.array([boundingBox[0], boundingBox[1], boundingBox[2]])
    masterJoints = np.append(masterJoints, 
                             Joint(origin, center, joint[0], joint[1], boundingBox, joint[2]))

# Create complete joint set from master joints: replicate master joints, but increment center
# of joint by spacing
for joint in masterJoints:
    cutoff = origin
    count = 1.0

    # This checks for joints that have normal vector parallal to one of the global x,y or z-axes.
    # Iteration constraints are modified accordingly to avoid an infinite loop
    if ((abs(joint.normalVec[0] - 1.0) < Joint.EPSILON) or
        (abs(joint.normalVec[1] - 1.0) < Joint.EPSILON) or
        (abs(joint.normalVec[2] - 1.0) < Joint.EPSILON)):

        while ((boundingBox[0] <= cutoff[0] <= boundingBox[3]) and
               (boundingBox[1] <= cutoff[1] <= boundingBox[4]) and
               (boundingBox[2] <= cutoff[2] <= boundingBox[5])):
            # Increment center location based on joint spacing
            centerIncrement = count * joint.spacing * joint.normalVec
            newCenter = np.array([joint.center[0] + centerIncrement[0], 
                                  joint.center[1] + centerIncrement[1],
                                  joint.center[2] + centerIncrement[2]])
            
            # Check that increment is into bounding box. If not, reverse direction in which incremented
            if ((newCenter[0] < boundingBox[0]) or
                (newCenter[1] < boundingBox[1]) or
                (newCenter[2] < boundingBox[2])):
                centerIncrement = -count * joint.spacing * joint.normalVec

            count = count + 1.0
            joints = np.append(joints,
                               Joint(joint.localOrigin,
                                     np.array([joint.center[0] + centerIncrement[0], 
                                               joint.center[1] + centerIncrement[1],
                                               joint.center[2] + centerIncrement[2]]),
                                     joint.strike, joint.dip, boundingBox, joint.spacing))
            cutoff = np.array([joint.center[0] + centerIncrement[0], 
                               joint.center[1] + centerIncrement[1],
                               joint.center[2] + centerIncrement[2]])


    else:
        while ((boundingBox[0] < cutoff[0] < boundingBox[3]) or
               (boundingBox[1] < cutoff[1] < boundingBox[4]) or
               (boundingBox[2] < cutoff[2] < boundingBox[5])):
            # Increment center location based on joint spacing
            centerIncrement = count * joint.spacing * joint.normalVec
            newCenter = np.array([joint.center[0] + centerIncrement[0], 
                                  joint.center[1] + centerIncrement[1],
                                  joint.center[2] + centerIncrement[2]])
            
            # Check that increment is into bounding box. If not, reverse direction in which incremented
            if ((newCenter[0] < boundingBox[0]) or
                (newCenter[1] < boundingBox[1]) or
                (newCenter[2] < boundingBox[2])):
                centerIncrement = -count * joint.spacing * joint.normalVec

            count = count + 1.0
            joints = np.append(joints,
                               Joint(joint.localOrigin,
                                     np.array([joint.center[0] + centerIncrement[0], 
                                               joint.center[1] + centerIncrement[1],
                                               joint.center[2] + centerIncrement[2]]),
                                     joint.strike, joint.dip, boundingBox, joint.spacing))
            cutoff = np.array([joint.center[0] + centerIncrement[0], 
                               joint.center[1] + centerIncrement[1],
                               joint.center[2] + centerIncrement[2]])

# Write face data to output file:
f = open(outputFile, 'w')

for face in faces:
    f.write(str(face.normalVec[0])+" "+str(face.normalVec[1])+" "+str(face.normalVec[2])+" "+
            str(face.distance)+" "+str(face.phi)+" "+str(face.cohesion)+" \n")


# Write symbol to show transition to joint data (Yes, this is silly...)
f.write("%\n")

# Write joint data to output file
for joint in joints:
    f.write(str(joint.normalVec[0])+" "+str(joint.normalVec[1])+" "+str(joint.normalVec[2])+" "+
            str(joint.localOrigin[0])+" "+str(joint.localOrigin[1])+" "+str(joint.localOrigin[2])+
            " "+str(joint.center[0])+" "+str(joint.center[1])+" "+str(joint.center[2])+" "+
            str(joint.phi)+" "+str(joint.cohesion)+" \n")

f.close

print "Input for sparkRocks written to: ", outputFile
