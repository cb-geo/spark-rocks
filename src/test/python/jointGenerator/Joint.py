import numpy as np
import math

# These are support functions for rockInput.py
# They convert the input data to something that can be read by sparkRocks

EPSILON = 1E-6

def jointNormal(strike, dip):
    """
    This function takes the input strike and dip and then converts 
    that to a normal vector
    
    Input:
    strike: Strike of the joint plane given in degrees.
    dip:    Dip of the joint plane given in degrees.

    Output:
    Numpy array that specifies the normal vector of the input joint plane

    >>> jointNormal(0.0, 0.0)
    array([ 0.,  0., -1.])
    >>> jointNormal(0.0, 90.0)
    array([ 0.,  1.,  0.])
    """

    strikeRadians = strike * np.pi/180.0
    dipRadians = dip * np.pi/180.0
    strikeVector = np.array([math.cos(-strikeRadians), math.sin(-strikeRadians), 0.0])
    if (abs(dip - 90.0) < EPSILON):
        dipVector = np.array([0.0, 0.0, -1.0])
    else:
        dipVector = np.array([math.cos(-(strikeRadians + math.pi/2.0)), 
                  math.sin(-(strikeRadians + math.pi/2.0)), 
                  -math.sin(dipRadians)])   
    
    strikeCrossDip = np.cross(strikeVector, dipVector)
    return strikeCrossDip / np.linalg.norm(strikeCrossDip)

def findDistance(normalVec, localOrigin, center):
    """
    This function finds the distance of a joint plane from a given local origin

    Input:
    normalVec:   numpy array that specifies the normal vector of the joint plane
    localOrigin: numpy array that specifies the coordinate of the local origin
    center:      numpy array that specifies the center of the joint plane

    Output:
    Shortest distance of joint to local origin
    """

    w = np.empty(3, dtype = float)

    if (abs(normalVec[2]) >= EPSILON):
        w[0] = localOrigin[0]
        w[1] = localOrigin[1]
        w[2] = localOrigin[2] - center[2]
    elif (abs(normalVec[1]) >= EPSILON):
        w[0] = localOrigin[0]
        w[1] = localOrigin[1] - center[1]
        w[2] = localOrigin[2]
    elif (abs(normalVec[0]) >= EPSILON):
        w[0] = localOrigin[0] - center[0]
        w[1] = localOrigin[1]
        w[2] = localOrigin[2]
        
    n = np.array([normalVec[0], normalVec[1], normalVec[2]])
    return -np.vdot(n, w)/np.linalg.norm(n)

class Joint:
    def __init__(self, localOrigin, center, strike, dip, spacing = None, phi = 0.0, cohesion = 0.0):
        self.normalVec = jointNormal(strike, dip)
        self.localOrigin = localOrigin
        self.center = center
        self.distance = findDistance(self.normalVec, self.localOrigin, self.center)
        self.spacing = spacing
        self.phi = phi
        self.cohesion = cohesion
        self.strike = strike
        self.dip = dip

    def updateDistance(self, updatedOrigin):
        self.distance = findDistance(self.normalVec, updatedOrigin, self.center)
