import numpy as np
import math

# These are support functions for rockInput.py
# They convert the input data to something that can be read by sparkRocks

class Joint:
    EPSILON = 1E-6

    @staticmethod
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
        if (abs(dip - 90.0) < Joint.EPSILON):
            dipVector = np.array([0.0, 0.0, -1.0])
        else:
            dipVector = np.array([math.cos(-(strikeRadians + math.pi/2.0)), 
                                  math.sin(-(strikeRadians + math.pi/2.0)), 
                                  -math.sin(dipRadians)])   
        
        strikeCrossDip = np.cross(strikeVector, dipVector)
        return strikeCrossDip / np.linalg.norm(strikeCrossDip)

    @staticmethod
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
        
        if (abs(normalVec[2]) >= Joint.EPSILON):
            w[0] = localOrigin[0]
            w[1] = localOrigin[1]
            w[2] = localOrigin[2] - center[2]
        elif (abs(normalVec[1]) >= Joint.EPSILON):
            w[0] = localOrigin[0]
            w[1] = localOrigin[1] - center[1]
            w[2] = localOrigin[2]
        elif (abs(normalVec[0]) >= Joint.EPSILON):
            w[0] = localOrigin[0] - center[0]
            w[1] = localOrigin[1]
            w[2] = localOrigin[2]
        
        n = np.array([normalVec[0], normalVec[1], normalVec[2]])
        return -np.vdot(n, w)/np.linalg.norm(n)
    
    @staticmethod
    def distanceSignCheck(distance, normalVector):
        """
        This function checks the distance of the joint and modifies the input normal vector and distance
        if the input distance is negative.
        
        Input:
        distance:      Signed distance of the joint to local origin
        normalVector:  Numpy array that represents the three components of the normal vector of the joint plane
        
        Output:        If distance is positive, return unchanged inputs. If distance is negative, returns distance
                       and normal vector with signs reversed
        """
        if (distance < 0.0):
            return (-distance, np.array((-normalVector[0], -normalVector[1], -normalVector[2]), dtype = float))
        else:
            return (distance, np.array((normalVector[0], normalVector[1], normalVector[2]), dtype = float)) 

    def zeroDistanceCheck(self, normalVector, boundingBox):
        """
        This function determines what the orientation of the normal vector should be such that it points outward from
        the initial rock volume when the distance of the joint is zero.
    
        Input:
        normalVector:  Normal vector to the joint plane, given as a numpy array with three components that 
                       represent the vector
        boundingBox:   Numpy array that contains coordinates of the lower left and upper right corners of bounding box

        Output:        Normal vector with orientation that point outward from the input rock volume
        """
        shiftedOrigin = np.array((self.localOrigin[0] + 0.1*(boundingBox[3] - boundingBox[0]),
                                  self.localOrigin[1] + 0.1*(boundingBox[4] - boundingBox[1]),
                                  self.localOrigin[2] + 0.1*(boundingBox[5] - boundingBox[2])), 
                                  dtype = float)
        distance = self.findDistance(normalVector, shiftedOrigin, self.center)
        if (distance < 0.0):
            return np.array((-normalVector[0], -normalVector[1], -normalVector[2]), dtype = float)
        else:
            return normalVector

    def __init__(self, localOrigin, center, strike, dip, boundingBox,
                 spacing = None, phi = 0.0, cohesion = 0.0, bound = False):
        """
        Constructor for Joint class that represents discontinuity in rock mass
        
        Input:
        localOrigin:  Local origin that distance will be referenced to.
        center:       Center of joint in joint plane
        strike:       Strike of joint in degrees
        dip:          Dip of joint in degrees
        boundingBox:  Numpy array that contains coordinates of the lower left and upper right corners of bounding box.
                      Global origin should be as close as possible to lower left corner of bounding box (NOTE: This is
                      pretty arbitrary for now, but will be changed later to be more general)
        spacing:      Spacing between joints
        phi:          Friction angle along joint plane
        cohesion:     Cohesion along joint plane
        bound:        True for joints that represent faces on the input rock volume
        """

        self.localOrigin = localOrigin
        self.center = center
        tempNormal = self.jointNormal(strike, dip)
        tempDistance = self.findDistance(tempNormal, self.localOrigin, self.center)
        if ((bound == False) and (abs(tempDistance) > 0.0)):
            self.distance, self.normalVec = tempDistance, tempNormal
        # This ensures that the distance for bounding faces on the input rock volume are always positive
        elif (abs(tempDistance) < Joint.EPSILON):
            self.distance, self.normalVec = tempDistance, self.zeroDistanceCheck(tempNormal, boundingBox)
        else:
            self.distance, self.normalVec = self.distanceSignCheck(tempDistance, tempNormal)
        self.spacing = spacing
        self.phi = phi
        self.cohesion = cohesion
        self.strike = strike
        self.dip = dip

    # Updates joint distance based on new local origin
    def updateDistance(self, updatedOrigin):
        self.distance = findDistance(self.normalVec, updatedOrigin, self.center)
