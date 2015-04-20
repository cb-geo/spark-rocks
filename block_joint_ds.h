/* SIMPLY COPIED FROM PAGE 8 OF THE BOON ET AL. (2015) PAPER. */

struct Discontinuity {  
  /* Comment: (centre_x, centre_y, centre_z) is the position of the discontinuity */
  double centre_x;
  double centre_y;
  double centre_z;
  /* double dip angle and dip direction */
  double dip; 
  double dipDir; 
  /* Comment: (a,b,c) is the vector normal to the plane of the discontinuity */
  double a; 
  double b; 
  double c; 
  /* Comment: d is the distance of the discontinuity from the local centre */
  double d;
  /* Comment: phi is the friction angle along the discontinuity */
  double phi;
  /* Comment: cohesion is the cohesion along the discontinuity */
  double cohesion; 
  /* Comment: number of lines delimiting the joint extent */
  int N_lines; 
  /* Comment: (shape_a[i], shape_b[i]) is the ith line representing the polygonal shape of the joint with respect to local coordinates orthogonal to the joint normal. Define the arrays with entry sizes larger than the largest expected number of lines delimiting the joint extent, N_lines. */
  double shape_a[10];
  double shape_b[10];
  /* Comment: shape_d[i] is the distance of the ith trace line with respect to the local coordinates orthogonal to the joint normal */
  double shape_d[10];
};

struct Block {
  /* Comment: (centre_x, centre_y, centre_z) is the position of the block */
  double centre_x;
  double centre_y;
  double centre_z;
  /* Comment: number of planes defining the block */
  int N_planes;
  /* Comment: (a[i], b[i], c[i]) is the normal vector of the ith entry of the faces. Define tje arrays with entry sizes larger than the largest expected number of faces */
  double a[40];
  double b[40];
  double c[40];
  /* Comment: d[i] is the distance of the (a[i], b[i], c[i]) face from the local centre */
  double d[40]; 
  /* Comment: phi[i] is the friction angle of the (a[i], b[i], c[i]) face */
  double phi[40];
  /* Comment: cohesion[i] is the cohesion of the (a[i], b[i], c[i]) face*/
  double cohesion[40];
};
