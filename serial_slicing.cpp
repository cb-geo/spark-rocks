/* Supplementary material.  A new rock slicing algorithm based on linear programming. */

/* The routine calls the linear programming software IBM ILOG CPLEX 9.0.  This commercial software (IBM ILOG CPLEX) is freely available to academics through membership with the IBM Academic Initiative.  This routine was implemented into the open-source distinct element code, YADE.  The routine employs built-in functions in YADE. Refer to Fig. 12 in the manuscript */


/* The routine takes as input the rock joint data structure and the block data structure illustrated in Fig. 11 */
#include <ilcplex/cplex.h>
bool BlockGen::intersection_Block_Plane(struct Discontinuity joint, struct Block block){  

  /* Total number of block faces*/
  int faceNo = block.N_planes;
  /* Number of lines delimiting the joint */
  int lineNo = joint.N_lines;
  /* Variables to keep things neat */
  int NUMCON = 1 /* equality */ + faceNo /*block face inequality */ + lineNo /*boundaries of joint shape inequalities */; 
  int NUMVAR = 3/*3D x, y, z*/ + 1  /* s in Eq. (16) */; 
  double s = 0.0;
  bool converge = true;


  /* This routine is on Eq. (16) */
  /* The linear constraints and objective function in this routine can be modified to solve a different linear programming problem in the manuscript */

  /* In Eq. (16):
     The objective function (minimisation) uses an auxiliary variable to check whether all the constraints can be satisfied. Refer to Boyd, S.P. & Vandenberghe, L. (2004) Convex Optimization. Cambridge University Press. 
     The linear constraints consists of:
     (i) joint plane, equality (solution has to lie on the plane), 
     (ii) block faces, inequalities (solution has to be inside the polyhedron),
     (iii) lines defining the joint shape, inequality (solution has to be inside the joint area)
  */

  /* In Eq. (8):
     To identify geometrically redundant inequalities: the objective function (maximisation) is parallel to the normal vector of one of the block faces. The linear constraints consists of: (i) block faces, inequalities (solution has to be inside the polyhedron) */

  /* In Eq. (17):
     To work out the most positive and most negative x, y, z coordinates inside the polyhedron: the objective function (maximisation) is parallel to one of the Cartesian axes. The linear constraints consists of: (i) block faces, inequalities (solution has to be inside the polyhedron) */

  /* In Eq. (18):
     To work out the radius of the largest inscribed sphere: The objective function (minimisation) is the radius of the largest inscribed sphere The linear constraints consists of: (i) block faces, inequalities (solution has to be inside the polyhedron)*/

  /* To establish whether a pair of convex blocks are in contact for discontinuum analysis.  
     Refer to Boon, C.W., Houlsby G.T., Utili, S. (2012). A new algorithm for contact detection between polygonal and polyhedral particles in the discrete element method.  Computers and Geotechnics, 44, pp. 73-82.
     The objective function uses an auxiliary variable to check whether all the constraints can be satisfied (similar to Eq. (16) here)
     The linear constraints consists of:
     (i) the block faces of Polyhedron_A, inequalities (solution has to be inside Polyhedron_A)
     (ii) the block faces of Polyhedron_B, inequalities (solution has to be inside Polyhedron_B)
  */

  /* The lines defining the joint shape are transformed from local to global Eq. (14) and Eq. (15) using built-in functions used in the DEM code YADE*/
  double globalShape_a[lineNo];
  double globalShape_b[lineNo];
  double globalShape_c[lineNo];
  double globalShape_d[lineNo];

  for(int i=0; i<lineNo; i++ ){
    Vector3r localShapeLine = Vector3r(joint.shape_a[i], joint.shape_b[i], 0.0); //shape is defined in the x-y plane.  Local-z axis is the local direction of normal vector
    Vector3r Nplane (joint.a, joint.b, joint.c); //normal vector of the joint plane
    Vector3r Ndipdir = Vector3r(cos(joint.dipDir), sin(joint.dipDir), 0.0); //vector of dip direction
    Vector3r Nstrike = Vector3r(cos(joint.strike), sin(joint.strike), 0.0); //vector of strike
    Vector3r Ndip = Nplane.cross(Nstrike);
    Ndip.normalize();
    Matrix3r Qp;
    Qp (0,0) = Nstrike.x(); Qp(0,1) = Ndip.x(); Qp(0,2) = Nplane.x();
    Qp(1,0) = Nstrike.y();  Qp(1,1) = Ndip.y(); Qp(1,2) = Nplane.y();
    Qp(2,0)=Nstrike.z();    Qp(2,1) = Ndip.z();  Qp(2,2) = Nplane.z();
    Vector3r globalShapeLine = Qp*localShapeLine; //Eq. (14)
    globalShape_a[i] = globalShapeLine.x();
    globalShape_b[i] = globalShapeLine.y();
    globalShape_c[i] = globalShapeLine.z();
    globalShape_d[i] = joint.shape_d[i] + globalShape_a[i]*joint.centre_x + globalShape_b[i]*joint.centre_y + globalShape_c[i]*joint.centre_z; 
  }

  /* LINEAR CONSTRAINTS */
  char bkc[NUMCON]; 
  bkc[0] = 'E'; //Equality so that the solution falls on the joint plane 
  for(int i=0; i<faceNo; i++ ){
    bkc[1+i] = 'L'; //Inequalities for block faces, L = lower than
  };
  for(int i=0; i<lineNo; i++ ){
    bkc[1+faceNo+i] = 'L'; //Inequalities for lines delimiting rock joint, L = lower than
  };
  

  double buc[NUMCON]; 
  buc[0] = joint.d; // linear equality  
  for(int i=0; i<faceNo; i++  ){
    buc[1+i] = block.d[i]; //Upper bound of linear inequalities for block faces
  };
  for(int i=0; i<lineNo; i++ ){
    buc[1+faceNo+i] = globalShape_d[i]; //Upper bound of linear inequalities for lines delimiting the shape of rock joint (global coordinates). 
  };


  /* BOUNDS OF VARIABLES*/ 
  double blx[NUMVAR]; 
  blx[0] = -CPX_INFBOUND; //0 x 
  blx[1] = -CPX_INFBOUND; //1 y
  blx[2] = -CPX_INFBOUND; //2 z
  blx[3] = -CPX_INFBOUND; //3 s
  
  double bux[NUMVAR]; 
  bux[0] = +CPX_INFBOUND; //0 x
  bux[1] = +CPX_INFBOUND; //1 y
  bux[2] = +CPX_INFBOUND; //2 z
  bux[3] = +CPX_INFBOUND; //3 s
  
  /* OBJECTIVE FUNCTION */
  double c[NUMVAR]; 
  c[0] = 0.0; 
  c[1] = 0.0; 
  c[2] = 0.0; 	
  c[3] = 1.0;

  /* Assemble coefficients for linear equality or inequalities (Ax = b or Ax <= b) */ 
  /* Each constraint specified earlier on represents a row in the matrix A */
  /* The first row is the linear inequality constraint for the joint plane */
  /* The subsequent rows (faceNo) are the linear inequality constraints for the block faces */
  /* The final rows are the linear inequality constraints specifying the shape of the non-persistent joint */
 
  vector<double> aval; /* records non-zero values of entries */
  vector<int> aptrb; /* records entry number of first input */
  vector<int> aptre; /* records entry of last input */
  vector<int> asub;  /* records row number of entries */
  vector<int> columnCount; /* records number of entries in each column */

  /* The entries in matrix A are specified column by column */

  /* column 0 xA*/
  aptrb.push_back(0);
  int count = 0;
  if(fabs(joint.a) > pow(10,-12) ){ //Entries for linear equality of rock joint
    aval.push_back(joint.a);  asub.push_back(0); 
    count++;
  }
  for(int i=0; i < faceNo; i++){ //Entries for linear inequalities of block faces
    if(fabs(block.a[i]) > pow(10,-12) ){
      aval.push_back(block.a[i]);  asub.push_back(1+i);
      count++;
    }
  }
  for(int i=0; i < lineNo; i++){ //Entries for linear inequalities of lines delimiting the shape of rock joint
    if(fabs(joint.globalShape_a[i]) > pow(10,-12) ){
      aval.push_back(globalShape_a[i]);  asub.push_back(1+faceNo+i);
      count++;
    }
  }
  aptre.push_back(count);
  columnCount.push_back(count);

  /* column 1 yA*/
  aptrb.push_back(aptre[0]);
  count = 0;
  if(fabs(joint.b) > pow(10,-12) ){ //Entries for linear equality of rock joint
    aval.push_back(joint.b);  asub.push_back(0);
    count++;
  }
  for(int i=0; i < faceNo; i++){ //Entries for linear inequalities of block faces
    if(fabs(block.b[i]) > pow(10,-12) ){
      aval.push_back(block.b[i]);  asub.push_back(1+i);
      count++;
    }
  }

  for(int i=0; i < lineNo; i++){ //Entries for linear inequalities of lines delimiting the shape of rock joint
    if(fabs(joint.globalShape_b[i]) > pow(10,-12) ){
      aval.push_back(globalShape_b[i]);  asub.push_back(1+faceNo+i);
      count++;
    }
  }
  aptre.push_back(aptrb[1] + count);
  columnCount.push_back(count);

  /* column 2 zA*/
  aptrb.push_back(aptre[1]);
  count = 0;
  if(fabs(joint.c) > pow(10,-12) ){ //Entries for linear equality of rock joint
    aval.push_back(joint.c);  asub.push_back(0);
    count++;
  }
  for(int i=0; i < faceNo; i++){ //Entries for linear inequalities of block faces
    if(fabs(block.c[i]) > pow(10,-12) ){
      aval.push_back(block.c[i]);  asub.push_back(1+i);
      count++;
    }
  }
  for(int i=0; i < lineNo; i++){ //Entries for linear inequalities of lines delimiting the shape of rock joint
    if(fabs(joint.globalShape_c[i]) > pow(10,-12) ){
      aval.push_back(globalShape_c[i]);  asub.push_back(1+faceNo+i);
      count++;
    }
  }
  aptre.push_back(aptrb[2] + count);
  columnCount.push_back(count);

  /* column 3 s*/
  aptrb.push_back(aptre[2]); count = 0; 
  for(int i=0; i < faceNo+lineNo; i++){ 
    aval.push_back(-1.0);  asub.push_back(1 + i);
    count++;
  }
  aptre.push_back(aptrb[3]+faceNo+lineNo);
  columnCount.push_back(count);

  double        xx[NUMVAR]; xx[0] = 0.0; xx[1] = 0.0; xx[2]=0.0;  xx[3]=0.0;  
  CPXENVptr     env = NULL;
  CPXENVptr     &env=CPLEXenv;
  CPXLPptr      lp = NULL;
  int           status; int lpstat;
  double        objval;
   
  status = 0;
  env = CPXopenCPLEX (&status);
  /* If an error occurs, the status value indicates the reason for failure.  A call to CPXgeterrorstring will produce the text of the error message.  Note that CPXopenCPLEX produces no output, so the only way to see the cause of the error is to use CPXgeterrorstring.  For other CPLEX routines, the errors will be seen if the CPX_PARAM_SCRIND indicator is set to CPX_ON.  */

  if ( env == NULL ) {
    char  errmsg[1024];
    fprintf (stderr, "Could not open CPLEX environment.\n");
    CPXgeterrorstring (env, status, errmsg);
    fprintf (stderr, "%s", errmsg);
    goto TERMINATE;
  }

  /* Turn on output to the screen */
  status = CPXsetintparam (env, CPX_PARAM_THREADS, 1); //std::cout<<"status threads: "<<status<<endl;
  status = CPXsetintparam (env, CPX_PARAM_PREIND, 0);
  status = CPXsetintparam (env, CPX_PARAM_AUXROOTTHREADS, -1);
  status = CPXsetintparam (env, CPX_PARAM_LPMETHOD, CPX_ALG_DUAL);
  status = CPXsetintparam (env, CPX_PARAM_SCRIND, CPX_OFF);
  status = CPXsetintparam (env, CPX_PARAM_ADVIND, CPX_ON);
  if ( status ) {
    fprintf (stderr, 
	     "Failure to turn on screen indicator, error %d.\n", status);
    goto TERMINATE;
  }

  status = CPXsetintparam (env, CPX_PARAM_SIMDISPLAY, 0);
  if ( status ) {
    fprintf (stderr,"Failed to turn up simplex display level.\n");
    goto TERMINATE;
  }

  /* Create the problem */
  lp = CPXcreateprob (env, &status, "chvatal");
  if ( lp == NULL ) {
    fprintf (stderr,"Failed to create subproblem\n");
    status = 1;
    goto TERMINATE;
  }

  /* Copy network part of problem.  */
  status = CPXcopylp (env, lp, NUMVAR, NUMCON, CPX_MIN, c, buc, bkc, &aptrb[0], &columnCount[0], &asub[0], &aval[0], blx, bux, NULL);

  if ( status ) {
    fprintf (stderr, "CPXcopylp failed.\n");
    goto TERMINATE;
  }

  // status = CPXsetintparam (env, CPX_PARAM_LPMETHOD, 1);
  if ( status ) {
    fprintf (stderr, 
	     "Failed to set the optimization method, error %d.\n", status);
    goto TERMINATE;
  }


  status = CPXlpopt (env, lp);
  if ( status ) {
    fprintf (stderr, "Failed to optimize LP.\n");
    goto TERMINATE;
  }

  status = CPXsolution (env, lp, &lpstat, &objval, xx, NULL, NULL, NULL);
  if ( status ) {
    fprintf (stderr,"CPXsolution failed.\n");
    goto TERMINATE;
  }

  //  printf ("Solution status %d\n",lpstat);
  //  printf ("Objective value %g\n",objval);
  //  printf ("Solution is:\n");
  //  for (j = 0; j < NUMVAR; j++) {
  //     printf ("x[%d] = %g\n",j,xx[j]);
  //  }
	
#if 0
  /* Put the problem and basis into a SAV file to use it in the
   * Interactive Optimizer and see if problem is correct */

  status = CPXwriteprob (env, lp, "lpex3.sav", NULL);
  if ( status ) {
    fprintf (stderr, "CPXwriteprob failed.\n");
    goto TERMINATE;
  }
#endif

  s = xx[3];

 TERMINATE:
  /* Free up the problem as allocated by CPXcreateprob, if necessary */

  if ( lp != NULL ) {
    status = CPXfreeprob (env, &lp); lp=NULL;
    if ( status ) {
      fprintf (stderr, "CPXfreeprob failed, error code %d.\n", status);
    }
  }

  /* Free up the CPLEX environment, if necessary */

  if ( env != NULL ) {
    status = CPXcloseCPLEX (&env); env=NULL;

    /* Note that CPXcloseCPLEX produces no output, so the only way to see the cause of the error is to use CPXgeterrorstring.  For other CPLEX routines, the errors will be seen if the CPX_PARAM_SCRIND indicator is set to CPX_ON. */

    if ( status ) {
      char  errmsg[1024];
      fprintf (stderr, "Could not close CPLEX environment.\n");
      CPXgeterrorstring (env, status, errmsg);
      fprintf (stderr, "%s", errmsg);
    }
  }

 
  aval.clear();
  asub.clear();
  aptrb.clear();
  aptre.clear();
  columnCount.clear();
  if(s>-pow(10,-9) || status == 1){ //where pow(10,-9) is the tolerance close to zero
    return false; /* no intersection */
  }else{
    return true; /* there is intersection */
  }
  /* a different test is required for Eq. (8) to check for geometrically redundant planes (see manuscript) */
}


