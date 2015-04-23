/* Open and read input file, producing list of linear inequalities specifying region and
 * list of linear equalities specifying discontinuities
 */

// Construct a bounding box for region and introduce articifial "processor joints"
// Generate a list of initial blocks and change it into an RDD
// Eliminate any blocks that are contained in the bounding box but not in the actual rock mass

/*
 * Iterate through the discontinuities, cutting blocks where appropriate and producing
 * a new list of blocks at each step
 */
// Merge together blocks that are separated only by a processor joint
// Remove redundant joints as described in the original paper

// Convert the list of rock blocks to JSON and save this to an output file
