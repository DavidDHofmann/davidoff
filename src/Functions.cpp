////////////////////////////////////////////////////////////////////////////////
//// Helper Functions to Simulate Dispersal
////////////////////////////////////////////////////////////////////////////////
#include "Rcpp.h"
#include "math.h"
using namespace Rcpp;
//' Calculate New Absolute Turning Angle
//'
//' Function to calculate new absolute turning angle
//' @export
//' @param absta Absolute turning angle
//' @param ta Relative turning angle
//' @return numeric value
// [[Rcpp::export]]
NumericVector getAbsNewC(double absta, NumericVector ta){

  // Prepare loop counter based on vector length
  int n = ta.length();

  // Calculate new absolute turning angle
  NumericVector absta_new(n);
  for (int i = 0; i < n; i++){
    absta_new[i] = absta + ta[i];

    // We need to make sure the turning angle is between 0 and 2 Pi
    if (absta_new[i] > 2 * M_PI){
      absta_new[i] = absta_new[i] - 2 * M_PI;
    } else if (absta_new[i] < 0){
      absta_new[i] = absta_new[i] + 2 * M_PI;
    }
  }

  // Return it
  return absta_new;
}

//' Calculate New Endpoints
//'
//' Function to calculate new endpoints
//' @export
//' @param xy \code{matrix} containing xy coordinates
//' @param absta absolute turning angle
//' @param sl step lengths
//' @return numeric value
// [[Rcpp::export]]
NumericMatrix calcEndpointsC(
    NumericMatrix xy    // Coordinates of start point
  , NumericVector absta // Absolute turning angle
  , NumericVector sl    // Step length
){

  // Prepare loop counter based on vector length
  int n = sl.length();

  // Make sure the step length covers at least a meter and convert to degrees
  NumericVector sl_new(n);
  for (int i = 0; i < n; i++){
    if (sl[i] < 1){
      sl_new[i] = 1 / 111000;
    } else {
      sl_new[i] = sl[i] / 111000;
    }
  }

  // Prepare matrix into which the new coordinates will go
  NumericMatrix xy_new(n, 2);
  for (int i = 0; i < n; i++){
    xy_new(i, 0) = xy(0, 0) + sin(absta[i]) * sl_new[i];
    xy_new(i, 1) = xy(0, 1) + cos(absta[i]) * sl_new[i];
  }

  // Return the results
  return xy_new;
}
