#include "Rcpp.h"
#include <string>


// [[Rcpp::export]]
Rcpp::List partial_make_data_table(int nbrows, int nbcols) {

  auto df    = Rcpp::List(nbcols);
  auto names = Rcpp::StringVector(nbcols);
  
  // create the columns of the data.table and asign them some data:
  for (uint64_t i=0; i<nbcols; ++i) {
    names[i] = "col_" + std::to_string(i);
    auto col = Rcpp::NumericVector(nbrows);

    for (uint64_t j=0; j<nbrows; ++j) {
      col[j] = i + j;
    }
    df[i] = col;
  }

  // assign names and class:
  df.names() = names;
  df.attr("class") = Rcpp::StringVector::create("data.table", "data.frame");
  
  return df;
}
