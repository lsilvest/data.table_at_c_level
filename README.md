# Constructing a _data.table_ at C level without a copy

This code shows (tentatively) how to create a _data.table_ instance at
C level without a deep copy of the structure.

It is composed of C code that builds a "partial" _data.table_ instance,
and some R code that finalizes the creation.

The C function `partial_make_data_table` constructs a list of vectors
at C level and assigns columns names and the classes `c("data.table",
"data.frame")`. This object is then returned to R.

An additonal function `make.data.table` is defined in which a call is
made on the returned list with `setalloccol` which is a function
exported by the _data.table_ package. This is inspired by the
_data.table_ function `as.data.table.data.frame`.

Here is for reference the _data.table_`as.data.table.data.frame`
function:

``` R
as.data.table.data.frame = function(x, keep.rownames=FALSE, key=NULL, ...) {
  if (!identical(keep.rownames, FALSE)) {
    # can specify col name to keep.rownames, #575
    ans = data.table(rn=rownames(x), x, keep.rownames=FALSE, key=key)
    if (is.character(keep.rownames))
      setnames(ans, 'rn', keep.rownames[1L])
    return(ans)
  }
  if (any(vapply_1i(x, function(xi) length(dim(xi))))) { # not is.atomic because is.atomic(matrix) is true
    # a data.frame with a column that is data.frame needs to be expanded; test 2013.4
    return(as.data.table.list(x, keep.rownames=keep.rownames, ...))
  }
  ans = copy(x)  # TO DO: change this deep copy to be shallow.
  setattr(ans, "row.names", .set_row_names(nrow(x)))

  ## NOTE: This test (#527) is no longer in effect ##
  # for nlme::groupedData which has class c("nfnGroupedData","nfGroupedData","groupedData","data.frame")
  # See test 527.
  ##

  # fix for #1078 and #1128, see .resetclass() for explanation.
  setattr(ans, "class", .resetclass(x, "data.frame"))
  setalloccol(ans)
  setkeyv(ans, key)
  ans
}
```

The main effect is the call to `setalloccol` which calls the C level
`alloccol`. The latter's main effect is the setting of the
_data.table_ self reference; here is the relevant code from "assign.c":

``` C
static int _selfrefok(SEXP x, Rboolean checkNames, Rboolean verbose) {
  SEXP v, p, tag, prot, names;
  v = getAttrib(x, SelfRefSymbol);
  if (v==R_NilValue || TYPEOF(v)!=EXTPTRSXP) {
    // .internal.selfref missing is expected and normal for i) a pre v1.7.8 data.table loaded
    //  from disk, and ii) every time a new data.table is over-allocated for the first time.
    //  Not being an extptr is for when users contruct a data.table via structure() using dput, post
    //  a question, and find the extptr doesn't parse so put quotes around it (for example).
    //  In both cases the selfref is not ok.
    return 0;
  }
  p = R_ExternalPtrAddr(v);
  if (p==NULL) {
    if (verbose) Rprintf(".internal.selfref ptr is NULL. This is expected and normal for a data.table loaded from disk. Please remember to always setDT() immediately after loading to prevent unexpected behavior. If this table was not loaded from disk or you've already run setDT(), please report to data.table issue tracker.\n");
    return -1;
  }
  if (!isNull(p)) error("Internal error: .internal.selfref ptr is not NULL or R_NilValue"); // # nocov
  tag = R_ExternalPtrTag(v);
  if (!(isNull(tag) || isString(tag))) error("Internal error: .internal.selfref tag isn't NULL or a character vector"); // # nocov
  names = getAttrib(x, R_NamesSymbol);
  if (names != tag && isString(names))
    SET_TRUELENGTH(names, LENGTH(names));
    // R copied this vector not data.table; it's not actually over-allocated. It looks over-allocated
    // because R copies the original vector's tl over despite allocating length.
  prot = R_ExternalPtrProtected(v);
  if (TYPEOF(prot) != EXTPTRSXP)   // Very rare. Was error(".internal.selfref prot is not itself an extptr").
    return 0;                      // # nocov ; see http://stackoverflow.com/questions/15342227/getting-a-random-internal-selfref-error-in-data-table-for-r
  if (x != R_ExternalPtrAddr(prot))
    SET_TRUELENGTH(x, LENGTH(x));  // R copied this vector not data.table, it's not actually over-allocated
  return checkNames ? names==tag : x==R_ExternalPtrAddr(prot);
}

static Rboolean selfrefok(SEXP x, Rboolean verbose) {   // for readability
  return(_selfrefok(x, FALSE, verbose)==1);
}
static Rboolean selfrefnamesok(SEXP x, Rboolean verbose) {
  return(_selfrefok(x, TRUE, verbose)==1);
}

static SEXP shallow(SEXP dt, SEXP cols, R_len_t n)
{
  // NEW: cols argument to specify the columns to shallow copy on. If NULL, all columns.
  // called from alloccol where n is checked carefully, or from shallow() at R level
  // where n is set to truelength (i.e. a shallow copy only with no size change)
  R_len_t i,l;
  int protecti=0;
  SEXP newdt = PROTECT(allocVector(VECSXP, n)); protecti++;   // to do, use growVector here?
  //copyMostAttrib(dt, newdt);   // including class
  DUPLICATE_ATTRIB(newdt, dt);
  // TO DO: keepattr() would be faster, but can't because shallow isn't merely a shallow copy. It
  //        also increases truelength. Perhaps make that distinction, then, and split out, but marked
  //        so that the next change knows to duplicate.
  //        Does copyMostAttrib duplicate each attrib or does it point? It seems to point, hence DUPLICATE_ATTRIB
  //        for now otherwise example(merge.data.table) fails (since attr(d4,"sorted") gets written by setnames).
  SEXP names = PROTECT(getAttrib(dt, R_NamesSymbol)); protecti++;
  SEXP newnames = PROTECT(allocVector(STRSXP, n)); protecti++;
  if (isNull(cols)) {
    l = LENGTH(dt);
    for (i=0; i<l; i++) SET_VECTOR_ELT(newdt, i, VECTOR_ELT(dt,i));
    if (length(names)) {
      if (length(names) < l) error("Internal error: length(names)>0 but <length(dt)"); // # nocov
      for (i=0; i<l; i++) SET_STRING_ELT(newnames, i, STRING_ELT(names,i));
    }
    // else an unnamed data.table is valid e.g. unname(DT) done by ggplot2, and .SD may have its names cleared in dogroups, but shallow will always create names for data.table(NULL) which has 100 slots all empty so you can add to an empty data.table by reference ok.
  } else {
    l = length(cols);
    for (i=0; i<l; i++) SET_VECTOR_ELT(newdt, i, VECTOR_ELT(dt,INTEGER(cols)[i]-1));
    if (length(names)) {
      // no need to check length(names) < l here. R-level checks if all value
      // in 'cols' are valid - in the range of 1:length(names(x))
      for (i=0; i<l; i++) SET_STRING_ELT( newnames, i, STRING_ELT(names,INTEGER(cols)[i]-1) );
    }
  }
  setAttrib(newdt, R_NamesSymbol, newnames);
  // setAttrib appears to change length and truelength, so need to do that first _then_ SET next,
  // otherwise (if the SET were were first) the 100 tl is assigned to length.
  SETLENGTH(newnames,l);
  SET_TRUELENGTH(newnames,n);
  SETLENGTH(newdt,l);
  SET_TRUELENGTH(newdt,n);
  setselfref(newdt);
  UNPROTECT(protecti);
  return(newdt);
}

SEXP alloccol(SEXP dt, R_len_t n, Rboolean verbose)
{
  SEXP names, klass;   // klass not class at request of pydatatable because class is reserved word in C++, PR #3129
  R_len_t l, tl;
  if (isNull(dt)) error("alloccol has been passed a NULL dt");
  if (TYPEOF(dt) != VECSXP) error("dt passed to alloccol isn't type VECSXP");
  klass = getAttrib(dt, R_ClassSymbol);
  if (isNull(klass)) error("dt passed to alloccol has no class attribute. Please report result of traceback() to data.table issue tracker.");
  l = LENGTH(dt);
  names = getAttrib(dt,R_NamesSymbol);
  // names may be NULL when null.data.table() passes list() to alloccol for example.
  // So, careful to use length() on names, not LENGTH().
  if (length(names)!=l) error("Internal error: length of names (%d) is not length of dt (%d)",length(names),l); // # nocov
  if (!selfrefok(dt,verbose))
    return shallow(dt,R_NilValue,(n>l) ? n : l);  // e.g. test 848 and 851 in R > 3.0.2
    // added (n>l) ? ... for #970, see test 1481.
  // TO DO:  test realloc names if selfrefnamesok (users can setattr(x,"name") themselves for example.
  // if (TRUELENGTH(getAttrib(dt,R_NamesSymbol))!=tl)
  //    error("Internal error: tl of dt passes checks, but tl of names (%d) != tl of dt (%d)", tl, TRUELENGTH(getAttrib(dt,R_NamesSymbol))); // # nocov

  tl = TRUELENGTH(dt);
  // R <= 2.13.2 and we didn't catch uninitialized tl somehow
  if (tl<0) error("Internal error, tl of class is marked but tl<0."); // # nocov
  if (tl>0 && tl<l) error("Internal error, please report (including result of sessionInfo()) to data.table issue tracker: tl (%d) < l (%d) but tl of class is marked.", tl, l); // # nocov
  if (tl>l+10000) warning("tl (%d) is greater than 10,000 items over-allocated (l = %d). If you didn't set the datatable.alloccol option to be very large, please report to data.table issue tracker including the result of sessionInfo().",tl,l);
  if (n>tl) return(shallow(dt,R_NilValue,n)); // usual case (increasing alloc)
  if (n<tl && verbose) Rprintf("Attempt to reduce allocation from %d to %d ignored. Can only increase allocation via shallow copy. Please do not use DT[...]<- or DT$someCol<-. Use := inside DT[...] instead.",tl,n);
        // otherwise the finalizer can't clear up the Large Vector heap
  return(dt);
}
```

So it seems that the two functions in "dtcreate.cpp" and "dtcreate.R"
are all that is needed to create a _data.table_ at C level without the
need for a deep copy.
