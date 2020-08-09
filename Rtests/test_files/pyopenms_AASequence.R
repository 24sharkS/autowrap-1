library(reticulate)
library(R6)
library(purrr)
listDepth <- plotrix::listDepth
check.numeric <- varhandle::check.numeric
Pymod <- reticulate::import("pyopenms")
reticulate::py_run_string("import gc")
copy <- reticulate::import("copy")
py_builtin <- reticulate::import_builtins(convert = F)

# R6 class object conversion to python class object.
`r_to_py.R6` <- function(i,...){
   tryCatch({
       i$.__enclos_env__$private$py_obj
   }, error = function(e) { "conversion not supported for this class"}
   )
}

# python function to convert a python dict having byte type key to R named list with names as string.
py_run_string(paste("def transform_dict(d):","    return dict(zip([k.decode('utf-8') for k in d.keys()], d.values()))",sep = "\n"))

# Returns the name of wrapper R6 class
class_to_wrap <- function(py_ob){
       strsplit(class(py_ob)[1],"\\.")[[1]][2]
} 
    
# R implementation of _AASequence

    # Representation of a peptide/protein sequence
    # This class represents amino acid sequences in OpenMS. An AASequence
    # instance primarily contains a sequence of residues.
AASequence <- R6Class(classname = "AASequence",cloneable = FALSE,

    private = list(py_obj = NA),

    
    public = list(
    
    # C++ signature: void AASequence()
    initialize = function(...){
        par <- list(...)
    if (!length(par) %in% c(0,1)) { stop("arg wrong type")}
    if (length(par)==1) {
        if ("python.builtin.object" %in% class(par[[1]]) && class_to_wrap(par[[1]]) == AASequence) { private$py_obj <- par[[1]] }
        else { stop("arg wrong type") }
    } else if (length(par)==0) {
    
        private$py_obj <- Pymod$AASequence()
    }
    
    },
    
    # C++ signature: bool empty()
    empty = function(){
    
        py_ans = private$py_obj$empty()
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: double getAverageWeight()
    getAverageWeight = function(){
    
        py_ans = private$py_obj$getAverageWeight()
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: double getMonoWeight()
    getMonoWeight = function(){
    
        py_ans = private$py_obj$getMonoWeight()
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: bool hasSubsequence(AASequence peptide)
    hasSubsequence = function(peptide){
    
        if(!(is.R6(peptide) && class(peptide)[1] == "AASequence")){ stop("arg peptide wrong type") }
    
        py_ans = private$py_obj$hasSubsequence(peptide)
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: bool hasPrefix(AASequence peptide)
    hasPrefix = function(peptide){
    
        if(!(is.R6(peptide) && class(peptide)[1] == "AASequence")){ stop("arg peptide wrong type") }
    
        py_ans = private$py_obj$hasPrefix(peptide)
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: bool hasSuffix(AASequence peptide)
    hasSuffix = function(peptide){
    
        if(!(is.R6(peptide) && class(peptide)[1] == "AASequence")){ stop("arg peptide wrong type") }
    
        py_ans = private$py_obj$hasSuffix(peptide)
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: bool hasNTerminalModification()
    hasNTerminalModification = function(){
    
        py_ans = private$py_obj$hasNTerminalModification()
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: bool hasCTerminalModification()
    hasCTerminalModification = function(){
    
        py_ans = private$py_obj$hasCTerminalModification()
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: bool isModified()
    isModified = function(){
    
        py_ans = private$py_obj$isModified()
        r_ans = py_ans
        return(r_ans)
    }
    
        )
)
    #' @export
    `+.AASequence` <- function(e1, e2){
       added <- e1$.__enclos_env__$private$py_obj + e2$.__enclos_env__$private$py_obj
       result <- AASequence$new(added)
       return(result)
    }
    
    # C++ signature: AASequence fromString(bool s)
    AASequence$fromString = function(s){
    
        if(!( (is_scalar_integer(s) || is_scalar_double(s)) && s == as.integer(s))){ stop("arg s wrong type") }
    
        py_ans = Pymod$AASequence$fromString(as.integer(s))
        r_ans = AASequence$new(py_ans)
        return(r_ans)
    }
    
    # C++ signature: AASequence fromStringPermissive(bool s, bool permissive)
    AASequence$fromStringPermissive = function(s, permissive){
    
        if(!( (is_scalar_integer(s) || is_scalar_double(s)) && s == as.integer(s))){ stop("arg s wrong type") }
        if(!( (is_scalar_integer(permissive) || is_scalar_double(permissive)) && permissive == as.integer(permissive))){ stop("arg permissive wrong type") }
    
    
        py_ans = Pymod$AASequence$fromStringPermissive(as.integer(s), as.integer(permissive))
        r_ans = AASequence$new(py_ans)
        return(r_ans)
    } 
