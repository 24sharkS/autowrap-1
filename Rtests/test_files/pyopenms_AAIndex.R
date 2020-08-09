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
    
# R implementation of _AAIndex
AAIndex <- R6Class(classname = "AAIndex",cloneable = FALSE,

    private = list(py_obj = NA),

    
    public = list(
    
    # C++ signature: double aliphatic(char aa)
    aliphatic = function(aa){
    
        if(!(is_scalar_character(aa))){ stop("arg aa wrong type") }
    py_run_string("aa = bytes(aa)")
        py_ans = private$py_obj$aliphatic(py$aa)
        py_run_string("del aa")
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: double acidic(char aa)
    acidic = function(aa){
    
        if(!(is_scalar_character(aa))){ stop("arg aa wrong type") }
    py_run_string("aa = bytes(aa)")
        py_ans = private$py_obj$acidic(py$aa)
        py_run_string("del aa")
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: double basic(char aa)
    basic = function(aa){
    
        if(!(is_scalar_character(aa))){ stop("arg aa wrong type") }
    py_run_string("aa = bytes(aa)")
        py_ans = private$py_obj$basic(py$aa)
        py_run_string("del aa")
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: double polar(char aa)
    polar = function(aa){
    
        if(!(is_scalar_character(aa))){ stop("arg aa wrong type") }
    py_run_string("aa = bytes(aa)")
        py_ans = private$py_obj$polar(py$aa)
        py_run_string("del aa")
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: double getKHAG800101(char aa)
    getKHAG800101 = function(aa){
    
        if(!(is_scalar_character(aa))){ stop("arg aa wrong type") }
    py_run_string("aa = bytes(aa)")
        py_ans = private$py_obj$getKHAG800101(py$aa)
        py_run_string("del aa")
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: double getVASM830103(char aa)
    getVASM830103 = function(aa){
    
        if(!(is_scalar_character(aa))){ stop("arg aa wrong type") }
    py_run_string("aa = bytes(aa)")
        py_ans = private$py_obj$getVASM830103(py$aa)
        py_run_string("del aa")
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: double getNADH010106(char aa)
    getNADH010106 = function(aa){
    
        if(!(is_scalar_character(aa))){ stop("arg aa wrong type") }
    py_run_string("aa = bytes(aa)")
        py_ans = private$py_obj$getNADH010106(py$aa)
        py_run_string("del aa")
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: double getNADH010107(char aa)
    getNADH010107 = function(aa){
    
        if(!(is_scalar_character(aa))){ stop("arg aa wrong type") }
    py_run_string("aa = bytes(aa)")
        py_ans = private$py_obj$getNADH010107(py$aa)
        py_run_string("del aa")
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: double getWILM950102(char aa)
    getWILM950102 = function(aa){
    
        if(!(is_scalar_character(aa))){ stop("arg aa wrong type") }
    py_run_string("aa = bytes(aa)")
        py_ans = private$py_obj$getWILM950102(py$aa)
        py_run_string("del aa")
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: double getROBB760107(char aa)
    getROBB760107 = function(aa){
    
        if(!(is_scalar_character(aa))){ stop("arg aa wrong type") }
    py_run_string("aa = bytes(aa)")
        py_ans = private$py_obj$getROBB760107(py$aa)
        py_run_string("del aa")
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: double getOOBM850104(char aa)
    getOOBM850104 = function(aa){
    
        if(!(is_scalar_character(aa))){ stop("arg aa wrong type") }
    py_run_string("aa = bytes(aa)")
        py_ans = private$py_obj$getOOBM850104(py$aa)
        py_run_string("del aa")
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: double getFAUJ880111(char aa)
    getFAUJ880111 = function(aa){
    
        if(!(is_scalar_character(aa))){ stop("arg aa wrong type") }
    py_run_string("aa = bytes(aa)")
        py_ans = private$py_obj$getFAUJ880111(py$aa)
        py_run_string("del aa")
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: double getFINA770101(char aa)
    getFINA770101 = function(aa){
    
        if(!(is_scalar_character(aa))){ stop("arg aa wrong type") }
    py_run_string("aa = bytes(aa)")
        py_ans = private$py_obj$getFINA770101(py$aa)
        py_run_string("del aa")
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: double getARGP820102(char aa)
    getARGP820102 = function(aa){
    
        if(!(is_scalar_character(aa))){ stop("arg aa wrong type") }
    py_run_string("aa = bytes(aa)")
        py_ans = private$py_obj$getARGP820102(py$aa)
        py_run_string("del aa")
        r_ans = py_ans
        return(r_ans)
    }
    
        )
) 
