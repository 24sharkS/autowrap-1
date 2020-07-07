library(reticulate)
library(purrr)
library(R6)
Pymod <- import("py_libcpp_test")
py_run_string("import gc") 
    
# R implementation of _ABS_Impl1
R6Class(classname = "ABS_Impl1",cloneable = FALSE,

    private = list(py_obj = NA),

    
    public = list(
    
    
    # C++ signature: void ABS_Impl1()
    init_0 = function(){
    
        stop("Cannot call this constructor!")
    
    },
    
    # C++ signature: void ABS_Impl1(int i)
    init_1 = function(i){
    
        if(!(is_scalar_integer(i))){ stop(arg i wrong type) }
    
    
        private$py_obj <- Pymod$ABS_Impl1(i)
        return(self)
    
    
    },
    
          # C++ signature: void ABS_Impl1()
          # C++ signature: void ABS_Impl1(int i)
    initialize = function(...){
        arg_list = list(...)
        if (length(arg_list)==0) { self$init_0(...) }
        else if ((length(arg_list)==1) && (is_scalar_integer(arg_list[[1]]))) { self$init_1(...) }
        else{
               stop('wrong arguments provided')
          }
    },
    
    
    # C++ signature: int get()
    get = function(){
    
        py_ans = private$py_obj$get()
        r_ans = py_ans
        return(r_ans)
    }
    
        )
) 
    
# R implementation of _ABS_Impl2
R6Class(classname = "ABS_Impl2",cloneable = FALSE,

    private = list(py_obj = NA),

    
    public = list(
    
    
    # C++ signature: void ABS_Impl2()
    init_0 = function(){
    
        stop("Cannot call this constructor!")
    
    },
    
    # C++ signature: void ABS_Impl2(int i)
    init_1 = function(i){
    
        if(!(is_scalar_integer(i))){ stop(arg i wrong type) }
    
    
        private$py_obj <- Pymod$ABS_Impl2(i)
        return(self)
    
    
    },
    
          # C++ signature: void ABS_Impl2()
          # C++ signature: void ABS_Impl2(int i)
    initialize = function(...){
        arg_list = list(...)
        if (length(arg_list)==0) { self$init_0(...) }
        else if ((length(arg_list)==1) && (is_scalar_integer(arg_list[[1]]))) { self$init_1(...) }
        else{
               stop('wrong arguments provided')
          }
    },
    
    
    # C++ signature: int get()
    get = function(){
    
        py_ans = private$py_obj$get()
        r_ans = py_ans
        return(r_ans)
    }
    
        )
) 
    
# R implementation of _Int
R6Class(classname = "Int",cloneable = FALSE,

    private = list(py_obj = NA),


    active = list(
        i_ = function(i_){
            if(!missing(i_)){
    
        
            private$py_obj$i_ <- i_
            }
            else {
        
            py_ans = private@py_obj@i_
                r_result = py_ans
                return(r_result)
                }
    
        }

    ),
    
    public = list(
    
    
    # C++ signature: void Int(int i)
    init_0 = function(i){
    
        if(!(is_scalar_integer(i))){ stop(arg i wrong type) }
    
    
        private$py_obj <- Pymod$Int(i)
        return(self)
    
    
    },
    
    # C++ signature: void Int(Int & i)
    init_1 = function(i){
    
        if(!(all(class(i) == c("R6","Int")))){ stop(arg i wrong type) }
    
    
        private$py_obj <- Pymod$Int(i$get_py_object())
        return(self)
    
    
    },
    
          # C++ signature: void Int(int i)
          # C++ signature: void Int(Int & i)
    initialize = function(...){
        arg_list = list(...)
        if ((length(arg_list)==1) && (is_scalar_integer(arg_list[[1]]))) { self$init_0(...) }
        else if ((length(arg_list)==1) && (all(class(arg_list[[1]]) == c("R6","Int")))) { self$init_1(...) }
        else{
               stop('wrong arguments provided')
          }
    },
    
) 
    
# R implementation of _LibCppTest

    # This is some class doc
    # Pretty cool stuff!
    # -----
    # With a trick, we can even get multiple paragraphs, allowing us to
    # write much longer documentation.
R6Class(classname = "LibCppTest",cloneable = FALSE,

    private = list(py_obj = NA),

    
    public = list(
    
    
    # C++ signature: void LibCppTest()
    init_0 = function(){
    
    
        private$py_obj <- Pymod$LibCppTest()
        return(self)
    
    
    },
    
    # C++ signature: void LibCppTest(int ii)
    init_1 = function(ii){
    
        if(!(is_scalar_integer(ii))){ stop(arg ii wrong type) }
    
    
        private$py_obj <- Pymod$LibCppTest(ii)
        return(self)
    
    
    },
    
          # C++ signature: void LibCppTest()
          # C++ signature: void LibCppTest(int ii)
    initialize = function(...){
        arg_list = list(...)
        if (length(arg_list)==0) { self$init_0(...) }
        else if ((length(arg_list)==1) && (is_scalar_integer(arg_list[[1]]))) { self$init_1(...) }
        else{
               stop('wrong arguments provided')
          }
    },
    
    
    # C++ signature: int gett()
# getting access to an integer
    gett = function(){
    
        py_ans = private$py_obj$get()
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: libcpp_pair[int,libcpp_string] twist(libcpp_pair[libcpp_string,int])
# Dont forget this stuff here!
    twist = function(in_0){
    
        if(!(is_list(in_0) && length(in_0) == 2 && is_scalar_character(in_0[[1]]) && is_scalar_integer(in_0[[2]]))){ stop(arg in_0 wrong type) }
        v0 = r_to_py(list(in_0[[1]],in_0[[2]]))
        py_ans = private$py_obj$twist(v0)
        r_ans = list(py_to_r(py_ans[[1]]), py_to_r(py_ans[[2]]))
        return(r_ans)
    },
    
    # C++ signature: libcpp_vector[int] process(libcpp_vector[int] &)
    process = function(in_0){
    
        if(!(is_list(in_0) && all(sapply(in_0,function(elemt_rec) is_scalar_integer(elemt_rec))))){ stop(arg in_0 wrong type) }
        cdef libcpp_vector[int] v0 = in_0
        _r = private$py_obj$process(v0)
        in_0[:] = v0
        cdef list r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: libcpp_pair[int,int] process2(libcpp_pair[int,int] &)
    process2 = function(in_0){
    
        if(!(is_list(in_0) && length(in_0) == 2 && is_scalar_integer(in_0[[1]]) && is_scalar_integer(in_0[[2]]))){ stop(arg in_0 wrong type) }
        v0 = r_to_py(list(in_0[[1]],in_0[[2]]))
        py_ans = private$py_obj$process2(v0)
        byref_0 = list(py_to_r(v0[0]), py_to_r(v0[1]))
        r_ans = list(py_to_r(py_ans[[1]]), py_to_r(py_ans[[2]]))
    
        tryCatch({
        eval.parent(substitute(in_0<-byref_0))
        return(r_ans)
        }, error = function(c) {return(r_ans)}
        )
    
    },
    
    # C++ signature: libcpp_pair[LibCppTest,int] process3(libcpp_pair[LibCppTest,int] &)
    process3 = function(in_0){
    
        if(!(is_list(in_0) && length(in_0) == 2 && all(class(in_0[[1]]) == c("R6","LibCppTest")) && is_scalar_integer(in_0[[2]]))){ stop(arg in_0 wrong type) }
        v0 = r_to_py(list(in_0[[1]]$get_py_object(),in_0[[2]]))
        py_ans = private$py_obj$process3(v0)
        temp1 = LibCppTest$new()$set_py_object(v0[0])
        byref_0 = list(temp1, py_to_r(v0[1]))
        out1 = LibCppTest$new()$set_py_object(py_ans[[1]])
        r_ans = list(out1, py_to_r(py_ans[[2]]))
    
        tryCatch({
        eval.parent(substitute(in_0<-byref_0))
        return(r_ans)
        }, error = function(c) {return(r_ans)}
        )
    
    },
    
    # C++ signature: libcpp_pair[int,LibCppTest] process4(libcpp_pair[int,LibCppTest] &)
    process4 = function(in_0){
    
        if(!(is_list(in_0) && length(in_0) == 2 && is_scalar_integer(in_0[[1]]) && all(class(in_0[[2]]) == c("R6","LibCppTest")))){ stop(arg in_0 wrong type) }
        v0 = r_to_py(list(in_0[[1]],in_0[[2]]$get_py_object()))
        py_ans = private$py_obj$process4(v0)
        temp2 = LibCppTest$new()$set_py_object(v0[1])
        byref_0 = list(py_to_r(v0[0]), temp2)
        out2 = LibCppTest$new()$set_py_object(py_ans[[2]])
        r_ans = list(py_to_r(py_ans[[1]]), out2)
    
        tryCatch({
        eval.parent(substitute(in_0<-byref_0))
        return(r_ans)
        }, error = function(c) {return(r_ans)}
        )
    
    },
    
    # C++ signature: libcpp_pair[LibCppTest,LibCppTest] process5(libcpp_pair[LibCppTest,LibCppTest] &)
    process5 = function(in_0){
    
        if(!(is_list(in_0) && length(in_0) == 2 && all(class(in_0[[1]]) == c("R6","LibCppTest")) && all(class(in_0[[2]]) == c("R6","LibCppTest")))){ stop(arg in_0 wrong type) }
        v0 = r_to_py(list(in_0[[1]]$get_py_object(),in_0[[2]]$get_py_object()))
        py_ans = private$py_obj$process5(v0)
        temp1 = LibCppTest$new()$set_py_object(v0[0])
        temp2 = LibCppTest$new()$set_py_object(v0[1])
        byref_0 = list(temp1, temp2)
        out1 = LibCppTest$new()$set_py_object(py_ans[[1]])
        out2 = LibCppTest$new()$set_py_object(py_ans[[2]])
        r_ans = list(out1, out2)
    
        tryCatch({
        eval.parent(substitute(in_0<-byref_0))
        return(r_ans)
        }, error = function(c) {return(r_ans)}
        )
    
    },
    
    # C++ signature: libcpp_pair[int,EEE] process7(libcpp_pair[EEE,int] &)
    process7 = function(in_0){
    
        if(!(is_list(in_0) && length(in_0) == 2 && in_0[[1]] %in% c(0, 1) && is_scalar_integer(in_0[[2]]))){ stop(arg in_0 wrong type) }
        v0 = r_to_py(list(in_0[[1]],in_0[[2]]))
        py_ans = private$py_obj$process7(v0)
        byref_0 = list(py_to_r(v0[0]), py_to_r(v0[1]))
        out2 = py_ans[[2]]
        r_ans = list(py_to_r(py_ans[[1]]), out2)
    
        tryCatch({
        eval.parent(substitute(in_0<-byref_0))
        return(r_ans)
        }, error = function(c) {return(r_ans)}
        )
    
    },
    
    # C++ signature: void process24(libcpp_pair[int,float] & in_, libcpp_pair[int,int] & arg2)
    process24 = function(in_, arg2){
    
        if(!(is_list(in_) && length(in_) == 2 && is_scalar_integer(in_[[1]]) && is_scalar_double(in_[[2]]))){ stop(arg in_ wrong type) }
        if(!(is_list(arg2) && length(arg2) == 2 && is_scalar_integer(arg2[[1]]) && is_scalar_integer(arg2[[2]]))){ stop(arg arg2 wrong type) }
        v0 = r_to_py(list(in_[[1]],in_[[2]]))
        v1 = r_to_py(list(arg2[[1]],arg2[[2]]))
        private$py_obj$process24(v0, v1)
        byref_1 = list(py_to_r(v1[0]), py_to_r(v1[1]))
        byref_0 = list(py_to_r(v0[0]), py_to_r(v0[1]))
    
        tryCatch({
        eval.parent(substitute(in_<-byref_0))
        eval.parent(substitute(arg2<-byref_1))
        invisible(NULL)
        }, error = function(c) {invisible(NULL)}
        )
    
    }
    
        )
) 

EEE = R6Class(classname = "EEE", cloneable = FALSE,
    A = 0
    B = 1 
