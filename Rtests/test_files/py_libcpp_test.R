#' @import reticulate
#' @import R6
#' @import purrr

listDepth <- NULL
Pymod <- NULL
npy <- NULL
py_builtin <- NULL
r_to_py <- NULL

.onLoad <- function(libname, pkgname) {
   Pymod <<- import("pyopenms", delay_load = TRUE)
   npy <<- import("numpy", convert = F, delay_load = TRUE)
   r_to_py <<- reticulate::r_to_py
   py_builtin <<- import_builtins(convert = F)
   listDepth <<- plotrix::listDepth
}

# R6 class object conversion to underlying python object.
`r_to_py.R6` <- function(i,...){
   tryCatch({
       i$.__enclos_env__$private$py_obj
   }, error = function(e) { "conversion not supported for this class"}
   )
}

# Returns the name of wrapper R6 class
class_to_wrap <- function(py_ob){
       class <- tail(strsplit(class(py_ob)[1],"\\.")[[1]],n = 1)
       # To correctly return the class name for Interfaces (BinaryDataArray,Chromatogram,Spectrum) by removing "_Interfaces_"
       comp <- strsplit(class,"_Interfaces_")[[1]]
       if (length(comp) == 1 && comp[1] == class){
           return(class)
       }
       else { return(comp[-1]) }
} 

#' ABS_Impl1 Interface
#'
#' @description
#' R6 implementation of _ABS_Impl1
#'
#' Documentation is available at:
#'
#'
#' @section Methods:
#' \code{$new()} create object of ABS_Impl1
#'
#' \code{$init_0()}
#'
#' \code{$init_1()}
#'
#' \code{$get()}
#'
#' @name ABS_Impl1
NULL

#' @export
ABS_Impl1 <- R6::R6Class(classname = "ABS_Impl1",cloneable = FALSE,lock_objects = T,

    private = list(py_obj = NA),

    
    public = list(
    
    #' @description
    #' * C++ signature : \code{void ABS_Impl1()}
    init_0 = function(){
    
        stop("Cannot call this constructor!")
    
    },
    
    #' @description
    #' * C++ signature : \code{void ABS_Impl1(int i)}
    #' @param i int
    init_1 = function(i){
    
        if(!((is.numeric(i) && length(i) == 1 && isTRUE(all.equal(i,as.integer(i)))))){ stop("arg i wrong type") }
    
    
        private$py_obj <- Pymod$ABS_Impl1(as.integer(i))
        invisible()
    
    
    },
    
    #' @description initialize
    #' This method calls one of the below:
    #' * \code{init_0} : void ABS_Impl1().
    #' * \code{init_1} : void ABS_Impl1(int i).
    initialize = function(...){
        arg_list = list(...)
        if (length(arg_list)==0) { self$init_0(...) }
        else if ((length(arg_list)==1) && ((is.numeric(arg_list[[1]]) && length(arg_list[[1]]) == 1 && isTRUE(all.equal(arg_list[[1]],as.integer(arg_list[[1]])))))) { self$init_1(...) }
        else{
               # to create a new R object and set its underlying python object as the one supplied in the constructor.
               # this helps avoid use of set_py_object(), s.t., the user is not able to manipulate the python object in a direct fashion.
               if( length(arg_list)==1 && ( "python.builtin.object" %in% class(arg_list[[1]]) && class_to_wrap(arg_list[[1]]) == "ABS_Impl1" ) )
               { private$py_obj <- arg_list[[1]]  }
               else {
                    stop("wrong arguments provided")
               }
           }
    
    },
    
    #' @description
    #' * C++ signature : \code{int get()}
    get = function(){
    
        py_ans = private$py_obj$get()
        r_ans = py_ans
        return(r_ans)
    }
)
)
ABS_Impl1$lock_class <- TRUE
 

#' ABS_Impl2 Interface
#'
#' @description
#' R6 implementation of _ABS_Impl2
#'
#' Documentation is available at:
#'
#'
#' @section Methods:
#' \code{$new()} create object of ABS_Impl2
#'
#' \code{$init_0()}
#'
#' \code{$init_1()}
#'
#' \code{$get()}
#'
#' @name ABS_Impl2
NULL

#' @export
ABS_Impl2 <- R6::R6Class(classname = "ABS_Impl2",cloneable = FALSE,lock_objects = T,

    private = list(py_obj = NA),

    
    public = list(
    
    #' @description
    #' * C++ signature : \code{void ABS_Impl2()}
    init_0 = function(){
    
        stop("Cannot call this constructor!")
    
    },
    
    #' @description
    #' * C++ signature : \code{void ABS_Impl2(int i)}
    #' @param i int
    init_1 = function(i){
    
        if(!((is.numeric(i) && length(i) == 1 && isTRUE(all.equal(i,as.integer(i)))))){ stop("arg i wrong type") }
    
    
        private$py_obj <- Pymod$ABS_Impl2(as.integer(i))
        invisible()
    
    
    },
    
    #' @description initialize
    #' This method calls one of the below:
    #' * \code{init_0} : void ABS_Impl2().
    #' * \code{init_1} : void ABS_Impl2(int i).
    initialize = function(...){
        arg_list = list(...)
        if (length(arg_list)==0) { self$init_0(...) }
        else if ((length(arg_list)==1) && ((is.numeric(arg_list[[1]]) && length(arg_list[[1]]) == 1 && isTRUE(all.equal(arg_list[[1]],as.integer(arg_list[[1]])))))) { self$init_1(...) }
        else{
               # to create a new R object and set its underlying python object as the one supplied in the constructor.
               # this helps avoid use of set_py_object(), s.t., the user is not able to manipulate the python object in a direct fashion.
               if( length(arg_list)==1 && ( "python.builtin.object" %in% class(arg_list[[1]]) && class_to_wrap(arg_list[[1]]) == "ABS_Impl2" ) )
               { private$py_obj <- arg_list[[1]]  }
               else {
                    stop("wrong arguments provided")
               }
           }
    
    },
    
    #' @description
    #' * C++ signature : \code{int get()}
    get = function(){
    
        py_ans = private$py_obj$get()
        r_ans = py_ans
        return(r_ans)
    }
)
)
ABS_Impl2$lock_class <- TRUE
 

#' Int Interface
#'
#' @description
#' R6 implementation of _Int
#'
#' Documentation is available at:
#'
#'
#' @section Methods:
#' \code{$i_}
#'
#' \code{$new()} create object of Int
#'
#' \code{$init_0()}
#'
#' \code{$init_1()}
#'
#' @name Int
NULL

#' @export
Int <- R6::R6Class(classname = "Int",cloneable = FALSE,lock_objects = T,

    private = list(py_obj = NA),


    active = list(
    #' @field i_
    #' Wrapper for i_ attribute
    #'
    #' Assuming obj is object of Int
    #'
    #' \bold{Accessing the attribute:}
    #' obj$i_
    #'
    #' \bold{Modifying the attribute:}
    #' obj$i_ <- value
        i_ = function(i_){
    
        if(!missing(i_)){
            if(!((is.numeric(i_) && length(i_) == 1 && isTRUE(all.equal(i_,as.integer(i_)))))){ stop("arg i_ wrong type") }
        
        
            private$py_obj$i_ <- as.integer(i_)
            } else {
        
                py_ans = private$py_obj$i_
                r_result = py_ans
                return(r_result)
                }
        }

    ),
    
    public = list(
    
    #' @description
    #' * C++ signature : \code{void Int(int i)}
    #' @param i int
    init_0 = function(i){
    
        if(!((is.numeric(i) && length(i) == 1 && isTRUE(all.equal(i,as.integer(i)))))){ stop("arg i wrong type") }
    
    
        private$py_obj <- Pymod$Int(as.integer(i))
        invisible()
    
    
    },
    
    #' @description
    #' * C++ signature : \code{void Int(Int & i)}
    #' @param i Int &
    init_1 = function(i){
    
        if(!(is.R6(i) && class(i)[1] == "Int")){ stop("arg i wrong type") }
    
    
        private$py_obj <- Pymod$Int(r_to_py(i))
        invisible()
    
    
    },
    
    #' @description initialize
    #' This method calls one of the below:
    #' * \code{init_0} : void Int(int i).
    #' * \code{init_1} : void Int(Int & i).
    initialize = function(...){
        arg_list = list(...)
        if ((length(arg_list)==1) && ((is.numeric(arg_list[[1]]) && length(arg_list[[1]]) == 1 && isTRUE(all.equal(arg_list[[1]],as.integer(arg_list[[1]])))))) { self$init_0(...) }
        else if ((length(arg_list)==1) && (is.R6(arg_list[[1]]) && class(arg_list[[1]])[1] == "Int")) { self$init_1(...) }
        else{
               # to create a new R object and set its underlying python object as the one supplied in the constructor.
               # this helps avoid use of set_py_object(), s.t., the user is not able to manipulate the python object in a direct fashion.
               if( length(arg_list)==1 && ( "python.builtin.object" %in% class(arg_list[[1]]) && class_to_wrap(arg_list[[1]]) == "Int" ) )
               { private$py_obj <- arg_list[[1]]  }
               else {
                    stop("wrong arguments provided")
               }
           }
    
    }
)
)
Int$lock_class <- TRUE
 

#' LibCppTest Interface
#'
#' @description
#' R6 implementation of _LibCppTest
#'
#' Documentation is available at:
#'
#'
#' @section Methods:
#' \code{$integer_vector_ptr}
#'
#' \code{$integer_ptr}
#'
#' \code{$new()} create object of LibCppTest
#'
#' \code{$init_0()}
#'
#' \code{$init_1()}
#'
#' \code{$gett()}
#'
#' \code{$twist()}
#'
#' \code{$process()}
#'
#' \code{$process2()}
#'
#' \code{$process3()}
#'
#' \code{$process4()}
#'
#' \code{$process5()}
#'
#' \code{$process6()}
#'
#' \code{$process7()}
#'
#' \code{$process8()}
#'
#' \code{$process9()}
#'
#' \code{$process10()}
#'
#' \code{$process11()}
#'
#' \code{$process12()}
#'
#' \code{$process13()}
#'
#' \code{$process14()}
#'
#' \code{$process15()}
#'
#' \code{$process16()}
#'
#' \code{$process17()}
#'
#' \code{$process18()}
#'
#' \code{$process19()}
#'
#' \code{$process20()}
#'
#' \code{$process21()}
#'
#' \code{$process211()}
#'
#' \code{$process212()}
#'
#' \code{$process214()}
#'
#' \code{$process22()}
#'
#' \code{$process23()}
#'
#' \code{$process24()}
#'
#' \code{$process25()}
#'
#' \code{$process26()}
#'
#' \code{$process27()}
#'
#' \code{$process28()}
#'
#' \code{$process29()}
#'
#' \code{$process30()}
#'
#' \code{$process31()}
#'
#' \code{$process32()}
#'
#' \code{$process33()}
#'
#' \code{$process34()}
#'
#' \code{$process35()}
#'
#' \code{$process36()}
#'
#' \code{$process37()}
#'
#' \code{$process38()}
#'
#' \code{$process39()}
#'
#' \code{$process40()}
#'
#' \code{$process40_0()}
#'
#' \code{$process40_1()}
#'
#' @name LibCppTest
NULL

#' @export
LibCppTest <- R6::R6Class(classname = "LibCppTest",cloneable = FALSE,lock_objects = T,

    private = list(py_obj = NA),


    active = list(
    #' @field integer_vector_ptr
    #' Wrapper for integer_vector_ptr attribute
    #'
    #' Assuming obj is object of LibCppTest
    #'
    #' \bold{Accessing the attribute:}
    #' obj$integer_vector_ptr
    #'
    #' \bold{Modifying the attribute:}
    #' obj$integer_vector_ptr <- value
        integer_vector_ptr = function(integer_vector_ptr){
    
        if(!missing(integer_vector_ptr)){
            if(!(is.vector(integer_vector_ptr) && all(sapply(integer_vector_ptr,function(elemt_rec) is.R6(elemt_rec) && class(elemt_rec)[1] == "Int")))){ stop("arg integer_vector_ptr wrong type") }
        
            v0 <- r_to_py(lapply(integer_vector_ptr,function(a) r_to_py(a)))
            private$py_obj$integer_vector_ptr <- v0
            } else {
            
            if (is.null(private$py_obj$integer_vector_ptr)) {
               stop("Cannot access NULL pointer")
            }
            else {
                py_ans = private$py_obj$integer_vector_ptr
            r_result = map(py_ans,function(i) Int$new(i))
                return(r_result)
                }
            }
        },
    #' @field integer_ptr
    #' Wrapper for integer_ptr attribute
    #'
    #' Assuming obj is object of LibCppTest
    #'
    #' \bold{Accessing the attribute:}
    #' obj$integer_ptr
    #'
    #' \bold{Modifying the attribute:}
    #' obj$integer_ptr <- value
        integer_ptr = function(integer_ptr){
    
        if(!missing(integer_ptr)){
            if(!(is.R6(integer_ptr) && class(integer_ptr)[1] == "Int")){ stop("arg integer_ptr wrong type") }
        
        
            private$py_obj$integer_ptr <- r_to_py(integer_ptr)
            } else {
        
            if (is.null(private$py_obj$integer_ptr)) {
               stop("Cannot access NULL pointer")
            }
            else {
                py_ans = private$py_obj$integer_ptr ; if( is.null(py_ans) ) { return(NULL) }
            r_result = Int$new(py_ans)
                return(r_result)
                }
            }
        }

    ),
    
    public = list(
    
    #' @description
    #' * C++ signature : \code{void LibCppTest()}
    init_0 = function(){
    
    
        private$py_obj <- Pymod$LibCppTest()
        invisible()
    
    
    },
    
    #' @description
    #' * C++ signature : \code{void LibCppTest(int ii)}
    #' @param ii int
    init_1 = function(ii){
    
        if(!((is.numeric(ii) && length(ii) == 1 && isTRUE(all.equal(ii,as.integer(ii)))))){ stop("arg ii wrong type") }
    
    
        private$py_obj <- Pymod$LibCppTest(as.integer(ii))
        invisible()
    
    
    },
    
    #' @description initialize
    #' This method calls one of the below:
    #' * \code{init_0} : void LibCppTest().
    #' * \code{init_1} : void LibCppTest(int ii).
    initialize = function(...){
        arg_list = list(...)
        if (length(arg_list)==0) { self$init_0(...) }
        else if ((length(arg_list)==1) && ((is.numeric(arg_list[[1]]) && length(arg_list[[1]]) == 1 && isTRUE(all.equal(arg_list[[1]],as.integer(arg_list[[1]])))))) { self$init_1(...) }
        else{
               # to create a new R object and set its underlying python object as the one supplied in the constructor.
               # this helps avoid use of set_py_object(), s.t., the user is not able to manipulate the python object in a direct fashion.
               if( length(arg_list)==1 && ( "python.builtin.object" %in% class(arg_list[[1]]) && class_to_wrap(arg_list[[1]]) == "LibCppTest" ) )
               { private$py_obj <- arg_list[[1]]  }
               else {
                    stop("wrong arguments provided")
               }
           }
    
    },
    
    #' @description
    #' * C++ signature : \code{int gett()}
    gett = function(){
    
        py_ans = private$py_obj$gett()
        r_ans = py_ans
        return(r_ans)
    },
    
    #' @description
    #' * C++ signature : \code{libcpp_pair[int,libcpp_string] twist(libcpp_pair[libcpp_string,int])}
    #' @param in_0 libcpp_pair[libcpp_string,int]
    twist = function(in_0){
    
        if(!(is.vector(in_0) && length(in_0) == 2 && is_scalar_character(in_0[[1]]) && (is.numeric(in_0[[2]]) && length(in_0[[2]]) == 1 && isTRUE(all.equal(in_0[[2]],as.integer(in_0[[2]])))))){ stop("arg in_0 wrong type") }
        v0 = r_to_py(list(py_builtin$bytes(in_0[[1]],'utf-8'),as.integer(in_0[[2]])))
        py_ans = private$py_obj$twist(v0)
        r_ans = list(py_ans[[1]], as.character(py_ans[[2]]))
        return(r_ans)
    },
    
    #' @description
    #' * C++ signature : \code{libcpp_vector[int] process(libcpp_vector[int] &)}
    #' @param in_0 libcpp_vector[int] &
    process = function(in_0){
    
        if(!(is.vector(in_0) && all(sapply(in_0,function(elemt_rec) (is.numeric(elemt_rec) && length(elemt_rec) == 1 && isTRUE(all.equal(elemt_rec,as.integer(elemt_rec)))))))){ stop("arg in_0 wrong type") }
        v0 <- r_to_py(map_depth(in_0,1,as.integer))
        py_ans = private$py_obj$process(v0)
        byref_0 <- py_to_r(v0)
        r_ans <- py_ans
    
        tryCatch({
        eval.parent(substitute(in_0 <- byref_0))
        return(r_ans)
        }, error = function(c) {return(r_ans)}
        )
    
    },
    
    #' @description
    #' * C++ signature : \code{libcpp_pair[int,int] process2(libcpp_pair[int,int] &)}
    #' @param in_0 libcpp_pair[int,int] &
    process2 = function(in_0){
    
        if(!(is.vector(in_0) && length(in_0) == 2 && (is.numeric(in_0[[1]]) && length(in_0[[1]]) == 1 && isTRUE(all.equal(in_0[[1]],as.integer(in_0[[1]])))) && (is.numeric(in_0[[2]]) && length(in_0[[2]]) == 1 && isTRUE(all.equal(in_0[[2]],as.integer(in_0[[2]])))))){ stop("arg in_0 wrong type") }
        v0 = r_to_py(list(as.integer(in_0[[1]]),as.integer(in_0[[2]])))
        py_ans = private$py_obj$process2(v0)
        byref_0 = list(py_to_r(v0[0]), py_to_r(v0[1]))
        r_ans = list(py_ans[[1]], py_ans[[2]])
    
        tryCatch({
        eval.parent(substitute(in_0 <- byref_0))
        return(r_ans)
        }, error = function(c) {return(r_ans)}
        )
    
    },
    
    #' @description
    #' * C++ signature : \code{libcpp_pair[LibCppTest,int] process3(libcpp_pair[LibCppTest,int] &)}
    #' @param in_0 libcpp_pair[LibCppTest,int] &
    process3 = function(in_0){
    
        if(!(is.vector(in_0) && length(in_0) == 2 && is.R6(in_0[[1]]) && class(in_0[[1]])[1] == "LibCppTest" && (is.numeric(in_0[[2]]) && length(in_0[[2]]) == 1 && isTRUE(all.equal(in_0[[2]],as.integer(in_0[[2]])))))){ stop("arg in_0 wrong type") }
        v0 = r_to_py(list(r_to_py(in_0[[1]]),as.integer(in_0[[2]])))
        py_ans = private$py_obj$process3(v0)
        temp1 = LibCppTest$new(py_to_r(v0[0]))
        byref_0 = list(temp1, py_to_r(v0[1]))
        out1 = LibCppTest$new(py_ans[[1]])
        r_ans = list(out1, py_ans[[2]])
    
        tryCatch({
        eval.parent(substitute(in_0 <- byref_0))
        return(r_ans)
        }, error = function(c) {return(r_ans)}
        )
    
    },
    
    #' @description
    #' * C++ signature : \code{libcpp_pair[int,LibCppTest] process4(libcpp_pair[int,LibCppTest] &)}
    #' @param in_0 libcpp_pair[int,LibCppTest] &
    process4 = function(in_0){
    
        if(!(is.vector(in_0) && length(in_0) == 2 && (is.numeric(in_0[[1]]) && length(in_0[[1]]) == 1 && isTRUE(all.equal(in_0[[1]],as.integer(in_0[[1]])))) && is.R6(in_0[[2]]) && class(in_0[[2]])[1] == "LibCppTest")){ stop("arg in_0 wrong type") }
        v0 = r_to_py(list(as.integer(in_0[[1]]),in_0[[2]]))
        py_ans = private$py_obj$process4(v0)
        temp2 = LibCppTest$new(py_to_r(v0[1]))
        byref_0 = list(py_to_r(v0[0]), temp2)
        out2 = LibCppTest$new(py_ans[[2]])
        r_ans = list(py_ans[[1]], out2)
    
        tryCatch({
        eval.parent(substitute(in_0 <- byref_0))
        return(r_ans)
        }, error = function(c) {return(r_ans)}
        )
    
    },
    
    #' @description
    #' * C++ signature : \code{libcpp_pair[LibCppTest,LibCppTest] process5(libcpp_pair[LibCppTest,LibCppTest] &)}
    #' @param in_0 libcpp_pair[LibCppTest,LibCppTest] &
    process5 = function(in_0){
    
        if(!(is.vector(in_0) && length(in_0) == 2 && is.R6(in_0[[1]]) && class(in_0[[1]])[1] == "LibCppTest" && is.R6(in_0[[2]]) && class(in_0[[2]])[1] == "LibCppTest")){ stop("arg in_0 wrong type") }
        v0 = r_to_py(list(r_to_py(in_0[[1]]),in_0[[2]]))
        py_ans = private$py_obj$process5(v0)
        temp1 = LibCppTest$new(py_to_r(v0[0]))
        temp2 = LibCppTest$new(py_to_r(v0[1]))
        byref_0 = list(temp1, temp2)
        out1 = LibCppTest$new(py_ans[[1]])
        out2 = LibCppTest$new(py_ans[[2]])
        r_ans = list(out1, out2)
    
        tryCatch({
        eval.parent(substitute(in_0 <- byref_0))
        return(r_ans)
        }, error = function(c) {return(r_ans)}
        )
    
    },
    
    #' @description
    #' * C++ signature : \code{libcpp_vector[libcpp_pair[int,double]] process6(libcpp_vector[libcpp_pair[int,double]] &)}
    #' @param in_0 libcpp_vector[libcpp_pair[int,double]] &
    process6 = function(in_0){
    
        if(!(is.vector(in_0) && all(sapply(in_0,function(elemt_rec) is.vector(elemt_rec) && length(elemt_rec) == 2 && (is.numeric(elemt_rec[[1]]) && length(elemt_rec[[1]]) == 1 && isTRUE(all.equal(elemt_rec[[1]],as.integer(elemt_rec[[1]])))) && is_scalar_double(elemt_rec[[2]]))))){ stop("arg in_0 wrong type") }
        v0 <- r_to_py(map_depth(in_0,1, function(a) list(as.integer(a[[1]]),r_to_py(a[[2]]))))
        py_ans = private$py_obj$process6(v0)
        byref_0 <- py_to_r(v0)
        r_ans <- py_ans
    
        tryCatch({
        eval.parent(substitute(in_0 <- byref_0))
        return(r_ans)
        }, error = function(c) {return(r_ans)}
        )
    
    },
    
    #' @description
    #' * C++ signature : \code{libcpp_pair[int,EEE] process7(libcpp_pair[EEE,int] &)}
    #' @param in_0 libcpp_pair[EEE,int] &
    process7 = function(in_0){
    
        if(!(is.vector(in_0) && length(in_0) == 2 && in_0[[1]] %in% c(0, 1) && (is.numeric(in_0[[2]]) && length(in_0[[2]]) == 1 && isTRUE(all.equal(in_0[[2]],as.integer(in_0[[2]])))))){ stop("arg in_0 wrong type") }
        v0 = r_to_py(list(as.integer(in_0[[1]]),as.integer(in_0[[2]])))
        py_ans = private$py_obj$process7(v0)
        byref_0 = list(py_to_r(v0[0]), py_to_r(v0[1]))
        out2 = py_ans[[2]]
        r_ans = list(py_ans[[1]], out2)
    
        tryCatch({
        eval.parent(substitute(in_0 <- byref_0))
        return(r_ans)
        }, error = function(c) {return(r_ans)}
        )
    
    },
    
    #' @description
    #' * C++ signature : \code{libcpp_vector[EEE] process8(libcpp_vector[EEE] &)}
    #' @param in_0 libcpp_vector[EEE] &
    process8 = function(in_0){
    
        if(!(is.vector(in_0) && all(sapply(in_0,function(elemt_rec) elemt_rec %in% c(0, 1))))){ stop("arg in_0 wrong type") }
        v0 <- r_to_py(map(in_0,as.integer))
        py_ans = private$py_obj$process8(v0)
        byref_0 <- py_to_r(v0)
        r_ans <- py_ans
    
        tryCatch({
        eval.parent(substitute(in_0 <- byref_0))
        return(r_ans)
        }, error = function(c) {return(r_ans)}
        )
    
    },
    
    #' @description
    #' * C++ signature : \code{libcpp_set[int] process9(libcpp_set[int] &)}
    #' @param in_0 libcpp_set[int] &
    process9 = function(in_0){
    
        if(!(is.vector(in_0) && all(sapply(in_0,function(el) (is.numeric(el) && length(el) == 1 && isTRUE(all.equal(el,as.integer(el)))))) && !(TRUE %in% duplicated(in_0)))){ stop("arg in_0 wrong type") }
        v0 <- py_builtin$set(lapply(in_0,as.integer))
        py_ans = private$py_obj$process9(v0)
        byref_0 <- py_to_r(py_builtin$list(v0))
        r_ans <- py_to_r(py_builtin$list(py_ans))
    
        tryCatch({
        eval.parent(substitute(in_0 <- byref_0))
        return(r_ans)
        }, error = function(c) {return(r_ans)}
        )
    
    },
    
    #' @description
    #' * C++ signature : \code{libcpp_set[EEE] process10(libcpp_set[EEE] &)}
    #' @param in_0 libcpp_set[EEE] &
    process10 = function(in_0){
    
        if(!(is.vector(in_0) && all(sapply(in_0,function(el) el %in% c(0, 1))) && !(TRUE %in% duplicated(in_0)))){ stop("arg in_0 wrong type") }
        v0 <- py_builtin$set(lapply(in_0,as.integer))
        py_ans = private$py_obj$process10(v0)
        byref_0 <- py_to_r(py_builtin$list(v0))
        r_ans <- py_to_r(py_builtin$list(py_ans))
    
        tryCatch({
        eval.parent(substitute(in_0 <- byref_0))
        return(r_ans)
        }, error = function(c) {return(r_ans)}
        )
    
    },
    
    #' @description
    #' * C++ signature : \code{libcpp_set[LibCppTest] process11(libcpp_set[LibCppTest] &)}
    #' @param in_0 libcpp_set[LibCppTest] &
    process11 = function(in_0){
    
        if(!(
    is.vector(in_0) && all(sapply(in_0,function(el) is.R6(el) && class(el)[1] == "LibCppTest")) && length(in_0) == py_to_r(py_builtin$len(py_builtin$set(lapply(in_0,function(a) r_to_py(a)))))
              )){ stop("arg in_0 wrong type") }
        v0 <- py_builtin$set(lapply(in_0, function(a) r_to_py(a)))
        py_ans = private$py_obj$process11(v0)
        byref_0 <- lapply(py_to_r(py_builtin$list(v0)),function(t) LibCppTest$new(t))
        r_ans <- lapply(py_to_r(py_builtin$list(py_ans)), function(a) LibCppTest$new(a))
    
        tryCatch({
        eval.parent(substitute(in_0 <- byref_0))
        return(r_ans)
        }, error = function(c) {return(r_ans)}
        )
    
    },
    
    #' @description
    #' * C++ signature : \code{libcpp_map[int,float] process12(int i, float f)}
    #' @param i int
    #' @param f float
    process12 = function(i, f){
    
        if(!((is.numeric(i) && length(i) == 1 && isTRUE(all.equal(i,as.integer(i)))))){ stop("arg i wrong type") }
        if(!(is_scalar_double(f))){ stop("arg f wrong type") }
    
    
        py_ans = py_call(private$py_obj$process12,as.integer(i), f)
        r_ans <- collections::dict(py_to_r(py_builtin$list(py_ans$values())),py_to_r(py_builtin$list(py_ans$keys())))
        return(r_ans)
    },
    
    #' @description
    #' * C++ signature : \code{libcpp_map[EEE,int] process13(EEE e, int i)}
    #' @param e EEE
    #' @param i int
    process13 = function(e, i){
    
        if(!(e %in% c(0, 1))){ stop("arg e wrong type") }
        if(!((is.numeric(i) && length(i) == 1 && isTRUE(all.equal(i,as.integer(i)))))){ stop("arg i wrong type") }
    
    
        py_ans = py_call(private$py_obj$process13,as.integer(e), as.integer(i))
        r_ans <- collections::dict(py_to_r(py_builtin$list(py_ans$values())),py_to_r(py_builtin$list(py_ans$keys())))
        return(r_ans)
    },
    
    #' @description
    #' * C++ signature : \code{libcpp_map[int,EEE] process14(EEE e, int i)}
    #' @param e EEE
    #' @param i int
    process14 = function(e, i){
    
        if(!(e %in% c(0, 1))){ stop("arg e wrong type") }
        if(!((is.numeric(i) && length(i) == 1 && isTRUE(all.equal(i,as.integer(i)))))){ stop("arg i wrong type") }
    
    
        py_ans = py_call(private$py_obj$process14,as.integer(e), as.integer(i))
        r_ans <- collections::dict(py_to_r(py_builtin$list(py_ans$values())),py_to_r(py_builtin$list(py_ans$keys())))
        return(r_ans)
    },
    
    #' @description
    #' * C++ signature : \code{libcpp_map[long int,LibCppTest] process15(int ii)}
    #' @param ii int
    process15 = function(ii){
    
        if(!((is.numeric(ii) && length(ii) == 1 && isTRUE(all.equal(ii,as.integer(ii)))))){ stop("arg ii wrong type") }
    
        py_ans = py_call(private$py_obj$process15,as.integer(ii))
        r_ans <- collections::dict(lapply(py_to_r(py_builtin$list(py_ans$values())), function(i) LibCppTest$new(i)), py_to_r(py_builtin$list(py_ans$keys())))
        return(r_ans)
    },
    
    #' @description
    #' * C++ signature : \code{float process16(libcpp_map[int,float] in_)}
    #' @param in_ libcpp_map[int,float]
    process16 = function(in_){
    
        if(!(
          is.environment(in_) && identical(parent.env(in_), asNamespace("collections")) && identical(strsplit(capture.output(in_$print())," ")[[1]][1], "dict")
          && all(sapply(in_$keys(),function(k) (is.numeric(k) && length(k) == 1 && isTRUE(all.equal(k,as.integer(k))))))
          && all(sapply(in_$values(),function(v) is_scalar_double(v)))
          )){ stop("arg in_ wrong type") }
        v0 <- py_dict(as.integer(in_$keys()),in_$values())
        py_ans = private$py_obj$process16(v0)
        r_ans = py_ans
        return(r_ans)
    },
    
    #' @description
    #' * C++ signature : \code{float process17(libcpp_map[EEE,float] in_)}
    #' @param in_ libcpp_map[EEE,float]
    process17 = function(in_){
    
        if(!(
          is.environment(in_) && identical(parent.env(in_), asNamespace("collections")) && identical(strsplit(capture.output(in_$print())," ")[[1]][1], "dict")
          && all(sapply(in_$keys(),function(k) k %in% c(0, 1)))
          && all(sapply(in_$values(),function(v) is_scalar_double(v)))
          )){ stop("arg in_ wrong type") }
        v0 <- py_dict(as.integer(in_$keys()),in_$values())
        py_ans = private$py_obj$process17(v0)
        r_ans = py_ans
        return(r_ans)
    },
    
    #' @description
    #' * C++ signature : \code{int process18(libcpp_map[int,LibCppTest] in_)}
    #' @param in_ libcpp_map[int,LibCppTest]
    process18 = function(in_){
    
        if(!(
          is.environment(in_) && identical(parent.env(in_), asNamespace("collections")) && identical(strsplit(capture.output(in_$print())," ")[[1]][1], "dict")
          && all(sapply(in_$keys(),function(k) (is.numeric(k) && length(k) == 1 && isTRUE(all.equal(k,as.integer(k))))))
          && all(sapply(in_$values(),function(v) is.R6(v) && class(v)[1] == "LibCppTest"))
          )){ stop("arg in_ wrong type") }
        v0 <- py_dict(as.integer(in_$keys()),lapply(in_$values(), function(v) r_to_py(v)))
        py_ans = private$py_obj$process18(v0)
        r_ans = py_ans
        return(r_ans)
    },
    
    #' @description
    #' * C++ signature : \code{void process19(libcpp_map[int,LibCppTest] & in_)}
    #' @param in_ libcpp_map[int,LibCppTest] &
    process19 = function(in_){
    
        if(!(
          is.environment(in_) && identical(parent.env(in_), asNamespace("collections")) && identical(strsplit(capture.output(in_$print())," ")[[1]][1], "dict")
          && all(sapply(in_$keys(),function(k) (is.numeric(k) && length(k) == 1 && isTRUE(all.equal(k,as.integer(k))))))
          && all(sapply(in_$values(),function(v) is.R6(v) && class(v)[1] == "LibCppTest"))
          )){ stop("arg in_ wrong type") }
        v0 <- py_dict(as.integer(in_$keys()),lapply(in_$values(), function(v) r_to_py(v)))
        private$py_obj$process19(v0)
        byref_0 <- collections::dict(lapply(py_to_r(py_builtin$list(v0$values())),function(i) LibCppTest$new(i)), py_to_r(py_builtin$list(v0$keys())))
    
        tryCatch({
        eval.parent(substitute(in_ <- byref_0))
        invisible()
        }, error = function(c) {invisible()}
        )
    
    },
    
    #' @description
    #' * C++ signature : \code{void process20(libcpp_map[int,float] & in_)}
    #' @param in_ libcpp_map[int,float] &
    process20 = function(in_){
    
        if(!(
          is.environment(in_) && identical(parent.env(in_), asNamespace("collections")) && identical(strsplit(capture.output(in_$print())," ")[[1]][1], "dict")
          && all(sapply(in_$keys(),function(k) (is.numeric(k) && length(k) == 1 && isTRUE(all.equal(k,as.integer(k))))))
          && all(sapply(in_$values(),function(v) is_scalar_double(v)))
          )){ stop("arg in_ wrong type") }
        v0 <- py_dict(as.integer(in_$keys()),in_$values())
        private$py_obj$process20(v0)
        byref_0 <- collections::dict(py_to_r(py_builtin$list(v0$values())),py_to_r(py_builtin$list(v0$keys())))
    
        tryCatch({
        eval.parent(substitute(in_ <- byref_0))
        invisible()
        }, error = function(c) {invisible()}
        )
    
    },
    
    #' @description
    #' * C++ signature : \code{void process21(libcpp_map[int,float] & in_, libcpp_map[int,int] & arg2)}
    #' @param in_ libcpp_map[int,float] &
    #' @param arg2 libcpp_map[int,int] &
    process21 = function(in_, arg2){
    
        if(!(
          is.environment(in_) && identical(parent.env(in_), asNamespace("collections")) && identical(strsplit(capture.output(in_$print())," ")[[1]][1], "dict")
          && all(sapply(in_$keys(),function(k) (is.numeric(k) && length(k) == 1 && isTRUE(all.equal(k,as.integer(k))))))
          && all(sapply(in_$values(),function(v) is_scalar_double(v)))
          )){ stop("arg in_ wrong type") }
        if(!(
          is.environment(arg2) && identical(parent.env(arg2), asNamespace("collections")) && identical(strsplit(capture.output(arg2$print())," ")[[1]][1], "dict")
          && all(sapply(arg2$keys(),function(k) (is.numeric(k) && length(k) == 1 && isTRUE(all.equal(k,as.integer(k))))))
          && all(sapply(arg2$values(),function(v) (is.numeric(v) && length(v) == 1 && isTRUE(all.equal(v,as.integer(v))))))
          )){ stop("arg arg2 wrong type") }
        v0 <- py_dict(as.integer(in_$keys()),in_$values())
        v1 <- py_dict(as.integer(arg2$keys()),as.integer(arg2$values()))
        private$py_obj$process21(v0, v1)
        byref_1 <- collections::dict(py_to_r(py_builtin$list(v1$values())),py_to_r(py_builtin$list(v1$keys())))
        byref_0 <- collections::dict(py_to_r(py_builtin$list(v0$values())),py_to_r(py_builtin$list(v0$keys())))
    
        tryCatch({
        eval.parent(substitute(in_ <- byref_0))
        eval.parent(substitute(arg2 <- byref_1))
        invisible()
        }, error = function(c) {invisible()}
        )
    
    },
    
    #' @description
    #' * C++ signature : \code{void process211(libcpp_map[int,float] & in_, libcpp_map[libcpp_string,libcpp_vector[int]] & arg2)}
    #' @param in_ libcpp_map[int,float] &
    #' @param arg2 libcpp_map[libcpp_string,libcpp_vector[int]] &
    process211 = function(in_, arg2){
    
        if(!(
          is.environment(in_) && identical(parent.env(in_), asNamespace("collections")) && identical(strsplit(capture.output(in_$print())," ")[[1]][1], "dict")
          && all(sapply(in_$keys(),function(k) (is.numeric(k) && length(k) == 1 && isTRUE(all.equal(k,as.integer(k))))))
          && all(sapply(in_$values(),function(v) is_scalar_double(v)))
          )){ stop("arg in_ wrong type") }
        if(!(
          is.environment(arg2) && identical(parent.env(arg2), asNamespace("collections")) && identical(strsplit(capture.output(arg2$print())," ")[[1]][1], "dict")
          && all(sapply(arg2$keys(),function(k) is_scalar_character(k)))
          && all(sapply(arg2$values(),function(v) is.vector(v) && all(sapply(v,function(elemt_rec) (is.numeric(elemt_rec) && length(elemt_rec) == 1 && isTRUE(all.equal(elemt_rec,as.integer(elemt_rec))))))))
          )){ stop("arg arg2 wrong type") }
        v0 <- py_dict(as.integer(in_$keys()),in_$values())
        v1 <- py_dict(modify_depth(arg2$keys(),1,function(a) py_builtin$bytes(a,'utf-8')),modify_depth(arg2$values(),2,as.integer))
        private$py_obj$process211(v0, v1)
        byref_1 <- collections::dict(py_to_r(py_builtin$list(v1$values())), lapply(py_to_r(py_builtin$list(v1$keys())),as.character))
        byref_0 <- collections::dict(py_to_r(py_builtin$list(v0$values())),py_to_r(py_builtin$list(v0$keys())))
    
        tryCatch({
        eval.parent(substitute(in_ <- byref_0))
        eval.parent(substitute(arg2 <- byref_1))
        invisible()
        }, error = function(c) {invisible()}
        )
    
    },
    
    #' @description
    #' * C++ signature : \code{void process212(libcpp_map[int,float] & in_, libcpp_map[libcpp_string,libcpp_vector[libcpp_vector[int]]] & arg2)}
    #' @param in_ libcpp_map[int,float] &
    #' @param arg2 libcpp_map[libcpp_string,libcpp_vector[libcpp_vector[int]]] &
    process212 = function(in_, arg2){
    
        if(!(
          is.environment(in_) && identical(parent.env(in_), asNamespace("collections")) && identical(strsplit(capture.output(in_$print())," ")[[1]][1], "dict")
          && all(sapply(in_$keys(),function(k) (is.numeric(k) && length(k) == 1 && isTRUE(all.equal(k,as.integer(k))))))
          && all(sapply(in_$values(),function(v) is_scalar_double(v)))
          )){ stop("arg in_ wrong type") }
        if(!(
          is.environment(arg2) && identical(parent.env(arg2), asNamespace("collections")) && identical(strsplit(capture.output(arg2$print())," ")[[1]][1], "dict")
          && all(sapply(arg2$keys(),function(k) is_scalar_character(k)))
          && all(sapply(arg2$values(),function(v) is.vector(v) && all(sapply(v,function(elemt_rec) is.vector(elemt_rec) && all(sapply(elemt_rec,function(elemt_rec_rec) (is.numeric(elemt_rec_rec) && length(elemt_rec_rec) == 1 && isTRUE(all.equal(elemt_rec_rec,as.integer(elemt_rec_rec))))))))))
          )){ stop("arg arg2 wrong type") }
        v0 <- py_dict(as.integer(in_$keys()),in_$values())
        v1 <- py_dict(modify_depth(arg2$keys(),1,function(a) py_builtin$bytes(a,'utf-8')),modify_depth(arg2$values(),3,as.integer))
        private$py_obj$process212(v0, v1)
        byref_1 <- collections::dict(py_to_r(py_builtin$list(v1$values())), lapply(py_to_r(py_builtin$list(v1$keys())),as.character))
        byref_0 <- collections::dict(py_to_r(py_builtin$list(v0$values())),py_to_r(py_builtin$list(v0$keys())))
    
        tryCatch({
        eval.parent(substitute(in_ <- byref_0))
        eval.parent(substitute(arg2 <- byref_1))
        invisible()
        }, error = function(c) {invisible()}
        )
    
    },
    
    #' @description
    #' * C++ signature : \code{void process214(libcpp_map[int,float] & in_, libcpp_map[libcpp_string,libcpp_vector[libcpp_pair[int,int]]] & arg2)}
    #' @param in_ libcpp_map[int,float] &
    #' @param arg2 libcpp_map[libcpp_string,libcpp_vector[libcpp_pair[int,int]]] &
    process214 = function(in_, arg2){
    
        if(!(
          is.environment(in_) && identical(parent.env(in_), asNamespace("collections")) && identical(strsplit(capture.output(in_$print())," ")[[1]][1], "dict")
          && all(sapply(in_$keys(),function(k) (is.numeric(k) && length(k) == 1 && isTRUE(all.equal(k,as.integer(k))))))
          && all(sapply(in_$values(),function(v) is_scalar_double(v)))
          )){ stop("arg in_ wrong type") }
        if(!(
          is.environment(arg2) && identical(parent.env(arg2), asNamespace("collections")) && identical(strsplit(capture.output(arg2$print())," ")[[1]][1], "dict")
          && all(sapply(arg2$keys(),function(k) is_scalar_character(k)))
          && all(sapply(arg2$values(),function(v) is.vector(v) && all(sapply(v,function(elemt_rec) is.vector(elemt_rec) && length(elemt_rec) == 2 && (is.numeric(elemt_rec[[1]]) && length(elemt_rec[[1]]) == 1 && isTRUE(all.equal(elemt_rec[[1]],as.integer(elemt_rec[[1]])))) && (is.numeric(elemt_rec[[2]]) && length(elemt_rec[[2]]) == 1 && isTRUE(all.equal(elemt_rec[[2]],as.integer(elemt_rec[[2]]))))))))
          )){ stop("arg arg2 wrong type") }
        v0 <- py_dict(as.integer(in_$keys()),in_$values())
        v1 <- py_dict(modify_depth(arg2$keys(),1,function(a) py_builtin$bytes(a,'utf-8')),map_depth(arg2$values(),2,function(a) list(as.integer(a[[1]]),as.integer(a[[2]]))))
        private$py_obj$process214(v0, v1)
        byref_1 <- collections::dict(py_to_r(py_builtin$list(v1$values())), lapply(py_to_r(py_builtin$list(v1$keys())),as.character))
        byref_0 <- collections::dict(py_to_r(py_builtin$list(v0$values())),py_to_r(py_builtin$list(v0$keys())))
    
        tryCatch({
        eval.parent(substitute(in_ <- byref_0))
        eval.parent(substitute(arg2 <- byref_1))
        invisible()
        }, error = function(c) {invisible()}
        )
    
    },
    
    #' @description
    #' * C++ signature : \code{void process22(libcpp_set[int] &, libcpp_set[float] &)}
    #' @param in_0 libcpp_set[int] &
    #' @param in_1 libcpp_set[float] &
    process22 = function(in_0, in_1){
    
        if(!(is.vector(in_0) && all(sapply(in_0,function(el) (is.numeric(el) && length(el) == 1 && isTRUE(all.equal(el,as.integer(el)))))) && !(TRUE %in% duplicated(in_0)))){ stop("arg in_0 wrong type") }
        if(!(is.vector(in_1) && all(sapply(in_1,function(el) is_scalar_double(el))) && !(TRUE %in% duplicated(in_1)))){ stop("arg in_1 wrong type") }
        v0 <- py_builtin$set(lapply(in_0,as.integer))
        v1 <- py_builtin$set(in_1)
        private$py_obj$process22(v0, v1)
        byref_1 <- py_to_r(py_builtin$list(v1))
        byref_0 <- py_to_r(py_builtin$list(v0))
    
        tryCatch({
        eval.parent(substitute(in_0 <- byref_0))
        eval.parent(substitute(in_1 <- byref_1))
        invisible()
        }, error = function(c) {invisible()}
        )
    
    },
    
    #' @description
    #' * C++ signature : \code{void process23(libcpp_vector[int] &, libcpp_vector[float] &)}
    #' @param in_0 libcpp_vector[int] &
    #' @param in_1 libcpp_vector[float] &
    process23 = function(in_0, in_1){
    
        if(!(is.vector(in_0) && all(sapply(in_0,function(elemt_rec) (is.numeric(elemt_rec) && length(elemt_rec) == 1 && isTRUE(all.equal(elemt_rec,as.integer(elemt_rec)))))))){ stop("arg in_0 wrong type") }
        if(!(is.vector(in_1) && all(sapply(in_1,function(elemt_rec) is_scalar_double(elemt_rec))))){ stop("arg in_1 wrong type") }
        v0 <- r_to_py(map_depth(in_0,1,as.integer))
        v1 <- r_to_py(in_1)
        private$py_obj$process23(v0, v1)
        byref_1 <- py_to_r(v1)
        byref_0 <- py_to_r(v0)
    
        tryCatch({
        eval.parent(substitute(in_0 <- byref_0))
        eval.parent(substitute(in_1 <- byref_1))
        invisible()
        }, error = function(c) {invisible()}
        )
    
    },
    
    #' @description
    #' * C++ signature : \code{void process24(libcpp_pair[int,float] & in_, libcpp_pair[int,int] & arg2)}
    #' @param in_ libcpp_pair[int,float] &
    #' @param arg2 libcpp_pair[int,int] &
    process24 = function(in_, arg2){
    
        if(!(is.vector(in_) && length(in_) == 2 && (is.numeric(in_[[1]]) && length(in_[[1]]) == 1 && isTRUE(all.equal(in_[[1]],as.integer(in_[[1]])))) && is_scalar_double(in_[[2]]))){ stop("arg in_ wrong type") }
        if(!(is.vector(arg2) && length(arg2) == 2 && (is.numeric(arg2[[1]]) && length(arg2[[1]]) == 1 && isTRUE(all.equal(arg2[[1]],as.integer(arg2[[1]])))) && (is.numeric(arg2[[2]]) && length(arg2[[2]]) == 1 && isTRUE(all.equal(arg2[[2]],as.integer(arg2[[2]])))))){ stop("arg arg2 wrong type") }
        v0 = r_to_py(list(as.integer(in_[[1]]),in_[[2]]))
        v1 = r_to_py(list(as.integer(arg2[[1]]),as.integer(arg2[[2]])))
        private$py_obj$process24(v0, v1)
        byref_1 = list(py_to_r(v1[0]), py_to_r(v1[1]))
        byref_0 = list(py_to_r(v0[0]), py_to_r(v0[1]))
    
        tryCatch({
        eval.parent(substitute(in_ <- byref_0))
        eval.parent(substitute(arg2 <- byref_1))
        invisible()
        }, error = function(c) {invisible()}
        )
    
    },
    
    #' @description
    #' * C++ signature : \code{int process25(libcpp_vector[Int] in_)}
    #' @param in_ libcpp_vector[Int]
    process25 = function(in_){
    
        if(!(is.vector(in_) && all(sapply(in_,function(elemt_rec) is.R6(elemt_rec) && class(elemt_rec)[1] == "Int")))){ stop("arg in_ wrong type") }
        v0 <- r_to_py(lapply(in_,function(a) r_to_py(a)))
        py_ans = private$py_obj$process25(v0)
        
        r_ans = py_ans
        return(r_ans)
    },
    
    #' @description
    #' * C++ signature : \code{int process26(libcpp_vector[libcpp_vector[Int]] in_)}
    #' @param in_ libcpp_vector[libcpp_vector[Int]]
    process26 = function(in_){
    
        if(!(is.vector(in_) && all(sapply(in_,function(elemt_rec) is.vector(elemt_rec) && all(sapply(elemt_rec,function(elemt_rec_rec) is.R6(elemt_rec_rec) && class(elemt_rec_rec)[1] == "Int")))))){ stop("arg in_ wrong type") }
        depth_0 <- listDepth(in_)
        v0 <- r_to_py(modify_depth(in_, depth_0, function(a) r_to_py(a)))
        py_ans = private$py_obj$process26(v0)
        
        r_ans = py_ans
        return(r_ans)
    },
    
    #' @description
    #' * C++ signature : \code{int process27(libcpp_vector[libcpp_vector[libcpp_vector[Int]]] in_)}
    #' @param in_ libcpp_vector[libcpp_vector[libcpp_vector[Int]]]
    process27 = function(in_){
    
        if(!(is.vector(in_) && all(sapply(in_,function(elemt_rec) is.vector(elemt_rec) && all(sapply(elemt_rec,function(elemt_rec_rec) is.vector(elemt_rec_rec) && all(sapply(elemt_rec_rec,function(elemt_rec_rec_rec) is.R6(elemt_rec_rec_rec) && class(elemt_rec_rec_rec)[1] == "Int")))))))){ stop("arg in_ wrong type") }
        depth_0 <- listDepth(in_)
        v0 <- r_to_py(modify_depth(in_, depth_0, function(a) r_to_py(a)))
        py_ans = private$py_obj$process27(v0)
        
        r_ans = py_ans
        return(r_ans)
    },
    
    #' @description
    #' * C++ signature : \code{int process28(libcpp_vector[libcpp_vector[libcpp_vector[libcpp_vector[Int]]]] in_)}
    #' @param in_ libcpp_vector[libcpp_vector[libcpp_vector[libcpp_vector[Int]]]]
    process28 = function(in_){
    
        if(!(is.vector(in_) && all(sapply(in_,function(elemt_rec) is.vector(elemt_rec) && all(sapply(elemt_rec,function(elemt_rec_rec) is.vector(elemt_rec_rec) && all(sapply(elemt_rec_rec,function(elemt_rec_rec_rec) is.vector(elemt_rec_rec_rec) && all(sapply(elemt_rec_rec_rec,function(elemt_rec_rec_rec_rec) is.R6(elemt_rec_rec_rec_rec) && class(elemt_rec_rec_rec_rec)[1] == "Int")))))))))){ stop("arg in_ wrong type") }
        depth_0 <- listDepth(in_)
        v0 <- r_to_py(modify_depth(in_, depth_0, function(a) r_to_py(a)))
        py_ans = private$py_obj$process28(v0)
        
        r_ans = py_ans
        return(r_ans)
    },
    
    #' @description
    #' * C++ signature : \code{void process29(libcpp_vector[libcpp_vector[Int]] & in_)}
    #' @param in_ libcpp_vector[libcpp_vector[Int]] &
    process29 = function(in_){
    
        if(!(is.vector(in_) && all(sapply(in_,function(elemt_rec) is.vector(elemt_rec) && all(sapply(elemt_rec,function(elemt_rec_rec) is.R6(elemt_rec_rec) && class(elemt_rec_rec)[1] == "Int")))))){ stop("arg in_ wrong type") }
        depth_0 <- listDepth(in_)
        v0 <- r_to_py(modify_depth(in_, depth_0, function(a) r_to_py(a)))
        private$py_obj$process29(v0)
        v0 <- py_to_r(v0)
        byref_0 <- map_depth(v0,depth_0,function(t) eval(parse(text = paste0(class_to_wrap(t),"$","new(t)"))))
    
        tryCatch({
        eval.parent(substitute(in_ <- byref_0))
        invisible()
        }, error = function(c) {invisible()}
        )
    
    },
    
    #' @description
    #' * C++ signature : \code{void process30(libcpp_vector[libcpp_vector[libcpp_vector[libcpp_vector[Int]]]] & in_)}
    #' @param in_ libcpp_vector[libcpp_vector[libcpp_vector[libcpp_vector[Int]]]] &
    process30 = function(in_){
    
        if(!(is.vector(in_) && all(sapply(in_,function(elemt_rec) is.vector(elemt_rec) && all(sapply(elemt_rec,function(elemt_rec_rec) is.vector(elemt_rec_rec) && all(sapply(elemt_rec_rec,function(elemt_rec_rec_rec) is.vector(elemt_rec_rec_rec) && all(sapply(elemt_rec_rec_rec,function(elemt_rec_rec_rec_rec) is.R6(elemt_rec_rec_rec_rec) && class(elemt_rec_rec_rec_rec)[1] == "Int")))))))))){ stop("arg in_ wrong type") }
        depth_0 <- listDepth(in_)
        v0 <- r_to_py(modify_depth(in_, depth_0, function(a) r_to_py(a)))
        private$py_obj$process30(v0)
        v0 <- py_to_r(v0)
        byref_0 <- map_depth(v0,depth_0,function(t) eval(parse(text = paste0(class_to_wrap(t),"$","new(t)"))))
    
        tryCatch({
        eval.parent(substitute(in_ <- byref_0))
        invisible()
        }, error = function(c) {invisible()}
        )
    
    },
    
    #' @description
    #' * C++ signature : \code{int process31(libcpp_vector[int] in_)}
    #' @param in_ libcpp_vector[int]
    process31 = function(in_){
    
        if(!(is.vector(in_) && all(sapply(in_,function(elemt_rec) (is.numeric(elemt_rec) && length(elemt_rec) == 1 && isTRUE(all.equal(elemt_rec,as.integer(elemt_rec)))))))){ stop("arg in_ wrong type") }
        v0 <- r_to_py(map_depth(in_,1,as.integer))
        py_ans = private$py_obj$process31(v0)
        
        r_ans = py_ans
        return(r_ans)
    },
    
    #' @description
    #' * C++ signature : \code{int process32(libcpp_vector[libcpp_vector[int]] in_)}
    #' @param in_ libcpp_vector[libcpp_vector[int]]
    process32 = function(in_){
    
        if(!(is.vector(in_) && all(sapply(in_,function(elemt_rec) is.vector(elemt_rec) && all(sapply(elemt_rec,function(elemt_rec_rec) (is.numeric(elemt_rec_rec) && length(elemt_rec_rec) == 1 && isTRUE(all.equal(elemt_rec_rec,as.integer(elemt_rec_rec)))))))))){ stop("arg in_ wrong type") }
        v0 <- r_to_py(map_depth(in_,2,as.integer))
        py_ans = private$py_obj$process32(v0)
        
        r_ans = py_ans
        return(r_ans)
    },
    
    #' @description
    #' * C++ signature : \code{int process33(shared_ptr[Int] in_)}
    #' @param in_ shared_ptr[Int]
    process33 = function(in_){
    
        if(!(all(class(in_) == c('Int','R6')))){ stop("arg in_ wrong type") }
        input_in_ <- r_to_py(in_)
        py_ans = private$py_obj$process33(input_in_)
        r_ans = py_ans
        return(r_ans)
    },
    
    #' @description
    #' * C++ signature : \code{shared_ptr[Int] process34(shared_ptr[Int] in_)}
    #' @param in_ shared_ptr[Int]
    process34 = function(in_){
    
        if(!(all(class(in_) == c('Int','R6')))){ stop("arg in_ wrong type") }
        input_in_ <- r_to_py(in_)
        py_ans = private$py_obj$process34(input_in_)
        r_ans = Int$new(py_ans)
        return(r_ans)
    },
    
    #' @description
    #' * C++ signature : \code{shared_ptr[const Int] process35(shared_ptr[Int] in_)}
    #' @param in_ shared_ptr[Int]
    process35 = function(in_){
    
        if(!(all(class(in_) == c('Int','R6')))){ stop("arg in_ wrong type") }
        input_in_ <- r_to_py(in_)
        py_ans = private$py_obj$process35(input_in_)
        r_ans = Int$new(py_ans)
        return(r_ans)
    },
    
    #' @description
    #' * C++ signature : \code{int process36(Int * in_)}
    #' @param in_ Int *
    process36 = function(in_){
    
        if(!(is.R6(in_) && class(in_)[1] == "Int")){ stop("arg in_ wrong type") }
    
        py_ans = private$py_obj$process36(r_to_py(in_))
        r_ans = py_ans
        return(r_ans)
    },
    
    #' @description
    #' * C++ signature : \code{Int * process37(Int * in_)}
    #' @param in_ Int *
    process37 = function(in_){
    
        if(!(is.R6(in_) && class(in_)[1] == "Int")){ stop("arg in_ wrong type") }
    
        py_ans = private$py_obj$process37(r_to_py(in_)) ; if( is.null(py_ans) ) { return(NULL) }
        r_ans = Int$new(py_ans)
        return(r_ans)
    },
    
    #' @description
    #' * C++ signature : \code{libcpp_vector[libcpp_vector[UInt]] process38(int)}
    #' @param in_0 int
    process38 = function(in_0){
    
        if(!((is.numeric(in_0) && length(in_0) == 1 && isTRUE(all.equal(in_0,as.integer(in_0)))))){ stop("arg in_0 wrong type") }
    
        py_ans = private$py_obj$process38(as.integer(in_0))
        r_ans <- py_ans
        return(r_ans)
    },
    
    #' @description
    #' * C++ signature : \code{const Int * process39(Int * in_)}
    #' @param in_ Int *
    process39 = function(in_){
    
        if(!(is.R6(in_) && class(in_)[1] == "Int")){ stop("arg in_ wrong type") }
    
        py_ans = private$py_obj$process39(r_to_py(in_)) ; if( is.null(py_ans) ) { return(NULL) }
        r_ans = Int$new(py_ans)
        return(r_ans)
    },
    
    #' @description
    #' * C++ signature : \code{int process40(ABS_Impl1 * in_)}
    #' @param in_ ABS_Impl1 *
    process40_0 = function(in_){
    
        if(!(is.R6(in_) && class(in_)[1] == "ABS_Impl1")){ stop("arg in_ wrong type") }
    
        py_ans = private$py_obj$`_process40_0`(r_to_py(in_))
        r_ans = py_ans
        return(r_ans)
    },
    
    #' @description
    #' * C++ signature : \code{int process40(ABS_Impl2 * in_)}
    #' @param in_ ABS_Impl2 *
    process40_1 = function(in_){
    
        if(!(is.R6(in_) && class(in_)[1] == "ABS_Impl2")){ stop("arg in_ wrong type") }
    
        py_ans = private$py_obj$`_process40_1`(r_to_py(in_))
        r_ans = py_ans
        return(r_ans)
    },
    
    #' @description process40
    #' This method calls one of the below:
    #' * \code{process40_0} : int process40(ABS_Impl1 * in_).
    #' * \code{process40_1} : int process40(ABS_Impl2 * in_).
    process40 = function(...){
        arg_list = list(...)
        if ((length(arg_list)==1) && (is.R6(arg_list[[1]]) && class(arg_list[[1]])[1] == "ABS_Impl1")) { self$process40_0(...) }
        else if ((length(arg_list)==1) && (is.R6(arg_list[[1]]) && class(arg_list[[1]])[1] == "ABS_Impl2")) { self$process40_1(...) }
        else {
              stop("wrong arguments provided")
        }
    
    }
)
)
LibCppTest$lock_class <- TRUE
 
#' @title EEE
#' @description
#' Returns object of class EEE with the following fields:
#' * A = 0L
#' * B = 1L
#' @examples
#' e <- EEE()
#' e$A
#' e$B
#' @export
EEE = function(){
    Pymod$EEE()
} 
