library(reticulate)
listDepth <- plotrix::listDepth
library(purrr)
check.numeric <- varhandle::check.numeric
library(R6)
Pymod <- import("py_libcpp_test")
py_run_string("import gc")
copy <- import("copy")
py_builtin <- import_builtins()

`r_to_py.R6` <- function(i,...){
   tryCatch({
       i$.__enclos_env__$private$py_obj
   }, error = function(e) { "Conversion not supported for this R6 Class"}
   )
}

cast_names_list <- function(i){
   if (all(check.numeric(i))) return(as.numeric(i))
   return(i)
}

class_to_wrap <- function(py_ob){
       strsplit(class(py_ob)[1],"\\.")[[1]][2]
}

py_run_string(paste("def transform_dict(d):","    return dict(zip([k.decode('utf-8') for k in d.keys()], list(d.values())))",sep = "\n")) 
    
# R implementation of _ABS_Impl1
ABS_Impl1 <- R6Class(classname = "ABS_Impl1",cloneable = FALSE,

    private = list(py_obj = NA),

    
    public = list(
    
    
    # C++ signature: void ABS_Impl1()
    init_0 = function(){
    
        stop("Cannot call this constructor!")
    
    },
    
    # C++ signature: void ABS_Impl1(int i)
    init_1 = function(i){
    
        if(!( (is_scalar_integer(i) || is_scalar_double(i)) && i == as.integer(i))){ stop("arg i wrong type") }
    
    
        private$py_obj <- Pymod$ABS_Impl1(as.integer(i))
        return(self)
    
    
    },
    
          # C++ signature: void ABS_Impl1()
          # C++ signature: void ABS_Impl1(int i)
    initialize = function(...){
        arg_list = list(...)
        if (length(arg_list)==0) { self$init_0(...) }
        else if ((length(arg_list)==1) && ( (is_scalar_integer(arg_list[[1]]) || is_scalar_double(arg_list[[1]])) && arg_list[[1]] == as.integer(arg_list[[1]]))) { self$init_1(...) }
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
    
    # C++ signature: int get()
    get = function(){
    
        py_ans = private$py_obj$get()
        r_ans = py_ans
        return(r_ans)
    }
    
        )
) 
    
# R implementation of _ABS_Impl2
ABS_Impl2 <- R6Class(classname = "ABS_Impl2",cloneable = FALSE,

    private = list(py_obj = NA),

    
    public = list(
    
    
    # C++ signature: void ABS_Impl2()
    init_0 = function(){
    
        stop("Cannot call this constructor!")
    
    },
    
    # C++ signature: void ABS_Impl2(int i)
    init_1 = function(i){
    
        if(!( (is_scalar_integer(i) || is_scalar_double(i)) && i == as.integer(i))){ stop("arg i wrong type") }
    
    
        private$py_obj <- Pymod$ABS_Impl2(as.integer(i))
        return(self)
    
    
    },
    
          # C++ signature: void ABS_Impl2()
          # C++ signature: void ABS_Impl2(int i)
    initialize = function(...){
        arg_list = list(...)
        if (length(arg_list)==0) { self$init_0(...) }
        else if ((length(arg_list)==1) && ( (is_scalar_integer(arg_list[[1]]) || is_scalar_double(arg_list[[1]])) && arg_list[[1]] == as.integer(arg_list[[1]]))) { self$init_1(...) }
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
    
    # C++ signature: int get()
    get = function(){
    
        py_ans = private$py_obj$get()
        r_ans = py_ans
        return(r_ans)
    }
    
        )
) 
    
# R implementation of _Int
Int <- R6Class(classname = "Int",cloneable = FALSE,

    private = list(py_obj = NA),


    active = list(
        i_ = function(i_){
            if(!missing(i_)){
                if(!( (is_scalar_integer(i_) || is_scalar_double(i_)) && i_ == as.integer(i_))){ stop("arg i_ wrong type") }
        
                   private$py_obj$i_ <- as.integer(i_)
            } else {
        
                py_ans = private$py_obj$i_
                r_result = py_ans
                return(r_result)
                }
        }

    ),
    
    public = list(
    
    
    # C++ signature: void Int(int i)
    init_0 = function(i){
    
        if(!( (is_scalar_integer(i) || is_scalar_double(i)) && i == as.integer(i))){ stop("arg i wrong type") }
    
    
        private$py_obj <- Pymod$Int(as.integer(i))
        return(self)
    
    
    },
    
    # C++ signature: void Int(Int & i)
    init_1 = function(i){
    
        if(!(is.R6(i) && class(i)[1] == "Int")){ stop("arg i wrong type") }
    
    
        private$py_obj <- Pymod$Int(i)
        return(self)
    
    
    },
    
          # C++ signature: void Int(int i)
          # C++ signature: void Int(Int & i)
    initialize = function(...){
        arg_list = list(...)
        if ((length(arg_list)==1) && ( (is_scalar_integer(arg_list[[1]]) || is_scalar_double(arg_list[[1]])) && arg_list[[1]] == as.integer(arg_list[[1]]))) { self$init_0(...) }
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
    
# R implementation of _LibCppTest

    # This is some class doc
    # Pretty cool stuff!
    # -----
    # With a trick, we can even get multiple paragraphs, allowing us to
    # write much longer documentation.
LibCppTest <- R6Class(classname = "LibCppTest",cloneable = FALSE,

    private = list(py_obj = NA),

    
    public = list(
    
    
    # C++ signature: void LibCppTest()
    init_0 = function(){
    
    
        private$py_obj <- Pymod$LibCppTest()
        return(self)
    
    
    },
    
    # C++ signature: void LibCppTest(int ii)
    init_1 = function(ii){
    
        if(!( (is_scalar_integer(ii) || is_scalar_double(ii)) && ii == as.integer(ii))){ stop("arg ii wrong type") }
    
    
        private$py_obj <- Pymod$LibCppTest(as.integer(ii))
        return(self)
    
    
    },
    
          # C++ signature: void LibCppTest()
          # C++ signature: void LibCppTest(int ii)
    initialize = function(...){
        arg_list = list(...)
        if (length(arg_list)==0) { self$init_0(...) }
        else if ((length(arg_list)==1) && ( (is_scalar_integer(arg_list[[1]]) || is_scalar_double(arg_list[[1]])) && arg_list[[1]] == as.integer(arg_list[[1]]))) { self$init_1(...) }
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
    
    # C++ signature: int gett()
# getting access to an integer
    gett = function(){
    
        py_ans = private$py_obj$gett()
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: libcpp_pair[int,libcpp_string] twist(libcpp_pair[libcpp_string,int])
# Dont forget this stuff here!
    twist = function(in_0){
    
        if(!(is_list(in_0) && length(in_0) == 2 && is_scalar_character(in_0[[1]]) &&  (is_scalar_integer(in_0[[2]]) || is_scalar_double(in_0[[2]])) && in_0[[2]] == as.integer(in_0[[2]]))){ stop("arg in_0 wrong type") }
        py$str0 <- in_0[[1]]
        py_run_string("str0 = bytes(str0,'utf-8')")
        v0 = r_to_py(list(py$str0,as.integer(in_0[[2]])))
        py_ans = private$py_obj$twist(v0)
        py_run_string("del str0")
        py_run_string("gc.collect()")
        r_ans = list(py_ans[[1]], as.character(py_ans[[2]]))
        return(r_ans)
    },
    
    # C++ signature: libcpp_vector[int] process(libcpp_vector[int] &)
    process = function(in_0){
    
        if(!(is_list(in_0) && all(sapply(in_0,function(elemt_rec)  (is_scalar_integer(elemt_rec) || is_scalar_double(elemt_rec)) && elemt_rec == as.integer(elemt_rec))))){ stop("arg in_0 wrong type") }
        v0 <- r_to_py(modify_depth(in_0,1,as.integer))
        py_ans = private$py_obj$process(v0)
        byref_0 <- map_depth(py_to_r(v0),0,as.list)
        r_ans <- map(py_ans,as.integer)
    
        tryCatch({
        eval.parent(substitute(in_0 <- byref_0))
        return(r_ans)
        }, error = function(c) {return(r_ans)}
        )
    
    },
    
    # C++ signature: libcpp_pair[int,int] process2(libcpp_pair[int,int] &)
    process2 = function(in_0){
    
        if(!(is_list(in_0) && length(in_0) == 2 &&  (is_scalar_integer(in_0[[1]]) || is_scalar_double(in_0[[1]])) && in_0[[1]] == as.integer(in_0[[1]]) &&  (is_scalar_integer(in_0[[2]]) || is_scalar_double(in_0[[2]])) && in_0[[2]] == as.integer(in_0[[2]]))){ stop("arg in_0 wrong type") }
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
    
    # C++ signature: libcpp_pair[LibCppTest,int] process3(libcpp_pair[LibCppTest,int] &)
    process3 = function(in_0){
    
        if(!(is_list(in_0) && length(in_0) == 2 && is.R6(in_0[[1]]) && class(in_0[[1]])[1] == "LibCppTest" &&  (is_scalar_integer(in_0[[2]]) || is_scalar_double(in_0[[2]])) && in_0[[2]] == as.integer(in_0[[2]]))){ stop("arg in_0 wrong type") }
        v0 = r_to_py(list(in_0[[1]]$.__enclos_env__$private$py_obj,as.integer(in_0[[2]])))
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
    
    # C++ signature: libcpp_pair[int,LibCppTest] process4(libcpp_pair[int,LibCppTest] &)
    process4 = function(in_0){
    
        if(!(is_list(in_0) && length(in_0) == 2 &&  (is_scalar_integer(in_0[[1]]) || is_scalar_double(in_0[[1]])) && in_0[[1]] == as.integer(in_0[[1]]) && is.R6(in_0[[2]]) && class(in_0[[2]])[1] == "LibCppTest")){ stop("arg in_0 wrong type") }
        v0 = r_to_py(list(as.integer(in_0[[1]]),in_0[[2]]$.__enclos_env__$private$py_obj))
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
    
    # C++ signature: libcpp_pair[LibCppTest,LibCppTest] process5(libcpp_pair[LibCppTest,LibCppTest] &)
    process5 = function(in_0){
    
        if(!(is_list(in_0) && length(in_0) == 2 && is.R6(in_0[[1]]) && class(in_0[[1]])[1] == "LibCppTest" && is.R6(in_0[[2]]) && class(in_0[[2]])[1] == "LibCppTest")){ stop("arg in_0 wrong type") }
        v0 = r_to_py(list(in_0[[1]]$.__enclos_env__$private$py_obj,in_0[[2]]$.__enclos_env__$private$py_obj))
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
    
    # C++ signature: libcpp_vector[libcpp_pair[int,double]] process6(libcpp_vector[libcpp_pair[int,double]] &)
    process6 = function(in_0){
    
        if(!(is_list(in_0) && all(sapply(in_0,function(elemt_rec) is_list(elemt_rec) && length(elemt_rec) == 2 &&  (is_scalar_integer(elemt_rec[[1]]) || is_scalar_double(elemt_rec[[1]])) && elemt_rec[[1]] == as.integer(elemt_rec[[1]]) && is_scalar_double(elemt_rec[[2]]))))){ stop("arg in_0 wrong type") }
        v0 <- r_to_py(map_depth(in_0,1, function(a) list(as.integer(a[[1]]),a[[2]])))
        py_ans = private$py_obj$process6(v0)
        byref_0 <- py_to_r(v0)
        r_ans <- py_ans
    
        tryCatch({
        eval.parent(substitute(in_0 <- byref_0))
        return(r_ans)
        }, error = function(c) {return(r_ans)}
        )
    
    },
    
    # C++ signature: libcpp_pair[int,EEE] process7(libcpp_pair[EEE,int] &)
    process7 = function(in_0){
    
        if(!(is_list(in_0) && length(in_0) == 2 && in_0[[1]] %in% c(0, 1) &&  (is_scalar_integer(in_0[[2]]) || is_scalar_double(in_0[[2]])) && in_0[[2]] == as.integer(in_0[[2]]))){ stop("arg in_0 wrong type") }
        v0 = r_to_py(list(in_0[[1]],as.integer(in_0[[2]])))
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
    
    # C++ signature: libcpp_vector[EEE] process8(libcpp_vector[EEE] &)
    process8 = function(in_0){
    
        if(!(is_list(in_0) && all(sapply(in_0,function(elemt_rec) elemt_rec %in% c(0, 1))))){ stop("arg in_0 wrong type") }
        v0 <- r_to_py(map(in_0,as.integer))
        py_ans = private$py_obj$process8(v0)
        byref_0 <- as.list(py_to_r(v0))
        r_ans <- as.list(py_ans)
    
        tryCatch({
        eval.parent(substitute(in_0 <- byref_0))
        return(r_ans)
        }, error = function(c) {return(r_ans)}
        )
    
    },
    
    # C++ signature: libcpp_set[int] process9(libcpp_set[int] &)
    process9 = function(in_0){
    
        if(!(is_list(in_0) && all(sapply(in_0,function(el)  (is_scalar_integer(el) || is_scalar_double(el)) && el == as.integer(el))) && !(TRUE %in% duplicated(in_0)))){ stop("arg in_0 wrong type") }
        py$v0 <- in_0
        py_run_string("v0 = [int(t) for t in v0];v0 = set(v0)")
        py_ans = private$py_obj$process9(py$v0)
        byref_0 <- as.list(py_eval("list(v0)"))
        py_run_string("del v0; gc.collect()")
        py$res <- py_ans
        r_ans = as.list(py_eval("list(res)"))
        py_run_string("del res;gc.collect()")
    
        tryCatch({
        eval.parent(substitute(in_0 <- byref_0))
        return(r_ans)
        }, error = function(c) {return(r_ans)}
        )
    
    },
    
    # C++ signature: libcpp_set[EEE] process10(libcpp_set[EEE] &)
    process10 = function(in_0){
    
        if(!(is_list(in_0) && all(sapply(in_0,function(el) el %in% c(0, 1))) && !(TRUE %in% duplicated(in_0)))){ stop("arg in_0 wrong type") }
        py$v0 <- in_0
        py_run_string("v0 = [int(t) for t in v0];v0 = set(v0)")
        py_ans = private$py_obj$process10(py$v0)
        byref_0 <- as.list(py_eval("list(v0)"))
        py_run_string("del v0; gc.collect()")
        py$res <- py_ans
        r_ans = as.list(py_eval("list(res)"))
        py_run_string("del res;gc.collect()")
    
        tryCatch({
        eval.parent(substitute(in_0 <- byref_0))
        return(r_ans)
        }, error = function(c) {return(r_ans)}
        )
    
    },
    
    # C++ signature: libcpp_set[LibCppTest] process11(libcpp_set[LibCppTest] &)
    process11 = function(in_0){
    
        if(!(is_list(in_0) && all(sapply(in_0,function(el) is.R6(el) && class(el)[1] == "LibCppTest")) && length(in_0) == py_builtin$len(py_builtin$set(r_to_py(in_0))))){ stop("arg in_0 wrong type") }
        py$v0 <- lapply(in_0,function(item0) item0$.__enclos_env__$private$py_obj)
        py_run_string("v0 = set(v0)")
        py_ans = private$py_obj$process11(py$v0)
        byref_0 <- py_eval("list(v0)")
        byref_0 <- lapply(byref_0,function(x) LibCppTest$new(x))
        py_run_string("del v0; gc.collect()")
        py$res <- py_ans
        r_ans = py_eval("list(res)")
        r_ans <- lapply(r_ans, function(x) LibCppTest$new(x))
        py_run_string("del res;gc.collect()")
    
        tryCatch({
        eval.parent(substitute(in_0 <- byref_0))
        return(r_ans)
        }, error = function(c) {return(r_ans)}
        )
    
    },
    
    # C++ signature: libcpp_map[int,float] process12(int i, float f)
    process12 = function(i, f){
    
        if(!( (is_scalar_integer(i) || is_scalar_double(i)) && i == as.integer(i))){ stop("arg i wrong type") }
        if(!(is_scalar_double(f))){ stop("arg f wrong type") }
    
    
        py_ans = private$py_obj$process12(as.integer(i), f)
        r_ans <- py_ans
        return(r_ans)
    },
    
    # C++ signature: libcpp_map[EEE,int] process13(EEE e, int i)
    process13 = function(e, i){
    
        if(!(e %in% c(0, 1))){ stop("arg e wrong type") }
        if(!( (is_scalar_integer(i) || is_scalar_double(i)) && i == as.integer(i))){ stop("arg i wrong type") }
    
    
        py_ans = private$py_obj$process13(e, as.integer(i))
        r_ans <- py_ans
        return(r_ans)
    },
    
    # C++ signature: libcpp_map[int,EEE] process14(EEE e, int i)
    process14 = function(e, i){
    
        if(!(e %in% c(0, 1))){ stop("arg e wrong type") }
        if(!( (is_scalar_integer(i) || is_scalar_double(i)) && i == as.integer(i))){ stop("arg i wrong type") }
    
    
        py_ans = private$py_obj$process14(e, as.integer(i))
        r_ans <- py_ans
        return(r_ans)
    },
    
    # C++ signature: libcpp_map[long int,LibCppTest] process15(int ii)
    process15 = function(ii){
    
        if(!( (is_scalar_integer(ii) || is_scalar_double(ii)) && ii == as.integer(ii))){ stop("arg ii wrong type") }
    
        py_ans = private$py_obj$process15(as.integer(ii))
        r_ans <- lapply(py_ans, function(i) eval(parse(text = paste0(class_to_wrap(i),"$","new(i)"))))
        return(r_ans)
    },
    
    # C++ signature: float process16(libcpp_map[int,float] in_)
    process16 = function(in_){
    
        if(!(is_list(in_) && ifelse(length(in_)==0,TRUE,!is.null(names(in_)) && all(sapply( as.numeric(names(in_)),function(k)  (is_scalar_integer(k) || is_scalar_double(k)) && k == as.integer(k))) && all(sapply(in_,function(v) is_scalar_double(v))) && length(unique(names(in_))) == length(names(in_))))){ stop("arg in_ wrong type") }
        v0 <- py_dict(map(names(in_),as.integer),unname(in_))
        py_ans = private$py_obj$process16(v0)
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: float process17(libcpp_map[EEE,float] in_)
    process17 = function(in_){
    
        if(!(is_list(in_) && ifelse(length(in_)==0,TRUE,!is.null(names(in_)) && all(sapply( as.numeric(names(in_)),function(k) k %in% c(0, 1))) && all(sapply(in_,function(v) is_scalar_double(v))) && length(unique(names(in_))) == length(names(in_))))){ stop("arg in_ wrong type") }
        v0 <- py_dict(map(names(in_),as.integer),unname(in_))
        py_ans = private$py_obj$process17(v0)
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: int process18(libcpp_map[int,LibCppTest] in_)
    process18 = function(in_){
    
        if(!(is_list(in_) && ifelse(length(in_)==0,TRUE,!is.null(names(in_)) && all(sapply( as.numeric(names(in_)),function(k)  (is_scalar_integer(k) || is_scalar_double(k)) && k == as.integer(k))) && all(sapply(in_,function(v) is.R6(v) && class(v)[1] == "LibCppTest")) && length(unique(names(in_))) == length(names(in_))))){ stop("arg in_ wrong type") }
        v0 <- py_dict(map(names(in_),as.integer),unname(in_))
        py_ans = private$py_obj$process18(v0)
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: void process19(libcpp_map[int,LibCppTest] & in_)
    process19 = function(in_){
    
        if(!(is_list(in_) && ifelse(length(in_)==0,TRUE,!is.null(names(in_)) && all(sapply( as.numeric(names(in_)),function(k)  (is_scalar_integer(k) || is_scalar_double(k)) && k == as.integer(k))) && all(sapply(in_,function(v) is.R6(v) && class(v)[1] == "LibCppTest")) && length(unique(names(in_))) == length(names(in_))))){ stop("arg in_ wrong type") }
        v0 <- py_dict(map(names(in_),as.integer),unname(in_))
        private$py_obj$process19(v0)
        v0 <- lapply(py_to_r(v0), function(i) eval(parse(text = paste0(class_to_wrap(i),"$","new(i)"))))
        byref_0 <- v0
    
        tryCatch({
        eval.parent(substitute(in_ <- byref_0))
        invisible(NULL)
        }, error = function(c) {invisible(NULL)}
        )
    
    },
    
    # C++ signature: void process20(libcpp_map[int,float] & in_)
    process20 = function(in_){
    
        if(!(is_list(in_) && ifelse(length(in_)==0,TRUE,!is.null(names(in_)) && all(sapply( as.numeric(names(in_)),function(k)  (is_scalar_integer(k) || is_scalar_double(k)) && k == as.integer(k))) && all(sapply(in_,function(v) is_scalar_double(v))) && length(unique(names(in_))) == length(names(in_))))){ stop("arg in_ wrong type") }
        v0 <- py_dict(map(names(in_),as.integer),unname(in_))
        private$py_obj$process20(v0)
        byref_0 <- py_to_r(v0)
    
        tryCatch({
        eval.parent(substitute(in_ <- byref_0))
        invisible(NULL)
        }, error = function(c) {invisible(NULL)}
        )
    
    },
    
    # C++ signature: void process21(libcpp_map[int,float] & in_, libcpp_map[int,int] & arg2)
    process21 = function(in_, arg2){
    
        if(!(is_list(in_) && ifelse(length(in_)==0,TRUE,!is.null(names(in_)) && all(sapply( as.numeric(names(in_)),function(k)  (is_scalar_integer(k) || is_scalar_double(k)) && k == as.integer(k))) && all(sapply(in_,function(v) is_scalar_double(v))) && length(unique(names(in_))) == length(names(in_))))){ stop("arg in_ wrong type") }
        if(!(is_list(arg2) && ifelse(length(arg2)==0,TRUE,!is.null(names(arg2)) && all(sapply( as.numeric(names(arg2)),function(k)  (is_scalar_integer(k) || is_scalar_double(k)) && k == as.integer(k))) && all(sapply(arg2,function(v)  (is_scalar_integer(v) || is_scalar_double(v)) && v == as.integer(v))) && length(unique(names(arg2))) == length(names(arg2))))){ stop("arg arg2 wrong type") }
        v0 <- py_dict(map(names(in_),as.integer),unname(in_))
        v1 <- py_dict(map(names(arg2),as.integer),as.integer(unname(arg2)))
        private$py_obj$process21(v0, v1)
        byref_1 <- py_to_r(v1)
        byref_0 <- py_to_r(v0)
    
        tryCatch({
        eval.parent(substitute(in_ <- byref_0))
        eval.parent(substitute(arg2 <- byref_1))
        invisible(NULL)
        }, error = function(c) {invisible(NULL)}
        )
    
    },
    
    # C++ signature: void process211(libcpp_map[int,float] & in_, libcpp_map[libcpp_string,libcpp_vector[int]] & arg2)
    process211 = function(in_, arg2){
    
        if(!(is_list(in_) && ifelse(length(in_)==0,TRUE,!is.null(names(in_)) && all(sapply( as.numeric(names(in_)),function(k)  (is_scalar_integer(k) || is_scalar_double(k)) && k == as.integer(k))) && all(sapply(in_,function(v) is_scalar_double(v))) && length(unique(names(in_))) == length(names(in_))))){ stop("arg in_ wrong type") }
        if(!(is_list(arg2) && ifelse(length(arg2)==0,TRUE,!is.null(names(arg2)) && all(sapply(arg2,function(v) is_list(v) && all(sapply(v,function(elemt_rec)  (is_scalar_integer(elemt_rec) || is_scalar_double(elemt_rec)) && elemt_rec == as.integer(elemt_rec))))) && length(unique(names(arg2))) == length(names(arg2))))){ stop("arg arg2 wrong type") }
        v0 <- py_dict(map(names(in_),as.integer),unname(in_))
        v1 <- py_dict(map(names(arg2),function(a) py_builtin$bytes(a,'utf-8')),modify_depth(unname(arg2),2,as.integer))
        private$py_obj$process211(v0, v1)
        v1 <- py$transform_dict(v1)
        byref_1 <- map_depth(v1,2,as.list)
        byref_0 <- py_to_r(v0)
    
        tryCatch({
        eval.parent(substitute(in_ <- byref_0))
        eval.parent(substitute(arg2 <- byref_1))
        invisible(NULL)
        }, error = function(c) {invisible(NULL)}
        )
    
    },
    
    # C++ signature: void process212(libcpp_map[int,float] & in_, libcpp_map[libcpp_string,libcpp_vector[libcpp_vector[int]]] & arg2)
    process212 = function(in_, arg2){
    
        if(!(is_list(in_) && ifelse(length(in_)==0,TRUE,!is.null(names(in_)) && all(sapply( as.numeric(names(in_)),function(k)  (is_scalar_integer(k) || is_scalar_double(k)) && k == as.integer(k))) && all(sapply(in_,function(v) is_scalar_double(v))) && length(unique(names(in_))) == length(names(in_))))){ stop("arg in_ wrong type") }
        if(!(is_list(arg2) && ifelse(length(arg2)==0,TRUE,!is.null(names(arg2)) && all(sapply(arg2,function(v) is_list(v) && all(sapply(v,function(elemt_rec) is_list(elemt_rec) && all(sapply(elemt_rec,function(elemt_rec_rec)  (is_scalar_integer(elemt_rec_rec) || is_scalar_double(elemt_rec_rec)) && elemt_rec_rec == as.integer(elemt_rec_rec))))))) && length(unique(names(arg2))) == length(names(arg2))))){ stop("arg arg2 wrong type") }
        v0 <- py_dict(map(names(in_),as.integer),unname(in_))
        v1 <- py_dict(map(names(arg2),function(a) py_builtin$bytes(a,'utf-8')),modify_depth(unname(arg2),3,as.integer))
        private$py_obj$process212(v0, v1)
        v1 <- py$transform_dict(v1)
        byref_1 <- map_depth(v1,3,as.list)
        byref_0 <- py_to_r(v0)
    
        tryCatch({
        eval.parent(substitute(in_ <- byref_0))
        eval.parent(substitute(arg2 <- byref_1))
        invisible(NULL)
        }, error = function(c) {invisible(NULL)}
        )
    
    },
    
    # C++ signature: void process214(libcpp_map[int,float] & in_, libcpp_map[libcpp_string,libcpp_vector[libcpp_pair[int,int]]] & arg2)
    process214 = function(in_, arg2){
    
        if(!(is_list(in_) && ifelse(length(in_)==0,TRUE,!is.null(names(in_)) && all(sapply( as.numeric(names(in_)),function(k)  (is_scalar_integer(k) || is_scalar_double(k)) && k == as.integer(k))) && all(sapply(in_,function(v) is_scalar_double(v))) && length(unique(names(in_))) == length(names(in_))))){ stop("arg in_ wrong type") }
        if(!(is_list(arg2) && ifelse(length(arg2)==0,TRUE,!is.null(names(arg2)) && all(sapply(arg2,function(v) is_list(v) && all(sapply(v,function(elemt_rec) is_list(elemt_rec) && length(elemt_rec) == 2 &&  (is_scalar_integer(elemt_rec[[1]]) || is_scalar_double(elemt_rec[[1]])) && elemt_rec[[1]] == as.integer(elemt_rec[[1]]) &&  (is_scalar_integer(elemt_rec[[2]]) || is_scalar_double(elemt_rec[[2]])) && elemt_rec[[2]] == as.integer(elemt_rec[[2]]))))) && length(unique(names(arg2))) == length(names(arg2))))){ stop("arg arg2 wrong type") }
        v0 <- py_dict(map(names(in_),as.integer),unname(in_))
        v1 <- py_dict(map(names(arg2),function(a) py_builtin$bytes(a,'utf-8')),map_depth(unname(arg2),2,function(a) list(as.integer(a[[1]]),as.integer(a[[2]]))))
        private$py_obj$process214(v0, v1)
        v1 <- py$transform_dict(v1)
        byref_1 <- map_depth(v1,2,as.list)
        byref_0 <- py_to_r(v0)
    
        tryCatch({
        eval.parent(substitute(in_ <- byref_0))
        eval.parent(substitute(arg2 <- byref_1))
        invisible(NULL)
        }, error = function(c) {invisible(NULL)}
        )
    
    },
    
    # C++ signature: void process22(libcpp_set[int] &, libcpp_set[float] &)
    process22 = function(in_0, in_1){
    
        if(!(is_list(in_0) && all(sapply(in_0,function(el)  (is_scalar_integer(el) || is_scalar_double(el)) && el == as.integer(el))) && !(TRUE %in% duplicated(in_0)))){ stop("arg in_0 wrong type") }
        if(!(is_list(in_1) && all(sapply(in_1,function(el) is_scalar_double(el))) && !(TRUE %in% duplicated(in_1)))){ stop("arg in_1 wrong type") }
        py$v0 <- in_0
        py_run_string("v0 = [int(t) for t in v0];v0 = set(v0)")
        py$v1 <- in_1
        py_run_string("v1 = set(v1)")
        private$py_obj$process22(py$v0, py$v1)
        byref_1 <- as.list(py_eval("list(v1)"))
        py_run_string("del v1; gc.collect()")
        byref_0 <- as.list(py_eval("list(v0)"))
        py_run_string("del v0; gc.collect()")
    
        tryCatch({
        eval.parent(substitute(in_0 <- byref_0))
        eval.parent(substitute(in_1 <- byref_1))
        invisible(NULL)
        }, error = function(c) {invisible(NULL)}
        )
    
    },
    
    # C++ signature: void process23(libcpp_vector[int] &, libcpp_vector[float] &)
    process23 = function(in_0, in_1){
    
        if(!(is_list(in_0) && all(sapply(in_0,function(elemt_rec)  (is_scalar_integer(elemt_rec) || is_scalar_double(elemt_rec)) && elemt_rec == as.integer(elemt_rec))))){ stop("arg in_0 wrong type") }
        if(!(is_list(in_1) && all(sapply(in_1,function(elemt_rec) is_scalar_double(elemt_rec))))){ stop("arg in_1 wrong type") }
        v0 <- r_to_py(modify_depth(in_0,1,as.integer))
        v1 <- r_to_py(in_1)
        private$py_obj$process23(v0, v1)
        byref_1 <- map_depth(py_to_r(v1),0,as.list)
        byref_0 <- map_depth(py_to_r(v0),0,as.list)
    
        tryCatch({
        eval.parent(substitute(in_0 <- byref_0))
        eval.parent(substitute(in_1 <- byref_1))
        invisible(NULL)
        }, error = function(c) {invisible(NULL)}
        )
    
    },
    
    # C++ signature: void process24(libcpp_pair[int,float] & in_, libcpp_pair[int,int] & arg2)
    process24 = function(in_, arg2){
    
        if(!(is_list(in_) && length(in_) == 2 &&  (is_scalar_integer(in_[[1]]) || is_scalar_double(in_[[1]])) && in_[[1]] == as.integer(in_[[1]]) && is_scalar_double(in_[[2]]))){ stop("arg in_ wrong type") }
        if(!(is_list(arg2) && length(arg2) == 2 &&  (is_scalar_integer(arg2[[1]]) || is_scalar_double(arg2[[1]])) && arg2[[1]] == as.integer(arg2[[1]]) &&  (is_scalar_integer(arg2[[2]]) || is_scalar_double(arg2[[2]])) && arg2[[2]] == as.integer(arg2[[2]]))){ stop("arg arg2 wrong type") }
        v0 = r_to_py(list(as.integer(in_[[1]]),in_[[2]]))
        v1 = r_to_py(list(as.integer(arg2[[1]]),as.integer(arg2[[2]])))
        private$py_obj$process24(v0, v1)
        byref_1 = list(py_to_r(v1[0]), py_to_r(v1[1]))
        byref_0 = list(py_to_r(v0[0]), py_to_r(v0[1]))
    
        tryCatch({
        eval.parent(substitute(in_ <- byref_0))
        eval.parent(substitute(arg2 <- byref_1))
        invisible(NULL)
        }, error = function(c) {invisible(NULL)}
        )
    
    },
    
    # C++ signature: int process25(libcpp_vector[Int] in_)
    process25 = function(in_){
    
        if(!(is_list(in_) && all(sapply(in_,function(elemt_rec) is.R6(elemt_rec) && class(elemt_rec)[1] == "Int")))){ stop("arg in_ wrong type") }
        v0 <- r_to_py(in_)
        py_ans = private$py_obj$process25(v0)
        
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: int process26(libcpp_vector[libcpp_vector[Int]] in_)
    process26 = function(in_){
    
        if(!(is_list(in_) && all(sapply(in_,function(elemt_rec) is_list(elemt_rec) && all(sapply(elemt_rec,function(elemt_rec_rec) is.R6(elemt_rec_rec) && class(elemt_rec_rec)[1] == "Int")))))){ stop("arg in_ wrong type") }
        v0 <- r_to_py(in_)
        py_ans = private$py_obj$process26(v0)
        
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: int process27(libcpp_vector[libcpp_vector[libcpp_vector[Int]]] in_)
    process27 = function(in_){
    
        if(!(is_list(in_) && all(sapply(in_,function(elemt_rec) is_list(elemt_rec) && all(sapply(elemt_rec,function(elemt_rec_rec) is_list(elemt_rec_rec) && all(sapply(elemt_rec_rec,function(elemt_rec_rec_rec) is.R6(elemt_rec_rec_rec) && class(elemt_rec_rec_rec)[1] == "Int")))))))){ stop("arg in_ wrong type") }
        v0 <- r_to_py(in_)
        py_ans = private$py_obj$process27(v0)
        
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: int process28(libcpp_vector[libcpp_vector[libcpp_vector[libcpp_vector[Int]]]] in_)
    process28 = function(in_){
    
        if(!(is_list(in_) && all(sapply(in_,function(elemt_rec) is_list(elemt_rec) && all(sapply(elemt_rec,function(elemt_rec_rec) is_list(elemt_rec_rec) && all(sapply(elemt_rec_rec,function(elemt_rec_rec_rec) is_list(elemt_rec_rec_rec) && all(sapply(elemt_rec_rec_rec,function(elemt_rec_rec_rec_rec) is.R6(elemt_rec_rec_rec_rec) && class(elemt_rec_rec_rec_rec)[1] == "Int")))))))))){ stop("arg in_ wrong type") }
        v0 <- r_to_py(in_)
        py_ans = private$py_obj$process28(v0)
        
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: void process29(libcpp_vector[libcpp_vector[Int]] & in_)
    process29 = function(in_){
    
        if(!(is_list(in_) && all(sapply(in_,function(elemt_rec) is_list(elemt_rec) && all(sapply(elemt_rec,function(elemt_rec_rec) is.R6(elemt_rec_rec) && class(elemt_rec_rec)[1] == "Int")))))){ stop("arg in_ wrong type") }
        v0 <- r_to_py(in_)
        private$py_obj$process29(v0)
        v0 <- py_to_r(v0)
        byref_0 <- map_depth(v0,listDepth(v0),function(t) eval(parse(text = paste0(class_to_wrap(t),"$","new(t)"))))
    
        tryCatch({
        eval.parent(substitute(in_ <- byref_0))
        invisible(NULL)
        }, error = function(c) {invisible(NULL)}
        )
    
    },
    
    # C++ signature: void process30(libcpp_vector[libcpp_vector[libcpp_vector[libcpp_vector[Int]]]] & in_)
    process30 = function(in_){
    
        if(!(is_list(in_) && all(sapply(in_,function(elemt_rec) is_list(elemt_rec) && all(sapply(elemt_rec,function(elemt_rec_rec) is_list(elemt_rec_rec) && all(sapply(elemt_rec_rec,function(elemt_rec_rec_rec) is_list(elemt_rec_rec_rec) && all(sapply(elemt_rec_rec_rec,function(elemt_rec_rec_rec_rec) is.R6(elemt_rec_rec_rec_rec) && class(elemt_rec_rec_rec_rec)[1] == "Int")))))))))){ stop("arg in_ wrong type") }
        v0 <- r_to_py(in_)
        private$py_obj$process30(v0)
        v0 <- py_to_r(v0)
        byref_0 <- map_depth(v0,listDepth(v0),function(t) eval(parse(text = paste0(class_to_wrap(t),"$","new(t)"))))
    
        tryCatch({
        eval.parent(substitute(in_ <- byref_0))
        invisible(NULL)
        }, error = function(c) {invisible(NULL)}
        )
    
    },
    
    # C++ signature: int process31(libcpp_vector[int] in_)
    process31 = function(in_){
    
        if(!(is_list(in_) && all(sapply(in_,function(elemt_rec)  (is_scalar_integer(elemt_rec) || is_scalar_double(elemt_rec)) && elemt_rec == as.integer(elemt_rec))))){ stop("arg in_ wrong type") }
        v0 <- r_to_py(modify_depth(in_,1,as.integer))
        py_ans = private$py_obj$process31(v0)
        
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: int process32(libcpp_vector[libcpp_vector[int]] in_)
    process32 = function(in_){
    
        if(!(is_list(in_) && all(sapply(in_,function(elemt_rec) is_list(elemt_rec) && all(sapply(elemt_rec,function(elemt_rec_rec)  (is_scalar_integer(elemt_rec_rec) || is_scalar_double(elemt_rec_rec)) && elemt_rec_rec == as.integer(elemt_rec_rec))))))){ stop("arg in_ wrong type") }
        v0 <- r_to_py(modify_depth(in_,2,as.integer))
        py_ans = private$py_obj$process32(v0)
        
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: int process33(shared_ptr[Int] in_)
    process33 = function(in_){
    
        if(!(all(class(in_) == c('Int','R6')))){ stop("arg in_ wrong type") }
        input_in_ <- r_to_py(in_)
        py_ans = private$py_obj$process33(in_)
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: shared_ptr[Int] process34(shared_ptr[Int] in_)
    process34 = function(in_){
    
        if(!(all(class(in_) == c('Int','R6')))){ stop("arg in_ wrong type") }
        input_in_ <- r_to_py(in_)
        py_ans = private$py_obj$process34(in_)
        r_ans = Int$new(py_ans)
        return(r_ans)
    },
    
    # C++ signature: shared_ptr[const Int] process35(shared_ptr[Int] in_)
    process35 = function(in_){
    
        if(!(all(class(in_) == c('Int','R6')))){ stop("arg in_ wrong type") }
        input_in_ <- r_to_py(in_)
        py_ans = private$py_obj$process35(in_)
        r_ans = Int$new(py_ans)
        return(r_ans)
    },
    
    # C++ signature: int process36(Int * in_)
    process36 = function(in_){
    
        if(!(is.R6(in_) && class(in_)[1] == "Int")){ stop("arg in_ wrong type") }
    
        py_ans = private$py_obj$process36(in_)
        r_ans = py_ans
        return(r_ans)
    },
    
    # C++ signature: Int * process37(Int * in_)
    process37 = function(in_){
    
        if(!(is.R6(in_) && class(in_)[1] == "Int")){ stop("arg in_ wrong type") }
    
        py_ans = private$py_obj$process37(in_)
        if( is.null(py_ans) ) {
            return(NULL)
        }
        r_ans = Int$new(py_ans)
        return(r_ans)
    },
    
    # C++ signature: libcpp_vector[libcpp_vector[UInt]] process38(int)
    process38 = function(in_0){
    
        if(!( (is_scalar_integer(in_0) || is_scalar_double(in_0)) && in_0 == as.integer(in_0))){ stop("arg in_0 wrong type") }
    
        py_ans = private$py_obj$process38(as.integer(in_0))
        r_ans <- py_ans
        return(r_ans)
    }
    
        )
) 

EEE = R6Class(classname = "EEE", cloneable = FALSE,

        public = list(

        A = 0L,
        B = 1L,

        getMapping = function() {
            return( Pymod$EEE()$getMapping() )
        }
    )
) 
