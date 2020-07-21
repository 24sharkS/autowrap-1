context("Testing R code generated for libcpp_test")
source("./test_files/py_libcpp_test.R")

test_that("Test Copy constructors",{
  
      # create new Int, make copy (shallow)
      int_wrp <- Int$new(1)
      expect_identical(int_wrp$i_,1L)
      int_wrpcpy <- int_wrp
      expect_identical(int_wrpcpy$i_,1L)
      
      # changing one should change the other
      int_wrpcpy$i_ <- 2
      expect_identical(int_wrp$i_,2L)
      expect_identical(int_wrpcpy$i_,2L)
      
      # Make real copy (deepcopy)
      int_wrp3 <- Int$new(int_wrp$.__enclos_env__$private$py_obj)
      int_wrp3$i_ <- 5
      expect_identical(int_wrp3$i_,5L)
      expect_identical(int_wrp$i_,2L)
      expect_identical(int_wrpcpy$i_,2L)
      
      # Make real copy using copy constructor
      int_wrp4 <- Int$new(int_wrp)
      int_wrp4$i_ <- 6
      expect_identical(int_wrp4$i_,6L)
      expect_identical(int_wrp$i_,2L)
      expect_identical(int_wrpcpy$i_,2L)
      
      # changing one should change the other
      int_wrpcpy$i_ <- 1
      expect_identical(int_wrp$i_,1L)
      expect_identical(int_wrpcpy$i_,1L)
      
      
    }
  )

test_that("Testing LibCppTest Processes",{
    
      # creating a new object of LibCppTest
      t <- LibCppTest$new()
      
      expect_equivalent(t$twist(list("hi",2)),list(2L,"hi"))

      # process
      li = list(1)
      li2 <- t$process(li)
      expect_equivalent(li,li2)
      expect_equivalent(li,list(1L,42L))

      in1 <- as.list(1:2)
      out <- t$process2(in1)
      expect_identical(in1,out)
      expect_equivalent(in1,list(42L,11L))
      
      in1 <- list(t,1)
      out <- t$process3(in1)
      expect_identical(in1[[1]]$gett(),0L)
      expect_identical(in1[[2]],42L)
      expect_identical(out[[1]]$gett(),0L)
      expect_identical(out[[2]],42L)
      
      in1 <- list(1,t)
      out <- t$process4(in1)
      
      expect_identical(in1[[1]],42L)
      expect_identical(in1[[2]]$gett(),0L)
      expect_identical(out[[1]],42L)
      expect_identical(out[[2]]$gett(),0L)
      
      t2 <- LibCppTest$new(12)
      in1 <- list(t,t2)
      out <- t$process5(in1)
      expect_identical(in1[[1]]$gett(),43L)
      expect_identical(in1[[2]]$gett(),12L)
      expect_identical(out[[1]]$gett(),12L)
      expect_identical(out[[2]]$gett(),43L)

      # process 6
      in1 <- list(list(1L,2.0),list(2L,3.0))
      out <- t$process6(in1)
      expect_equal(in1,list(list(1, 2.0), list(2, 3.0), list(7, 11.0)))
      expect_equivalent(out,rev(list(list(1, 2.0), list(2, 3.0), list(7, 11.0))))
      
      out = t$process7(as.list(0:1))
      expect_equivalent(out,as.list(1:0))
      
      # list should be unique
      in_ <- as.list(1:2)
      out <- t$process9(in_)
      expect_equivalent(sort(unlist(out)),as.integer(c(1,2,42)))
      expect_equivalent(sort(unlist(in_)),as.integer(c(1,2,42)))
      
      # list should be unique
      in_ <- as.list(EEE$new()$A,EEE$new()$B)
      out <- t$process10(in_)
      expect_equivalent(sort(unlist(out)),sort(unlist(in_)))
      expect_equivalent(sort(unlist(in_)),sort(unlist(in_)))
      
      # list should be unique
      in_ <- list(t2)
      out <- t$process11(in_)
      expect_equivalent(sort(sapply(in_,function(x) x$gett())),c(12L,42L))
      expect_equivalent(sort(sapply(out,function(x) x$gett())),c(12L,42L))
      
      d1 <- list(42L)
      d2 <- list()
      t$process22(d1,d2)
      expect_equivalent(d1,list())
      expect_equivalent(d2,list(42))
      
      l1 <- list(1,2)
      l2 <- list(2,3)
      t$process24(l1,l2)
      expect_equivalent(l1, list(3L,2))
}
)