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
      #int_wrp3 <- Int$new(int_wrp$.__enclos_env__$private$py_obj)
      #int_wrp3$i_ <- 5
      #expect_identical(int_wrp3$i_,5L)
      #expect_identical(int_wrp$i_,2L)
      #expect_identical(int_wrpcpy$i_,2L)

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
      in1 <- list(list(1,2.0),list(2,3.0))
      out <- t$process6(in1)
      expect_equivalent(in1,list(list(1L, 2.0), list(2L, 3.0), list(7L, 11.0)))
      expect_equal(out,rev(list(list(1, 2.0), list(2, 3.0), list(7, 11.0))))
      
      out = t$process7(as.list(0:1))
      expect_equivalent(out,as.list(1:0))

      # process 8
      in_ <- list(0,1,0)
      out <- t$process8(in_)
      expect_equivalent(in_,rev(out))

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

      # process 12
      out <- t$process12(1,2.0)
      expect_identical(out,list("1"=2.0))

      # process 13
      out <- t$process13(EEE$new()$A,2)
      print(out)
      p13 <- list(2)
      names(p13) <- EEE$new()$A
      print(p13)
      print(unlist(out) == unlist(p13))
      expect_equivalent(out,p13)

      # process 14
      out <- t$process14(EEE$new()$A,3)
      expect_equivalent(out,list("3" = EEE$new()$A))

      # process 15
      out <- t$process15(12)
      k <- as.integer(names(out));v <- unname(out)[[1]]
      expect_equivalent(k,12L)
      expect_equivalent(v$gett(),12L)

      # process 16
      expect_equivalent(t$process16(list("42"=2.0,"12"=1.0)),2.0)

      # process 17
      p17 <- list(2.0,1.0)
      names(p17) <- c(EEE$new()$A,EEE$new()$B)
      expect_equivalent(t$process17(p17),2.0)

      # process 18
      expect_equivalent(t$process18(list("23"=t,"12"=t2)),t$gett())

      # process 19
      dd <- list()
      t$process19(dd)
      expect_equal(length(dd),1)
      expect_equal(as.integer(names(dd)),23)
      expect_equivalent(dd[[1]]$gett(),12L)

      # process 20
      dd <- list()
      t$process20(dd)
      expect_equal(as.integer(names(dd)),23)
      expect_equivalent(dd[[1]],42.0)

      # process 21
      d1 <- list()
      t$process21(d1,list("42"=11))
      expect_equivalent(d1[["1"]],11L)

      # process 211
      d1 <- list()
      t$process211(d1,list("42"=list(11,6)))
      expect_equivalent(d1[["1"]],11L)

      # process 212
      d2 <- list()
      t$process212(d2,list("42"=list(list(11,6),list(2),list(8))))
      expect_equivalent(d1[["1"]],11L)

      # process 214
      d3 <- list()
      t$process214(d3,list("42"=list(list(11,6),list(2,8))))
      expect_equivalent(d1[["1"]],11L)

      d1 <- list(42L)
      d2 <- list()
      t$process22(d1,d2)
      expect_equivalent(d1,list())
      expect_equivalent(d2,list(42))

      # process 23
      l1 <- list(1,2)
      l2 <- list()
      t$process23(l1,l2)
      expect_equivalent(l1,list(1L))
      expect_equivalent(l2,list(2.0))

      l1 <- list(1,2.0)
      l2 <- list(2,3)
      t$process24(l1,l2)
      expect_equivalent(l1, list(3L,2.0))

      # process 25
      i1 <- Int$new(1)
      i2 <- Int$new(2)
      i3 <- Int$new(3)

      expect_equal(t$process25(list(i1,i2,i3)),6)
      expect_equal(t$process25(list()),0)

      expect_equal(t$process26(list(list(i1,i2,i3))),6)
      expect_equal(t$process26(list(list(i1,i2,i3),list(i1))),7)
      expect_equal(t$process26(list(list(i1,i2,i3),list(i1),list(i1,i2))),10)

      empty_list = list(list(),list(),list())
      t$process29(empty_list)
      expect_equal(length(empty_list),3)
      expect_equal(length(empty_list[[1]]),1)
      expect_equal(length(empty_list[[2]]),1)
      expect_equal(length(empty_list[[3]]),1)
      expect_equal(empty_list[[1]][[1]]$i_,42)

      empty_list <- list(list(list(list())),    list(list(list())))
      t$process30(empty_list)

      expect_equal(length(empty_list),2)
      expect_equal(length(empty_list[[1]]),2)
      expect_equal(length(empty_list[[2]]),2)
      expect_equal(empty_list[[1]][[2]][[1]][[1]]$i_,42)
      expect_equal(empty_list[[2]][[2]][[1]][[1]]$i_,42)

      expect_equivalent(t$process31(list(1,2,3)),6L)
      expect_equivalent(t$process31(list()),0L)

      expect_equivalent(t$process32(list(list(1,2,3))),6L)
      expect_equivalent(t$process32(list(list(1,2,3),list(1))),7L)
      expect_equivalent(t$process32(list(list(1,2,3),list(1),list(1,2))),10L)

      # process 33
      i1 <- Int$new(1)
      expect_equal(t$process33(i1),2)
      i2 <- Int$new(10)
      expect_equal(t$process33(i2),11)

      # process 34
      i1 <- Int$new(10)
      i2 <- t$process34(i1)
      expect_equal(class(i2),c("Int","R6"))
      expect_equal(i1$i_,11)
      expect_equal(i2$i_,11)
      i3 <- t$process34(i2)
      expect_equal(i1$i_,12)
      expect_equal(i2$i_,12)
      expect_equal(i3$i_,12)

      # process 35
      i1 <- Int$new(20)
      i2 <- t$process35(i1)
      expect_equal(class(i2),c("Int","R6"))
      expect_equal(i1$i_,21)
      expect_equal(i1$i_,21)
      i3 <- t$process35(i2)
      expect_equal(i1$i_,21)
      expect_equal(i2$i_,22)
      expect_equal(i3$i_,22)

      # process 36 (Raw Ptr)
      i1 <- Int$new(1)
      expect_equal(t$process36(i1),2)
      expect_equal(t$process36(i1),3)
      i2 <- Int$new(10)
      expect_equal(t$process36(i2),11)

      # process 37
      i1 <- Int$new(1)
      expect_equal(t$process37(i1)$i_,2)
      expect_equal(t$process37(i1)$i_,3)
      i2 <- Int$new(10)
      expect_equal(t$process37(i2)$i_,11)

      # return of NULL
      i1 <- Int$new(18)
      expect_equal(t$process37(i1),NULL)
}
)