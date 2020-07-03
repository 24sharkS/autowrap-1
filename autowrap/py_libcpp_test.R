#cython: c_string_encoding=ascii  # for cython>=0.19
#cython: embedsignature=False
from  libcpp.string  cimport string as libcpp_string
from  libcpp.string  cimport string as libcpp_utf8_string
from  libcpp.string  cimport string as libcpp_utf8_output_string
from  libcpp.set     cimport set as libcpp_set
from  libcpp.vector  cimport vector as libcpp_vector
from  libcpp.pair    cimport pair as libcpp_pair
from  libcpp.map     cimport map  as libcpp_map
from  libcpp cimport bool
from  libc.string cimport const_char
from cython.operator cimport dereference as deref, preincrement as inc, address as address
from  AutowrapRefHolder cimport AutowrapRefHolder
from  AutowrapPtrHolder cimport AutowrapPtrHolder
from  AutowrapConstPtrHolder cimport AutowrapConstPtrHolder
from  smart_ptr cimport shared_ptr
from libcpp_test cimport EEE as _EEE
from libcpp_test cimport ABS_Impl1 as _ABS_Impl1
from libcpp_test cimport ABS_Impl2 as _ABS_Impl2
from libcpp_test cimport AbstractBaseClass as _AbstractBaseClass
from libcpp_test cimport Int as _Int
from libcpp_test cimport LibCppTest as _LibCppTest

cdef extern from "autowrap_tools.hpp":
    char * _cast_const_away(char *) 

cdef class ABS_Impl1:
    """
    Cython implementation of _ABS_Impl1
     -- Inherits from ['AbstractBaseClass']
    """

    cdef shared_ptr[_ABS_Impl1] inst

    def __dealloc__(self):
         self.inst.reset()

    
    def _init_0(self):
        """Cython signature: void ABS_Impl1()"""
        pass
    
    def _init_1(self,  i ):
        """Cython signature: void ABS_Impl1(int i)"""
        assert isinstance(i, (int, long)), 'arg i wrong type'
    
        self.inst = shared_ptr[_ABS_Impl1](new _ABS_Impl1((<int>i)))
    
    def __init__(self, *args , **kwargs):
        """
          - Cython signature: void ABS_Impl1()
          - Cython signature: void ABS_Impl1(int i)
"""
        if kwargs.get("__createUnsafeObject__") is True:
             self._init_0(*args)
        elif (len(args)==1) and (isinstance(args[0], (int, long))):
             self._init_1(*args)
        else:
               raise Exception('can not handle type of %s' % (args,))
    
    def get(self):
        """Cython signature: int get()"""
        cdef int _r = self.inst.get().get()
        py_result = <int>_r
        return py_result 

cdef class ABS_Impl2:
    """
    Cython implementation of _ABS_Impl2
     -- Inherits from ['AbstractBaseClass']
    """

    cdef shared_ptr[_ABS_Impl2] inst

    def __dealloc__(self):
         self.inst.reset()

    
    def _init_0(self):
        """Cython signature: void ABS_Impl2()"""
        pass
    
    def _init_1(self,  i ):
        """Cython signature: void ABS_Impl2(int i)"""
        assert isinstance(i, (int, long)), 'arg i wrong type'
    
        self.inst = shared_ptr[_ABS_Impl2](new _ABS_Impl2((<int>i)))
    
    def __init__(self, *args , **kwargs):
        """
          - Cython signature: void ABS_Impl2()
          - Cython signature: void ABS_Impl2(int i)
"""
        if kwargs.get("__createUnsafeObject__") is True:
             self._init_0(*args)
        elif (len(args)==1) and (isinstance(args[0], (int, long))):
             self._init_1(*args)
        else:
               raise Exception('can not handle type of %s' % (args,))
    
    def get(self):
        """Cython signature: int get()"""
        cdef int _r = self.inst.get().get()
        py_result = <int>_r
        return py_result 

cdef class Int:
    """
    Cython implementation of _Int
    """

    cdef shared_ptr[_Int] inst

    def __dealloc__(self):
         self.inst.reset()

    
    property i_:
        def __set__(self,  i_):
        
            self.inst.get().i_ = (<int>i_)
        
    
        def __get__(self):
            cdef int _r = self.inst.get().i_
            py_result = <int>_r
            return py_result
    
    def __copy__(self):
       cdef Int rv = Int.__new__(Int)
       rv.inst = shared_ptr[_Int](new _Int(deref(self.inst.get())))
       return rv
    
    def __deepcopy__(self, memo):
       cdef Int rv = Int.__new__(Int)
       rv.inst = shared_ptr[_Int](new _Int(deref(self.inst.get())))
       return rv
    
    def _init_0(self,  i ):
        """Cython signature: void Int(int i)"""
        assert isinstance(i, (int, long)), 'arg i wrong type'
    
        self.inst = shared_ptr[_Int](new _Int((<int>i)))
    
    def _init_1(self, Int i ):
        """Cython signature: void Int(Int & i)"""
        assert isinstance(i, Int), 'arg i wrong type'
    
        self.inst = shared_ptr[_Int](new _Int((deref(i.inst.get()))))
    
    def __init__(self, *args , **kwargs):
        """
          - Cython signature: void Int(int i)
          - Cython signature: void Int(Int & i)
"""
        if (len(args)==1) and (isinstance(args[0], (int, long))):
             self._init_0(*args)
        elif (len(args)==1) and (isinstance(args[0], Int)):
             self._init_1(*args)
        else:
               raise Exception('can not handle type of %s' % (args,)) 

cdef class LibCppTest:
    """
    Cython implementation of _LibCppTest

    This is some class doc
    Pretty cool stuff!
    -----
    With a trick, we can even get multiple paragraphs, allowing us to
    write much longer documentation.
    """

    cdef shared_ptr[_LibCppTest] inst

    def __dealloc__(self):
         self.inst.reset()


    def __hash__(self):
      # The only required property is that objects which compare equal have
      # the same hash value:
      return hash(deref(self.inst.get()).get() )

    
    def _init_0(self):
        """Cython signature: void LibCppTest()"""
        self.inst = shared_ptr[_LibCppTest](new _LibCppTest())
    
    def _init_1(self,  ii ):
        """Cython signature: void LibCppTest(int ii)"""
        assert isinstance(ii, (int, long)), 'arg ii wrong type'
    
        self.inst = shared_ptr[_LibCppTest](new _LibCppTest((<int>ii)))
    
    def __init__(self, *args , **kwargs):
        """
          - Cython signature: void LibCppTest()
          - Cython signature: void LibCppTest(int ii)
"""
        if not args:
             self._init_0(*args)
        elif (len(args)==1) and (isinstance(args[0], (int, long))):
             self._init_1(*args)
        else:
               raise Exception('can not handle type of %s' % (args,))
    
    def gett(self):
        """Cython signature: int gett()
        getting access to an integer
"""
        cdef int _r = self.inst.get().get()
        py_result = <int>_r
        return py_result
    
    def twist(self, list in_0 ):
        """Cython signature: libcpp_pair[int,libcpp_string] twist(libcpp_pair[libcpp_string,int])
        Dont forget this stuff here!
"""
        assert isinstance(in_0, list) and len(in_0) == 2 and isinstance(in_0[0], bytes) and isinstance(in_0[1], (int, long)), 'arg in_0 wrong type'
        cdef libcpp_pair[libcpp_string, int] v0
        v0.first = in_0[0]
        v0.second = in_0[1]
        _r = self.inst.get().twist(v0)
        cdef list py_result = [_r.first, _r.second]
        return py_result
    
    def process(self, list in_0 ):
        """Cython signature: libcpp_vector[int] process(libcpp_vector[int] &)"""
        assert isinstance(in_0, list) and all(isinstance(elemt_rec, (int, long)) for elemt_rec in in_0), 'arg in_0 wrong type'
        cdef libcpp_vector[int] v0 = in_0
        _r = self.inst.get().process(v0)
        in_0[:] = v0
        cdef list py_result = _r
        return py_result
    
    def process2(self, list in_0 ):
        """Cython signature: libcpp_pair[int,int] process2(libcpp_pair[int,int] &)"""
        assert isinstance(in_0, list) and len(in_0) == 2 and isinstance(in_0[0], (int, long)) and isinstance(in_0[1], (int, long)), 'arg in_0 wrong type'
        cdef libcpp_pair[int, int] v0
        v0.first = in_0[0]
        v0.second = in_0[1]
        _r = self.inst.get().process2(v0)
        in_0[:] = [v0.first, v0.second]
        cdef list py_result = [_r.first, _r.second]
        return py_result
    
    def process3(self, list in_0 ):
        """Cython signature: libcpp_pair[LibCppTest,int] process3(libcpp_pair[LibCppTest,int] &)"""
        assert isinstance(in_0, list) and len(in_0) == 2 and isinstance(in_0[0], LibCppTest) and isinstance(in_0[1], (int, long)), 'arg in_0 wrong type'
        cdef libcpp_pair[_LibCppTest, int] v0
        v0.first = deref((<LibCppTest>in_0[0]).inst.get())
        v0.second = in_0[1]
        _r = self.inst.get().process3(v0)
        cdef LibCppTest temp1 = LibCppTest.__new__(LibCppTest)
        temp1.inst = shared_ptr[_LibCppTest](new _LibCppTest(v0.first))
        in_0[:] = [temp1, v0.second]
        cdef LibCppTest out1 = LibCppTest.__new__(LibCppTest)
        out1.inst = shared_ptr[_LibCppTest](new _LibCppTest(_r.first))
        cdef list py_result = [out1, _r.second]
        return py_result
    
    def process4(self, list in_0 ):
        """Cython signature: libcpp_pair[int,LibCppTest] process4(libcpp_pair[int,LibCppTest] &)"""
        assert isinstance(in_0, list) and len(in_0) == 2 and isinstance(in_0[0], (int, long)) and isinstance(in_0[1], LibCppTest), 'arg in_0 wrong type'
        cdef libcpp_pair[int, _LibCppTest] v0
        v0.first = in_0[0]
        v0.second = deref((<LibCppTest>in_0[1]).inst.get())
        _r = self.inst.get().process4(v0)
        cdef LibCppTest temp2 = LibCppTest.__new__(LibCppTest)
        temp2.inst = shared_ptr[_LibCppTest](new _LibCppTest(v0.second))
        in_0[:] = [v0.first, temp2]
        cdef LibCppTest out2 = LibCppTest.__new__(LibCppTest)
        out2.inst = shared_ptr[_LibCppTest](new _LibCppTest(_r.second))
        cdef list py_result = [_r.first, out2]
        return py_result
    
    def process5(self, list in_0 ):
        """Cython signature: libcpp_pair[LibCppTest,LibCppTest] process5(libcpp_pair[LibCppTest,LibCppTest] &)"""
        assert isinstance(in_0, list) and len(in_0) == 2 and isinstance(in_0[0], LibCppTest) and isinstance(in_0[1], LibCppTest), 'arg in_0 wrong type'
        cdef libcpp_pair[_LibCppTest, _LibCppTest] v0
        v0.first = deref((<LibCppTest>in_0[0]).inst.get())
        v0.second = deref((<LibCppTest>in_0[1]).inst.get())
        _r = self.inst.get().process5(v0)
        cdef LibCppTest temp1 = LibCppTest.__new__(LibCppTest)
        temp1.inst = shared_ptr[_LibCppTest](new _LibCppTest(v0.first))
        cdef LibCppTest temp2 = LibCppTest.__new__(LibCppTest)
        temp2.inst = shared_ptr[_LibCppTest](new _LibCppTest(v0.second))
        in_0[:] = [temp1, temp2]
        cdef LibCppTest out1 = LibCppTest.__new__(LibCppTest)
        out1.inst = shared_ptr[_LibCppTest](new _LibCppTest(_r.first))
        cdef LibCppTest out2 = LibCppTest.__new__(LibCppTest)
        out2.inst = shared_ptr[_LibCppTest](new _LibCppTest(_r.second))
        cdef list py_result = [out1, out2]
        return py_result
    
    def process6(self, list in_0 ):
        """Cython signature: libcpp_vector[libcpp_pair[int,double]] process6(libcpp_vector[libcpp_pair[int,double]] &)"""
        assert isinstance(in_0, list) and all(isinstance(elemt_rec, list) and len(elemt_rec) == 2 and isinstance(elemt_rec[0], (int, long)) and isinstance(elemt_rec[1], float) for elemt_rec in in_0), 'arg in_0 wrong type'
        cdef libcpp_vector[libcpp_pair[int,double]] v0 = in_0
        _r = self.inst.get().process6(v0)
        in_0[:] = v0
        cdef list py_result = _r
        return py_result
    
    def process7(self, list in_0 ):
        """Cython signature: libcpp_pair[int,EEE] process7(libcpp_pair[EEE,int] &)"""
        assert isinstance(in_0, list) and len(in_0) == 2 and in_0[0] in [0, 1] and isinstance(in_0[1], (int, long)), 'arg in_0 wrong type'
        cdef libcpp_pair[_EEE, int] v0
        v0.first = (<EEE>in_0[0])
        v0.second = in_0[1]
        _r = self.inst.get().process7(v0)
        in_0[:] = [v0.first, v0.second]
        cdef _EEE out2 = (<_EEE> _r.second)
        cdef list py_result = [_r.first, out2]
        return py_result
    
    def process8(self, list in_0 ):
        """Cython signature: libcpp_vector[EEE] process8(libcpp_vector[EEE] &)"""
        assert isinstance(in_0, list) and all(elemt_rec in [0, 1] for elemt_rec in in_0), 'arg in_0 wrong type'
        cdef libcpp_vector[_EEE] * v0 = new libcpp_vector[_EEE]()
        cdef int item0
        for item0 in in_0:
            v0.push_back(<_EEE> item0)
        _r = self.inst.get().process8(deref(v0))
        replace = []
        cdef libcpp_vector[_EEE].iterator it_in_0 = v0.begin()
        while it_in_0 != v0.end():
            replace.append(<int> deref(it_in_0))
            inc(it_in_0)
        in_0[:] = replace
        del v0
        py_result = []
        cdef libcpp_vector[_EEE].iterator it__r = _r.begin()
        while it__r != _r.end():
           py_result.append(<int>deref(it__r))
           inc(it__r)
        return py_result
    
    def process9(self, set in_0 ):
        """Cython signature: libcpp_set[int] process9(libcpp_set[int] &)"""
        assert isinstance(in_0, set) and all(isinstance(li, (int, long)) for li in in_0), 'arg in_0 wrong type'
        cdef libcpp_set[int] v0 = in_0
        _r = self.inst.get().process9(v0)
        in_0.clear()
        in_0.update(v0)
        cdef set py_result = _r
        return py_result
    
    def process10(self, set in_0 ):
        """Cython signature: libcpp_set[EEE] process10(libcpp_set[EEE] &)"""
        assert isinstance(in_0, set) and all(li in [0, 1] for li in in_0), 'arg in_0 wrong type'
        cdef libcpp_set[_EEE] * v0 = new libcpp_set[_EEE]()
        cdef int item0
        for item0 in in_0:
           v0.insert(<_EEE> item0)
        _r = self.inst.get().process10(deref(v0))
        replace = set()
        cdef libcpp_set[_EEE].iterator it_in_0 = v0.begin()
        while it_in_0 != v0.end():
           replace.add(<int> deref(it_in_0))
           inc(it_in_0)
        in_0.clear()
        in_0.update(replace)
        del v0
        py_result = set()
        cdef libcpp_set[_EEE].iterator it__r = _r.begin()
        while it__r != _r.end():
           py_result.add(<int>deref(it__r))
           inc(it__r)
        return py_result
    
    def process11(self, set in_0 ):
        """Cython signature: libcpp_set[LibCppTest] process11(libcpp_set[LibCppTest] &)"""
        assert isinstance(in_0, set) and all(isinstance(li, LibCppTest) for li in in_0), 'arg in_0 wrong type'
        cdef libcpp_set[_LibCppTest] * v0 = new libcpp_set[_LibCppTest]()
        cdef LibCppTest item0
        for item0 in in_0:
           v0.insert(deref(item0.inst.get()))
        _r = self.inst.get().process11(deref(v0))
        replace = set()
        cdef libcpp_set[_LibCppTest].iterator it_in_0 = v0.begin()
        while it_in_0 != v0.end():
           item0 = LibCppTest.__new__(LibCppTest)
           item0.inst = shared_ptr[_LibCppTest](new _LibCppTest(deref(it_in_0)))
           replace.add(item0)
           inc(it_in_0)
        in_0.clear()
        in_0.update(replace)
        del v0
        py_result = set()
        cdef libcpp_set[_LibCppTest].iterator it__r = _r.begin()
        cdef LibCppTest item_py_result
        while it__r != _r.end():
           item_py_result = LibCppTest.__new__(LibCppTest)
           item_py_result.inst = shared_ptr[_LibCppTest](new _LibCppTest(deref(it__r)))
           py_result.add(item_py_result)
           inc(it__r)
        return py_result
    
    def process12(self,  i , float f ):
        """Cython signature: libcpp_map[int,float] process12(int i, float f)"""
        assert isinstance(i, (int, long)), 'arg i wrong type'
        assert isinstance(f, float), 'arg f wrong type'
    
    
        _r = self.inst.get().process12((<int>i), (<float>f))
        py_result = dict()
        cdef libcpp_map[int, float].iterator it__r = _r.begin()
        while it__r != _r.end():
           py_result[<int>(deref(it__r).first)] = <float>(deref(it__r).second)
           inc(it__r)
        return py_result
    
    def process13(self, int e ,  i ):
        """Cython signature: libcpp_map[EEE,int] process13(EEE e, int i)"""
        assert e in [0, 1], 'arg e wrong type'
        assert isinstance(i, (int, long)), 'arg i wrong type'
    
    
        _r = self.inst.get().process13((<_EEE>e), (<int>i))
        py_result = dict()
        cdef libcpp_map[_EEE, int].iterator it__r = _r.begin()
        while it__r != _r.end():
           py_result[<_EEE>(deref(it__r).first)] = <int>(deref(it__r).second)
           inc(it__r)
        return py_result
    
    def process14(self, int e ,  i ):
        """Cython signature: libcpp_map[int,EEE] process14(EEE e, int i)"""
        assert e in [0, 1], 'arg e wrong type'
        assert isinstance(i, (int, long)), 'arg i wrong type'
    
    
        _r = self.inst.get().process14((<_EEE>e), (<int>i))
        py_result = dict()
        cdef libcpp_map[int, _EEE].iterator it__r = _r.begin()
        while it__r != _r.end():
           py_result[<int>(deref(it__r).first)] = <_EEE>(deref(it__r).second)
           inc(it__r)
        return py_result
    
    def process15(self,  ii ):
        """Cython signature: libcpp_map[long int,LibCppTest] process15(int ii)"""
        assert isinstance(ii, (int, long)), 'arg ii wrong type'
    
        _r = self.inst.get().process15((<int>ii))
        py_result = dict()
        cdef libcpp_map[long int, _LibCppTest].iterator it__r = _r.begin()
        cdef LibCppTest item_py_result
        while it__r != _r.end():
           item_py_result = LibCppTest.__new__(LibCppTest)
           item_py_result.inst = shared_ptr[_LibCppTest](new _LibCppTest((deref(it__r)).second))
           py_result[<long int>(deref(it__r).first)] = item_py_result
           inc(it__r)
        return py_result
    
    def process16(self, dict in_ ):
        """Cython signature: float process16(libcpp_map[int,float] in_)"""
        assert isinstance(in_, dict) and all(isinstance(k, (int, long)) for k in in_.keys()) and all(isinstance(v, float) for v in in_.values()), 'arg in_ wrong type'
        cdef libcpp_map[int, float] * v0 = new libcpp_map[int, float]()
        for key, value in in_.items():
        
        
            deref(v0)[ (<int>key) ] = (<float>value)
        
        
        cdef float _r = self.inst.get().process16(deref(v0))
        del v0
        py_result = <float>_r
        return py_result
    
    def process17(self, dict in_ ):
        """Cython signature: float process17(libcpp_map[EEE,float] in_)"""
        assert isinstance(in_, dict) and all(k in [0, 1] for k in in_.keys()) and all(isinstance(v, float) for v in in_.values()), 'arg in_ wrong type'
        cdef libcpp_map[_EEE, float] * v0 = new libcpp_map[_EEE, float]()
        for key, value in in_.items():
        
        
            deref(v0)[ <_EEE> key ] = (<float>value)
        
        
        cdef float _r = self.inst.get().process17(deref(v0))
        del v0
        py_result = <float>_r
        return py_result
    
    def process18(self, dict in_ ):
        """Cython signature: int process18(libcpp_map[int,LibCppTest] in_)"""
        assert isinstance(in_, dict) and all(isinstance(k, (int, long)) for k in in_.keys()) and all(isinstance(v, LibCppTest) for v in in_.values()), 'arg in_ wrong type'
        cdef libcpp_map[int, _LibCppTest] * v0 = new libcpp_map[int, _LibCppTest]()
        for key, value in in_.items():
        
        
            deref(v0)[ (<int>key) ] = deref((<LibCppTest>value).inst.get())
        
        
        cdef int _r = self.inst.get().process18(deref(v0))
        del v0
        py_result = <int>_r
        return py_result
    
    def process19(self, dict in_ ):
        """Cython signature: void process19(libcpp_map[int,LibCppTest] & in_)"""
        assert isinstance(in_, dict) and all(isinstance(k, (int, long)) for k in in_.keys()) and all(isinstance(v, LibCppTest) for v in in_.values()), 'arg in_ wrong type'
        cdef libcpp_map[int, _LibCppTest] * v0 = new libcpp_map[int, _LibCppTest]()
        for key, value in in_.items():
        
        
            deref(v0)[ (<int>key) ] = deref((<LibCppTest>value).inst.get())
        
        
        self.inst.get().process19(deref(v0))
        replace = dict()
        cdef libcpp_map[int, _LibCppTest].iterator it_in_ = v0.begin()
        cdef LibCppTest item_in_
        while it_in_ != v0.end():
           item_in_ = LibCppTest.__new__(LibCppTest)
           item_in_.inst = shared_ptr[_LibCppTest](new _LibCppTest((deref(it_in_)).second))
           replace[<int> deref(it_in_).first] = item_in_
           inc(it_in_)
        in_.clear()
        in_.update(replace)
        del v0
    
    def process20(self, dict in_ ):
        """Cython signature: void process20(libcpp_map[int,float] & in_)"""
        assert isinstance(in_, dict) and all(isinstance(k, (int, long)) for k in in_.keys()) and all(isinstance(v, float) for v in in_.values()), 'arg in_ wrong type'
        cdef libcpp_map[int, float] * v0 = new libcpp_map[int, float]()
        for key, value in in_.items():
        
        
            deref(v0)[ (<int>key) ] = (<float>value)
        
        
        self.inst.get().process20(deref(v0))
        replace = dict()
        cdef libcpp_map[int, float].iterator it_in_ = v0.begin()
        while it_in_ != v0.end():
           replace[<int> deref(it_in_).first] = <float> deref(it_in_).second
           inc(it_in_)
        in_.clear()
        in_.update(replace)
        del v0
    
    def process21(self, dict in_ , dict arg2 ):
        """Cython signature: void process21(libcpp_map[int,float] & in_, libcpp_map[int,int] & arg2)"""
        assert isinstance(in_, dict) and all(isinstance(k, (int, long)) for k in in_.keys()) and all(isinstance(v, float) for v in in_.values()), 'arg in_ wrong type'
        assert isinstance(arg2, dict) and all(isinstance(k, (int, long)) for k in arg2.keys()) and all(isinstance(v, (int, long)) for v in arg2.values()), 'arg arg2 wrong type'
        cdef libcpp_map[int, float] * v0 = new libcpp_map[int, float]()
        for key, value in in_.items():
        
        
            deref(v0)[ (<int>key) ] = (<float>value)
        
        
        cdef libcpp_map[int, int] * v1 = new libcpp_map[int, int]()
        for key, value in arg2.items():
        
        
            deref(v1)[ (<int>key) ] = (<int>value)
        
        
        self.inst.get().process21(deref(v0), deref(v1))
        replace = dict()
        cdef libcpp_map[int, int].iterator it_arg2 = v1.begin()
        while it_arg2 != v1.end():
           replace[<int> deref(it_arg2).first] = <int> deref(it_arg2).second
           inc(it_arg2)
        arg2.clear()
        arg2.update(replace)
        del v1
        replace = dict()
        cdef libcpp_map[int, float].iterator it_in_ = v0.begin()
        while it_in_ != v0.end():
           replace[<int> deref(it_in_).first] = <float> deref(it_in_).second
           inc(it_in_)
        in_.clear()
        in_.update(replace)
        del v0 

cdef class EEE:
    A = 0
    B = 1

    def getMapping(self):
        return dict([ (v, k) for k, v in self.__class__.__dict__.items() if isinstance(v, int) ]) 
