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

    
    property integer_vector_ptr:
        def __set__(self, list integer_vector_ptr):
            cdef libcpp_vector[_Int] * v0 = new libcpp_vector[_Int]()
            cdef Int item0
            for item0 in integer_vector_ptr:
                v0.push_back(deref(item0.inst.get()))
            self.inst.get().integer_vector_ptr = v0
    
        def __get__(self):
            if self.inst.get().integer_vector_ptr is NULL:
                 raise Exception("Cannot access pointer that is NULL")
            _r = deref(self.inst.get().integer_vector_ptr)
            py_result = []
            cdef libcpp_vector[_Int].iterator it__r = _r.begin()
            cdef Int item_py_result
            while it__r != _r.end():
               item_py_result = Int.__new__(Int)
               item_py_result.inst = shared_ptr[_Int](new _Int(deref(it__r)))
               py_result.append(item_py_result)
               inc(it__r)
            return py_result
    
    property integer_ptr:
        def __set__(self, Int integer_ptr):
        
            self.inst.get().integer_ptr = (integer_ptr.inst.get())
        
    
        def __get__(self):
            if self.inst.get().integer_ptr is NULL:
                 raise Exception("Cannot access pointer that is NULL")
            cdef  _Int * __r = (self.inst.get().integer_ptr)
            if __r == NULL:
                return None
            cdef _Int * _r = new _Int(deref(__r))
            cdef Int py_result = Int.__new__(Int)
            py_result.inst = shared_ptr[_Int](_r)
            return py_result
    
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
    
    def process211(self, dict in_ , dict arg2 ):
        """Cython signature: void process211(libcpp_map[int,float] & in_, libcpp_map[libcpp_string,libcpp_vector[int]] & arg2)"""
        assert isinstance(in_, dict) and all(isinstance(k, (int, long)) for k in in_.keys()) and all(isinstance(v, float) for v in in_.values()), 'arg in_ wrong type'
        assert isinstance(arg2, dict) and all(isinstance(k, bytes) for k in arg2.keys()) and all(isinstance(v, list) and all(isinstance(elemt_rec, (int, long)) for elemt_rec in v) for v in arg2.values()), 'arg arg2 wrong type'
        cdef libcpp_map[int, float] * v0 = new libcpp_map[int, float]()
        for key, value in in_.items():
        
        
            deref(v0)[ (<int>key) ] = (<float>value)
        
        
        cdef libcpp_vector[int] svec1
        cdef libcpp_map[libcpp_string, libcpp_vector[int]] * v1 = new libcpp_map[libcpp_string, libcpp_vector[int]]()
        for key, value in arg2.items():
        
            svec1 = value
            deref(v1)[ (<libcpp_string>key) ] = svec1
        
            
        self.inst.get().process211(deref(v0), deref(v1))
        replace = dict()
        cdef libcpp_map[libcpp_string, libcpp_vector[int]].iterator it_arg2 = v1.begin()
        while it_arg2 != v1.end():
           replace[<libcpp_string> deref(it_arg2).first] = <libcpp_vector[int]> deref(it_arg2).second
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
    
    def process212(self, dict in_ , dict arg2 ):
        """Cython signature: void process212(libcpp_map[int,float] & in_, libcpp_map[libcpp_string,libcpp_vector[libcpp_vector[int]]] & arg2)"""
        assert isinstance(in_, dict) and all(isinstance(k, (int, long)) for k in in_.keys()) and all(isinstance(v, float) for v in in_.values()), 'arg in_ wrong type'
        assert isinstance(arg2, dict) and all(isinstance(k, bytes) for k in arg2.keys()) and all(isinstance(v, list) and all(isinstance(elemt_rec, list) and all(isinstance(elemt_rec_rec, (int, long)) for elemt_rec_rec in elemt_rec) for elemt_rec in v) for v in arg2.values()), 'arg arg2 wrong type'
        cdef libcpp_map[int, float] * v0 = new libcpp_map[int, float]()
        for key, value in in_.items():
        
        
            deref(v0)[ (<int>key) ] = (<float>value)
        
        
        cdef libcpp_vector[libcpp_vector[int]] svec1
        cdef libcpp_map[libcpp_string, libcpp_vector[libcpp_vector[int]]] * v1 = new libcpp_map[libcpp_string, libcpp_vector[libcpp_vector[int]]]()
        for key, value in arg2.items():
        
            svec1 = value
            deref(v1)[ (<libcpp_string>key) ] = svec1
        
            
        self.inst.get().process212(deref(v0), deref(v1))
        replace = dict()
        cdef libcpp_map[libcpp_string, libcpp_vector[libcpp_vector[int]]].iterator it_arg2 = v1.begin()
        while it_arg2 != v1.end():
           replace[<libcpp_string> deref(it_arg2).first] = <libcpp_vector[libcpp_vector[int]]> deref(it_arg2).second
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
    
    def process214(self, dict in_ , dict arg2 ):
        """Cython signature: void process214(libcpp_map[int,float] & in_, libcpp_map[libcpp_string,libcpp_vector[libcpp_pair[int,int]]] & arg2)"""
        assert isinstance(in_, dict) and all(isinstance(k, (int, long)) for k in in_.keys()) and all(isinstance(v, float) for v in in_.values()), 'arg in_ wrong type'
        assert isinstance(arg2, dict) and all(isinstance(k, bytes) for k in arg2.keys()) and all(isinstance(v, list) and all(isinstance(elemt_rec, list) and len(elemt_rec) == 2 and isinstance(elemt_rec[0], (int, long)) and isinstance(elemt_rec[1], (int, long)) for elemt_rec in v) for v in arg2.values()), 'arg arg2 wrong type'
        cdef libcpp_map[int, float] * v0 = new libcpp_map[int, float]()
        for key, value in in_.items():
        
        
            deref(v0)[ (<int>key) ] = (<float>value)
        
        
        cdef libcpp_vector[libcpp_pair[int,int]] svec1
        cdef libcpp_map[libcpp_string, libcpp_vector[libcpp_pair[int,int]]] * v1 = new libcpp_map[libcpp_string, libcpp_vector[libcpp_pair[int,int]]]()
        for key, value in arg2.items():
        
            svec1 = value
            deref(v1)[ (<libcpp_string>key) ] = svec1
        
            
        self.inst.get().process214(deref(v0), deref(v1))
        replace = dict()
        cdef libcpp_map[libcpp_string, libcpp_vector[libcpp_pair[int,int]]].iterator it_arg2 = v1.begin()
        while it_arg2 != v1.end():
           replace[<libcpp_string> deref(it_arg2).first] = <libcpp_vector[libcpp_pair[int,int]]> deref(it_arg2).second
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
    
    def process22(self, set in_0 , set in_1 ):
        """Cython signature: void process22(libcpp_set[int] &, libcpp_set[float] &)"""
        assert isinstance(in_0, set) and all(isinstance(li, (int, long)) for li in in_0), 'arg in_0 wrong type'
        assert isinstance(in_1, set) and all(isinstance(li, float) for li in in_1), 'arg in_1 wrong type'
        cdef libcpp_set[int] v0 = in_0
        cdef libcpp_set[float] v1 = in_1
        self.inst.get().process22(v0, v1)
        in_1.clear()
        in_1.update(v1)
        in_0.clear()
        in_0.update(v0)
    
    def process23(self, list in_0 , list in_1 ):
        """Cython signature: void process23(libcpp_vector[int] &, libcpp_vector[float] &)"""
        assert isinstance(in_0, list) and all(isinstance(elemt_rec, (int, long)) for elemt_rec in in_0), 'arg in_0 wrong type'
        assert isinstance(in_1, list) and all(isinstance(elemt_rec, float) for elemt_rec in in_1), 'arg in_1 wrong type'
        cdef libcpp_vector[int] v0 = in_0
        cdef libcpp_vector[float] v1 = in_1
        self.inst.get().process23(v0, v1)
        in_1[:] = v1
        in_0[:] = v0
    
    def process24(self, list in_ , list arg2 ):
        """Cython signature: void process24(libcpp_pair[int,float] & in_, libcpp_pair[int,int] & arg2)"""
        assert isinstance(in_, list) and len(in_) == 2 and isinstance(in_[0], (int, long)) and isinstance(in_[1], float), 'arg in_ wrong type'
        assert isinstance(arg2, list) and len(arg2) == 2 and isinstance(arg2[0], (int, long)) and isinstance(arg2[1], (int, long)), 'arg arg2 wrong type'
        cdef libcpp_pair[int, float] v0
        v0.first = in_[0]
        v0.second = in_[1]
        cdef libcpp_pair[int, int] v1
        v1.first = arg2[0]
        v1.second = arg2[1]
        self.inst.get().process24(v0, v1)
        arg2[:] = [v1.first, v1.second]
        in_[:] = [v0.first, v0.second]
    
    def process25(self, list in_ ):
        """Cython signature: int process25(libcpp_vector[Int] in_)"""
        assert isinstance(in_, list) and all(isinstance(elemt_rec, Int) for elemt_rec in in_), 'arg in_ wrong type'
        cdef libcpp_vector[_Int] * v0 = new libcpp_vector[_Int]()
        cdef Int item0
        for item0 in in_:
            v0.push_back(deref(item0.inst.get()))
        cdef int _r = self.inst.get().process25(deref(v0))
        del v0
        py_result = <int>_r
        return py_result
    
    def process26(self, list in_ ):
        """Cython signature: int process26(libcpp_vector[libcpp_vector[Int]] in_)"""
        assert isinstance(in_, list) and all(isinstance(elemt_rec, list) and all(isinstance(elemt_rec_rec, Int) for elemt_rec_rec in elemt_rec) for elemt_rec in in_), 'arg in_ wrong type'
        cdef libcpp_vector[libcpp_vector[_Int]] * v0 = new libcpp_vector[libcpp_vector[_Int]]()
        cdef libcpp_vector[_Int] * v0_rec = new libcpp_vector[_Int]()
        cdef Int item0_rec
        for in__rec in in_:
            v0_rec.clear()
            for item0_rec in in__rec:
                v0_rec.push_back(deref(item0_rec.inst.get()))
            v0.push_back(deref(v0_rec))
        cdef int _r = self.inst.get().process26(deref(v0))
        del v0
        del v0_rec
        py_result = <int>_r
        return py_result
    
    def process27(self, list in_ ):
        """Cython signature: int process27(libcpp_vector[libcpp_vector[libcpp_vector[Int]]] in_)"""
        assert isinstance(in_, list) and all(isinstance(elemt_rec, list) and all(isinstance(elemt_rec_rec, list) and all(isinstance(elemt_rec_rec_rec, Int) for elemt_rec_rec_rec in elemt_rec_rec) for elemt_rec_rec in elemt_rec) for elemt_rec in in_), 'arg in_ wrong type'
        cdef libcpp_vector[libcpp_vector[libcpp_vector[_Int]]] * v0 = new libcpp_vector[libcpp_vector[libcpp_vector[_Int]]]()
        cdef libcpp_vector[libcpp_vector[_Int]] * v0_rec = new libcpp_vector[libcpp_vector[_Int]]()
        cdef libcpp_vector[_Int] * v0_rec_rec = new libcpp_vector[_Int]()
        cdef Int item0_rec_rec
        for in__rec in in_:
            v0_rec.clear()
            for in__rec_rec in in__rec:
                v0_rec_rec.clear()
                for item0_rec_rec in in__rec_rec:
                    v0_rec_rec.push_back(deref(item0_rec_rec.inst.get()))
                v0_rec.push_back(deref(v0_rec_rec))
            v0.push_back(deref(v0_rec))
        cdef int _r = self.inst.get().process27(deref(v0))
        del v0
        del v0_rec
        del v0_rec_rec
        py_result = <int>_r
        return py_result
    
    def process28(self, list in_ ):
        """Cython signature: int process28(libcpp_vector[libcpp_vector[libcpp_vector[libcpp_vector[Int]]]] in_)"""
        assert isinstance(in_, list) and all(isinstance(elemt_rec, list) and all(isinstance(elemt_rec_rec, list) and all(isinstance(elemt_rec_rec_rec, list) and all(isinstance(elemt_rec_rec_rec_rec, Int) for elemt_rec_rec_rec_rec in elemt_rec_rec_rec) for elemt_rec_rec_rec in elemt_rec_rec) for elemt_rec_rec in elemt_rec) for elemt_rec in in_), 'arg in_ wrong type'
        cdef libcpp_vector[libcpp_vector[libcpp_vector[libcpp_vector[_Int]]]] * v0 = new libcpp_vector[libcpp_vector[libcpp_vector[libcpp_vector[_Int]]]]()
        cdef libcpp_vector[libcpp_vector[libcpp_vector[_Int]]] * v0_rec = new libcpp_vector[libcpp_vector[libcpp_vector[_Int]]]()
        cdef libcpp_vector[libcpp_vector[_Int]] * v0_rec_rec = new libcpp_vector[libcpp_vector[_Int]]()
        cdef libcpp_vector[_Int] * v0_rec_rec_rec = new libcpp_vector[_Int]()
        cdef Int item0_rec_rec_rec
        for in__rec in in_:
            v0_rec.clear()
            for in__rec_rec in in__rec:
                v0_rec_rec.clear()
                for in__rec_rec_rec in in__rec_rec:
                    v0_rec_rec_rec.clear()
                    for item0_rec_rec_rec in in__rec_rec_rec:
                        v0_rec_rec_rec.push_back(deref(item0_rec_rec_rec.inst.get()))
                    v0_rec_rec.push_back(deref(v0_rec_rec_rec))
                v0_rec.push_back(deref(v0_rec_rec))
            v0.push_back(deref(v0_rec))
        cdef int _r = self.inst.get().process28(deref(v0))
        del v0
        del v0_rec
        del v0_rec_rec
        del v0_rec_rec_rec
        py_result = <int>_r
        return py_result
    
    def process29(self, list in_ ):
        """Cython signature: void process29(libcpp_vector[libcpp_vector[Int]] & in_)"""
        assert isinstance(in_, list) and all(isinstance(elemt_rec, list) and all(isinstance(elemt_rec_rec, Int) for elemt_rec_rec in elemt_rec) for elemt_rec in in_), 'arg in_ wrong type'
        cdef libcpp_vector[libcpp_vector[_Int]] * v0 = new libcpp_vector[libcpp_vector[_Int]]()
        cdef libcpp_vector[_Int] * v0_rec = new libcpp_vector[_Int]()
        cdef Int item0_rec
        cdef libcpp_vector[_Int].iterator it_in__rec
        for in__rec in in_:
            v0_rec.clear()
            for item0_rec in in__rec:
                v0_rec.push_back(deref(item0_rec.inst.get()))
            v0.push_back(deref(v0_rec))
        self.inst.get().process29(deref(v0))
        cdef libcpp_vector[libcpp_vector[_Int]].iterator it_in_ = v0.begin()
        replace_0 = []
        while it_in_ != v0.end():
            it_in__rec = deref(it_in_).begin()
            replace_1 = []
            while it_in__rec != deref(it_in_).end():
                item0_rec = Int.__new__(Int)
                item0_rec.inst = shared_ptr[_Int](new _Int(deref(it_in__rec)))
                replace_1.append(item0_rec)
                inc(it_in__rec)
            replace_0.append(replace_1)
            inc(it_in_)
        in_[:] = replace_0
        del v0
    
    def process30(self, list in_ ):
        """Cython signature: void process30(libcpp_vector[libcpp_vector[libcpp_vector[libcpp_vector[Int]]]] & in_)"""
        assert isinstance(in_, list) and all(isinstance(elemt_rec, list) and all(isinstance(elemt_rec_rec, list) and all(isinstance(elemt_rec_rec_rec, list) and all(isinstance(elemt_rec_rec_rec_rec, Int) for elemt_rec_rec_rec_rec in elemt_rec_rec_rec) for elemt_rec_rec_rec in elemt_rec_rec) for elemt_rec_rec in elemt_rec) for elemt_rec in in_), 'arg in_ wrong type'
        cdef libcpp_vector[libcpp_vector[libcpp_vector[libcpp_vector[_Int]]]] * v0 = new libcpp_vector[libcpp_vector[libcpp_vector[libcpp_vector[_Int]]]]()
        cdef libcpp_vector[libcpp_vector[libcpp_vector[_Int]]] * v0_rec = new libcpp_vector[libcpp_vector[libcpp_vector[_Int]]]()
        cdef libcpp_vector[libcpp_vector[libcpp_vector[_Int]]].iterator it_in__rec
        cdef libcpp_vector[libcpp_vector[_Int]] * v0_rec_rec = new libcpp_vector[libcpp_vector[_Int]]()
        cdef libcpp_vector[libcpp_vector[_Int]].iterator it_in__rec_rec
        cdef libcpp_vector[_Int] * v0_rec_rec_rec = new libcpp_vector[_Int]()
        cdef Int item0_rec_rec_rec
        cdef libcpp_vector[_Int].iterator it_in__rec_rec_rec
        for in__rec in in_:
            v0_rec.clear()
            for in__rec_rec in in__rec:
                v0_rec_rec.clear()
                for in__rec_rec_rec in in__rec_rec:
                    v0_rec_rec_rec.clear()
                    for item0_rec_rec_rec in in__rec_rec_rec:
                        v0_rec_rec_rec.push_back(deref(item0_rec_rec_rec.inst.get()))
                    v0_rec_rec.push_back(deref(v0_rec_rec_rec))
                v0_rec.push_back(deref(v0_rec_rec))
            v0.push_back(deref(v0_rec))
        self.inst.get().process30(deref(v0))
        cdef libcpp_vector[libcpp_vector[libcpp_vector[libcpp_vector[_Int]]]].iterator it_in_ = v0.begin()
        replace_0 = []
        while it_in_ != v0.end():
            it_in__rec = deref(it_in_).begin()
            replace_1 = []
            while it_in__rec != deref(it_in_).end():
                it_in__rec_rec = deref(it_in__rec).begin()
                replace_2 = []
                while it_in__rec_rec != deref(it_in__rec).end():
                    it_in__rec_rec_rec = deref(it_in__rec_rec).begin()
                    replace_3 = []
                    while it_in__rec_rec_rec != deref(it_in__rec_rec).end():
                        item0_rec_rec_rec = Int.__new__(Int)
                        item0_rec_rec_rec.inst = shared_ptr[_Int](new _Int(deref(it_in__rec_rec_rec)))
                        replace_3.append(item0_rec_rec_rec)
                        inc(it_in__rec_rec_rec)
                    replace_2.append(replace_3)
                    inc(it_in__rec_rec)
                replace_1.append(replace_2)
                inc(it_in__rec)
            replace_0.append(replace_1)
            inc(it_in_)
        in_[:] = replace_0
        del v0
    
    def process31(self, list in_ ):
        """Cython signature: int process31(libcpp_vector[int] in_)"""
        assert isinstance(in_, list) and all(isinstance(elemt_rec, (int, long)) for elemt_rec in in_), 'arg in_ wrong type'
        cdef libcpp_vector[int] v0 = in_
        cdef int _r = self.inst.get().process31(v0)
        
        py_result = <int>_r
        return py_result
    
    def process32(self, list in_ ):
        """Cython signature: int process32(libcpp_vector[libcpp_vector[int]] in_)"""
        assert isinstance(in_, list) and all(isinstance(elemt_rec, list) and all(isinstance(elemt_rec_rec, (int, long)) for elemt_rec_rec in elemt_rec) for elemt_rec in in_), 'arg in_ wrong type'
        cdef libcpp_vector[libcpp_vector[int]] v0 = in_
        cdef int _r = self.inst.get().process32(v0)
        
        py_result = <int>_r
        return py_result
    
    def process33(self, Int in_ ):
        """Cython signature: int process33(shared_ptr[Int] in_)"""
        assert isinstance(in_, Int), 'arg in_ wrong type'
        cdef shared_ptr[_Int] input_in_ = in_.inst
        cdef int _r = self.inst.get().process33(input_in_)
        py_result = <int>_r
        return py_result
    
    def process34(self, Int in_ ):
        """Cython signature: shared_ptr[Int] process34(shared_ptr[Int] in_)"""
        assert isinstance(in_, Int), 'arg in_ wrong type'
        cdef shared_ptr[_Int] input_in_ = in_.inst
        cdef shared_ptr[_Int] _r = self.inst.get().process34(input_in_)
        cdef Int py_result
        py_result = Int.__new__(Int)
        py_result.inst = _r
        return py_result
    
    def process35(self, Int in_ ):
        """Cython signature: shared_ptr[const Int] process35(shared_ptr[Int] in_)"""
        assert isinstance(in_, Int), 'arg in_ wrong type'
        cdef shared_ptr[_Int] input_in_ = in_.inst
        cdef shared_ptr[const _Int] _r = self.inst.get().process35(input_in_)
        # Const shared_ptr detected, we need to produce a non-const copy to stick into Python object
        cdef _Int * raw__r = new _Int((deref(<_Int * const>_r.get())))
        cdef Int py_result
        py_result = Int.__new__(Int)
        py_result.inst = shared_ptr[_Int](raw__r)
        return py_result
    
    def process36(self, Int in_ ):
        """Cython signature: int process36(Int * in_)"""
        assert isinstance(in_, Int), 'arg in_ wrong type'
    
        cdef int _r = self.inst.get().process36((in_.inst.get()))
        py_result = <int>_r
        return py_result
    
    def process37(self, Int in_ ):
        """Cython signature: Int * process37(Int * in_)"""
        assert isinstance(in_, Int), 'arg in_ wrong type'
    
        cdef  _Int * __r = (self.inst.get().process37((in_.inst.get())))
        if __r == NULL:
            return None
        cdef _Int * _r = new _Int(deref(__r))
        cdef Int py_result = Int.__new__(Int)
        py_result.inst = shared_ptr[_Int](_r)
        return py_result
    
    def process38(self,  in_0 ):
        """Cython signature: libcpp_vector[libcpp_vector[UInt]] process38(int)"""
        assert isinstance(in_0, (int, long)), 'arg in_0 wrong type'
    
        _r = self.inst.get().process38((<int>in_0))
        cdef list py_result = _r
        return py_result
    
    def process39(self, Int in_ ):
        """Cython signature: const Int * process39(Int * in_)"""
        assert isinstance(in_, Int), 'arg in_ wrong type'
    
        cdef const _Int * __r = (self.inst.get().process39((in_.inst.get())))
        if __r == NULL:
            return None
        cdef _Int * _r = new _Int(deref(__r))
        cdef Int py_result = Int.__new__(Int)
        py_result.inst = shared_ptr[_Int](_r)
        return py_result
    
    def _process40_0(self, ABS_Impl1 in_ ):
        """Cython signature: int process40(ABS_Impl1 * in_)"""
        assert isinstance(in_, ABS_Impl1), 'arg in_ wrong type'
    
        cdef int _r = self.inst.get().process40((in_.inst.get()))
        py_result = <int>_r
        return py_result
    
    def _process40_1(self, ABS_Impl2 in_ ):
        """Cython signature: int process40(ABS_Impl2 * in_)"""
        assert isinstance(in_, ABS_Impl2), 'arg in_ wrong type'
    
        cdef int _r = self.inst.get().process40((in_.inst.get()))
        py_result = <int>_r
        return py_result
    
    def process40(self, *args ):
        """
          - Cython signature: int process40(ABS_Impl1 * in_)
          - Cython signature: int process40(ABS_Impl2 * in_)
"""
        if (len(args)==1) and (isinstance(args[0], ABS_Impl1)):
            return self._process40_0(*args)
        elif (len(args)==1) and (isinstance(args[0], ABS_Impl2)):
            return self._process40_1(*args)
        else:
               raise Exception('can not handle type of %s' % (args,))
    
    def __richcmp__(self, other, op):
        if op not in (2, 3):
           op_str = {0: '<', 2: '==', 4: '>', 1: '<=', 3: '!=', 5: '>='}[op]
           raise Exception("comparions operator %s not implemented" % op_str)
        if not isinstance(other, LibCppTest):
            return False
        cdef LibCppTest other_casted = other
        cdef LibCppTest self_casted = self
        if op==2:
            return deref(self_casted.inst.get()) == deref(other_casted.inst.get())
        if op==3:
            return deref(self_casted.inst.get()) != deref(other_casted.inst.get()) 

cdef class EEE:
    A = 0
    B = 1

    def getMapping(self):
        return dict([ (v, k) for k, v in self.__class__.__dict__.items() if isinstance(v, int) ]) 
