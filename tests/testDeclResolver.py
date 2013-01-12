import pdb
import autowrap.DeclResolver as DeclResolver
import autowrap.PXDParser
import os
import autowrap.Utils

#TODO: use parse_str so that the pxd code is next to the testing code


def _resolve(*names):
    root = os.path.join(os.path.dirname(__file__), "test_files")
    return autowrap.DeclResolver.resolve_decls_from_files(*names, root=root)

def test_simple():
    cdcl, enumdcl = _resolve("minimal.pxd")
    assert cdcl.name == "Minimal"
    assert enumdcl.name == "ABCorD"

def test_singular():
    return  
    # TODO: this test is broken !
    resolved = _resolve("templates.pxd")

    assert len(resolved) == 2, len(resolved)
    res0, res1 = resolved
    assert res0.name == "TemplatesInt", res0.name
    assert res1.name == "TemplatesMixed", res1.name
    res0_restypes = map(str, (m.result_type for m in
        res0.get_flattened_methods()))

    assert res0_restypes == ['void', 'int', 'int', 'int', 'int', 'void',
                             'TemplatesInt',
                             'TemplatesMixed', 'Templates[double,float]',
                             'TemplatesInt'], res0_restypes

    res0_names =  map(lambda m: m.name, res0.get_flattened_methods())
    assert res0_names ==  ["TemplatesInt", "getA", "getB", "toA",
            "toB", "convert", "r0", "r1", "r2", "r3"], res0_names

    first_arg_names= map(lambda m: None if len(m.arguments) == 0 else
            str(m.arguments[0][0]), res0.get_flattened_methods())
    assert first_arg_names == ["a", None, None, None, None, "arg0", "",
                               "", None, ""], first_arg_names

    second_arg_names= map(lambda m: None if len(m.arguments) < 2 else
            str(m.arguments[1][0]), res0.get_flattened_methods())
    assert second_arg_names == ["b", None, None, None, None, "arg1", None,
                               None, None, ""], second_arg_names

    first_arg_types= map(lambda m: None if len(m.arguments) == 0 else
            str(m.arguments[0][1]), res0.get_flattened_methods())

    assert first_arg_types == ["int", None, None, None, None, "list[int]",
            "TemplatesMixed"  , "TemplatesInt", None , "int"], first_arg_types

    second_arg_types= map(lambda m: None if len(m.arguments) < 2 else
            str(m.arguments[1][1]), res0.get_flattened_methods())
    assert second_arg_types == ["int", None, None, None, None, "list[int] &",
                                    None, None, None , "int"], second_arg_types


    res1_restypes = map(lambda m: str(m.result_type),
            res1.get_flattened_methods())
    assert res1_restypes == ['void', 'int', 'float', 'int', 'float', 'void',
                             'TemplatesInt',
                             'TemplatesMixed', 'Templates[double,float]',
                             'TemplatesMixed'], res1_restypes

    res1_names =  map(lambda m: m.name, res1.get_flattened_methods())
    assert res1_names ==  ["TemplatesMixed", "getA", "getB", "toA",
            "toB", "convert", "r0", "r1", "r2", "r3"], res1_names

    first_arg_names= map(lambda m: None if len(m.arguments) == 0 else
            str(m.arguments[0][0]), res1.get_flattened_methods())
    assert first_arg_names == ["a", None, None, None, None, "arg0", "",
                               "", None, ""], first_arg_names

    second_arg_names= map(lambda m: None if len(m.arguments) < 2 else
            str(m.arguments[1][0]), res1.get_flattened_methods())
    assert second_arg_names == ["b", None, None, None, None, "arg1", None,
                               None, None, ""], second_arg_names

    first_arg_types= map(lambda m: None if len(m.arguments) == 0 else
            str(m.arguments[0][1]), res1.get_flattened_methods())

    assert first_arg_types == ["int", None, None, None, None, "list[int]",
            "TemplatesMixed"   , "TemplatesInt", None , "int"], first_arg_types

    second_arg_types= map(lambda m: None if len(m.arguments) < 2 else
            str(m.arguments[1][1]), res1.get_flattened_methods())
    assert second_arg_types == ["float", None, None, None, None,
                                "list[float] &", None, None, None,
                                "float"], second_arg_types


def test_multi_inherit():
    resolved = DeclResolver.resolve_decls_from_string("""
cdef extern from "A.h":
    cdef cppclass A[U]:
        # wrap-ignore
        void Afun(U, int)

cdef extern from "B.h":
    cdef cppclass B[X]:
        # wrap-ignore
        # wrap-inherits:
        #     A[X]
        X BIdentity(X)

cdef extern from "C.h":
    cdef cppclass C[Y]:
        # wrap-ignore
        # wrap-inherits:
        #     A[Y]
        void Cint(int, Y)

cdef extern from "D.h":
    cdef cppclass D[F, G]:
        # wrap-inherits:
        #  B[G]
        #  C[F]
        #
        # wrap-instances:
        #  D1[float,int]
        #  D2[int,float]
        pass
    """)

    data = dict()
    for class_instance in resolved:
        mdata = []
        for m in class_instance.get_flattened_methods():
            li = [ str(m.result_type), m.name ]
            li += [ str(t) for n, t in m.arguments ]
            mdata.append(li)
        data[class_instance.name] = mdata

    assert data == {'D1': [['void', u'Afun', 'int', 'int'],
                            ['void', u'Afun', 'float', 'int'],
                            ['int', u'BIdentity', 'int'],
                            ['void', u'Cint', 'int', 'float']],
                    'D2': [['void', u'Afun', 'float', 'int'],
                            ['void', u'Afun', 'int', 'int'],
                            ['float', u'BIdentity', 'float'],
                            ['void', u'Cint', 'int', 'int']]}

def expect_exception(fun):
    def wrapper(*a, **kw):
        try:
            fun(*a, **kw)
        except Exception:
            if 0:
                print "info: expected excption. here some more info:"
                import traceback
                traceback.print_exc()
                print
            pass
        else:
            assert False, "%s did not raise exception" % fun
    # set name, so that test frame work recognizes wrapped function
    wrapper.__name__ = fun.__name__+"__exception_wrapped"
    return wrapper

@expect_exception
def test_cycle_detection_in_class_hierarchy0():
    _resolve("Cycle0.pxd", "Cycle1.pxd", "Cycle2.pxd")

@expect_exception
def test_cycle_detection_in_class_hierarchy1():
    _resolve("Cycle1.pxd", "Cycle2.pxd", "Cycle0.pxd")

@expect_exception
def test_cycle_detection_in_class_hierarchy2():
    _resolve("Cycle2.pxd", "Cycle0.pxd", "Cycle1.pxd")


#@expect_exception
#def test_template_class_without_wrapas():
    #DeclResolver.resolve_decls_from_string("""
#cdef extern from "A.h":
    #cdef cppclass A[U]:
            #A()
                   #""")

def test_nested_templates():

    i1, i2, = DeclResolver.resolve_decls_from_string("""
from libcpp.string cimport string as libcpp_string
from libcpp.vector cimport vector as libcpp_vector

cdef extern from "templated.hpp":

    cdef cppclass T:
        T(int)
        T(T) # wrap-ignore
        int get()

    cdef cppclass Templated[X]:
        # wrap-instances:
        #   Templated[T]
        Templated(X)
        libcpp_vector[Templated[X]] reverse(libcpp_vector[Templated[X]] v)
        int getTwice(Templated[X])
            """)

    rev, = i2.methods.get("reverse")
    (n, t), = rev.arguments
    assert str(t) == "libcpp_vector[Templated[T]]"

    rev, = i2.methods.get("getTwice")
    (n, t), = rev.arguments
    assert str(t) == "Templated[T]", str(t)

def test_non_template_class_with_annotation():
    instance, = DeclResolver.resolve_decls_from_string("""
cdef extern from "A.h":
    cdef cppclass A:
        # wrap-instances:
        #  B
        pass
    """)
    assert instance.name == "B"

def test_template_class_with_ptrtype():
    instance, = DeclResolver.resolve_decls_from_string("""
cdef extern from "A.h":
    cdef cppclass A[X]:
        # wrap-instances:
        #  Ax[int*]
        pass
    """)
    assert instance.name == "Ax"
    assert map(str, instance.type_.template_args) == ["int *"], map(str,
            instance.type_.template_args)

def test_multi_decls_in_one_file():
    inst1, inst2, enum = DeclResolver.resolve_decls_from_string("""
cdef extern from "A.h":
    cdef cppclass A[B,C] :
        # wrap-instances:
        #   A[int,int]
        pass

    cdef cppclass C[E] :
        # wrap-instances:
        #   C[float]
        pass

    cdef enum F:
            G, H=4, I
    """)
    assert inst1.name == "A"
    T1, T2 =  inst1.cpp_decl.template_parameters
    assert T1 == "B", T1
    assert T2 == "C", T2
    assert len(inst1.methods) == 0

    assert inst2.name == "C"
    T1, =  inst2.cpp_decl.template_parameters
    assert T1 == "E", T1
    assert len(inst2.methods) == 0

    assert enum.name == "F"
    G, H, I = enum.items
    assert G == ("G", 0)
    assert H == ("H", 4)
    assert I == ("I", 5)


def test_int_container():
    resolved  = _resolve("int_container_class.pxd")
    assert resolved[0].name == "Xint"
    assert [ m.name for m in resolved[0].get_flattened_methods()] == ["Xint", "operator+",
    "getValue"]
    assert resolved[1].name == "XContainerInt"
    assert [ m.name for m in resolved[1].get_flattened_methods()] == ["XContainerInt",
            "push_back", "size",]

def test_typedef_with_fun():
    resolved, = DeclResolver.resolve_decls_from_string("""
cdef extern from "X.h":
    ctypedef int X
    X fun(X x)
            """)
    assert resolved.name == "fun"
    assert str(resolved.result_type) == "int"
    (n, t), = resolved.arguments
    assert n == "x"
    assert str(t) == "int"

def test_typedef_chaining():
    function, = DeclResolver.resolve_decls_from_string("""
cdef extern from "X.h":
    ctypedef int X
    ctypedef X* iptr
    ctypedef X Y
    ctypedef Y *iptr2
    iptr2 fun(iptr, Y *)
            """)

    assert str(function.result_type) == "int *"
    t1, t2 = map(str, (t for (n, t) in function.arguments))
    assert t1 == "int *", t1
    assert t2 == "int *", t2

@expect_exception
def double_ptr_typedef():
    function, = DeclResolver.resolve_decls_from_string("""
cdef extern from "X.h":
    ctypedef int X
    ctypedef X * iptr
    ctypedef X * Y
    ctypedef Y * iptr2
    iptr2 fun(iptr, Y)
            """)

@expect_exception
def ctypedef_with_cycle():
    function, = DeclResolver.resolve_decls_from_string("""
cdef extern from "X.h":
    ctypedef int X
    ctypedef X Y
    ctypedef Y Z
    ctypedef Z X
    iptr2 fun(iptr, Y)
            """)

def test_typedef_with_class():
    resolved, fun = DeclResolver.resolve_decls_from_string("""
cdef extern from "X.h":
    ctypedef int X
    ctypedef int * Y
    cdef cppclass A[B]:
        # wrap-instances:
        #   A[X]
        X foo(B)
        B bar(X *)
    Y fun(X *)
            """)
    assert resolved.name == "A"
    tinstance, = resolved.type_.template_args
    assert str(tinstance) == "int"

    foo, = resolved.methods.get("foo")
    assert str(foo.result_type) == "int", foo.result_type
    (__, arg_t), = foo.arguments
    assert str(arg_t) == "int"

    bar, = resolved.methods.get("bar")
    assert str(bar.result_type) == "int"
    (__, arg_t), = bar.arguments
    assert str(arg_t) == "int *"

    assert fun.name == "fun"
    assert str(fun.result_type) == "int *"
    (__, arg_t), = fun.arguments
    assert str(arg_t) == "int *", str(arg_t)


def test_typedef_with_class2():
    resolved, = DeclResolver.resolve_decls_from_string("""
cdef extern from "X.h":
    ctypedef int X
    cdef cppclass A[B]:
        # wrap-instances:
        #   A[X *]
        X foo(B)
        B bar(X)
            """)
    assert resolved.name == "A"
    tinstance, = resolved.type_.template_args

    assert str(tinstance) == "int *", str(tinstance)

    foo, = resolved.methods.get("foo")
    assert str(foo.result_type) == "int", foo.result_type
    (__, arg_t), = foo.arguments
    assert str(arg_t) == "int *", str(arg_t)

    bar, = resolved.methods.get("bar")
    assert str(bar.result_type) == "int *"
    (__, arg_t), = bar.arguments
    assert str(arg_t) == "int", str(arg_t)

def test_typedef_with_class3():
    resolved, = DeclResolver.resolve_decls_from_string("""
cdef extern from "X.h":
    ctypedef int X
    cdef cppclass A[B,C]:
        # wrap-instances:
        #   A[X *,int]
        X foo(C*)
        C* bar(B)
            """)
    assert resolved.name == "A"
    tinst1, tinst2 = resolved.type_.template_args

    assert str(tinst1) == "int *", str(tinst1)
    assert str(tinst2) == "int", str(tinst2)

    foo, = resolved.methods.get("foo")
    assert str(foo.result_type) == "int", foo.result_type
    (__, arg_t), = foo.arguments
    assert str(arg_t) == "int *", str(arg_t)

    bar, = resolved.methods.get("bar")
    assert str(bar.result_type) == "int *"
    (__, arg_t), = bar.arguments
    assert str(arg_t) == "int *", str(arg_t)

def test_without_header():
    return
    resolved, = DeclResolver.resolve_decls_from_string("""
cdef extern:
    ctypedef int X
    X fun(X x)
            """)

def test_method_return_values():
    resolved, = DeclResolver.resolve_decls_from_string("""
cdef extern from "minimal.hpp":
    cdef cppclass Minimal:
        Minimal create()
""")
    meth, = resolved.methods.get("create")

def test_class_and_enum():
    A, E = DeclResolver.resolve_decls_from_string("""
cdef extern from "":

    cdef cppclass A:
        A()

    cdef enum E:
                A, B, C
    """)

    assert A.name == "A"
    method, = A.methods.values()[0]
    assert method.name == "A"
    assert len(method.arguments) == 0

    assert E.name == "E"
    A, B, C = E.items
    assert A == ("A", 0)
    assert B == ("B", 1)
    assert C == ("C", 2)
