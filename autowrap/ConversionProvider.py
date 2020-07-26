# encoding: utf-8


# left Map and Vector converter for now.

from collections import defaultdict

from autowrap.Types import CppType  # , printable
from autowrap.Code import Code

import logging as L
import string

try:
    unicode = unicode
except NameError:
    # 'unicode' is undefined, must be Python 3
    str = str
    unicode = str
    bytes = bytes
    basestring = (str, bytes)
else:
    # 'unicode' exists, must be Python 2
    str = str
    unicode = unicode
    bytes = str
    basestring = basestring


def mangle(s):
    s = s.replace("(", "_l_")
    s = s.replace(")", "_r_")
    s = s.replace("<", "_lt_")
    s = s.replace(">", "_gt_")
    s = s.replace("[", "_lb_")
    s = s.replace("]", "_rb_")
    s = s.replace(".", "_dot_")
    return s


class ConverterRegistry(object):

    """
    Works with two level lookup: first find converters which support base_type,
    then call .matches on them to find the finally matching converters

    Therefore TypeConverterBase has methods .get_base_types and .matches
    """

    def __init__(self, instance_mapping, names_of_classes_to_wrap,
                 names_of_enums_to_wrap):

        self.lookup = defaultdict(list)

        self.names_of_wrapper_classes = list(instance_mapping.keys())
        self.names_of_wrapper_classes += ["const %s" % k for k in instance_mapping.keys()]
        self.names_of_classes_to_wrap = names_of_classes_to_wrap
        self.names_of_enums_to_wrap = names_of_enums_to_wrap

        self.process_and_set_type_mapping(instance_mapping)

    def process_and_set_type_mapping(self, instance_mapping):
        # as original c++ class decls are decorated with a beginning "_"
        # we have to process  the instance mapping:

        map_ = dict((name, CppType("_" + name)) for name in
                    self.names_of_classes_to_wrap + self.names_of_enums_to_wrap)
        self.instance_mapping = dict()
        for alias, type_ in instance_mapping.items():
            self.instance_mapping[alias] = type_.transformed(map_)

    def register(self, converter):

        assert isinstance(converter, TypeConverterBase)
        L.info("register %s" % converter)
        converter._set_converter_registry(self)

        # we take a defaultdict(list) for lookup as base_type is only some kind
        # of "hash value" to speedup lookup. what finally counts is the matches
        # method of the converters, see get() below:

        for base_type in converter.get_base_types():
            self.lookup[base_type].append(converter)

    def get(self, cpp_type):

        rv = [conv for conv in self.lookup[cpp_type.base_type]
              if conv.matches(cpp_type)]
        if len(rv) < 1:
            raise Exception("no converter for %s" % cpp_type)

        # allways take the latest converter which allows overwriting existing
        # standard converters !
        return rv[-1]

    def __contains__(self, cpp_type):
        try:
            self.get(cpp_type)
            return True
        except:
            return False

    def cython_type(self, type_):
        if isinstance(type_, basestring):
            type_ = CppType(type_)
        return type_.transformed(self.instance_mapping)


class TypeConverterBase(object):

    def set_enums_to_wrap(self, enums_to_wrap):
        self.enums_to_wrap = enums_to_wrap

    def _set_converter_registry(self, r):
        self.converters = r

    def get_base_types(self):
        """
        for first level lookup in registry
        """
        raise NotImplementedError()

    def matches(self, cpp_type):
        """
        for second level lookup in registry
        """
        raise NotImplementedError()

    def call_method(self, res_type, cy_call_str):
        """
        Creates a temporary object which has the type of the current TypeConverter object.

        The object is *always* named "py_ans" and will be of type "res_type". It
        will be assigned the value of "cy_call_str".

        Note that Cython cannot declare C++ references, therefore 

           cdef int & _r 

        Is illegal to declare and we have to remove any references from the type.
        """
        cy_res_type = self.converters.cython_type(res_type)
        if cy_res_type.is_ref:
            cy_res_type = cy_res_type.base_type
            return "py_ans = %s" % cy_call_str
            # return "cdef %s _r = %s" % cy_call_str

        return "py_ans = %s" % cy_call_str
        # return "cdef %s _r = %s" % (cy_res_type, cy_call_str)

    def matching_python_type(self, cpp_type):
        raise NotImplementedError()

    def type_check_expression(self, cpp_type, argument_var):
        raise NotImplementedError()

    def input_conversion(self, cpp_type, argument_var, arg_num):
        """
        Sets up the conversion of input arguments.

        Returns a tuple as follows:
          - code : a code object to be added to the beginning of the function
          - call_as : a piece of code indicating how the argument should be called as going forward
          - cleanup : a piece of cleanup code to be added to the bottom of the function
        """
        raise NotImplementedError()

    def output_conversion(self, cpp_type, input_cpp_var, output_py_var):
        raise NotImplementedError()


    def _codeForInstantiateObjectFromIter(self, cpp_type, it):
        """
        Code for new object instantation from iterator (double deref for iterator-ptr)

        Note that if cpp_type is a pointer and the iterator therefore refers to
        a STL object of std::vector< _FooObject* >, then we need the base type
        to instantate a new object and dereference twice.

        Example output:
            shared_ptr[ _FooObject ] (new _FooObject (*foo_iter)  )
            shared_ptr[ _FooObject ] (new _FooObject (**foo_iter_ptr)  )
        """

        if cpp_type.is_ref:
            cpp_type = cpp_type.base_type

        if cpp_type.is_ptr:
            cpp_type_base = cpp_type.base_type
            return string.Template("shared_ptr[$cpp_type_base](new $cpp_type_base(deref(deref($it))))").substitute(locals())
        else:
            return string.Template("shared_ptr[$cpp_type](new $cpp_type(deref($it)))").substitute(locals())

class VoidConverter(TypeConverterBase):

    def get_base_types(self):
        """
        for first level lookup in registry
        """
        return "void",

    def matches(self, cpp_type):
        """
        for second level lookup in registry
        """
        return not cpp_type.is_ptr

    def call_method(self, res_type, cy_call_str):
        return cy_call_str

    def matching_python_type(self, cpp_type):
        raise NotImplementedError("void has no matching python type")

    def type_check_expression(self, cpp_type, argument_var):
        raise NotImplementedError("void has no matching python type")

    def input_conversion(self, cpp_type, argument_var, arg_num):
        raise NotImplementedError("void has no matching python type")

    def output_conversion(self, cpp_type, input_cpp_var, output_py_var):
        return None


class IntegerConverter(TypeConverterBase):

    """
    wraps long and int. "long" base_type is converted to "int" by the
    cython parser !
    """

    def get_base_types(self):
        return ("int", "bool", "long", "int32_t", "ptrdiff_t", "int64_t",
                "uint32_t", "uint64_t", "size_t")

    def matches(self, cpp_type):
        return not cpp_type.is_ptr

    def matching_python_type(self, cpp_type):
        return ""

    def type_check_expression(self, cpp_type, argument_var):
        return " (is_scalar_integer(%s) || is_scalar_double(%s)) && %s == as.integer(%s)" % (argument_var,argument_var,argument_var,argument_var)
        #return "isinstance(%s, (int, long))" % (argument_var,)

    def input_conversion(self, cpp_type, argument_var, arg_num):
        code = ""
        call_as = "as.integer(%s)" % argument_var
        #call_as = "(<%s>%s)" % (cpp_type, argument_var)
        cleanup = ""
        return code, call_as, cleanup

    def output_conversion(self, cpp_type, input_cpp_var, output_py_var):
        return "    %s = %s" % (output_py_var, input_cpp_var)
        #return "%s = <%s>%s" % (output_py_var, cpp_type, input_cpp_var)


# TODO: common base class for float, int, str conversion

class DoubleConverter(TypeConverterBase):

    def get_base_types(self):
        return "double",

    def matches(self, cpp_type):
        return not cpp_type.is_ptr

    def matching_python_type(self, cpp_type):
        return "double"

    # def type_check_expression(self, cpp_type, argument_var):
    #     return "isinstance(%s, float)" % (argument_var,)

    def type_check_expression(self, cpp_type, argument_var):
        return "is_scalar_double(%s)" % (argument_var,)

    def input_conversion(self, cpp_type, argument_var, arg_num):
        code = ""
        call_as = "%s" % argument_var
        #call_as = "(<%s>%s)" % (cpp_type, argument_var)
        cleanup = ""
        return code, call_as, cleanup

    def output_conversion(self, cpp_type, input_cpp_var, output_py_var):
        return "    %s = %s" % (output_py_var,input_cpp_var)
        #return "%s = <%s>%s" % (output_py_var, cpp_type, input_cpp_var)


class FloatConverter(TypeConverterBase):

    def get_base_types(self):
        return "float",

    def matches(self, cpp_type):
        return not cpp_type.is_ptr

    def matching_python_type(self, cpp_type):
        #return "float"
        return "double"

    # def type_check_expression(self, cpp_type, argument_var):
    #     return "isinstance(%s, float)" % (argument_var,)

    def type_check_expression(self, cpp_type, argument_var):
        return "is_scalar_double(%s)" % (argument_var,)
        #return "isinstance(%s, float)" % (argument_var,)

    def input_conversion(self, cpp_type, argument_var, arg_num):
        code = ""
        call_as = "%s" % argument_var
        #call_as = "(<%s>%s)" % (cpp_type, argument_var)
        cleanup = ""
        return code, call_as, cleanup

    def output_conversion(self, cpp_type, input_cpp_var, output_py_var):
        return "    %s = %s" % (output_py_var,input_cpp_var)
        #return "%s = <%s>%s" % (output_py_var, cpp_type, input_cpp_var)


class EnumConverter(TypeConverterBase):

    def __init__(self, enum):
        self.enum = enum

    def get_base_types(self):
        return self.enum.name,

    def matches(self, cpp_type):
        return not cpp_type.is_ptr

    def matching_python_type(self, cpp_type):
        return "int"

    def type_check_expression(self, cpp_type, argument_var):
        values = ", ".join(str(v) for (__, v) in self.enum.items)
        return "{} %in% c({})".format(argument_var, values)
        #return "%s in [%s]" % (argument_var, values)

    def input_conversion(self, cpp_type, argument_var, arg_num):
        code = ""
        call_as = "%s" % argument_var
        # call_as = "(<_%s>%s)" % (cpp_type.base_type, argument_var)
        cleanup = ""
        return code, call_as, cleanup

    def output_conversion(self, cpp_type, input_cpp_var, output_py_var):
        return "    %s = %s" % (output_py_var, input_cpp_var)
        #return "%s = <int>%s" % (output_py_var, input_cpp_var)


class CharConverter(TypeConverterBase):

    def get_base_types(self):
        return "char",

    def matches(self, cpp_type):
        return not cpp_type.is_ptr

    def matching_python_type(self, cpp_type):
        return "bytes"

    def type_check_expression(self, cpp_type, argument_var):
        return "is_scalar_character(%s)" % (argument_var,)
        #return "isinstance(%s, bytes) and len(%s) == 1" % (argument_var, argument_var,)

    # not final
    def input_conversion(self, cpp_type, argument_var, arg_num):
        code = "py_run_string(\"%s = bytes(%s)\")" % (argument_var, argument_var,)
        call_as = "py$%s" % argument_var
        cleanup = "py_run_string(\"del %s\")" % argument_var
        #code = ""
        #call_as = "(<char>((%s)[0]))" % argument_var
        #cleanup = ""
        return code, call_as, cleanup

    def call_method(self, res_type, cy_call_str):
        return "py_ans = %s" % cy_call_str

    def output_conversion(self, cpp_type, input_py_var, output_r_var):
        return "    %s = as.character(%s)" % (output_r_var, input_py_var)

    # def output_conversion(self, cpp_type, input_cpp_var, output_py_var):
    #     return "%s = chr(<char>(%s))" % (output_py_var, input_cpp_var)


class ConstCharPtrConverter(TypeConverterBase):

    def get_base_types(self):
        return "const_char",

    def matches(self, cpp_type):
        return cpp_type.is_ptr

    def matching_python_type(self, cpp_type):
        return "bytes"

    def type_check_expression(self, cpp_type, argument_var):
        return "is_scalar_character(%s)" % (argument_var,)
        #return "isinstance(%s, bytes)" % (argument_var,)

    def input_conversion(self, cpp_type, argument_var, arg_num):
        code = "py_run_string(\"%s = bytes(%s)\")" % (argument_var, argument_var,)
        call_as = "py%s" % argument_var
        cleanup = "py_run_string(\"del %s\")" % argument_var
        # code = Code().add("cdef const_char * input_%s = <const_char *> %s" % (argument_var, argument_var))
        # call_as = "input_%s" % argument_var
        # cleanup = ""
        return code, call_as, cleanup

    def call_method(self, res_type, cy_call_str):
        return "py_ans = %s" % cy_call_str

    def output_conversion(self, cpp_type, input_py_var, output_r_var):
        return "    %s = as.character(%s)" % (output_r_var, input_py_var)

    # def output_conversion(self, cpp_type, input_cpp_var, output_py_var):
    #     return "%s = <const_char *>(%s)" % (output_py_var, input_cpp_var)


class CharPtrConverter(TypeConverterBase):

    def get_base_types(self):
        return "char",

    def matches(self, cpp_type):
        return cpp_type.is_ptr


    def matching_python_type(self, cpp_type):
        return "bytes"

    def type_check_expression(self, cpp_type, argument_var):
        return "is_scalar_character(%s)" % (argument_var,)
        #return "isinstance(%s, bytes)" % (argument_var,)

    def input_conversion(self, cpp_type, argument_var, arg_num):
        code = "py_run_string(\"%s = bytes(%s)\")" % (argument_var, argument_var,)
        call_as = "%s" % argument_var
        cleanup = "py_run_string(\"del %s\")" % argument_var
        # code = ""
        # call_as = "(<char *>%s)" % argument_var
        # cleanup = ""
        return code, call_as, cleanup

    def call_method(self, res_type, cy_call_str):
        return "py_ans = %s" % cy_call_str

    def output_conversion(self, cpp_type, input_py_var, output_r_var):
        return "    %s = as.character(%s)" % (output_r_var, input_py_var)

    # def output_conversion(self, cpp_type, input_cpp_var, output_py_var):
    #     return "%s = <char *>(%s)" % (output_py_var, input_cpp_var)


class TypeToWrapConverter(TypeConverterBase):

    def __init__(self, class_):
        self.class_ = class_

    def get_base_types(self):
        return self.class_.name,

    def matches(self, cpp_type):
        return True

    def matching_python_type(self, cpp_type):
        return cpp_type.base_type

    #not final
    def type_check_expression(self, cpp_type, argument_var):
        # cpp_type = cpp_type.replace('&','')
        return "is.R6({}) && class({})[1] == \"{}\"".format(argument_var,argument_var,cpp_type.base_type)
        #return "isinstance(%s, %s)" % (argument_var, cpp_type.base_type)

    def input_conversion(self, cpp_type, argument_var, arg_num):
        code = ""
        call_as = "%s" % (argument_var, )
        cy_type = self.converters.cython_type(cpp_type)
        #code = ""
        # if cpp_type.is_ptr:
        #     call_as = "(%s.inst.get())" % (argument_var, )
        # else:
        #     call_as = "(deref(%s.inst.get()))" % (argument_var, )

        cleanup = ""
        return code, call_as, cleanup

    def call_method(self, res_type, cy_call_str):
        t = self.converters.cython_type(res_type)

        if t.is_ref:
            # If t is a ref, we would like to call on the base type
            t = t.base_type
        elif t.is_ptr:

            # Special treatment for const raw ptr
            const = ""
            if t.is_const:
                const = "const"

            # If t is a pointer, we would like to call on the base type
            t = t.base_type
            code = Code().add("""
                |py_ans = $cy_call_str
                |if( is.null(py_ans) ) {
                |    return(NULL)
                |}
                """, locals())
            return code

        return "py_ans = %s" % (cy_call_str)

    def output_conversion(self, cpp_type, input_py_var, output_r_var):

        cy_clz = self.converters.cython_type(cpp_type)

        # Need to ensure that type inside the raw ptr is an object and not a ref/ptr
        if cpp_type.is_ptr or cpp_type.is_ref:
            cy_clz = cy_clz.base_type


        # output_py_var : output_r_var
        # input_cpp_var : input_py_var
        t = cpp_type.base_type
        return Code().add("""
                      |$output_r_var = $t$$new($input_py_var)
        """, locals())
        # return Code().add("""
        #               |cdef $t $output_py_var = $t.__new__($t)
        #               |$output_py_var.inst = shared_ptr[$cy_clz]($input_cpp_var)
        # """, locals())


class StdPairConverter(TypeConverterBase):

    # remark: we use list instead of tuple internally, in order to
    # provide call by ref args. Python tuples are immutable.

    def get_base_types(self):
        return "libcpp_pair",

    def matches(self, cpp_type):
        return True

    def matching_python_type(self, cpp_type):
        return "list"

    def type_check_expression(self, cpp_type, arg_var):
        t1, t2, = cpp_type.template_args
        inner_conv1 = self.converters.get(t1)
        inner_conv2 = self.converters.get(t2)
        assert inner_conv1 is not None, "arg type %s not supported" % t1
        assert inner_conv2 is not None, "arg type %s not supported" % t2
        inner_check1 = inner_conv1.type_check_expression(t1, "%s[[1]]" % arg_var)
        inner_check2 = inner_conv2.type_check_expression(t2, "%s[[2]]" % arg_var)

        return Code().add("""
          |is_list($arg_var) && length($arg_var) == 2 && $inner_check1
          + && $inner_check2
          """, locals()).render()
        # return Code().add("""
        #   |isinstance($arg_var, list) and len($arg_var) == 2 and $inner_check1
        #   + and $inner_check2
        #   """, locals()).render()

    def input_conversion(self, cpp_type, argument_var, arg_num):
        t1, t2, = cpp_type.template_args
        cr_ref = False
        temp_var = "v%d" % arg_num
        i1 = self.converters.cython_type(t1)
        inner_conv1 = self.converters.get(t1)
        i2 = self.converters.cython_type(t2)
        inner_conv2 = self.converters.get(t2)

        code = Code()
        if i1.is_enum:
            assert not t1.is_ptr
            arg0 = "%s[[1]]" % (argument_var)
            #arg0 = "(<%s>%s[0])" % (t1, argument_var)
        elif t1.base_type in self.converters.names_of_wrapper_classes:
            assert not t1.is_ptr
            arg0 = "%s[[1]]$.__enclos_env__$private$py_obj" % (argument_var)
            #arg0 = "deref((<%s>%s[0]).inst.get())" % (t1, argument_var)
        else:
            if isinstance(inner_conv1,StdStringConverter):
                code.add("""
                    |py$$str0 <- $argument_var[[1]]
                    |py_run_string(\"str0 = bytes(str0,'utf-8')\")
                    """, locals())
                arg0 = "py$str0"
            else:
                if isinstance(inner_conv1,IntegerConverter):
                    arg0 = "as.integer(%s[[1]])" % argument_var
                else:
                    arg0 = "%s[[1]]" % argument_var
        if i2.is_enum:
            assert not t2.is_ptr
            arg1 = "%s[[2]]"
            # arg1 = "(<%s>%s[0])" % (t2, argument_var)
        elif t2.base_type in self.converters.names_of_wrapper_classes:
            assert not t2.is_ptr
            arg1 = "%s[[2]]$.__enclos_env__$private$py_obj" % (argument_var)
            # arg1 = "deref((<%s>%s[1]).inst.get())" % (t2, argument_var)
        else:
            if isinstance(inner_conv2,StdStringConverter):
                code.add("""
                    |py$$str1 <- $argument_var[[2]]
                    |py_run_string(\"str1 = bytes(str1,'utf-8')\")
                    """, locals())
                arg1 = "py$str1"
            else:
                if isinstance(inner_conv2,IntegerConverter):
                    arg1 = "as.integer(%s[[2]])" % argument_var
                else:
                    arg1 = "%s[[2]]" % argument_var

        code.add("""
            |$temp_var = r_to_py(list($arg0,$arg1))
            """, locals())
        # code = Code().add("""
        #     |cdef libcpp_pair[$i1, $i2] $temp_var
        #     |$temp_var.first = $arg0
        #     |$temp_var.second = $arg1
        #     """, locals())

        cleanup_code = Code()
        if cpp_type.is_ref and not cpp_type.is_const:
            cr_ref = True
            if not i1.is_enum and t1.base_type in self.converters.names_of_wrapper_classes:
                temp1 = "temp1"
                cleanup_code.add("""
                    |$temp1 = $t1$$new(py_to_r($temp_var[0]))
                                   """, locals())
                # cleanup_code.add("""
                #     |cdef $t1 $temp1 = $t1.__new__($t1)
                #     |$temp1.inst = shared_ptr[$i1](new $i1($temp_var.first))
                #                    """, locals())
            else:
                temp1 = "py_to_r(%s[0])" % temp_var
            if not i2.is_enum and t2.base_type in self.converters.names_of_wrapper_classes:
                temp2 = "temp2"
                cleanup_code.add("""
                    |$temp2 = $t2$$new(py_to_r($temp_var[1]))
                                   """, locals())
                # cleanup_code.add("""
                #     |cdef $t2 $temp2 = $t2.__new__($t2)
                #     |$temp2.inst = shared_ptr[$i2](new $i2($temp_var.second))
                #                    """, locals())
            else:
                temp2 = "py_to_r(%s[1])" % temp_var

            cleanup_code.add("""
                |byref_$arg_num = list($temp1, $temp2)
                """, locals())
            # cleanup_code.add("""
            #     |$argument_var[:] = [$temp1, $temp2]
            #     """, locals())
        cleanup = False
        if isinstance(inner_conv1,StdStringConverter):
            cleanup_code.add("""
                |py_run_string("del str0")
                """)
            cleanup = True
        if isinstance(inner_conv2,StdStringConverter):
            cleanup_code.add("""
                |py_run_string("del str1")
                """)
            cleanup = True
        if cleanup:
            cleanup_code.add("""
                |py_run_string("gc.collect()")
                """)
        return code, "%s" % temp_var, cleanup_code, cr_ref

    def call_method(self, res_type, cy_call_str):
        return "py_ans = %s" % (cy_call_str)

    # output is R list. chnaged input_cpp_var -> input_r_var & output_py_var to output_r_var (Since, automatic conversion takes place)
    def output_conversion(self, cpp_type, input_r_var, output_r_var):

        assert not cpp_type.is_ptr
        t1, t2, = cpp_type.template_args
        cr_ref = False

        i1 = self.converters.cython_type(t1)
        inner_conv1 = self.converters.get(t1)
        i2 = self.converters.cython_type(t2)
        inner_conv2 = self.converters.get(t2)

        code = Code()

        if i1.is_enum:
            out1 = "out1"
            code.add("""$out1 = $input_r_var[[1]]
                       """, locals())
            # code.add("""cdef $i1 out1 = (<$i1> $input_cpp_var.first)
            #            """, locals())

        elif t1.base_type in self.converters.names_of_wrapper_classes:
            out1 = "out1"
            # input_cpp_var : input_r_var
            code.add("""out1 = $t1$$new($input_r_var[[1]])
                       """, locals())
            # code.add("""cdef $t1 out1 = $t1.__new__($t1)
            #            |out1.inst = shared_ptr[$i1](new $i1($input_cpp_var.first))
            #            """, locals())
        else:
            if isinstance(inner_conv1,StdStringConverter):
                out1 = "as.character(%s[[1]])" % input_r_var
            else:
                out1 = "%s[[1]]" % input_r_var
        if i2.is_enum:
            out2 = "out2"
            code.add("""out2 = $input_r_var[[2]]
                       """, locals())
            # code.add("""cdef $i2 out2 = (<$i2> $input_cpp_var.second)
            #            """, locals())
        elif t2.base_type in self.converters.names_of_wrapper_classes:
            out2 = "out2"
            code.add("""out2 = $t2$$new($input_r_var[[2]])
                       """, locals())
            # code.add("""cdef $t2 out2 = $t2.__new__($t2)
            #            |out2.inst = shared_ptr[$i2](new $i2($input_cpp_var.second))
            #            """, locals())
        else:
            if isinstance(inner_conv2,StdStringConverter):
                out2 = "as.character(%s[[2]])" % input_r_var
            else:
                out2 = "%s[[2]]" % input_r_var


        code.add("""$output_r_var = list($out1, $out2)
            """, locals())
        # code.add("""cdef list $output_py_var = [$out1, $out2]
        #     """, locals())
        return code


class StdMapConverter(TypeConverterBase):

    def get_base_types(self):
        return "libcpp_map",

    def matches(self, cpp_type):
        return True

    def matching_python_type(self, cpp_type):
        return "dict"

    def type_check_expression(self, cpp_type, arg_var):
        tt_key, tt_value = cpp_type.template_args
        inner_conv_1 = self.converters.get(tt_key)
        inner_conv_2 = self.converters.get(tt_value)
        assert inner_conv_1 is not None, "arg type %s not supported" % tt_key
        assert inner_conv_2 is not None, "arg type %s not supported" % tt_value

        inner_check_1 = inner_conv_1.type_check_expression(tt_key, "k")
        inner_check_2 = inner_conv_2.type_check_expression(tt_value, "v")
        if isinstance(inner_conv_1,(IntegerConverter,DoubleConverter,FloatConverter,EnumConverter)):
            return Code().add("""
              |is_list($arg_var)
              + && ifelse(length($arg_var)==0,TRUE,!is.null(names($arg_var))
              + && all(sapply( as.numeric(names($arg_var)),function(k) $inner_check_1)) && all(sapply($arg_var,function(v) $inner_check_2))
              + && length(unique(names($arg_var))) == length(names($arg_var)))
              """, locals()).render()
        elif isinstance(inner_conv_1,(CharConverter,StdStringConverter)):
            return Code().add("""
              |is_list($arg_var)
              + && ifelse(length($arg_var)==0,TRUE,!is.null(names($arg_var))
              + && all(sapply($arg_var,function(v) $inner_check_2))
              + && length(unique(names($arg_var))) == length(names($arg_var)))
              """, locals()).render()
        else:
            return Code().add("""
              |is_list($arg_var)
              + && ifelse(length($arg_var)==0,TRUE,!is.null(names($arg_var))
              + && all(sapply( names($arg_var),function(k) $inner_check_1)) && all(sapply($arg_var,function(v) $inner_check_2))
              + && length(unique(names($arg_var))) == length(names($arg_var)))
              """, locals()).render()

        # return Code().add("""
        #   |isinstance($arg_var, dict)
        #   + and all($inner_check_1 for k in $arg_var.keys())
        #   + and all($inner_check_2 for v in $arg_var.values())
        #   """, locals()).render()

    def input_conversion(self, cpp_type, argument_var, arg_num):
        tt_key, tt_value = cpp_type.template_args
        print("(k,v) ",tt_key.base_type, tt_value.base_type)
        temp_var = "v%d" % arg_num
        cr_ref = False
        code = Code()

        # Counter for key of nested vectors.
        

        cy_tt_key = self.converters.cython_type(tt_key)
        cy_tt_value = self.converters.cython_type(tt_value)

        py_tt_key = tt_key

        if (not cy_tt_value.is_enum and tt_value.base_type in self.converters.names_of_wrapper_classes) \
           and (not cy_tt_key.is_enum and tt_key.base_type in self.converters.names_of_wrapper_classes):
            raise Exception("Converter can not handle wrapped classes as keys and values in map")

        value_conv = ""
        key_conv = ""

        if cy_tt_value.is_enum:
            value_conv = "map(unname(%s),as.integer)" % argument_var
            # value_conv = "<%s> value" % cy_tt_value
        elif tt_value.base_type in self.converters.names_of_wrapper_classes:
            value_conv = "unname(%s)" % argument_var
            # value_conv = "deref((<%s>value).inst.get())" % tt_value.base_type
        elif tt_value.template_args is not None and tt_value.base_type == "libcpp_vector":
            # Special case: the value type is a std::vector< X >, maybe something we can convert?

            # code_top = """
            value_var = "value"
            tt, = tt_value.template_args
            # to check for nested vector of int/libcpp_string/pair of int.
            tt_val = tt
            depth_cnt = 2
            vtemp_var = "svec%s" % arg_num
            inner = self.converters.cython_type(tt)

            # Check whether the inner vector has any classes we need to wrap (we cannot do that)
            contains_classes_to_wrap = tt.template_args is not None and \
                len(set(self.converters.names_of_wrapper_classes).intersection(
                    set(tt.all_occuring_base_types()))) > 0

            if self.converters.cython_type(tt).is_enum:
                # Case 1: We wrap a std::vector<> with an enum base type
                raise Exception("Not Implemented")
            elif tt.base_type in self.converters.names_of_wrapper_classes:
                # Case 2: We wrap a std::vector<> with a base type we need to wrap
                raise Exception("Not Implemented")
            elif tt.template_args is not None and tt.base_type == "shared_ptr" \
                    and len(set(tt.template_args[0].all_occuring_base_types())) == 1:
                # Case 3: We wrap a std::vector< shared_ptr<X> > where X needs to be a type that is easy to wrap
                raise Exception("Not Implemented")
            elif tt.template_args is not None and tt.base_type != "libcpp_vector" and \
                len(set(self.converters.names_of_wrapper_classes).intersection(
                    set(tt.all_occuring_base_types()))) > 0:
                    # Only if the std::vector contains a class that we need to wrap somewhere,
                    # we cannot do it ...
                raise Exception(
                    "Recursion in std::vector<T> is not implemented for other STL methods and wrapped template arguments")
            elif tt.template_args is not None and tt.base_type == "libcpp_vector" and contains_classes_to_wrap:
                # Case 4: We wrap a std::vector<> with a base type that contains
                #         further nested std::vector<> inside
                #         -> deal with recursion
                raise Exception("Not Implemented")
            else:
                # Case 5: We wrap a regular type
                inner = self.converters.cython_type(tt)
                # cython cares for conversion of stl containers with std types,
                # but we need to add the definition to the top
                # Code().add("""
                #     |cdef libcpp_vector[$inner] $vtemp_var
                #     """, locals())

                value_conv_cleanup = Code().add("")
                while (tt_val.base_type == "libcpp_vector"):
                    depth_cnt += 1
                    tt_val, = tt_val.template_args
                if tt_val.base_type == "int":
                    value_conv = "modify_depth(unname(%s),%s,as.integer)" % (argument_var, depth_cnt)
                elif tt_val.base_type == "string":
                    value_conv = "modify_depth(unname(%s),%s,function(a) as.list(py_builtin$bytes(a,'utf-8')))" % (
                    argument_var, depth_cnt)
                elif tt_val.base_type == "libcpp_pair":
                    t1, t2 = tt_val.template_args
                    if t1.base_type == "int":
                        if t2.base_type == "int":
                            value_conv = "map_depth(unname(%s),%s,function(a) list(as.integer(a[[1]]),as.integer(a[[2]])))" % (
                            argument_var, depth_cnt)
                        elif t2.base_type == "libcpp_string":
                            value_conv = "map_depth(unname(%s),%s,function(a) list(as.integer(a[[1]]),py_builtin$bytes(a[[2]],'utf-8')))" % (
                            argument_var, depth_cnt)
                        else:
                            value_conv = "map_depth(unname(%s),%s,function(a) list(as.integer(a[[1]]),a[[2]]))" % (
                                argument_var, depth_cnt)
                    elif t2.base_type == "int":
                        if t1.base_type == "libcpp_string":
                            value_conv = "map_depth(unname(%s),%s,function(a) list(py_builtin$bytes(a[[1]],'utf-8'),as.integer(a[[2]])))" % (
                                argument_var, depth_cnt)
                        else:
                            value_conv = "map_depth(unname(%s),%s,function(a) list(a[[1]],as.integer(a[[2]])))" % (
                                argument_var, depth_cnt)
                    elif t1.base_type == "libcpp_string":
                        if t2.base_type == "libcpp_string":
                            value_conv = "map_depth(unname(%s),%s,function(a) list(py_builtin$bytes(a[[1]],'utf-8'),py_builtin$bytes(a[[2]],'utf-8')))" % (
                                argument_var, depth_cnt)
                        else:
                            value_conv = "map_depth(unname(%s),%s,function(a) list(py_builtin$bytes(a[[1]],'utf-8'),a[[2]]))" % (
                                argument_var, depth_cnt)
                    elif t2.base_type == "libcpp_string":
                        value_conv = "map_depth(unname(%s),%s,function(a) list(a[[1]],py_builtin$bytes(a[[2]],'utf-8')))" % (
                            argument_var, depth_cnt)
                    else:
                        value_conv = "unname(%s)" % argument_var
                else:
                    value_conv = "unname(%s)" % argument_var
        elif tt_value in self.converters:
            value  = "unname(%s)" % argument_var
            value_conv_code, value_conv, value_conv_cleanup = \
                self.converters.get(tt_value).input_conversion(tt_value, value, 0)
        else:
            value_conv = "unname(%s)" % argument_var

        if cy_tt_key.is_enum:
            key_conv = "map(names(%s),as.integer)" % argument_var
        elif tt_key.base_type in self.converters.names_of_wrapper_classes:
            key_conv = "r_to_py(names(%s))" % argument_var
            # key_conv = "deref(<%s *> (<%s> key).inst.get())" % (cy_tt_key, py_tt_key)
        elif tt_key.base_type == "int":
            key_conv = "map(names(%s),as.integer)" % argument_var
        elif tt_key.base_type == "libcpp_string":
            key_conv = "map(names(%s),function(a) py_builtin$bytes(a,'utf-8'))" % argument_var
        elif tt_key in self.converters:
            key = "names(%s)" % argument_var
            key_conv_code, key_conv, key_conv_cleanup = \
                self.converters.get(tt_key).input_conversion(tt_key, key, 0)
        else:
            key_conv = "names(%s)" % argument_var

        code.add("""
            |$temp_var <- py_dict(${key_conv},${value_conv})
            """, locals())

        if cpp_type.is_ref and not cpp_type.is_const:
            cr_ref = True
            it = mangle("it_" + argument_var)

            if tt_key.base_type in self.converters.names_of_wrapper_classes \
              and not tt_value.base_type in self.converters.names_of_wrapper_classes:
                # value_conv = "<%s> deref(%s).second" % (cy_tt_value, it)
                cy_tt = tt_value.base_type
                item = mangle("item_" + argument_var)
                item_key = mangle("itemk_" + argument_var)
                cleanup_code = Code().add("""
                    |names($temp_var) <- sapply(names($temp_var), function(i) eval(parse(text = paste0(class_to_wrap(i),\"$$\",\"new(i)\"))))
                    |byref_${arg_num} <- $temp_var 
                    """, locals())
                # cleanup_code = Code().add("""
                #     |replace = dict()
                #     |cdef libcpp_map[$cy_tt_key, $cy_tt_value].iterator $it = $temp_var.begin()
                #     |cdef $py_tt_key $item_key
                #     |while $it != $temp_var.end():
                #     |   $item_key = $py_tt_key.__new__($py_tt_key)
                #     |   $item_key.inst = shared_ptr[$cy_tt_key](new $cy_tt_key((deref($it)).first))
                #     |   replace[$item_key] = $value_conv
                #     |   inc($it)
                #     |$argument_var.clear()
                #     |$argument_var.update(replace)
                #     |del $temp_var
                #     """, locals())
            elif not cy_tt_value.is_enum and tt_value.base_type in self.converters.names_of_wrapper_classes\
                    and not tt_key.base_type in self.converters.names_of_wrapper_classes:
                cy_tt = tt_value.base_type
                item = mangle("item_" + argument_var)
                if tt_key.base_type == "libcpp_string":
                    cleanup_code = Code().add("""
                        |$temp_var <- py$$transform_dict(${temp_var})
                        |byref_${arg_num} <- lapply($temp_var, function(i) eval(parse(text = paste0(class_to_wrap(i),\"$$\",\"new(i)\"))))
                        """, locals())
                else:
                    cleanup_code = Code().add("""
                        |$temp_var <- lapply(py_to_r($temp_var), function(i) eval(parse(text = paste0(class_to_wrap(i),\"$$\",\"new(i)\"))))
                        |byref_${arg_num} <- $temp_var 
                        """, locals())
                # cleanup_code = Code().add("""
                #     |replace = dict()
                #     |cdef libcpp_map[$cy_tt_key, $cy_tt_value].iterator $it = $temp_var.begin()
                #     |cdef $cy_tt $item
                #     |while $it != $temp_var.end():
                #     |   $item = $cy_tt.__new__($cy_tt)
                #     |   $item.inst = shared_ptr[$cy_tt_value](new $cy_tt_value((deref($it)).second))
                #     |   replace[$key_conv] = $item
                #     |   inc($it)
                #     |$argument_var.clear()
                #     |$argument_var.update(replace)
                #     |del $temp_var
                #     """, locals())
            else:
                cleanup_code = Code().add("")
                if tt_value.base_type == "libcpp_vector":
                    if tt_key.base_type == "libcpp_string":
                        if tt_val.base_type == "int":
                            cleanup_code = Code().add("""
                            |$temp_var <- py$$transform_dict(${temp_var})
                            |byref_${arg_num} <- map_depth($temp_var,$depth_cnt,as.list)
                            """, locals())
                        elif tt_val.base_type == "libcpp_pair":
                            t1,t2 == tt_val.template_args
                            if t1.base_type == "int" and t2.base_type == "int":
                                cleanup_code = Code().add("""
                                |$temp_var <- py$$transform_dict(${temp_var})
                                |byref_${arg_num} <- map_depth($temp_var,$depth_cnt,as.list)
                                """, locals())
                            else:
                                cleanup_code = Code().add("""
                                |byref_${arg_num} <- py$$transform_dict(${temp_var})
                                """, locals())
                        else:
                            cleanup_code = Code().add("""
                            |byref_${arg_num} <- py$$transform_dict(${temp_var})
                            """, locals())
                    else:
                        if tt_val.base_type == "int":
                            cleanup_code = Code().add("""
                            |byref_${arg_num} <- map_depth(py_to_r($temp_var),$depth_cnt,as.list)
                            """, locals())
                        elif tt_val.base_type == "libcpp_pair":
                            t1,t2 == tt_val.template_args
                            if t1.base_type == "int" and t2.base_type == "int":
                                cleanup_code = Code().add("""
                                |byref_${arg_num} <- map_depth(py_to_r($temp_var),$depth_cnt,as.list)
                                """, locals())
                            else:
                                cleanup_code = Code().add("""
                                |byref_${arg_num} <- py_to_r($temp_var)
                                """, locals())
                else:
                    cleanup_code = Code().add("""
                        |byref_${arg_num} <- py_to_r($temp_var)
                        """, locals())
        else:
            cleanup_code = ""

        return code, "%s" % temp_var, cleanup_code, cr_ref

    def call_method(self, res_type, cy_call_str):
        return "py_ans = %s" % (cy_call_str)

    def output_conversion(self, cpp_type, input_r_var, output_r_var):

        assert not cpp_type.is_ptr

        tt_key, tt_value = cpp_type.template_args
        cy_tt_key = self.converters.cython_type(tt_key)
        cy_tt_value = self.converters.cython_type(tt_value)
        py_tt_key = tt_key

        it = mangle("it_" + input_r_var)

        if (not cy_tt_value.is_enum and tt_value.base_type in self.converters.names_of_wrapper_classes) \
           and (not cy_tt_key.is_enum and tt_key.base_type in self.converters.names_of_wrapper_classes):
            raise Exception("Converter can not handle wrapped classes as keys and values in map")

        elif not cy_tt_key.is_enum and tt_key.base_type in self.converters.names_of_wrapper_classes:
            key_conv = "cast_names_list(names(%s))" % it
            # key_conv = "deref(<%s *> (<%s> key).inst.get())" % (cy_tt_key, py_tt_key)
        else:
            key_conv = "names(%s)" % (it)

        if not cy_tt_value.is_enum and tt_value.base_type in self.converters.names_of_wrapper_classes:
            cy_tt = tt_value.base_type
            item = mangle("item_" + output_r_var)
            code = Code().add("""
                |$output_r_var <- lapply($input_r_var, function(i) eval(parse(text = paste0(class_to_wrap(i),\"$$\",\"new(i)\"))))
                """, locals())
            # code = Code().add("""
            #     |$output_py_var = dict()
            #     |cdef libcpp_map[$cy_tt_key, $cy_tt_value].iterator $it = $input_cpp_var.begin()
            #     |cdef $cy_tt $item
            #     |while $it != $input_cpp_var.end():
            #     |   $item = $cy_tt.__new__($cy_tt)
            #     |   $item.inst = shared_ptr[$cy_tt_value](new $cy_tt_value((deref($it)).second))
            #     |   $output_py_var[$key_conv] = $item
            #     |   inc($it)
            #     """, locals())
            return code
        elif not cy_tt_key.is_enum and tt_key.base_type in self.converters.names_of_wrapper_classes:
            value_conv = "<%s>(deref(%s).second)" % (cy_tt_value, it)
            item_key = mangle("itemk_" + output_py_var)
            code = Code().add("""
                |names($input_r_var) <- lapply(names($input_r_var), function(i) eval(parse(text = paste0(class_to_wrap(i),\"$$\",\"new(i)\"))))
                |$output_r_var <- $input_r_var
                """, locals())
            # code = Code().add("""
            #     |$output_py_var = dict()
            #     |cdef libcpp_map[$cy_tt_key, $cy_tt_value].iterator $it = $input_cpp_var.begin()
            #     |cdef $py_tt_key $item_key
            #     |while $it != $input_cpp_var.end():
            #     |   #$output_py_var[$key_conv] = $value_conv
            #     |   $item_key = $py_tt_key.__new__($py_tt_key)
            #     |   $item_key.inst = shared_ptr[$cy_tt_key](new $cy_tt_key((deref($it)).first))
            #     |   # $output_py_var[$key_conv] = $value_conv
            #     |   $output_py_var[$item_key] = $value_conv
            #     |   inc($it)
            #     """, locals())
            return code
        else:
            value_conv = "<%s>(deref(%s).second)" % (cy_tt_value, it)
            code = Code().add("""
                |$output_r_var <- $input_r_var
                """, locals())
            # code = Code().add("""
            #     |$output_py_var = dict()
            #     |cdef libcpp_map[$cy_tt_key, $cy_tt_value].iterator $it = $input_cpp_var.begin()
            #     |while $it != $input_cpp_var.end():
            #     |   $output_py_var[$key_conv] = $value_conv
            #     |   inc($it)
            #     """, locals())
            return code


class StdSetConverter(TypeConverterBase):

    def get_base_types(self):
        return "libcpp_set",

    def matches(self, cpp_type):
        return True

    def matching_python_type(self, cpp_type):
        return "set"

    def type_check_expression(self, cpp_type, arg_var):
        tt, = cpp_type.template_args
        inner_conv = self.converters.get(tt)
        assert inner_conv is not None, "arg type %s not supported" % tt
        inner_check = inner_conv.type_check_expression(tt, "el")

        if isinstance(inner_conv,TypeToWrapConverter):
            return Code().add("""
              |is_list($arg_var) && all(sapply($arg_var,function(el) $inner_check)) && length($arg_var) == py_builtin$$$$len(py_builtin$$$$set(r_to_py($arg_var)))
              """, locals()).render()
        else:
            return Code().add("""
              |is_list($arg_var) && all(sapply($arg_var,function(el) $inner_check)) && !(TRUE %in% duplicated($arg_var))
              """, locals()).render()

        # return Code().add("""
        #   |isinstance($arg_var, set) and all($inner_check for li in $arg_var)
        #   """, locals()).render()

    def input_conversion(self, cpp_type, argument_var, arg_num):
        tt, = cpp_type.template_args
        cr_ref = False
        temp_var = "v%d" % arg_num
        inner = self.converters.cython_type(tt)
        # getting the inner converter.
        inner_conv = self.converters.get(tt)
        it = mangle("it_" + argument_var)
        if inner.is_enum:
            item = "item%d" % arg_num
            code = Code().add("""
                |py$$$temp_var <- $argument_var
                |py_run_string(\"$temp_var = [int(t) for t in $temp_var];$temp_var = set($temp_var)\")
                """, locals())
            # code = Code().add("""
            #     |cdef libcpp_set[$inner] * $temp_var = new libcpp_set[$inner]()
            #     |cdef int $item
            #     |for $item in $argument_var:
            #     |   $temp_var.insert(<$inner> $item)
            #     """, locals())
            if cpp_type.is_ref and not cpp_type.is_const:
                cr_ref = True
                cleanup_code = Code().add("""
                    |byref_$arg_num <- as.list(py_eval("list($temp_var)"))
                    |py_run_string("del $temp_var; gc.collect()")
                    """, locals())

                # cleanup_code = Code().add("""
                #     |replace = set()
                #     |cdef libcpp_set[$inner].iterator $it = $temp_var.begin()
                #     |while $it != $temp_var.end():
                #     |   replace.add(<int> deref($it))
                #     |   inc($it)
                #     |$argument_var.clear()
                #     |$argument_var.update(replace)
                #     |del $temp_var
                #     """, locals())
            else:
                # check if not working
                cleanup_code = "py_run_string('del %s');gc.collect()" % temp_var
            return code, "py$%s" % temp_var, cleanup_code, cr_ref

        elif tt.base_type in self.converters.names_of_wrapper_classes:
            base_type = tt.base_type
            inner = self.converters.cython_type(tt)

            # Only dereference for non-ptr types
            do_deref = "deref"
            if inner.is_ptr:
                do_deref = ""

            cy_tt = tt.base_type
            item = "item%d" % arg_num
            code = Code().add("""
                |py$$$temp_var <- lapply($argument_var,function($item) $item$$.__enclos_env__$$private$$py_obj)
                |py_run_string("$temp_var = set($temp_var)")
                """, locals())
            # code = Code().add("""
            #     |cdef libcpp_set[$inner] * $temp_var = new libcpp_set[$inner]()
            #     |cdef $base_type $item
            #     |for $item in $argument_var:
            #     |   $temp_var.insert($do_deref($item.inst.get()))
            #     """, locals())
            if cpp_type.is_ref and not cpp_type.is_const:
                cr_ref = True
                instantiation = self._codeForInstantiateObjectFromIter(inner, it)
                cleanup_code = Code().add("""
                    |byref_$arg_num <- py_eval("list($temp_var)")
                    |byref_$arg_num <- lapply(byref_$arg_num,function(x) $cy_tt$$new(x))
                    |py_run_string("del $temp_var; gc.collect()")
                    """, locals())

                # cleanup_code = Code().add("""
                #     |replace = set()
                #     |cdef libcpp_set[$inner].iterator $it = $temp_var.begin()
                #     |while $it != $temp_var.end():
                #     |   $item = $cy_tt.__new__($cy_tt)
                #     |   $item.inst = $instantiation
                #     |   replace.add($item)
                #     |   inc($it)
                #     |$argument_var.clear()
                #     |$argument_var.update(replace)
                #     |del $temp_var
                #     """, locals())

            else:
                cleanup_code = "py_run_string('del %s;gc.collect()')" % temp_var
                # cleanup_code = "del %s" % temp_var
            return code, "py$%s" % temp_var, cleanup_code, cr_ref
        else:
            inner = self.converters.cython_type(tt)
            # cython cares for conversion of stl containers with std types:
            if isinstance(inner_conv,IntegerConverter):
                code = Code().add("""
                    |py$$$temp_var <- $argument_var
                    |py_run_string("$temp_var = [int(t) for t in $temp_var];$temp_var = set($temp_var)")
                    """, locals())
            else:
                code = Code().add("""
                    |py$$$temp_var <- $argument_var
                    |py_run_string("$temp_var = set($temp_var)")
                    """, locals())

            # code = Code().add("""
            #     |cdef libcpp_set[$inner] $temp_var = $argument_var
            #     """, locals())

            cleanup_code = ""
            if cpp_type.is_ref and not cpp_type.is_const:
                cr_ref = True
                cleanup_code = Code().add("""
                    |byref_$arg_num <- as.list(py_eval("list($temp_var)"))
                    |py_run_string("del $temp_var; gc.collect()")
                    """, locals())
                # cleanup_code = Code().add("""
                #     |$argument_var.clear()
                #     |$argument_var.update($temp_var)
                #     """, locals())
            return code, "py$%s" % temp_var, cleanup_code, cr_ref

    def call_method(self, res_type, cy_call_str):
        return "py_ans = %s" % (cy_call_str)

    def output_conversion(self, cpp_type, input_cpp_var, output_py_var):

        assert not cpp_type.is_ptr

        tt, = cpp_type.template_args
        inner = self.converters.cython_type(tt)
        if inner.is_enum:
            it = mangle("it_" + input_cpp_var)
            # input_cpp_var : input_py_var
            # if python list elements are of same type (int/long/float) then reticulate converts the python list into vector
            # rather than list. Therefore, converting returned vector to list in such cases.
            code = Code().add("""
                |py$$res <- $input_cpp_var
                |$output_py_var = as.list(py_eval("list(res)"))
                |py_run_string("del res;gc.collect()")
                """, locals())
            # code = Code().add("""
            #     |$output_py_var = set()
            #     |cdef libcpp_set[$inner].iterator $it = $input_cpp_var.begin()
            #     |while $it != $input_cpp_var.end():
            #     |   $output_py_var.add(<int>deref($it))
            #     |   inc($it)
            #     """, locals())
            return code

        elif tt.base_type in self.converters.names_of_wrapper_classes:
            cy_tt = tt.base_type
            inner = self.converters.cython_type(tt)
            it = mangle("it_" + input_cpp_var)
            item = mangle("item_" + output_py_var)

            instantiation = self._codeForInstantiateObjectFromIter(inner, it)
            # input_cpp_var : input_py_var
            code = Code().add("""
                |py$$res <- $input_cpp_var
                |$output_py_var = py_eval("list(res)")
                |$output_py_var <- lapply($output_py_var, function(x) $cy_tt$$new(x))
                |py_run_string("del res;gc.collect()")
                """, locals())
            # code = Code().add("""
            #     |$output_py_var = set()
            #     |cdef libcpp_set[$inner].iterator $it = $input_cpp_var.begin()
            #     |cdef $cy_tt $item
            #     |while $it != $input_cpp_var.end():
            #     |   $item = $cy_tt.__new__($cy_tt)
            #     |   $item.inst = $instantiation
            #     |   $output_py_var.add($item)
            #     |   inc($it)
            #     """, locals())
            return code
        else:
            code = Code().add("""
                |py$$res <- $input_cpp_var
                |$output_py_var = as.list(py_eval("list(res)"))
                |py_run_string("del res;gc.collect()")
                """, locals())
            return code


class StdVectorConverter(TypeConverterBase):

    def get_base_types(self):
        return "libcpp_vector",

    def matches(self, cpp_type):
        return True

    def matching_python_type(self, cpp_type):
        return "list"

    def type_check_expression(self, cpp_type, arg_var):
        tt, = cpp_type.template_args
        inner_conv = self.converters.get(tt)
        assert inner_conv is not None, "arg type %s not supported" % tt
        if arg_var[-4:] == "_rec":
            arg_var_next = "%s_rec" % arg_var
        else:
            # first recursion, set element name
            arg_var_next = "elemt_rec"

        inner_check = inner_conv.type_check_expression(tt, arg_var_next)

        return Code().add("""
          |is_list($arg_var) && all(sapply($arg_var,function($arg_var_next) $inner_check))
          """, locals()).render()
        # return Code().add("""
        #   |isinstance($arg_var, list) and all($inner_check for $arg_var_next in $arg_var)
        #   """, locals()).render()

    def _prepare_nonrecursive_cleanup(self, cpp_type, bottommost_code, it_prev, temp_var, recursion_cnt, *a, **kw):
        # B) Prepare the post-call
        if cpp_type.topmost_is_ref and not cpp_type.topmost_is_const:
            # If the vector is passed by reference, we need to store the
            # result for Python.
            btm_add = ""
            if recursion_cnt > 0:
                # If we are inside a recursion, we have to dereference the
                # _previous_ iterator.
                # not sure
                a[0]["temp_var_used"] = "$temp_var[[%s]]" % it_prev
                #a[0]["temp_var_used"] = "deref(%s)" % it_prev
                tp_add = "$it = $temp_var_used.begin()"
            else:
                tp_add = "cdef libcpp_vector[$inner].iterator $it = $temp_var.begin()"
                btm_add = """
                |$argument_var[:] = replace_$recursion_cnt
                |del $temp_var
                """
                # tp_add = "cdef libcpp_vector[$inner].iterator $it = $temp_var.begin()"
                # btm_add = """
                # |$argument_var[:] = replace_$recursion_cnt
                # |del $temp_var
                # """
                a[0]["temp_var_used"] = temp_var

            # Add cleanup code (loop through the temporary vector C++ and
            # add items to the python replace_n list).
            cleanup_code = Code().add(tp_add + """
                |replace_$recursion_cnt = []
                |for($it in sequence(length($temp_var_used)))
                |    $item = $cy_tt.__new__($cy_tt)
                |    $item.inst = $instantiation
                |    replace_$recursion_cnt.append($item)
                |    inc($it)
                """ + btm_add, *a, **kw)

            # cleanup_code = Code().add(tp_add + """
            #     |replace_$recursion_cnt = []
            #     |while $it != $temp_var_used.end():
            #     |    $item = $cy_tt.__new__($cy_tt)
            #     |    $item.inst = $instantiation
            #     |    replace_$recursion_cnt.append($item)
            #     |    inc($it)
            #     """ + btm_add, *a, **kw)
        else:
            if recursion_cnt == 0:
                if cpp_type.is_ptr:
                    cleanup_code = Code()
                else:
                    cleanup_code = Code().add("del %s" % temp_var)
            else:
                cleanup_code = Code()
                bottommost_code.add("del %s" % temp_var)
        return cleanup_code

    def _prepare_recursive_cleanup(self, cpp_type, bottommost_code, it_prev, temp_var, recursion_cnt, *a, **kw):
        # B) Prepare the post-call
        if cpp_type.topmost_is_ref and not cpp_type.topmost_is_const:
            # If the vector is passed by reference, we need to store the
            # result for Python.
            if recursion_cnt > 0:
                # If we are inside a recursion, we have to dereference the
                # _previous_ iterator.
                a[0]["temp_var_used"] = "deref(%s)" % it_prev
                tp_add = "$it = $temp_var_used.begin()"
            else:
                tp_add = "cdef libcpp_vector[$inner].iterator $it = $temp_var.begin()"
                a[0]["temp_var_used"] = temp_var
            cleanup_code = Code().add(tp_add + """
                |replace_$recursion_cnt = []
                |while $it != $temp_var_used.end():
                """, *a, **kw)
        else:
            if recursion_cnt == 0:
                cleanup_code = Code().add("del %s" % temp_var)
            else:
                cleanup_code = Code()
                bottommost_code.add("del %s" % temp_var)
        return cleanup_code

    def _prepare_nonrecursive_precall(self, topmost_code, cpp_type, code_top, do_deref, *a, **kw):
            # A) Prepare the pre-call
        if topmost_code is not None:
            if cpp_type.topmost_is_ref and not cpp_type.topmost_is_const:
                    # add cdef statements for the iterators (part of B, post-call but needs to
                    # be on top)
                code_top += "|cdef libcpp_vector[$inner].iterator $it"
            topmost_code.add(code_top, *a, **kw)
            code_top = ""
        # Now prepare the loop itself
        code = Code().add(code_top + """
                |for $item in $argument_var:
                |    $temp_var.push_back($do_deref($item.inst.get()))
                """, *a, **kw)
        return code

    def _perform_recursion(self, cpp_type, tt, arg_num, item, topmost_code,
                           bottommost_code, code, cleanup_code, recursion_cnt,
                           *a, **kw):

        converter = self.cr.get(tt)
        py_type = converter.matching_python_type(tt)
        rec_arg_num = "%s_rec" % arg_num
        # the current item is the argument var of the recursive call
        rec_argument_var = item
        topmost_code_callback = Code()
        bottommost_code_callback = Code()
        #
        # Perform the recursive call
        #
        conv_code, call_as, cleanup =\
            converter.input_conversion(tt, rec_argument_var, rec_arg_num,
                                       topmost_code_callback, bottommost_code_callback, recursion_cnt + 1)
        # undo the "deref" if it was added ...
        new_item = call_as
        if call_as.find("deref") != -1:
            new_item = new_item[6:]
            new_item = new_item[:-1]
        a[0]["new_item"] = new_item
        #
        # A) Prepare the pre-call, Step 2
        # add all the "topmost" code from all recursive calls if we are in the topmost recursion
        #
        if topmost_code is None:
            code.content.extend(topmost_code_callback.content)
        else:
            topmost_code.content.extend(topmost_code_callback.content)

        #
        # A) Prepare the pre-call, Step 3
        # add the outer loop
        #
        code.add("""
            |for $item in $argument_var:
            """, *a, **kw)
        #
        # A) Prepare the pre-call, Step 4
        # clear the vector since it needs to be empty before we start the inner loop
        #
        code.add(Code().add("""
            |$new_item.clear()""", *a, **kw))
        #
        # A) Prepare the pre-call, Step 5
        # add the inner loop (may contain more inner loops ... )
        #
        code.add(conv_code)
        #
        # A) Prepare the pre-call, Step 6
        # store the vector from the inner loop in our vector (since
        # it is a std::vector object, there is no "inst" to get).
        #
        code.add("""
            |    $temp_var.push_back(deref($new_item))
            """, *a, **kw)

        #
        # B) Prepare the post-call, Step 1
        # add the inner loop (may contain more inner loops ... )
        #
        if hasattr(cleanup, "content"):
            cleanup_code.add(cleanup)
        else:
            cleanup_code.content.append(cleanup)

        #
        # B) Prepare the post-call, Step 2
        # append the result from the inner loop iteration to the current result
        # (only if we actually give back the reference)
        #
        if cpp_type.topmost_is_ref and not cpp_type.topmost_is_const:
            cleanup_code.add("""
                |    replace_$recursion_cnt.append(replace_$recursion_cnt_next)
                |    inc($it)
                             """, *a, **kw)

        #
        # B) Prepare the post-call, Step 3
        # append the "bottommost" code
        #
        if bottommost_code is None:
            # we are the outermost loop
            cleanup_code.content.extend(bottommost_code_callback.content)
            if cpp_type.topmost_is_ref and not cpp_type.topmost_is_const:
                cleanup_code.add("""
                    |$argument_var[:] = replace_$recursion_cnt
                    |del $temp_var
                                 """, *a, **kw)
        else:
            bottommost_code.content.extend(bottommost_code_callback.content)

    def input_conversion(self, cpp_type, argument_var, arg_num, topmost_code=None, bottommost_code=None, recursion_cnt=0):
        """Do the input conversion for a std::vector<T>

        In this case, the template argument is tt (or "inner").

        It is possible to nest or recurse multiple vectors (like in
        std::vector< std::vector< T > > which is detected since the template
        argument of tt itself is not None).
        """
        # If we are inside a recursion, we expect the toplmost and bottom most code to be present...
        if recursion_cnt > 1:
            assert topmost_code is not None
            assert bottommost_code is not None
        tt, = cpp_type.template_args

        # To check for nested vector of integer or pair containing integer.
        k = tt
        # counter for depth of nesting
        depth_cnt = 1

        temp_var = "v%s" % arg_num
        inner = self.converters.cython_type(tt)
        it = mangle("it_" + argument_var)  # + "_%s" % recursion_cnt
        recursion_cnt_next = recursion_cnt + 1
        it_prev = ""
        if recursion_cnt > 0:
            it_prev = mangle("it_" + argument_var[:-4])

        base_type = tt.base_type
        inner = self.converters.cython_type(tt)
        cy_tt = tt.base_type

        print("Inner converter :", inner)
        # to check for pass by reference.
        cr_ref = False

        # Prepare the code that should be at the very outer level of the
        # function, thus variable declarations (e.g. to prevent a situation
        # where new is called within a loop multiple times and memory loss
        # occurs).
        # code_top = """
        #     |cdef libcpp_vector[$inner] * $temp_var
        #     + = new libcpp_vector[$inner]()
        # """

        contains_classes_to_wrap = tt.template_args is not None and \
            len(set(self.converters.names_of_wrapper_classes).intersection(
                set(tt.all_occuring_base_types()))) > 0

        if self.converters.cython_type(tt).is_enum:
            # Case 1: We wrap a std::vector<> with an enum base type
            item = "item%s" % arg_num
            if topmost_code is not None:
                raise Exception("Recursion in std::vector<T> not yet implemented for enum")
                code_top = ""
            code = Code().add("""
                |$temp_var <- r_to_py(map($argument_var,as.integer))
                """, locals())
            # code = Code().add("""
            #     |cdef libcpp_vector[$inner] * $temp_var
            #     + = new libcpp_vector[$inner]()
            #     |cdef int $item
            #     |for $item in $argument_var:
            #     |    $temp_var.push_back(<$inner> $item)
            #     """, locals())
            if cpp_type.topmost_is_ref and not cpp_type.topmost_is_const:
                cr_ref = True
                cleanup_code = Code().add("""
                    |byref_${arg_num} <- as.list(py_to_r($temp_var))
                    """, locals())
                # cleanup_code = Code().add("""
                #     |replace = []
                #     |cdef libcpp_vector[$inner].iterator $it = $temp_var.begin()
                #     |while $it != $temp_var.end():
                #     |    replace.append(<int> deref($it))
                #     |    inc($it)
                #     |$argument_var[:] = replace
                #     |del $temp_var
                #     """, locals())
            else:
                cleanup_code = Code.add("")
            return code, "%s" % temp_var, cleanup_code, cr_ref

        elif tt.base_type in self.converters.names_of_wrapper_classes:
            # Case 2: We wrap a std::vector<> with a base type we need to wrap
            item = "item%s" % arg_num

            # Add cdef of the base type to the toplevel code
            # code_top += """
            #     |cdef $base_type $item
            # """

            # Only dereference for non-ptr types
            do_deref = "deref"
            if inner.is_ptr:
                do_deref = ""

            # instantiation = self._codeForInstantiateObjectFromIter(inner, it)
            # code = self._prepare_nonrecursive_precall(topmost_code, cpp_type, code_top, do_deref, locals())
            code = Code().add("""
                |$temp_var <- r_to_py($argument_var)
                """, locals())

            if cpp_type.topmost_is_ref and not cpp_type.topmost_is_const:
                cr_ref = True
                cleanup_code = Code().add("""
                    |byref_${arg_num} <- map(py_to_r($temp_var),function(t) eval(parse(text = paste0(class_to_wrap(t),\"$$\",\"new(t)\"))))
                    """, locals())
                # cleanup_code = Code().add("""
                #     |replace = []
                #     |cdef libcpp_vector[$inner].iterator $it = $temp_var.begin()
                #     |while $it != $temp_var.end():
                #     |    replace.append(<int> deref($it))
                #     |    inc($it)
                #     |$argument_var[:] = replace
                #     |del $temp_var
                #     """, locals())
            else:
                cleanup_code = Code().add("")

            # cleanup_code = self._prepare_nonrecursive_cleanup(
            #     cpp_type, bottommost_code, it_prev, temp_var, recursion_cnt, locals())

            if cpp_type.is_ptr:
                call_fragment = temp_var
            else:
                call_fragment = "%s" % temp_var

            return code, call_fragment, cleanup_code, cr_ref

        elif tt.template_args is not None and tt.base_type == "shared_ptr" \
                and len(set(tt.template_args[0].all_occuring_base_types())) == 1:
            # Case 3: We wrap a std::vector< shared_ptr<X> > where X needs to be a type that is easy to wrap

            base_type, = tt.template_args
            cpp_tt, = inner.template_args

            item = "%s_rec" % argument_var
            code = Code().add("""
                |$temp_var <- r_to_py($argument_var)
                """, locals())
            # code = Code().add("""
            #     |cdef libcpp_vector[$inner] $temp_var
            #     |cdef $base_type $item
            #     |for $item in $argument_var:
            #     |    $temp_var.push_back($item.inst)
            #     |# call
            #     """, locals())

            cleanup_code = Code().add("")

            if cpp_type.topmost_is_ref and not cpp_type.topmost_is_const:
                cr_ref = True
                cleanup_code = Code().add("""
                    |byref_${arg_num} <- map(py_to_r($temp_var),function(t) eval(parse(text = paste0($base_type,\"$$\","new(t)"))))
                    """, locals())

                # cleanup_code = Code().add("""
                #     |# gather results
                #     |replace = list()
                #     |cdef libcpp_vector[$inner].iterator $it = $temp_var.begin()
                #     |while $it != $temp_var.end():
                #     |   $item = $base_type.__new__($base_type)
                #     |   $item.inst = deref($it)
                #     |   replace.append($item)
                #     |   inc($it)
                #     |# replace old vector with new contents
                #     |$argument_var[:] = []
                #     |for $item2 in replace:
                #     |    $argument_var.append($item2)
                #     """, locals())

            return code, "%s" % temp_var, cleanup_code, cr_ref

        elif tt.template_args is not None and tt.base_type != "libcpp_vector" and \
            len(set(self.converters.names_of_wrapper_classes).intersection(
                set(tt.all_occuring_base_types()))) > 0:
                # Only if the std::vector contains a class that we need to wrap somewhere,
                # we cannot do it ...
            raise Exception(
                "Recursion in std::vector<T> is not implemented for other STL methods and wrapped template arguments")

        elif tt.template_args is not None and tt.base_type == "libcpp_vector" and contains_classes_to_wrap:
            # Case 4: We wrap a std::vector<> with a base type that contains
            #         further nested std::vector<> inside
            #         -> deal with recursion
            item = "%s_rec" % argument_var
            # Same case as that of simply an unnested vector of classes to wrap.
            code = Code().add("""
                |$temp_var <- r_to_py($argument_var)
                """, locals())

            cleanup_code = Code().add("")
            if cpp_type.topmost_is_ref and not cpp_type.topmost_is_const:
                cr_ref = True
                cleanup_code = Code().add("""
                    |$temp_var <- py_to_r($temp_var)
                    |byref_${arg_num} <- map_depth($temp_var,listDepth($temp_var),function(t) eval(parse(text = paste0(class_to_wrap(t),\"$$\","new(t)"))))
                    """, locals())

            # if code_top != "":
            #     code.add(code_top, locals())

            # cleanup_code = self._prepare_recursive_cleanup(
            #     cpp_type, bottommost_code, it_prev, temp_var, recursion_cnt, locals())

            # Go into recursion (if possible)
            # if hasattr(self, "cr"):
            #     self._perform_recursion(
            #         cpp_type, tt, arg_num, item, topmost_code, bottommost_code, code, cleanup_code, recursion_cnt, locals())
            # else:
            #     raise Exception(
            #         "Error: For recursion in std::vector<T> to work, we need a ConverterRegistry instance at self.cr")

            return code, "%s" % temp_var, cleanup_code, cr_ref
        else:
            # Case 5: We wrap a regular type

            while(k.base_type == "libcpp_vector"):
                depth_cnt+=1
                k, = k.template_args
            print(k.base_type)
            if k.base_type == "libcpp_string":
                code = Code().add("""
                    |$temp_var <- r_to_py(modify_depth($argument_var,$depth_cnt, function(a) py_builtin$bytes(a, 'utf-8')))
                    """, locals())
            elif k.base_type == "int":
                code = Code().add("""
                    |$temp_var <- r_to_py(modify_depth($argument_var,$depth_cnt,as.integer))
                    """, locals())
            elif k.base_type == "libcpp_pair":
                t1, t2 = k.template_args
                if t1.base_type == "int":
                    if t2.base_type == "int":
                        code = Code().add("""
                            |$temp_var <- r_to_py(map_depth($argument_var,$depth_cnt, function(a) list(as.integer(a[[1]]),as.integer(a[[2]]))))
                            """, locals())
                    elif t2.base_type == "libcpp_string":
                        code = Code().add("""
                            |$temp_var <- r_to_py(map_depth($argument_var,$depth_cnt, function(a) list(as.integer(a[[1]]),py_builtin$bytes(a[[2]],'utf-8'))))
                            """, locals())
                    else:
                        code = Code().add("""
                            |$temp_var <- r_to_py(map_depth($argument_var,$depth_cnt, function(a) list(as.integer(a[[1]]),a[[2]])))
                            """, locals())
                elif t2.base_type == "int":
                    if t1.base_type == "libcpp_string":
                        code = Code().add("""
                            |$temp_var <- r_to_py(map_depth($argument_var,$depth_cnt, function(a) list(py_builtin$bytes(a[[1]],'utf-8'),as.integer(a[[2]]))))
                            """, locals())
                    else:
                        code = Code().add("""
                            |$temp_var <- r_to_py(map_depth($argument_var,$depth_cnt, function(a) list(a[[1]],as.integer(a[[2]]))))
                            """, locals())
                elif t1.base_type == "libcpp_string":
                    if t2.base_type == "libcpp_string":
                        code = Code().add("""
                            |$temp_var <- r_to_py(map_depth($argument_var,$depth_cnt, function(a) list(py_builtin$bytes(a[[1]],'utf-8'),py_builtin$bytes(a[[2]],'utf-8'))))
                            """, locals())
                    else:
                        code = Code().add("""
                            |$temp_var <- r_to_py(map_depth($argument_var,$depth_cnt, function(a) list(py_builtin$bytes(a[[1]],'utf-8'),a[[2]])))
                            """, locals())
                elif t2.base_type == "libcpp_string":
                    code = Code().add("""
                        |$temp_var <- r_to_py(map_depth($argument_var,$depth_cnt, function(a) list(a[[1]],py_builtin$bytes(a[[2]],'utf-8'))))
                        """, locals())
                else:
                    code = Code().add("""
                        |$temp_var <- r_to_py($argument_var)
                        """, locals())
            else:
                code = Code().add("""
                    |$temp_var <- r_to_py($argument_var)
                    """, locals())

            # inner = self.converters.cython_type(tt)
            # inner_conv = self.converters.get(tt)
            # cython cares for conversion of stl containers with std types:
            # if isinstance(inner_conv,IntegerConverter):
            #     code = Code().add("""
            #         |$temp_var <- r_to_py(map($argument_var,as.integer))
            #         """, locals())
            # elif isinstance(inner_conv,StdStringConverter):
            #     code = Code().add("""
            #         |$temp_var <- r_to_py(map($argument_var,function(a) py_builtin$bytes(a,'utf-8')))
            #         """, locals())
            # elif isinstance(inner_conv,StdPairConverter):
            #     in1, in2 = inner.template_args
            #     print("Base Types of these converters are: ",in1.base_type,in2.base_type)
            #     # in1 = self.converters.get(in1)
            #     # in2 = self.converters.get(in2)
            #     if in1.base_type == "int":
            #         if in2.base_type == "int":
            #             code = Code().add("""
            #                 |$temp_var <- r_to_py(lapply($argument_var,function(a) list(as.integer(a[[1]]), as.integer(a[[2]]))))
            #                 """, locals())
            #         elif in2.base_type == "libcpp_string":
            #             code = Code().add("""
            #                 |$temp_var <- r_to_py(lapply($argument_var, function(a) list(as.integer(a[[1]]), py_builtin$bytes(a[[2]], 'utf-8'))))
            #                 """, locals())
            #         else:
            #             code = Code().add("""
            #                 |$temp_var <- r_to_py(lapply($argument_var, function(a) list(as.integer(a[[1]]), a[[2]])))
            #                 """, locals())
            #     elif in2.base_type == "int":
            #         if in1.base_type == "libcpp_string":
            #             code = Code().add("""
            #                 |$temp_var <- r_to_py(lapply($argument_var, function(a) list(py_builtin$bytes(a[[1]], 'utf-8'), as.integer(a[[2]]))))
            #                 """, locals())
            #         else:
            #             code = Code().add("""
            #                 |$temp_var <- r_to_py(lapply($argument_var, function(a) list(a[[1]], as.integer(a[[2]]))))
            #                 """, locals())
            #     elif in1.base_type == "libcpp_string":
            #         if in2.base_type == "libcpp_string":
            #             code = Code().add("""
            #                 |$temp_var <- r_to_py(lapply($argument_var, function(a) list(py_builtin$bytes(a[[1]], 'utf-8'), py_builtin$bytes(a[[2]], 'utf-8'))))
            #                 """, locals())
            #         else:
            #             code = Code().add("""
            #                 |$temp_var <- r_to_py(lapply($argument_var, function(a) list(py_builtin$bytes(a[[1]], 'utf-8'), a[[2]])))
            #                 """, locals())
            #     elif in2.base_type == "libcpp_string":
            #         code = Code().add("""
            #             |$temp_var <- r_to_py(lapply($argument_var, function(a) list(a[[1]], py_builtin$bytes(a[[2]], 'utf-8'))))
            #             """, locals())
            #     else:
            #         code = Code().add("""
            #             |$temp_var <- r_to_py($argument_var)
            #             """, locals())
            # else:
            #     code = Code().add("""
            #         |$temp_var <- r_to_py($argument_var)
            #         """, locals())

            cleanup_code = Code().add("")
            if cpp_type.topmost_is_ref and not cpp_type.topmost_is_const:
                cr_ref = True
                if k.base_type == "libcpp_string":
                    cleanup_code = Code().add("""
                        |byref_${arg_num} <- modify_depth(py_to_r($temp_var),$depth_cnt,as.character)
                        """, locals())
                elif k.base_type == "int" or k.base_type == "float" or k.base_type == "double":
                    depth_cnt-=1
                    cleanup_code = Code().add("""
                        |byref_${arg_num} <- map_depth(py_to_r($temp_var),$depth_cnt,as.list)
                        """, locals())
                else:
                    if k.base_type == "libcpp_pair":
                        depth_cnt-=1
                        t1,t2 = k.template_args
                        if (t1.base_type,t2.base_type) == ("int","int"):
                            cleanup_code = Code().add("""
                                |byref_${arg_num} <- map_depth(py_to_r($temp_var),$depth_cnt,as.list)
                                """, locals())
                        else:
                            cleanup_code = Code().add("""
                                |byref_${arg_num} <- py_to_r($temp_var)
                                """, locals())
                    else:
                        cleanup_code = Code().add("""
                            |byref_${arg_num} <- py_to_r($temp_var)
                            """, locals())
            return code, "%s" % temp_var, cleanup_code, cr_ref

    def call_method(self, res_type, cy_call_str):

        t = self.converters.cython_type(res_type)
        if t.is_ptr:
            return "py_ans = %s" % (cy_call_str)

        return "py_ans = %s" % (cy_call_str)

    # NOTE: function returns the result as R type only.
    def output_conversion(self, cpp_type, input_r_var, output_r_var):

        tt, = cpp_type.template_args
        inner = self.converters.cython_type(tt)
        inner_conv = self.converters.get(tt)

        if inner.is_enum:
            it = mangle("it_" + input_r_var)
            code = Code().add("""
                |$output_r_var <- as.list($input_r_var)
                """, locals())
            # code = Code().add("""
            #     |$output_py_var = []
            #     |cdef libcpp_vector[$inner].iterator $it = $input_cpp_var.begin()
            #     |while $it != $input_cpp_var.end():
            #     |   $output_py_var.append(<int>deref($it))
            #     |   inc($it)
            #     """, locals())
            return code

        elif tt.base_type in self.converters.names_of_wrapper_classes:
            cy_tt = tt.base_type
            inner = self.converters.cython_type(tt)
            it = mangle("it_" + input_r_var)
            item = mangle("item_" + output_r_var)

            # instantiation = self._codeForInstantiateObjectFromIter(inner, it)
            code = Code().add("""
                |$output_r_var = map($input_r_var,function(i) eval(parse(text = paste0(class_to_wrap(i),\"$$\",\"new(i)\"))))
                """, locals())
            # code = Code().add("""
            #     |$output_py_var = []
            #     |cdef libcpp_vector[$inner].iterator $it = $input_cpp_var.begin()
            #     |cdef $cy_tt $item
            #     |while $it != $input_cpp_var.end():
            #     |   $item = $cy_tt.__new__($cy_tt)
            #     |   $item.inst = $instantiation
            #     |   $output_py_var.append($item)
            #     |   inc($it)
            #     """, locals())
            return code

        elif tt.base_type == "shared_ptr" \
                and len(set(tt.template_args[0].all_occuring_base_types())) == 1:

            inner = self.converters.cython_type(tt)
            it = mangle("it_" + input_r_var)
            item = mangle("item_" + output_r_var)

            base_type, = tt.template_args
            cpp_tt, = inner.template_args

            code = Code().add("""
                |$output_r_var = map($input_r_var,function(i) eval(parse(text = paste0($base_type,\"$$\",\"new(i)\"))))
                """, locals())
            return code

        else:
            # cython cares for conversion of stl containers with std types:
            if isinstance(inner_conv,StdStringConverter):
                code = Code().add("""
                    |$output_r_var <- map($input_r_var,as.character)
                    """, locals())
            elif isinstance(inner_conv,IntegerConverter):
                code = Code().add("""
                    |$output_r_var <- map($input_r_var,as.integer)
                    """, locals())
            else:
                code = Code().add("""
                    |$output_r_var <- $input_r_var
                    """, locals())

            return code


class StdStringConverter(TypeConverterBase):

    def get_base_types(self):
        return "libcpp_string",

    def matches(self, cpp_type):
        return not cpp_type.is_ptr

    def matching_python_type(self, cpp_type):
        return "bytes"

    def input_conversion(self, cpp_type, argument_var, arg_num):
        code = "%s_%s = py_builtin$$bytes(%s,'utf-8')" % (argument_var,arg_num,argument_var)
        call_as = "%s" % argument_var
        cleanup = ""
        return code, call_as, cleanup

    def type_check_expression(self, cpp_type, argument_var):
        return "is_scalar_character(%s)" % argument_var
        #return "isinstance(%s, bytes)" % argument_var

    def output_conversion(self, cpp_type, input_cpp_var, output_py_var):
        # input_cpp_var : input_py_var
        return "%s = as.character(%s)" % (output_py_var, input_cpp_var)
        # return "%s = <libcpp_string>%s" % (output_py_var, input_cpp_var)


class StdStringUnicodeConverter(StdStringConverter):
    def get_base_types(self):
        return "libcpp_utf8_string",

    def matching_python_type(self, cpp_type):
        return ""

    def input_conversion(self, cpp_type, argument_var, arg_num):
        code = Code()
        code.add("""
            |$argument_var = py_builtin$$bytes($argument_var,'utf-8')
            """, locals())
        # code.add("""
        #     |if isinstance($argument_var, unicode):
        #     |    $argument_var = $argument_var.encode('utf-8')
        #     """, locals())
        call_as = "%s" % argument_var
        cleanup = ""
        return code, call_as, cleanup

    def type_check_expression(self, cpp_type, argument_var):
        return "is_scalar_character(%s)" % argument_var
        # return "isinstance(%s, (bytes, unicode))" % argument_var


class StdStringUnicodeOutputConverter(StdStringUnicodeConverter):

    def get_base_types(self):
        return "libcpp_utf8_output_string",

    def output_conversion(self, cpp_type, input_cpp_var, output_py_var):
        # input_cpp_var :: input_py_var
        # output_py_var :: output_r_var
        return "%s = as.character(%s)" % (output_py_var, input_cpp_var)


class SharedPtrConverter(TypeConverterBase):

    """
    This converter deals with functions that expect a shared_ptr[BaseClass] as
    an argument. For this to work, BaseClass needs to have a Python type and
    thus the expected pointer already exists at BaseClass.inst => all we need
    to do is to pass this inst shared_ptr to the function.
    """

    def get_base_types(self):
        return "shared_ptr",

    def matches(self, cpp_type):
        tt, = cpp_type.template_args
        return tt in self.converters.names_of_wrapper_classes

    def matching_python_type(self, cpp_type):
        tt, = cpp_type.template_args
        return str(tt)

    def input_conversion(self, cpp_type, argument_var, arg_num):
        tt, = cpp_type.template_args
        inner = self.converters.cython_type(tt)
        cr_ref = False
        # Cython expects us to get a C++ type (we cannot just stick var.inst into the function)
        code = ""
        code = Code().add("""
            |input_$argument_var <- r_to_py($argument_var)
            """, locals())
        call_as = "%s" % argument_var
        #call_as = "input_" + argument_var
        cleanup = ""
        # Put the pointer back if we pass by reference
        # environments are passed by reference in R
        if cpp_type.is_ref and not cpp_type.is_const:
            cr_ref = True
            cleanup = Code().add("""
                |$argument_var.inst = py_to_r(input_$argument_var)
                """, locals())
        return code, call_as, cleanup, cr_ref

    def type_check_expression(self, cpp_type, argument_var):
        # We can just use the Python type of the template argument
        tt, = cpp_type.template_args
        return "all(class({}) == c('{}','R6'))".format(argument_var,tt)
        # return "isinstance(%s, %s)" % (argument_var, tt)

    def output_conversion(self, cpp_type, input_cpp_var, output_py_var):
        L.info("Output conversion for %s" % (cpp_type))
        tt, = cpp_type.template_args
        code = Code()

        if tt.is_const:
            # If the template argument is constant, we need to have non-const base-types for our code
            inner = self.converters.cython_type(tt).toString(False)
            tt = tt.toString(withConst=False)

        code.add("""
            |$output_py_var = $tt$$new($input_cpp_var)
            """, locals())
        return code

special_converters = []


def setup_converter_registry(classes_to_wrap, enums_to_wrap, instance_map):

    names_of_classes_to_wrap = list(set(c.cpp_decl.name for c in
                                        classes_to_wrap))
    names_of_enums_to_wrap = list(set(c.cpp_decl.name for c in enums_to_wrap))

    converters = ConverterRegistry(instance_map,
                                   names_of_classes_to_wrap,
                                   names_of_enums_to_wrap)

    converters.register(IntegerConverter())
    converters.register(FloatConverter())
    converters.register(DoubleConverter())
    converters.register(ConstCharPtrConverter())
    converters.register(CharPtrConverter())
    converters.register(CharConverter())
    converters.register(StdStringConverter())
    converters.register(StdStringUnicodeConverter())
    converters.register(StdStringUnicodeOutputConverter())
    converters.register(StdVectorConverter())
    converters.register(StdSetConverter())
    converters.register(StdMapConverter())
    converters.register(StdPairConverter())
    converters.register(VoidConverter())
    converters.register(SharedPtrConverter())

    for clz in classes_to_wrap:
        converters.register(TypeToWrapConverter(clz))

    for enum in enums_to_wrap:
        converters.register(EnumConverter(enum))

    # now special converters which may overlap / overwrite the already
    # registered  converters of types to wrap:
    for converter in special_converters:
        converters.register(converter)

    return converters

# now one can externally register own converters:
#
# from autowrap.ConversionProvider import TypeConverterBase, special_converters
#
# class MyConverter(TypeConverterBase):
#     ...
#
# special_converters.append(MyConverter())
#
