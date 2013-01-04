from contextlib import contextmanager
import os.path, sys

from ConversionProvider import setup_converter_registry
from DeclResolver import ResolvedClass

import Code

@contextmanager
def stdout_redirect(stream):
    sys.stdout = stream
    yield
    sys.stdout = sys.__stdout__


def augmented_args(method):
    return [(t, n if (n and n != "self") else "in_%d" % i)\
                                  for i, (n, t) in enumerate(method.arguments)]


class Tee(object):

    def __init__(self, *fps):
        self.fps = fps

    def write(self, *a):
        for fp in self.fps:
            fp.write(*a)

    def writelines(self, *a):
        for fp in self.fps:
            fp.writelines(*a)

    def flush(self, *a):
        for fp in self.fps:
            fp.flush(*a)


def _normalize(path):
    path = os.path.abspath(path)
    if path.endswith("/"):
        path = path[:-1]
    return path


def _diff(a_path, b_path):
    """ a_path minus b_path prefix """
    a_path = _normalize(a_path)
    b_path = _normalize(b_path)
    assert os.path.commonprefix([a_path, b_path]) == b_path,\
           "%s is not a prefix of %s" % (b_path, a_path)

    return a_path[len(b_path)+1:]


def _has_module_marker(dir_):
    return os.path.isfile(os.path.join(dir_, "__init__.py")) or \
           os.path.isfile(os.path.join(dir_, "__init__.pyx"))


def test_for_module_markers(start_at_dir, up_to_dir):
    start_at_dir = _normalize(start_at_dir)
    up_to_dir = _normalize(up_to_dir)

    assert os.path.commonprefix([start_at_dir, up_to_dir]) == up_to_dir,\
           "%s is not a prefix of %s" % (up_to_dir, start_at_dir)

    current_dir = start_at_dir
    while current_dir != up_to_dir:
        # test for __init__.pyx or __init__.py in current_dir
        if not _has_module_marker(current_dir):
               raise Exception("__init__.py[x] missing in %s" % current_dir)
        current_dir, _ = os.path.split(current_dir)


def cimport_path(pxd_path, target_dir):
    pxd_path = _normalize(pxd_path)
    pxd_dir  = _normalize(os.path.dirname(pxd_path))
    target_dir = _normalize(target_dir)

    base_pxd, _  = os.path.splitext(os.path.basename(pxd_path))
    parts = [base_pxd]
    current_dir = pxd_dir
    while _has_module_marker(current_dir):
        parts.append(os.path.split(current_dir)[1])
        current_dir, _ = os.path.split(current_dir)

    return ".".join(parts[::-1])


class CodeGenerator(object):

    def __init__(self, decls, target_path=None):
        self.decls = decls
        self.target_path = os.path.abspath(target_path)
        self.target_dir  = os.path.dirname(self.target_path)

        self.class_decls = [d for d in decls if isinstance(d, ResolvedClass)]
        class_names = [c.name for c in self.class_decls]

        self.cr = setup_converter_registry(class_names)

        self.code = Code.Code()

    def setup_cimport_paths(self):
        for decl in self.class_decls:
            pxd_path = decl.cpp_decl.pxd_path
            pxd_dir = os.path.dirname(pxd_path)
            test_for_module_markers(pxd_dir, self.target_dir)
            decl.pxd_import_path = cimport_path(pxd_path, self.target_dir)

    def create_pyx_file(self, debug=False):
        self.setup_cimport_paths()
        self.create_cimports()
        for decl in self.decls:
            if decl.items:
                self.create_wrapper_for_enum(decl)
            else:
                self.create_wrapper_for_class(decl)

        code = self.code.render()
        code += "\n\n"

        if debug:
            print code
        with open(self.target_path, "w") as fp:
            print >> fp, code

    def create_wrapper_for_enum(self, decl):
        self.code.add("cdef class $name:", name=decl.name)
        for (name, value) in decl.items:
            self.code.add("    $name = $value", name=name, value=value)


    def create_wrapper_for_class(self, decl):
        name = decl.name
        cy_type = self.cr.cy_decl_str(decl.type_)
        self.code.add("""
               |cdef class $name:
               |    cdef $cy_type * inst
               |    def __dealloc__(self):
               |        if self.inst:
               |            del self.inst """, locals())

        cons_created = False
        for (name, methods) in decl.methods.items():
            if name == decl.name:
                self.create_wrapper_for_constructor(decl, methods)
                cons_created = True
            else:
                self.create_wrapper_for_method(decl, name, methods)
        assert cons_created, "no constructor for %s created" % name

    def _create_overloaded_method_decl(self, code, cpp_name,
                                      dispatched_m_names, methods, use_return):

        code.add("""def $cpp_name(self, *args):""", locals())

        first_iteration = True
        for (dispatched_m_name, method) in zip(dispatched_m_names, methods):
            args = augmented_args(method)
            if not args:
                check_expr = "not args"
            else:
                tns = [ (t, "args[%d]" % i) for i, (t, n) in enumerate(args)]
                checks = ["len(args)==%d" % len(tns)]
                checks += [self.cr.get(t).type_check_expression(t, n)\
                                                            for (t, n) in tns]
                check_expr = " and ".join( "(%s)" % c for c in checks)
            return_ = "return" if use_return else ""
            if_elif = "if" if first_iteration else "elif"
            code.add("""
                    |    $if_elif $check_expr:
                    |        $return_ self.$dispatched_m_name(*args)
                    """, locals())
            first_iteration = False

        code.add("""    else:
                   |        raise Exception('can not handle %s' % (args,))""")

    def create_wrapper_for_method(self, decl, cpp_name, methods):
        if len(methods) == 1:
            self.create_wrapper_for_nonoverloaded_method(decl, cpp_name,
                                                         cpp_name, methods[0])
        else:
            dispatched_m_names = []
            for (i, method) in enumerate(methods):
                dispatched_m_name = "_%s_%d" % (cpp_name, i)
                dispatched_m_names.append(dispatched_m_name)
                self.create_wrapper_for_nonoverloaded_method(decl,
                                                             dispatched_m_name,
                                                             cpp_name,
                                                             method)

            meth_code = Code.Code()
            self._create_overloaded_method_decl(meth_code, cpp_name,
                                                           dispatched_m_names,
                                                           methods,
                                                           True)
            self.code.add(meth_code)

    def _create_meth_decl_and_input_conversion(self, code, py_name, method):
        args = augmented_args(method)

        # collect conversion data for input args
        py_signature_parts = []
        input_conversion_codes = []
        call_args = []
        for arg_num, (t, n) in enumerate(args):
            converter = self.cr.get(t)
            py_type = converter.matching_python_type(t)
            conv_code, call_as = converter.input_conversion(t, n, arg_num)
            py_signature_parts.append("%s %s " % (py_type, n))
            input_conversion_codes.append(conv_code)
            call_args.append(call_as)

        # create method decl statement
        py_signature = ", ".join(["self"] + py_signature_parts)
        code.add("def $py_name($py_signature):", locals())

        # create code which convert python input args to c++ args of wrapped
        # method:
        for conv_code in input_conversion_codes:
            code.add(conv_code)

        call_args = ", ".join(call_args)
        return call_args

    def create_wrapper_for_nonoverloaded_method(self, decl, py_name, cpp_name,
                                                method):

        meth_code = Code.Code()

        call_args = self._create_meth_decl_and_input_conversion(meth_code,
                                                                py_name,
                                                                method)

        # call wrapped method and convert result value back to python
        res_t = method.result_type
        cy_result_type = self.cr.cy_decl_str(res_t)
        to_py_code = self.cr.get(method.result_type).output_conversion(res_t,
                                                                   "_r",
                                                                   "py_result")
        meth_code.add("""
            |    cdef $cy_result_type _r = self.inst.$cpp_name($call_args)
            """, locals())
        if isinstance(to_py_code, basestring):
            to_py_code = "    %s" % to_py_code
        meth_code.add(to_py_code)
        meth_code.add("    return py_result")

        self.code.add(meth_code)

    def create_wrapper_for_constructor(self, class_decl, constructors):
        if len(constructors) == 1:
            self.create_wrapper_for_nonoverloaded_constructor(class_decl,
                                                              "__init__",
                                                              constructors[0])
        else:
            dispatched_cons_names =[]
            for (i, constructor) in enumerate(constructors):
                dispatched_cons_name = "_init_%d" % i
                dispatched_cons_names.append(dispatched_cons_name)
                self.create_wrapper_for_nonoverloaded_constructor(class_decl,
                                             dispatched_cons_name, constructor)
            cons_code = Code.Code()
            self._create_overloaded_method_decl(cons_code,
                                                "__init__",
                                                dispatched_cons_names,
                                                constructors,
                                                False)
            self.code.add(cons_code)


    def create_wrapper_for_nonoverloaded_constructor(self, class_decl, py_name,
                                                           cons_decl):

        """ py_name ist name for constructor, as we dispatch overloaded
            constructors in __init__() the name of the method calling the
            c++ constructor is variable and given by `py_name`.

        """
        cons_code = Code.Code()

        call_args = self._create_meth_decl_and_input_conversion(cons_code,
                                                                py_name,
                                                                cons_decl)

        # create instance of wrapped class
        name = self.cr.cy_decl_str(class_decl.type_)
        cons_code.add("""    self.inst = new $name($call_args)""", locals())

        # add cons code to overall code:
        self.code.add(cons_code)


    def create_cimports(self):
        self.create_std_cimports()
        for decl in self.decls:
            if isinstance(decl, ResolvedClass):
                cpp_decl = decl.cpp_decl
                rel_pxd_path = os.path.relpath(cpp_decl.pxd_path,
                                               self.target_path)
                cython_dir_name = rel_pxd_path.replace(os.sep, ".")
                if os.altsep:
                    cython_dir_name = cython_dir_name.replace(os.altsep, ".")
                import_from = decl.pxd_import_path
                self.code.add("from $from_ cimport $name as _$name",
                                       from_=import_from, name = cpp_decl.name)

    def create_std_cimports(self):
        self.code.add("""
           |from libcpp.string cimport string as std_string
           |from libcpp.vector cimport vector as std_vector
           |from cython.operator cimport dereference as deref,
           + preincrement as inc, address as address""")

