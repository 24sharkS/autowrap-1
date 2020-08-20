# encoding: utf-8

from __future__ import print_function
import pdb

__license__ = """

Copyright (c) 2012-2014, Uwe Schmitt, all rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

Redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.

Redistributions in binary form must reproduce the above copyright notice, this
list of conditions and the following disclaimer in the documentation and/or
other materials provided with the distribution.

Neither the name of the ETH Zurich nor the names of its contributors may be
used to endorse or promote products derived from this software without specific
prior written permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
"""

from contextlib import contextmanager
import os.path
import sys
import re
from collections import defaultdict

from autowrap.ConversionProvider import setup_converter_registry
from autowrap.DeclResolver import (ResolvedClass, ResolvedEnum, ResolvedTypeDef,
                                   ResolvedFunction)
from autowrap.Types import CppType  # , printable
import autowrap.Code as Code

import logging as L

special_class_doc = ""
def namespace_handler(ns):
    return ns

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


def augment_arg_names(method):
    """ replaces missing arg_names with "in_%d" % i, where i is the position
        number of the arg """
    return [(t, n if (n and n != "self") else "in_%d" % i)
            for i, (n, t) in enumerate(method.arguments)]


def fixed_include_dirs(include_boost):
    import pkg_resources
    boost = pkg_resources.resource_filename("autowrap", "data_files/boost")
    data = pkg_resources.resource_filename("autowrap", "data_files")
    autowrap_internal = pkg_resources.resource_filename("autowrap", "data_files/autowrap")

    if not include_boost:
      return [autowrap_internal]
    else:
      return [boost, data, autowrap_internal]


class CodeGenerator(object):

    """
    This is the main Code Generator.

    Its main entry function is "create_r_file" which generates the pyx file
    from the input (given in the initializiation).

    The actual conversion of input/output arguments is done in the
    ConversionProviders for each argument type.
    """
    # changing pyx_target_path -> r_target_path and pyx_code -> R_code
    def __init__(self, resolved, instance_mapping, r_target_path=None,
                 manual_code=None, extra_cimports=None, allDecl={}):

        if manual_code is None:
            manual_code = dict()

        self.manual_code = manual_code
        self.extra_cimports = extra_cimports

        self.include_shared_ptr=True
        self.include_refholder=True
        self.include_numpy=False

        # adding new field r_target_path
        self.r_target_path = r_target_path
        self.target_path = os.path.abspath(r_target_path)
        self.target_pxd_path = re.split("pyopenms_..R",self.target_path)[0] + "imports.R"
        self.target_dir = os.path.dirname(self.target_path)

        # If true, we will write separate pxd and pyx files (need to ensure the
        # right code goes to header if we use pxd headers). Alternatively, we
        # will simply write a single pyx file.
        self.write_pxd = len(allDecl) > 0

        ## Step 1: get all classes of current module
        self.classes = [d for d in resolved if isinstance(d, ResolvedClass)]
        self.enums = [d for d in resolved if isinstance(d, ResolvedEnum)]
        self.functions = [d for d in resolved if isinstance(d, ResolvedFunction)]
        self.typedefs = [d for d in resolved if isinstance(d, ResolvedTypeDef)]

        self.resolved = []
        self.resolved.extend(sorted(self.typedefs, key=lambda d: d.name))
        self.resolved.extend(sorted(self.enums, key=lambda d: d.name))
        self.resolved.extend(sorted(self.functions, key=lambda d: d.name))
        self.resolved.extend(sorted(self.classes, key=lambda d: d.name))

        self.instance_mapping = instance_mapping
        self.allDecl = allDecl

        ## Step 2: get classes of complete project (includes other modules)
        self.all_typedefs = self.typedefs
        self.all_enums = self.enums
        self.all_functions = self.functions
        self.all_classes = self.classes
        self.all_resolved = self.resolved
        if len(allDecl) > 0:
            
            self.all_typedefs = []
            self.all_enums = []
            self.all_functions = []
            self.all_classes = []
            for modname, v in allDecl.items():
                self.all_classes.extend( [d for d in v["decls"] if isinstance(d, ResolvedClass)] )
                self.all_enums.extend( [d for d in v["decls"] if isinstance(d, ResolvedEnum)] )
                self.all_functions.extend( [d for d in v["decls"] if isinstance(d, ResolvedFunction)] )
                self.all_typedefs.extend( [d for d in v["decls"] if isinstance(d, ResolvedTypeDef)] )

            self.all_resolved = []
            self.all_resolved.extend(sorted(self.all_typedefs, key=lambda d: d.name))
            self.all_resolved.extend(sorted(self.all_enums, key=lambda d: d.name))
            self.all_resolved.extend(sorted(self.all_functions, key=lambda d: d.name))
            self.all_resolved.extend(sorted(self.all_classes, key=lambda d: d.name))

        # Register using all classes so that we know about the complete project
        self.cr = setup_converter_registry(self.all_classes, self.all_enums, instance_mapping)

        self.top_level_code = []
        self.top_level_R_code = []
        self.class_codes = defaultdict(list)
        self.class_codes_extra = defaultdict(list)
        self.class_pxd_codes = defaultdict(list)
        self.wrapped_enums_cnt = 0
        self.wrapped_classes_cnt = 0
        self.wrapped_methods_cnt = 0

    def get_include_dirs(self, include_boost):
        if self.pxd_dir is not None:
            return fixed_include_dirs(include_boost) + [self.pxd_dir]
        else:
            return fixed_include_dirs(include_boost)

    def setup_cimport_paths(self):

        pxd_dirs = set()
        for inst in self.all_classes + self.all_enums + self.all_functions + self.all_typedefs:
            pxd_path = os.path.abspath(inst.cpp_decl.pxd_path)
            pxd_dir = os.path.dirname(pxd_path)
            pxd_dirs.add(pxd_dir)
            pxd_file = os.path.basename(pxd_path)
            inst.pxd_import_path, __ = os.path.splitext(pxd_file)

        assert len(pxd_dirs) <= 1, "pxd files must be located in same directory"

        self.pxd_dir = pxd_dirs.pop() if pxd_dirs else None

    # changed name from create_pyx_file to create_r_file.
    def create_r_file(self, debug=False):
        """This creates the actual R code

        It calls create_wrapper_for_class, create_wrapper_for_enum and
        create_wrapper_for_free_function respectively to create the code for
        all classes, enums and free functions.
        """
        self.setup_cimport_paths()
        # self.create_cimports()
        # self.create_foreign_cimports()

        # Modifying create_includes function to import packages and create python module.
        self.create_includes()

        def create_for(clz, method):
            for resolved in self.resolved:
                if resolved.wrap_ignore:
                    continue
                if isinstance(resolved, clz):
                    method(resolved)

        # first wrap classes, so that self.class_codes[..] is initialized
        # for attaching enums or static functions
        create_for(ResolvedClass, self.create_wrapper_for_class)
        create_for(ResolvedEnum, self.create_wrapper_for_enum)
        create_for(ResolvedFunction, self.create_wrapper_for_free_function)

        # resolve extra
        # for clz, codes in self.class_codes_extra.items():
        #     if clz not in self.class_codes:
        #         raise Exception("Cannot attach to class", clz, "make sure all wrap-attach are in the same file as parent class")
        #     for c in codes:
        #         self.class_codes[clz].add(c)

        # Create code for the R file
        if self.write_pxd:
            R_code = "" + "\n".join(ci.render() for ci in self.top_level_R_code)
        else:
            R_code = "\n".join(ci.render() for ci in self.top_level_code)
            R_code += "\n".join(ci.render() for ci in self.top_level_R_code)

        R_code += " \n"
        names = set()
        for n, c in self.class_codes.items():
            R_code += c.render()
            R_code += " \n"
            names.add(n)

        # manual code which does not extend wrapped classes:
        for name, c in self.manual_code.items():
            if name not in names:
                R_code += c.render()
            R_code += " \n"

        # Create code for the pxd file
        pxd_code = "\n".join(ci.render() for ci in self.top_level_code)
        pxd_code += " \n"
        for n, c in self.class_pxd_codes.items():
            pxd_code += c.render()
            pxd_code += " \n"

        if debug:
            print(pxd_code)
            print(R_code)
        with open(self.target_path, "w") as fp:
            fp.write(R_code)

        if self.write_pxd:
            with open(self.target_pxd_path, "w") as fp:
                fp.write(pxd_code)

    def filterout_iterators(self, methods):
        def parse(anno):
            m = re.match(r"(\S+)\((\S+)\)", anno)
            assert m is not None, "invalid iter annotation"
            name, type_str = m.groups()
            return name, CppType.from_string(type_str)

        begin_iterators = dict()
        end_iterators = dict()
        non_iter_methods = defaultdict(list)
        for name, mi in methods.items():
            for method in mi:
                annotations = method.cpp_decl.annotations
                if "wrap-iter-begin" in annotations:
                    py_name, res_type = parse(annotations["wrap-iter-begin"])
                    begin_iterators[py_name] = (method, res_type)
                elif "wrap-iter-end" in annotations:
                    py_name, res_type = parse(annotations["wrap-iter-end"])
                    end_iterators[py_name] = (method, res_type)
                else:
                    non_iter_methods[name].append(method)

        begin_names = set(begin_iterators.keys())
        end_names = set(end_iterators.keys())
        common_names = begin_names & end_names
        if begin_names != end_names:
            # TODO: diesen fall testen
            raise Exception("iter declarations not balanced")

        for py_name in common_names:
            __, res_type_begin = begin_iterators[py_name]
            __, res_type_end = end_iterators[py_name]
            assert res_type_begin == res_type_end, "iter value types do not match"

        begin_methods = dict((n, m) for n, (m, __) in begin_iterators.items())
        end_methods = dict((n, m) for n, (m, __) in end_iterators.items())
        res_types = dict((n, t) for n, (__, t) in end_iterators.items())

        iterators = dict()
        for n in common_names:
            iterators[n] = (begin_methods[n], end_methods[n], res_types[n])

        return iterators, non_iter_methods

    def create_wrapper_for_enum(self, decl):
        self.wrapped_enums_cnt += 1
        if decl.cpp_decl.annotations.get("wrap-attach"):
            name = decl.name
            # name = "__" + decl.name
        else:
            name = decl.name
        L.info("create wrapper for enum %s" % name)
        code = Code.Code()
        enum_pxd_code = Code.Code()

        code.add("""
                   |
                   |$name = R6Class(classname = \"$name\", cloneable = FALSE,
                   |
                   |        public = list(
                   |
                 """, name=name)
        for (name, value) in decl.items:
            code.add("        $name = ${value}L,", name=name, value=value)

        code.add("        initialize = function(){")
        for name,_ in decl.items:
            code.add("            lockBinding(\"$name\",self)", name=name)
        code.add("        },")
        # Add mapping of int (enum) to the value of the enum (as string)
        code.add("""
                |        getMapping = function() {
                |            return( Pymod$$$name()$$getMapping() )
                |        }
                |    )
                |)""", name = decl.name )

        self.class_codes[decl.name] = code
        # self.class_pxd_codes[decl.name] = enum_pxd_code

        for class_name in decl.cpp_decl.annotations.get("wrap-attach", []):
            code = Code.Code()
            display_name = decl.cpp_decl.annotations.get("wrap-as", [decl.name])[0]
            # code.add("%s = %s" % (display_name, "__" + decl.name))
            # self.class_codes[class_name].add(code)

    def create_wrapper_for_class(self, r_class):
        """Create R code for a single class
        
        Note that the cdef class definition and the member variables go into
        the .pxd file while the Python-level implementation goes into the .pyx
        file. This allows us to cimport these classes later across modules.
        """
        self.wrapped_classes_cnt += 1
        self.wrapped_methods_cnt += len(r_class.methods)
        cname = r_class.name

        # changing pyname to rname
        rname = cname
        temp = rname.split("_Interfaces_")
        if len(temp) > 1:
            rname = temp[-1]

        # if r_class.cpp_decl.annotations.get("wrap-attach"):
        #     pyname = "__" + r_class.name
        # else:
        #     pyname = cname

        L.info("create wrapper for class %s" % cname)
        cy_type = self.cr.cython_type(cname)

        # Attempt to derive sane class name and namespace
        cpp_name = str(cy_type)
        namespace = namespace_handler(r_class.ns)
        if cpp_name.startswith("_"):
            cpp_name = cpp_name[1:]

        class_pxd_code = Code.Code()
        class_code = Code.Code()

        # Class documentation (multi-line)
        docstring = "\n# R implementation of %s\n" % cy_type
        # docstring = "Cython implementation of %s\n" % cy_type
        docstring += special_class_doc % locals()

        # commenting as not relevant for libcpp_test
        # if r_class.cpp_decl.annotations.get("wrap-inherits", "") != "":
        #     docstring += "     -- Inherits from %s\n" % r_class.cpp_decl.annotations.get("wrap-inherits", "")
        #
        extra_doc = r_class.cpp_decl.annotations.get("wrap-doc", "")
        for extra_doc_line in extra_doc:
            docstring += "\n# " + extra_doc_line

        if r_class.methods:
            pass
            if len(r_class.wrap_manual_memory) != 0:
                pass

        if r_class.methods or len(r_class.attributes) > 0:
            py_ob = "private = list(py_obj = NA),"
        else:
            py_ob = "private = list(py_obj = NA)"

        class_code.add("""
                        |$docstring
                        |$rname <- R6Class(classname = \"$rname\",cloneable = FALSE,
                        |
                        |    $py_ob
                        |
                        """, locals())

        if len(r_class.wrap_hash) != 0:
            pass
            # As the underlying python object has already the __hash__ property implemented
            # so, we can overload the "==" operator for our R6 classes, which checks for the equality of the wrapped python objects.
            # i.e. return(o1$get_py_object() == o2$get_py_object())
            # class_code.add("""
            #                 |
            #                 |    def __hash__(self):
            #                 |      # The only required property is that objects which compare equal have
            #                 |      # the same hash value:
            #                 |      return hash(deref(self.inst.get()).%s )
            #                 |
            #                 """ % r_class.wrap_hash[0], locals())

        # self.class_pxd_codes[cname] = class_pxd_code
        self.class_codes[cname] = class_code

        cons_created = False

        if len(r_class.attributes) > 0:
            class_code.add("""
                |
                |    active = list(
                """, locals())

        # counter for attribute
        attr_cur = 0
        # total attributes (excluding wrap-ignore)
        total_attr_cnt = 0

        # to count all attributes (excluding wrap-ignore)
        for attr in r_class.attributes:
            if not attr.wrap_ignore:
                total_attr_cnt += 1

        for attribute in r_class.attributes:
            if not attribute.wrap_ignore:
                attr_cur+=1
                class_code.add(self._create_wrapper_for_attribute(attribute,attr_cur,total_attr_cnt))

        if len(r_class.attributes) > 0 and r_class.methods:
            class_code.add("""
                |
                |    ),
                """, locals())
        elif len(r_class.attributes) > 0 and not r_class.methods:
            class_code.add("""
                |
                |    )
                """, locals())

        iterators, non_iter_methods = self.filterout_iterators(r_class.methods)

        # total number of methods in that class
        total_method_count = 0

        for name,methods in non_iter_methods.items():
            if not name == r_class.name:
                if name.startswith("operator"):
                    __, __, op = name.partition("operator")
                    if op == "()" or "[]":
                        pass
                        # total_method_count += len(methods)
                else:
                    total_method_count += len(methods)
            else:
                total_method_count += len(methods)


        # TODO counter for wrapped methods (currently only wrapping methods excluding __iter__).
        meth_cnt  = 0

        if len(non_iter_methods.items()) > 0:
            # Add public field before creating any constructor/method.
            ps = Code.Code()
            ps.add("""
               |
               |public = list(
                """, locals())
            class_code.add(ps)

        # store the code for operator methods [ (),[],+,*,+= ]
        operator_code = []
        for (name, methods) in non_iter_methods.items():
            if name == r_class.name:
                meth_cnt += len(methods)
                codes = self.create_wrapper_for_constructor(r_class, methods,meth_cnt,total_method_count)
                cons_created = True
            else:
                if name.startswith("operator"):
                    __, __, op = name.partition("operator")
                    if op == "()" or "[]":
                        pass
                        # meth_cnt += len(methods)
                else:
                    meth_cnt += len(methods)
                codes = self.create_wrapper_for_method(r_class, name, methods,meth_cnt,total_method_count,operator_code)

            for ci in codes:
                class_code.add(ci)

        if len(non_iter_methods.items()) > 0:
            class_code.add("""
               |)
                """, locals())

        has_ops = dict()
        for ops in ["==", "!=", "<", "<=", ">", ">="]:
            has_op = ("operator%s" % ops) in non_iter_methods
            has_ops[ops] = has_op

        # Iterators not implemented.
        # codes = self._create_iter_methods(iterators, r_class.instance_map, r_class.local_map)
        # for ci in codes:
        #     class_code.add(ci)


        # Dealt with static functions
        # Ignoring the case of nested classes
        for class_name in r_class.cpp_decl.annotations.get("wrap-attach", []):
            pass
            # code = Code.Code()
            # display_name = r_class.cpp_decl.annotations.get("wrap-as", [r_class.name])[0]
            # code.add("%s = %s" % (display_name, "__" + r_class.name))
            # tmp = self.class_codes_extra.get(class_name, [])
            # tmp.append(code)
            # self.class_codes_extra[class_name] = tmp

        class_code.add("""
                        |)
                        """)

        if not cons_created:
            class_code.add("""
                            |$rname$$set("public","initialize",function(){
                            |   private$$py_obj <- Pymod$$$rname()
                            |},overwrite = TRUE)
                            """, locals())

        if len(operator_code) > 0:
            for c in operator_code:
                self.class_codes[r_class.name].add(c)

        if any(v for v in has_ops.values()):
            code = self.create_special_cmp_method(r_class, has_ops)
            class_code.add(code)

        # Adding all the extra methods at last
        extra_methods_code = self.manual_code.get(cname)
        if extra_methods_code:
            class_code.add(extra_methods_code)

    def _create_iter_methods(self, iterators, instance_mapping, local_mapping):
        """
        Create Iterator methods using the Python yield keyword
        """
        codes = []
        for name, (begin_decl, end_decl, res_type) in iterators.items():
            L.info("   create wrapper for iter %s" % name)
            meth_code = Code.Code()
            begin_name = begin_decl.name
            end_name = end_decl.name

            # TODO: this step is duplicated from DeclResolver.py
            # can we combine both maps to one single map ?
            res_type = res_type.transformed(local_mapping)
            res_type = res_type.inv_transformed(instance_mapping)

            cy_type = self.cr.cython_type(res_type)
            base_type = res_type.base_type

            meth_code.add("""
                            |
                            |def $name(self):
                            |    it = self.inst.get().$begin_name()
                            |    cdef $base_type out
                            |    while it != self.inst.get().$end_name():
                            |        out = $base_type.__new__($base_type)
                            |        out.inst =
                            + shared_ptr[$cy_type](new $cy_type(deref(it)))
                            |        yield out
                            |        inc(it)
                            """, locals())

            codes.append(meth_code)
        return codes


    def _create_overloaded_method_decl(self, r_name, dispatched_m_names, methods, use_return, use_kwargs=False, for_cons = None, r_classname = None, deepcopy_for_cons = None):

        L.info("   create wrapper decl for overloaded method %s" % r_name)

        method_code = Code.Code()
        # kwargs = ""
        # if use_kwargs:
        #     kwargs = ", **kwargs"

        docstrings = "\n"
        for method in methods:
            # Prepare docstring
            docstrings += " " * 4 + "# C++ signature: %s" % method
            extra_doc = method.cpp_decl.annotations.get("wrap-doc", "")
            if len(extra_doc) > 0:
                docstrings += "\n" + " " * 8 + "# "+extra_doc
            docstrings += "\n"

        method_code.add("""
                          |$docstrings
                          |$r_name = function(...){
                          |    arg_list = list(...)
                        """, locals())

        first_iteration = True

        for (dispatched_m_name, method) in zip(dispatched_m_names, methods):
            args = augment_arg_names(method)
            # Added flags here
            print("Argument List: ",args)
            if not args:
                check_expr = "length(arg_list)==0"

                # Special case for empty constructors with a pass
                if method.cpp_decl.annotations.get("wrap-pass-constructor", False):
                    pass
                    # assert use_kwargs, "Cannot use wrap-pass-constructor without setting kwargs (e.g. outside a constructor)"
                    # check_expr = 'kwargs.get("__createUnsafeObject__") is True'

            else:
                tns = [(t, "arg_list[[%d]]" % (i+1)) for i, (t, n) in enumerate(args)]
                # added flag here
                print("Type and Arguments: ",tns)
                checks = ["length(arg_list)==%d" % len(tns)]
                checks += [self.cr.get(t).type_check_expression(t, n) for (t, n) in tns]
                check_expr = " && ".join("(%s)" % c for c in checks)
            # return_ = "return" if use_return else "invisible"
            return_ = "return"
            if_elif = "if" if first_iteration else "else if"
            method_code.add("""
                            |    $if_elif ($check_expr) { self$$$dispatched_m_name(...) }
                            """, locals())
            first_iteration = False

        if for_cons is None:
            method_code.add(""" 
                            |    else {
                            |          stop("wrong arguments provided")
                            |    }     
                            """, locals())
        elif for_cons == True and r_classname is not None:
            if r_classname is not None:
                temp = r_classname.split("_Interfaces_")
                if len(temp) > 1:
                    r_classname = temp[-1]
            method_code.add("""    else{
                            |           # to create a new R object and set its underlying python object as the one supplied in the constructor.
                            |           # this helps avoid use of set_py_object(), s.t., the user is not able to manipulate the python object in a direct fashion.
                            |           if( length(arg_list)==1 && ( "python.builtin.object" %in% class(arg_list[[1]]) && class_to_wrap(arg_list[[1]]) == "$r_classname" ) )
                            """, locals())
            if deepcopy_for_cons == True:
                method_code.add(""" 
                                |           { private$$py_obj <- arg_list[[1]]  }
                                |           else {
                                |                stop("wrong arguments provided")
                                |           }
                                |       }
                                """, locals())
            else:
                method_code.add("""
                                |           { private$$py_obj <- arg_list[[1]]  }
                                |           else {
                                |                stop("wrong arguments provided")
                                |           }
                                |       }
                                """, locals())
        return method_code

    def create_wrapper_for_method(self, cdcl, py_name, methods,meth_cnt,total_method_count, operator_code):
        if py_name.startswith("operator"):
            __, __, op = py_name.partition("operator")
            if op in ["!=", "==", "<", "<=", ">", ">="]:
                # handled in create_wrapper_for_class, as one has to collect
                # these
                return []
            elif op == "()":
                codes = self.create_cast_methods(methods,meth_cnt,total_method_count,class_name = cdcl.name)
                operator_code.extend(codes)
                return []
            elif op == "[]":
                assert len(methods) == 1, "overloaded operator[] not suppored"
                code_get = self.create_special_getitem_method(methods[0],cdcl)
                code_set = self.create_special_setitem_method(methods[0],cdcl)
                operator_code.extend([code_get, code_set])
                return []
            elif op == "+":
                assert len(methods) == 1, "overloaded operator+ not suppored"
                code = self.create_special_add_method(cdcl, methods[0])
                operator_code.append(code)
                return []
            elif op == "*":
                assert len(methods) == 1, "overloaded operator* not suppored"
                code = self.create_special_mul_method(cdcl, methods[0])
                operator_code.append(code)
                return []
            elif op == "+=":
                #
                # can't support += operator in R
                #
                assert len(methods) == 1, "overloaded operator+= not suppored"
                # code = self.create_special_iadd_method(cdcl, methods[0])
                return []

        if len(methods) == 1:
            code = self.create_wrapper_for_nonoverloaded_method(cdcl, py_name, methods[0],meth_cnt,total_method_count)
            return [code]
        else:
            # TODO: what happens if two distinct c++ types as float, double
            # map to the same python type ??
            # -> 1) detection
            # -> 2) force method renaming
            codes = []
            dispatched_m_names = []
            meth_cnt -= len(methods)
            for (i, method) in enumerate(methods):
                dispatched_m_name = "%s_%d" % (py_name, i)
                dispatched_m_names.append(dispatched_m_name)
                code = self.create_wrapper_for_nonoverloaded_method(cdcl,
                                                                    dispatched_m_name,
                                                                    method,meth_cnt,total_method_count,from_overloaded = True)
                codes.append(code)

            code = self._create_overloaded_method_decl(py_name, dispatched_m_names, methods, True)
            codes.append(code)
            meth_cnt += len(methods)
            if meth_cnt == total_method_count:
                codes.append(Code.Code().add("""
                   |
                   |}
                    """))
            else:
                codes.append(Code.Code().add("""
                   |
                   |},
                    """))
            return codes

    def _create_fun_decl_and_input_conversion(self, code, py_name, method, is_free_fun=False, is_single_cons = None,class_decl = None,for_method = None,for_modifiers = False):
        """ Creates the function declarations and the input conversion to python
        and the output conversion back to R.

        The input conversion is directly added to the "code" object while the
        conversion back to R is returned as "cleanups".
        """
        args = augment_arg_names(method)

        # create a list to store argument number which are being passed by reference.
        store_ref_arg = []

        # Step 0: collect conversion data for input args and call
        # input_conversion for more sophisticated conversion code (e.g. std::vector<Obj>)
        py_signature_parts = []
        input_conversion_codes = []
        cleanups = []
        call_args = []
        checks = []
        in_types = []
        for arg_num, (t, n) in enumerate(args):
            # get new ConversionProvider using the converter registry
            converter = self.cr.get(t)
            converter.cr = self.cr
            py_type = converter.matching_python_type(t)
            cr_ref = False
            conv_code, call_as, cleanup, cr_ref = converter.input_conversion(t, n, arg_num)
            if(cr_ref):
                store_ref_arg.append((n,arg_num))
            py_signature_parts.append("%s" % (n))
            # py_signature_parts.append("%s %s " % (py_type, n))
            input_conversion_codes.append(conv_code)
            cleanups.append(cleanup)
            call_args.append(call_as)
            in_types.append(t)
            checks.append((n, converter.type_check_expression(t, n)))

        str_eval_missing = []
        if is_single_cons is True:
            if len(py_signature_parts) > 1:
                for i in py_signature_parts[1:]:
                    str_eval_missing.append("missing(%s)" % i)
                str_eval_missing = ' && '.join(str_eval_missing)

        # Step 1: create method decl statement
        # if not is_free_fun:
        #     py_signature_parts.insert(0, "self")

        # Prepare docstring
        docstring = "\n    # C++ signature: %s" % method
        extra_doc = method.cpp_decl.annotations.get("wrap-doc", "")
        if len(extra_doc) > 0:
            docstring += "\n" + "#" + " "+ extra_doc

        py_signature = ", ".join(py_signature_parts)

        if for_method is True:
            alter_name = "initialise" if py_name == "initialize" else py_name
        else:
            alter_name = py_name
        if is_single_cons is True and len(str_eval_missing) == 0:
            code.add("""
                       |$docstring
                       |$alter_name = function(...){
                       |    par <- list(...)
                       """, locals())
        else:
            if not for_modifiers:
                code.add("""
                           |$docstring
                           |$alter_name = function($py_signature){
                           |
                           """, locals())
        # Step 2a: create code which convert R input args to python args of
        # wrapped method
        if is_single_cons in (None,False):
            for n, check in checks:
                code.add("""
                           |    if(!($check)){ stop(\"arg $n wrong type\") }
                           """,locals())
        else:
            if is_single_cons is True:
                class_name = class_decl.name
                if len(str_eval_missing) > 0:
                    first = py_signature_parts[0]
                    code.add("""
                               |if($str_eval_missing){
                               |     if( "python.builtin.object" %in% class($first) && class_to_wrap($first) == $class_name ) { private$$py_obj <- $first }
                               |     else { stop("arg wrong type") }
                               |  } else {
                               """, locals())
                    for n, check in checks:
                        code.add("    if(!($check)){ stop(\"arg $n wrong type\") }", locals())
                else:
                    code.add("""
                               |if (!length(par) %in% c(0,1)) { stop("arg wrong type")}
                               |if (length(par)==1) {
                               |    if ("python.builtin.object" %in% class(par[[1]]) && class_to_wrap(par[[1]]) == $class_name) { private$$py_obj <- par[[1]] }
                               |    else { stop("arg wrong type") }
                               |} else if (length(par)==0) {
                               """, locals())
                    for n, check in checks:
                        code.add("    if(!($check)){ stop(\"arg $n wrong type\") }", locals())

        # Step 2b: add any more sophisticated conversion code that was created
        # above:
        for conv_code in input_conversion_codes:
            code.add(conv_code)

        return call_args, cleanups, in_types, store_ref_arg

    def _create_wrapper_for_attribute(self, attribute, attr_cur,total_attr_cnt):
        code = Code.Code()
        name = attribute.name
        wrap_as = attribute.cpp_decl.annotations.get("wrap-as", name)
        wrap_constant = attribute.cpp_decl.annotations.get("wrap-constant", False)

        t = attribute.type_

        converter = self.cr.get(t)
        py_type = converter.matching_python_type(t)
        type_check = converter.type_check_expression(t,name)
        conv_code, call_as, cleanup,cr_ref = converter.input_conversion(t, name, 0)

        if wrap_constant:
            code.add("""
                |    
                |    $wrap_as = function($name){
                |        if(!missing($name)) { stop("Cannot set constant")}
                |        else {
                |
                """, locals())
            # code.add("""
            #     |    def __set__(self, $py_type $name):
            #     |       raise AttributeError("Cannot set constant")
            #     """, locals())

        else:
            code.add("""
                |    $name = function($name){
                |
                |    if(!missing($name)){
                """, locals())
            # code.add("""
            #     |    def __set__(self, $py_type $name):
            #     """, locals())

            # TODO: add mit indent level
            indented = Code.Code()
            indented.add("""
                       |    if(!($type_check)){ stop(\"arg $name wrong type\") }
                       |
                       """, locals())
            indented.add(conv_code)
            code.add(indented)

            code.add("""
                |        private$$py_obj$$$name <- $call_as
                |        } else {
                """, locals())
            indented = Code.Code()

            if isinstance(cleanup, basestring):
                cleanup = "    %s" % cleanup

            indented.add(cleanup)
            code.add(indented)

        to_r_code = converter.output_conversion(t, "py_ans", "r_result")
        access_stmt = converter.call_method(t,"""private$$py_obj$$%s""" % name)

        cy_type = self.cr.cython_type(t)

        if isinstance(to_r_code, basestring):
            to_r_code = "    %s" % to_r_code

        if isinstance(access_stmt, basestring):
            access_stmt = "        %s" % access_stmt

        if t.is_ptr:
            # For pointer types, we need to guard against unsafe access
            code.add("""
                |        if (is.null(private$$py_obj$$%s)) {
                |           stop("Cannot access NULL pointer")
                |        }
                |        else {
                """ % name)
            # code.add("""
            #     |
            #     |    def __get__(self):
            #     |        if self.inst.get().%s is NULL:
            #     |             raise Exception("Cannot access pointer that is NULL")
            #     """ % name, locals())
        else:
            pass
            # code.add("""
            #     |
            #     |    def __get__(self):
            #     """, locals())
        indented = Code.Code()
        indented.add(access_stmt)
        indented.add(to_r_code)
        code.add(indented)
        code.add("""
            |            return(r_result)
            |            }
            """)

        if t.is_ptr:
            code.add("""
                |        }
                """)

        if attr_cur == total_attr_cnt:
            code.add("""
                |    }
                """)
        else:
            code.add("""
                |    },
                """)
        return code

    def create_wrapper_for_nonoverloaded_method(self, cdcl, py_name, method,meth_cnt,total_method_count,from_overloaded = None):

        L.info("   create wrapper for %s ('%s')" % (py_name, method))
        # Here added code for the method.
        # ref_arg stores the argument numbers which are to be passed by reference.
        # print("meth_cnt = "+str(meth_cnt))
        meth_code = Code.Code()

        call_args, cleanups, in_types, ref_arg = self._create_fun_decl_and_input_conversion(meth_code,py_name,method,for_method = True)

        # call wrapped method and convert result value back to R
        cpp_name = method.cpp_decl.name
        call_args_str = ", ".join(call_args)
        if from_overloaded is True:
            cy_call_str = "private$py_obj$`_%s`(%s)" % (py_name, call_args_str)
        else:
            cy_call_str = "private$py_obj$%s(%s)" % (py_name, call_args_str)
        # cy_call_str = "private$py_obj$%s(%s)" % (cpp_name, call_args_str)

        res_t = method.result_type
        out_converter = self.cr.get(res_t)
        if out_converter.get_base_types()[0] in ["libcpp_map","Map"]:
            cy_call_str = "py_call(private$py_obj$%s,%s)" % (py_name, call_args_str)
        full_call_stmt = out_converter.call_method(res_t, cy_call_str)

        if method.with_nogil:
            # meth_code.add("""
            #   |    with nogil:
            #   """)
            indented = Code.Code()
        else:
            indented = meth_code

        if isinstance(full_call_stmt, basestring):
            indented.add("""
                |    $full_call_stmt
                """, locals())
        else:
            indented.add(full_call_stmt)

        for cleanup in reversed(cleanups):
            if not cleanup:
                continue
            if isinstance(cleanup, basestring):
                cleanup = "    %s" % cleanup
            indented.add(cleanup)

        to_r_code = out_converter.output_conversion(res_t, "py_ans", "r_ans")

        # if to_r_code is not None: # for non void return value
        #     if isinstance(to_r_code,basestring):
        #         to_py_Code = "    %s" % to_r_code
        #     indented.add(to_r_code)
        if to_r_code is not None:  # for non void return value
            print("Non-void return val")
            if(ref_arg != []):
                # Atleast one argument is passed by reference.
                print("Has ref args")
                if isinstance(to_r_code, basestring):
                    to_py_code = "    %s" % to_r_code
                indented.add(to_r_code)
                indented.add("""
                  |
                  |    tryCatch({
                  """)
                for arg,num in ref_arg:
                    indented.add("""
                      |    eval.parent(substitute(%s <- byref_%s))
                      """ % (arg,num))
                indented.add("""
                  |    return(r_ans)
                  |    }, error = function(c) {return(r_ans)}
                  |    )
                  |
                  """)
                if(meth_cnt == total_method_count):
                    indented.add("""
                      |}
                      """)
                else:
                    indented.add("""
                      |},
                      """)
            else:
                print("No ref args")
                if isinstance(to_r_code, basestring):
                    to_py_code = "    %s" % to_r_code
                indented.add(to_r_code)
                indented.add("    return(r_ans)")
                if(meth_cnt == total_method_count):
                    indented.add("""
                      |}
                      """)
                else:
                    indented.add("""
                      |},
                      """)
        else:
            print("void return val")
            if(ref_arg != []):
                print("Has ref args")
                indented.add("""
                  |
                  |    tryCatch({
                  """)
                for arg,num in ref_arg:
                    indented.add("""
                      |    eval.parent(substitute(%s <- byref_%s))
                      """ % (arg,num))
                indented.add("""
                  |    invisible()
                  |    }, error = function(c) {invisible()}
                  |    )
                  |
                  """)
                if(meth_cnt == total_method_count):
                    indented.add("""
                      |}
                      """)
                else:
                    indented.add("""
                      |},
                      """)
            else:
                print("No ref args")
                indented.add("    invisible()")
                if(meth_cnt == total_method_count):
                    indented.add("""
                      |}
                      """)
                else:
                    indented.add("""
                      |},
                      """)

        return meth_code

    def create_wrapper_for_free_function(self, decl):
        L.info("create wrapper for free function %s" % decl.name)
        self.wrapped_methods_cnt += 1
        static_clz = decl.cpp_decl.annotations.get("wrap-attach")
        if static_clz is None:
            code = self._create_wrapper_for_free_function(decl)
        else:
            code = Code.Code()
            static_name = "%s$%s" % (static_clz, decl.name) # name used to attach to class
            # code.add("%s = $static_name" % (decl.name),locals())
            # self.class_codes[static_clz].add(code)
            orig_cpp_name = decl.cpp_decl.name # original cpp name (not displayname)
            code = self._create_wrapper_for_free_function(decl, static_name, orig_cpp_name)

        self.class_codes[static_clz].add(code)

    def _create_wrapper_for_free_function(self, decl, name=None, orig_cpp_name=None):
        if name is None:
            name = decl.name

        # Need to the original cpp name and not the display name (which is for
        # Python only and C++ knows nothing about)
        if orig_cpp_name is None:
            orig_cpp_name = decl.name

        fun_code = Code.Code()

        call_args, cleanups, in_types, ref_arg =\
            self._create_fun_decl_and_input_conversion(fun_code, name, decl, is_free_fun=True)

        call_args_str = ", ".join(call_args)
        # mangled_name = "_" + orig_cpp_name + "_" + decl.pxd_import_path
        cy_call_str = "Pymod$%s(%s)" % (name,call_args_str)

        res_t = decl.result_type
        out_converter = self.cr.get(res_t)
        if out_converter.get_base_types()[0] in ("libcpp_map","Map"):
            cy_call_str = "py_call(private$py_obj$%s,%s)" % (py_name, call_args_str)
        full_call_stmt = out_converter.call_method(res_t, cy_call_str)

        if isinstance(full_call_stmt, basestring):
            fun_code.add("""
                |    $full_call_stmt
                """, locals())
        else:
            fun_code.add(full_call_stmt)

        for cleanup in reversed(cleanups):
            if not cleanup:
                continue
            if isinstance(cleanup, basestring):
                cleanup = "    %s" % cleanup
            fun_code.add(cleanup)

        to_py_code = out_converter.output_conversion(res_t, "py_ans", "r_ans")

        out_vars = ["r_ans"]
        if to_py_code is not None:  # for non void return value

            if isinstance(to_py_code, basestring):
                to_py_code = "    %s" % to_py_code
            fun_code.add(to_py_code)
            fun_code.add("    return(%s)" % (", ".join(out_vars)))

        fun_code.add("}")
        return fun_code

    def create_wrapper_for_constructor(self, class_decl, constructors,meth_cnt, total_method_count):
        real_constructors = []
        codes = []
        # to store number of constructor arguments.
        cons_arg_len = []
        deepcopy_for_cons = False
        for cons in constructors:
            real_constructors.append(cons)
            cons_arg_len.append(len(cons.arguments))
            # commenting for now.
            if len(cons.arguments) == 1:
                (n, t), = cons.arguments
                if t.base_type == class_decl.name and t.is_ref:
                    deepcopy_for_cons = True
            #         code = self.create_special_copy_method(class_decl)
            #         codes.append(code)

        # Flag to check if the class has single constructor only
        is_single_cons = False
        # print("length of non_iter_methods = ",str(len_non_iter_method))

        if len(real_constructors) == 1:
            is_single_cons = True
            if real_constructors[0].cpp_decl.annotations.get("wrap-pass-constructor", False):
                cons_code.add("""
                   |
                   |initialize = function(...){
                   |    stop("Cannot call this constructor")
                   |},
                    """, locals())
                return codes

            code = self.create_wrapper_for_nonoverloaded_constructor(class_decl,
                                                                     "initialize",
                                                                     real_constructors[0],is_single_cons = is_single_cons)
        else:
            dispatched_cons_names = []
            for (i, constructor) in enumerate(real_constructors):
                dispatched_cons_name = "init_%s" % i
                dispatched_cons_names.append(dispatched_cons_name)
                code = self.create_wrapper_for_nonoverloaded_constructor(class_decl,
                                                                         dispatched_cons_name,
                                                                         constructor, is_single_cons = is_single_cons)
                codes.append(code)
                codes.append(Code.Code().add("""
                   |
                   |},
                    """))

            code = self._create_overloaded_method_decl("initialize", dispatched_cons_names,
                                                       constructors, False, True, for_cons = True, r_classname = class_decl.name,deepcopy_for_cons = deepcopy_for_cons)

        codes.append(code)
        if meth_cnt == total_method_count:
            codes.append(Code.Code().add("""
               |
               |}
                """))
        else:
            codes.append(Code.Code().add("""
               |
               |},
                """))

        return codes

    def create_wrapper_for_nonoverloaded_constructor(self, class_decl, r_name,
                                                     cons_decl, is_single_cons = None):
        """ r_name is the name for constructor

        """
        L.info("   create wrapper for non overloaded constructor %s" % r_name)
        cons_code = Code.Code()
        call_args, cleanups, in_types, ref_arg = \
            self._create_fun_decl_and_input_conversion(cons_code, r_name, cons_decl,is_single_cons = is_single_cons, class_decl = class_decl)

        wrap_pass = cons_decl.cpp_decl.annotations.get("wrap-pass-constructor", False)

        if wrap_pass:
            cons_code.add("""
               |    stop("Cannot call this constructor!")
                """)
            return cons_code

        # create instance of wrapped class
        call_args_str = ", ".join(call_args)
        name = class_decl.name
        temp = name.split("_Interfaces_")
        if len(temp) > 1:
            name = "Interfaces$"+ temp[-1]
        print("Name is : ",name," ",class_decl.name)
        # cy_type = self.cr.cython_type(name)
        if is_single_cons in (None,False):
            cons_code.add("""
               |
               |    private$$py_obj <- Pymod$$$name($call_args_str)
               |    invisible()
               |
                """, locals())
        elif is_single_cons is True:
            cons_code.add("""
               |
               |    private$$py_obj <- Pymod$$$name($call_args_str)
               |    invisible()
               |}
                """, locals())

        for cleanup in reversed(cleanups):
            if not cleanup:
                continue
            if isinstance(cleanup, basestring):
                cleanup = "    %s" % cleanup
            cons_code.add(cleanup)

        # if is_single_cons is True and call_args_str == "":
        #     cons_code.add("""
        #        |}
        #         """, locals())

        return cons_code

    def create_special_mul_method(self, cdcl, mdcl):
        L.info("   create wrapper for operator*")
        assert len(mdcl.arguments) == 1, "operator* has wrong signature"
        (__, t), = mdcl.arguments
        name = cdcl.name
        assert t.base_type == name, "can only multiply with myself"
        assert mdcl.result_type.base_type == name, "can only return same type"
        cy_t = self.cr.cython_type(t)
        code = Code.Code()
        code.add("""
        |
        |#' @export
        |`*.$name` <- function(e1,e2){
        |   multiplied <- e1$$.__enclos_env__$$private$$py_obj * e2$$.__enclos_env__$$private$$py_obj
        |   result <- $name$$new(multiplied)
        |   return(result)
        |}
        """, locals())
        # code.add("""
        # |
        # |def __mul__($name self, $name other not None):
        # |    cdef $cy_t * this = self.inst.get()
        # |    cdef $cy_t * that = other.inst.get()
        # |    cdef $cy_t multiplied = deref(this) * deref(that)
        # |    cdef $name result = $name.__new__($name)
        # |    result.inst = shared_ptr[$cy_t](new $cy_t(multiplied))
        # |    return result
        # """, locals())
        return code

    def create_special_add_method(self, cdcl, mdcl):
        L.info("   create wrapper for operator+")
        assert len(mdcl.arguments) == 1, "operator+ has wrong signature"
        (__, t), = mdcl.arguments
        name = cdcl.name
        assert t.base_type == name, "can only add to myself"
        assert mdcl.result_type.base_type == name, "can only return same type"
        cy_t = self.cr.cython_type(t)
        code = Code.Code()
        code.add("""
        |#' @export
        |`+.$name` <- function(e1, e2){
        |   added <- e1$$.__enclos_env__$$private$$py_obj + e2$$.__enclos_env__$$private$$py_obj
        |   result <- $name$$new(added)
        |   return(result)
        |}
        """, locals())
        # code.add("""
        # |
        # |def __add__($name self, $name other not None):
        # |    cdef $cy_t  * this = self.inst.get()
        # |    cdef $cy_t * that = other.inst.get()
        # |    cdef $cy_t added = deref(this) + deref(that)
        # |    cdef $name result = $name.__new__($name)
        # |    result.inst = shared_ptr[$cy_t](new $cy_t(added))
        # |    return result
        # """, locals())
        return code

    def create_special_iadd_method(self, cdcl, mdcl):
        L.info("   create wrapper for operator+")
        assert len(mdcl.arguments) == 1, "operator+ has wrong signature"
        (__, t), = mdcl.arguments
        name = cdcl.name
        assert t.base_type == name, "can only add to myself"
        assert mdcl.result_type.base_type == name, "can only return same type"
        cy_t = self.cr.cython_type(t)
        code = Code.Code()
        code.add("""
        |
        |def __iadd__($name self, $name other not None):
        |    cdef $cy_t * this = self.inst.get()
        |    cdef $cy_t * that = other.inst.get()
        |    _iadd(this, that)
        |    return self
        """, locals())

        tl = Code.Code()
        tl.add("""
                |cdef extern from "autowrap_tools.hpp":
                |    void _iadd($cy_t *, $cy_t *)
                """, locals())

        self.top_level_code.append(tl)

        return code

    def create_special_getitem_method(self, mdcl,cdcl):
        L.info("   create get wrapper for operator[]")
        meth_code = Code.Code()
        class_name = cdcl.name

        meth_code.add("""
                     |    #' @export
                     |    `[.$class_name` <- function(x,ix){
                     |          stopifnot(is.R6(x))         
                     """, locals())

        (call_arg,), cleanups, (in_type,), ref_arg =\
            self._create_fun_decl_and_input_conversion(meth_code, "__getitem__", mdcl,for_modifiers=True)

        meth_code.add("""
                     |    idx = $call_arg
                     """, locals())

        if in_type.is_unsigned:
            meth_code.add("""
                        |    if (idx < 0) { stop(paste("invalid index ",idx)) }
                        """, locals())

        size_guard = mdcl.cpp_decl.annotations.get("wrap-upper-limit")
        if size_guard:
            meth_code.add("""
                     |    if (idx >=  x$$.__enclos_env__$$private$$py_obj$$inst$$get()$$$size_guard) { stop(paste("invalid index",idx)) }
                     """, locals())

        # call wrapped method and convert result value back to python

        cy_call_str = "x$.__enclos_env__$private$py_obj[%s]" % call_arg

        res_t = mdcl.result_type
        out_converter = self.cr.get(res_t)
        # if out_converter.get_base_types()[0] in ("libcpp_map","Map"):
        #     cy_call_str = "py_call(private$py_obj$%s,%s)" % (py_name, call_args_str)
        full_call_stmt = out_converter.call_method(res_t, cy_call_str)

        if isinstance(full_call_stmt, basestring):
            meth_code.add("""
                |    $full_call_stmt
                """, locals())
        else:
            meth_code.add(full_call_stmt)

        for cleanup in reversed(cleanups):
            if not cleanup:
                continue
            if isinstance(cleanup, basestring):
                cleanup = Code.Code().add(cleanup)
            meth_code.add(cleanup)

        out_var = "r_ans"
        to_py_code = out_converter.output_conversion(res_t, "py_ans", out_var)
        if to_py_code is not None:  # for non void return value

            if isinstance(to_py_code, basestring):
                to_py_code = "    %s" % to_py_code
            meth_code.add(to_py_code)
            meth_code.add("    return($out_var)", locals())

        meth_code.add("""
            |}
            """, locals())

        return meth_code

    def create_special_setitem_method(self, mdcl,cdcl):
        # Note: setting will only work with a ref signature
        #   Object operator[](size_t k)  -> only get is implemented
        #   Object& operator[](size_t k) -> get and set is implemented
        res_t = mdcl.result_type
        if not res_t.is_ref:
            L.info("   skip set wrapper for operator[] since return value is not a reference")
            return Code.Code()

        res_t_base = res_t.base_type

        L.info("   create set wrapper for operator[]")
        meth_code = Code.Code()

        call_arg = "as.integer(key)"
        value_arg = "value"
        class_name = cdcl.name

        meth_code.add("""
                     |#' @export
                     |`[<-.$class_name` <- function(x,key,value){
                     |    stopifnot(is.R6(x))
                     |    if(!is.TRUE(all.equal(key,as.integer()))) { stop("arg index wrong type") }
                     |    idx = $call_arg
                     |    if (idx < 0) { stop(paste("invalid index",idx)) }
                     """, locals())
        # meth_code.add("""
        #              |def __setitem__(self, key, $res_t_base value):
        #              |    \"\"\"Test\"\"\"
        #              |    assert isinstance(key, (int, long)), 'arg index wrong type'
        #              |
        #              |    cdef long _idx = $call_arg
        #              |    if _idx < 0:
        #              |        raise IndexError("invalid index %d" % _idx)
        #              """, locals())

        size_guard = mdcl.cpp_decl.annotations.get("wrap-upper-limit")
        if size_guard:
            meth_code.add("""
                     |    if (idx >= x$$.__enclos_env__$$private$$py_obj$$inst$$get()$$$size_guard ) { stop(paste("invalid index",idx)) }
                     """, locals())

        # Store the input argument as
        #  CppObject[ idx ] = value
        #
        cy_call_str = "x$.__enclos_env__$private$py_obj[%s]" % call_arg
        out_converter = self.cr.get(res_t)
        code, call_as, cleanup = out_converter.input_conversion(res_t, value_arg, 0)
        meth_code.add("""
                 |    $cy_call_str = $call_as
                 |}
                 """, locals())
        return meth_code

    def create_cast_methods(self, mdecls,meth_cnt,total_method_count,class_name):
        py_names = []
        for mdcl in mdecls:
            name = mdcl.cpp_decl.annotations.get("wrap-cast")
            if name is None:
                raise Exception("need wrap-cast annotation for %s" % mdcl)
            if name in py_names:
                raise Exception("wrap-cast annotation not unique for %s" % mdcl)
            py_names.append(name)
        codes = []
        for (py_name, mdecl) in zip(py_names, mdecls):
            code = Code.Code()
            res_t = mdecl.result_type
            cy_t = self.cr.cython_type(res_t)
            out_converter = self.cr.get(res_t)

            # code.add("""
            #          |
            #          |def $py_name(self):""", locals())
            # code.add("""
            #          |
            #          |$py_name = function(){""", locals())

            # call_stmt = "<%s>(deref(self.inst.get()))" % cy_t
            call_stmt = "private$py_obj$%s()" % (py_name)
            full_call_stmt = out_converter.call_method(res_t, call_stmt)

            # if isinstance(full_call_stmt, basestring):
            #     code.add("""
            #         |    $full_call_stmt
            #         """, locals())
            # else:
            #     code.add(full_call_stmt)

            to_py_code = out_converter.output_conversion(res_t, "py_ans", "r_ans")
            if isinstance(to_py_code, basestring):
                to_py_code = "    %s" % to_py_code

            if class_name == "DataValue" and py_name == "toString":
                pass
            else:
                code.add("""
                         |$class_name$$set("public","$py_name",
                         |  function(){
                         |      $full_call_stmt
                         |      $to_py_code
                         |      return(r_ans)
                         |  }
                         |)
                         |""", locals())

            codes.append(code)
        return codes

    def create_special_cmp_method(self, cdcl, ops):
        L.info("   create wrapper for comparions methods")
        meth_code = Code.Code()
        name = cdcl.name
        op_code_map = {'<': 0,
                       '==': 2,
                       '>': 4,
                       '<=': 1,
                       '!=': 3,
                       '>=': 5, }
        # inv_op_code_map = dict((v, k) for (k, v) in op_code_map.items())

        # implemented_op_codes = tuple(op_code_map[k] for (k, v) in ops.items() if v)
        implemented_op_codes = list(k for (k,v) in op_code_map.items())
        for op in implemented_op_codes:
            meth_code.add("""
               |#' @export
               |`$op.$name` = function(e1,e2){
               |    stopifnot(is.R6(e1))
               |    stopifnot(is.R6(e2))
               |    tryCatch({
               |        e1$$.__enclos_env__$$private$$py_obj $op e2$$.__enclos_env__$$private$$py_obj
               |    },  error = function(e) { e }
               |    )
               |}
               """, locals())

        # meth_code.add("""
        #    |
        #    |def __richcmp__(self, other, op):
        #    |    if op not in $implemented_op_codes:
        #    |       op_str = $inv_op_code_map[op]
        #    |       raise Exception("comparions operator %s not implemented" % op_str)
        #    |    if not isinstance(other, $name):
        #    |        return False
        #    |    cdef $name other_casted = other
        #    |    cdef $name self_casted = self
        #    """, locals())

        # for op in implemented_op_codes:
        #     op_sign = inv_op_code_map[op]
        #     meth_code.add("""    if op==$op:
        #                     |        return deref(self_casted.inst.get())
        #                     + $op_sign deref(other_casted.inst.get())""",
        #                   locals())
        return meth_code

    # creates copy and deepcopy methods for classes.
    def create_special_copy_method(self, class_decl):
        L.info("   create wrapper __copy__")
        meth_code = Code.Code()
        name = class_decl.name
        cy_type = self.cr.cython_type(name)
        meth_code.add("""
                        |
                        |def __copy__(self):
                        |   cdef $name rv = $name.__new__($name)
                        |   rv.inst = shared_ptr[$cy_type](new $cy_type(deref(self.inst.get())))
                        |   return rv
                        """, locals())
        meth_code.add("""
                        |
                        |def __deepcopy__(self, memo):
                        |   cdef $name rv = $name.__new__($name)
                        |   rv.inst = shared_ptr[$cy_type](new $cy_type(deref(self.inst.get())))
                        |   return rv
                        """, locals())
        return meth_code

    def create_foreign_cimports(self):
        """Iterate over foreign modules and import all relevant classes from them

        It is necessary to let Cython know about other autowrap-created classes
        that may reside in other modules, basically any "cdef" definitions that
        we may be using in this compilation unit. Since we are passing objects
        as arguments quite frequently, we need to know about all other wrapped
        classes and we need to cimport them.
        
        E.g. if we have module1 containing classA, classB and want to access it
        through the pxd header, then we need to add:

            from module1 import classA, classB
        """
        code = Code.Code()
        L.info("Create foreign imports for module %s" % self.target_path)
        for module in self.allDecl:
            # We skip our own module
            if os.path.basename(self.target_path).split(".pyx")[0] != module:

                for resolved in self.allDecl[module]["decls"]:

                    # We need to import classes and enums that could be used in
                    # the Cython code in the current module 

                    # use Cython name, which correctly imports template classes (instead of C name)
                    name = resolved.name

                    if resolved.__class__ in (ResolvedEnum,):
                        if resolved.cpp_decl.annotations.get("wrap-attach"):
                            # No need to import attached classes as they are
                            # usually in the same pxd file and should not be
                            # globally exported.
                            pass
                        else:
                            code.add("from $module cimport $name", locals())
                    if resolved.__class__ in (ResolvedClass, ):

                        # Skip classes that explicitely should not have a pxd
                        # import statement (abstract base classes and the like)
                        if not resolved.no_pxd_import:
                            if resolved.cpp_decl.annotations.get("wrap-attach"):
                                code.add("from $module cimport __$name", locals())
                            else:
                                code.add("from $module cimport $name", locals())

            else: 
                L.info("Skip imports from self (own module %s)" % module)

        self.top_level_code.append(code)

    # def create_cimports(self):
    #     self.create_std_cimports()
    #
    #     self.top_level_code.append(code)

    def create_default_cimports(self):
        code = Code.Code()

        return code

    def create_std_cimports(self):
        code = self.create_default_cimports()
        if self.extra_cimports is not None:
            for stmt in self.extra_cimports:
                code.add(stmt)

        self.top_level_code.append(code)
        return code

    def create_includes(self):
        packages = Code.Code()
        packages.add("""
                |library(reticulate)
                |library(R6)
                |library(purrr)
                |listDepth <- plotrix::listDepth
                |check.numeric <- varhandle::check.numeric
                """)
        self.top_level_code.append(packages)

        helper = Code.Code()
        lib = "pyopenms" if "openms" in self.r_target_path.split('.R')[0].lower() else self.r_target_path.split('.R')[0]
        helper.add("""
                |Pymod <- reticulate::import("%s")
                |reticulate::py_run_string("import gc")
                |copy <- reticulate::import("copy")
                |py_builtin <- reticulate::import_builtins(convert = F)
                |
                |# R6 class object conversion to python class object.
                |`r_to_py.R6` <- function(i,...){
                |   tryCatch({
                |       i$$.__enclos_env__$$private$$py_obj
                |   }, error = function(e) { "conversion not supported for this class"}
                |   )
                |}
                |
                |# python function to convert a python dict having byte type key to R named list with names as string.
                |py_run_string(paste("def transform_dict(d):","    return dict(zip([k.decode('utf-8') for k in d.keys()], d.values()))",sep = "\\n"))
                |
                |# Returns the name of wrapper R6 class
                |class_to_wrap <- function(py_ob){
                |       class <- tail(strsplit(class(py_ob)[1],"\\\.")[[1]],n = 1)
                |       # To correctly return the class name for Interfaces (BinaryDataArray,Chromatogram,Spectrum) by removing "_Interfaces_"
                |       comp <- strsplit(class,"_Interfaces_")[[1]]
                |       if (length(comp) == 1 && comp[1] == class){
                |           return(class)
                |       }
                |       else { return(comp[-1]) }
                |}
                """ % lib)
        self.top_level_code.append(helper)
