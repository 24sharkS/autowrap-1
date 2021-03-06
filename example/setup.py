import os, pkg_resources
from distutils.core import setup, Extension
from Cython.Distutils import build_ext

data_dir = pkg_resources.resource_filename("autowrap", "data_files")
include_dir = os.path.join(data_dir, "autowrap")

ext = Extension("py_libcpp_test",
                sources = ['py_libcpp_test.cpp'],
                language="c++",
                include_dirs = [include_dir, data_dir],
               )

setup(cmdclass={'build_ext':build_ext},
      name="py_libcpp_test",
      version="0.0.1",
      ext_modules = [ext]
)

