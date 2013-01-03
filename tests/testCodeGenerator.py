import autowrap.DeclResolver
import autowrap.CodeGenerator
import autowrap.PXDParser
import autowrap.Utils
import autowrap

import os

test_files = os.path.join(os.path.dirname(__file__), "test_files")

def testMinimal():
    target = os.path.join(test_files, "minimal_wrapper.pyx")

    autowrap.parse_and_generate_code("minimal.pxd",
            root=test_files, target=target,  debug=True)

    cpp_source = os.path.join(test_files, "minimal.cpp")


    wrapped = autowrap.Utils.compile_and_import("wrapped", [target, cpp_source],
                                                [test_files])
    os.remove(target)
    assert wrapped.__name__ == "wrapped"

    minimal=wrapped.Minimal()
    assert minimal.compute(3) == 4

    assert minimal.compute(1, 2) == 3
    assert minimal.compute_int(4) == 5
    assert minimal.compute("uwe") == "ewu"
    assert minimal.compute_str("emzed") == "dezme"

    assert minimal.compute_int() == 42

    try:
        minimal.compute(3.0)
    except:
        pass
    else:
        assert False, "expected exception"


    assert minimal.compute_charp("uwe") == 3

    assert minimal.run(minimal) == 4
    assert minimal.run2(minimal) == 5

    assert minimal.create().compute(3) == 4

    assert minimal.sumup([1,2,3]) == 6


    m2 = wrapped.Minimal(-1)
    assert m2.compute(3) == 3

    assert wrapped.ABCorD.A == 0
    assert wrapped.ABCorD.B == 2
    assert wrapped.ABCorD.C == 3
    assert wrapped.ABCorD.D == 4

    assert minimal.call([m2, minimal]) == 1






