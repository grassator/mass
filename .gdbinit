python

import os
import sys
import gdb

sys.path.insert(0, os.getcwd())

try:
    import gdb_prelude
    import gdb_mass
    gdb.pretty_printers.append(gdb_prelude.printer)
    gdb.pretty_printers.append(gdb_mass.printer)
except ImportError:
    import traceback
    traceback.print_exc()

end
