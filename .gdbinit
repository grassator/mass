python

import os
import sys
import gdb

sys.path.insert(0, os.getcwd())

try:
    import gdb_prelude
    gdb.pretty_printers.append(gdb_prelude.printer)
except ImportError:
    import traceback
    traceback.print_exc()

end
