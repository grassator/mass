import gdb


SIGNED_INTEGERS = {'s8', 's16', 's32', 's64'}
UNSIGNED_INTEGERS = {'u8', 'u16', 'u32', 'u64'}
INTEGERS = SIGNED_INTEGERS.union(UNSIGNED_INTEGERS)
class ArrayPrinter:
    # The constructor takes the value and stores it for later.
    def __init__(self, val):
        self.val = val

    def data(self):
        return self.val['data'].dereference()

    def children(self):
        data = self.data()
        length = int(data['length'])
        for i in range(length):
            yield ('[%d]' % i, data['items'][i])

    def display_hint(self):
        return 'array'


class SlicePrinter:
    # The constructor takes the value and stores it for later.
    def __init__(self, val):
        self.val = val

    def to_string(self):
        length = int(self.val['length'])
        return self.val['bytes'].string(encoding='utf-8', length=length)

    def display_hint(self):
        return 'string'


def printer(val):
    type_string = str(val.type)
    if type_string.startswith('Array_'):
        return ArrayPrinter(val)
    if type_string == 'Slice':
        return SlicePrinter(val)


