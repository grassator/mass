class ValueViewPrinter:
    # The constructor takes the value and stores it for later.
    def __init__(self, val):
        self.val = val

    def children(self):
        data = self.val['values'].dereference()
        length = int(self.val['length'])
        for i in range(length):
            yield ('[%d]' % i, data[i])

    def display_hint(self):
        return 'array'

class RegisterBitsetPrinter:

    REGISTER_NAMES = ['A', 'C', 'D', 'B', 'SP', 'BP', 'SI', 'DI']
    # The constructor takes the value and stores it for later.
    def __init__(self, val):
        self.val = val

    def to_string(self):
        registers = []
        bits = int(self.val['bits'])
        for i in range(32):
            if i < 8:
                name = self.REGISTER_NAMES[i]
            elif i < 16:
                name = f"R{i}"
            else:
                name = f"XMM{i}"
            if bits & (1 << i):
                registers.append(name)
        return "{" + ", ".join(registers) + "}"

def printer(val):
    type_string = str(val.type)
    if type_string == 'Value_View':
        return ValueViewPrinter(val)
    if type_string == 'Register_Bitset':
        return RegisterBitsetPrinter(val)
