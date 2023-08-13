import generated_gdb

class ValueViewPrinter:
    # The constructor takes the value and stores it for later.
    def __init__(self, val):
        self.val = val

    def children(self):
        data = self.val['values'].dereference()
        length = int(self.val['length'])
        for i in range(length):
            yield ('[%d]' % i, data[i])

    @staticmethod
    def display_hint():
        return 'array'

class SymbolPrinter:
    # The constructor takes the value and stores it for later.
    def __init__(self, val):
        self.val = val

    def to_string(self):
        return f"Symbol({self.val['name'].format_string()})"

class HashMapPrinter:
    # The constructor takes the value and stores it for later.
    def __init__(self, val):
        self.val = val

    def is_string_like_key(self):
        key_type = str(self.val['entries'].dereference()['key'].dereference().type.unqualified())
        return key_type == 'Slice' or key_type == 'Symbol'

    def children(self):
        keys = {}
        entries = self.val['entries']
        length = int(self.val['capacity'])
        hint = self.display_hint()
        for i in range(length):
            entry = entries[i]
            if bool(entry['occupied']) and not bool(entry['tombstone']):
                key = entry['key'].dereference()
                value = entry['value'].dereference()
                if hint == 'array':
                    key_string = key.format_string()
                    # for Symbols we can have duplicates which is a bug and nice to see in the debugger
                    if key_string in keys:
                        key_string += " (duplicate)"
                    else:
                        keys[key_string] = True
                    yield key_string, value
                else:
                    yield f"key{i}", key
                    yield f"value{i}", value

    def display_hint(self):
        return 'array' if self.is_string_like_key() else 'map'

class TaggedUnionPrinter:
    # The constructor takes the value and stores it for later.
    def __init__(self, val):
        self.val = val

    def children(self):
        tag = self.val['tag']
        type_name = str(self.val.type.unqualified())
        tag_name = str(tag).replace(type_name + "_Tag_", "")
        for key in self.val.type.keys():
            value = self.val[key]
            if key.startswith('_') and key.endswith('_padding'):
                continue
            # anonymous union
            if key == '':
                if tag_name in value.type.keys():
                    yield (tag_name, value[tag_name])
                else:
                    yield (tag_name, '<invalid tag value>')
            else:
                yield (key, value)

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
    type_string = str(val.type.unqualified())
    if type_string == 'Value_View':
        return ValueViewPrinter(val)
    if type_string == 'Symbol':
        return SymbolPrinter(val)
    if type_string == 'Register_Bitset':
        return RegisterBitsetPrinter(val)
    if type_string in generated_gdb.MASS_TYPES:
        kind = generated_gdb.MASS_TYPES[type_string]
        if kind == 'tagged_union':
            return TaggedUnionPrinter(val)
        if kind == 'hash_map':
            return HashMapPrinter(val)



