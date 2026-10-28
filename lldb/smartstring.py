# smartstring_lldb.py
import json
import lldb

# Bound debugger memory reads for unusually large strings.
_MAX_SUMMARY_BYTES = 4096


def _child(value, name):
    child = value.GetChildMemberWithName(name)
    if not child.IsValid():
        raise ValueError(f"missing field {name!r}")
    return child.GetNonSyntheticValue()


def _address(value):
    addr = value.GetLoadAddress()
    if addr == lldb.LLDB_INVALID_ADDRESS:
        raise ValueError("value has no load address")
    return addr


def _read_memory(proc, addr, size):
    if size == 0:
        return b""

    err = lldb.SBError()
    raw = proc.ReadMemory(addr, size, err)
    if not err.Success() or raw is None:
        raise ValueError(err.GetCString() or "memory read failed")
    if len(raw) != size:
        raise ValueError(f"short memory read: {len(raw)} of {size} bytes")
    return raw


def _format_bytes(raw, truncated=False):
    # An intentionally truncated read can end inside a UTF-8 character.
    if truncated:
        import codecs
        decoder = codecs.getincrementaldecoder("utf-8")(errors="strict")
        text = decoder.decode(raw, final=False)
    else:
        text = raw.decode("utf-8")

    quoted = json.dumps(text, ensure_ascii=False)
    return quoted + (" … <truncated>" if truncated else "")


def smartstring_summary(val, _internal_dict):
    try:
        val = val.GetNonSyntheticValue()

        # Also allow LLDB to pass a pointer/reference to SmartString.
        typ = val.GetType()
        if typ.IsPointerType() or typ.IsReferenceType():
            val = val.Dereference().GetNonSyntheticValue()
            if not val.IsValid():
                raise ValueError("cannot dereference SmartString")

        target = val.GetTarget()
        proc = target.GetProcess()

        storage = _child(val, "data")
        inline = _child(_child(storage, "value"), "value")
        marker_value = _child(inline, "marker")

        # Marker is a tuple struct containing one u8.
        marker = _read_memory(proc, _address(marker_value), 1)[0]

        if marker & 1:
            # Inline: low bit is the discriminant; upper bits are length.
            length = marker >> 1
            data = _child(inline, "data")
            capacity = data.GetType().GetByteSize()

            if length > capacity:
                raise ValueError(
                    f"invalid inline length {length}, capacity {capacity}"
                )

            raw = _read_memory(proc, _address(data), length)
            return _format_bytes(raw)

        # Heap: BoxedString has an explicit repr(C) layout.
        word_size = target.GetAddressByteSize()
        storage_size = storage.GetType().GetByteSize()
        if storage_size != 3 * word_size:
            raise ValueError(
                f"unsupported storage size {storage_size}; "
                f"expected {3 * word_size}"
            )

        byte_order = target.GetByteOrder()
        if byte_order == lldb.eByteOrderLittle:
            endian = "little"
        elif byte_order == lldb.eByteOrderBig:
            endian = "big"
        else:
            raise ValueError("unsupported target byte order")

        raw = _read_memory(proc, _address(storage), storage_size)
        words = [
            int.from_bytes(raw[i:i + word_size], endian)
            for i in range(0, storage_size, word_size)
        ]

        if endian == "little":
            pointer, capacity, length = words
        else:
            length, capacity, pointer = words

        if length > capacity:
            raise ValueError(
                f"invalid heap length {length}, capacity {capacity}"
            )
        if length and not pointer:
            raise ValueError("null heap pointer")
        if pointer & 1:
            raise ValueError("unaligned heap pointer")

        read_length = min(length, _MAX_SUMMARY_BYTES)
        raw = _read_memory(proc, pointer, read_length)
        return _format_bytes(raw, truncated=(read_length < length))

    except Exception as exc:
        return f"<SmartString error: {exc}>"


def __lldb_init_module(debugger, _internal_dict):
    # Use the actual imported module name instead of hard-coding it.
    callback = f"{__name__}.smartstring_summary"

    for pattern in (
        r"^smartstring::SmartString(<.+>)?$",
        r"^smartstring::smartstring::SmartString(<.+>)?$",
        r"^smartstring::alias::String$",
    ):
        debugger.HandleCommand(
            f'type summary add -w Rust -F {callback} -x "{pattern}"'
        )

    debugger.HandleCommand("type category enable Rust")
