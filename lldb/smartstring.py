# smartstring_lldb.py
import lldb

def _derive_inline_len_by_zero_scan(proc, addr, cap):
    if not addr or cap == 0:
        return 0
    err = lldb.SBError()
    raw = proc.ReadMemory(addr, cap, err)
    if not err.Success() or raw is None:
        return 0
    # find first zero; if none, assume fully used
    zero = raw.find(b'\x00')
    return zero if zero != -1 else cap

def _read_utf8(proc, addr, n):
    err = lldb.SBError()
    raw = proc.ReadMemory(addr, n, err)
    if not err.Success():
        return None
    try:
        return raw.decode('utf-8')
    except Exception:
        return raw.decode('utf-8', 'replace')

def smartstring_summary(val, _internal_dict):
    try:
        target = val.GetTarget()
        proc = target.GetProcess()

        if val.GetTypeName() == 'smartstring::SmartString<smartstring::config::LazyCompact>':
            print("Got lazy compact SmartString, descending...")
            data = val.GetChildMemberWithName("data").GetChildMemberWithName("value").GetChildMemberWithName("value").GetChildMemberWithName("data")
            len = _derive_inline_len_by_zero_scan(proc, data.GetLoadAddress(), data.GetType().GetByteSize())
            if len and data and data.IsValid():
                datum = data
                if datum.GetNumChildren() == 1 and not datum.GetChildAtIndex(0).GetName():
                    datum = datum.GetChildAtIndex(0)
                first = datum.GetChildAtIndex(0)
                if first and first.IsValid():
                    addr = first.AddressOf().GetValueAsUnsigned()
                    s = _read_utf8(proc, addr, len)
                    if s is not None:
                        return f'"{s}"'
        else:
            print(f"SmartString type: {val.GetTypeName()}")
        return '<SmartString: unreadable>'
    except Exception as e:
        return f'<SmartString error: {e}>'

def __lldb_init_module(debugger, _internal_dict):
    # Add summaries first, then enable the category to avoid "empty category" warning
    debugger.HandleCommand(
        'type summary add -w Rust -F smartstring.smartstring_summary -x "^smartstring::SmartString(<.+>)?$"'
    )
    debugger.HandleCommand(
        'type summary add -w Rust -F smartstring.smartstring_summary -x "^smartstring::smartstring::SmartString(<.+>)?$"'
    )
    debugger.HandleCommand(
        'type summary add -w Rust -F smartstring.smartstring_summary -x "^smartstring::alias::String$"'
    )
    debugger.HandleCommand('type category enable Rust')