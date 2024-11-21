# Can be used within LLDB by executing `command script import $abs_path_to_lldb/type_summaries.py` at the prompt.

import lldb
from datetime import datetime, timedelta

def option_summary(value, internal_dict):
    return "None"
    return value.GetChildAtIndex(0).GetSummary()
    if value.GetNumChildren() > 0:
        inner_summary = value.GetChildAtIndex(0).GetSummary()
        if inner_summary:
            return f"Some({inner_summary})"
        else:
            return "Some(...)"
    else:
        return "None"

def decimal_summary(value, internal_dict):
    # Assuming `Decimal` has these fields: lo, mid, hi, flags
    lo = value.GetChildMemberWithName("lo").GetValueAsUnsigned()
    mid = value.GetChildMemberWithName("mid").GetValueAsUnsigned()
    hi = value.GetChildMemberWithName("hi").GetValueAsUnsigned()
    flags = value.GetChildMemberWithName("flags").GetValueAsUnsigned()

    # Reconstruct the full 96-bit integer from parts (if that's how it's stored)
    combined = lo + (mid << 32) + (hi << 64)

    # Extract the scale from flags
    scale = (flags >> 16) & 0xFF
    # Extract the sign from flags
    sign = (flags >> 31) & 0x1 == 1 and "-" or ""

    # Convert to a readable decimal string
    scale_factor = 10 ** scale
    integer_part = combined // scale_factor
    fractional_part = combined % scale_factor
    
    return f"{sign}{integer_part}.{fractional_part:0{scale}d}"

def naive_date_summary(value, internal_dict):
    ymdf = value.GetChildMemberWithName("ymdf").GetValueAsUnsigned()
    year = (ymdf >> 13) - 1
    doy = ((ymdf >> 4) & 511) - 1
    timestamp = 86400 * ((year * 365 + year // 4 - year // 100 + year // 400) -
                         (1969 * 365 + 1969 // 4 - 1969 // 100 + 1969 // 400) + doy)
    date = datetime.fromtimestamp(timestamp)
    date_str = date.strftime("%Y-%m-%d")
    return date_str

def naive_time_summary(value, internal_dict):
    secs = value.GetChildMemberWithName("secs").GetValueAsUnsigned()
    time_delta = timedelta(seconds=secs)
    hours, remainder = divmod(time_delta.seconds, 3600)
    minutes, seconds = divmod(remainder, 60)
    return f"{hours:02}:{minutes:02}:{seconds:02}"

def amount_summary(value, internal_dict):
    quantity = value.GetChildMemberWithName("quantity")
    quantity_summary = decimal_summary(quantity, internal_dict)

    unit = value.GetChildMemberWithName("unit")
    unit_code = unit.GetChildMemberWithName("code").GetSummary().strip('"')

    if len(unit_code) == 1:
        return f"{unit_code}{quantity_summary}"
    else:
        return f"{quantity_summary} {unit_code}"
    
def valued_amount_summary(value, internal_dict):
    amount = amount_summary(value.GetChildMemberWithName("amount_expr").GetChildMemberWithName("amount"), internal_dict)

    valuation_str = ""
    some_valuations = value.GetChildMemberWithName("valuations").GetChildAtIndex(0)
    for valuation in some_valuations.GetChildAtIndex(0):
        valuation = valuation.GetChildMemberWithName("value")
        if "Total" in valuation.GetType().GetName():
          valuation_str += " @@ "
          valuation_str += amount_summary(valuation.GetChildAtIndex(0).GetChildMemberWithName("amount"), internal_dict)
    return f"{amount}{valuation_str}"

def pool_balance_summary(value, internal_dict):
    return value.GetChildAtIndex(0).GetSummary()

def amount_adj_summary(value, internal_dict):
    #value = value.GetChildMemberWithName("value")
    return value.GetChildAtIndex(0).GetSummary()

def adjustment_summary(value, internal_dict):
    amount_adj = value.GetChildMemberWithName("amount_adj").GetSummary()
    cons_adj = value.GetChildMemberWithName("consideration_adj").GetSummary()
    return f"{amount_adj}, {cons_adj}"

def deal_summary(value, internal_dict):
    va = value.GetChildMemberWithName("valued_amount").GetSummary()
    expenses = value.GetChildMemberWithName("expenses").GetSummary()
    return f"{va}, {expenses}"

def deal_group_summary(value, internal_dict):
    return value.GetChildMemberWithName("holding").GetChildMemberWithName("0").GetChildMemberWithName("pointer").GetChildMemberWithName("pointer").GetSummary()

def balance_deal_holding_summary(value, internal_dict):
    value = value.GetChildMemberWithName("0")
    if value.GetChildMemberWithName("balance").IsValid():
        return value.GetChildMemberWithName("balance").GetSummary()
    else:
        return value.GetSummary()
def balance2_deal_holding_summary(value, internal_dict):
    return value.GetChildMemberWithName("balance").GetSummary()

def deal_holding_summary(value, internal_dict):
    return value.GetChildAtIndex(0).GetSummary()

def __lldb_init_module(debugger, internal_dict):
    #debugger.HandleCommand(
    #    'type summary add -F type_summaries.option_summary -x core::option::Option<.*>'
    #)
    debugger.HandleCommand(
        'type summary add -F type_summaries.decimal_summary rust_decimal::decimal::Decimal'
    )
    debugger.HandleCommand(
        'type summary add -F type_summaries.naive_date_summary chrono::naive::date::NaiveDate'
    )
    debugger.HandleCommand(
        'type summary add -F type_summaries.naive_time_summary chrono::naive::time::NaiveTime'
    )

    debugger.HandleCommand(
        'type summary add -F type_summaries.amount_summary journ_core::amount::Amount'
    )
    debugger.HandleCommand(
        'type summary add -F type_summaries.valued_amount_summary journ_core::valued_amount::ValuedAmount'
    )
    debugger.HandleCommand(
        'type summary add -F type_summaries.pool_balance_summary journ_tax::pool::PoolBalance'
    )
    debugger.HandleCommand(
        'type summary add -F type_summaries.amount_adj_summary journ_tax::adjustment::AmountAdjustment'
    )
    debugger.HandleCommand(
        'type summary add -F type_summaries.adjustment_summary journ_tax::adjustment::Adjustment'
    )
    debugger.HandleCommand(
        'type summary add -F type_summaries.deal_summary journ_tax::deal::Deal'
    )
    debugger.HandleCommand(
        'type summary add -F type_summaries.deal_group_summary journ_tax::deal_group::DealGroup'
    )
    debugger.HandleCommand(
        'type summary add -F type_summaries.balance_deal_holding_summary journ_tax::deal_holding::DealHolding::Sequence'
    )
    debugger.HandleCommand(
        'type summary add -F type_summaries.balance_deal_holding_summary journ_tax::deal_holding::DealHolding::Single'
    )
    debugger.HandleCommand(
        'type summary add -F type_summaries.balance_deal_holding_summary journ_tax::deal_holding::DealHolding::Group:64'
    )
    debugger.HandleCommand(
        'type summary add -F type_summaries.balance_deal_holding_summary journ_tax::deal_holding::DealHolding::Average'
    )
    debugger.HandleCommand(
        'type summary add -F type_summaries.balance_deal_holding_summary journ_tax::deal_holding::DealHolding::Adjusted'
    )
    debugger.HandleCommand(
        'type summary add -F type_summaries.balance2_deal_holding_summary journ_tax::deal_holding::AdjustedDealHolding'
    )
    debugger.HandleCommand(
        'type summary add -F type_summaries.balance2_deal_holding_summary journ_tax::deal_holding::AverageDealHolding'
    )
    debugger.HandleCommand(
        'type summary add -F type_summaries.balance2_deal_holding_summary journ_tax::deal_holding::SequenceDealHolding'
    )
    debugger.HandleCommand(
        'type summary add -F type_summaries.deal_holding_summary journ_tax::deal_holding::DealHolding'
    )

