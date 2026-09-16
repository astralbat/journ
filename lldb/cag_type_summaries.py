# Can be used within LLDB by executing `command script import $abs_path_to_lldb/type_summaries.py` at the prompt
# or by adding the string to ~/.lldbinit file.

import lldb
from datetime import datetime, timedelta

def adjusted_value_summary(value, internal_dict):
    amount_summary = value.GetChildAtIndex(0).GetSummary()
    value_summary = value.GetChildAtIndex(1).GetSummary()
    expenses_summary = value.GetChildAtIndex(2).GetSummary()
    if amount_summary.startswith("-"):
        return f"{amount_summary} @@ {value_summary} ++ {expenses_summary}"
    else:
        return f"{amount_summary} @@ {value_summary} -- {expenses_summary}"

def amount_adj_summary(value, internal_dict):
    #value = value.GetChildMemberWithName("value")
    return value.GetChildAtIndex(0).GetSummary()

def adjustment_summary(value, internal_dict):
    amount_adjs = value.GetChildMemberWithName("amount_adjustments")
    adj_1 = amount_adjs.GetChildAtIndex(0).GetSummary()
    if (amount_adjs.GetChildAtIndex(1)):
        adj_2 = ", " + amount_adjs.GetChildAtIndex(1).GetSummary()
    else:
        adj_2 = ""
    return f"{adj_1}{adj_2}"

def deal_summary(value, internal_dict):
    av = value.GetChildMemberWithName("adjusted_value").GetSummary()
    return f"{av}"

def single_deal_holding_summary(value, internal_dict):
    return value.GetChildMemberWithName("deal").GetSummary()

def balance_deal_holding_summary(value, internal_dict):
    value = value.GetChildMemberWithName("0")
    if value.GetChildMemberWithName("adjusted_value").IsValid():
        return value.GetChildMemberWithName("adjusted_value").GetSummary()
    else:
        return value.GetSummary()
def balance2_deal_holding_summary(value, internal_dict):
    return value.GetChildMemberWithName("adjusted_value").GetSummary()

def deal_holding_summary(value, internal_dict):
    return value.GetChildAtIndex(0).GetSummary()

def pool_summary(value, internal_dict):
    return value.GetChildMemberWithName("name").GetSummary()

def __lldb_init_module(debugger, internal_dict):
    debugger.HandleCommand(
        'type summary add -F cag_type_summaries.adjusted_value_summary journ_cag::adjusted_value::AdjustedValue'
    )
    debugger.HandleCommand(
        'type summary add -F cag_type_summaries.amount_adj_summary journ_cag::adjustment::AmountAdjustment'
    )
    debugger.HandleCommand(
        'type summary add -F cag_type_summaries.adjustment_summary journ_cag::adjustment::Adjustment'
    )
    debugger.HandleCommand(
        'type summary add -F cag_type_summaries.deal_summary journ_cag::deal::Deal'
    )
    debugger.HandleCommand(
        'type summary add -F cag_type_summaries.balance_deal_holding_summary journ_cag::holding::deal_holding::DealHolding::Sequence'
    )
    debugger.HandleCommand(
        'type summary add -F cag_type_summaries.balance_deal_holding_summary journ_cag::holding::deal_holding::DealHolding::Single'
    )
    debugger.HandleCommand(
        'type summary add -F cag_type_summaries.balance_deal_holding_summary journ_cag::holding::deal_holding::DealHolding::Average'
    )
    debugger.HandleCommand(
        'type summary add -F cag_type_summaries.balance2_deal_holding_summary journ_cag::holding::average::AverageDealHolding'
    )
    debugger.HandleCommand(
        'type summary add -F cag_type_summaries.balance2_deal_holding_summary journ_cag::holding::sequence::SequenceDealHolding'
    )
    debugger.HandleCommand(
        'type summary add -F cag_type_summaries.single_deal_holding_summary journ_cag::holding::single::SingleDealHolding'
    )
    debugger.HandleCommand(
        'type summary add -F cag_type_summaries.balance2_deal_holding_summary journ_cag::holding::summary::DealHoldingSummary'
    )
    debugger.HandleCommand(
        'type summary add -F cag_type_summaries.pool_summary journ_cag::pool::Pool'
    )

