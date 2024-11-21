# Copyright (c) 2019-2023. Mark Barrett
# This file is part of Journ.
# Journ is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version.
# Journ is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details.
# You should have received a copy of the GNU Affero General Public License along with Journ. If not, see <https://www.gnu.org/licenses/>.
import sys
import os
from datetime import datetime
from types import ModuleType
from decimal import *
import bisect

# Append the current working directory to import the journ library.
# TODO Install the journ library somewhere proper on the system?
# sys.path.append(os.getcwd())
# import journ

ledger = ModuleType("ledger")
sys.modules["ledger"] = ledger;

class Error(Exception):
    pass

class PriceLookupError(Error):
    def __init__(self, message):
        self.message = message
