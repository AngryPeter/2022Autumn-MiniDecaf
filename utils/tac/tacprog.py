from typing import Any, Optional, Union

from .tacfunc import TACFunc
from frontend.ast.tree import Declaration


# A TAC program consists of several TAC functions.
class TACProg:
    def __init__(self, funcs: list[TACFunc], globals: dict[str, Declaration]) -> None:
        self.funcs = funcs
        self.globals = globals

    def printTo(self) -> None:
        for func in self.funcs:
            func.printTo()
