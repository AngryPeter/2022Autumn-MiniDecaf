from enum import Enum, auto, unique


# Kinds of instructions.
@unique
class InstrKind(Enum):
    # Labels.
    LABEL = auto()
    # Sequential instructions (unary operations, binary operations, etc).
    SEQ = auto()
    # Branching instructions.
    JMP = auto()
    # Branching with conditions.
    COND_JMP = auto()
    # Return instruction.
    RET = auto()


# Kinds of unary operations.
@unique
class UnaryOp(Enum):
    NEG = auto()
    NOT = auto()
    SEQZ = auto()   # set equal zero
    SNEZ = auto()   # set if not equal zero


# Kinds of binary operations.
@unique
class BinaryOp(Enum):
    ADD = auto()
    SUB = auto()
    MUL = auto()
    DIV = auto()
    REM = auto()
    EQU = auto()
    NEQ = auto()
    SLT = auto()
    LEQ = auto()
    SGT = auto()
    GEQ = auto()
    AND = auto()
    OR = auto()


# Kinds of branching with conditions.
@unique
class CondBranchOp(Enum):
    BEQ = auto()
    BNE = auto()
