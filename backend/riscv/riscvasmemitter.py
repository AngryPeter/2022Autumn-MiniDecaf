from typing import Sequence, Tuple

from backend.asmemitter import AsmEmitter
from utils.error import IllegalArgumentException
from utils.label.label import Label, LabelKind
from utils.riscv import Riscv
from utils.tac.reg import Reg
from utils.tac.tacfunc import TACFunc
from utils.tac.tacinstr import *
from utils.tac.tacvisitor import TACVisitor

from ..subroutineemitter import SubroutineEmitter
from ..subroutineinfo import SubroutineInfo

from utils.tac import tacop
from frontend.ast.tree import Declaration
from frontend.ast.node import NULL
from frontend.type.array import ArrayType
"""
RiscvAsmEmitter: an AsmEmitter for RiscV
"""


class RiscvAsmEmitter(AsmEmitter):
    def __init__(
        self,
        allocatableRegs: list[Reg],
        callerSaveRegs: list[Reg],
        globals: dict[str, Declaration]
    ) -> None:
        super().__init__(allocatableRegs, callerSaveRegs)
        self.globals = globals
        self.bss = {}
        self.data = {}
        self.sp_offset = 0  # 记录开数组时的 SP 偏移，在 exit 时恢复

    
        # the start of the asm code
        # int step10, you need to add the declaration of global var here
        # TODO: Step10-7 处理全局变量的声明: .data & .bss
        for key, value in self.globals.items():
            if value.init_expr is NULL: # 无初值
                self.bss[key] = value
            else:   # 有初值
                self.data[key] = value
        if self.bss:    # 生成 bss 段
            self.printer.println(".bss")
            for key, value in self.bss.items():
                self.printer.println(".globl " + key)
                self.printer.println(key + ":")
                # TODO: step11-6 为全局数组分配空间
                self.printer.println("    " + ".space " + str(value.var_t.type.size))
        if self.data:    # 生成 bss 段
            self.printer.println(".data")
            for key, value in self.data.items():
                self.printer.println(".globl " + key)
                self.printer.println(key + ":")
                if type(value.var_t.type) == ArrayType:
                    for init_value in value.init_expr.values:
                        self.printer.println("    " + ".word " + str(init_value.value))
                    if len(value.init_expr.values) < value.var_t.type.length:
                        self.printer.println("    " + ".zero " + str((value.var_t.type.length - len(value.init_expr.values)) * 4))
                else:
                    self.printer.println("    " + ".word " + str(value.init_expr.value))
        
        self.printer.println(".text")
        self.printer.println(".global main")
        self.printer.println("")

    # transform tac instrs to RiscV instrs
    # collect some info which is saved in SubroutineInfo for SubroutineEmitter
    def selectInstr(self, func: TACFunc) -> tuple[list[str], SubroutineInfo]:

        selector: RiscvAsmEmitter.RiscvInstrSelector = (
            RiscvAsmEmitter.RiscvInstrSelector(func.entry)
        )
        for instr in func.getInstrSeq():
            instr.accept(selector)
            if type(instr) == Alloc:
                self.sp_offset += instr.size

        info = SubroutineInfo(func.entry)

        return (selector.seq, info)

    # use info to construct a RiscvSubroutineEmitter
    def emitSubroutine(self, info: SubroutineInfo):
        return RiscvSubroutineEmitter(self, info, self.sp_offset)

    # return all the string stored in asmcodeprinter
    def emitEnd(self):
        return self.printer.close()

    class RiscvInstrSelector(TACVisitor):
        def __init__(self, entry: Label) -> None:
            self.entry = entry
            self.seq = []
            self.params = 0

        # in step11, you need to think about how to deal with globalTemp in almost all the visit functions. 
        def visitReturn(self, instr: Return) -> None:
            if instr.value is not None:
                self.seq.append(Riscv.Move(Riscv.A0, instr.value))
            else:
                self.seq.append(Riscv.LoadImm(Riscv.A0, 0))
            self.seq.append(Riscv.JumpToEpilogue(self.entry))

        def visitMark(self, instr: Mark) -> None:
            self.seq.append(Riscv.RiscvLabel(instr.label))

        def visitLoadImm4(self, instr: LoadImm4) -> None:
            self.seq.append(Riscv.LoadImm(instr.dst, instr.value))

        def visitUnary(self, instr: Unary) -> None:
            self.seq.append(Riscv.Unary(instr.op, instr.dst, instr.operand))
 
        def visitBinary(self, instr: Binary) -> None:
            # TODO: step4-2 完成指令翻译，将一个TAC翻译成多个RISCV指令
            if instr.op == tacop.BinaryOp.EQU:
                self.seq.append(Riscv.Binary(tacop.BinaryOp.SUB, instr.dst, instr.lhs, instr.rhs))
                self.seq.append(Riscv.Unary(tacop.UnaryOp.SEQZ, instr.dst, instr.dst))
            elif instr.op == tacop.BinaryOp.NEQ:
                self.seq.append(Riscv.Binary(tacop.BinaryOp.SUB, instr.dst, instr.lhs, instr.rhs))
                self.seq.append(Riscv.Unary(tacop.UnaryOp.SNEZ, instr.dst, instr.dst))
            elif instr.op == tacop.BinaryOp.GEQ:
                self.seq.append(Riscv.Binary(tacop.BinaryOp.SLT, instr.dst, instr.lhs, instr.rhs))
                self.seq.append(Riscv.Unary(tacop.UnaryOp.SEQZ, instr.dst, instr.dst))
            elif instr.op == tacop.BinaryOp.LEQ:
                self.seq.append(Riscv.Binary(tacop.BinaryOp.SGT, instr.dst, instr.lhs, instr.rhs))
                self.seq.append(Riscv.Unary(tacop.UnaryOp.SEQZ, instr.dst, instr.dst))
            elif instr.op == tacop.BinaryOp.AND:
                self.seq.append(Riscv.Unary(tacop.UnaryOp.SNEZ, instr.dst, instr.lhs))
                self.seq.append(Riscv.Binary(tacop.BinaryOp.SUB, instr.dst, Riscv.ZERO, instr.dst))
                self.seq.append(Riscv.Binary(tacop.BinaryOp.AND, instr.dst, instr.dst, instr.rhs))
                self.seq.append(Riscv.Unary(tacop.UnaryOp.SNEZ, instr.dst, instr.dst))
            elif instr.op == tacop.BinaryOp.OR:
                self.seq.append(Riscv.Binary(tacop.BinaryOp.OR, instr.dst, instr.lhs, instr.rhs))
                self.seq.append(Riscv.Unary(tacop.UnaryOp.SNEZ, instr.dst, instr.dst))
            else:
                self.seq.append(Riscv.Binary(instr.op, instr.dst, instr.lhs, instr.rhs))

        def visitAssign(self, instr: Assign) -> None:
            # TODO: step5-7 为 TAC 中的 Assign 指令提供汇编指令选择
            self.seq.append(Riscv.Move(instr.dst, instr.src))

        def visitCondBranch(self, instr: CondBranch) -> None:
            self.seq.append(Riscv.Branch(instr.cond, instr.label))
        
        def visitBranch(self, instr: Branch) -> None:
            self.seq.append(Riscv.Jump(instr.target))

        # in step9, you need to think about how to pass the parameters and how to store and restore callerSave regs
        # TODO: Step9-13 新增 visitParam 和 visitDirectCall 函数
        def visitParam(self, instr: Param) -> None:
            """新增 visitParam: 用于保存函数调用的参数（可以保存在栈上或者函数调用寄存器里）"""
            self.seq.append(Riscv.Param(instr.param, instr.index))

        def visitCall(self, instr: Call) -> None:
            """visitDirectCall: 调用函数（函数调用前后 caller-save 寄存器的保存和恢复，
               函数调用以及对函数返回值的处理）。"""
            self.seq.append(Riscv.Call(instr.dst, instr.func))
        
        def visitGetParam(self, instr: GetParam) -> None:
            """获取参数"""
            self.seq.append(Riscv.GetParam(instr.param, instr.index))
        
        # TODO: Step10-8 为 Load 和 LoadSymbol 选择对应的 RISCV指令
        def visitLoad(self, instr: Load) -> None:
            """从地址中获取全局变量"""
            self.seq.append(Riscv.LoadGlobalVar(instr.dst, instr.src, instr.offset))

        def visitStore(self, instr: Store) -> None:
            """向地址为全局变量赋值"""
            self.seq.append(Riscv.Store(instr.data, instr.addr, instr.offset))
        
        def visitLoadSymbol(self, instr: LoadSymbol) -> None:
            """获取全局变量的地址"""
            self.seq.append(Riscv.LoadSymbol(instr.dst, instr.symbol))
        # in step11, you need to think about how to store the array 

        # TODO: step11-7 为数组分配地址空间 Alloc 对应的 riscv 指令生成
        def visitAlloc(self, instr: Alloc) -> None:
            """为数组分配地址空间"""
            self.seq.append(Riscv.Move(instr.dst, Riscv.SP))
"""
RiscvAsmEmitter: an SubroutineEmitter for RiscV
"""

class RiscvSubroutineEmitter(SubroutineEmitter):
    def __init__(self, emitter: RiscvAsmEmitter, info: SubroutineInfo, sp_offset: int) -> None:
        super().__init__(emitter, info)
        self.sp_offset = sp_offset
        
        # + 4 is for the RA reg 
        self.nextLocalOffset = 4 * len(Riscv.CalleeSaved) + 4 + sp_offset
        
        # the buf which stored all the NativeInstrs in this function
        self.buf: list[NativeInstr] = []

        # from temp to int
        # record where a temp is stored in the stack
        self.offsets = {}

        self.printer.printLabel(info.funcLabel)

        # TODO: Step9-14 处理参数和栈的相关行为
        """处理把 Temp 保存到栈上和从栈上把 Temp 读出来的操作。
        （参数在栈上特定的区域）需要处理好 ra 寄存器的保存以及 callee-save 寄存器的保存和恢复。
        可以考虑把所有参数存栈上。需要设计好参数在栈上的布局。
        函数调用前后 caller-save 寄存器的保存和恢复可以考虑通过添加新的伪指令，
        在寄存器分配阶段实现保存和恢复。"""

        # in step9, step11 you can compute the offset of local array and parameters here

    def emitComment(self, comment: str) -> None:
        # you can add some log here to help you debug
        # print(comment)
        pass
    
    # store some temp to stack
    # usually happen when reaching the end of a basicblock
    # in step9, you need to think about the fuction parameters here
    def emitStoreToStack(self, src: Reg) -> None:
        if src.temp.index not in self.offsets:
            self.offsets[src.temp.index] = self.nextLocalOffset
            self.nextLocalOffset += 4
        self.buf.append(
            Riscv.NativeStoreWord(src, Riscv.SP, self.offsets[src.temp.index])
        )

    # load some temp from stack
    # usually happen when using a temp which is stored to stack before
    # in step9, you need to think about the fuction parameters here
    def emitLoadFromStack(self, dst: Reg, src: Temp):
        if src.index not in self.offsets:
            raise IllegalArgumentException()
        else:
            self.buf.append(
                Riscv.NativeLoadWord(dst, Riscv.SP, self.offsets[src.index])
            )

    # add a NativeInstr to buf
    # when calling the fuction emitEnd, all the instr in buf will be transformed to RiscV code
    def emitNative(self, instr: NativeInstr):
        self.buf.append(instr)

    def emitLabel(self, label: Label):
        self.buf.append(Riscv.RiscvLabel(label).toNative([], []))

    
    def emitEnd(self):
        self.printer.printComment("start of prologue")
        self.printer.printInstr(Riscv.SPAdd(-self.nextLocalOffset))

        # in step9, you need to think about how to store RA here
        # you can get some ideas from how to save CalleeSaved regs
        for i in range(len(Riscv.CalleeSaved)):
            if Riscv.CalleeSaved[i].isUsed():
                self.printer.printInstr(
                    Riscv.NativeStoreWord(Riscv.CalleeSaved[i], Riscv.SP, 4 * i + self.sp_offset)
                )
        # save RA
        self.printer.printInstr(
            Riscv.NativeStoreWord(Riscv.RA, Riscv.SP, 4 * len(Riscv.CalleeSaved) + self.sp_offset)
        )

        self.printer.printComment("end of prologue")
        self.printer.println("")

        self.printer.printComment("start of body")

        # in step9, you need to think about how to pass the parameters here
        # you can use the stack or regs

        # using asmcodeprinter to output the RiscV code
        for instr in self.buf:
            self.printer.printInstr(instr)

        self.printer.printComment("end of body")
        self.printer.println("")

        self.printer.printLabel(
            Label(LabelKind.TEMP, self.info.funcLabel.name + Riscv.EPILOGUE_SUFFIX)
        )
        self.printer.printComment("start of epilogue")

        for i in range(len(Riscv.CalleeSaved)):
            if Riscv.CalleeSaved[i].isUsed():
                self.printer.printInstr(
                    Riscv.NativeLoadWord(Riscv.CalleeSaved[i], Riscv.SP, 4 * i + self.sp_offset)
                )
        # load RA
        self.printer.printInstr(
                Riscv.NativeLoadWord(Riscv.RA, Riscv.SP, 4 * len(Riscv.CalleeSaved) + self.sp_offset)
            )

        self.printer.printInstr(Riscv.SPAdd(self.nextLocalOffset))
        self.printer.printComment("end of epilogue")
        self.printer.println("")

        self.printer.printInstr(Riscv.NativeReturn())
        self.printer.println("")
