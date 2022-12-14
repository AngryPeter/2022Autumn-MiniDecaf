import random

from backend.dataflow.basicblock import BasicBlock, BlockKind
from backend.dataflow.cfg import CFG
from backend.dataflow.loc import Loc
from backend.reg.regalloc import RegAlloc
from backend.riscv.riscvasmemitter import RiscvAsmEmitter
from backend.subroutineemitter import SubroutineEmitter
from backend.subroutineinfo import SubroutineInfo
from utils.riscv import Riscv
from utils.tac.holeinstr import HoleInstr
from utils.tac.reg import Reg
from utils.tac.temp import Temp

"""
BruteRegAlloc: one kind of RegAlloc

bindings: map from temp.index to Reg

we don't need to take care of GlobalTemp here
because we can remove all the GlobalTemp in selectInstr process

1. accept：根据每个函数的 CFG 进行寄存器分配，寄存器分配结束后生成相应汇编代码
2. bind：将一个 Temp 与寄存器绑定
3. unbind：将一个 Temp 与相应寄存器解绑定
4. localAlloc：根据数据流对一个 BasicBlock 内的指令进行寄存器分配
5. allocForLoc：每一条指令进行寄存器分配
6. allocRegFor：根据数据流决定为当前 Temp 分配哪一个寄存器
"""

class BruteRegAlloc(RegAlloc):
    def __init__(self, emitter: RiscvAsmEmitter) -> None:
        super().__init__(emitter)
        self.bindings = {}
        for reg in emitter.allocatableRegs:
            reg.used = False

    def accept(self, graph: CFG, info: SubroutineInfo) -> None:
        subEmitter = self.emitter.emitSubroutine(info)
        for bb in graph.iterator():
            # TODO: Step7-3 跳过不可达基本块
            # you need to think more here
            # maybe we don't need to alloc regs for all the basic blocks
            if graph.unreachable(bb.id):
                continue
            if bb.label is not None:
                subEmitter.emitLabel(bb.label)
            self.localAlloc(bb, subEmitter)
        subEmitter.emitEnd()

    def bind(self, temp: Temp, reg: Reg):
        reg.used = True
        self.bindings[temp.index] = reg
        reg.occupied = True
        reg.temp = temp

    def unbind(self, temp: Temp):
        if temp.index in self.bindings:
            self.bindings[temp.index].occupied = False
            self.bindings.pop(temp.index)

    def localAlloc(self, bb: BasicBlock, subEmitter: SubroutineEmitter):
        self.bindings.clear()
        for reg in self.emitter.allocatableRegs:
            reg.occupied = False

        # in step9, you may need to think about how to store callersave regs here
        for loc in bb.allSeq():
            subEmitter.emitComment(str(loc.instr))

            self.allocForLoc(loc, subEmitter)

        for tempindex in bb.liveOut:
            if tempindex in self.bindings:
                subEmitter.emitStoreToStack(self.bindings.get(tempindex))

        if (not bb.isEmpty()) and (bb.kind is not BlockKind.CONTINUOUS):
            self.allocForLoc(bb.locs[len(bb.locs) - 1], subEmitter)

    def allocForLoc(self, loc: Loc, subEmitter: SubroutineEmitter):
        instr = loc.instr
        srcRegs: list[Reg] = []
        dstRegs: list[Reg] = []
        # TODO: step9-15 为函数传参进行特殊指令生成

        if type(instr) == Riscv.Param:
            if instr.index > 7: # 压栈
                temp = instr.srcs[0]
                reg = self.allocRegFor(temp, True, loc.liveIn, subEmitter)
                subEmitter.emitNative(Riscv.NativeStoreWord(reg, Riscv.SP, 4 * (instr.index - 8)))
            else:
                temp = instr.srcs[0]
                reg = Riscv.ArgRegs[instr.index]
                if reg.occupied:
                    subEmitter.emitStoreToStack(reg)
                    subEmitter.emitComment("  spill {} ({})".format(str(reg), str(reg.temp)))
                    self.unbind(reg.temp)
                dstRegs = [reg]
                srcRegs.append(self.allocRegFor(temp, True, loc.liveIn, subEmitter))
                mv_instr = Riscv.Move(reg, self.bindings[temp.index])
                subEmitter.emitNative(mv_instr.toNative(dstRegs, srcRegs))
        elif type(instr) == Riscv.GetParam:
            if instr.index > 7: # 出栈
                temp = instr.dsts[0]
                reg = self.allocRegFor(temp, False, loc.liveIn, subEmitter)
                subEmitter.emitNative(Riscv.NativeLoadWord(reg, Riscv.SP, subEmitter.nextLocalOffset + 4 * (instr.index - 8)))
            else:
                temp = instr.dsts[0]
                if isinstance(temp, Reg):
                    dstRegs.append(temp)
                else:
                    dstRegs.append(self.allocRegFor(temp, False, loc.liveIn, subEmitter))
                reg = Riscv.ArgRegs[instr.index]
                srcRegs = [reg]
                mv_instr = Riscv.Move(dstRegs[0], reg)
                subEmitter.emitNative(mv_instr.toNative(dstRegs, srcRegs))
        elif type(instr) == Riscv.Call:
            # 保存 caller-saved 寄存器
            for i in range(len(Riscv.CallerSaved)):
                if Riscv.CallerSaved[i].isUsed():
                    subEmitter.emitStoreToStack(Riscv.CallerSaved[i])
            temp = instr.dsts[0]
            if isinstance(temp, Reg):
                dstRegs.append(temp)
            else:
                dstRegs.append(self.allocRegFor(temp, False, loc.liveIn, subEmitter))
            srcRegs = [Riscv.A0]
            mv_instr = Riscv.Move(dstRegs[0], srcRegs[0])
            subEmitter.emitNative(instr.toNative(dstRegs, srcRegs))
            subEmitter.emitNative(mv_instr.toNative(dstRegs, srcRegs))
            subEmitter.emitStoreToStack(dstRegs[0])
        else:
            for i in range(len(instr.srcs)):
                temp = instr.srcs[i]
                if isinstance(temp, Reg):
                    srcRegs.append(temp)
                else:
                    srcRegs.append(self.allocRegFor(temp, True, loc.liveIn, subEmitter))

            for i in range(len(instr.dsts)):
                temp = instr.dsts[i]
                if isinstance(temp, Reg):
                    dstRegs.append(temp)
                else:
                    dstRegs.append(self.allocRegFor(temp, False, loc.liveIn, subEmitter))
            subEmitter.emitNative(instr.toNative(dstRegs, srcRegs))
        

    def allocRegFor(
        self, temp: Temp, isRead: bool, live: set[int], subEmitter: SubroutineEmitter
    ):
        if temp.index in self.bindings:
            return self.bindings[temp.index]

        for reg in self.emitter.allocatableRegs:
            if (not reg.occupied) or (not reg.temp.index in live):
                subEmitter.emitComment(
                    "  allocate {} to {}  (read: {}):".format(
                        str(temp), str(reg), str(isRead)
                    )
                )
                if isRead:
                    subEmitter.emitLoadFromStack(reg, temp)
                if reg.occupied:
                    self.unbind(reg.temp)
                self.bind(temp, reg)
                return reg

        reg = self.emitter.allocatableRegs[
            random.randint(0, len(self.emitter.allocatableRegs))
        ]
        subEmitter.emitStoreToStack(reg)
        subEmitter.emitComment("  spill {} ({})".format(str(reg), str(reg.temp)))
        self.unbind(reg.temp)
        self.bind(temp, reg)
        subEmitter.emitComment(
            "  allocate {} to {} (read: {})".format(str(temp), str(reg), str(isRead))
        )
        if isRead:
            subEmitter.emitLoadFromStack(reg, temp)
        return reg
