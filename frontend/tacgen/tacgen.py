from unittest import case
from webbrowser import get
import utils.riscv as riscv
from frontend.ast import node
from frontend.ast.tree import *
from frontend.ast.visitor import Visitor
from frontend.symbol.varsymbol import VarSymbol
from frontend.type.array import ArrayType
from utils.tac import tacop
from utils.tac.funcvisitor import FuncVisitor
from utils.tac.programwriter import ProgramWriter
from utils.tac.tacprog import TACProg
from utils.tac.temp import Temp

"""
The TAC generation phase: translate the abstract syntax tree into three-address code.
"""


class TACGen(Visitor[FuncVisitor, None]):
    def __init__(self) -> None:
        pass

    # Entry of this phase
    def transform(self, program: Program) -> TACProg:
        # TODO: Step9-11 为每个函数体进行 TAC 生成
        # mainFunc = program.mainFunc()
        # pw = ProgramWriter(["main"])
        # # The function visitor of 'main' is special.
        # mv = pw.visitMainFunc()

        # mainFunc.body.accept(self, mv)
        # # Remember to call mv.visitEnd after the translation a function.
        # mv.visitEnd()

        # # Remember to call pw.visitEnd before finishing the translation phase.
        # return pw.visitEnd()
        functions = program.functions()
        func_idents = [func for func in functions.keys()]
        pw = ProgramWriter(func_idents)
        nextTempId = 0
        for func_ident in func_idents:
            if func_ident == 'main':
                mv = pw.visitMainFunc()
                mv.nextTempId = nextTempId
                functions[func_ident].body.accept(self, mv)
                mv.visitEnd()
            else:
                paramLen = functions[func_ident].params.__len__
                fv = pw.visitFunc(func_ident, paramLen)
                fv.nextTempId = nextTempId
                for param in functions[func_ident].params:
                    param.accept(self, fv)
                    fv.visitGetParam(param.getattr("symbol").temp)
                functions[func_ident].body.accept(self, fv)
                fv.visitEnd()
                nextTempId = fv.nextTempId
        return pw.visitEnd()
    
    # TODO: Step9-12 新增visitCall函数
    def visitCall(self, call: Call, mv: FuncVisitor) -> None:
        """依次访问所有调用参数, 并将它们的返回值(getattr("val"))依次执行 PARAM 指令，
        然后执行 CALL 指令, 并新建临时变量为函数设置返回值。"""
        for arg in call.arguments.children[::-1]:
            arg.accept(self, mv)
            mv.visitParam(arg.getattr("val"))
        call.setattr("val", mv.visitCall(mv.ctx.getFuncLabel(call.ident.value)))

    def visitBlock(self, block: Block, mv: FuncVisitor) -> None:
        for child in block:
            child.accept(self, mv)

    def visitReturn(self, stmt: Return, mv: FuncVisitor) -> None:
        stmt.expr.accept(self, mv)
        mv.visitReturn(stmt.expr.getattr("val"))

    def visitBreak(self, stmt: Break, mv: FuncVisitor) -> None:
        mv.visitBranch(mv.getBreakLabel())

    def visitIdentifier(self, ident: Identifier, mv: FuncVisitor) -> None:
        """
        1. Set the 'val' attribute of ident as the temp variable of the 'symbol' attribute of ident.
        """
        # TODO: step5-4 设置该表达式的返回值 val 为该变量对应符号里的 symbol
        ident.setattr("val", ident.getattr("symbol").temp)

    def visitDeclaration(self, decl: Declaration, mv: FuncVisitor) -> None:
        """
        1. Get the 'symbol' attribute of decl.
        2. Use mv.freshTemp to get a new temp variable for this symbol.
        3. If the declaration has an initial value, use mv.visitAssignment to set it.
        """
        # TODO: step5-5 生成声明语句的TAC
        # 使用 getattr 方法获得在符号表构建阶段建立的符号
        symbol = decl.getattr("symbol")
        # FuncVisitor.freshTemp 函数获取一个新的临时变量 temp 存储该变量
        symbol.temp = mv.freshTemp()
        # 如果有设置初值， visitAssignment 赋值
        if decl.init_expr != NULL:
            decl.init_expr.accept(self, mv)
            mv.visitAssignment(getattr(symbol, "temp"), decl.init_expr.getattr("val"))

    def visitAssignment(self, expr: Assignment, mv: FuncVisitor) -> None:
        """
        1. Visit the right hand side of expr, and get the temp variable of left hand side.
        2. Use mv.visitAssignment to emit an assignment instruction.
        3. Set the 'val' attribute of expr as the value of assignment instruction.
        """
        # TODO: step5-6 设置赋值语句的中间代码（参考 visitBinary 实现）
        expr.rhs.accept(self, mv)
        # 使用 temp 添加 TAC 指令，而非 var
        temp = expr.lhs.getattr("symbol").temp
        mv.visitAssignment(temp, expr.rhs.getattr("val"))
        # 设置表达式 val
        expr.setattr("val", temp)

    def visitIf(self, stmt: If, mv: FuncVisitor) -> None:
        stmt.cond.accept(self, mv)

        if stmt.otherwise is NULL:
            skipLabel = mv.freshLabel()
            mv.visitCondBranch(
                tacop.CondBranchOp.BEQ, stmt.cond.getattr("val"), skipLabel
            )
            stmt.then.accept(self, mv)
            mv.visitLabel(skipLabel)
        else:
            skipLabel = mv.freshLabel()
            exitLabel = mv.freshLabel()
            mv.visitCondBranch(
                tacop.CondBranchOp.BEQ, stmt.cond.getattr("val"), skipLabel
            )
            stmt.then.accept(self, mv)
            mv.visitBranch(exitLabel)
            mv.visitLabel(skipLabel)
            stmt.otherwise.accept(self, mv)
            mv.visitLabel(exitLabel)

    def visitWhile(self, stmt: While, mv: FuncVisitor) -> None:
        beginLabel = mv.freshLabel()
        loopLabel = mv.freshLabel()
        breakLabel = mv.freshLabel()
        mv.openLoop(breakLabel, loopLabel)

        mv.visitLabel(beginLabel)
        stmt.cond.accept(self, mv)
        mv.visitCondBranch(tacop.CondBranchOp.BEQ, stmt.cond.getattr("val"), breakLabel)

        stmt.body.accept(self, mv)
        mv.visitLabel(loopLabel)
        mv.visitBranch(beginLabel)
        mv.visitLabel(breakLabel)
        mv.closeLoop()
    
    # TODO: Step8-8 新增循环 TAC 生成
    def visitDoWhile(self, stmt: While, mv: FuncVisitor) -> None:
        beginLabel = mv.freshLabel()
        loopLabel = mv.freshLabel()
        breakLabel = mv.freshLabel()
        # 压栈
        mv.openLoop(breakLabel, loopLabel)
        # begin label
        mv.visitLabel(beginLabel)
        stmt.body.accept(self, mv)
        # loop label
        mv.visitLabel(loopLabel)
        stmt.cond.accept(self, mv)
        mv.visitCondBranch(tacop.CondBranchOp.BEQ, stmt.cond.getattr("val"), breakLabel)
        mv.visitBranch(beginLabel)
        # break label
        mv.visitLabel(breakLabel)
        mv.closeLoop()

    def visitFor(self, stmt: For, mv: FuncVisitor) -> None:
        # print(stmt)
        beginLabel = mv.freshLabel()
        loopLabel = mv.freshLabel()
        breakLabel = mv.freshLabel()
        mv.openLoop(breakLabel, loopLabel)
        # init
        if stmt.init is not NULL:
            stmt.init.accept(self, mv)
        # begin label
        mv.visitLabel(beginLabel)
        # condition
        if stmt.cond is not NULL:
            stmt.cond.accept(self, mv)
            mv.visitCondBranch(tacop.CondBranchOp.BEQ, stmt.cond.getattr("val"), breakLabel)
        # body
        stmt.body.accept(self, mv)
        # loop label
        mv.visitLabel(loopLabel)
        # update
        if not stmt.update is NULL:
            stmt.update.accept(self, mv)
        # j begin label
        mv.visitBranch(beginLabel)
        # break label
        mv.visitLabel(breakLabel)
        mv.closeLoop()
    
    def visitContinue(self, stmt: Break, mv: FuncVisitor) -> None:
        mv.visitBranch(mv.getContinueLabel())

    def visitUnary(self, expr: Unary, mv: FuncVisitor) -> None:
        # TODO: step2 完成负、反、非运算
        expr.operand.accept(self, mv)

        op = {
            node.UnaryOp.Neg: tacop.UnaryOp.NEG,        # -
            node.UnaryOp.BitNot: tacop.UnaryOp.NOT,     # ~
            node.UnaryOp.LogicNot: tacop.UnaryOp.SEQZ,  # !
            # You can add unary operations here.
        }[expr.op]
        expr.setattr("val", mv.visitUnary(op, expr.operand.getattr("val")))

    def visitBinary(self, expr: Binary, mv: FuncVisitor) -> None:
        # TODO: step3 完成加、减、乘、除、模运算
        # TODO: step4-1 完成比较、逻辑与或运算
        expr.lhs.accept(self, mv)
        expr.rhs.accept(self, mv)

        op = {
            # 算术运算
            node.BinaryOp.Add: tacop.BinaryOp.ADD,  # +
            node.BinaryOp.Sub: tacop.BinaryOp.SUB,  # -
            node.BinaryOp.Mul: tacop.BinaryOp.MUL,  # *
            node.BinaryOp.Div: tacop.BinaryOp.DIV,  # /
            node.BinaryOp.Mod: tacop.BinaryOp.REM,  # %
            # 比较运算
            node.BinaryOp.LT: tacop.BinaryOp.SLT,  # <
            node.BinaryOp.LE: tacop.BinaryOp.LEQ,  # <=
            node.BinaryOp.GE: tacop.BinaryOp.GEQ,  # >=
            node.BinaryOp.GT: tacop.BinaryOp.SGT,  # >
            node.BinaryOp.EQ: tacop.BinaryOp.EQU,  # ==
            node.BinaryOp.NE: tacop.BinaryOp.NEQ,  # !=
            # 逻辑运算
            node.BinaryOp.LogicAnd: tacop.BinaryOp.AND,  # &&
            node.BinaryOp.LogicOr: tacop.BinaryOp.OR,    # ||
            # You can add binary operations here.
        }[expr.op]
        expr.setattr(
            "val", mv.visitBinary(op, expr.lhs.getattr("val"), expr.rhs.getattr("val"))
        )

    def visitCondExpr(self, expr: ConditionExpression, mv: FuncVisitor) -> None:
        """
        1. Refer to the implementation of visitIf and visitBinary.
        """
        # TODO: Step6-2 仿照 visitIf 实现
        expr.cond.accept(self, mv)
        skipLabel = mv.freshLabel()
        exitLabel = mv.freshLabel()
        # 临时变量存储表达式的值
        temp = mv.freshTemp()
        mv.visitCondBranch(
            tacop.CondBranchOp.BEQ, expr.cond.getattr("val"), skipLabel
        )
        expr.then.accept(self, mv)
        # 临时变量存储 then 表达式的值
        mv.visitAssignment(temp, expr.then.getattr("val"))
        mv.visitBranch(exitLabel)
        mv.visitLabel(skipLabel)
        expr.otherwise.accept(self, mv)
        # 临时变量存储 otherwise 表达式的值
        mv.visitAssignment(temp, expr.otherwise.getattr("val"))
        mv.visitLabel(exitLabel)
        # 完成条件表达式后应进行赋值
        expr.setattr("val", temp)
        
    def visitIntLiteral(self, expr: IntLiteral, mv: FuncVisitor) -> None:
        expr.setattr("val", mv.visitLoad(expr.value))
