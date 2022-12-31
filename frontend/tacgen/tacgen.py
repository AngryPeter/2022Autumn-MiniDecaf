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
from utils.error import *

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
        globals = program.globals()
        func_idents = [func for func in functions.keys()]
        pw = ProgramWriter(func_idents, globals)
        self.pw = pw
        nextTempId = 0
        for func_ident in func_idents:
            if func_ident == 'main':
                mv = pw.visitMainFunc()
                mv.nextTempId = nextTempId
                functions[func_ident].body.accept(self, mv)
                mv.visitEnd()
            elif not functions[func_ident].body is NULL:
                # 只处理定义函数
                paramLen = functions[func_ident].params.__len__
                fv = pw.visitFunc(func_ident, paramLen)
                fv.nextTempId = nextTempId
                index = 0
                # if len(functions[func_ident].params) < 9:
                for param in functions[func_ident].params:
                    param.accept(self, fv)
                    fv.visitGetParam(param.getattr("symbol").temp, index)
                    index += 1
                # else:
                #     for param in functions[func_ident].params[:8]:
                #         param.accept(self, fv)
                #         fv.visitGetParam(param.getattr("symbol").temp, index)
                #         index += 1
                #     for param in functions[func_ident].params[::-1][:-8]:
                #         param.accept(self, fv)
                #         fv.visitGetParam(param.getattr("symbol").temp, index)
                #         index += 1
                functions[func_ident].body.accept(self, fv)
                fv.visitEnd()
                nextTempId = fv.nextTempId
            # 不确定是否要额外处理只声明的函数
        return pw.visitEnd()
    
    # TODO: Step9-12 新增visitCall函数
    def visitCall(self, call: Call, mv: FuncVisitor) -> None:
        """依次访问所有调用参数, 并将它们的返回值(getattr("val"))依次执行 PARAM 指令，
        然后执行 CALL 指令, 并新建临时变量为函数设置返回值。"""
        index = 0
        # if len(call.arguments.children) < 9:
        for arg in call.arguments.children:
            arg.accept(self, mv)
        for arg in call.arguments.children:
            # print(arg.getattr("symbol"))
            symbol = arg.getattr("symbol")
            if symbol != None and symbol.isGlobal and type(symbol.type) == ArrayType:
                mv.visitParam(arg.getattr("addr"), index)
            else:
                mv.visitParam(arg.getattr("val"), index)
            index += 1
        # else:
        #     for arg in call.arguments.children[:8]: # 前 8 个参数使用寄存器顺序传参
        #         arg.accept(self, mv)
        #         mv.visitParam(arg.getattr("val"), index)
        #         index += 1
        #     for arg in call.arguments.children[::-1][:-8]:  # 多余的参数通过栈传参
        #         arg.accept(self, mv)
        #         mv.visitParam(arg.getattr("val"), index)
        #         index += 1
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
        # TODO: step10-6 加载全局变量方式为：首先加载全局变量符号的地址，然后根据地址来加载数据。因此，需要定义两个中间代码指令，完成全局变量值的加载
        symbol = ident.getattr("symbol")
        if symbol.isGlobal:
            for globalVar in self.pw.globals:
                if globalVar == ident.value:    # 变量名匹配
                    src = mv.visitLoadSymbol(ident.value)
                    dst = mv.visitLoadGlobalVar(src, 0)
                    symbol.temp = dst
                    symbol.isGlobal = True
                    ident.setattr("addr", src)
                    ident.setattr("global", True)   # 为之后给全局变量赋值提供信息
                    # print("test: ", ident.getattr("global"))
                    ident.setattr("addr", src)
                    ident.setattr("val", symbol.temp)
                    break
        else:
            # TODO: step5-4 设置该表达式的返回值 val 为该变量对应符号里的 symbol
            ident.setattr("global", False)
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
        if type(symbol.type) == ArrayType:
            if decl.is_param == False:
                symbol.temp = mv.visitAlloc(decl.var_t.type.size)
                # TODO: step12-3 生成数组初始化的 TAC
                if decl.init_expr != NULL:
                    # pass
                    length = len(decl.init_expr.values)
                    for rank in range(length):
                        expr_list = ExpressionList()
                        expr_list.children.append(IntLiteral(rank))
                        idx_expr = IndexExpr(decl.ident, expr_list)
                        value = decl.init_expr.values[rank]
                        assign_expr = Assignment(idx_expr, value)
                        assign_expr.accept(self, mv)
                    if length < symbol.type.length:
                        for rank in range(length, symbol.type.length):
                            expr_list = ExpressionList()
                            expr_list.children.append(IntLiteral(rank))
                            idx_expr = IndexExpr(decl.ident, expr_list)
                            assign_expr = Assignment(idx_expr, IntLiteral(0))
                            assign_expr.accept(self, mv)
            else:
                symbol.temp = mv.freshTemp()
        else:
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
        # TODO: step10-9 将对全局变量的赋值特殊处理
        expr.lhs.accept(self, mv)
        expr.rhs.accept(self, mv)
        if type(expr.lhs) == IndexExpr:
            # TODO: step11-5 数组变量赋值
            temp = expr.rhs.getattr("val")
            mv.visitStore(temp, expr.lhs.getattr("addr"), 0)
            expr.setattr("val", expr.rhs.getattr("val"))
        else:
            if type(expr.lhs.getattr("symbol").type) == ArrayType:
                raise DecafTypeMismatchError()
            temp = expr.lhs.getattr("symbol").temp
            # 使用 temp 添加 TAC 指令，而非 var
            isGlobal = expr.lhs.getattr("global")
            if isGlobal:
                addr = expr.lhs.getattr("addr")
                mv.visitStore(expr.rhs.getattr("val"), addr, 0)
            else:
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

        if expr.operand.getattr("symbol") is not None and type(expr.operand.getattr("symbol").type) == ArrayType:
            raise DecafTypeMismatchError()


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

        if expr.lhs.getattr("symbol") is not None and type(expr.lhs.getattr("symbol").type) == ArrayType:
            raise DecafTypeMismatchError()

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

    def visitIndexExpr(self, expr: IndexExpr, mv: FuncVisitor) -> None:
        # TODO: step11-4 为数组索引表达式生成 TAC 代码
        for child in expr.index.children:
            child.accept(self, mv)
        expr.base.accept(self, mv)
        symbol = expr.base.getattr("symbol")
        indexes = symbol.type.indexes
        offset_list = []
        offset_list.append(mv.visitLoad(4))
        offset = 1
        for i in range(len(indexes) - 1):
            rank = len(indexes) - 1 - i
            offset *= (indexes[rank] * 4)
            offset_list.append(mv.visitLoad(offset))
        offset_list = offset_list[::-1]
        add_temp = mv.visitBinary(tacop.BinaryOp.MUL, offset_list[0], expr.index.children[0].getattr("val"))
        for i in range(1, len(offset_list)):
            new_add_temp = mv.visitBinary(tacop.BinaryOp.MUL, offset_list[i], expr.index.children[i].getattr("val"))
            add_temp = mv.visitBinary(tacop.BinaryOp.ADD, add_temp, new_add_temp)
        if symbol.isGlobal:
            src = mv.visitLoadSymbol(expr.base.value)
            addr = mv.visitBinary(tacop.BinaryOp.ADD, add_temp, src)
        else:
            addr = mv.visitBinary(tacop.BinaryOp.ADD, add_temp, symbol.temp)
        expr.setattr("addr", addr)
        expr.setattr("val", mv.visitLoadGlobalVar(addr, 0))
