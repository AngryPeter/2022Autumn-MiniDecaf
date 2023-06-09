from textwrap import indent
from typing import Protocol, TypeVar, cast

from frontend.ast.node import Node, NullType
from frontend.ast.tree import *
from frontend.ast.visitor import RecursiveVisitor, Visitor
from frontend.scope.globalscope import GlobalScope
from frontend.scope.scope import Scope, ScopeKind
from frontend.scope.scopestack import ScopeStack
from frontend.symbol.funcsymbol import FuncSymbol
from frontend.symbol.symbol import Symbol
from frontend.symbol.varsymbol import VarSymbol
from frontend.type.array import ArrayType
from frontend.type.type import DecafType
from utils.error import *
from utils.riscv import MAX_INT

"""
The namer phase: resolve all symbols defined in the abstract syntax tree and store them in symbol tables (i.e. scopes).
"""


class Namer(Visitor[ScopeStack, None]):
    def __init__(self) -> None:
        pass

    # Entry of this phase
    def transform(self, program: Program) -> Program:
        # Global scope. You don't have to consider it until Step 9.
        program.globalScope = GlobalScope
        ctx = ScopeStack(program.globalScope)

        program.accept(self, ctx)
        return program

    def visitProgram(self, program: Program, ctx: ScopeStack) -> None:
        # Check if the 'main' function is missing
        if not program.hasMainFunc():
            raise DecafNoMainFuncError
        # TODO: Step9-6 依次遍历所有函数
        for function in program.children:
            function.accept(self, ctx)
            # program.mainFunc().accept(self, ctx)
        
    def visitFunction(self, func: Function, ctx: ScopeStack) -> None:
        # TODO: Step9-7 新建符号表
        """首先需要新建一个函数符号 FuncSymbol, Scope(ScopeKind.LOCAL)函数作用域，
        这个作用域既包括函数体内部的变量，也包括函数的所有参数。
        因此，在访问函数体之前，需要先扫描参数列表，
        新增 visitParameter 函数为所有参数建立符号，并存入符号表"""
        symbol = ctx.findConflict(func.ident.value)
        if symbol == None:
            if func.body is NULL:
                symbol = FuncSymbol(func.ident.value, func.ret_t.type, ctx.globalscope, False)
            else:
                symbol = FuncSymbol(func.ident.value, func.ret_t.type, ctx.globalscope, True)
            # 函数名加入全局符号表
            ctx.globalscope.declare(symbol)
            func.setattr("symbol", symbol)
            # localScope = Scope(ScopeKind.LOCAL, True)
            # ctx.open(localScope)
            if not func.params is NULL:
                # 添加参数类型
                for param in func.params:
                    symbol.addParaType(param.var_t.type)
            if not func.body is NULL:
                func.body.add_params(func.params)
                # 定义函数
                func.body.accept(self, ctx)
            # ctx.close()
        else:
            # 重复声明，要求类型一致
            if func.body is NULL and func.ret_t == symbol.type:
                pass
            elif not func.body is NULL: # 定义函数
                if symbol.definition:   # 已经定义
                    raise DecafGlobalVarDefinedTwiceError(func.ident.value)
                else:
                    # 需要让 param 和 body 在同一个作用域
                    # localScope = Scope(ScopeKind.LOCAL) 会导致函数内的 block 不能新开作用域
                    # ctx.open(localScope)
                    if not func.params is NULL:
                        # 添加参数类型
                        for param in func.params:
                            symbol.addParaType(param.var_t.type)
                            # 将参数添加到局部作用域
                        # func.params.accept(self, ctx)
                    # 定义函数
                    else:   # 无参数
                        func.body.add_params(func.params)
                        func.body.accept(self, ctx)
                    symbol.definition = True
                    # ctx.close()

    def visitParameterList(self, params:ParameterList, ctx: ScopeStack) -> None:
        for child in params:
            child.accept(self, ctx)
    
    def visitExpressionList(self, arguments:ExpressionList, ctx: ScopeStack) -> None:
        for child in arguments:
            child.accept(self, ctx)

    def visitCall(self, call: Call, ctx: ScopeStack) -> None:
        # TODO: Step9-8 通过类似 Expression 的方式进行语义检查
        """函数调用除了检查参数是否合法，调用函数是否经过定义之外，
        还要在类型检查(Typer)中检查函数参数和定义是否一致。"""
        symbol = ctx.lookup(call.ident.value)
        if symbol == None:  # 无定义报错
            raise DecafUndefinedFuncError(call.ident.value)
        else:
            if len(call.arguments.children) != symbol.parameterNum:
                # 参数数目不一致
                raise DecafBadFuncCallError(call.ident.value)
            # print(symbol.parameterNum)
            else:
                for i in range(symbol.parameterNum):
                    # TODO: step12-4 检查类型
                    if type(call.arguments.children[i]) == Identifier:
                        arg_symbol = ctx.lookup(call.arguments.children[i].value)
                        # print(arg_symbol.type)
                        # print(symbol.getParaType(i))
                        if type(arg_symbol.type) != type(symbol.getParaType(i)):
                            raise DecafBadFuncCallError(call.ident.value)
            call.arguments.accept(self, ctx)
            
    def visitBlock(self, block: Block, ctx: ScopeStack) -> None:
        # TODO: Step7-1 设置局部作用域
        # print("block-namer:", ctx.currentScope().funcScope)
        # if ctx.currentScope().funcScope:
        #     for child in block:
        #         child.accept(self, ctx)
        # else:
        localScope = Scope(ScopeKind.LOCAL)
        ctx.open(localScope)
        if block.params is not None:
            block.params.accept(self, ctx)
        for child in block:
            child.accept(self, ctx)
        ctx.close()

    def visitReturn(self, stmt: Return, ctx: ScopeStack) -> None:
        stmt.expr.accept(self, ctx)

        """
        def visitFor(self, stmt: For, ctx: ScopeStack) -> None:

        1. Open a local scope for stmt.init.
        2. Visit stmt.init, stmt.cond, stmt.update.
        3. Open a loop in ctx (for validity checking of break/continue)
        4. Visit body of the loop.
        5. Close the loop and the local scope.
        """

    def visitIf(self, stmt: If, ctx: ScopeStack) -> None:
        stmt.cond.accept(self, ctx)
        stmt.then.accept(self, ctx)

        # check if the else branch exists
        if not stmt.otherwise is NULL:
            stmt.otherwise.accept(self, ctx)

    def visitWhile(self, stmt: While, ctx: ScopeStack) -> None:
        stmt.cond.accept(self, ctx)
        ctx.openLoop()
        stmt.body.accept(self, ctx)
        ctx.closeLoop()

    # TODO: Step8-7 新增循环相关符号表遍历函数
    def visitDoWhile(self, stmt: While, ctx: ScopeStack) -> None:
        """
        1. Open a loop in ctx (for validity checking of break/continue)
        2. Visit body of the loop.
        3. Close the loop.
        4. Visit the condition of the loop.
        """
        stmt.body.accept(self, ctx)
        ctx.openLoop()
        ctx.closeLoop()
        stmt.cond.accept(self, ctx)

    def visitFor(self, stmt: For, ctx: ScopeStack) -> None:
        # for 循环需要自带一个作用域
        # 要模仿 visitBlock 函数，打开/关闭对应的局部作用域 "Scope(ScopeKind.LOCAL)"。
        localScope = Scope(ScopeKind.LOCAL)
        ctx.open(localScope)
        if not stmt.init is NULL:
            stmt.init.accept(self, ctx)
        if not stmt.cond is NULL:
            stmt.cond.accept(self, ctx)
        ctx.openLoop()
        stmt.body.accept(self, ctx)
        if not stmt.update is NULL:
            stmt.update.accept(self, ctx)
        ctx.closeLoop()
        ctx.close()

    def visitContinue(self, stmt: Continue, ctx: ScopeStack) -> None:
        """
        1. Refer to the implementation of visitBreak.
        """
        if not ctx.inLoop():
            raise DecafContinueOutsideLoopError()    

    def visitBreak(self, stmt: Break, ctx: ScopeStack) -> None:
        if not ctx.inLoop():
            raise DecafBreakOutsideLoopError()

    def visitDeclaration(self, decl: Declaration, ctx: ScopeStack) -> None:
        """
        1. Use ctx.findConflict to find if a variable with the same name has been declared.
        2. If not, build a new VarSymbol, and put it into the current scope using ctx.declare.
        3. Set the 'symbol' attribute of decl.
        4. If there is an initial value, visit it.
        """
        # TODO: Step5-3 处理声明
        # ScopeStack.findConflict : 报错 or 定义 VarSymbol 对象
        symbol = ctx.findConflict(decl.ident.value)
        if symbol != None:
            raise DecafGlobalVarDefinedTwiceError(decl.ident.value)
        # ScopeStack.declare : 加入符号表
        else:
            # TODO: Step10-3 判断全局变量作用域，并且全局变量只支持常量初始化，需要进行语义检查
            isGlobal = ctx.isGlobalScope()
            symbol = VarSymbol(decl.ident.value, decl.var_t.type, isGlobal)
            ctx.declare(symbol)
            # Declaration.setattr : VarSymbol 存入 AST
            decl.setattr("symbol", symbol)
            if type(decl.var_t.type) == ArrayType:
                decl.ident.setattr("symbol", symbol)
            else:
                # 初值表达式
                if decl.init_expr != NULL:
                    if type(decl.init_expr) == Call and isGlobal:
                        raise DecafGlobalVarBadInitValueError(decl.ident.value)
                    if type(decl.init_expr) == Identifier:
                        rhs_symbol = ctx.lookup(decl.init_expr.value)
                        if type(rhs_symbol.type) == ArrayType:
                            raise DecafTypeMismatchError()
                    decl.init_expr.accept(self, ctx)

    def visitIndexExpr(self, array: IndexExpr, ctx: T) -> None:
        # TODO: step11-3 为数组类型进行类型检查
        array.base.accept(self, ctx)
        array.index.accept(self, ctx)
        # 作为表达式的左值必须是形如 a[0][1][2] 的索引表达式
        symbol = ctx.lookup(array.base.value)
        if symbol.type.dim != len(array.index.children):
            raise DecafBadIndexError(array.base.value)

    def visitAssignment(self, expr: Assignment, ctx: ScopeStack) -> None:
        """
        1. Refer to the implementation of visitBinary.
        """
        # TODO: step5-2 处理赋值
        # ScopeStack.lookup 检查左值是否定义
        if type(expr.lhs) == IndexExpr:   # 数组类型需要检查数组变量名是否已定义
            symbol = ctx.lookup(expr.lhs.base.value)
        else:
            symbol = ctx.lookup(expr.lhs.value)
        if symbol == None:  # 无定义报错
            raise DecafUndefinedVarError(expr.lhs.value)
        else:
            expr.lhs.setattr("symbol", symbol)
            if type(expr.rhs) == Identifier:
                rhs_symbol = ctx.lookup(expr.rhs.value)
                if type(rhs_symbol.type) == ArrayType:
                    raise DecafTypeMismatchError()
        expr.lhs.accept(self, ctx)  # 对数组索引表达式进行类型检查
        # 类似 visitBinary
        expr.rhs.accept(self, ctx)

    def visitUnary(self, expr: Unary, ctx: ScopeStack) -> None:
        expr.operand.accept(self, ctx)

    def visitBinary(self, expr: Binary, ctx: ScopeStack) -> None:
        expr.lhs.accept(self, ctx)
        expr.rhs.accept(self, ctx)

    def visitCondExpr(self, expr: ConditionExpression, ctx: ScopeStack) -> None:
        """
        1. Refer to the implementation of visitBinary.
        """
        # TODO: Step6-1 仿照 visitIf 实现
        expr.cond.accept(self, ctx)
        expr.then.accept(self, ctx)
        expr.otherwise.accept(self, ctx)

    def visitIdentifier(self, ident: Identifier, ctx: ScopeStack) -> None:
        """
        1. Use ctx.lookup to find the symbol corresponding to ident.
        2. If it has not been declared, raise a DecafUndefinedVarError.
        3. Set the 'symbol' attribute of ident.
        """
        # TODO: step5-1 处理定义
        # ScopeStack.lookup : 检查是否已定义
        symbol = ctx.lookup(ident.value)
        if symbol == None:  # 无定义报错
            raise DecafUndefinedVarError(ident.value)
        else:   # 否则设置 symbol 属性
            ident.setattr("symbol", symbol)

    def visitIntLiteral(self, expr: IntLiteral, ctx: ScopeStack) -> None:
        value = expr.value
        if value > MAX_INT:
            raise DecafBadIntValueError(value)