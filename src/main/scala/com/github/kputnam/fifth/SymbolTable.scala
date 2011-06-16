package com.github.kputnam.fifth

class SymbolTable
abstract class Symbol
case class TypeSymbol // definition
case class VariableSymbol // type
case class ConstantSymbol // type, value
case class FunctionSymbol // parameter list, output list

abstract class Type
class IntegerType
class FloatType
class BooleanType
class StringType
class TupleType
class RecordType
class FunctionType

abstract class Declaration
class TypeDeclaration
class VariableDeclaration
class ConstantDeclaration
class FunctionDeclaration

abstract class Expression
class ConstantExpression
class ApplyExpression
class QuoteExpression