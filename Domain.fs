namespace SBash

module Domain =

    type Path = Path of string
    type Argument = 
    | SingleArg of string
    | ArgVal of string*string

    type Identifier = Identifier of string

    type Name = Name of string
    type LiteralEqual = char
    type Value = Value of string

    type ControlOperator =
        | Newline of char
        | Semicolon of char

    type CommandArgs = CommandArgs of string
    type Parameter = Parameter of Name * Value
    type Command = Command of Path * CommandArgs
    type Declare = Declare of Identifier * Argument list

    type Statement =
        | ParamStatement of Parameter
        | CommandStatement of Command
        | DeclareStatement of Declare

    type Program = Program of Statement list

    type TypeProvider =
        | CSV
        | Nothing

    type ASTParamProp =
        | TPParam of TypeProvider
        | Nothing

    type ASTParameter = Identifier * Value * ASTParamProp
    type ParameterTable = ParameterTable of Map<Identifier, ASTParameter>
    type AST = AST of ParameterTable * Program