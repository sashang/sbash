namespace SBash

module Domain =

    type Identifier = Identifier of string
    type Path = Path of string
    type Argument = 
    | SingleArg of string
    | ArgVal of string*Identifier


    type LiteralEqual = char
    type Value = Value of string

    type ControlOperator =
        | Newline of char
        | Semicolon of char

    type CommandArgs = CommandArgs of string
    type Parameter = Parameter of Identifier * Value
    type Command = Command of Path * CommandArgs
    type Declare = Declare of Identifier * Argument list
    type Decltp = Decltp of Identifier * Argument list

    type Statement =
        | ParamBindingStatement of Parameter
        | CommandStatement of Command
        | DeclareStatement of Declare
        | DecltpStatement of Decltp

    type Program = Program of Statement list

    type TypeProvider =
        | CSV
        | Nothing

    type ParamAttr =
        | TPParam of TypeProvider

    type ParameterAttributes = Value * ParamAttr option
    type ParameterTable = ParameterTable of Map<Identifier, ParameterAttributes>
    type AST = AST of ParameterTable * Program