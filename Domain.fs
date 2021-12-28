namespace SBash

module Domain =

    type Identifier = Identifier of string
    type Path = Path of string

    // Option for a command. eg: "svar -s" or "<command> --opt1"
    // WithArg represents things like "<command> -s arg1 -t arg2" or "<command> --opt1 arg1"
    type CommandOption = 
        | NoArg of string
        | WithArg of string*Identifier


    type LiteralEqual = char
    type Value = Value of string

    type ControlOperator =
        | Newline of char
        | Semicolon of char

    type CommandArgs = CommandArgs of string
    type Parameter = Parameter of Identifier * Value
    type Command = Command of Path * CommandArgs
    type Declare = Declare of Identifier * CommandOption list
    type SVar = SVar of Identifier * CommandOption list

    type Statement =
        | ParamBindingStatement of Parameter
        | CommandStatement of Command
        | DeclareStatement of Declare
        | SVarStatement of SVar

    type Program = Program of Statement list

    type TypeProvider =
        | CSV
        | Nothing

    type ParamAttr =
        | TPParam of TypeProvider

    type ParameterAttributes = Value * ParamAttr option
    type ParameterTable = ParameterTable of Map<Identifier, ParameterAttributes>
    type AST = AST of ParameterTable * Program