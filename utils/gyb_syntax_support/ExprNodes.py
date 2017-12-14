from Child import Child
from Node import Node  # noqa: I201

EXPR_NODES = [
    # An inout expression.
    # &x
    Node('InOutExpr', kind='Expr',
         children=[
             Child('Ampersand', kind='AmpersandToken'),
             Child('Identifier', kind='IdentifierToken'),
         ]),

    # A #column expression.
    Node('PoundColumnExpr', kind='Expr',
         children=[
             Child('PoundColumn', kind='PoundColumnToken'),
         ]),

    Node('FunctionCallArgumentList', kind='SyntaxCollection',
         element='FunctionCallArgument'),

    Node('TupleElementList', kind='SyntaxCollection',
         element='TupleElement'),

    Node('ArrayElementList', kind='SyntaxCollection',
         element='ArrayElement'),

    Node('DictionaryElementList', kind='SyntaxCollection',
         element='DictionaryElement'),

    # The try operator.
    # try foo()
    # try? foo()
    # try! foo()
    Node('TryExpr', kind='Expr',
         children=[
             Child('TryKeyword', kind='TryToken'),
             Child('QuestionOrExclamationMark', kind='Token',
                   is_optional=True,
                   token_choices=[
                       'PostfixQuestionMarkToken',
                       'ExclamationMarkToken',
                   ]),
             Child('Expression', kind='Expr'),
         ]),

    # An identifier expression.
    Node('IdentifierExpr', kind='Expr',
         children=[
             Child('Identifier', kind='IdentifierToken'),
         ]),

    # A nil expression.
    Node('NilLiteralExpr', kind='Expr',
         children=[
             Child('NilKeyword', kind='NilToken'),
         ]),

    # A _ expression.
    Node('DiscardAssignmentExpr', kind='Expr',
         children=[
             Child('Wildcard', kind='WildcardToken'),
         ]),

    # An = expression.
    Node('AssignmentExpr', kind='Expr',
         children=[
             Child('AssignToken', kind='EqualToken'),
         ]),

    # A flat list of expressions before sequence folding, e.g. 1 + 2 + 3.
    Node('SequenceExpr', kind='Expr',
         children=[
             Child('Elements', kind='ExprList'),
         ]),

    # A #line expression.
    Node('PoundLineExpr', kind='Expr',
         children=[
             Child('PoundLine', kind='PoundLineToken'),
         ]),

    # A #file expression.
    Node('PoundFileExpr', kind='Expr',
         children=[
             Child('PoundFile', kind='PoundFileToken'),
         ]),

    # A #function expression.
    Node('PoundFunctionExpr', kind='Expr',
         children=[
             Child('PoundFunction', kind='PoundFunctionToken'),
         ]),

    # symbolic-reference-expression -> identifier generic-argument-clause?
    Node('SymbolicReferenceExpr', kind='Expr',
         children=[
             Child('Identifier', kind='IdentifierToken'),
             Child('GenericArgumentClause', kind='GenericArgumentClause',
                   is_optional=True),
         ]),

    # A prefix operator expression.
    # -x
    # !true
    Node('PrefixOperatorExpr', kind='Expr',
         children=[
             Child('OperatorToken', kind='PrefixOperatorToken',
                   is_optional=True),
             Child('PostfixExpression', kind='Expr'),
         ]),

    # An operator like + or -.
    Node('BinaryOperatorExpr', kind='Expr',
         children=[
             Child('OperatorToken', kind='BinaryOperatorToken'),
         ]),

    # A floating-point literal
    # 4.0
    # -3.9
    # +4e20
    Node('FloatLiteralExpr', kind='Expr',
         children=[
             Child('FloatingDigits', kind='FloatingLiteralToken'),
         ]),

    Node('FunctionCallExpr', kind='Expr',
         children=[
             Child('CalledExpression', kind='Expr'),
             Child('LeftParen', kind='LeftParenToken'),
             Child('ArgumentList', kind='FunctionCallArgumentList'),
             Child('RightParen', kind='RightParenToken'),
         ]),

    Node('TupleExpr', kind='Expr',
         children=[
             Child('LeftParen', kind='LeftParenToken'),
             Child('ElementList', kind='TupleElementList'),
             Child('RightParen', kind='RightParenToken'),
         ]),

    # Array literal, e.g. [1, 2, 3]
    Node('ArrayExpr', kind='Expr',
         children=[
             Child('LeftSquare', kind='LeftSquareBracketToken'),
             Child('Elements', kind='ArrayElementList'),
             Child('RightSquare', kind='RightSquareBracketToken'),
         ]),

    # Dictionary literal, e.g. [1:1, 2:2, 3:3]
    Node('DictionaryExpr', kind='Expr',
         children=[
             Child('LeftSquare', kind='LeftSquareBracketToken'),
             Child('Elements', kind='DictionaryElementList'),
             Child('RightSquare', kind='RightSquareBracketToken'),
         ]),

    # function-call-argument -> label? ':'? expression ','?
    Node('FunctionCallArgument', kind='Syntax',
         children=[
             Child('Label', kind='IdentifierToken',
                   is_optional=True),
             Child('Colon', kind='ColonToken',
                   is_optional=True),
             Child('Expression', kind='Expr'),
             Child('TrailingComma', kind='CommaToken',
                   is_optional=True),
         ]),

    # An element inside a tuple element list
    Node('TupleElement', kind='Syntax',
         children=[
             Child('Label', kind='IdentifierToken',
                   is_optional=True),
             Child('Colon', kind='ColonToken',
                   is_optional=True),
             Child('Expression', kind='Expr'),
             Child('TrailingComma', kind='CommaToken',
                   is_optional=True),
         ]),

    # element inside an array expression: expression ','?
    Node('ArrayElement', kind='Syntax',
         children=[
             Child('Expression', kind='Expr'),
             Child('TrailingComma', kind='CommaToken', is_optional=True),
         ]),

    # element inside an array expression: expression ','?
    Node('DictionaryElement', kind='Syntax',
         children=[
             Child('KeyExpression', kind='Expr'),
             Child('Colon', kind='ColonToken'),
             Child('ValueExpression', kind='Expr'),
             Child('TrailingComma', kind='CommaToken', is_optional=True),
         ]),

    # An integer literal.
    # 3
    # +3_400
    # +0x4f
    Node('IntegerLiteralExpr', kind='Expr',
         children=[
             Child('Digits', kind='IntegerLiteralToken'),
         ]),

    Node('StringLiteralExpr', kind='Expr',
         children=[
             Child("StringLiteral", kind='StringLiteralToken')
         ]),

    # true or false
    Node('BooleanLiteralExpr', kind='Expr',
         children=[
             Child("BooleanLiteral", kind='Token',
                   token_choices=[
                       'TrueToken',
                       'FalseToken',
                   ])
         ]),

    # a ? 1 : 0
    Node('TernaryExpr', kind='Expr',
         children=[
             Child("ConditionExpression", kind='Expr'),
             Child("QuestionMark", kind='InfixQuestionMarkToken'),
             Child("FirstChoice", kind='Expr'),
             Child("ColonMark", kind='ColonToken'),
             Child("SecondChoice", kind='Expr')
         ]),

    # a.b
    Node('MemberAccessExpr', kind='Expr',
         children=[
             Child("Base", kind='Expr'),
             Child("Dot", kind='PeriodToken'),
             Child("Name", kind='Token')
         ]),

    # is TypeName
    Node('IsExpr', kind='Expr',
         children=[
             Child("IsTok", kind='IsToken'),
             Child("TypeName", kind='Type')
         ]),

    # as TypeName
    Node('AsExpr', kind='Expr',
         children=[
             Child("AsTok", kind='AsToken'),
             Child("QuestionOrExclamationMark", kind='Token',
                   is_optional=True,
                   token_choices=[
                       'PostfixQuestionMarkToken',
                       'ExclamationMarkToken',
                   ]),
             Child("TypeName", kind='Type')
         ]),

    # Type
    Node('TypeExpr', kind='Expr',
         children=[
             Child('Type', kind='Type'),
         ]),

    Node('ClosureCaptureItem', kind='Syntax',
         children=[
             Child("Specifier", kind='TokenList', is_optional=True),
             Child("Name", kind='IdentifierToken', is_optional=True),
             Child('AssignToken', kind='EqualToken', is_optional=True),
             Child("Expression", kind='Expr'),
             Child('TrailingComma', kind='CommaToken', is_optional=True),
         ]),

    Node('ClosureCaptureItemList', kind='SyntaxCollection',
         element='ClosureCaptureItem'),

    Node('ClosureCaptureSignature', kind='Syntax',
         children=[
             Child('LeftSquare', kind='LeftSquareToken'),
             Child('Items', kind='ClosureCaptureItemList', is_optional=True),
             Child('RightSquare', kind='RightSquareToken'),
         ]),

    Node('ClosureParam', kind='Syntax',
         children=[
             Child('Name', kind='Token',
                   token_choices=[
                       'IdentifierToken',
                       'WildcardToken',
                   ]),
             Child('TrailingComma', kind='CommaToken', is_optional=True),
         ]),

    # a, b, c
    Node('ClosureParamList', kind='SyntaxCollection', element='ClosureParam'),

    Node('ClosureSignature', kind='Syntax',
         children=[
             Child('Capture', kind='ClosureCaptureSignature',
                   is_optional=True),
             # FIXME: one and only one of these two children is required
             Child('SimpleInput', kind='ClosureParamList', is_optional=True),
             Child('Input', kind='ParameterClause', is_optional=True),
             Child('ThrowsTok', kind='ThrowsToken', is_optional=True),
             Child('Output', kind='ReturnClause', is_optional=True),
             Child('InTok', kind='InToken'),
         ]),

    Node('ClosureExpr', kind='Expr',
         children=[
             Child('LeftBrace', kind='LeftBraceToken'),
             Child('Signature', kind='ClosureSignature', is_optional=True),
             Child('Statements', kind='StmtList'),
             Child('RightBrace', kind='RightBraceToken'),
         ]),
]
