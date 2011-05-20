module WebKit
{
    import Language;

    language IDL
    {
        syntax Main = empty | m:ModuleDef => m;
        
        interleave Skippable
            = Base.Whitespace
            | Grammar.Comment;
                     
        syntax ModuleDef
            = "module" name:Identifier "{" mod:Module "}" ";"?
                => Module[Name[name], valuesof(mod)];
            
        syntax Module = interfaces:InterfaceDef+ => interfaces;
        
        syntax IntefaceParentElement = id:NamespaceQualifiedType => Parent[id];
        
        syntax InterfaceParent
            = ":" super:List(IntefaceParentElement, ",")
                => super;
        
        syntax InterfaceDef
            = "interface" ea:ExtendedAttributeDef? name:Identifier
              super:InterfaceParent?
              "{" body:InterfaceBody "}" ";"?
                  => Interface[
                      Name[name],
                      valuesof(super),
                      valuesof(ea),
                      valuesof(body)];
        
        syntax ExtendedAttributeDef
            = "[" list:List(ExtendedAttribute, ",") "]" => list;
            
        syntax ExtendedAttribute
            = label:Identifier
                => ExtendedAttribute[Name[label]]
            | label:Identifier "=" value:ExtendedAttributeValue
                => ExtendedAttribute[Name[label], value];
            
        syntax ExtendedAttributeValue
            = val:Guid => Value[val]
            | val:List(Identifier, "&") => Value[valuesof(val)];
            
        syntax InterfaceBody = x:InterfaceElement* => x;
            
        syntax InterfaceElement
            = c:Constant => c
            | a:Attribute => a
            | m:Method => m;
            
        syntax Constant
            = "const" t:NamespaceQualifiedType id:Identifier "=" val:HexDigits ";"
                => Constant[Type[t], Name[id], Value[val]];
                
        syntax Attribute
            = precedence 2: a:ReadOnlyAttribute => a
            | precedence 1: a:ReadWriteAttribute => a;

        syntax ReadOnlyAttribute
            = ro:ReadOnly a:ReadWriteAttribute
                => Attribute[ReadOnly[ro], valuesof(a)];
                
        syntax ReadWriteAttribute
            = "attribute" ea:ExtendedAttributeDef? t:NamespaceQualifiedType id:Identifier r:AttributeRaises?";"
                => Attribute[Type[t], Name[id], valuesof(ea), r];
        
        syntax AttributeRaises
            = "getter" r:Raises => GetterRaises[valuesof(r)]
            | "setter" r:Raises => SetterRaises[valuesof(r)];
        
        syntax Method
            = ea:ExtendedAttributeDef? t:NamespaceQualifiedType id:Identifier "(" args:List(MethodArgument, ",") ")" r:Raises? ";"
                => Method[Type[t], Name[id], valuesof(args), valuesof(ea), r];

        syntax MethodArgument
            = dir:("in" | "out") ea:ExtendedAttributeDef? t:Type id:Identifier
                => Argument[Direction[valuesof(dir)], Type[t], Name[id], valuesof(ea)];

        syntax Raises = "raises" "(" id:List(NamespaceQualifiedType, ",") ")" => Raises[valuesof(id)];
        
        syntax List(Content, Seperator)
            = c:Content => [c]
            | l:List(Content, Seperator) Seperator c:Content => [valuesof(l), c]
            | empty => [];

        syntax NamespaceQualifiedType
            = precedence 1: t:Type => t
            | precedence 2: t:NamespaceQualifiedIdentifier => t;
        
        token NamespaceQualifiedIdentifier = Identifier ("::" Identifier)+;
                    
        syntax Type
            = precedence 3: t:Primitives => t
            | precedence 4: t:UnsignedPrimitives => t
            | precedence 2: t:Identifier => t
            | precedence 1: t:NamespaceQualifiedIdentifier => t;
        
        token Primitives
            = "void"
            | "boolean"
            | "byte"
            | "short"
            | "int"
            | "long"
            | "long long";
        
        token UnsignedPrimitives
            = "unsigned short"
            | "unsigned int"
            | "unsigned long"
            | "unsigned long long";

        token ReadOnly = "readonly";
        
        token Guid = HexDigit#8 "-" HexDigit#4 "-" HexDigit#4 "-" HexDigit#4 "-"HexDigit#12;

        token HexDigit = '0'..'9' | 'a'..'f' | 'A'..'F';
        token HexDigits = "0x"? HexDigit+;
        
        token NonDigit = 'a'..'z' | 'A'..'Z' | '_';
        token Digit = '0'..'9';
        
        token Identifier = NonDigit (NonDigit | Digit)*;
    }
}