#light
open System
open System.CodeDom
open System.CodeDom.Compiler
open System.Collections.Generic
open System.Dataflow
open System.Diagnostics
open System.IO
open System.Reflection
open System.Reflection.Emit
open System.Text
open System.Text.RegularExpressions

type iDirection = In | Out

type iExtendedAttribute = string * string

type iAttribute =
    { AttributeName: string;
      Type: string;
      ReadOnly: bool;
      ExtendedAttributes: iExtendedAttribute array; }

type iMethodArgument =
    { ArgumentName: string;
      Type: string;
      ExtendedAttributes: iExtendedAttribute array;
      Direction: iDirection; }

type iMethod =
    { MethodName: string;
      Type: string;
      ExtendedAttributes: iExtendedAttribute array;
      Arguments: iMethodArgument array; }

type iConstant =
    { ConstantName: string;
      Type: string;
      Value: obj; }

type iInterface =
    { InterfaceName: string;
      Parents: string array;
      ExtendedAttributes: iExtendedAttribute array;
      Attributes: iAttribute array;
      Methods: iMethod array;
      Constants: iConstant array; }

type iModule =
    { ModuleName: string;
      Interfaces: iInterface array; }
    
type GraphHelper() =
    member x.graphBuilder = GraphBuilder()
    member x.isNode node = x.graphBuilder.IsNode node
    member x.successors node = x.graphBuilder.GetSuccessors node
    member x.label node = x.graphBuilder.GetLabel node :?> Identifier
    member x.labelText node = (x.label node).Text
    member x.seqElements (node:obj) = x.graphBuilder.GetSequenceElements node
    member x.seqElement (node:obj) index = x.graphBuilder.GetSequenceElementAt(node, index)
    member x.hasSeqElement node (name:string) = x.seqElements node |> Seq.exists (fun y -> if x.isNode y then (x.labelText y).Equals(name) else false)
    member x.seqLabel node = x.graphBuilder.GetSequenceLabel node :?> Identifier
    member x.namedElement (node:obj) (name:string) = x.seqElements node |> Seq.find (fun y -> (x.labelText y).Equals(name))
    member x.seqValue (node:obj) name = x.seqElement (x.namedElement node name) 0 :?> string
    member x.hasName node = x.hasSeqElement node "Name"
    member x.hasValue node = x.hasSeqElement node "Value"
    member x.hasParent node = x.hasSeqElement node "Parent"
    member x.name node = x.seqValue node "Name"
    member x.nodeName node = x.name node
    member x.nodeType node = x.seqValue node "Type"
    member x.nodeDirection node = match x.seqValue node "Direction" with "in" -> In | "out" -> Out | _ -> failwith "unknown direction"
    member x.value node = x.seqValue node "Value"
    member x.nodeValue node = x.value node
    member x.nodeParent node = x.seqValue node "Parent"
    member x.nodeParents node = x.seqElements node |> Seq.filter (fun y -> (x.labelText y).Equals("Parent")) |> Seq.map (fun y -> (x.seqElement y 0) :?> string) |> Array.of_seq
    
type Generator() =
    let gh = GraphHelper()

    let cpp fileName =
        let cl = @"c:\Program Files (x86)\Microsoft Visual Studio 9.0\VC\bin\cl.exe"
        let startInfo = ProcessStartInfo(cl, sprintf "/nologo /C /E %s" fileName)
        startInfo.UseShellExecute <- false
        startInfo.RedirectStandardOutput <- true
        startInfo.EnvironmentVariables.["PATH"] <- Environment.GetEnvironmentVariable("PATH") + @";C:\Program Files (x86)\Microsoft Visual Studio 9.0\Common7\IDE"
        
        use cpp = Process.Start(startInfo)
        let processed = cpp.StandardOutput.ReadToEnd()
        Regex.Replace(processed, "^(\#.*)$", String.Empty, RegexOptions.ExplicitCapture ||| RegexOptions.Multiline)
        
    let filter f ie = ie |> Seq.choose f |> Array.of_seq
    let filter2 f node = gh.seqElements node |> filter f
    let strip_namespace (name:string) = name.Split([| "::" |], StringSplitOptions.RemoveEmptyEntries) |> Array.rev |> Seq.hd

    let (|ExtendedAttribute|_|) node =
        try
            match gh.labelText node, gh.hasValue node with
            | "ExtendedAttribute", false -> Some(gh.name node, String.Empty)
            | "ExtendedAttribute", true -> Some(gh.name node, gh.value node)
            | _ -> None
        with
            _ -> None
        
    let filter_ea node = node |> filter2 (function ExtendedAttribute(foo) -> Some(foo) | _ -> None);

    let (|MethodArgument|_|) node =
        match gh.labelText node with
        | "Argument" -> Some {
                ArgumentName = gh.nodeName node;
                Type = gh.nodeType node;
                Direction = gh.nodeDirection node;
                ExtendedAttributes = filter_ea node; }
        | _-> None
        
    let (|Method|_|) node =
        match gh.labelText node with
        | "Method" -> Some {
                MethodName = gh.nodeName node;
                Type = gh.nodeType node;
                Arguments = node |> filter2 (function MethodArgument(foo) -> Some(foo) | _ -> None);
                ExtendedAttributes = filter_ea node; }
        | _ -> None
        
    let (|Attribute|_|) node =
        match gh.labelText node with
        | "Attribute" -> Some {
                AttributeName = gh.nodeName node;
                Type = gh.nodeType node;
                ReadOnly = true; //Boolean.Parse(gh.seqValue node "ReadOnly");
                ExtendedAttributes = filter_ea node; }
        | _ -> None
    
    let (|Constant|_|) node = 
        match gh.labelText node with
        | "Constant" -> Some {
                ConstantName = gh.nodeName node;
                Type = gh.nodeType node;
                Value = gh.nodeValue node; }
        | _ -> None
        
    let (|Interface|_|) (node:obj) =
        match gh.labelText node with
        | "Interface" -> Some {
                InterfaceName = gh.nodeName node;
                Parents = gh.nodeParents node |> Array.map strip_namespace;
                ExtendedAttributes = filter_ea node;
                Attributes = node |> filter2 (function Attribute(foo) -> Some(foo) | _ -> None);
                Methods = node |> filter2 (function Method(foo) -> Some(foo) | _ -> None);
                Constants = node |> filter2 (function Constant(foo) -> Some(foo) | _ -> None); }
        | _ -> None
        
    let (|Module|_|) node =
        match gh.labelText node with
        | "Module" -> Some { ModuleName = gh.name node; Interfaces = node |> filter2 (function Interface(foo) -> Some(foo) | _ -> None); }
        | _ -> None
        
    let get_ea_option iface (name:string) =
        iface.ExtendedAttributes |> Array.first (fun ea -> if (fst ea).Equals(name) then Some(snd ea) else None)
        
    let get_ea_default iface name default_value =
        match get_ea_option iface name with
        | Some(inner) -> inner
        | None -> default_value().ToString()

    let get_ea iface name =
        match get_ea_option iface name with
        | Some(inner) -> inner
        | None -> failwith <| sprintf "extended attribute %s not defined on interface %s" name iface.InterfaceName
    
    let map_ea_to_attributes eas =
        let chooser ea =
            match ea with
            | "InterfaceUUID", _ -> Some(CodeAttributeDeclaration())
            | _ -> None
        
        eas |> Array.choose chooser

    let get_class_name t =
        match t with
        | "EventListener" -> "System.EventHandler"
        | "unsigned long" -> "System.UInt32"
        | "long" -> "System.Int32"
        | "boolean" -> "System.Boolean"
        | t -> t

    let get_parent_interface iface =
        if iface.Parents.Length = 0 then
            "System.Object"
        else
            match strip_namespace iface.Parents.[0] with
            | "EventTargetNode" -> "NWebKit.Node"
            | t -> get_class_name t
        
    member private x.types = Dictionary<string, string>()
    
    member private x.parse fileName =
        let preprocessed = cpp fileName
        use cppReader = new StringReader(preprocessed)
        let language = DynamicParser.LoadFromMgx(@"c:\temp\WebKitClrGenerator\WebKitIDL.mgx", "WebKit.IDL")
        let result = language.Parse<obj>(fileName, cppReader, ErrorReporter.Standard)
        
        match result with Module(mod2) -> mod2 | _ -> failwith "malformed"
        
    member private x.generate interfaces type_to_emit =
        let typeDecl = CodeTypeDeclaration(type_to_emit.InterfaceName)
        typeDecl.IsInterface <- true
        
        let guid = get_ea_default type_to_emit "InterfaceUUID" Guid.NewGuid
        
        typeDecl.CustomAttributes.Add(CodeAttributeDeclaration(CodeTypeReference(typeof<Guid>), [| CodeAttributeArgument(CodePrimitiveExpression(guid)) |])) |> ignore
        webKitNamespace.Types.Add(typeDecl) |> ignore
        
        let gen_method i =
            let meth2 = CodeMemberMethod()
            meth2.Name <- i.MethodName
            meth2.ReturnType <- CodeTypeReference(i.Type)
            meth2.Parameters.AddRange(i.Arguments |> Array.map (fun x -> CodeParameterDeclarationExpression(x.Type, x.ArgumentName)))
            meth2.Attributes <- MemberAttributes.Abstract ||| MemberAttributes.Public
            meth2.CustomAttributes.AddRange(map_ea_to_attributes i.ExtendedAttributes)
            meth2 :> CodeTypeMember

        let gen_attribute i =
            let attr2 = CodeMemberProperty()
            attr2.Name <- i.AttributeName
            attr2.Type <- CodeTypeReference(i.Type)
            attr2.HasGet <- true
            attr2.HasSet <- i.ReadOnly = false
            attr2.Attributes <- MemberAttributes.Abstract ||| MemberAttributes.Public
            attr2 :> CodeTypeMember

        let gen_constant i =
            let const2 = CodeMemberField()
            const2.Name <- i.ConstantName
            const2.Type <- CodeTypeReference(i.Type)
            const2.InitExpression <- CodePrimitiveExpression(i.Value)
            const2.Attributes <- MemberAttributes.Public
            const2 :> CodeTypeMember

        typeDecl.Members.AddRange(type_to_emit.Methods |> Array.map gen_method)
        typeDecl.Members.AddRange(type_to_emit.Attributes |> Array.map gen_attribute)
        typeDecl.Members.AddRange(type_to_emit.Constants |> Array.map gen_constant)
        typeDecl

    member x.main (fileNames:string array) =
        let interfaces = fileNames |> Seq.map_concat (fun fileName -> (x.parse fileName).Interfaces)
                                   |> Seq.map (fun iface -> iface.InterfaceName, iface)
                                   |> Map.of_seq
                                   
        let orphans = interfaces |> Map.to_seq
                                 |> Seq.map_concat (fun iface -> (snd iface).Parents)
                                 |> Seq.filter (fun name -> (interfaces.ContainsKey name) <> true)
                                 |> Seq.distinct
                                 |> Seq.to_array

        if (orphans.Length > 0) then
            printfn "Unable to resolve inherited interfaces, %d failures" orphans.Length
            orphans |> Array.iter (fun orphan -> printfn "> %s" orphan)
                                 
        let webKitNamespace = CodeNamespace("NWebKit")
        let compileUnit =
            let cu = CodeCompileUnit()
            cu.Namespaces.Add(webKitNamespace) |> ignore
            cu

        let codes = interfaces |> Map.mapi (fun key -> fun value -> x.generate interfaces value)

        let codeGenOptions = CodeGeneratorOptions()
        codeGenOptions.BlankLinesBetweenMembers <- true
        codeGenOptions.ElseOnClosing <- false
        codeGenOptions.VerbatimOrder <- false
        codeGenOptions.BracingStyle <- "C"
        
        use cppCodeGen = new Microsoft.VisualC.CppCodeGenerator()
        for code in codes do
            use target = File.CreateText(sprintf @"c:\temp\codegen\CLR_%s.h" code.Key)
            cppCodeGen.GenerateCodeFromCompileUnit(code.Value, target, codeGenOptions)
            target.Flush()
        
Generator().main (Directory.GetFiles(@"d:\WebKit\WebCore\", "*.idl", SearchOption.AllDirectories))
