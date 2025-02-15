namespace FSharp

    module Binseq =
        
        open System
        open System.IO
        open System.ComponentModel
        open System.Reflection

        [<Literal>]
        let NANOS_PER_TICKS = 100L

        [<AttributeUsage(AttributeTargets.Class)>]
        type TypeIdentifierAttribute(identifier: int) =
            inherit Attribute()
            member val Identifier = identifier

        module TypeIdentifier =
            let inline tryGetTypeId (t: Type) =
                match t.GetTypeInfo() |> fun ti -> ti.GetCustomAttribute(typeof<TypeIdentifierAttribute>) with
                | null -> None
                | (att: Attribute) -> att :?> TypeIdentifierAttribute |> fun a ->  Some a.Identifier

        type Binseq =
        | Reader of BinaryReader
        | Writer of BinaryWriter

        type Binseq<'a> = Binseq -> BinseqResult<'a>*Binseq
        and
            BinseqResult<'a> = Result<'a, string>

        [<AutoOpen>]
        module Functional =
            let inline init (a: 'a) : Binseq<'a> =
                fun bin ->
                    Ok a, bin
            let inline bind (l: Binseq<'a>) (f: 'a -> Binseq<'b>) =
                fun bin ->
                    match l bin with
                    | Ok a, bin -> (f a) bin
                    | Error e, bin -> Error e, bin

            let inline testb (f: Binseq< 'a -> 'b>) (m: Binseq<'a>) : Binseq<'b> =
                bind f (fun x ->
                    bind m (fun t ->
                        init(x t)))
            let inline apply (f: Binseq<'a -> 'b>) (m: Binseq<'a>) : Binseq<'b> =
                bind f (fun f' ->
                    bind m (fun m' ->
                        init (f' m')))
            let inline map (f: 'a -> 'b) (m: Binseq<'a>) : Binseq<'b> =
                bind m (fun m' ->
                    init (f m'))
            let inline map2 (f: 'a -> 'b -> 'c) (m1: Binseq<'a>) (m2: Binseq<'b>) : Binseq<'c> =
                let n1 = init f
                let n2 = apply n1 m1
                let n3 = apply n2 m2
                apply (apply (init f) m1) m2    

        [<AutoOpen>]
        module Operators =
            let inline ( *>) m1 m2 = map2 (fun _ x -> x) m1 m2
            let inline ( ?>) (m: Binseq<'a>) (f: 'a -> 'b) : Binseq<'b> = map f m
            // let inline ( -->) (f: 'a -> 'b) (m: Binseq<'a>) : Binseq<'b> = map f m

        [<AutoOpen>]
        module Builder =
            type BinseqBuilder() = 
                member __.Bind (m1, m2) : Binseq<_> =
                    bind m1 m2

                member __.Combine (m1, m2) : Binseq<_> =
                    bind m1 (fun () -> m2)

                member __.Delay (f) : Binseq<_> =
                    bind (init ()) f

                member __.Return (x) : Binseq<_> =
                    init x

                member __.ReturnFrom (f) : Binseq<_> = f

                member __.Zero () : Binseq<_> =
                    init ()
        let binseq =
            BinseqBuilder ()

        [<RequireQualifiedAccess>]
        module Decode = 
            let inline private withReader<'T> (f: BinaryReader -> 'T) =
                fun bin ->
                    match bin with
                    | Writer _ -> (Error "Can not read from a writer!"),bin
                    | Reader r ->
                        try (f r |> Ok),bin with ex -> (Error $"An exeption was thrown while trying to decode a value.{Environment.NewLine}{ex}"),bin
            let byte : Binseq<byte> = withReader (fun r -> r.ReadByte())
            let int16 : Binseq<int16> = withReader (fun r -> r.ReadInt16())
            let uint16 : Binseq<uint16> = withReader (fun r -> r.ReadUInt16())
            let int : Binseq<int> = withReader (fun r -> r.ReadInt32())
            let uint : Binseq<uint> = withReader (fun r -> r.ReadUInt32())
            let int64 : Binseq<int64> = withReader (fun r -> r.ReadInt64())
            let uint64 : Binseq<uint64> = withReader (fun r -> r.ReadUInt64())

            let char : Binseq<char> = withReader (fun r -> r.ReadChar())
            let string : Binseq<string> = withReader (fun r -> 
                let length = r.ReadInt32()
                match length with | l when l > 0 -> r.ReadChars(length) |> String | _ -> String.Empty
            )
            let inline fixedLengthString length : Binseq<string> = withReader (fun r -> r.ReadChars(length) |> Array.filter (fun c -> c <> '\x00') |> String)
            let float : Binseq<float> = withReader (fun r -> r.ReadDouble())
            let single : Binseq<float32> = withReader (fun r -> r.ReadSingle())
            let bool : Binseq<bool> = withReader (fun r -> r.ReadBoolean())
            let decimal : Binseq<decimal> = withReader (fun r -> r.ReadDecimal())
            let dateTime : Binseq<DateTime> = withReader (fun r -> DateTime.FromBinary(r.ReadInt64()))
            let dateTimeUtcAsUnixNanos : Binseq<DateTime> = withReader (fun r -> DateTime.UnixEpoch.AddTicks(r.ReadInt64() / NANOS_PER_TICKS))
            let tryDateTimeUtcAsUnixNanos : Binseq<DateTime option> = withReader (fun r -> 
                match r.ReadInt64() with
                | n when n <= 0L || n = Int64.MaxValue -> None
                | i -> DateTime.UnixEpoch.AddTicks(i / NANOS_PER_TICKS) |> Some
            )
            let dateTimeOffset : Binseq<DateTimeOffset> = withReader (fun r -> DateTimeOffset(DateTime.FromBinary(r.ReadInt64()), TimeSpan.FromTicks(r.ReadInt64())))
            let dateTimeOffsetUtcAsUnixNanos : Binseq<DateTimeOffset> = withReader (fun r -> DateTimeOffset.UnixEpoch.AddTicks(r.ReadInt64() / NANOS_PER_TICKS))
            let tryDateTimeOffsetUtcAsUnixNanos : Binseq<DateTimeOffset option> = withReader (fun r -> 
                match r.ReadInt64() with
                | n when n <= 0L || n = Int64.MaxValue -> None
                | i -> DateTimeOffset.UnixEpoch.AddTicks(i / NANOS_PER_TICKS) |> Some
            )
            let timeSpan : Binseq<TimeSpan> = withReader (fun r -> TimeSpan.FromTicks(r.ReadInt64()))
            let timeSpanAsNanos : Binseq<TimeSpan> = withReader (fun r -> TimeSpan.FromTicks(r.ReadInt64() / NANOS_PER_TICKS))
            let guid : Binseq<Guid> = withReader (fun r -> Guid(r.ReadBytes(16)))
            let bytes : Binseq<byte array> = withReader (fun r -> r.ReadBytes(r.ReadInt32()))
            let inline skip length : Binseq<unit> = withReader (fun r -> r.ReadBytes(length) |> ignore)
            let inline returnNoRead a : Binseq<'a> = init a
            
            let inline arrayOf<'a> (readElement: Binseq<'a>) : Binseq<'a array> = fun bin ->
                match bin with
                | Writer _ -> (Error "Can not read from a writer!"),bin
                | Reader r ->
                try 
                    let length = r.ReadInt32()
                    let executed = 
                        ((Reader r), 0) 
                        |> Array.unfold (fun state -> 
                            match state with 
                            | (bin,l) when l < length -> 
                                let (a, next) = readElement bin
                                Some (a, (next, l + 1))
                            | _ -> None
                            )
                    executed 
                    |> Array.choose (function | Error e -> Some (Error e) | _ -> None ) 
                    |> Array.tryHead
                    |> Option.map (fun e -> e,bin)
                    |> Option.defaultValue ((executed |> Array.choose (function | Ok value -> Some value | _ -> None) |> Ok),bin)
                    
                with ex -> (Error $"An exeption was thrown while trying to decode array.{Environment.NewLine}{ex}"),bin

            let inline optionOf<'a> (readElement: Binseq<'a>) : Binseq<'a option> = fun bin ->
                match bin with
                | Writer _ -> (Error "Can not read from a writer!"),bin
                | Reader r ->
                    try
                        match r.ReadBoolean() with
                        | true -> 
                            match readElement bin with
                            | Ok a,_ -> Ok (Some a), bin
                            | Error e,_ -> Error e, bin
                        | _ -> Ok None, bin
                    with ex -> (Error $"An exeption was thrown while trying to decode an option.{Environment.NewLine}{ex}"),bin

            let inline resultOf<'a, 'b> (readSuccess: Binseq<'a>) (readError: Binseq<'b>) : Binseq<Result<'a,'b>> = fun bin ->
                match bin with
                | Writer _ -> (Error "Can not read from a writer!"),bin
                | Reader r ->
                    try
                        match r.ReadBoolean() with
                        | true -> 
                            match readSuccess bin with
                            | Ok a,_ -> Ok (Ok a), bin
                            | Error e,_ -> Error e, bin
                        | _ -> 
                            match readError bin with
                            | Ok b,_ -> Ok (Error b), bin
                            | Error e,_ -> Error e, bin
                    with ex -> (Error $"An exeption was thrown while trying to decode an option.{Environment.NewLine}{ex}"),bin

            let inline error<'a> s : Binseq<'a> = 
                fun (binseq: Binseq) ->
                    (Error s),binseq

            let bytesToHex bytes = 
                bytes 
                |> Array.map (fun (x : byte) -> System.String.Format("{0:X2}", x))
                |> String.concat String.Empty

        [<RequireQualifiedAccess>]
        module Stream = 
            let inline toHeapBuffer (stream: Stream) =
                use ms = new MemoryStream()
                stream.CopyTo(ms)
                ms.ToArray()
        
        [<RequireQualifiedAccess>]
        type Encode =
            [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
            static member inline private encodeWithWriter f : Binseq<unit> = (fun p ->
                match p with
                | Reader _ -> failwith "Can not write to a reader!"
                | Writer w -> match f w with | Ok () -> (Ok ()),p | Error (e: string) -> (Error e),p)
            [<EditorBrowsableAttribute(EditorBrowsableState.Never)>]
            static member inline private safeCage f =
                try
                    f()
                    Ok ()
                with ex -> Error $"An exeption was thrown while trying to encode a value.{Environment.NewLine}{ex}"
            static member inline byte (x: byte) : Binseq<unit> = Encode.encodeWithWriter (fun w -> Encode.safeCage (fun () -> w.Write(x)))
            static member inline uint16 (x: uint16) : Binseq<unit> = Encode.encodeWithWriter (fun w -> Encode.safeCage (fun () -> w.Write(x)))
            static member inline int16 (x: int16) : Binseq<unit> = Encode.encodeWithWriter (fun w -> Encode.safeCage (fun () -> w.Write(x)))
            static member inline int (x: int) : Binseq<unit> = Encode.encodeWithWriter (fun w -> Encode.safeCage (fun () -> w.Write(x)))
            static member inline uint (x: uint) : Binseq<unit> = Encode.encodeWithWriter (fun w -> Encode.safeCage (fun () -> w.Write(x)))
            static member inline int64 (x: int64) : Binseq<unit> = Encode.encodeWithWriter (fun w -> Encode.safeCage (fun () -> w.Write(x)))
            static member inline uint64 (x: uint64) : Binseq<unit> = Encode.encodeWithWriter (fun w -> Encode.safeCage (fun () -> w.Write(x)))
            static member inline float (x: float) : Binseq<unit> = Encode.encodeWithWriter (fun w -> Encode.safeCage (fun () -> w.Write(x)))
            static member inline single (x: float32) : Binseq<unit> = Encode.encodeWithWriter (fun w -> Encode.safeCage (fun () -> w.Write(x)))
            static member inline bool (x: bool) : Binseq<unit> = Encode.encodeWithWriter (fun w -> Encode.safeCage (fun () -> w.Write(x)))
            static member inline decimal (x: decimal) : Binseq<unit> = Encode.encodeWithWriter (fun w -> Encode.safeCage (fun () -> w.Write(x)))
            static member inline char (x: char) : Binseq<unit> = Encode.encodeWithWriter (fun w -> Encode.safeCage (fun () -> w.Write(x)))
            static member inline dateTime (x: DateTime) : Binseq<unit> = Encode.encodeWithWriter (fun w -> Encode.safeCage (fun () -> w.Write(x.ToBinary())))
            static member inline dateTimeUtcAsUnixNanos (x: DateTime) : Binseq<unit> = Encode.encodeWithWriter (fun w -> Encode.safeCage (fun () -> w.Write((x.ToUniversalTime().Ticks - DateTimeOffset.UnixEpoch.Ticks) * NANOS_PER_TICKS)))
            static member inline dateTimeOffset (x: DateTimeOffset) : Binseq<unit> = Encode.encodeWithWriter (fun w -> Encode.safeCage (fun () -> w.Write(x.Ticks); w.Write(x.Offset.Ticks)))
            static member inline dateTimeOffsetUtcAsUnixNanos (x: DateTimeOffset) : Binseq<unit> = Encode.encodeWithWriter (fun w -> Encode.safeCage (fun () -> w.Write((x.UtcTicks - DateTimeOffset.UnixEpoch.Ticks) * NANOS_PER_TICKS)))
            static member inline timeSpan (x: TimeSpan) : Binseq<unit> = Encode.encodeWithWriter (fun w -> Encode.safeCage (fun () -> w.Write(x.Ticks)))
            static member inline timeSpanAsNanos (x: TimeSpan) : Binseq<unit> = Encode.encodeWithWriter (fun w -> Encode.safeCage (fun () -> w.Write(x.Ticks * NANOS_PER_TICKS)))
            static member inline guid (x: Guid) : Binseq<unit> = Encode.encodeWithWriter (fun w -> Encode.safeCage (fun () -> w.Write(x.ToByteArray())))
            static member inline bytes (x: byte array) : Binseq<unit> = Encode.encodeWithWriter (fun w -> Encode.safeCage (fun () -> w.Write(x.Length); w.Write(x, 0, x.Length)))
            static member inline appendPadding length : Binseq<unit> = Encode.encodeWithWriter (fun w -> Encode.safeCage (fun () -> w.Write(Array.zeroCreate<byte> length, 0, length)))
            static member inline string (x: string) : Binseq<unit> = 
                Encode.encodeWithWriter (fun w -> Encode.safeCage (fun () -> 
                    let c = x.ToCharArray()
                    w.Write c.Length
                    if c.Length > 0 then w.Write c
                    )
                )
            static member inline fixedLengthString length (x: string) : Binseq<unit> =
                Encode.encodeWithWriter (fun w -> Encode.safeCage (fun () -> 
                    let c = x.ToCharArray()
                    w.Write c
                    if c.Length > length then
                        failwith $"String length {c.Length} exceeds the fixed length {length}"
                    else
                        w.Write (Array.init (length - c.Length) (fun _ -> 0uy))
                    )
                )
            static member inline arrayOf<'a> (writeElement: 'a -> Binseq<unit>) (x: 'a array) : Binseq<unit> = fun (bin: Binseq) ->
                match bin with
                | Reader _ -> failwith "Can not write to a reader!"
                | Writer w ->
                    let originalPosition = w.BaseStream.Position
                    try
                        w.Write(x.Length)
                        
                        let result =
                            x 
                            |> Array.mapFold (fun b element -> writeElement element b) bin
                            |> fst
                            |> Array.choose (function | Error e -> Some (Error e) | _ -> None) 
                            |> Array.tryHead 
                            |> Option.defaultValue (Ok ())

                        (result),bin

                    with ex -> 
                        if w.BaseStream.CanSeek then w.BaseStream.Seek(originalPosition, SeekOrigin.Begin) |> ignore
                        Error $"An exeption was thrown while trying to encode array.{Environment.NewLine}{ex}",bin

            static member inline optionOf<'a> (writeElement: 'a -> Binseq<unit>) (x: 'a option) : Binseq<unit> = fun (bin: Binseq) ->
                match bin with
                | Reader _ -> failwith "Can not write to a reader!"
                | Writer w ->
                    let originalPosition = w.BaseStream.Position
                    try
                        match x with 
                        | None -> 
                            w.Write(false)
                            (Ok ()),bin
                        | Some a -> 
                            w.Write(true)
                            writeElement a bin
                    with ex -> 
                        if w.BaseStream.CanSeek then w.BaseStream.Seek(originalPosition, SeekOrigin.Begin) |> ignore
                        Error $"An exeption was thrown while trying to encode an option.{Environment.NewLine}{ex}",bin

            static member inline resultOf<'a,'b> (writeSuccess: 'a -> Binseq<unit>) (writeError: 'b -> Binseq<unit>) (x: Result<'a,'b>) : Binseq<unit> = fun (bin: Binseq) ->
                match bin with
                | Reader _ -> failwith "Can not write to a reader!"
                | Writer w ->
                    let originalPosition = w.BaseStream.Position
                    try
                        match x with 
                        | Ok a -> 
                            w.Write(true)
                            writeSuccess a bin
                        | Error b -> 
                            w.Write(false)
                            writeError b (Writer w)
                    with ex -> 
                        if w.BaseStream.CanSeek then w.BaseStream.Seek(originalPosition, SeekOrigin.Begin) |> ignore
                        Error $"An exeption was thrown while trying to encode a result.{Environment.NewLine}{ex}",bin

            // static member inline unsafeWriteRecord (continueWith: Stream -> 'b) (writeHeader: int64 -> Binseq<unit>) (writeData: Binseq<unit>)  =
            //     let ptr = Microsoft.FSharp.NativeInterop.NativePtr.stackalloc<byte> 512
            //     use ms = new UnmanagedMemoryStream(ptr, 1, 512, FileAccess.ReadWrite)
            //     try
            //         Encode.writeRecord ms continueWith writeHeader writeData
            //     finally
            //         ms.Close()
            //         Microsoft.FSharp.NativeInterop.NativePtr.clear ptr
            

        [<RequireQualifiedAccess>]
        module Record =
            let inline write (writer: BinaryWriter) (continueWith: BinaryWriter -> 'b) (writeHeader: int64 -> Binseq<unit>) (writeData: Binseq<unit>)  =
                try
                    let initialPosition = writer.BaseStream.Position
                    match writeHeader 0L (Writer writer) with 
                    | Error e,_ -> Error e 
                    | _ ->
                        writer.Flush()
                        let headerLength = writer.BaseStream.Position - initialPosition
                        match writeData (Writer writer) with
                        | Error e,_ -> Error e
                        | _ ->
                            writer.Flush()
                            let length = writer.BaseStream.Position - headerLength
                            if writer.BaseStream.CanSeek then writer.BaseStream.Seek(initialPosition, SeekOrigin.Begin) |> ignore
                            let writeHeader' = writeHeader length
                            match writeHeader' (Writer writer) with
                            | Error e,_ -> Error e
                            | _ -> 
                                writer.Flush()
                                writer.BaseStream.Seek(initialPosition, SeekOrigin.Begin) |> ignore
                                Ok (continueWith writer)
                with ex -> Error $"An exeption was thrown while trying to binary write a value.{Environment.NewLine}{ex}"
            let inline writeStream (stream: Stream) (continueWith: BinaryWriter -> 'b) (writeHeader: int64 -> Binseq<unit>) (writeData: Binseq<unit>)  =
                use w = new BinaryWriter(stream, Text.Encoding.Default, true)
                try
                    write w continueWith writeHeader writeData
                finally
                    w.Close()
            let inline toBuffer writeHeader writeData =
                use ms = new MemoryStream()
                writeStream ms (fun _ -> ms.ToArray()) writeHeader writeData

            let inline read (readHeader: Binseq<'header>) (readData: 'header -> Binseq<'data>) (reader: BinaryReader) =
                try
                    match readHeader (Reader reader) with
                    | Error e,_ -> Error e
                    | Ok header,_ -> 
                        match readData header (Reader reader) with
                        | Error e,_ -> Error e
                        | Ok data,_ -> Ok data
                with ex -> Error $"An exeption was thrown while trying to binary read a value.{Environment.NewLine}{ex}"

            let inline readStream (readHeader: Binseq<'header>) (readData: 'header -> Binseq<'data>) (stream: Stream) =
                use r = new BinaryReader(stream, Text.Encoding.Default, true)
                try
                    read readHeader readData r
                finally
                    r.Close()
            
            let inline fromBuffer (readHeader: Binseq<'header>) (readData: 'header -> Binseq<'data>) (buffer: byte array) =
                use ms = new MemoryStream(buffer)
                ms.Seek(0, SeekOrigin.Begin) |> ignore
                readStream readHeader readData ms
        
        [<RequireQualifiedAccess>]
        module Raw =
            let inline write (writer: BinaryWriter) (continueWith: BinaryWriter -> 'b) (writeData: Binseq<unit>)  =
                try
                    match writeData (Writer writer) with
                    | Error e,_ -> Error e
                    | _ ->
                        writer.Flush()
                        Ok (continueWith writer)
                with ex -> Error $"An exeption was thrown while trying to binary write a value.{Environment.NewLine}{ex}"
            
            let inline writeStream (stream: Stream) (continueWith: BinaryWriter -> 'b) (writeData: Binseq<unit>)  =
                use w = new BinaryWriter(stream, Text.Encoding.Default, true)
                try
                    write w continueWith writeData
                finally
                    w.Close()
            
            let inline toBuffer writeData =
                use ms = new MemoryStream()
                writeStream ms (fun _ -> ms.ToArray()) writeData

            let inline read (readData: Binseq<'data>) (reader: BinaryReader) =
                try
                    match readData (Reader reader) with
                    | Error e,_ -> Error e
                    | Ok data,_ -> Ok data
                with ex -> Error $"An exeption was thrown while trying to binary read a value.{Environment.NewLine}{ex}"

            let inline readStream (readData: Binseq<'data>) (stream: Stream) =
                use r = new BinaryReader(stream, Text.Encoding.Default, true)
                try
                    read readData r
                finally
                    r.Close()

            let inline fromBuffer (readData: Binseq<'data>) (buffer: byte array) =
                use ms = new MemoryStream(buffer)
                ms.Seek(0, SeekOrigin.Begin) |> ignore
                readStream readData ms