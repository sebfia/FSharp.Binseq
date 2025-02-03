namespace Tests

open FsUnit

type InitMsgUtils() =
    inherit FSharpCustomMessageFormatter()

[<AutoOpen>]
module Types =
    
    open System
    
    type Shape =
    | Circle of radius: int
    | Rectangle of width: int * height: int

    type Book = {
        Id: Guid
        Title: string
        Author: string
    }

    type Bookshelf =
        | Empty
        | Filled of favorite:Book option*books: Book array

    type Bookstore = {
        Shape: Shape
        Bookshelf: Bookshelf
        FeaturedBook: Book
    }

module Encoder =
    open FSharp.Binseq
    open System

    let inline ofHeader recType (length: int64) =
        Encode.byte (Convert.ToByte length) *> Encode.byte recType

    let inline ofShape x =
        match x with
        | Circle r -> 
            Encode.byte 1uy 
            *> Encode.int r
        | Rectangle (w,h) -> 
            Encode.byte 2uy 
            *> Encode.int w 
            *> Encode.int h

    let inline ofBook (x: Book) =
        Encode.guid x.Id
        *> Encode.string x.Title
        *> Encode.string x.Author

    let ofBookShelf (x: Bookshelf) =
        match x with
        | Empty -> Encode.byte 0uy
        | Filled (fave,books) -> 
            Encode.byte 1uy
            *> Encode.optionOf ofBook fave
            *> Encode.arrayOf ofBook books

    let ofBookstore (x: Bookstore) =
        ofShape x.Shape
        *> ofBookShelf x.Bookshelf
        *> ofBook x.FeaturedBook

module Decoder =
    open FSharp.Binseq
    open System

    let header = binseq {
        let! length = Decode.byte
        let! recType = Decode.byte
        return ((Convert.ToInt32 length), recType)
    }

    let ofShape = binseq {
        match! Decode.byte  with
        | 1uy -> 
            let! r = Decode.int
            return Circle r
        | 2uy ->
            let! w = Decode.int
            let! h = Decode.int
            return Rectangle(w,h)
        | _ -> return! Decode.error "Invalid data for type Shape!"
    }

    let ofBook = binseq {
        let! id = Decode.guid
        let! title = Decode.string
        let! author = Decode.string
        return { Id = id; Title = title; Author = author }
    }

    let ofBookShelf = binseq {
        match! Decode.byte with
        | 0uy -> return Empty
        | 1uy -> 
            let! fave = Decode.optionOf ofBook
            let! books = Decode.arrayOf ofBook
            return Filled (fave,books)
        | _ -> return! Decode.error "Invalid data for Bookshelf!"
    }

    let ofBookstore = binseq {
        let! shape = ofShape
        let! shelf = ofBookShelf
        let! book = ofBook
        return { Shape = shape; Bookshelf = shelf; FeaturedBook = book }
    }

module ``Simple Encoder and Decoder`` =
    open System
    open System.IO
    open NUnit.Framework
    open FSharp.Binseq

    [<Test>]
    let ``Encoding and Decoding Boolean`` () =
        let buffer = (Encode.bool >> Raw.toBuffer) false |> Result.defaultWith (fun e -> failwith e)
        (Raw.fromBuffer Decode.bool) buffer |> Result.defaultWith (fun e -> failwith e) |> should equal false

    [<Test>]
    let ``Encoding and Decoding Int16`` () =
        let buffer = (Encode.int16 >> Raw.toBuffer) 42s |> Result.defaultWith (fun e -> failwith e)
        (Raw.fromBuffer Decode.int16) buffer |> Result.defaultWith (fun e -> failwith e) |> should equal 42s

    [<Test>]
    let ``Encoding and Decoding Int32`` () =
        let buffer = (Encode.int >> Raw.toBuffer) 142 |> Result.defaultWith (fun e -> failwith e)
        (Raw.fromBuffer Decode.int) buffer |> Result.defaultWith (fun e -> failwith e) |> should equal 142

    [<Test>]
    let ``Encoding and Decoding Int64`` () =
        let buffer = (Encode.int64 >> Raw.toBuffer) 4242L |> Result.defaultWith (fun e -> failwith e)
        (Raw.fromBuffer Decode.int64) buffer |> Result.defaultWith (fun e -> failwith e) |> should equal 4242L

    [<Test>]
    let ``Encoding and Decoding Float`` () =
        let buffer = (Encode.float >> Raw.toBuffer) 42.42 |> Result.defaultWith (fun e -> failwith e)
        (Raw.fromBuffer Decode.float) buffer |> Result.defaultWith (fun e -> failwith e) |> should equal 42.42

    [<Test>]
    let ``Encoding and Decoding Single`` () =
        let buffer = (Encode.single >> Raw.toBuffer) 4.2f |> Result.defaultWith (fun e -> failwith e)
        (Raw.fromBuffer Decode.single) buffer |> Result.defaultWith (fun e -> failwith e) |> should equal 4.2f

    [<Test>]
    let ``Encoding and Decoding Decimal`` () =
        let buffer = (Encode.decimal >> Raw.toBuffer) 4242.4242m |> Result.defaultWith (fun e -> failwith e)
        (Raw.fromBuffer Decode.decimal) buffer |> Result.defaultWith (fun e -> failwith e) |> should equal 4242.4242m

    [<Test>]
    let ``Encoding and Decoding Byte`` () =
        let buffer = (Encode.byte >> Raw.toBuffer) 42uy |> Result.defaultWith (fun e -> failwith e)
        (Raw.fromBuffer Decode.byte) buffer |> Result.defaultWith (fun e -> failwith e) |> should equal 42uy

    [<Test>]
    let ``Encoding and Decoding String`` () =
        let expected = "I am the number 42"
        let buffer = (Encode.string >> Raw.toBuffer) expected |> Result.defaultWith (fun e -> failwith e)
        (Raw.fromBuffer Decode.string) buffer |> Result.defaultWith (fun e -> failwith e) |> should equal expected

    [<Test>]
    let ``Encoding and Decoding Of Empty String`` () =
        let expected = String.Empty
        let buffer = (Encode.string >> Raw.toBuffer) expected |> Result.defaultWith (fun e -> failwith e)
        (Raw.fromBuffer Decode.string) buffer |> Result.defaultWith (fun e -> failwith e) |> should equal expected

    [<Test>]
    let ``Encoding and Decoding Of Fixed Length String`` () =
        let expected = "I am number 42"
        let buffer = (Encode.fixedLengthString 22 >> Raw.toBuffer) expected |> Result.defaultWith (fun e -> failwith e)
        (Decode.fixedLengthString 22 |> Raw.fromBuffer) buffer |> Result.defaultWith (fun e -> failwith e) |> should equal expected

    [<Test>]
    let ``Encoding and Decoding Bytes`` () =
        let expected = "I am the number 42" |> Text.Encoding.UTF8.GetBytes
        let buffer = (Encode.bytes >> Raw.toBuffer) expected |> Result.defaultWith (fun e -> failwith e)
        (Raw.fromBuffer Decode.bytes) buffer |> Result.defaultWith (fun e -> failwith e) |> should equal expected

    [<Test>]
    let ``Encoding and Decoding Guid`` () =
        let expected = Guid.NewGuid()
        let buffer = (Encode.guid >> Raw.toBuffer) expected |> Result.defaultWith (fun e -> failwith e)
        (Raw.fromBuffer Decode.guid) buffer |> Result.defaultWith (fun e -> failwith e) |> should equal expected

    [<Test>]
    let ``Encoding and Decoding DateTimeOffset`` () =
        let expected = DateTimeOffset.Now
        let buffer = (Encode.dateTimeOffset >> Raw.toBuffer) expected |> Result.defaultWith (fun e -> failwith e)
        (Raw.fromBuffer Decode.dateTimeOffset) buffer |> Result.defaultWith (fun e -> failwith e) |> should equal expected
    
    [<Test>]
    let ``Encoding and Decoding DateTimeOffset as Unix Nanos`` () =
        let expected = DateTimeOffset.Now
        let buffer = (Encode.dateTimeOffsetUtcAsUnixNanos >> Raw.toBuffer) expected |> Result.defaultWith (fun e -> failwith e)
        (Raw.fromBuffer Decode.dateTimeOffsetUtcAsUnixNanos) buffer |> Result.defaultWith (fun e -> failwith e) |> should equal (expected.ToUniversalTime())

    [<Test>]
    let ``Try Encoding and Decoding Out Of Bounds Up DateTimeOffset as Unix Nanos`` () =
        let buffer = (Encode.int64 >> Raw.toBuffer) Int64.MaxValue |> Result.defaultWith (fun e -> failwith e)
        (Raw.fromBuffer Decode.tryDateTimeOffsetUtcAsUnixNanos) buffer |> Result.defaultWith (fun e -> failwith e) |> should equal None

    [<Test>]
    let ``Try Encoding and Decoding Out Of Bounds Lower DateTimeOffset as Unix Nanos`` () =
        let buffer = (Encode.int64 >> Raw.toBuffer) 0L |> Result.defaultWith (fun e -> failwith e)
        (Raw.fromBuffer Decode.tryDateTimeOffsetUtcAsUnixNanos) buffer |> Result.defaultWith (fun e -> failwith e) |> should equal None

    [<Test>]
    let ``Try Encoding and Decoding Valid DateTimeOffset as Unix Nanos`` () =
        let expected = DateTimeOffset.Now
        let buffer = (Encode.dateTimeOffsetUtcAsUnixNanos >> Raw.toBuffer) expected |> Result.defaultWith (fun e -> failwith e)
        (Raw.fromBuffer Decode.tryDateTimeOffsetUtcAsUnixNanos) buffer |> Result.defaultWith (fun e -> failwith e) |> should equal (expected.ToUniversalTime() |> Some)


    [<Test>]
    let ``Encoding and Decoding DateTime`` () =
        let expected = DateTime.Now
        let buffer = (Encode.dateTime >> Raw.toBuffer) expected |> Result.defaultWith (fun e -> failwith e)
        (Raw.fromBuffer Decode.dateTime) buffer |> Result.defaultWith (fun e -> failwith e) |> should equal expected

    [<Test>]
    let ``Encoding and Decoding DateTime as Unix Nanos`` () =
        let expected = DateTime.Now
        let buffer = (Encode.dateTimeUtcAsUnixNanos >> Raw.toBuffer) expected |> Result.defaultWith (fun e -> failwith e)
        (Raw.fromBuffer Decode.dateTimeUtcAsUnixNanos) buffer |> Result.defaultWith (fun e -> failwith e) |> should equal (expected.ToUniversalTime())

    [<Test>]
    let ``Try Encoding and Decoding Out Of Bounds Up DateTime as Unix Nanos`` () =
        let buffer = (Encode.int64 >> Raw.toBuffer) Int64.MaxValue |> Result.defaultWith (fun e -> failwith e)
        (Raw.fromBuffer Decode.tryDateTimeUtcAsUnixNanos) buffer |> Result.defaultWith (fun e -> failwith e) |> should equal None

    [<Test>]
    let ``Try Encoding and Decoding Out Of Bounds Lower DateTime as Unix Nanos`` () =
        let buffer = (Encode.int64 >> Raw.toBuffer) 0L |> Result.defaultWith (fun e -> failwith e)
        (Raw.fromBuffer Decode.tryDateTimeUtcAsUnixNanos) buffer |> Result.defaultWith (fun e -> failwith e) |> should equal None

    [<Test>]
    let ``Try Encoding and Decoding Valid DateTime as Unix Nanos`` () =
        let expected = DateTime.Now
        let buffer = (Encode.dateTimeUtcAsUnixNanos >> Raw.toBuffer) expected |> Result.defaultWith (fun e -> failwith e)
        (Raw.fromBuffer Decode.tryDateTimeUtcAsUnixNanos) buffer |> Result.defaultWith (fun e -> failwith e) |> should equal (expected.ToUniversalTime() |> Some)
    
    [<Test>]
    let ``Try Encoding and Decoding DateTime as Unix Nanos`` () =
        let expected = DateTime.Now
        let buffer = (Encode.dateTimeUtcAsUnixNanos >> Raw.toBuffer) expected |> Result.defaultWith (fun e -> failwith e)
        (Raw.fromBuffer Decode.dateTimeUtcAsUnixNanos) buffer |> Result.defaultWith (fun e -> failwith e) |> should equal (expected.ToUniversalTime())

    [<Test>]
    let ``Encoding and Decoding TimeSpan`` () =
        let expected = TimeSpan.FromSeconds(42.)
        let buffer = (Encode.timeSpan >> Raw.toBuffer) expected |> Result.defaultWith (fun e -> failwith e)
        (Raw.fromBuffer Decode.timeSpan) buffer |> Result.defaultWith (fun e -> failwith e) |> should equal expected

    [<Test>]
    let ``Append Padding`` () =
        let expectedLength = 56
        let buffer = (Encode.appendPadding >> Raw.toBuffer) expectedLength|> Result.defaultWith (fun e -> failwith e)
        buffer.Length |> should equal expectedLength // for length of header

    [<Test>]
    let ``Encoding and Decoding of String Array`` () =
        let expected = [| "I am the number 42"; "I am the number 43"; "I am the number 44" |]
        let buffer = (Encode.arrayOf (Encode.string) >> Raw.toBuffer) expected|> Result.defaultWith (fun e -> failwith e)
        (Decode.arrayOf Decode.string |> Raw.fromBuffer) buffer |> Result.defaultWith (fun e -> failwith e) |> should equal expected

    [<Test>]
    let ``Encoding and Decoding of Fixed Length String Array`` () =
        let expected = [| "I am the number 42"; "I am the number 43"; "I am the number 44" |]
        let buffer = (Encode.arrayOf (Encode.fixedLengthString 22) >> Raw.toBuffer) expected|> Result.defaultWith (fun e -> failwith e)
        let decoded = (Decode.fixedLengthString 22 |> Decode.arrayOf |> Raw.fromBuffer) buffer |> Result.defaultWith (fun e -> failwith e)
        decoded.Length |> should equal expected.Length
        decoded[0] |> should equal expected.[0]
        decoded[1] |> should equal expected.[1]
        decoded[2] |> should equal expected.[2]
        decoded |> should equal expected

    [<Test>]
    let ``Encoding and Decoding of Option with Some Value`` () =
        let expected = Some "I am the number 42"
        let buffer = (Encode.optionOf Encode.string >> Raw.toBuffer) expected|> Result.defaultWith (fun e -> failwith e)
        let decoded = (Decode.optionOf Decode.string |> Raw.fromBuffer) buffer |> Result.defaultWith (fun e -> failwith e)
        decoded |> should equal expected

    [<Test>]
    let ``Encoding and Decoding of Option with None Value`` () =
        let expected = None
        let buffer = (Encode.optionOf Encode.string >> Raw.toBuffer) expected|> Result.defaultWith (fun e -> failwith e)
        let decoded = (Decode.optionOf Decode.string |> Raw.fromBuffer) buffer |> Result.defaultWith (fun e -> failwith e)
        decoded |> should equal expected

    [<Test>]
    let ``Encoding and Decoding of Result with Ok Value`` () =
        let expected : Result<int64,string> = Ok 42L
        let buffer = (Encode.resultOf Encode.int64 Encode.string >> Raw.toBuffer) expected|> Result.defaultWith (fun e -> failwith e)
        let decoded = (Decode.resultOf Decode.int64 Decode.string |> Raw.fromBuffer) buffer |> Result.defaultWith (fun e -> failwith e)
        decoded |> should equal expected

    [<Test>]
    let ``Encoding and Decoding of Result with Error Value`` () =
        let expected : Result<int64,string> = Error "This is a clear error!"
        let buffer = (Encode.resultOf Encode.int64 Encode.string >> Raw.toBuffer) expected|> Result.defaultWith (fun e -> failwith e)
        let decoded = (Decode.resultOf Decode.int64 Decode.string |> Raw.fromBuffer) buffer |> Result.defaultWith (fun e -> failwith e)
        decoded |> should equal expected

module ``Back And Forth With More Complex Types`` =
    open System
    open NUnit.Framework
    open FSharp.Binseq

    [<Test>]
    let ``Encoding and Decoding Circle Shape`` () =
        let expected = Circle 2
        let buffer = (Encoder.ofShape >> Raw.toBuffer) expected |> Result.defaultWith (fun e -> failwith e)
        (Raw.fromBuffer Decoder.ofShape) buffer |> Result.defaultWith (fun e -> failwith e) |> should equal expected
    
    [<Test>]
    let ``Encoding And Decoding Rectangle Shape`` () =
        let expected = Rectangle(2,4)
        let buffer = (Encoder.ofShape >> Raw.toBuffer) expected |> Result.defaultWith (fun e -> failwith e)
        (Raw.fromBuffer Decoder.ofShape) buffer |> Result.defaultWith (fun e -> failwith e) |> should equal expected

    [<Test>]
    let ``Encoding And Decoding Of Book Record`` () =
        let expected = { Id = Guid.NewGuid(); Title = "Don Quixote"; Author = "Miguel de Cervantes"}
        let buffer = (Encoder.ofBook >> Raw.toBuffer) expected |> Result.defaultWith (fun e -> failwith e)
        (Raw.fromBuffer Decoder.ofBook) buffer |> Result.defaultWith (fun e -> failwith e) |> should equal expected

    [<Test>]
    let ``Encoding And Decoding Of Book Record Array`` () =
        let expected = [|
            { Id = Guid.NewGuid(); Title = "Lord of the Rings"; Author = "J.R.R. Tolkien"}
            { Id = Guid.NewGuid(); Title = "Harry Potter and the Sorcerer's Stone"; Author = "J.K. Rowling"}
            { Id = Guid.NewGuid(); Title = "And Then There Were None"; Author = "Agatha Christie"}
            { Id = Guid.NewGuid(); Title = "Alice's Adventures in Wonderland"; Author = "Lewis Carroll"}
            { Id = Guid.NewGuid(); Title = "The Lion, the Witch, and the Wardrobe"; Author = "C.S. Lewis"}
        |]
        let buffer = (Encode.arrayOf Encoder.ofBook >> Raw.toBuffer) expected |> Result.defaultWith (fun e -> failwith e)
        let actual = (Decode.arrayOf Decoder.ofBook |> Raw.fromBuffer) buffer |> Result.defaultWith (fun e -> failwith e)
        expected |> should equal actual

    [<Test>]
    let ``Encoding And Decoding Of Bookshelf with Books`` () =
        let expected = Filled (
            Some { Id = Guid.NewGuid(); Title = "Lord of the Rings"; Author = "J.R.R. Tolkien"},
            [|
                { Id = Guid.NewGuid(); Title = "Lord of the Rings"; Author = "J.R.R. Tolkien"}
                { Id = Guid.NewGuid(); Title = "Harry Potter and the Sorcerer's Stone"; Author = "J.K. Rowling"}
                { Id = Guid.NewGuid(); Title = "And Then There Were None"; Author = "Agatha Christie"}
                { Id = Guid.NewGuid(); Title = "Alice's Adventures in Wonderland"; Author = "Lewis Carroll"}
                { Id = Guid.NewGuid(); Title = "The Lion, the Witch, and the Wardrobe"; Author = "C.S. Lewis"}
            |])

        let buffer = (Encoder.ofBookShelf >> Raw.toBuffer) expected |> Result.defaultWith (fun e -> failwith e)
        let actual = (Raw.fromBuffer Decoder.ofBookShelf) buffer |> Result.defaultWith (fun e -> failwith e)
        expected |> should equal actual

    [<Test>]
    let ``Encoding And Decoding Of Bookstore`` () =
        let shelf = Filled (
            Some { Id = Guid.NewGuid(); Title = "Lord of the Rings"; Author = "J.R.R. Tolkien"},
            [|
                { Id = Guid.NewGuid(); Title = "Lord of the Rings"; Author = "J.R.R. Tolkien"}
                { Id = Guid.NewGuid(); Title = "Harry Potter and the Sorcerer's Stone"; Author = "J.K. Rowling"}
                { Id = Guid.NewGuid(); Title = "And Then There Were None"; Author = "Agatha Christie"}
                { Id = Guid.NewGuid(); Title = "Alice's Adventures in Wonderland"; Author = "Lewis Carroll"}
                { Id = Guid.NewGuid(); Title = "The Lion, the Witch, and the Wardrobe"; Author = "C.S. Lewis"}
            |])
        let expected = {
            Shape = Rectangle (10, 20)
            Bookshelf = shelf
            FeaturedBook = { Id = Guid.NewGuid(); Title = "The Lion, the Witch, and the Wardrobe"; Author = "C.S. Lewis"}
        }

        let buffer = (Encoder.ofBookstore >> Raw.toBuffer) expected |> Result.defaultWith (fun e -> failwith e)
        let actual = (Raw.fromBuffer Decoder.ofBookstore) buffer |> Result.defaultWith (fun e -> failwith e)
        expected |> should equal actual

module ``Using the Record Module for Prepending Headers`` =
    open System
    open NUnit.Framework
    open FSharp.Binseq

    type RecordType =
        | Book = 1uy
        | Bookshelf = 2uy

    module Header =

        let decode = binseq {
            let! length = Decode.int64
            let! recType = Decode.byte ?> LanguagePrimitives.EnumOfValue<byte, RecordType>
            return recType, length
        }

        let encode (recType: RecordType) length = Encode.int64 length *> Encode.byte (recType |> LanguagePrimitives.EnumToValue)

    let decodeBook header = binseq {
        let! book = Decoder.ofBook
        return header,book
    }

    [<Test>]
    let ``Create a header for a simple record`` () =
        let expected = { Id = Guid.NewGuid(); Title = "Lord of the Rings"; Author = "J.R.R. Tolkien"}
        let buffer = (Encoder.ofBook >> Record.toBuffer (Header.encode RecordType.Book)) expected |> Result.defaultWith (fun e -> failwith e)
        let compareBuffer = (Encoder.ofBook >> Raw.toBuffer) expected |> Result.defaultWith (fun e -> failwith e)
        let (recType,length),book = (Record.fromBuffer Header.decode decodeBook) buffer |> Result.defaultWith (fun e -> failwith e)
        book |> should equal expected
        recType |> should equal RecordType.Book
        length |> should equal compareBuffer.Length
        buffer.Length |> should equal (compareBuffer.Length + 9)