# Binseq

**Binseq** is an F# library that supports encoding and decoding of complex data from and to any sequential binary representation and provides a collection of functions to encode, decode and combine binary data in a functional style.  
**Binseq** has additionally built-in functions to encode and decode **arrays** and **FSharp** types like **Option** or **Result<'a,'b>**.  
It does not rely on any additional library or protocol (eg. protobuf).  

## Features

- **Binary Representation**: Convert integers, strings, or custom data types to and from binary sequences.

- **Utility Functions**: Safely handle edge cases (e.g., empty or oversized sequences).

## Installation

1. **Using Paket or NuGet**:

   ```bash
   # For .NET CLI
   dotnet add package Binseq
   # For Paket
   paket add Binseq

## Usage

### Simple Data Types

Below are simple examples showing how to encode and decode simple values using **Binseq**:

```fsharp
open Binseq

// Encoding an decoding a boolean:
let buffer = (Encode.bool >> Raw.toBuffer) false |> Result.defaultWith (fun e -> failwith e)
let b = (Raw.fromBuffer Decode.bool) buffer |> Result.defaultWith (fun e -> failwith e) // b will be false

// Encoding and decoding integers:
let buffer = (Encode.int >> Raw.toBuffer) 142 |> Result.defaultWith (fun e -> failwith e)
let i = (Raw.fromBuffer Decode.int) buffer |> Result.defaultWith (fun e -> failwith e) // will be 142
```

#### Arrays of simple data types

```fsharp
//Encoding and decoding array of simple data types:
let expected = [| "I am the number 42"; "I am the number 43"; "I am the number 44" |]
let buffer = (Encode.arrayOf (Encode.fixedLengthString 22) >> Raw.toBuffer) expected|> Result.defaultWith (fun e -> failwith e)
let decoded = (Decode.fixedLengthString 22 |> Decode.arrayOf |> Raw.fromBuffer) buffer |> Result.defaultWith (fun e -> failwith e) // decoded will match expected.
```

#### Special cases with simple values

Some binary formats require to store strings as **fixed length char arrays** in order to improve compression etc.

```fsharp
// Encoding and decoding fixed-length strings:
let buffer = (Encode.fixedLengthString 22 >> Raw.toBuffer) "I am number 42" |> Result.defaultWith (fun e -> failwith e) //pads with spaces if shorter  
let str = (Decode.fixedLengthString 22 |> Raw.fromBuffer) buffer |> Result.defaultWith (fun e -> failwith e) // will be "I am number 42"
```

#### DateTime and DateTimeOffset Handling

Default DateTime Encoding:  
The standard encoding uses .NET's binary format.  

**Binseq** however, provides multiple ways to encode and decode DateTime and DateTimeOffset values, including Unix epoch-based timestamps with nanosecond precision.

```fsharp
// Encoding and decoding DateTime by default:
let now = DateTime.Now
let buffer = (Encode.dateTime >> Raw.toBuffer) now |> Result.defaultWith (fun e -> failwith e)
let decoded = (Raw.fromBuffer Decode.dateTime) buffer |> Result.defaultWith (fun e -> failwith e) // decoded will match now

// Encoding and decoding DateTimeOffset by default:
let nowOffset = DateTimeOffset.Now
let buffer = (Encode.dateTimeOffset >> Raw.toBuffer) nowOffset |> Result.defaultWith (fun e -> failwith e)
let decoded = (Raw.fromBuffer Decode.dateTimeOffset) buffer |> Result.defaultWith (fun e -> failwith e) // decoded will match nowOffset

// Encoding and decoding Unix epoch-based DateTime with nanosecond precision:
let now = DateTime.UtcNow
let buffer = (Encode.unixTimeNanos >> Raw.toBuffer) now |> Result.defaultWith (fun e -> failwith e)
let decoded = (Raw.fromBuffer Decode.unixTimeNanos) buffer |> Result.defaultWith (fun e -> failwith e) // decoded will match now

// Encoding and decoding Unix epoch-based DateTimeOffset with nanosecond precision:
let nowOffset = DateTimeOffset.UtcNow
let buffer = (Encode.unixTimeOffsetNanos >> Raw.toBuffer) nowOffset |> Result.defaultWith (fun e -> failwith e)
let decoded = (Raw.fromBuffer Decode.unixTimeOffsetNanos) buffer |> Result.defaultWith (fun e -> failwith e) // decoded will match nowOffset
```

#### FSharp Option

```fsharp
// Encoding and decoding Some:
let buffer = (Encode.optionOf Encode.string >> Raw.toBuffer) (Some "I am the number 42") |> Result.defaultWith (fun e -> failwith e)  
let decoded = (Decode.optionOf Decode.string |> Raw.fromBuffer) buffer |> Result.defaultWith (fun e -> failwith e)  
// decoded will be (Some "I am the number 42")

// Encoding and decoding None:
let buffer = (Encode.optionOf Encode.string >> Raw.toBuffer) None|> Result.defaultWith (fun e -> failwith e)
let decoded = (Decode.optionOf Decode.string |> Raw.fromBuffer) buffer |> Result.defaultWith (fun e -> failwith e)
// decoded will be None
```

#### FSharp Result<'a,'b>

```fsharp
// Encoding and decoding an Ok value:
let expected : Result<int64,string> = Ok 42L
let buffer = (Encode.resultOf Encode.int64 Encode.string >> Raw.toBuffer) expected|> Result.defaultWith (fun e -> failwith e)
let decoded = (Decode.resultOf Decode.int64 Decode.string |> Raw.fromBuffer) buffer |> Result.defaultWith (fun e -> failwith e)
// decoded will be (Ok 42L)

// Encoding and decoding an Error:
let expected : Result<int64,string> = Error "This is a clear error!"
let buffer = (Encode.resultOf Encode.int64 Encode.string >> Raw.toBuffer) expected|> Result.defaultWith (fun e -> failwith e)
let decoded = (Decode.resultOf Decode.int64 Decode.string |> Raw.fromBuffer) buffer |> Result.defaultWith (fun e -> failwith e)
// decoded will be (Error "This is a clear error!")
```

### Complex Type Examples

**Binseq** uses a computation expression (`binseq`) for composing decoders, making it easy to work with complex types.  
The `*>` operator combines encoders sequentially.

For more advanced scenarios (e.g., shapes, books, arrays), see Tests.fs.  

#### Here's how to create encoders and decoders for custom types using **Binseq**

```fsharp
// A discriminated union:
type Shape =
    | Circle of radius: int
    | Rectangle of width: int * height: int

let encodeShape = function
    | Circle r -> 
        Encode.byte 1uy 
        *> Encode.int r
    | Rectangle (w,h) -> 
        Encode.byte 2uy 
        *> Encode.int w
        *> Encode.int h

let decodeShape = binseq {
    match! Decode.byte with
    | 1uy -> 
        let! r = Decode.int
        return Circle r
    | 2uy ->
        let! w = Decode.int
        let! h = Decode.int
        return Rectangle(w,h)
    | _ -> return! Decode.error "Invalid shape type!"
}

// A record:
type Book = {
    Id: Guid
    Title: string
    Author: string
}

let encodeBook (book: Book) =
    Encode.guid book.Id
    *> Encode.string book.Title
    *> Encode.string book.Author

let decodeBook = binseq {
    let! id = Decode.guid
    let! title = Decode.string
    let! author = Decode.string
    return { Id = id; Title = title; Author = author }
}

// Handling nested arrays
type Bookshelf = {
    ShelfCode: string
    Books: Book array
}

let encodeBookshelf (shelf: Bookshelf) =
    Encode.string shelf.ShelfCode
    *> (Encode.arrayOf encodeBook) shelf.Books

let decodeBookshelf = binseq {
    let! code = Decode.string
    let! books = Decode.arrayOf decodeBook
    return { ShelfCode = code; Books = books }
}

// Handling nested types and arrays
type Bookstore = {
    Name: string
    Address: string
    Shelves: Bookshelf array
}

let encodeBookstore (store: Bookstore) =
    Encode.string store.Name
    *> Encode.string store.Address
    *> (Encode.arrayOf encodeBookshelf) store.Shelves

let decodeBookstore = binseq {
    let! name = Decode.string
    let! addr = Decode.string
    let! shelves = Decode.arrayOf decodeBookshelf
    return { Name = name; Address = addr; Shelves = shelves }
}
```

#### How to use above encoders and decoders for custom types using **Binseq**

```fsharp
// A book
let book = { 
    Id = Guid.NewGuid()
    Title = "Sample Book"
    Author = "John Doe" 
}

// Encode and decode a book
let buffer = (Encoder.ofBook >> Raw.toBuffer) book |> Result.defaultWith (fun e -> failwith e)
let decoded = (Raw.fromBuffer Decoder.ofBook) buffer |> Result.defaultWith (fun e -> failwith e)

// Encode and decode a shape
let shape = Circle 2
let buffer = (Encoder.ofShape >> Raw.toBuffer) shape |> Result.defaultWith (fun e -> failwith e)
let decodedShape = (Raw.fromBuffer Decoder.ofShape) buffer |> Result.defaultWith (fun e -> failwith e)

//Encoding and decoding nested types
// A bookshelf
let shelf = Filled (
    Some { Id = Guid.NewGuid(); Title = "Lord of the Rings"; Author = "J.R.R. Tolkien"},
    [|
        { Id = Guid.NewGuid(); Title = "Lord of the Rings"; Author = "J.R.R. Tolkien"}
        { Id = Guid.NewGuid(); Title = "Harry Potter and the Sorcerer's Stone"; Author = "J.K. Rowling"}
        { Id = Guid.NewGuid(); Title = "And Then There Were None"; Author = "Agatha Christie"}
        { Id = Guid.NewGuid(); Title = "Alice's Adventures in Wonderland"; Author = "Lewis Carroll"}
        { Id = Guid.NewGuid(); Title = "The Lion, the Witch, and the Wardrobe"; Author = "C.S. Lewis"}
    |])

// and a bookstore with a nested bookshelf
let bookStore = {
    Shape = Rectangle (10, 20)
    Bookshelf = shelf
    FeaturedBook = { Id = Guid.NewGuid(); Title = "The Lion, the Witch, and the Wardrobe"; Author = "C.S. Lewis"}
}

// Encode and decode the bookstore
let buffer = (Encoder.ofBookstore >> Raw.toBuffer) bookStore |> Result.defaultWith (fun e -> failwith e)
let decoded = (Raw.fromBuffer Decoder.ofBookstore) buffer |> Result.defaultWith (fun e -> failwith e)
```

### Using the Record Module

The Record module provides functionality for writing and reading binary data with headers. This is particularly useful when dealing with files or streams that contain multiple records of different types.

Here's how to use it with our complex types:

### **Headers** - Working with Mixed Types in a Single Stream

When writing different types to a single binary sequence like a stream, a way to identify the type of data that follows is needed. This is typically done using a header containing:

1. The **length** of the following data
2. A Type **discriminator** (usually a byte)
3. Optional **metadata** (e.g., timestamp)

**Important**: Headers must have a fixed, predictable size to enable sequential reading. All header fields should use fixed-length types:

- Use **fixed-size** integers (byte, int32, int64)
- Use **fixed-length** strings
- **Avoid variable-length** types in headers

As we are using **Binseq** one would expect to be able to encode the header in the same way the payload is being encoded.  
And that is exactly how this is done.  

As the length of the encoded payload is not known beforehand, the encoder for the header needs a parameter for the length.  

Its signature is therefore: ```int64 -> Binseq<unit>```.  
Similarly data from the header might be necessary to decode the underlying payload. A timestamp that is contained in the metadata of the header might be carried over to the decoded data of the payload.

#### Using the Record Module with Complex Types

Below is a minimal sample of how to combine a header with a typed payload using the Record module:

```fsharp
// Define a discriminator for different types written to the same stream
type RecordType =
    | Book = 1uy
    | Bookshelf = 2uy

// Here we have a separate module for encoding and decoding headers. Simple headers do not necessarily need to have a defined type
module Header =

    // Our header contains only of a record type, that takes one byte and a length that takes 8 bytes -> 9 bytes at all
    let decode = binseq {
        let! length = Decode.int64
            // ...optional metadata... (timestamp, version etc.)
        let! recType = Decode.byte ?> LanguagePrimitives.EnumOfValue<byte, RecordType>
        return recType, length
    }

    // As the length of the succeeding data isn't (always) known beforehand we can leave the task of giving the header-writer this number to the writer of the payload after all data has been written. Currently this requires the underlying stream to be seekable.
    let encode (recType: RecordType) length = 
        Encode.int64 length 
            // ...optional metadata... (timestamp, version etc.)
        *> Encode.byte (recType |> LanguagePrimitives.EnumToValue)

// for convenience we define a function to decode a book from a re

// 2) Write a Bookstore to a stream with Record.writeStream:
let writeBookstoreToStream stream (store: Bookstore) =
    Record.writeStream stream (fun _ -> ())
        (headerEncoder length) // Provide a length if needed
        (Encoder.ofBookstore store)

// 3) Define a header decoder:
let headerDecoder = binseq {
    let! length = Decode.int64
    let! recordType = Decode.byte
    if recordType <> 1uy then
        return! Decode.error "Invalid record type!"
    return length
}

// 4) Read a Bookstore from a stream with Record.readStream:
let readBookstoreFromStream stream =
    Record.readStream headerDecoder (fun _ -> Decoder.ofBookstore) stream
```

## Contributing

- **Issues & Ideas**: Open an issue or submit a pull request.
- **Testing**: Add or update tests in Tests.fs to maintain coverage.
- **Coding Guidelines**: Use idiomatic F# patterns (immutability, pattern matching, etc.).

## License

This project is available under the MIT License.
Feel free to use, modify, and distribute it as permitted.
