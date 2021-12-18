module AdventCalendar2021.Day16

open System

type PacketType = Literal | Operator
type Packet = {Version:int; Type:PacketType; Bits:string seq; Value:int option; SubPackets:Packet[] option}

let convertHexCharToBinaryString char = Convert.ToString(Convert.ToInt32(char, 16), 2).PadLeft(4, '0')

let line = "8A004A801A8002F478"
            |> Seq.map (Char.ToString >> convertHexCharToBinaryString)
            |> Seq.collect id
            |> Seq.map Char.ToString
            
let rec getLiteralBits bits =
    let flagBit = Seq.head bits
    match flagBit with
    | "1" -> (bits |> Seq.skip 1 |> Seq.take 4 |> String.concat String.Empty) + (getLiteralBits (bits |> Seq.skip 5))
    | _ -> bits |> Seq.skip 1 |> Seq.take 4 |> String.concat String.Empty

let rec parsePacket packetBits =
    let versionBits = packetBits |> Seq.take 3 |> String.concat String.Empty
    let version = Convert.ToInt32(versionBits, 2)
    let typeBits = packetBits |> Seq.skip 3 |> Seq.take 3 |> String.concat String.Empty
    let packetType = match Convert.ToInt32(typeBits, 2) with
                     | 4 -> Literal
                     | _ -> Operator
    
    let bits = packetBits |> Seq.skip 6
    if packetType = Literal
    then
        let literalBits = getLiteralBits bits
        let packetLengthWithoutPadding = 3 + 3 + literalBits.Length + (literalBits.Length / 4)
//        let paddingLength = 4 - (packetLengthWithoutPadding % 4)
        let totalPacketLength = packetLengthWithoutPadding //+ paddingLength
        let bitsLeftOver = packetBits |> Seq.skip totalPacketLength
        ({
         Version = version;
         Type = packetType;
         Bits = bits
         Value = Some (Convert.ToInt32(getLiteralBits bits, 2));
         SubPackets = None     
         }, bitsLeftOver)
    else
        let (subPackets, bitsLeftOver) = parseOperatorPacket bits    
        ({
         Version = version;
         Type = packetType;
         Bits = bits
         Value = None;
         SubPackets = subPackets
         }, bitsLeftOver)
and  parseOperatorPacket bits =
    let lengthType = Seq.head bits
    match lengthType with
    | "1" ->
        let number = Convert.ToInt32(bits |> Seq.skip 1 |> Seq.take 11 |> String.concat String.Empty, 2)
        parseOperatorPacketByNumber (bits |> Seq.skip 12) [] number
    | _ ->
        let length = Convert.ToInt32(bits |> Seq.skip 1 |> Seq.take 15 |> String.concat String.Empty, 2)
        let subPackets = parseOperatorPacketByLength (bits |> Seq.skip 16 |> Seq.take length) []
        let bitsLeft = bits |> Seq.skip length
        (subPackets, bitsLeft)
and parseOperatorPacketByNumber bits packets count =
    if count = 0
    then
        (packets |> List.toArray |> Some, bits)
    else
    let (newPacket, bitsLeft) = parsePacket bits    
    parseOperatorPacketByNumber bitsLeft (newPacket::packets) (count-1)
and parseOperatorPacketByLength bits packets =   
    if Seq.length bits = 0
    then
        packets |> Seq.toArray |> Some
    else
        let (newPacket, bitsLeft) = parsePacket bits    
        parseOperatorPacketByLength bitsLeft (newPacket::packets)  
    
    

    
let packetTree =     parsePacket line
           
let part1 = packetTree