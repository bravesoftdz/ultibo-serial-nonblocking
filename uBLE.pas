unit uBLE;
{$mode objfpc}{$H+}
//{$define show_data}

interface

uses 
Classes, SysUtils;

const 
 STANDBY_STATE               = 1;
 ADVERTISING_STATE           = 2;
 SCANNING_STATE              = 3;
 INITIATING_STATE            = 4;
 CONNECTION_STATE            = 5;

 // Markers
 INITIAL_SETUP_DONE          = 1;

 // HCI Funcional Specification Section 7.8.10, Core 4.1 page 1255
 LL_SCAN_PASSIVE            = $00;
 LL_SCAN_ACTIVE            = $01;

 //  BLUETOOTH SPECIFICATION Version 4.2 [Vol 2, Part E] page 970
 // advertising type
 ADV_IND                     = $00; // Connectable undirected advertising (default)
 ADV_DIRECT_IND_HI           = $01; // Connectable high duty cycle directed advertising
 ADV_SCAN_IND                = $02; // Scannable undirected advertising
 ADV_NONCONN_IND             = $03; // Non connectable undirected advertising
 ADV_DIRECT_IND_LO           = $04; // Connectable low duty cycle directed advertising
 // $05 – $FF Reserved for future use

 // own address type
 LL_ADR_PUBLIC               = $00; // Public Device Address (default)
 ll_ADR_RANDOM               = $01; // Random Device Address
 LL_ADR_PRIVATE_PUBLIC       = $02; // Controller generates Resolvable Private Address based on the local
 // IRK from resolving list. If resolving list contains no matching entry,
 // use public address.
 LL_ADR_PRIVATE_RANDOM       = $03; // Controller generates Resolvable Private Address based on the local
 // IRK from resolving list. If resolving list contains no matching entry,
 // use random address from LE_Set_Random_Address.
 // $04 – $FF Reserved for future use
 // peer address type
 LL_PEER_PUBLI               = $00; // Public Device Address (default) or Public Identity Address
 LL_PEER_RANDOM              = $01; // Random Device Address or Random (static) Identity Address
 // $02 – $FF Reserved for future use
  (*
  Value Parameter Description
  0xXXXXXXXXXXXX Public Device Address, Random Device Address, Public Identity
  Address, or Random (static) Identity Address of the device to be
  connected        *)

 LL_CHAN37                   = $01;
 LL_CHAN38                   = $02;
 LL_CHAN39                   = $04;
 LL_ALL_CHANS                = LL_CHAN37 or LL_CHAN38 or LL_CHAN39;

 // Advertising Data Types
 ADT_FLAGS                   = $01;      // Flags
 ADT_INCOMPLETE_UUID16       = $02;      // Incomplete List of 16-bit Service Class UUIDs
 ADT_COMPLETE_UUID16         = $03;      // Complete List of 16-bit Service Class UUIDs
 ADT_INCOMPLETE_UUID32       = $04;      // Incomplete List of 32-bit Service Class UUIDs
 ADT_COMPLETE_UUID32         = $05;      // Complete List of 32-bit Service Class UUIDs
 ADT_INCOMPLETE_UUID128      = $06;      // Incomplete List of 128-bit Service Class UUIDs
 ADT_COMPLETE_UUDI128        = $07;      // Complete List of 128-bit Service Class UUIDs
 ADT_SHORTENED_LOCAL_NAME    = $08;      // Shortened Local name
 ADT_COMPLETE_LOCAL_NAME     = $09;      // Complete Local name


 ADT_POWER_LEVEL             = $0A;      // Tx Power Level
 ADT_DEVICE_CLASS            = $0D;      // Class of Device


 ADT_MANUFACTURER_SPECIFIC   = $FF;































































(*
  0x01 	«Flags» 	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.3 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.3 and 18.1 (v4.0)Core Specification Supplement, Part A, section 1.3
0x02 	«Incomplete List of 16-bit Service Class UUIDs» 	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.1 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.1 and 18.2 (v4.0)Core Specification Supplement, Part A, section 1.1
0x03 	«Complete List of 16-bit Service Class UUIDs» 	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.1 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.1 and 18.2 (v4.0)Core Specification Supplement, Part A, section 1.1
0x04 	«Incomplete List of 32-bit Service Class UUIDs» 	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.1 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, section 18.2 (v4.0)Core Specification Supplement, Part A, section 1.1
0x05 	«Complete List of 32-bit Service Class UUIDs» 	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.1 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, section 18.2 (v4.0)Core Specification Supplement, Part A, section 1.1
0x06 	«Incomplete List of 128-bit Service Class UUIDs» 	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.1 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.1 and 18.2 (v4.0)Core Specification Supplement, Part A, section 1.1
0x07 	«Complete List of 128-bit Service Class UUIDs» 	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.1 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.1 and 18.2 (v4.0)Core Specification Supplement, Part A, section 1.1
0x08 	«Shortened Local Name» 	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.2 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.2 and 18.4 (v4.0)Core Specification Supplement, Part A, section 1.2
0x09 	«Complete Local Name» 	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.2 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.2 and 18.4 (v4.0)Core Specification Supplement, Part A, section 1.2
0x0A 	«Tx Power Level» 	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.5 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.5 and 18.3 (v4.0)Core Specification Supplement, Part A, section 1.5
0x0D 	«Class of Device» 	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.6 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.5 and 18.5 (v4.0)Core Specification Supplement, Part A, section 1.6
0x0E 	«Simple Pairing Hash C» 	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.6 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.5 and 18.5 (v4.0)
0x0E 	«Simple Pairing Hash C-192» 	Core Specification Supplement, Part A, section 1.6
0x0F 	«Simple Pairing Randomizer R» 	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.6 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.5 and 18.5 (v4.0)
0x0F 	«Simple Pairing Randomizer R-192» 	Core Specification Supplement, Part A, section 1.6
0x10 	«Device ID» 	Device ID Profile v1.3 or later
0x10 	«Security Manager TK Value» 	Bluetooth Core Specification:Vol. 3, Part C, sections 11.1.7 and 18.6 (v4.0)Core Specification Supplement, Part A, section 1.8
0x11 	«Security Manager Out of Band Flags» 	Bluetooth Core Specification:Vol. 3, Part C, sections 11.1.6 and 18.7 (v4.0)Core Specification Supplement, Part A, section 1.7
0x12 	«Slave Connection Interval Range» 	Bluetooth Core Specification:Vol. 3, Part C, sections 11.1.8 and 18.8 (v4.0)Core Specification Supplement, Part A, section 1.9
0x14 	«List of 16-bit Service Solicitation UUIDs» 	Bluetooth Core Specification:Vol. 3, Part C, sections 11.1.9 and 18.9 (v4.0)Core Specification Supplement, Part A, section 1.10
0x1F 	«List of 32-bit Service Solicitation UUIDs» 	Core Specification Supplement, Part A, section 1.10
0x15 	«List of 128-bit Service Solicitation UUIDs» 	Bluetooth Core Specification:Vol. 3, Part C, sections 11.1.9 and 18.9 (v4.0)Core Specification Supplement, Part A, section 1.10
0x16 	«Service Data» 	Bluetooth Core Specification:Vol. 3, Part C, sections 11.1.10 and 18.10 (v4.0)
0x16 	«Service Data - 16-bit UUID» 	Core Specification Supplement, Part A, section 1.11
0x20 	«Service Data - 32-bit UUID» 	Core Specification Supplement, Part A, section 1.11
0x21 	«Service Data - 128-bit UUID» 	Core Specification Supplement, Part A, section 1.11
0x22 	«LE Secure Connections Confirmation Value» 	Core Specification Supplement Part A, Section 1.6
0x23 	«LE Secure Connections Random Value» 	Core Specification Supplement Part A, Section 1.6
0x24 	«URI» 	Bluetooth Core Specification:Core Specification Supplement, Part A, section 1.18
0x25 	«Indoor Positioning» 	Indoor Posiioning Service v1.0 or later
0x26 	«Transport Discovery Data» 	Transport Discovery Service v1.0 or later
0x17 	«Public Target Address» 	Bluetooth Core Specification:Core Specification Supplement, Part A, section 1.13
0x18 	«Random Target Address» 	Bluetooth Core Specification:Core Specification Supplement, Part A, section 1.14
0x19 	«Appearance» 	Bluetooth Core Specification:Core Specification Supplement, Part A, section 1.12
0x1A 	«Advertising Interval» 	Bluetooth Core Specification:Core Specification Supplement, Part A, section 1.15
0x1B 	«LE Bluetooth Device Address» 	Core Specification Supplement, Part A, section 1.16
0x1C 	«LE Role» 	Core Specification Supplement, Part A, section 1.17
0x1D 	«Simple Pairing Hash C-256» 	Core Specification Supplement, Part A, section 1.6
0x1E 	«Simple Pairing Randomizer R-256» 	Core Specification Supplement, Part A, section 1.6
0x3D 	«3D Information Data» 	3D Synchronization Profile, v1.0 or later
0xFF 	«Manufacturer Specific Data» 	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.4 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.4 and 18.11 (v4.0)Core Specification Supplement, Part A, section 1.4
0x27 	«LE Supported Features» 	Core Specification Supplement, Part A, Section 1.19
0x28 	«Channel Map Update Indication» 	Core Specification Supplement, Part A, Section 1.20


0x01	Flags	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.3 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.3 and 18.1 (v4.0)Core Specification Supplement, Part A, section 1.3
0x02	Incomplete List of 16-bit Service Class UUIDs	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.1 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.1 and 18.2 (v4.0)Core Specification Supplement, Part A, section 1.1
0x03	Complete List of 16-bit Service Class UUIDs	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.1 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.1 and 18.2 (v4.0)Core Specification Supplement, Part A, section 1.1
0x04	Incomplete List of 32-bit Service Class UUIDs	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.1 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, section 18.2 (v4.0)Core Specification Supplement, Part A, section 1.1
0x05	Complete List of 32-bit Service Class UUIDs	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.1 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, section 18.2 (v4.0)Core Specification Supplement, Part A, section 1.1
0x06	Incomplete List of 128-bit Service Class UUIDs	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.1 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.1 and 18.2 (v4.0)Core Specification Supplement, Part A, section 1.1
0x07	Complete List of 128-bit Service Class UUIDs	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.1 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.1 and 18.2 (v4.0)Core Specification Supplement, Part A, section 1.1
0x08	Shortened Local Name	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.2 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.2 and 18.4 (v4.0)Core Specification Supplement, Part A, section 1.2
0x09	Complete Local Name	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.2 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.2 and 18.4 (v4.0)Core Specification Supplement, Part A, section 1.2
0x0A	Tx Power Level	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.5 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.5 and 18.3 (v4.0)Core Specification Supplement, Part A, section 1.5
0x0D	Class of Device	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.6 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.5 and 18.5 (v4.0)Core Specification Supplement, Part A, section 1.6
0x0E	Simple Pairing Hash C	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.6 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.5 and 18.5 (v4.0)
0x0E	Simple Pairing Hash C-192	Core Specification Supplement, Part A, section 1.6
0x0F	Simple Pairing Randomizer R	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.6 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.5 and 18.5 (v4.0)
0x0F	Simple Pairing Randomizer R-192	Core Specification Supplement, Part A, section 1.6
0x10	Device ID	Device ID Profile v1.3 or later
0x10	Security Manager TK Value	Bluetooth Core Specification:Vol. 3, Part C, sections 11.1.7 and 18.6 (v4.0)Core Specification Supplement, Part A, section 1.8
0x11	Security Manager Out of Band Flags	Bluetooth Core Specification:Vol. 3, Part C, sections 11.1.6 and 18.7 (v4.0)Core Specification Supplement, Part A, section 1.7
0x12	Slave Connection Interval Range	Bluetooth Core Specification:Vol. 3, Part C, sections 11.1.8 and 18.8 (v4.0)Core Specification Supplement, Part A, section 1.9
0x14	List of 16-bit Service Solicitation UUIDs	Bluetooth Core Specification:Vol. 3, Part C, sections 11.1.9 and 18.9 (v4.0)Core Specification Supplement, Part A, section 1.10
0x15	List of 128-bit Service Solicitation UUIDs	Bluetooth Core Specification:Vol. 3, Part C, sections 11.1.9 and 18.9 (v4.0)Core Specification Supplement, Part A, section 1.10
0x16	Service Data	Bluetooth Core Specification:Vol. 3, Part C, sections 11.1.10 and 18.10 (v4.0)
0x16	Service Data - 16-bit UUID	Core Specification Supplement, Part A, section 1.11
0x17	Public Target Address	Bluetooth Core Specification:Core Specification Supplement, Part A, section 1.13
0x18	Random Target Address	Bluetooth Core Specification:Core Specification Supplement, Part A, section 1.14
0x19	Appearance	Bluetooth Core Specification:Core Specification Supplement, Part A, section 1.12
0x1A	Advertising Interval	Bluetooth Core Specification:Core Specification Supplement, Part A, section 1.15
0x1B	LE Bluetooth Device Address	Core Specification Supplement, Part A, section 1.16
0x1C	LE Role	Core Specification Supplement, Part A, section 1.17
0x1D	Simple Pairing Hash C-256	Core Specification Supplement, Part A, section 1.6
0x1E	Simple Pairing Randomizer R-256	Core Specification Supplement, Part A, section 1.6
0x1F	List of 32-bit Service Solicitation UUIDs	Core Specification Supplement, Part A, section 1.10
0x20	Service Data - 32-bit UUID	Core Specification Supplement, Part A, section 1.11
0x21	Service Data - 128-bit UUID	Core Specification Supplement, Part A, section 1.11
0x22	LE Secure Connections Confirmation Value	Core Specification Supplement Part A, Section 1.6
0x23	LE Secure Connections Random Value	Core Specification Supplement Part A, Section 1.6
0x24	URI	Bluetooth Core Specification:Core Specification Supplement, Part A, section 1.18
0x25	Indoor Positioning	Indoor Posiioning Service v1.0 or later
0x26	Transport Discovery Data	Transport Discovery Service v1.0 or later
0x3D	3D Information Data	3D Synchronization Profile, v1.0 or later
0xFF	Manufacturer Specific Data	Bluetooth Core Specification:Vol. 3, Part C, section 8.1.4 (v2.1 + EDR, 3.0 + HS and 4.0)Vol. 3, Part C, sections 11.1.4 and 18.11 (v4.0)Core Specification Supplement, Part A, section 1.4


*)

 // ADVERTISING_CHANNEL_ADDRESS = $8E89BED6;   // 4.2 pg 38

 // Company IDs
 ID_APPLE                    = $004C;

 //const
 //  ADVERTISING_CHANNEL_ADDRESS : TADV_ADDRESS = ($00, $00, $8E, $89, $BE, $D6);   // 4.2 pg 38

var 
 LLState : integer = STANDBY_STATE; // link layer state
 AdData : array of byte;

procedure InitialSetup;               // page 133 v4.2
procedure StartUndirectedAdvertising; // page 136 v4.2
procedure StopAdvertising;

function AdvertisingTypeToStr (Type_ : byte) : string;

// helper functions
procedure ClearAdvertisingData;
procedure AddAdvertisingData (Type_ : byte); overload;
procedure AddAdvertisingData (Type_ : byte; Data : array of byte); overload;
procedure AddAdvertisingData (Type_ : byte; Data : string); overload;

implementation

uses uHCI, uLog;

procedure InitialSetup;
begin
 NoOP;
 ReadLocalSupportedCommands;
 ReadLocalSupportedFeatures;
 SetLEEventMask ($ff);
 ReadLEBufferSize;
 ReadLESupportedFeatures;
 ReadBDADDR;
 AddMarker (INITIAL_SETUP_DONE);
end;

function AdvertisingTypeToStr (Type_ : byte) : string;
begin
 case Type_ of 
  ADV_IND           : Result:='Connectable undirected advertising (default)';
  ADV_DIRECT_IND_HI : Result:='Connectable high duty cycle directed advertising';
  ADV_SCAN_IND      : Result:='Scannable undirected advertising';
  ADV_NONCONN_IND   : Result:='Non connectable undirected advertising';
  ADV_DIRECT_IND_LO : Result:='Connectable low duty cycle directed advertising';
  else                Result:='Reserved for future use (' + Type_.ToHexString(2) + ')';
 end;
end;

procedure StartUndirectedAdvertising;
begin
 ReadLEAdvertisingChannelTxPower;
 SetLEAdvertisingData (AdData);
 SetLEAdvertisingEnable (true);
 AddMarker (ADVERTISING_STATE);
end;

procedure StopAdvertising;
begin
 SetLEAdvertisingEnable (false);
end;

procedure ClearAdvertisingData;
begin
 SetLength (AdData, 0);
end;

procedure AddAdvertisingData (Type_ : byte);
begin
 AddAdvertisingData (Type_, []);
end;

procedure AddAdvertisingData (Type_ : byte; Data : array of byte);
var 
 Len : byte;
 i : integer;
begin
 Len:=Length (AdData);
 SetLength (AdData, Len + length (Data) + 2);
 AdData[Len]:=Length (Data) + 1;
 AdData[Len + 1]:=Type_;
 for i:=0 to high (Data) do
  AdData[Len + 2 + i]:=Data[i];
end;

procedure AddAdvertisingData (Type_ : byte; Data : string);
var 
 Len : byte;
 i : integer;
begin
 Len:=Length (AdData);
 SetLength (AdData, Len + length (Data) + 2);
 AdData[Len]:=Length (Data) + 1;
 AdData[Len + 1]:=Type_;
 for i:=1 to length (Data) do
  AdData[Len + 1 + i]:=ord (Data[i]);
end;

function UUIDToStr (uuid : array of byte) : string;
begin
 if length (uuid) = 16 then
  Result:=format ('%.2X%.2X%.2X%.2X-%.2X%.2X-%.2X%.2X-%.2X%.2X-%.2X%.2X%.2X%.2X%.2X%.2X',
          [uuid[0], uuid[1], uuid[2], uuid[3], uuid[4], uuid[5], uuid[6], uuid[7],
          uuid[8], uuid[9], uuid[10], uuid[11], uuid[12], uuid[13], uuid[14], uuid[15]])
 else
  Result:='';
end;

procedure LeCreateConnection(A,B,C,D,E,F:Byte);
begin
 Log('create connection');
 AddHCICommand (OGF_LE_CONTROL, $0d, [100,0, 100,0, 0,1, A,B,C,D,E,F, 1, 100,0,150,0, 50,0, $80,$0C, 0,0,0,8])
end;

procedure DoLEEvent (SubEvent : byte; Params : array of byte);
const 
 ReportHeaderLength = 9;
var 
 ConnectionHandle:Word;
{$ifdef show_data}
 s : string;
{$endif}
begin
{$ifdef show_data}
 s:='';
 for i:=low (Params) to high (Params) do
  s:=s + ' ' + Params[i].ToHexString (2);
 Log ('LEEvent ' + SubEvent.ToHexString (2) + ' Params ' + s);
{$endif}
 case SubEvent of 
  $01 :
       begin
        ConnectionHandle:=Params[1] + Params[2]*256;
        Log (Format('%02.2x) connected',[ConnectionHandle]));
        //      L:=6;
        //      SendData([HCI_ACLDATA_PKT,lo(ConnectionHandle),hi(ConnectionHandle),4+L+1,0,L+1,0,4,0,$8,1,0,$ff,$ff,$03,$28]);
       end;
 end;
end;

initialization
ClearAdvertisingData;
SetLEEvent (@DoLEEvent);
end.
