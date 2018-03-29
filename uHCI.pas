unit uHCI;
{$mode objfpc}{$H+}
//{$define show_data}

interface

uses 
Classes,SysUtils,Serial,BCM2710,GlobalConst,uLog;
(*

Connection to BCM4343 chip

UART0      115200 BAUD,8 DATA,1 STOP,NO PARITY,NO FLOW CONTROL

PIN 15     SET TO INPUT
PIN 32     SET TO ALT3
PIN 33     SET TO ALT3

OCF        OPCODE GROUP FIELD(HIGHER 6 BITS OF OP CODE)

  OGF_LINK_CONTROL	   0x01 COMMANDS TO CONTROL CONNECTIONS TO OTHER BT DEVICES
  OGF_LINK_POLICY	     0x02
  OGF_HOST_CONTROL 	   0x03 COMMANDS TO ACCESS AND CONTROL HARDWARE
  OGF_INFORMATIONAL	   0x04 COMMANDS THAT PROVIDE INFO ON AND CAPABLILITIES OF HARDWARE
  OGF_STATUS           0x05 COMMANDS THAT ACCESS STATE OF LINK MANAGER
  OGF_LE_CONTROL       0x08 COMMANDS THAT ACCESS THE LOW ENEGY(LE) FEATURES
  OGF_VENDOR           0x3F VENDOR SPECIFIC COMMANDS

  OP CODE =(OGF SHL 10) OR OCF

COMMAND FORMAT

    HCI_COMMAND_PKT    0x01
    LO(OP CODE)
    HI(OP CODE)
    PARAM LENGTH
    PARAMS....
*)

const 

 HCI_COMMAND_PKT              = $01;
 HCI_ACLDATA_PKT              = $02;
 HCI_SCODATA_PKT              = $03;
 HCI_EVENT_PKT                = $04;
 HCI_VENDOR_PKT              = $ff;

 // Markers
 FIRMWARE_START                = 100;
 FIRMWARE_END                  = 101;
 DELAY_50MSEC                  = 102;
 DELAY_2SEC                    = 103;
 INIT_COMPLETE                 = 104;
 FLUSH_PORT                    = 105;
 OPEN_PORT                     = 106;
 CLOSE_PORT                    = 107;
 SEND_DATA                     = 108;
 SYSTEM_RESTART                = 109;
 CONNECTION_TERMINATED         = 110;

 // from bluez-5.32 hciattach_bcm43xx.c
 CC_MIN_SIZE                   = 7;

 // Link Layer specification Section 4.4.3,Core 4.1 page 2535
 LL_SCAN_WINDOW_MAX          = 10240000; // 10.24s
 LL_SCAN_INTERVAL_MAX         = 10240000; // 10.24s

 SCAN_WINDOW                  = 200000;
 SCAN_INTERVAL                = 500000;
 SCAN_DURATION                = 10000000; // 10s

 BDADDR_LEN                 = 6;

 BDADDR_TYPE_PUBLIC          = 0;
 BDADDR_TYPE_RANDOM          = 1;

 BT_MAX_HCI_EVENT_SIZE         = 257;
 BT_MAX_HCI_COMMAND_SIZE       = 258;
 BT_MAX_DATA_SIZE             = BT_MAX_HCI_COMMAND_SIZE;

 BT_BD_ADDR_SIZE              = 6;
 BT_CLASS_SIZE                = 3;
 BT_NAME_SIZE                = 248;

 OGF_MARKER                    = $00;
 OGF_LINK_CONTROL             = $01;
 OGF_LINK_POLICY               = $02;
 OGF_HOST_CONTROL              = $03;
 OGF_INFORMATIONAL             = $04;
 OGF_LE_CONTROL                = $08;
 OGF_VENDOR                    = $3f;

 //OP_CODE_WRITE_CLASS_OF_DEVICE	= OGF_HCI_CONTROL_BASEBAND or $024;

 INQUIRY_LAP_GIAC            = $9E8B33; // General Inquiry Access Code

 INQUIRY_LENGTH_MIN          = $01;  // 1.28s
 INQUIRY_LENGTH_MAX          = $30;  // 61.44s
 //#define INQUIRY_LENGTH(secs)		(((secs) * 100 + 64) / 128)
 INQUIRY_NUM_RESPONSES_UNLIMITED = $00;

type 
 TBTMarkerEvent = procedure (no:integer);
 TBTLEEvent = procedure (SubEvent:byte; Params:array of byte);

 TBDAddr = array [0 .. BDADDR_LEN - 1] of byte;
 TLEData = array [0 .. 30] of byte;
 TLEKey = array [0 .. 15] of byte;

 PQueueItem = ^TQueueItem;
 TQueueItem = record
  OpCode:Word;
  Params:array of byte;
  Prev,Next:PQueueItem;
 end;

function  OpenUART0:boolean;
procedure CloseUART0;

procedure SendData(Data:Array of Byte);
procedure AddHCICommand(OGF:byte; OCF:Word; Params:array of byte); overload;
procedure AddHCICommand(OpCode:Word; Params:array of byte); overload;

function ogf(Op:Word):byte;
function ocf(Op:Word):Word;
function BDAddrToStr(Addr:TBDAddr):string;
function ErrToStr(Code:byte):string;

procedure NoOP;
procedure AddMarker(Marker:Word);
procedure SetMarkerEvent(anEvent:TBTMarkerEvent);
procedure SetLEEvent(anEvent:TBTLEEvent);

// HCI Commands
procedure ResetChip;
procedure ReadLocalName;
procedure ReadScanEnable;
procedure WriteScanEnable(Enable:byte);

// Link Control
procedure Inquiry;

// Informational Parameters
procedure ReadLocalVersion;
procedure ReadLocalSupportedCommands;
procedure ReadLocalSupportedFeatures;
procedure ReadBDADDR;

// LE
procedure SetLEEventMask(Mask:QWord);
procedure ReadLEBufferSize;
procedure ReadLESupportedFeatures;
procedure SetLERandomAddress(Addr:TBDAddr);
procedure SetLEAdvertisingParameters(MinInterval,MaxInterval:Word;
                                     Type_:byte;
                                     OwnAddressType,PeerAddressType:byte;
                                     PeerAddr:TBDAddr;
                                     ChannelMap,FilterPolicy:byte);
procedure ReadLEAdvertisingChannelTxPower;
procedure SetLEAdvertisingData(Data:array of byte);
procedure SetLEScanResponseData(Data:array of byte);
procedure SetLEAdvertisingEnable(State:boolean);
procedure SetLEScanParameters(Type_:byte; Interval,Window:Word;
                              OwnAddressType,FilterPolicy:byte);
procedure SetLEScanEnable(State,Duplicates:boolean);
procedure LERand;

// BCM Vendor Specific
procedure BCMSetBDAddr(Addr:TBDAddr);
procedure BCMEnableRadio(Enable:boolean);
procedure BCMLoadFirmware(fn:string);
function EventTypeToStr(Type_:byte):string;
procedure RestoreBootFile(Prefix,FileName: String);

var 
 ChipName:string = '';
 Ver:byte = 0;
 Rev:Word = 0;
 BDAddr:TBDAddr = ($b8,$27,$e8,$cc,$72,$27);
 FWHandle:integer; // firmware file handle
 RxBuffer:array of byte;
 HciSequenceNumber:Integer;
 NonBlockEntered,NonBlockReturned:Integer;

implementation

uses Platform,GlobalTypes,GlobalConfig,Threads,FileSystem,SyncObjs,Ultibo;

procedure RestoreBootFile(Prefix,FileName: String);
var 
 Source:String;
begin
 Source:=Prefix + '-' + FileName;
 Log(Format('Restoring from %s ...',[Source]));
 while not DirectoryExists('C:\') do
  sleep(500);
 if FileExists(Source) then
  CopyFile(PChar(Source),PChar(FileName),False);
 Log(Format('Restoring from %s done',[Source]));
end;

var 
 UART0:PSerialDevice = Nil;
 First:PQueueItem = Nil;
 Last:PQueueItem = Nil;
 ReadHandle:TThreadHandle = INVALID_HANDLE_VALUE;
 Queue:TMailslotHandle;
 QueueHandle:TThreadHandle = INVALID_HANDLE_VALUE;
 QueueEvent:TEvent;
 MarkerEvent:TBTMarkerEvent = Nil;
 LEEvent:TBTLEEvent = Nil;
 MonitorReadExecuteHandle:TThreadHandle = INVALID_HANDLE_VALUE;

const 
 ADV_IND                     = $00; // Connectable undirected advertising(default)
 ADV_DIRECT_IND_HI           = $01; // Connectable high duty cycle directed advertising
 ADV_SCAN_IND                = $02; // Scannable undirected advertising
 ADV_NONCONN_IND             = $03; // Non connectable undirected advertising
 ADV_DIRECT_IND_LO           = $04; // Connectable low duty cycle directed advertising


function EventTypeToStr(Type_:byte):string;
begin
 case Type_ of 
  ADV_IND          :Result:='Connectable undirected advertising(default)';
  ADV_DIRECT_IND_HI:Result:='Connectable high duty cycle directed advertising';
  ADV_SCAN_IND     :Result:='Scannable undirected advertising';
  ADV_NONCONN_IND  :Result:='Non connectable undirected advertising';
  ADV_DIRECT_IND_LO:Result:='Connectable low duty cycle directed advertising';
  else                Result:='Reserved for future use(' + Type_.ToHexString(2) + ')';
 end;
end;

function ogf(op:Word):byte;
begin
 Result:=(op shr 10) and $3f;
end;

function ocf(op:Word):Word;
begin
 Result:=op and $3ff;
end;

procedure SetMarkerEvent(anEvent:TBTMarkerEvent);
begin
 MarkerEvent:=anEvent;
end;

procedure SetLEEvent(anEvent:TBTLEEvent);
begin
 LEEvent:=anEvent;
end;

function BDAddrToStr(Addr:TBDAddr):string;
var 
 i:integer;
begin
 Result:='';
 for i:=5 downto 0 do
  if i = 5 then
   Result:=Addr[i].ToHexString(2)
  else
   Result:=Result + ':' + Addr[i].ToHexString(2);
end;

procedure DecodeEvent(ev:array of byte);
var 
 len,num:byte;
 op:Word;
 nr,ofs,se:byte; // number of responses
 i,j:integer;
 prm:array of byte;
 s:string;

procedure ListAD(ad:array of byte);
begin
 if length(ad) = 0 then exit;
 case ad[0] of 
  $01:// flags
      if length(ad) = 2 then
       Log('Flags:' + ad[1].ToHexString(2));
  $ff:// manufacturer specific
      begin
       Log('Manufacturer Specific');
      end;
 end;
end;

begin
 if length(ev) < 3 then exit;
 if ev[0] <> HCI_EVENT_PKT then
  exit;
 len:=ev[2];
 num:=0;
 if len + 2 <> high(ev) then exit;
 case ev[1] of 
  // event code
  $01:  // inquiry complete
      begin
       if len = 1 then Log('Inquiry Complete:' + ErrToStr(ev[3]));
      end;
  $02:// inquiry result event
      begin
       if len > 1 then
        begin
         nr:=ev[3];
         if (nr * 14) + 1 = len then
          begin
           Log('Inquiry result nos ' + ev[3].tostring + ' Len ' + len.ToString);
           for i:=1 to nr do
            begin
             ofs:=((i - 1) * 14) + 3;
             Log('  Controller ' + i.ToString);
             s:='  BD';
             for j:=0 to 5 do
              s:=s + ' ' + ev[j + ofs].ToHexString(2);
             Log(s);
            end;
          end;
        end;
      end;
  $05 :
       begin
        Log(Format('%02.2x) connection terminated status 0x%02.2x reason 0x%02.2x',[256*ev[5] + ev[4],ev[3],ev[6]]));
        AddMarker(CONNECTION_TERMINATED);
       end;
  $0e:  // command complete
      begin
       num:=ev[3];          // num packets controller can accept
       op:=ev[5] * $100 + ev[4];
       //Log('OGF ' + inttohex(ogf(op),2) + ' OCF ' + inttohex(ocf(op),3) + ' OP Code ' + inttohex(op,4) + ' Num ' + num.ToString + ' Len ' + len.ToString);
       if (len > 3) and(ev[6] > 0) then Log('Status ' + ErrToStr(ev[6]));
       case op of 
        $0c14:// read name
              begin
               ChipName:='';
               i:=7;
               while (i <= len + 3) and(ev[i] <> $00) do
                begin
                 ChipName:=ChipName + chr(ev[i]);
                 i:=i + 1;
                end;
              end;
        $1001:// read local version
              begin
               if len = 12 then
                begin
                 Ver:=ev[7];
                 Rev:=ev[9] * $100 + ev[8];
                end;
              end;
        $1009:// read bd addr
              begin
               if len = 10 then for i:=0 to 5 do
                                 BDAddr[i]:=ev[7 + i];
              end;
        $2007:// read le channel tx power
              begin
               //             if len = 5 then Log('Tx Power ' + ev[7].ToString);
              end;
       end;
       // case op
      end;
  // command complete
  $0f:// command status
      begin
       //    Log('Command Status');
       if len = 4 then
        begin
         num:=ev[4];
         if ev[3] <> 0 then
          begin
           op:=ev[6] * $100 + ev[5];
           Log('  Status ' + inttohex(ev[3],2));
           Log('  OGF ' + inttohex(ogf(op),2) + ' OCF ' + inttohex(ocf(op),3) + ' OP Code ' + inttohex(op,4));
          end;
        end;
      end;
  $13:// transmit status
      begin
      end;
  $3e:// le meta event
      begin
       if (len > 2) then
        begin
         se:=ev[3];
         if Assigned(LEEvent) then
          begin
           SetLength(prm,len - 1);
           Move(ev[4],prm[0],len - 1);
           LEEvent(se,prm);
          end;
        end;
       // len <=
      end
      // case
      else
       begin
        s:='';
        for i:=low(ev) + 1 to high(ev) do
         s:=s + ' ' + ev[i].ToHexString(2);
        Log('Unknown event ' + s);
       end;
 end;
 if num > 0 then
  QueueEvent.SetEvent;
 // else
 //  Log(Format('num is %d',[num]));
end;

procedure SendCharacteristic(ConnectionHandle:Word;Handle:Word;AttributeType:Word);
const 
 ReadProperty = $02;
begin
 Log(Format('return characteristic handle %04.4x type %04.4x',[Handle,AttributeType]));
 SendData([HCI_ACLDATA_PKT,lo(ConnectionHandle),hi(ConnectionHandle),7+2+4,0,7+2,0,4,0,$09,7,lo(Handle),hi(Handle),ReadProperty,lo(Handle+1),hi(Handle+1),lo(AttributeType),hi(AttributeType)]);
end;

var 
 StringData:Array of Byte;

procedure SendString2(ConnectionHandle:Word;Message:String);
var 
 I:Integer;
procedure AddByte(X:Byte);
begin
 StringData[I]:=X;
 Inc(I);
end;
begin
 Log(Format('send string %s',[Message]));
 I:=0;
 SetLength(StringData,length(Message)+10);
 AddByte(HCI_ACLDATA_PKT);
 AddByte(lo(ConnectionHandle));
 AddByte(hi(ConnectionHandle));
 AddByte(5+length(Message));
 AddByte(0);
 AddByte(1+length(Message));
 AddByte(0);
 AddByte(4);
 AddByte(0);
 AddByte($b);
 for I:= 0 to Length(Message) - 1 do
  AddByte(Ord(Message[I]));
 SendData(StringData);
end;

procedure SendString3(ConnectionHandle:Word;Message:String);
var 
 I:Integer;
 L:Integer;
procedure AddByte(X:Byte);
begin
 StringData[I]:=X;
 Inc(I);
end;
begin
 Log(Format('send string %s',[Message]));
 I:=0;
 L:=10; // Length(Message);
 SetLength(StringData,length(Message)+10);
 AddByte(HCI_ACLDATA_PKT);
 AddByte(lo(ConnectionHandle));
 AddByte(hi(ConnectionHandle));
 AddByte(5+L);
 AddByte(0);
 AddByte(1+L);
 AddByte(0);
 AddByte(4);
 AddByte(0);
 AddByte($b);
 for I:=1 to L do
  AddByte(I);
 SendData(StringData);
end;

procedure SendString(ConnectionHandle:Word;Message:String);
var 
 L:Integer;
begin
 Log(Format('send string %s',[Message]));
 L:=4;
 SendData([HCI_ACLDATA_PKT,lo(ConnectionHandle),hi(ConnectionHandle),4+L+1,0,L+1,0,4,0,$b,1,2,3,4]);
end;

procedure SendHandleNotFound(ConnectionHandle:Word;OpCode:Byte;Handle:Word);
begin
 Log(Format('handle not found %04.4x',[Handle]));
 SendData([HCI_ACLDATA_PKT,lo(ConnectionHandle),hi(ConnectionHandle),9,0,5,0,4,0,1,OpCode,lo(Handle),hi(Handle),$a]);
end;

procedure AclDataReceived(ConnectionHandle: Word; pkt:array of byte);
var 
 I,L:Integer;
 S:String;
 OpCode:Byte;
 StartHandle,EndHandle,AnotherHandle,AttributeType:Word;
 Properties:Byte;
function GetByte:Byte;
begin
 Result:=pkt[I];
 Inc(I);
end;
function GetWord:Word;
begin
 Result:=pkt[I] + 256*pkt[I+1];
 Inc(I,2);
end;
begin
 I:=0;
 OpCode:=GetByte;
 case OpCode of 
  $01:
      begin
       s:='';
       while I < Length(pkt) do
        s:=s + GetByte.ToHexString(2);
       Log(Format('%02.2x) rx error data %s',[ConnectionHandle,s]));
      end;
  $04:
      begin
       StartHandle:=GetWord;
       EndHandle:=GetWord;
       Log(Format('%02.2x) find information %04.4x-%04.4x',[ConnectionHandle,StartHandle,EndHandle]));
       if (StartHandle = 1) and(EndHandle >= 8) then
        SendData([HCI_ACLDATA_PKT,ConnectionHandle,$00,4+3*4+2,0,3*4+2,0,4,0,5,1,3,0,$00,$2a,5,0,$01,$2a,8,0,$05,$2a])
       else
        SendHandleNotFound(ConnectionHandle,OpCode,StartHandle);
      end;
  $08:
      begin
       StartHandle:=GetWord;
       EndHandle:=GetWord;
       AttributeType:=GetWord;
       case AttributeType of 
        $2803:
              begin
               Log(Format('%02.2x) Read characteristics %04.4x-%04.4x',[ConnectionHandle,StartHandle,EndHandle]));
               if (StartHandle <= 2) and(EndHandle >= 2) then
                SendCharacteristic(ConnectionHandle,2,$2a00)
               else if (StartHandle <= 4) and(EndHandle >= 4) then
                     SendCharacteristic(ConnectionHandle,4,$2a01)
               else if (StartHandle <= 7) and(EndHandle >= 7) then
                     SendCharacteristic(ConnectionHandle,7,$2a05)
               else
                SendHandleNotFound(ConnectionHandle,OpCode,StartHandle);
              end;
        else
         begin
          Log(Format('%02.2x) Read by type %04.4x-%04.4x %04.4x',[ConnectionHandle,StartHandle,EndHandle,AttributeType]));
          SendHandleNotFound(ConnectionHandle,OpCode,StartHandle);
         end;
       end;
      end;
  $09:
      begin
       GetByte;
       while I < High(Pkt) do
        begin
         StartHandle:=GetWord;
         Properties:=GetByte;
         AnotherHandle:=GetWord;
         AttributeType:=GetWord;
         Log(Format('%02.2x.%02.2x) properties 0x%02.2x handle 0x%02.2x type 0x%04.4x',[ConnectionHandle,StartHandle,Properties,AnotherHandle,AttributeType]));
         if (Properties and $02) <> 0 then
          begin
           //              L:=2; // can only have one outstanding request at a time
           //              SendData([HCI_ACLDATA_PKT,lo(ConnectionHandle),hi(ConnectionHandle),4+L+1,0,L+1,0,4,0,$a,lo(AnotherHandle),hi(AnotherHandle)]);
          end;
        end;
       L:=6;
       SendData([HCI_ACLDATA_PKT,lo(ConnectionHandle),hi(ConnectionHandle),4+L+1,0,L+1,0,4,0,$8,lo(StartHandle+1),hi(StartHandle+1),$ff,$ff,$03,$28]);
      end;
  $0A:
      begin
       StartHandle:=GetWord;
       Log(Format('%02.2x) read handle %04.4x',[ConnectionHandle,StartHandle]));
       //      if StartHandle = 3 then
       SendString(ConnectionHandle,'Ultibo Name')
       //      else
       //        SendHandleNotFound(ConnectionHandle,OpCode,StartHandle);
      end;
  $0B:
      begin
       s:='';
       while I < Length(pkt) do
        s:=s + GetByte.ToHexString(2);
       Log(Format('%02.2x) rx value %s',[ConnectionHandle,s]));
      end;
  $10:
      begin
       StartHandle:=GetWord;
       EndHandle:=GetWord;
       AttributeType:=GetWord;
       Log(Format('%02.2x) Read by group type %04.4x-%04.4x %04.4x',[ConnectionHandle,StartHandle,EndHandle,AttributeType]));
       if (StartHandle = 1) and(EndHandle = $ffff) then
        SendData([HCI_ACLDATA_PKT,ConnectionHandle,$00,18,0,14,0,4,0,$11,6,1,0,5,0,00,$18,6,0,8,0,01,$18])
       else
        SendHandleNotFound(ConnectionHandle,OpCode,StartHandle);
      end;
  $1b:
      begin
       StartHandle:=GetWord;
       s:='';
       while I < Length(pkt) do
        s:=s + GetByte.ToHexString(2);
       Log(Format('%02.2x.%02.2x) notified %s',[ConnectionHandle,StartHandle,s]));
      end;
  $1d:
      begin
       StartHandle:=GetWord;
       s:='';
       while I < Length(pkt) do
        s:=s + GetByte.ToHexString(2);
       Log(Format('%02.2x.%02.2x) indicated %s',[ConnectionHandle,StartHandle,s]));
       SendData([HCI_ACLDATA_PKT,lo(ConnectionHandle),hi(ConnectionHandle),5,0,1,0,4,0,$1e]);
      end
      else
       begin
        s:='';
        while I < Length(pkt) do
         s:=s + GetByte.ToHexString(2);
        Log(Format('%02.2x) rx opcode %02.2x data %s',[ConnectionHandle,OpCode,s]));
       end;
 end;
end;

function Monitor(Parameter:Pointer):PtrInt;
var
 Capture1,Capture2:Integer;
begin
 Result:=0;
 while True do
  begin
   Capture1:=NonBlockEntered;
   Capture2:=NonBlockReturned;
   if Capture1 <> Capture2 then
    Log(Format('ReadExecute is not balanced: %d entries %d exits',[Capture1,Capture2]));
   Sleep(5*1000);
  end;
end;

function ReadExecute(Parameter:Pointer):PtrInt;
var 
 DataLength:Integer;
 c:LongWord;
 b:byte;
 i,j,rm:integer;
 decoding:boolean;
 pkt:array of byte;
 res:LongWord;
begin
 try
  Result:=0;
  Log(Format('ReadExecute thread handle %8.8x',[ThreadGetCurrent]));
  // put monitoring thread on same cpu as ReadExecute to avoid cross-cpu caching issues
  ThreadSetCpu(MonitorReadExecuteHandle,CpuGetCurrent);
  ThreadYield;
  c:=0;
  while True do
   begin
    ThreadYield;
    Inc(NonBlockEntered);
    res:=SerialDeviceRead(UART0,@b,1,SERIAL_READ_NON_BLOCK,c);
    Inc(NonBlockReturned);
    if (res = ERROR_SUCCESS) and (c = 1) then
     begin
      // One byte was received,try to read everything that is available
      SetLength(RxBuffer,length(RxBuffer) + 1);
      RxBuffer[high(RxBuffer)]:=b;
      Inc(NonBlockEntered);
      res:=SerialDeviceRead(UART0,@b,1,SERIAL_READ_NON_BLOCK,c);
      Inc(NonBlockReturned);
      while (res = ERROR_SUCCESS) and(c = 1) do
       begin
        SetLength(RxBuffer,length(RxBuffer) + 1);
        RxBuffer[high(RxBuffer)]:=b;
        Inc(NonBlockEntered);
        res:=SerialDeviceRead(UART0,@b,1,SERIAL_READ_NON_BLOCK,c);
        Inc(NonBlockReturned);
       end;
      //if Length(RxBuffer) > 50 then
      // Log(Format('rx buffer %d',[Length(RxBuffer)]));
      i:=0;
      decoding:=True;
      while decoding do
       begin
        decoding:=False;
        if RxBuffer[i] = HCI_ACLDATA_PKT then
         begin
          if i + 5 - 1 <= High(RxBuffer) then
           begin
            DataLength:=256 * RxBuffer[i + 6] + RxBuffer[i + 5];
            //                Log(Format('waiting ACLDATA 0x%02.2x%02.2x %d',[RxBuffer[i + 2],RxBuffer[i + 1],DataLength]));
            if i + 9 + DataLength - 1 <= High(RxBuffer) then
             begin
              SetLength(pkt,DataLength);
              for j:=0 to length(pkt) - 1 do
               pkt[j]:=RxBuffer[9 + j];
              //                  s:='';
              //                  for j:=low(pkt) to high(pkt) do s:=s + ' ' + pkt[j].ToHexString(2);
              //                  Log('ACL DATA' + s);
              AclDataReceived((RxBuffer[1] + RxBuffer[2]*256) and $3ff,pkt);
              Inc(I,9 + DataLength);
              decoding:=i < high(RxBuffer);
             end;
           end;
         end
        else if RxBuffer[i] <> HCI_EVENT_PKT then
              Log(Format('not event %d',[RxBuffer[i]]))
        else if (i + 2 <= high(RxBuffer)) then // mimumum
              if i + RxBuffer[i + 2] + 2 <= high(RxBuffer) then
               begin
                SetLength(pkt,RxBuffer[i + 2] + 3);
                for j:=0 to length(pkt) - 1 do
                 pkt[j]:=RxBuffer[i + j];
{$ifdef show_data}
                s:='';
                for j:=low(pkt) to high(pkt) do
                 s:=s + ' ' + pkt[j].ToHexString(2);
                Log('<--' + s);
{$endif}
                DecodeEvent(pkt);
                i:=i + length(pkt);
                decoding:=i < high(RxBuffer);
               end;
       end;
      // decoding
      if i > 0 then
       begin
        rm:=length(RxBuffer) - i;
        //              Log('Remaining ' + IntToStr(rm));
        if rm > 0 then
         for j:=0 to rm - 1 do
          RxBuffer[j]:=RxBuffer[j + i];
        SetLength(RxBuffer,rm);
       end;
     end;
   end;
 except
  on E:Exception do
       Log(Format('ReadExecute exception %s',[E.Message]));
end;
end;

function OpenUART0:boolean;
var 
 res:LongWord;
begin
 Result:=False;
 UART0:=SerialDeviceFindByDescription(BCM2710_UART0_DESCRIPTION);
 if UART0 = nil then
  begin
   Log('Can''t find UART0');
   exit;
  end;
 res:=SerialDeviceOpen(UART0,115200,SERIAL_DATA_8BIT,SERIAL_STOP_1BIT,SERIAL_PARITY_NONE,SERIAL_FLOW_NONE,0,0);
 if res = ERROR_SUCCESS then
  begin
   Result:=True;
   GPIOFunctionSelect(GPIO_PIN_14,GPIO_FUNCTION_IN);
   GPIOFunctionSelect(GPIO_PIN_15,GPIO_FUNCTION_IN);
   GPIOPullSelect(GPIO_PIN_32,GPIO_PULL_NONE);                    //Added
   GPIOFunctionSelect(GPIO_PIN_32,GPIO_FUNCTION_ALT3);     // TXD0
   GPIOPullSelect(GPIO_PIN_33,GPIO_PULL_UP);                        //Added
   GPIOFunctionSelect(GPIO_PIN_33,GPIO_FUNCTION_ALT3);     // RXD0
   NonBlockEntered:=0;
   NonBlockReturned:=0;
   ReadHandle:=BeginThread(@ReadExecute,Nil,ReadHandle,THREAD_STACK_DEFAULT_SIZE);
   Result:=ReadHandle <> INVALID_HANDLE_VALUE;
  end;
end;

procedure CloseUART0;
begin
 if ReadHandle <> INVALID_HANDLE_VALUE then KillThread(ReadHandle);
 ReadHandle:=INVALID_HANDLE_VALUE;
 if UART0 <> nil then SerialDeviceClose(UART0);
 UART0:=Nil;
end;

procedure AddHCICommand(OGF:byte; OCF:Word; Params:array of byte);
begin
 AddHCICommand((OGF shl 10) or OCF,Params);
end;

procedure AddHCICommand(OpCode:Word; Params:array of byte);
var 
 anItem:PQueueItem;
 i:integer;
begin
 New(anItem);
 anItem^.OpCode:=OpCode;
 SetLength(anItem^.Params,length(Params));
 for i:=0 to length(Params) - 1 do
  anItem^.Params[i]:=Params[i];
 anItem^.Next:=Nil;
 anItem^.Prev:=Last;
 if First = nil then First:=anItem;
 if Last <> nil then Last^.Next:=anItem;
 Last:=anItem;
 if MailSlotSend(Queue,Integer(anItem)) <> ERROR_SUCCESS then
  Log('Error adding Command to queue.');
end;

function QueueHandler(Parameter:Pointer):PtrInt;
var 
 anItem:PQueueItem;
 Cmd:array of byte;
 i:integer;
 res,count:LongWord;
 s:string;
begin
 Result:=0;
 while True do
  begin
   QueueEvent.ResetEvent;
   anItem:=PQueueItem(MailslotReceive(Queue));
   if anItem <> nil then
    begin
     Inc(HciSequenceNumber);
     //if anItem^.OpCode <> $fc4c then
     // Log(Format('started hci sequence %d op code %04.4x',[HciSequenceNumber,anItem^.OpCode]));
     if (ogf(anItem^.OpCode) = OGF_MARKER) and(ocf(anItem^.OpCode) = SEND_DATA) then
      begin
       count:=0;
       s:='';
       for i:=10 to length(anItem^.Params) - 1 do
        s:=s + ' ' + anItem^.Params[i].ToHexString(2);
       Log(Format('%02.2x) tx opcode %02.2x%s',[anItem^.Params[1] + 256*anItem^.Params[2],anItem^.Params[9],s]));
       res:=SerialDeviceWrite(UART0,@anItem^.Params[0],length(anItem^.Params),SERIAL_WRITE_NONE,count);
       if res = ERROR_SUCCESS then
        begin
         //                if QueueEvent.WaitFor(10000) <> wrSignaled then
         //                  Log('Timeout waiting for BT Response.'); // should send nop ???
        end
       else
        Log('Error writing to BT.');
      end
     else if (ogf(anItem^.OpCode) = OGF_MARKER) and(ocf(anItem^.OpCode) > 0) then
           begin
            case ocf(anItem^.OpCode) of 
             DELAY_50MSEC:QueueEvent.WaitFor(50);
             DELAY_2SEC   :
                           begin
                            QueueEvent.WaitFor(2000); Log('2 seconds');
                           end;
             OPEN_PORT   :OpenUART0;
             CLOSE_PORT  :CloseUART0;
            end;
            if Assigned(@MarkerEvent) then MarkerEvent(ocf(anItem^.OpCode));
           end
     else
      begin
       SetLength(Cmd,length(anItem^.Params) + 4);
       Cmd[0]:=HCI_COMMAND_PKT;
       Cmd[1]:=lo(anItem^.OpCode);          // little endian so lowest sent first
       Cmd[2]:=hi(anItem^.OpCode);
       Cmd[3]:=length(anItem^.Params);
       for i:=0 to length(anItem^.Params) - 1 do
        Cmd[4 + i]:=anItem^.Params[i];
       count:=0;
{$ifdef show_data}
       s:='';
       for i:=0 to length(Cmd) - 1 do
        s:=s + ' ' + Cmd[i].ToHexString(2);
       Log('--> ' + s);
{$endif}
       res:=SerialDeviceWrite(UART0,@Cmd[0],length(Cmd),SERIAL_WRITE_NONE,count);
       if res = ERROR_SUCCESS then
        begin
         if QueueEvent.WaitFor(3*1000) <> wrSignaled then
          begin
           s:='';
           for i:=0 to length(Cmd) - 1 do
            s:=s + ' ' + Cmd[i].ToHexString(2);
           Log(Format('hci command sequence number %d op code %4.4x',[HciSequenceNumber,anItem^.OpCode]));
           Log(Format('-->(%d) %s',[Length(Cmd),s]));
           Log('Timeout waiting for BT Response.'); // should send nop ???
           s:='';
           for i:=0 to length(RxBuffer) - 1 do
            s:=s + ' ' + RxBuffer[i].ToHexString(2);
           Log('<-- ' + s);
           ThreadHalt(0);
          end;
        end
       else
        Log('Error writing to BT.');
      end;
     SetLength(anItem^.Params,0);
     Dispose(anItem);
    end;
  end;
end;

procedure NoOP;  // in spec but not liked by BCM chip
begin
 AddHCICommand($00,$00,[]);
end;

procedure SendData(Data:Array of Byte);
begin
 AddHCICommand(OGF_MARKER,SEND_DATA and $3ff,Data);
end;

procedure AddMarker(Marker:Word);
begin
 AddHCICommand(OGF_MARKER,Marker and $3ff,[]);
end;

// host control
procedure ResetChip;
begin
 AddHCICommand(OGF_HOST_CONTROL,$03,[]);
end;

procedure ReadLocalName;
begin
 AddHCICommand(OGF_HOST_CONTROL,$14,[]);
end;

procedure ReadScanEnable;
begin
 AddHCICommand(OGF_HOST_CONTROL,$19,[]);
end;

procedure WriteScanEnable(Enable:byte);
begin
 AddHCICommand(OGF_HOST_CONTROL,$14,[Enable]);
end;

// link control
procedure Inquiry;
var 
 Params:array of byte;
begin
 SetLength(Params,5);
 Params[0]:=$33;
 Params[1]:=$8b;
 Params[2]:=$9e;
 Params[3]:=1;       // inquiry length x * 1.28 secs
 Params[4]:=$00;     // unlimited number of responses
 AddHCICommand(OGF_LINK_CONTROL,$01,Params);
end;

// informational parameters
procedure ReadLocalVersion;
begin
 AddHCICommand(OGF_INFORMATIONAL,$01,[]);
end;

procedure ReadLocalSupportedCommands;
begin
 AddHCICommand(OGF_INFORMATIONAL,$02,[]);
end;

procedure ReadLocalSupportedFeatures;
begin
 AddHCICommand(OGF_INFORMATIONAL,$03,[]);
end;

procedure ReadBDADDR;
begin
 AddHCICommand(OGF_INFORMATIONAL,$09,[]);
end;

// le control
procedure SetLEEventMask(Mask:QWord);
var 
 Params:array of byte;
 MaskHi,MaskLo:DWord;
begin
 MaskHi:=hi(Mask);
 MaskLo:=lo(Mask);
 SetLength(Params,8);
 Params[0]:=MaskLo and $ff;   // lsb
 Params[1]:=(MaskLo shr 8) and $ff;
 Params[2]:=(MaskLo shr 16) and $ff;
 Params[3]:=(MaskLo shr 24) and $ff;
 Params[4]:=MaskHi and $ff;   // lsb
 Params[5]:=(MaskHi shr 8) and $ff;
 Params[6]:=(MaskHi shr 16) and $ff;
 Params[7]:=(MaskHi shr 24) and $ff;
 AddHCICommand(OGF_LE_CONTROL,$01,Params);
end;

procedure ReadLEBufferSize;
begin
 AddHCICommand(OGF_LE_CONTROL,$02,[]);
end;

procedure ReadLESupportedFeatures;
begin
 AddHCICommand(OGF_LE_CONTROL,$03,[]);
end;

procedure SetLERandomAddress(Addr:TBDAddr);
begin
 AddHCICommand(OGF_LE_CONTROL,$05,[Addr[5],Addr[4],Addr[3],Addr[2],Addr[1],Addr[0]]);
end;

procedure SetLEAdvertisingParameters(MinInterval,MaxInterval:Word;
                                     Type_:byte;
                                     OwnAddressType,PeerAddressType:byte;
                                     PeerAddr:TBDAddr;
                                     ChannelMap,FilterPolicy:byte);
begin
 AddHCICommand(OGF_LE_CONTROL,$06,[lo(MinInterval),hi(MinInterval),
 lo(MaxInterval),hi(MaxInterval),
 Type_,OwnAddressType,PeerAddressType,
 PeerAddr[0],PeerAddr[1],PeerAddr[2],
 PeerAddr[3],PeerAddr[4],PeerAddr[5],
 ChannelMap,FilterPolicy]);
end;

procedure ReadLEAdvertisingChannelTxPower;
begin
 AddHCICommand(OGF_LE_CONTROL,$07,[]);
end;

procedure SetLEAdvertisingData(Data:array of byte);
var 
 Params:array of byte;
 Len:byte;
 i:integer;
begin
 SetLength(Params,32);
 for i:=1 to 31 do
  Params[i]:=0;       // clear data
 Len:=length(Data);
 if Len > 31 then Len:=31;
 Params[0]:=Len;
 for i:=0 to Len - 1 do
  Params[i + 1]:=Data[i];
 AddHCICommand(OGF_LE_CONTROL,$08,Params);
end;

procedure SetLEScanResponseData(Data:array of byte);
var 
 Params:array of byte;
 Len:byte;
 i:integer;
begin
 SetLength(Params,32);
 for i:=1 to 31 do
  Params[i]:=0;       // clear data
 Len:=length(Data);
 if Len > 31 then Len:=31;
 Params[0]:=Len;
 for i:=0 to Len - 1 do
  Params[i + 1]:=Data[i];
 AddHCICommand(OGF_LE_CONTROL,$09,Params);
end;

procedure SetLEAdvertisingEnable(State:boolean);
begin
 if State then
  AddHCICommand(OGF_LE_CONTROL,$0a,[$01])
 else
  AddHCICommand(OGF_LE_CONTROL,$0a,[$00]);
end;

procedure SetLEScanParameters(Type_:byte; Interval,Window:Word;
                              OwnAddressType,FilterPolicy:byte);
begin
 AddHCICommand(OGF_LE_CONTROL,$0b,[Type_,lo(Interval),hi(Interval),
 lo(Window),hi(Window),
 OwnAddressType,FilterPolicy]);
end;

procedure SetLEScanEnable(State,Duplicates:boolean);
var 
 Params:array of byte;
begin
 SetLength(Params,2);
 if State then Params[0]:=$01
 else Params[0]:=$00;
 if Duplicates then Params[1]:=$01
 else Params[1]:=$00;
 AddHCICommand(OGF_LE_CONTROL,$0c,Params);
end;

procedure LERand;
begin
 AddHCICommand(OGF_LE_CONTROL,$18,[]);
end;

// BCM vendor specific      http://www.cypress.com/file/298311/download
procedure BCMSetBDAddr(Addr:TBDAddr);
var 
 i:integer;
 Params:array of byte;
begin
 SetLength(Params,6);
 for i:=0 to 5 do
  Params[i]:=Addr[i];
 AddHCICommand(OGF_VENDOR,$001,Params);
end;

procedure BCMLoadFirmware(fn:string);
var 
 hdr:array [0 .. 2] of byte;
 Params:array of byte;
 i,n,len:integer;
 Op:Word;
const 
 FirmwareLoadDelays = 1;
begin
 // firmware file BCM43430A1.hcd under \lib\firmware
 //Log('Loading Firmware file ' + fn);
 FWHandle:=FSFileOpen(fn,fmOpenRead);
 if FWHandle > 0 then
  begin
   AddMarker(FIRMWARE_START);
   AddHCICommand(OGF_VENDOR,$2e,[]);
   n:=FSFileRead(FWHandle,hdr,3);
   while (n = 3) do
    begin
     Op:=(hdr[1] * $100) + hdr[0];
     len:=hdr[2];
     SetLength(Params,len);
     n:=FSFileRead(FWHandle,Params[0],len);
     if (len <> n) then Log('Data mismatch.');
     AddHCICommand(Op,Params);
     n:=FSFileRead(FWHandle,hdr,3);
    end;
   FSFileClose(FWHandle);
   AddMarker(FIRMWARE_END);
   AddMarker(CLOSE_PORT);
   // AddMarker(DELAY_2SEC);
   Log(Format('(Using %d ms delay after firmware load)',[FirmwareLoadDelays*50]));
   for I:=1 to FirmwareLoadDelays do
    AddMarker(DELAY_50MSEC);
   AddMarker(OPEN_PORT);
  end
 else
  Log('Error loading Firmware file ' + fn);
end;

procedure BCMEnableRadio(Enable:boolean);
begin
 if Enable then
  AddHCICommand(OGF_VENDOR,$034,[$01])
 else
  AddHCICommand(OGF_VENDOR,$034,[$00]);
end;

function ErrToStr(code:byte):string;
begin
 // page 377 onwards 4.2
 case code of 
  $00:Result:='Success';
  $01:Result:='Unknown HCI Command';
  $02:Result:='Unknown Connection Identifier';
  $03:Result:='Hardware Failure';
  $04:Result:='Page Timeout';
  $05:Result:='Authentication Failure';
  $06:Result:='PIN or Key Missing';
  $07:Result:='Memory Capacity Exceeded';
  $08:Result:='Connection Timeout';
  $09:Result:='Connection Limit Exceeded';
  $0A:Result:='Synchronous Connection Limit To A Device Exceeded';
  $0B:Result:='ACL Connection Already Exists';
  $0C:Result:='Command Disallowed';
  $0D:Result:='Connection Rejected due to Limited Resources';
  $0E:Result:='Connection Rejected due To Security Reasons';
  $0F:Result:='Connection Rejected due to Unacceptable BD_ADDR';
  $10:Result:='Connection Accept Timeout Exceeded';
  $11:Result:='Unsupported Feature or Parameter Value';
  $12:Result:='Invalid HCI Command Parameters';
  $13:Result:='Remote User Terminated Connection';
  $14:Result:='Remote Device Terminated Connection due to Low Resources';
  $15:Result:='Remote Device Terminated Connection due to Power Off';
  $16:Result:='Connection Terminated By Local Host';
  $17:Result:='Repeated Attempts';
  $18:Result:='Pairing Not Allowed';
  $19:Result:='Unknown LMP PDU';
  $1A:Result:='Unsupported Remote Feature / Unsupported LMP Feature';
  $1B:Result:='SCO Offset Rejected';
  $1C:Result:='SCO Interval Rejected';
  $1D:Result:='SCO Air Mode Rejected';
  $1E:Result:='Invalid LMP Parameters / Invalid LL Parameters';
  $1F:Result:='Unspecified Error';
  $20:Result:='Unsupported LMP Parameter Value / Unsupported LL Parameter Value';
  $21:Result:='Role Change Not Allowed';
  $22:Result:='LMP Response Timeout / LL Response Timeout';
  $23:Result:='LMP Error Transaction Collision';
  $24:Result:='LMP PDU Not Allowed';
  $25:Result:='Encryption Mode Not Acceptable';
  $26:Result:='Link Key cannot be Changed';
  $27:Result:='Requested QoS Not Supported';
  $28:Result:='Instant Passed';
  $29:Result:='Pairing With Unit Key Not Supported';
  $2A:Result:='Different Transaction Collision';
  $2B:Result:='Reserved';
  $2C:Result:='QoS Unacceptable Parameter';
  $2D:Result:='QoS Rejected';
  $2E:Result:='Channel Classification Not Supported';
  $2F:Result:='Insufficient Security';
  $30:Result:='Parameter Out Of Mandatory Range';
  $31:Result:='Reserved';
  $32:Result:='Role Switch Pending';
  $33:Result:='Reserved';
  $34:Result:='Reserved Slot Violation';
  $35:Result:='Role Switch Failed';
  $36:Result:='Extended Inquiry Response Too Large';
  $37:Result:='Secure Simple Pairing Not Supported By Host';
  $38:Result:='Host Busy - Pairing';
  $39:Result:='Connection Rejected due to No Suitable Channel Found';
  $3A:Result:='Controller Busy';
  $3B:Result:='Unacceptable Connection Parameters';
  $3C:Result:='Directed Advertising Timeout';
  $3D:Result:='Connection Terminated due to MIC Failure';
  $3E:Result:='Connection Failed to be Established';
  $3F:Result:='MAC Connection Failed';
  $40:Result:='Coarse Clock Adjustment Rejected but Will Try to Adjust Using Clock';
 end;
end;

initialization
SetLength(RxBuffer,0);
Queue:=MailSlotCreate(1024);
QueueEvent:=TEvent.Create(Nil,True,False,'');
QueueHandle:=BeginThread(@QueueHandler,Nil,QueueHandle,THREAD_STACK_DEFAULT_SIZE);
MonitorReadExecuteHandle:=BeginThread(@Monitor,Nil,MonitorReadExecuteHandle,THREAD_STACK_DEFAULT_SIZE);
end.
