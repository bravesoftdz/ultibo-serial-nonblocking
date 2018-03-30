program BLETest;
{$mode objfpc}{$H+}

uses 
RaspberryPi3,HTTP,WebStatus,GlobalConfig,GlobalConst,GlobalTypes,Platform,Threads,SysUtils,
Classes,Console,Keyboard,Logging,Ultibo,Serial,BCM2710,FileSystem,SyncObjs;

const 
 HCI_COMMAND_PKT   = $01;
 HCI_EVENT_PKT     = $04;
 OGF_MARKER        = $00;
 OGF_HOST_CONTROL  = $03;
 OGF_INFORMATIONAL = $04;
 OGF_VENDOR        = $3f;

var 
 FWHandle:integer;
 HciSequenceNumber:Integer;
 Console1:TWindowHandle;
 ch:char;
 UART0:PSerialDevice = Nil;
 MonitorSerialDeviceReadHandle:TThreadHandle = INVALID_HANDLE_VALUE;
 MonitorKeyboardHandle:TThreadHandle = INVALID_HANDLE_VALUE;
 HTTPListener:THTTPListener;

procedure Log(s:string);
begin
 ConsoleWindowWriteLn(Console1,s);
end;

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

function ogf(op:Word):byte;
begin
 Result:=(op shr 10) and $3f;
end;

function ocf(op:Word):Word;
begin
 Result:=op and $3ff;
end;

function ErrToStr(code:byte):string;
begin
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

function MonitorSerialDeviceRead(Parameter:Pointer):PtrInt;
var 
 Capture1,Capture2:Integer;
begin
 Result:=0;
 while True do
  begin
   Capture1:=SerialDeviceReadEnterCount;
   Capture2:=SerialDeviceReadExitCount;
   if Capture1 <> Capture2 then
    Log(Format('Non-blocking SerialDeviceRead did not return: %d entries %d exits',[Capture1,Capture2]));
   Sleep(5*1000);
  end;
end;

procedure Fail(Message:String);
begin
 Log(Message);
 while True do
  Sleep(1*1000);
end;

function Readbyte:Byte;
var 
 c:LongWord;
 b:Byte;
 res:Integer;
 timestamp:LongWord;
begin
 Result:=0;
 timestamp:=ClockGetCount;
 while ClockGetCount - timestamp < 3*1000*1000 do
  begin
   c:=0;
   res:=SerialDeviceRead(UART0,@b,1,SERIAL_READ_NON_BLOCK,c);
   if (res = ERROR_SUCCESS) and (c = 1) then
    begin
     Result:=b;
     Exit;
    end
   else
    ThreadYield;
  end;
 Fail('timeout waiting for serial read byte');
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
   // GPIOPullSelect(GPIO_PIN_32,GPIO_PULL_NONE);                    //Added
   GPIOFunctionSelect(GPIO_PIN_32,GPIO_FUNCTION_ALT3);     // TXD0
   // GPIOPullSelect(GPIO_PIN_33,GPIO_PULL_UP);                        //Added
   GPIOFunctionSelect(GPIO_PIN_33,GPIO_FUNCTION_ALT3);     // RXD0
   Result:=True;
  end;
end;

procedure AddHCICommand(OpCode:Word; Params:array of byte);
var 
 i:integer;
 Cmd:array of byte;
 res,count:LongWord;
 PacketType,EventCode,PacketLength,CanAcceptPackets,Status:Byte;
begin
 Inc(HciSequenceNumber);
 SetLength(Cmd,length(Params) + 4);
 Cmd[0]:=HCI_COMMAND_PKT;
 Cmd[1]:=lo(OpCode);          // little endian so lowest sent first
 Cmd[2]:=hi(OpCode);
 Cmd[3]:=length(Params);
 for i:=0 to length(Params) - 1 do
  Cmd[4 + i]:=Params[i];
 count:=0;
 res:=SerialDeviceWrite(UART0,@Cmd[0],length(Cmd),SERIAL_WRITE_NONE,count);
 if res = ERROR_SUCCESS then
  begin
   PacketType:=ReadByte;
   if PacketType <> HCI_EVENT_PKT then
    Fail(Format('event type not hci event: %d',[PacketType]));
   EventCode:=ReadByte;
   if EventCode <> $0E then
    Fail(Format('event code not command completed: %d',[EventCode]));
   PacketLength:=ReadByte;
   if PacketLength <> 4 then
    Fail(Format('packet length not 4: %d',[PacketLength]));
   CanAcceptPackets:=ReadByte;
   if CanAcceptPackets <> 1 then
    Fail(Format('can accept packets not 1: %d',[CanAcceptPackets]));
   ReadByte; // completed command low
   ReadByte; // completed command high
   Status:=ReadByte;
   if Status <> 0 then
    Fail(Format('status not 0: %d',[Status]));
  end
 else
  Log('Error writing to BT.');
end;

procedure AddHCICommand(OGF:byte; OCF:Word; Params:array of byte);
begin
 AddHCICommand((OGF shl 10) or OCF,Params);
end;

procedure ResetChip;
begin
 AddHCICommand(OGF_HOST_CONTROL,$03,[]);
end;

procedure BCMLoadFirmware(fn:string);
var 
 hdr:array [0 .. 2] of byte;
 Params:array of byte;
 n,len:integer;
 Op:Word;
begin
 FWHandle:=FSFileOpen(fn,fmOpenRead);
 if FWHandle > 0 then
  begin
   Log('Firmware load ...');
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
   Log('Firmware load done');
  end
 else
  Log('Error loading Firmware file ' + fn);
end;

procedure WaitForSDDrive;
begin
 while not DirectoryExists('C:\') do
  sleep(500);
end;

procedure StartLogging;
begin
 LOGGING_INCLUDE_COUNTER:=False;
 CONSOLE_REGISTER_LOGGING:=True;
 LoggingConsoleDeviceAdd(ConsoleDeviceGetDefault);
 LoggingDeviceSetDefault(LoggingDeviceFindByType(LOGGING_TYPE_CONSOLE));
end;

procedure TestRestart;
begin
 Log('test was successful - delaying 3 seconds then restarting to try to obtain failure ...');
 Sleep(3*1000);
 RestoreBootFile('test','config.txt');
 Log('restarting ...');
 Sleep(1*1000);
 SystemRestart(0);
end;

function MonitorKeyboard(Parameter:Pointer):PtrInt;
begin
 Result:=0;
 while True do
  begin
   if ConsoleGetKey(ch,nil) then
    case uppercase(ch) of 
     'Q' : SystemRestart(0);
     'R' :
          begin
           RestoreBootFile('test','config.txt');
           SystemRestart(0);
          end;
     'C' : ConsoleWindowClear(Console1);
    end;
  end;
end;

begin
 Console1 := ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_LEFT,True);
 Log('Bluetooth Low Energy (BLE) Firmware Load Test');
 RestoreBootFile('default','config.txt');
 StartLogging;
 MonitorSerialDeviceReadHandle:=BeginThread(@MonitorSerialDeviceRead,Nil,MonitorSerialDeviceReadHandle,THREAD_STACK_DEFAULT_SIZE);
 ThreadSetCpu(MonitorSerialDeviceReadHandle,CpuGetCurrent);
 MonitorKeyboardHandle:=BeginThread(@MonitorKeyboard,Nil,MonitorKeyboardHandle,THREAD_STACK_DEFAULT_SIZE);

 Log('Q - Quit - use default-config.txt');
 Log('R - Restart - use test-config.txt');
 WaitForSDDrive;

 HTTPListener:=THTTPListener.Create;
 HTTPListener.Active:=True;
 WebStatusRegister(HTTPListener,'','',True);

 OpenUart0;
 Sleep(50);
 ResetChip;                            // reset chip
 BCMLoadFirmware('BCM43430A1.hcd');    // load firmware
 Log('Init complete');
 TestRestart;
end.
