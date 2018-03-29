program BLETest;
{$mode objfpc}{$H+}

uses 
RaspberryPi3,
HTTP,WebStatus,
GlobalConfig,
GlobalConst,
GlobalTypes,
Platform,
Threads,
SysUtils,
Classes,Console,Keyboard,
uLog,uBLE,
Ultibo,uHCI
  { Add additional units here };

var 
 Console1:TWindowHandle;
 ch : char;

procedure Log1(s : string);
begin
 ConsoleWindowWriteLn(Console1,s);
end;

procedure WaitForSDDrive;
begin
 while not DirectoryExists('C:\') do
  sleep(500);
end;

procedure StartLeAdvertising;
var 
 s : string;
begin
 s := 'Ultibo';
 ClearAdvertisingData;
 AddAdvertisingData(ADT_FLAGS,[$1a]);
 AddAdvertisingData(ADT_COMPLETE_LOCAL_NAME,s);
 StartUndirectedAdvertising;
end;

// called when a marker event is processed by the comms queue
procedure DoMarkerEvent(no : integer);
begin
 case no of 
  FIRMWARE_START : Log1('Load Firmware ...');
  FIRMWARE_END   : Log1('Load Firmware done');
  SYSTEM_RESTART :
                  begin
                   Log1('test was successful - delaying 3 seconds then restarting to try to obtain failure ...');
                   Sleep(3*1000);
                   RestoreBootFile('test','config.txt');
                   Log1('restarting ...');
                   Sleep(1*1000);
                   SystemRestart(0);
                  end;
  //OPEN_PORT      : Log1('Opening UART0.');
  //CLOSE_PORT     : Log1('Closing UART0.');
  CONNECTION_TERMINATED: StartLeAdvertising;
  INIT_COMPLETE  :
                  begin
                   Log1('BLE Chip Initialised');
                   Log1('  Name    : ' + ChipName);
                   Log1(format('  Version : %d Revision : %d',[Ver,Rev]));
                   Log1('  Address : ' + BDAddrToStr(BDAddr));
                  end;
 end;
end;

var 
 HTTPListener:THTTPListener;

begin
 Console1 := ConsoleWindowCreate(ConsoleDeviceGetDefault,CONSOLE_POSITION_LEFT,True);
 Log1('Bluetooth Low Energy (BLE) Peripheral Test');
 RestoreBootFile('default','config.txt');
 SetLogProc(@Log1);
 Log1('Q - Quit - use default-config.txt');
 Log1('R - Restart - use test-config.txt');
 WaitForSDDrive;

 begin
  HTTPListener:=THTTPListener.Create;
  HTTPListener.Active:=True;
  WebStatusRegister(HTTPListener,'','',True);

  SetMarkerEvent(@DoMarkerEvent);       // set marker event(called when marker processed on event queue)
  AddMarker(OPEN_PORT);                 // open uart
  AddMarker(DELAY_50MSEC);              // ensure read thread has started
  ResetChip;                             // reset chip
  BCMLoadFirmware('BCM43430A1.hcd');    // load firmware
  AddMarker(DELAY_50MSEC);              // ensure read thread has started
  ReadLocalName;                         // read new chip name
  AddMarker(DELAY_50MSEC);              // ensure read thread has started
  ReadLocalVersion;                      // read new HCI version
  AddMarker(DELAY_50MSEC);              // ensure read thread has started
  ReadBDADDR;                            // read newly assigned BD address
  AddMarker(INIT_COMPLETE);             // indicate initialisation complete
  StartLeAdvertising;
  AddMarker(SYSTEM_RESTART);

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
    ThreadHalt(0);
   end;
 end;
end.
