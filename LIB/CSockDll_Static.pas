{-----------------------------------------------------------------------------
 Unit Name: CSockDll
 Author:    oranke_f
 Date:      2010-09-01
 Purpose:
    CSock.dll, CSockE.dll 에 대한 인터페이스 유니트.

 History:
  2012-09-19
    OpenConnection, CloseConnection 에 aPending 인자 추가.
    연결 및 연결해제를 틱에서 처리하게 되어있는 구조를
    호출 즉시 처리할 수 있게 수정함.

  2021-04-13
    정적링크기능 유니트로 분리.

-----------------------------------------------------------------------------}

{$DEFINE UseGetFuncAddr}

unit CSockDll_Static;

{$R csock_as_res.RES}

interface

uses
  Windows, Classes, WinSock, ZLibEx, zhTypes;

const
  // 접근불가. Len+PacketID+Reason(2). Len = 1+2 = 3
  // Reason은 접속오류. 뭐 65536가지는 넘지 않겠지..
  // 여기서는 F000 이후만 사용하고 나머지는 어플에서 정의해서 쓴다. 
  ACCESS_DENINE = $FF;
    ADID_WRONGPACKET     = $F0A0; // 잘못된패킷. 날아오지 않아야 할 패킷의 경우..
      ADID_ZEROPKT        = $F0A1; // 유저에게는 WRONGPACKET을 전송. 서버이벤트로는 세부값을 사용.
      ADID_TOOLARGEPKT    = $F0A2;
      ADID_SOMUCHPKT      = $F0A3; // 너무 많은 패킷.

    ADID_EXCEPTION       = $F00C; // 처리에서 예외가 발생한 패킷.

    ADID_SVRBUG          = $F0FA; // 서버의 수행오류..
    ADID_SERVER_FULL     = $F0FB; // 서버 접속 풀.
    ADID_EMERGENCY       = $F0FC; // 긴급종료.

    ADID_OTHER           = $FFFF; // 나열되지 않은 기타이유.


  // 요청 거부. Len+PID+Reason
  REQ_DENINE  = $FE;
    RDID_OTHER           = $FFFF; // 나열되지 않은 기타이유.

type
  TConnEventStatus = (ctConnected, ctDisconnected, ctConnectFailed);

  TAccDenineEvent  = procedure (aReason: U16; aCode: U32; aParam: Pointer); stdcall;
  TReqDenineEvent  = procedure (aReason: U16; aCode: U32; aParam: Pointer); stdcall;
  TPacketEvent     = procedure (aPID: U8; aDataLen: U16; aData: PU8Array; aParam: Pointer); stdcall;
  TConnectEvent    = procedure (aStatus: TConnEventStatus; aParam: Pointer); stdcall;

type
  TCreateClient = function (
    const aClientName: PAnsiChar;
    aKey: U32;
    aAccDenineEvent  : TAccDenineEvent;
    aReqDenineEvent  : TReqDenineEvent;
    aPacketEvent     : TPacketEvent;
    aConnectEvent    : TConnectEvent;
    aParam: Pointer
  ) : Boolean; stdcall;

  TDeleteClient = function (const aClientName: PAnsiChar): Boolean; stdcall;
  TSetCurClient = TDeleteClient;
  TGetClientCount = function : I32; stdcall;

  TIsConnected  = function : Boolean; stdcall;
  TIsEnableToSend = function (const aDataSize: WORD): Boolean; stdcall;

  TOpenConnection   = procedure (const aIP: PAnsiChar; aPort: WORD; aPending: Boolean = true); stdcall;
  TOpenConnection2  = procedure (const aIP: TIPData; aPort: WORD; aPending: Boolean = true); stdcall;
  TCloseConnection  = procedure (aPending: Boolean = true); stdcall;
  TTickClient       = function (): Boolean; stdcall;
  TTickAllClient    = procedure (); stdcall;

  TToSvr_SendPacketB = function (
    const aPID: U8;
    const aBufCount: Integer;
    const aBufLens: PU8Array;
    const aBufs: PPointerArray
  ): Boolean; stdcall;

  TToSvr_SendPacket2B = function (
    const aPID, aSPID: U8;
    const aBufCount: Integer;
    const aBufLens: PU8Array;
    const aBufs: PPointerArray
  ): Boolean; stdcall;

  TToSvr_SendPacketW = function (
    const aPID: U8;
    const aBufCount: Integer;
    const aBufLens: PU16Array;
    const aBufs: PPointerArray
  ): Boolean; stdcall;

  TToSvr_SendPacket2W = function (
    const aPID, aSPID: U8;
    const aBufCount: U8;
    const aBufLens: PU16Array;
    const aBufs: PPointerArray
  ): Boolean; stdcall;


  TToSvr_BeginPacket = function (const aPID: U8): Boolean; stdcall;
  TToSvr_BeginPacket2 = function (const aPID, aSPID: U8): Boolean; stdcall;
  TToSvr_SendBuffer = function (const pBuf: Pointer; buflen: U16): Boolean; stdcall;
  TToSvr_SendBufferW = function (
    const aBufCount: U8;
    const aBufLens: PU16Array;
    const aBufs: PPointerArray
  ): Boolean; stdcall;
  TToSvr_EndPacket = function (): Boolean; stdcall;


var
  CreateClient    : TCreateClient = nil;
  DeleteClient    : TDeleteClient = nil;
  SetCurClient    : TSetCurClient = nil;
  GetClientCount  : TGetClientCount = nil; 

  IsConnected     : TIsConnected = nil;
  IsEnableToSend  : TIsEnableToSend = nil;

  OpenConnection  : TOpenConnection  = nil;
  OpenConnection2 : TOpenConnection2 = nil;
  CloseConnection : TCloseConnection = nil;
  TickClient      : TTickClient      = nil;
  TickAllClient   : TTickAllClient   = nil;

  ToSvr_SendPacketB  : TToSvr_SendPacketB  = nil;
  ToSvr_SendPacket2B : TToSvr_SendPacket2B = nil;
  ToSvr_SendPacketW  : TToSvr_SendPacketW  = nil;
  ToSvr_SendPacket2W : TToSvr_SendPacket2W = nil;

  ToSvr_BeginPacket   : TToSvr_BeginPacket = nil;
  ToSvr_BeginPacket2  : TToSvr_BeginPacket2 = nil;
  ToSvr_SendBuffer    : TToSvr_SendBuffer = nil;
  ToSvr_SendBufferW   : TToSvr_SendBufferW = nil;
  ToSvr_EndPacket     : TToSvr_EndPacket = nil;


// 델파이에서 쓰기 편하게..
function ToSvr_SendPacketBArr(
  const aPID: U8;
  const aBufCount: Integer;
  const aBufLens: array of Byte;
  const aBufs: array of Pointer
): Boolean;

function ToSvr_SendPacket2BArr(
  const aPID, aSPID: U8;
  const aBufCount: Integer;
  const aBufLens: array of Byte;
  const aBufs: array of Pointer
): Boolean;

function ToSvr_SendPacketWArr(
  const aPID: U8;
  const aBufCount: Integer;
  const aBufLens: array of Word;
  const aBufs: array of Pointer
): Boolean;

function ToSvr_SendPacket2WArr(
  const aPID, aSPID: U8;
  const aBufCount: Integer;
  const aBufLens: array of Word;
  const aBufs: array of Pointer
): Boolean;

function ToSvr_SendBufferWArr(
  const aBufCount: Integer;
  const aBufLens: array of Word;
  const aBufs: array of Pointer
): Boolean;

function HostToIP(const aHost: AnsiString): TIPData;

implementation

//const
  //CSock_Dll = 'CSockE.dll';

//function GetFuncs(const aFuncKey: U32): Pointer; stdcall; external CSock_Dll name 'GetFuncs';


//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------

procedure ExtractZResource(const aResName: String; aDstStream: TMemoryStream);
var
  z: TZDecompressionStream;
  OrgStream: TResourceStream;
  i: LongWord;
begin
  OrgStream:= TResourceStream.Create(hInstance, aResName, RT_RCDATA);
  try
    if OrgStream.Size = 0 then Exit;

    OrgStream.Read(i, SizeOf(LongWord));
    OrgStream.Position := SizeOf(LongWord) * 2;
    aDstStream.Size := i;

    z := TZDecompressionStream.Create(OrgStream);
    try
      z.ReadBuffer(aDstStream.Memory^, aDstStream.Size);
    finally
      z.Free;
    end;
  finally
    OrgStream.Free;
  end;
end;


//------------------------------------------------------------------------------
//Internal MemLoader functions
//------------------------------------------------------------------------------

// This structures only for x86
{$IFNDEF WIN64}
const
IMAGE_SIZEOF_SHORT_NAME            = 8;
IMAGE_NUMBEROF_DIRECTORY_ENTRIES   = 16;
IMAGE_ORDINAL_FLAG32               = $80000000;

type IMAGE_IMPORT_BY_NAME = packed record
     Hint : WORD;
     Name : packed array[0..0] of Char;
end;
type PIMAGE_IMPORT_BY_NAME = ^IMAGE_IMPORT_BY_NAME;

type IMAGE_IMPORT_DESCRIPTOR = packed record
   Characteristics: Cardinal;
   TimeDateStamp: Cardinal;
   ForwarderChain: Cardinal;
   Name: Cardinal;
   FirstThunk: Cardinal;
end;
type PIMAGE_IMPORT_DESCRIPTOR = ^IMAGE_IMPORT_DESCRIPTOR;

type IMAGE_DATA_DIRECTORY = packed record
    VirtualAddress  : DWORD;
    Size            : DWORD;
end;

type IMAGE_EXPORT_DIRECTORY = packed record
      Characteristics: DWord;
      TimeDateStamp: DWord;
      MajorVersion: Word;
      MinorVersion: Word;
      Name: DWord;
      Base: DWord;

      NumberOfFunctions: DWord;
      NumberOfNames: DWord;
      AddressOfFunctions: DWord;
      AddressOfNames: DWord;
      AddressOfNameOrdinals: DWord;
end;

type TISHMisc = packed record
case Integer of
0: (PhysicalAddress: DWORD);
1: (VirtualSize: DWORD);
end;

type IMAGE_SECTION_HEADER = packed record
    Name     : packed array [0..IMAGE_SIZEOF_SHORT_NAME-1] of Byte;
    Misc     : TISHMisc ;//PhysicalAddress : DWORD; // or VirtualSize (union);
    VirtualAddress  : DWORD;
    SizeOfRawData   : DWORD;
    PointerToRawData : DWORD;
    PointerToRelocations : DWORD;
    PointerToLinenumbers : DWORD;
    NumberOfRelocations : WORD;
    NumberOfLinenumbers : WORD;
    Characteristics : DWORD;
  end;

  IMAGE_OPTIONAL_HEADER = packed record
   { Standard fields. }
    Magic           : WORD;
    MajorLinkerVersion : Byte;
    MinorLinkerVersion : Byte;
    SizeOfCode      : DWORD;
    SizeOfInitializedData : DWORD;
    SizeOfUninitializedData : DWORD;
    AddressOfEntryPoint : DWORD;
    BaseOfCode      : DWORD;
    BaseOfData      : DWORD;
   { NT additional fields. }
    ImageBase       : DWORD;
    SectionAlignment : DWORD;
    FileAlignment   : DWORD;
    MajorOperatingSystemVersion : WORD;
    MinorOperatingSystemVersion : WORD;
    MajorImageVersion : WORD;
    MinorImageVersion : WORD;
    MajorSubsystemVersion : WORD;
    MinorSubsystemVersion : WORD;
    Reserved1       : DWORD;
    SizeOfImage     : DWORD;
    SizeOfHeaders   : DWORD;
    CheckSum        : DWORD;
    Subsystem       : WORD;
    DllCharacteristics : WORD;
    SizeOfStackReserve : DWORD;
    SizeOfStackCommit : DWORD;
    SizeOfHeapReserve : DWORD;
    SizeOfHeapCommit : DWORD;
    LoaderFlags     : DWORD;
    NumberOfRvaAndSizes : DWORD;
    DataDirectory: packed array[0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES-1] of
        IMAGE_DATA_DIRECTORY;
  end;

IMAGE_FILE_HEADER = packed record
    Machine              : WORD;
    NumberOfSections     : WORD;
    TimeDateStamp        : DWORD;
    PointerToSymbolTable : DWORD;
    NumberOfSymbols      : DWORD;
    SizeOfOptionalHeader : WORD;
    Characteristics      : WORD;
  end;

IMAGE_NT_HEADERS = packed record
  Signature       : DWORD;
  FileHeader      : IMAGE_FILE_HEADER;
  OptionalHeader  : IMAGE_OPTIONAL_HEADER;
end;
{$ENDIF}
type PIMAGE_NT_HEADERS = ^IMAGE_NT_HEADERS;
type PIMAGE_FILE_HEADER = ^IMAGE_FILE_HEADER;
type PIMAGE_OPTIONAL_HEADER = ^IMAGE_OPTIONAL_HEADER;
type PIMAGE_SECTION_HEADER = ^IMAGE_SECTION_HEADER;
type PIMAGE_DATA_DIRECTORY = ^IMAGE_DATA_DIRECTORY;

type
  TDWORDArray = array[0..9999999] of Cardinal;
type
  TWORDArray = array[0..99999999] of WORD;
type PWORDArray = ^TWORDArray;
type PDWORDArray = ^TDWORDArray;

type IMAGE_DOS_HEADER = packed record     // DOS .EXE header
    e_magic: WORD;                     // Magic number
    e_cblp: WORD;                      // Bytes on last page of file
    e_cp: WORD;                        // Pages in file
    e_crlc: WORD;                      // Relocations
    e_cparhdr: WORD;                   // Size of header in paragraphs
    e_minalloc: WORD;                  // Minimum extra paragraphs needed
    e_maxalloc: WORD;                  // Maximum extra paragraphs needed
    e_ss: WORD;                        // Initial (relative) SS value
    e_sp: WORD;                        // Initial SP value
    e_csum: WORD;                      // Checksum
    e_ip: WORD;                        // Initial IP value
    e_cs: WORD;                        // Initial (relative) CS value
    e_lfarlc: WORD;                    // File address of relocation table
    e_ovno: WORD;                      // Overlay number
    e_res: array[0..3] of WORD;        // Reserved words
    e_oemid: WORD;                     // OEM identifier (for e_oeminfo)
    e_oeminfo: WORD;                   // OEM information; e_oemid specific
    e_res2: array[0..9] of WORD;       // Reserved words
    e_lfanew: Cardinal;                // File address of new exe header
end;
type
  PIMAGE_DOS_HEADER = ^IMAGE_DOS_HEADER;

type
IMAGE_BASE_RELOCATION  = packed record
    VirtualAddress: Cardinal;
    SizeOfBlock: Cardinal;
//  WORD    TypeOffset[1];
end;
type
  PIMAGE_BASE_RELOCATION = ^IMAGE_BASE_RELOCATION;
type
  PIMAGE_EXPORT_DIRECTORY = ^IMAGE_EXPORT_DIRECTORY;

type
  DLLMAIN = function(hinstDLL: Pointer; fdwReason: Cardinal; lpvReserved: Pointer): Integer; stdcall;
type
  PDLLMAIN = ^DLLMAIN;

//---------------------------------------------------------------------------
//Internal_CopyMemory
//---------------------------------------------------------------------------

type
  TMemCpy = procedure(Destination: Pointer; Source: Pointer; Count: NativeUInt);cdecl;

type
  PMemCpy = ^TMemCpy;

procedure Internal_CopyMemory(Destination: Pointer; Source: Pointer; Count: NativeUInt); //was UInt64
var
  MemCpy: TMemCpy;
begin
    MemCpy := nil;
    //typedef void (__cdecl *P_memcpy)(LPVOID Destination, LPCVOID Source, SIZE_T Count);
    //static P_memcpy pmemcpy = NULL;
    if (nil = @MemCpy) then
    begin
        MemCpy := TMemCpy(GetProcAddress(GetModuleHandleA('ntdll.dll'), 'memcpy'));
    end;

    MemCpy(Destination, Source, Count);
end;


//---------------------------------------------------------------------------
//Internal_ZeroMemory
//---------------------------------------------------------------------------
type
  TZeroMem = procedure(What: Pointer; Count: NativeUInt);stdcall;

procedure Internal_ZeroMemory(What: Pointer; Count: NativeUInt); //was UInt64
var
  ZeroMem: TZeroMem;
begin
    ZeroMem := nil;
    if (nil = @ZeroMem) then
        ZeroMem := TZeroMem(GetProcAddress(GetModuleHandleA('kernel32.dll'), 'RtlZeroMemory'));

    ZeroMem(What, Count);
end;

{$IFDEF WIN64}
function AddToPointer(source: Pointer;value: Cardinal) : Pointer;overload;
begin
  Result := Pointer(NativeUInt(source) + NativeUInt(value));//Int64
end;
{$ENDIF}

function AddToPointer(source: Pointer;value: NativeUInt) : Pointer;{$IFDEF WIN64}overload;{$ENDIF}
begin
  Result := Pointer(NativeUInt(source) + value);
end;

function DecPointer(source: Pointer;value: Pointer) : NativeUInt;
begin
  Result := NativeUInt(source) - NativeUInt(value);
end;

function DecPointerInt(source: Pointer;value: NativeUInt) : NativeUInt;
begin
  Result := NativeUInt(source) - NativeUInt(value);
end;

function min(a: Integer; b: Integer): Integer;
begin
  if (a<b) then
    Result := a
  else
    Result := b;
end;

//------------------------------------------------------------------------------
//Internal_Load
//------------------------------------------------------------------------------
function Internal_Load(pData: Pointer) : Pointer;
var
  pPtr: Pointer;
  pImageNTHeaders: PIMAGE_NT_HEADERS;
  intSectionIndex: Integer;
  intImageBaseDelta: Size_t;
  intRelocationInfoSize: UInt;
  pImageBaseRelocations,pReloc: PIMAGE_BASE_RELOCATION;
  pImports,pImport: PIMAGE_IMPORT_DESCRIPTOR;
  pDllMain: DLLMAIN;
  pImageBase: Pointer;

  pImageSectionHeader: PIMAGE_SECTION_HEADER;
  intVirtualSectionSize: Integer;
  intRawSectionSize: Integer;
  pSectionBase: Pointer;

  intRelocCount: Integer;
  pwRelocInfo: PWORD;
  intRelocIndex: Integer;

  magic: PNativeUInt;//rename

  lpszLibName: LPSTR;
  hLib: HMODULE;
  lpPRVA_Import: PNativeUInt;//UInt, not PNativeUInt!
  lpszFunctionName: LPSTR;
begin

  pPtr := pData;

  pPtr := Pointer(Int64(pPtr) + Int64(PIMAGE_DOS_HEADER(pPtr).e_lfanew));
  pImageNTHeaders := PIMAGE_NT_HEADERS(pPtr);

  // Alloc memory for library
  pImageBase := VirtualAlloc(nil, pImageNTHeaders^.OptionalHeader.SizeOfImage, MEM_COMMIT, PAGE_EXECUTE_READWRITE);

  // Copy headers
  Internal_CopyMemory(pImageBase, pData, pImageNTHeaders^.OptionalHeader.SizeOfHeaders);

  // Set pPtr to section table
  pPtr := AddToPointer(pPtr,sizeof(pImageNTHeaders.Signature) + sizeof(pImageNTHeaders.FileHeader) + pImageNTHeaders.FileHeader.SizeOfOptionalHeader);

  for intSectionIndex := 0 to pImageNTHeaders.FileHeader.NumberOfSections-1 do
  begin
    pImageSectionHeader := PIMAGE_SECTION_HEADER(AddToPointer(pPtr,intSectionIndex*sizeof(IMAGE_SECTION_HEADER)));

    //
    intVirtualSectionSize := pImageSectionHeader.Misc.VirtualSize;  //PhysicalAddress new code
    //
    intRawSectionSize := pImageSectionHeader.SizeOfRawData;

    pSectionBase := AddToPointer(pImageBase,pImageSectionHeader.VirtualAddress);

    Internal_ZeroMemory(pSectionBase, intVirtualSectionSize);

    Internal_CopyMemory(pSectionBase,
                        AddToPointer(pData,pImageSectionHeader.PointerToRawData),
                        // Sometimes 0 == intRawSectionSize, for example, when the section has uninitialized data
                        min(intVirtualSectionSize, intRawSectionSize));
  end;
  // Relocations

  // Difference between bases
  intImageBaseDelta := DecPointerInt(pImageBase,pImageNTHeaders.OptionalHeader.ImageBase);

  intRelocationInfoSize :=
        pImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC].Size;

    pImageBaseRelocations :=
        PIMAGE_BASE_RELOCATION(AddToPointer(pImageBase,
                                pImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_BASERELOC].VirtualAddress));

    pReloc := pImageBaseRelocations;

    while DecPointer(pReloc,pImageBaseRelocations) < intRelocationInfoSize do
    begin
        // Number of relocs into current block
        intRelocCount := (pReloc.SizeOfBlock - sizeof(IMAGE_BASE_RELOCATION)) Div sizeof(WORD);

        pwRelocInfo := PWORD(AddToPointer(pReloc,sizeof(IMAGE_BASE_RELOCATION)));

        for intRelocIndex := 0 to intRelocCount-1 do
        begin
            if (pwRelocInfo^ and $f000) <> 0 then
            begin
              magic := PNativeUInt(AddToPointer(pImageBase,pReloc.VirtualAddress+(pwRelocInfo^ and $0fff)));
              magic^ := NativeUInt(magic^ + intImageBaseDelta);
              //*(char* *)((char*)pImageBase + pReloc->VirtualAddress + (pwRelocInfo^ and $0fff)) += intImageBaseDelta;
            end;

            Inc(pwRelocInfo);
        end;

        pReloc := PIMAGE_BASE_RELOCATION(pwRelocInfo);
    end;

    // Import table
    pImports :=
    PIMAGE_IMPORT_DESCRIPTOR(AddToPointer(pImageBase,
                            pImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_IMPORT].VirtualAddress));

    pImport := pImports;

    while 0 <> pImport.Name do
    begin
        // DLL Name
        lpszLibName := LPSTR(AddToPointer(pImageBase,pImport.Name));

        // Load library
        hLib := LoadLibraryA(lpszLibName);

        if 0 = pImport.TimeDateStamp then
          lpPRVA_Import := AddToPointer(pImageBase,pImport.FirstThunk)
        else
          lpPRVA_Import := AddToPointer(pImageBase,pImport.Characteristics);  //new code

        while lpPRVA_Import^ <> 0 do
        begin
            // Get function name

            if (PDWORD(lpPRVA_Import)^ and IMAGE_ORDINAL_FLAG32) <> 0 then
                // Importing by number (ordinal)
                // Ordinal is in the loword
                lpszFunctionName := LPSTR(PDWORD(lpPRVA_Import)^ and $ffff)
            else
                // Importing by name
                lpszFunctionName := LPSTR(@PIMAGE_IMPORT_BY_NAME(AddToPointer(pImageBase,PUInt(lpPRVA_Import)^)).Name[0]);

            // Get function address
            lpPRVA_Import^ := NativeUInt(GetProcAddress(hLib, lpszFunctionName));

            Inc(lpPRVA_Import);
        end;
        Inc(pImport);
    end;

    FlushInstructionCache(GetCurrentProcess(), pImageBase, pImageNTHeaders.OptionalHeader.SizeOfImage);
    if 0 <> pImageNTHeaders.OptionalHeader.AddressOfEntryPoint then
    begin
        // Entry point
        pDllMain := DLLMAIN(AddToPointer(pImageBase,pImageNTHeaders.OptionalHeader.AddressOfEntryPoint));

        if nil <> @pDllMain then
        begin
            pDllMain(Pointer(pImageBase), DLL_PROCESS_ATTACH, nil);
            pDllMain(Pointer(pImageBase), DLL_THREAD_ATTACH, nil);
        end;
    end;
    Result := Pointer(pImageBase);
end;

//------------------------------------------------------------------------------
//Internal_GetProcAddress
//------------------------------------------------------------------------------

function Internal_GetProcAddress(hModule: Pointer; lpProcName: PAnsiChar) : Pointer;
var
  pImageNTHeaders: PIMAGE_NT_HEADERS;
  pExports: PIMAGE_EXPORT_DIRECTORY;
  dwExportedSymbolIndex: Cardinal;
  pPtr: Pointer;
  dwVirtualAddressOfName: Cardinal;
  lpszName: PAnsiChar;
  wIndex: WORD;
  dwVirtualAddressOfAddressOfProc: Cardinal;
begin
  Result := nil;
  if nil <> hModule then
  begin
    pPtr := hModule;

    pPtr := Pointer(Int64(pPtr) + Int64(PIMAGE_DOS_HEADER(pPtr).e_lfanew));
    pImageNTHeaders := PIMAGE_NT_HEADERS(pPtr);

    // export table
    pExports := PIMAGE_EXPORT_DIRECTORY(AddToPointer(hModule,
                                                    pImageNTHeaders.OptionalHeader.DataDirectory[IMAGE_DIRECTORY_ENTRY_EXPORT].VirtualAddress));
    for dwExportedSymbolIndex := 0 to pExports.NumberOfNames-1 do
    begin
      // Cycle on exported by names
      dwVirtualAddressOfName := PDWORDArray(AddToPointer(hModule,pExports.AddressOfNames))[dwExportedSymbolIndex];
      lpszName := LPSTR(AddToPointer(hModule,dwVirtualAddressOfName));

      if lstrcmpA(lpszName, lpProcName) = 0 then // Function found!
      begin
        // Index
        wIndex :=
            PWORDArray(AddToPointer(hModule,pExports.AddressOfNameOrdinals))[dwExportedSymbolIndex];//new code

        dwVirtualAddressOfAddressOfProc :=
            PDWORDArray(AddToPointer(hModule,pExports.AddressOfFunctions))[wIndex];

        Result := AddToPointer(hModule,dwVirtualAddressOfAddressOfProc);
      end;
    end;
  end
end;

//------------------------------------------------------------------------------
//Internal_Unload
//------------------------------------------------------------------------------
procedure Internal_Unload(hModule: Pointer);
var
  pImageBase: Pointer;
  pPtr: Pointer;
  pImageNTHeaders: PIMAGE_NT_HEADERS;
  pDllMain: DLLMAIN;
begin
    if nil <> hModule then
    begin
      pImageBase := hModule;

      pPtr := Pointer(hModule);
      pPtr := Pointer(Int64(pPtr) + Int64(PIMAGE_DOS_HEADER(pPtr).e_lfanew));

      pImageNTHeaders := PIMAGE_NT_HEADERS(pPtr);

      pDllMain := DLLMAIN(AddToPointer(pImageBase,pImageNTHeaders.OptionalHeader.AddressOfEntryPoint));

      if nil <> @pDllMain then
      begin
          pDllMain(pImageBase, DLL_THREAD_DETACH, nil);
          pDllMain(pImageBase, DLL_PROCESS_DETACH, nil);
      end;
      VirtualFree(hModule, 0, MEM_RELEASE);
    end;
end;

//------------------------------------------------------------------------------
//------------------------------------------------------------------------------
//------------------------------------------------------------------------------

var
  ugCSockModule: Pointer = nil;
  //GetFuncs: function (const aFuncKey: U32): Pointer; stdcall = nil;

procedure initUnit();
var
  CSockDllStream: TMemoryStream;
  GetFuncs: function (const aFuncKey: U32): Pointer; stdcall;
begin

  CSockDllStream:= TMemoryStream.Create;
  try
    ExtractZResource('CSOCKE_DLL', CSockDllStream);

    ugCSockModule := Internal_Load(CSockDllStream.Memory);
    if CSockDllStream = nil then Exit;
  finally
    CSockDllStream.Free;
  end;

  GetFuncs := Internal_GetProcAddress(ugCSockModule, 'GetFuncs');


  CreateClient    := GetFuncs($31FE8FC7);
  DeleteClient    := GetFuncs($2F1DFAB6);
  SetCurClient    := GetFuncs($F8463809);
  GetClientCount  := GetFuncs($39C89E0E);

  IsConnected     := GetFuncs($7161419F);
  IsEnableToSend  := GetFuncs($73D45C70);

  OpenConnection  := GetFuncs($69B10188);
  OpenConnection2 := GetFuncs($041760AA);
  CloseConnection := GetFuncs($D611D556);
  TickClient      := GetFuncs($169B7788);
  TickAllClient   := GetFuncs($C53B96CF);

  ToSvr_SendPacketB  := GetFuncs($32145C27);
  ToSvr_SendPacket2B := GetFuncs($AF19A9EB);
  ToSvr_SendPacketW  := GetFuncs($32145C3C);
  ToSvr_SendPacket2W := GetFuncs($AF19AA00);

  ToSvr_BeginPacket   := GetFuncs($727E90C6);
  ToSvr_BeginPacket2  := GetFuncs($BDEBA0EC);
  ToSvr_SendBuffer    := GetFuncs($D1459A33);
  ToSvr_SendBufferW   := GetFuncs($1A53F2E4);
  ToSvr_EndPacket     := GetFuncs($7F1EF8B8);
end;

procedure closeUnit();
begin
  if Assigned(ugCSockModule) then
  begin
    Internal_Unload(ugCSockModule);
    ugCSockModule := nil;
  end;

end;

//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------


function ToSvr_SendPacketBArr(
  const aPID: U8;
  const aBufCount: Integer;
  const aBufLens: array of Byte;
  const aBufs: array of Pointer
): Boolean;
begin
  Result := ToSvr_SendPacketB(aPID, aBufCount, @aBufLens[0], @aBufs[0]);
end;

function ToSvr_SendPacket2BArr(
  const aPID, aSPID: U8;
  const aBufCount: Integer;
  const aBufLens: array of Byte;
  const aBufs: array of Pointer
): Boolean;
var
  pBufLens, pBufs: Pointer;
begin
  //Result := ToSvr_SendPacket2B(aPID, aSPID, aBufCount, @aBufLens[0], @aBufs[0]);
  if Length(aBufLens) > 0 then pBufLens := @aBufLens[0] else pBufLens := nil;
  if Length(aBufs) > 0 then pBufs := @aBufs[0] else pBufs := nil;
  Result := ToSvr_SendPacket2B(aPID, aSPID, aBufCount, pBufLens, pBufs);
end;

function ToSvr_SendPacketWArr(
  const aPID: U8;
  const aBufCount: Integer;
  const aBufLens: array of Word;
  const aBufs: array of Pointer
): Boolean;
var
  pBufLens, pBufs: Pointer;
begin
  //Result := ToSvr_SendPacketW(aPID, aBufCount, @aBufLens[0], @aBufs[0]);
  if Length(aBufLens) > 0 then pBufLens := @aBufLens[0] else pBufLens := nil;
  if Length(aBufs) > 0 then pBufs := @aBufs[0] else pBufs := nil;
  Result := ToSvr_SendPacketW(aPID, aBufCount, pBufLens, pBufs);
end;

function ToSvr_SendPacket2WArr(
  const aPID, aSPID: U8;
  const aBufCount: Integer;
  const aBufLens: array of Word;
  const aBufs: array of Pointer
): Boolean;
var
  pBufLens, pBufs: Pointer;
begin
  //Result := ToSvr_SendPacket2W(aPID, aSPID, aBufCount, @aBufLens[0], @aBufs[0]);
  if Length(aBufLens) > 0 then pBufLens := @aBufLens[0] else pBufLens := nil;
  if Length(aBufs) > 0 then pBufs := @aBufs[0] else pBufs := nil;
  Result := ToSvr_SendPacket2W(aPID, aSPID, aBufCount, pBufLens, pBufs);
end;


function ToSvr_SendBufferWArr(
  const aBufCount: Integer;
  const aBufLens: array of Word;
  const aBufs: array of Pointer
): Boolean;
var
  pBufLens, pBufs: Pointer;
begin
  //Result := ToSvr_SendBufferW(aBufCount, @aBufLens[0], @aBufs[0]);
  if Length(aBufLens) > 0 then pBufLens := @aBufLens[0] else pBufLens := nil;
  if Length(aBufs) > 0 then pBufs := @aBufs[0] else pBufs := nil;
  Result := ToSvr_SendBufferW(aBufCount, pBufLens, pBufs);
end;

function HostToIP(const aHost: AnsiString): TIPData;
var
  wsData   : TWSAData;
  HostEnt  : PHostEnt;
begin
  WSAStartup($0101, wsData);
  try
    hostEnt := GetHostByName(PAnsiChar(aHost));

    if Assigned(HostEnt) and
       Assigned(HostEnt^.H_Addr_List) and
       Assigned(HostEnt^.H_Addr_List^) then
      Result := PIPData(HostEnt^.H_Addr_List^)^
    else
      Result.LID := U32(-1);
  finally
    WSACleanup;
  end;
end;


initialization
  initUnit;


finalization
  closeUnit;

end.

