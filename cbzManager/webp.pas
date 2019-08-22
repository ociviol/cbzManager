unit webp;

{$mode objfpc}{$H+}

interface

//{$ifndef darwin}
{$define DLL}
//{$endif}

uses
  Classes, SysUtils,
{$ifdef DLL}
  dynlibs,
{$endif}
  Graphics;

type
  psmallint = ^smallint;
  pbyte = ^byte;
  pint = ^integer;

{$ifdef DLL}
  TWebPGetDecoderVersion = function:integer; cdecl;
  TWebPGetInfo = function(data : pbyte; data_size : int64; width : pint; height : pint):integer; cdecl;
  TWebPFree = procedure(p : pointer); cdecl;

  // Decodes WebP images pointed to by 'data' and returns RGBA samples, along
  // with the dimensions in *width and *height. The ordering of samples in
  // memory is R, G, B, A, R, G, B, A... in scan order (endian-independent).
  // The returned pointer should be deleted calling WebPFree().
  // Returns NULL in case of error.
  TWebPDecodeRGBA = function(data : pbyte; data_size : int64; width : pint; height : pint):pbyte; cdecl;
  //WEBP_EXTERN uint8_t* WebPDecodeRGBA(const uint8_t* data, size_t data_size,
  //                                    int* width, int* height);

  // Same as WebPDecodeRGBA, but returning A, R, G, B, A, R, G, B... ordered data.
  TWebPDecodeARGB = function(data : pbyte; data_size : int64; width : pint; height : pint):pbyte; cdecl;
  //WEBP_EXTERN uint8_t* WebPDecodeARGB(const uint8_t* data, size_t data_size,
  //                                    int* width, int* height);

  // Same as WebPDecodeRGBA, but returning B, G, R, A, B, G, R, A... ordered data.
  TWebPDecodeBGRA = function(data : pbyte; data_size : int64; width : pint; height : pint):pbyte; cdecl;
  //WEBP_EXTERN uint8_t* WebPDecodeBGRA(const uint8_t* data, size_t data_size,
  //                                    int* width, int* height);

  // Same as WebPDecodeRGBA, but returning R, G, B, R, G, B... ordered data.
  // If the bitstream contains transparency, it is ignored.
  TWebPDecodeRGB = function(data : pbyte; data_size : int64; width : pint; height : pint):pbyte; cdecl;
  //WEBP_EXTERN uint8_t* WebPDecodeRGB(const uint8_t* data, size_t data_size,
  //                                   int* width, int* height);

  // Same as WebPDecodeRGB, but returning B, G, R, B, G, R... ordered data.
  TWebPDecodeBGR = function(data : pbyte; data_size : int64; width : pint; height : pint):pbyte; cdecl;
  //WEBP_EXTERN uint8_t* WebPDecodeBGR(const uint8_t* data, size_t data_size,
  //                                   int* width, int* height);
{$endif}

const
{$ifdef darwin}
  {$ifndef DLL}
   clibwebp = 'libwebp.a';
   {$linklib /usr/local/lib/libwebp.a}
   {$else}
   cpath = '/usr/local/lib';
   clibwebp = 'libwebp.dylib';
  {$endif}
{$else}
   cpath = '/usr/lib';
   clibwebp = 'libwebp.so';
{$endif}


function WebpToBitmap(data : pointer; size : int64):Tbitmap;
function WebpFileToBitmap(const Filename : String):Tbitmap;
{$ifndef DLL}
function WebPGetDecoderVersion:integer; cdecl; external clibwebp;
function WebPGetInfo(data : pbyte; data_size : int64; width : pint; height : pint):integer; cdecl; external clibwebp;
procedure WebPFree(p : pointer); cdecl; external clibwebp;
function WebPDecodeBGRA(data : pbyte; data_size : int64; width : pint; height : pint):pbyte; cdecl; external clibwebp;
{$else}
function DoWebPGetDecoderVersion:integer;
function DoWebPGetInfo(data : pbyte; data_size : int64; width : pint; height : pint):integer;
procedure DoWebPFree(p : pointer);
{$endif}

{$ifdef DLL}
var
  PWebPGetDecoderVersion: TWebPGetDecoderVersion;
  PWebPGetInfo : TWebPGetInfo;
  PWebPFree : TWebPFree;
  PWebPDecodeRGBA : TWebPDecodeRGBA;
  PWebPDecodeARGB : TWebPDecodeARGB;
  PWebPDecodeBGRA : TWebPDecodeBGRA;
  PWebPDecodeRGB : TWebPDecodeRGB;
  PWebPDecodeBGR : TWebPDecodeBGR;
{$endif}

implementation

{$ifdef DLL}
uses
  Dialogs;
{$endif}


{$ifdef DLL}
var
//  files : TStringlist;
//  libwebp : string;
  HWebplib : TLibHandle;
{$endif}

{$ifdef DLL}
procedure DoWebPFree(p : pointer);
begin
  if HWebplib <> 0 then
    PWebPFree(p);
end;

function DoWebPGetDecoderVersion:integer;
begin
  if HWebplib <> 0 then
    result := PWebPGetDecoderVersion()
  else
    result := -1;
end;

function DoWebPGetInfo(data : pbyte; data_size : int64; width : pint; height : pint):integer;
begin
  if HWebplib <> 0 then
    result := PWebPGetInfo(data, data_size, width, height)
  else
    result := -1;
end;

function DoWebPDecodeBGRA(data : pbyte; data_size : int64; width : pint; height : pint):pbyte;
begin
  if HWebplib <> 0 then
    result := PWebPDecodeBGRA(data, data_size, width, height)
  else
    result := nil;
end;

function DoWebPDecodeRGBA(data : pbyte; data_size : int64; width : pint; height : pint):pbyte;
begin
  if HWebplib <> 0 then
    result := PWebPDecodeRGBA(data, data_size, width, height)
  else
    result := nil;
end;

function DoWebPDecodeARGB(data : pbyte; data_size : int64; width : pint; height : pint):pbyte;
begin
  if HWebplib <> 0 then
    result := PWebPDecodeARGB(data, data_size, width, height)
  else
    result := nil;
end;

function DoWebPDecodeRGB(data : pbyte; data_size : int64; width : pint; height : pint):pbyte;
begin
  if HWebplib <> 0 then
    result := PWebPDecodeRGB(data, data_size, width, height)
  else
    result := nil;
end;

function DoWebPDecodeBGR(data : pbyte; data_size : int64; width : pint; height : pint):pbyte;
begin
  if HWebplib <> 0 then
    result := PWebPDecodeBGR(data, data_size, width, height)
  else
    result := nil;
end;
{$endif}

function WebpToBitmap(data : pointer; size : int64):Tbitmap;
var
  bmp : TBitmap;
  p, pin, pout : pbyte;
  w, h : smallint;
  y, psz : integer;
begin
{$ifdef DLL}
  if HWebplib = 0 then
    Exit(nil);
{$endif}
  try
    bmp := TBitmap.Create;
    psz := 4;
{$ifdef DLL}
{$ifdef Darwin}
    p := DoWebPDecodeARGB(data, size, @w, @h);
{$else}
    p := DoWebPDecodeBGRA(data, size, @w, @h);
{$endif}
{$else}
    p := WebPDecodeBGRA(data, size, @w, @h);
{$endif}
    if p <> nil then
    try
      bmp.Width:= w;
      bmp.Height := h;
      bmp.PixelFormat := pf24bit;
      bmp.BeginUpdate;
      try
        for y := 0 to h-1 do
        begin
          pout := bmp.ScanLine[y];
          pin := p + ((w * psz) * y);
          Move(pin^, pout^, (w * psz));
        end;
      finally
        bmp.EndUpdate;
      end;
    finally
{$ifdef DLL}
      DoWebPFree(p);
{$else}
      WebPFree(p);
{$endif}
    end;
    result := bmp;
  except
    result := nil;
  end;
end;

function WebpFileToBitmap(const Filename : String):Tbitmap;
var
  m : TMemoryStream;
begin
{$ifdef DLL}
  if HWebplib = 0 then exit(nil);
{$endif}
  m := TmemoryStream.Create;
  try
    m.LoadFromFile(Filename);
    m.position := 0;
    result := WebpToBitmap(m.Memory, m.Size);
  finally
    m.free;
  end;
end;

{$ifdef DLL}
initialization
  HWebplib := 0;
{
Files := TSTringlist.Create;
  try
    GetFiles(cpath, clibwebp, Files);
    if Files.Count > 0 then
      libwebp := Files[0];
  finally
    Files.Free;
  end;
  }
  HWebplib := LoadLibrary(clibwebp);
  if HWebplib <> 0 then
  begin
    PWebPGetDecoderVersion := TWebPGetDecoderVersion(GetProcedureAddress(HWebplib, 'WebPGetDecoderVersion'));
    PWebPGetInfo := TWebPGetInfo(GetProcedureAddress(HWebplib, 'WebPGetInfo'));
    PWebPFree := TWebPFree(GetProcedureAddress(HWebplib, 'WebPFree'));
    PWebPDecodeRGBA := TWebPDecodeRGBA(GetProcedureAddress(HWebplib, 'WebPDecodeRGBA'));
    PWebPDecodeARGB := TWebPDecodeARGB(GetProcedureAddress(HWebplib, 'WebPDecodeARGB'));
    PWebPDecodeBGRA := TWebPDecodeBGRA(GetProcedureAddress(HWebplib, 'WebPDecodeBGRA'));
    PWebPDecodeRGB := TWebPDecodeRGB(GetProcedureAddress(HWebplib, 'WebPDecodeRGB'));
    PWebPDecodeBGR := TWebPDecodeBGR(GetProcedureAddress(HWebplib, 'WebPDecodeBGR'));
  end;

finalization
  if HWebplib <> 0 then
    UnloadLibrary(HWebplib);
{$endif}

end.


