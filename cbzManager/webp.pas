unit webp;

{$mode objfpc}{$H+}

interface

{$define DLL}
{$define UseInternalWebp}

uses
  Classes, SysUtils,
{$ifdef DLL}
  dynlibs,
{$endif}
  Graphics;

type
  psmallint = ^smallint;
  pbyte = ^byte;
  ppbyte = ^pbyte;
  pint = ^integer;
  float = single;

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

  // Return the encoder's version number, packed in hexadecimal using 8bits for
  // each of major/minor/revision. E.g: v2.5.7 is 0x020507.
  // WEBP_EXTERN int WebPGetEncoderVersion(void);
  TWebpGetEncoderVersion = function:integer; cdecl;

  //------------------------------------------------------------------------------
  // One-stop-shop call! No questions asked:

  // Returns the size of the compressed data (pointed to by *output), or 0 if
  // an error occurred. The compressed data must be released by the caller
  // using the call 'WebPFree(*output)'.
  // These functions compress using the lossy format, and the quality_factor
  // can go from 0 (smaller output, lower quality) to 100 (best quality,
  // larger output).

  // WEBP_EXTERN size_t WebPEncodeRGB(const uint8_t* rgb,
  //                                  int width, int height, int stride,
  //                                  float quality_factor, uint8_t** output);
  TWebpEncodeRGB = function(bgr : pbyte; width, height, stride : integer;
                            quality_factor : float; output : ppbyte):integer; cdecl;

  // WEBP_EXTERN size_t WebPEncodeBGR(const uint8_t* bgr,
  //                                  int width, int height, int stride,
  //                                  float quality_factor, uint8_t** output);
  TWebpEncodeBGR = function(bgr : pbyte; width, height, stride : integer;
                            quality_factor : float; output : ppbyte):integer; cdecl;
  TWebPEncodeLosslessBGR = function(bgr : pbyte; width, height, stride : integer; output : ppbyte):integer; cdecl;
  // WEBP_EXTERN size_t WebPEncodeRGBA(const uint8_t* rgba,
  //                                   int width, int height, int stride,
  //                                   float quality_factor, uint8_t** output);
  TWebpEncodeRGBA = function(bgr : pbyte; width, height, stride : integer;
                             quality_factor : float; output : ppbyte):integer; cdecl;

  // WEBP_EXTERN size_t WebPEncodeBGRA(const uint8_t* bgra,
  //                                   int width, int height, int stride,
  //                                   float quality_factor, uint8_t** output);
  TWebpEncodeBGRA = function(bgr : pbyte; width, height, stride : integer;
                             quality_factor : float; output : ppbyte):integer; cdecl;

{$endif}

const
{$if defined(darwin)}
  {$ifndef DLL}
   clibwebp = 'libwebp.a';
   {$linklib /usr/local/lib/libwebp.a}
   {$else}
   cpath = '/usr/local/lib';
   clibwebp = 'libwebp.dylib';
  {$endif}
{$elseif defined(Linux)}
   cpath = '/usr/lib';
   clibwebp = 'libwebp.so';
{$else}
  clibwebp = 'libwebpdecoder.dll';
  clibwebpenc = 'libwebp.dll';
var
  cpath : String;
{$endif}

var
  InternalcWebpAvail,
  InternaldWebpAvail : Boolean;

function WebpToBitmap(data : pointer; size : int64):Tbitmap;
function WebpFileToBitmap(const Filename : String):Tbitmap;
function BitmapToWebp(aBitmap : TBitmap; aDest : TMemoryStream):Boolean;
{$ifndef DLL}
function WebPGetDecoderVersion:integer; cdecl; external clibwebp;
function WebPGetInfo(data : pbyte; data_size : int64; width : pint; height : pint):integer; cdecl; external clibwebp;
procedure WebPFree(p : pointer); cdecl; external clibwebp;
function WebPDecodeBGRA(data : pbyte; data_size : int64; width : pint; height : pint):pbyte; cdecl; external clibwebp;
{$else}
function DoWebPGetDecoderVersion:integer;
function DoWebPGetEncoderVersion:integer;
function DoWebPGetInfo(data : pbyte; data_size : int64; width : pint; height : pint):integer;
procedure DoWebPFree(p : pointer);
{$endif}

{$ifdef DLL}
var
  PWebPGetDecoderVersion: TWebPGetDecoderVersion;
  PWebpGetEncoderVersion : TWebpGetEncoderVersion;
  PWebPGetInfo : TWebPGetInfo;
  PWebPFree : TWebPFree;
  PWebPDecodeRGBA : TWebPDecodeRGBA;
  PWebPDecodeARGB : TWebPDecodeARGB;
  PWebPDecodeBGRA : TWebPDecodeBGRA;
  PWebPDecodeRGB : TWebPDecodeRGB;
  PWebPEncodeLosslessBGR : TWebPEncodeLosslessBGR;
  PWebPDecodeBGR : TWebPDecodeBGR;
  PWebpEncodeRGB : TWebpEncodeRGB;
  PWebpEncodeRGBA : TWebpEncodeRGBA;
  PWebpEncodeBGR :TWebpEncodeBGR;
  PWebpEncodeBGRA : TWebpEncodeBGRA;
{$endif}

implementation

uses
{$ifdef Mswindows}
  Forms,
{$endif}
  intfgraphics;


{$ifdef DLL}
var
  HWebplib : TLibHandle;
{$ifdef Mswindows}
  HWebplibenc  : TLibHandle;
{$endif}
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

function DoWebPGetEncoderVersion:integer;
begin
  if HWebplib <> 0 then
    result := PWebPGetEncoderVersion()
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

function DoWebpEncodeRGB(bgr : pbyte; width, height, stride : integer;
                         quality_factor : float; output : ppbyte):integer;
begin
  if HWebplib <> 0 then
    result := PWebPEncodeRGB(bgr, width, height, stride, quality_factor, output)
  else
    result := 0;
end;

function DoWebpEncodeBGR(bgr : pbyte; width, height, stride : integer;
                         quality_factor : float; output : ppbyte):integer;
begin
  if HWebplib <> 0 then
    result := PWebPEncodeBGR(bgr, width, height, stride, quality_factor, output)
  else
    result := 0;
end;

function DoWebPEncodeLosslessBGR(bgr : pbyte; width, height, stride : integer; output : ppbyte):integer;
begin
  if HWebplib <> 0 then
    result := PWebPEncodeLosslessBGR(bgr, width, height, stride, output)
  else
    result := 0;
end;

function DoWebpEncodeRGBA(bgr : pbyte; width, height, stride : integer;
                          quality_factor : float; output : ppbyte):integer;
begin
  if HWebplib <> 0 then
    result := PWebPEncodeRGBA(bgr, width, height, stride, quality_factor, output)
  else
    result := 0;
end;

function DoWebpEncodeBGRA(bgr : pbyte; width, height, stride : integer;
                         quality_factor : float; output : ppbyte):integer;
begin
  if HWebplib <> 0 then
    result := PWebPEncodeBGRA(bgr, width, height, stride, quality_factor, output)
  else
    result := 0;
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
{$ifdef MsWindows}
      bmp.PixelFormat := pf32bit;
{$else}
      bmp.PixelFormat := pf24bit;
{$endif}
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

function BitmapToWebp(aBitmap : TBitmap; aDest : TMemoryStream):Boolean;
var
  p, pin, pout : pbyte;
  sz, psz : integer;
  w, h, x, y, clr, stride : integer;
begin
  result := false;
  w := aBitmap.Width;
  h := aBitmap.Height;
  case aBitmap.PixelFormat of
    pf32bit: psz := 4;
    pf24bit: psz := 3;
  end;

  p := GetMem((w * h) * psz);
  try
    stride := w * psz;
    for y := 0 to h-1 do
    begin
      pin := aBitmap.ScanLine[y];
      pout := p + ((w * psz) * y);
      Move(pin^, pout^, (w * psz));
    end;

    try
    {$ifdef Darwin}
      sz := DoWebpEncodeRGB(p, w, h, stride, 90, @pout);
    {$else}
//      sz := DoWebPEncodeLosslessBGR(p, w, h, stride, @pout);
      sz := DoWebPEncodeBGR(p, w, h, stride, 75, @pout);
    {$endif}
      aDest.Write(pout^, sz);
      aDest.Position := 0;
      result := True;
    finally
      DoWebPFree(pout);
    end;
  finally
    FreeMem(p);
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
  InternalcWebpAvail := False;
  InternaldWebpAvail := False;
{$ifdef Mswindows}
  cpath := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
{$endif}
  HWebplib := LoadLibrary({$ifdef Mswindows} cpath + {$ifdef DEBUG} 'Bin-Win\' + {$endif}{$endif} clibwebp);
  if HWebplib <> 0 then
  begin
    InternaldWebpAvail := True;
    PWebPGetDecoderVersion := TWebPGetDecoderVersion(GetProcedureAddress(HWebplib, 'WebPGetDecoderVersion'));
    PWebPGetInfo := TWebPGetInfo(GetProcedureAddress(HWebplib, 'WebPGetInfo'));
    PWebPFree := TWebPFree(GetProcedureAddress(HWebplib, 'WebPFree'));
    PWebPDecodeRGBA := TWebPDecodeRGBA(GetProcedureAddress(HWebplib, 'WebPDecodeRGBA'));
    PWebPDecodeARGB := TWebPDecodeARGB(GetProcedureAddress(HWebplib, 'WebPDecodeARGB'));
    PWebPDecodeBGRA := TWebPDecodeBGRA(GetProcedureAddress(HWebplib, 'WebPDecodeBGRA'));
    PWebPDecodeRGB := TWebPDecodeRGB(GetProcedureAddress(HWebplib, 'WebPDecodeRGB'));
    PWebPDecodeBGR := TWebPDecodeBGR(GetProcedureAddress(HWebplib, 'WebPDecodeBGR'));
{$ifdef UseInternalWebp}
{$ifndef Mswindows}
    InternalcWebpAvail := True;
    PWebpEncodeRGB := TWebpEncodeRGB(GetProcedureAddress(HWebplib, 'WebPEncodeRGB'));
    PWebpEncodeRGBA := TWebpEncodeRGBA(GetProcedureAddress(HWebplib, 'WebPEncodeRGBA'));
    PWebpEncodeBGR := TWebpEncodeBGR(GetProcedureAddress(HWebplib, 'WebPEncodeBGR'));
    PWebpEncodeBGRA := TWebpEncodeBGRA(GetProcedureAddress(HWebplib, 'WebPEncodeBGRA'));
{$endif}
{$endif}
  end;
{$ifdef Mswindows}
{$ifdef UseInternalWebp}
  HWebplibenc := LoadLibrary(cpath + {$ifdef DEBUG} 'Bin-Win\' + {$endif}clibwebpenc);
  if HWebplibenc <> 0 then
  begin
    InternalcWebpAvail := True;
    PWebpEncodeRGB := TWebpEncodeRGB(GetProcedureAddress(HWebplibenc, 'WebPEncodeRGB'));
    PWebpEncodeRGBA := TWebpEncodeRGBA(GetProcedureAddress(HWebplibenc, 'WebPEncodeRGBA'));
    PWebPEncodeLosslessBGR := TWebPEncodeLosslessBGR(GetProcedureAddress(HWebplibenc, 'WebPEncodeLosslessBGR'));
    PWebpEncodeBGR := TWebpEncodeBGR(GetProcedureAddress(HWebplibenc, 'WebPEncodeBGR'));
    PWebpEncodeBGRA := TWebpEncodeBGRA(GetProcedureAddress(HWebplibenc, 'WebPEncodeBGRA'));
  end;
{$endif}
{$endif}


finalization
  if HWebplib <> 0 then
    UnloadLibrary(HWebplib);
{$ifdef Mswindows}
  if HWebplibenc <> 0 then
    UnloadLibrary(HWebplibenc);
{$endif}

{$endif}

end.


