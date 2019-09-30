unit webp;

{
 Ollivier Civiol - 2019
 ollivier@civiol.eu
 https://ollivierciviolsoftware.wordpress.com/
}
{$mode objfpc}{$H+}

interface


uses
  Classes, SysUtils,
  dynlibs,
  Graphics;

type

  { TWebpImage }

  TWebpImage = Class
  private
    FBitmap : TBitmap;
    function GetHeight: Integer;
    function GetWidth: Integer;
  public
    constructor Create; overload;
    constructor Create(aBitmap : TBitmap); overload;
    constructor Create(aStream : TStream); overload;
    destructor Destroy; override;

    class function WebpEncoderVersion:string;
    class function WebpDecoderVersion:string;

    procedure Assign(aGraphic : TGraphic); overload;
    procedure Assign(aStream : TStream); overload;
    procedure SaveToFile(const aFilename : String; QualityFactor : Single = 75);
    procedure SaveToStream(aStream : TStream; QualityFactor : Single = 75);
    function GetBitmap:TBitmap;

    property Width : Integer read GetWidth;
    property Height : Integer read GetHeight;
  end;

var
  InternalcWebpAvail,
  InternaldWebpAvail : boolean;

implementation

uses
{$ifdef Mswindows}
  Forms,
{$endif}
  intfgraphics;

var
  HWebplib : TLibHandle;
{$ifdef Mswindows}
  HWebplibenc  : TLibHandle;
{$endif}


const
  // I/O image format identifiers.
  FIF_UNKNOWN = -1;
  FIF_BMP     = 0;
//  FIF_ICO     = 1;
  FIF_JPEG    = 2;
//  FIF_JNG     = 3;
  FIF_WEBP    = 4;
  FIF_PNG     = 5;
  FIF_JP2     = 6;
  FIF_GIF     = 7;

class function GetImageType(const aSrc : TStream; const aFileName : String = ''):Integer;
var
  //p : pbyte;
  p : pbyte;
  s : string;
  oldpos : int64;
begin
  if Assigned(aSrc) then
  begin
    GetMem(p, 12);
    try
      oldpos := aSrc.Position;
      aSrc.ReadBuffer(p^, 12);
      aSrc.Position := oldpos;

      if (p[0] = $FF) and (p[1] = $D8) then
        Exit(FIF_JPEG);

      if (p[0] = $89) and (p[1] = $50) and
         (p[2] = $4E) and (p[3] = $47) and
         (p[4] = $0D) and (p[5] = $0A) and
         (p[6] = $1A) and (p[7] = $0A) then
        Exit(FIF_PNG);

      if (p[0] = $00) and (p[1] = $00) and (p[2] = $00) and
         (p[3] = $0C) and (p[4] = $6A) and (p[5] = $50) and
         (p[6] = $20) and (p[7] = $20) and (p[8] = $0D) and
         (p[9] = $0A) then
        Exit(FIF_JP2);

      if ((p[0] = $47) and (p[1] = $49) and (p[2] = $46) and
          (p[3] = $38) and (p[4] = $37) and (p[5] = $61)) or
         ((p[0] = $47) and (p[1] = $49) and (p[2] = $46) and
          (p[3] = $38) and (p[4] = $39) and (p[5] = $61)) then
        Exit(FIF_GIF);

      if (p[0] = $52) and (p[1] = $49) and (p[2] = $46) and
         (p[3] = $46) and (p[8] = $57) and (p[9] = $45) and
         (p[10] =$42) and (p[11] = $50) then
        Exit(FIF_WEBP);

      if (p[0] = $42) and (p[1] = $4D) then
        Exit(FIF_BMP);
    finally
      FreeMem(p);
    end;
  end
  else
  begin
    // test using FileName
    s := ExtractFileExt(aFileName);
    if s.ToLower = '.webp' then
      Exit(FIF_WEBP);
    if (s.ToLower = '.jpg') or (s.ToLower = '.jpeg') then
      Exit(FIF_JPEG);
    if (s.ToLower = '.jp2') or (s.ToLower = '.jpx') or (s.ToLower = '.j2k') then
      Exit(FIF_JP2);
    if s.ToLower = '.gif' then
      Exit(FIF_GIF);
    if s.ToLower = '.bmp' then
      Exit(FIF_BMP);
    if s.ToLower = '.png' then
      Exit(FIF_PNG);
  end;
  result := FIF_UNKNOWN;
end;

type
  pbyte = ^byte;
  ppbyte = ^pbyte;
  pint = ^integer;
  float = single;

  TWebPGetDecoderVersion = function:longint; cdecl;
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
  TWebpGetEncoderVersion = function:longint; cdecl;

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


const
{$if defined(darwin)}
 cpath = '/usr/local/lib';
 clibwebp = 'libwebp.dylib';
{$elseif defined(Linux)}
 cpath = '/usr/lib';
 clibwebp = 'libwebp.so';
{$else}
  clibwebp = 'libwebpdecoder.dll';
  clibwebpenc = 'libwebp.dll';
var
  cpath : String;
{$endif}

{
var
InternalcWebpAvail,
InternaldWebpAvail : Boolean;

function WebpToBitmap(data : pointer; size : int64):Tbitmap;
function WebpFileToBitmap(const Filename : String):Tbitmap;
function BitmapToWebp(aBitmap : TBitmap; aDest : TMemoryStream):Boolean;
function DoWebPGetDecoderVersion:string;
function DoWebPGetEncoderVersion:string;
function DoWebPGetInfo(data : pbyte; data_size : int64; width : pint; height : pint):integer;
procedure DoWebPFree(p : pointer);
}

var
  PWebPGetDecoderVersion: TWebPGetDecoderVersion;
  PWebpGetEncoderVersion : TWebpGetEncoderVersion;
  PWebPGetInfo : TWebPGetInfo;
  PWebPFree : TWebPFree;
  PWebPDecodeRGBA : TWebPDecodeRGBA;
  PWebPDecodeARGB : TWebPDecodeARGB;
  PWebPDecodeBGRA : TWebPDecodeBGRA;
  PWebPDecodeRGB : TWebPDecodeRGB;
  PWebPDecodeBGR : TWebPDecodeBGR;
  PWebpEncodeRGB : TWebpEncodeRGB;
  PWebpEncodeRGBA : TWebpEncodeRGBA;
  PWebpEncodeBGR :TWebpEncodeBGR;
  PWebpEncodeBGRA : TWebpEncodeBGRA;


procedure DoWebPFree(p : pointer);
begin
  if HWebplib <> 0 then
    PWebPFree(p);
end;

function HexToVer(const aHex : String):String; inline;
begin
  result := copy(aHex, 1, 2) + '.' +
            copy(aHex, 3, 2) + '.' +
            copy(aHex, 5, 2);
end;

function DoWebPGetDecoderVersion:string;
begin
  if (HWebplib <> 0) and Assigned(PWebPGetDecoderVersion) then
    result := HexToVer(IntToHex(PWebPGetDecoderVersion(), 6))
  else
    result := 'Unavailable';
end;

function DoWebPGetEncoderVersion:string;
begin
  if (HWebplib <> 0) and Assigned(PWebPGetEncoderVersion) then
    result := HexToVer(IntToHex(PWebPGetEncoderVersion(), 6))
  else
    result := 'Unavailable';
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
  if {$ifdef MsWindows}HWebplibenc{$else}HWebplib{$endif} <> 0 then
    result := PWebPEncodeRGB(bgr, width, height, stride, quality_factor, output)
  else
    result := 0;
end;

function DoWebpEncodeBGR(bgr : pbyte; width, height, stride : integer;
                         quality_factor : float; output : ppbyte):integer;
begin
  if {$ifdef MsWindows}HWebplibenc{$else}HWebplib{$endif} <> 0 then
    result := PWebPEncodeBGR(bgr, width, height, stride, quality_factor, output)
  else
    result := 0;
end;

function DoWebpEncodeRGBA(bgr : pbyte; width, height, stride : integer;
                          quality_factor : float; output : ppbyte):integer;
begin
  if {$ifdef MsWindows}HWebplibenc{$else}HWebplib{$endif} <> 0 then
    result := PWebPEncodeRGBA(bgr, width, height, stride, quality_factor, output)
  else
    result := 0;
end;

function DoWebpEncodeBGRA(bgr : pbyte; width, height, stride : integer;
                         quality_factor : float; output : ppbyte):integer;
begin
  if {$ifdef MsWindows}HWebplibenc{$else}HWebplib{$endif} <> 0 then
    result := PWebPEncodeBGRA(bgr, width, height, stride, quality_factor, output)
  else
    result := 0;
end;

function WebpToBitmap(data : pointer; size : int64):Tbitmap;
var
  bmp : TBitmap;
  p, pin, pout : pbyte;
  w, h : smallint;
  y, psz : integer;
begin
  if HWebplib = 0 then
    Exit(nil);

  bmp := TBitmap.Create;
  try
    psz := 4;
{$ifdef Darwin}
    p := DoWebPDecodeARGB(data, size, @w, @h);
{$else}
    p := DoWebPDecodeBGRA(data, size, @w, @h);
{$endif}
    if p <> nil then
    try
      bmp.Width:= w;
      bmp.Height := h;
{$ifdef Linux}
      bmp.PixelFormat := pf24bit;
{$else}
      bmp.PixelFormat := pf32bit;
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
      DoWebPFree(p);
    end;
    result := bmp;
  except
    bmp.free;
    result := nil;
  end;
end;

function BitmapToWebp(aBitmap : TBitmap; aDest : TStream; QualityFactor : Single):Boolean;
var
  p, pin, pout : pbyte;
  sz, psz : integer;
  w, h, y, stride : integer;
begin
  result := false;
  w := aBitmap.Width;
  h := aBitmap.Height;
{$if defined(Linux)}
  if aBitmap.PixelFormat <> pf24bit then
    aBitmap.PixelFormat:=pf24bit;
{$else}
  if aBitmap.PixelFormat <> pf32bit then
    aBitmap.PixelFormat := pf32bit;
{$endif}
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
    {$if defined(Linux)}
      sz := DoWebpEncodeBGR(p, w, h, stride, QualityFactor, @pout);
    {$else}
      sz := DoWebPEncodeBGRA(p, w, h, stride, QualityFactor, @pout);
    {$endif}
      if sz > 0 then
      begin
        aDest.Write(pout^, sz);
        aDest.Position := 0;
        result := True;
      end;
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
  if HWebplib = 0 then exit(nil);

  m := TmemoryStream.Create;
  try
    m.LoadFromFile(Filename);
    m.position := 0;
    result := WebpToBitmap(m.Memory, m.Size);
  finally
    m.free;
  end;
end;

{ TWebpImage }

constructor TWebpImage.Create;
begin
  FBitmap := TBitmap.Create;
  inherited;
end;

constructor TWebpImage.Create(aBitmap : TBitmap); overload;
begin
  Create;
  Assign(aBitmap);
end;

constructor TWebpImage.Create(aStream : TStream); overload;
begin
  Create;
  Assign(aStream);
end;

destructor TWebpImage.Destroy;
begin
  FBitmap.Free;
  inherited Destroy;
end;

class function TWebpImage.WebpEncoderVersion:string;
begin
  if not InternalcWebpAvail then
    raise Exception.Create('Webp Encoder unavailable !');
  result := DoWebPGetEncoderVersion;
end;

class function TWebpImage.WebpDecoderVersion:string;
begin
  if not InternaldWebpAvail then
    raise Exception.Create('Webp Decoder unavailable !');
  result := DoWebPGetDecoderVersion;
end;

function TWebpImage.GetHeight: Integer;
begin
  result := FBitmap.Height;
end;

function TWebpImage.GetWidth: Integer;
begin
  result := FBitmap.Width;
end;

procedure TWebpImage.Assign(aGraphic: TGraphic);
var
  m : TMemoryStream;
begin
  m := TMemoryStream.Create;
  try
    aGraphic.SaveToStream(m);
    Assign(m);
  finally
    m.Free;
  end;
end;

procedure TWebpImage.Assign(aStream: TStream);
var
  p : TPicture;
  m : TMemoryStream;
  it : Integer;
begin
  it := GetImageType(aStream);

  if it = FIF_WEBP then
  begin
   m := TMemoryStream.Create;
   try
     FBitmap.Free;
     if aStream is TMemoryStream then
       FBitmap := WebpToBitmap(TMemoryStream(aStream).Memory, aStream.Size)
     else
     begin
        m.CopyFrom(aStream, aStream.Size);
        m.Position := 0;
        FBitmap := WebpToBitmap(m.Memory, aStream.Size)
     end;
    finally
      m.Free;
    end;
  end
  else
  if it <> FIF_BMP then
  begin
    m := TMemoryStream.Create;
    try
      p := TPicture.Create;
      try
        p.LoadFromStream(aStream);
        p.SaveToStreamWithFileExt(m, 'bmp');
        m.position := 0;
        FBitmap.LoadFromStream(m);
      finally
        p.Free;
      end;
    finally
      m.free;
    end;
  end
  else
    FBitmap.LoadFromStream(aStream);
end;

procedure TWebpImage.SaveToFile(const aFilename: String; QualityFactor : Single = 75);
var
  fs : TFileStream;
begin
  fs := TFileStream.Create(aFilename, fmCreate);
  try
    try
      SaveToStream(fs, QualityFactor);
    except
      raise Exception.Create('Webp error saving to file');
    end;
  finally
    fs.free;
  end;
end;

procedure TWebpImage.SaveToStream(aStream: TStream; QualityFactor : Single = 75);
begin
  if not BitmapToWebp(FBitmap, aStream, QualityFactor) then
    raise Exception.Create('Webp error saving to stream');
end;

function TWebpImage.GetBitmap: TBitmap;
begin
  result := TBitmap.Create;
  result.Assign(FBitmap);
end;


initialization
  HWebplib := 0;
  InternalcWebpAvail := false;
  InternaldWebpAvail := false;
{$ifdef Mswindows}
  cpath := IncludeTrailingPathDelimiter(ExtractFilePath(Application.ExeName));
{$endif}
  HWebplib := LoadLibrary({$ifdef Mswindows} cpath + {$ifdef DEBUG} 'Bin-Win\' + {$endif}{$endif} clibwebp);
  if HWebplib <> 0 then
  begin
    InternaldWebpAvail := true;
    PWebPGetDecoderVersion := TWebPGetDecoderVersion(GetProcedureAddress(HWebplib, 'WebPGetDecoderVersion'));
    PWebPGetInfo := TWebPGetInfo(GetProcedureAddress(HWebplib, 'WebPGetInfo'));
    PWebPFree := TWebPFree(GetProcedureAddress(HWebplib, 'WebPFree'));
    PWebPDecodeRGBA := TWebPDecodeRGBA(GetProcedureAddress(HWebplib, 'WebPDecodeRGBA'));
    PWebPDecodeARGB := TWebPDecodeARGB(GetProcedureAddress(HWebplib, 'WebPDecodeARGB'));
    PWebPDecodeBGRA := TWebPDecodeBGRA(GetProcedureAddress(HWebplib, 'WebPDecodeBGRA'));
    PWebPDecodeRGB := TWebPDecodeRGB(GetProcedureAddress(HWebplib, 'WebPDecodeRGB'));
    PWebPDecodeBGR := TWebPDecodeBGR(GetProcedureAddress(HWebplib, 'WebPDecodeBGR'));
{$ifndef MsWindows}
    InternalcWebpAvail := True;
    PWebpGetEncoderVersion := TWebpGetEncoderVersion(GetProcedureAddress(HWebplib, 'WebPGetEncoderVersion'));
    PWebpEncodeRGB := TWebpEncodeRGB(GetProcedureAddress(HWebplib, 'WebPEncodeRGB'));
    PWebpEncodeRGBA := TWebpEncodeRGBA(GetProcedureAddress(HWebplib, 'WebPEncodeRGBA'));
    PWebpEncodeBGR := TWebpEncodeBGR(GetProcedureAddress(HWebplib, 'WebPEncodeBGR'));
    PWebpEncodeBGRA := TWebpEncodeBGRA(GetProcedureAddress(HWebplib, 'WebPEncodeBGRA'));
{$endif}
  end;
{$ifdef Mswindows}
  HWebplibenc := LoadLibrary(cpath + {$ifdef DEBUG} 'Bin-Win\' + {$endif}clibwebpenc);
  if HWebplibenc <> 0 then
  begin
    InternalcWebpAvail := true;
    PWebpGetEncoderVersion := TWebpGetEncoderVersion(GetProcedureAddress(HWebplibenc, 'WebPGetEncoderVersion'));
    PWebpEncodeRGB := TWebpEncodeRGB(GetProcedureAddress(HWebplibenc, 'WebPEncodeRGB'));
    PWebpEncodeRGBA := TWebpEncodeRGBA(GetProcedureAddress(HWebplibenc, 'WebPEncodeRGBA'));
    PWebpEncodeBGR := TWebpEncodeBGR(GetProcedureAddress(HWebplibenc, 'WebPEncodeBGR'));
    PWebpEncodeBGRA := TWebpEncodeBGRA(GetProcedureAddress(HWebplibenc, 'WebPEncodeBGRA'));
  end;
{$endif}


finalization
  if HWebplib <> 0 then
    UnloadLibrary(HWebplib);
{$ifdef Mswindows}
  if HWebplibenc <> 0 then
    UnloadLibrary(HWebplibenc);
{$endif}


end.


