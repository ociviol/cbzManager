unit Utils.Graphics;

{
 Ollivier Civiol - 2019
 ollivier@civiol.eu
 https://ollivierciviolsoftware.wordpress.com/
}
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics,
{$if defined(Linux) or defined(Darwin)}
  cthreads,
{$endif}
  lcltype, intfgraphics;

type
  TResizeMode = (rmSmooth, rmFast);
  TFlipDir = (fdHorizontal, fdVertical, fdBoth);

  PRGBTripleArray = ^TRGBTripleArray; {for fast pixel routine}
//  {$ifdef mswindows}
//  TRGBTripleArray = array[0..trunc(bufwide/3)] of TRGBTriple; {for fast pixel routine}
//  {$else} {unix}
  TRGBTripleArray = array of tagRGBQUAD; {for fast pixel routine}
//  {$endif}

procedure SmoothStretchDraw(aCanvas : TCanvas; DestRect : TRect; aSrc : TBitmap);
function ResampleBitmap(const aSrc: TBitmap; DestWidth, DestHeight: Integer; rMode : TResizeMode = rmSmooth):TBitmap;
procedure Flip(aImg : TBitmap; aDirection : TFlipDir);
procedure RotateBitmap(Src : TBitmap; Angle : Integer);
procedure CropBitmap(aImg : TBitmap; aDest : TRect);

implementation

procedure RotateBitmap90(bitmap: TBitmap);
var
  tmp: TBitmap;
  src, dst: TLazIntfImage;
  ImgHandle, ImgMaskHandle: HBitmap;
  i, j, t, u, v: integer;
begin
  tmp := TBitmap.create;
  tmp.Width := Bitmap.Height;
  tmp.Height := Bitmap.Width;
  dst := TLazIntfImage.Create(0, 0);
  dst.LoadFromBitmap(tmp.Handle, tmp.MaskHandle);
  src := TLazIntfImage.Create(0, 0);
  src.LoadFromBitmap(bitmap.Handle, bitmap.MaskHandle);
  u := bitmap.width - 1;
  v := bitmap.height - 1;
  for i := 0 to u do begin
    t := u - i;
    for j := 0 to v do
      dst.Colors[j, t] := src.Colors[i, j];
  end;
  dst.CreateBitmaps(ImgHandle, ImgMaskHandle, false);
  tmp.Handle := ImgHandle;
  tmp.MaskHandle := ImgMaskHandle;
  dst.Free;
  bitmap.Assign(tmp);
  tmp.Free;
  src.Free;
end;

procedure RotateBitmap(Src : TBitmap; Angle : Integer);
var
  i : integer;
begin
  case Angle of
    90:  RotateBitmap90(Src);
    -90: for i:=0 to 2 do
           RotateBitmap90(Src);
  end;
end;

procedure Flipvertical(aImg : TBitmap);
var
  h, w : integer;
  imsrc, imdest : TLazIntfImage;
  bh, mh : HBitmap;
begin
  imsrc := TLazIntfImage.Create(aImg.Width, aImg.Height);
  imdest := TLazIntfImage.Create(aImg.Width, aImg.Height);
  try
    imsrc.LoadFromBitmap(aImg.Handle, aImg.MaskHandle);
    imdest.LoadFromBitmap(aImg.Handle, aImg.MaskHandle);
    for h := 0 to aImg.height - 1 do
      for w := 0 to aImg.Width - 1 do
        imdest.Colors[w, aImg.Height - 1 - h] := imsrc.Colors[w, h];

    imdest.CreateBitmaps(bh, mh);
    aImg.Handle:=bh;
    aImg.MaskHandle:=mh;
  finally
    imsrc.free;
    imdest.Free;
  end;
end;

procedure Fliphorizontal(aImg : TBitmap);
var
  h, w : integer;
  imsrc, imdest : TLazIntfImage;
  bh, mh : HBitmap;
begin
  imsrc := TLazIntfImage.Create(aImg.Width, aImg.Height);
  imdest := TLazIntfImage.Create(aImg.Width, aImg.Height);
  try
    imsrc.LoadFromBitmap(aImg.Handle, aImg.MaskHandle);
    imdest.LoadFromBitmap(aImg.Handle, aImg.MaskHandle);
    for h := 0 to aImg.height - 1 do
      for w := 0 to aImg.Width - 1 do
        imdest.Colors[aImg.Width - 1 - w, h] := imsrc.Colors[w, h];

    imdest.CreateBitmaps(bh, mh);
    aImg.Handle:=bh;
    aImg.MaskHandle:=mh;
  finally
    imsrc.free;
    imdest.Free;
  end;
end;

procedure Flip(aImg : TBitmap; aDirection : TFlipDir);
begin
  case aDirection of
    fdHorizontal: Fliphorizontal(aImg); // Horizontal flip
    fdVertical:   Flipvertical(aImg);   // Vertical flip
    fdBoth:       begin           // Both flip
                    Fliphorizontal(aImg);
                    Flipvertical(aImg);
                  end;
  end;
end;

procedure CropBitmap(aImg : TBitmap; aDest : TRect);
var
  h, w : integer;
  imsrc, imdest : TLazIntfImage;
  bh, mh : HBitmap;
begin
  imsrc := TLazIntfImage.Create(aImg.Width, aImg.Height);
  imdest := TLazIntfImage.Create(aImg.Width, aImg.Height);
  try
    imsrc.LoadFromBitmap(aImg.Handle, aImg.MaskHandle);
    imdest.LoadFromBitmap(aImg.Handle, aImg.MaskHandle);
    imdest.SetSize((aDest.Right-aDest.Left), (aDest.Bottom-aDest.Top));
    for h := aDest.Top to aDest.Bottom - 1 do
      for w := aDest.left to aDest.Right - 1 do
        imdest.Colors[w - aDest.Left, h - aDest.top] := imsrc.Colors[w, h];

    imdest.CreateBitmaps(bh, mh);
    aImg.Handle:=bh;
    aImg.MaskHandle:=mh;
  finally
    imsrc.free;
    imdest.Free;
  end;
end;

procedure SmoothResize(const Src, Dst: TBitmap);
var
  x, y, xP, yP, yP2, xP2: Integer;
  Read, Read2: PByteArray;
  t, t3, t13, z, z2, iz2: Integer;
  pc: PByteArray;
  w1, w2, w3, w4: Integer;
  Col1r, col1g, col1b, Col2r, col2g, col2b: byte;
begin
  xP2 := ((Src.Width - 1) shl 15) div Dst.Width;
  yP2 := ((Src.Height - 1) shl 15) div Dst.Height;
  yP := 0;
  for y := 0 to Dst.Height - 1 do
  begin
    xP := 0;
    Read := Src.ScanLine[yP shr 15];
    if yP shr 16 < Src.Height - 1 then
      Read2 := Src.ScanLine[yP shr 15 + 1]
    else
      Read2 := Src.ScanLine[yP shr 15];

    pc := Dst.ScanLine[y];
    z2 := yP and $7FFF;
    iz2 := $8000 - z2;
    for x := 0 to Dst.Width - 1 do
    begin
      t := xP shr 15;
      t3 := t * 3;
      t13 := t3 + 3;
      Col1r := Read^[t3];
      col1g := Read^[t3 + 1];
      col1b := Read^[t3 + 2];
      Col2r := Read2^[t3];
      col2g := Read2^[t3 + 1];
      col2b := Read2^[t3 + 2];
      z := xP and $7FFF;
      w2 := (z * iz2) shr 15;
      w1 := iz2 - w2;
      w4 := (z * z2) shr 15;
      w3 := z2 - w4;
      pc^[x * 3 + 2] := (col1b * w1 + Read^[t13 + 2] * w2 + col2b * w3 + Read2^[t13 + 2] * w4) shr 15;
      pc^[x * 3 + 1] := (col1g * w1 + Read^[t13 + 1] * w2 + col2g * w3 + Read2^[t13 + 1] * w4) shr 15;
      // (t+1)*3  is now t13
      pc^[x * 3] := (Col1r * w1 + Read2^[t13] * w2 + Col2r * w3 + Read2^[t13] * w4) shr 15;
      Inc(xP, xP2);
    end;
    Inc(yP, yP2);
  end;
end;

function ResampleBitmap(const aSrc: TBitmap; DestWidth, DestHeight: Integer; rMode : TResizeMode = rmSmooth):TBitmap;
begin
  result := TBitmap.Create;
  result.Width := DestWidth;
  result.Height := DestHeight;
  result.PixelFormat := aSrc.PixelFormat;
  if rMode = rmSmooth then
    SmoothResize(aSrc, result)
  else
    Result.Canvas.StretchDraw(Rect(0, 0, DestWidth, DestHeight), aSrc);
end;

procedure SmoothStretchDraw(aCanvas : TCanvas; DestRect : TRect; aSrc : TBitmap);
var
  b : TBitmap;
begin
  b := ResampleBitmap(aSrc, DestRect.Right - DestRect.Left, DestRect.Bottom - DestRect.Top);
  try
    aCanvas.Draw(DestRect.Top, DestRect.Left, b);
  finally
    b.Free;
  end;
end;

end.

